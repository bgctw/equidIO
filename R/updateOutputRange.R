#' @export
updateOutputRange <- function(
  ### update part of the data.frame with new values
  dsTarget              ##<< data.frame to update
  , dsNew               ##<< data.frame with new values
  , indexColumns = c()  ##<< other index columns beside date
  , dateColumn = "date" ##<< name of the column holding the dates/times
){
  ##details<< Update values of \code{dsNew} in \code{dsTarget}. Both
  ## data.frames must have the same columns. Each row is identified by
  ## data and values in the indexColumns.
  iMissing <- which(is.na(match(names(dsNew), names(dsTarget))))
  if (length(iMissing)) stop(
    "The following columns do not exist in target: "
    , paste(names(dsNew)[iMissing], collapse = ","))
  iMissing <- which(is.na(match(names(dsTarget), names(dsNew))))
  if (length(iMissing)) stop(
    "The following columns do not exist in source: "
    , paste(names(dsTarget)[iMissing], collapse = ","))
  nIndex <- length(indexColumns)
  ##details<< The function requires that both data.frames have unique dates
  ## per index in equidistant time steps. The time steps must match.
  dsNew <- if (nIndex) {
    dsNew %>%
      unite(".group", !!!syms(indexColumns), remove = FALSE) %>%
      mutate(.group = factor(!!sym(".group")))
  } else {
    dsNew %>% mutate(.group = factor(1))
  }
  diffDateSrc <- dsNew %>%
    group_by(!!sym(".group")) %>%
    arrange(!!sym(dateColumn)) %>%
    do(
      as.data.frame(table(diff(as.numeric(.[[dateColumn]]))))
    )
  if (any((diffDateSrc %>%  count())$n != 1 )) stop(
    "Src has no equidistant time steps within groups")
  if (any( diffDateSrc[[2]] != diffDateSrc[[2]][1])) stop(
    "Src has no equidistant time across groups")
  dsTarget <- if (nIndex) {
    dsTarget %>%
      unite(".group", !!!syms(indexColumns), remove = FALSE) %>%
      mutate(.group = factor(!!sym(".group")))
  } else {
    dsTarget %>% mutate(.group = factor(1))
  }
  diffDateTarget <- dsTarget %>%
    group_by(!!sym(".group")) %>%
    arrange(!!sym(dateColumn)) %>%
    do(
        as.data.frame(table(diff(as.numeric(.[[dateColumn]]))))
      )
  diffDateSec = diffDateSrc[[2]][1]
  if (any((diffDateTarget %>%  count())$n != 1 )) stop(
    "Target has no equidistant time steps within groups")
  if (any( diffDateTarget[[2]] != diffDateTarget[[2]][1])) stop(
    "Target has no equidistant time across groups")
  if (diffDateTarget[[2]][1] != diffDateSec) stop(
    "Target has a different time step than source")
  diffDateSec = as.numeric(levels(diffDateSec)[diffDateSec])
  # make sure group levels are the same
  newGroups <- levels(dsNew$.group)
  if (nIndex) {
    groupLevels <- lvls_union(list(dsTarget$.group, dsNew$.group))
    dsTarget <- dsTarget %>%
      mutate(.group = fct_expand(!!sym(".group"), groupLevels))
    dsNew <- dsNew %>%
      mutate(.group = fct_expand(!!sym(".group"), groupLevels))
  }
  ##details<< Existing rows of the same index and time in the range of
  ## \code{dsNew}
  ## are dropped from \code{dsTarget}, and rows of \code{dsNew} are appended.
  # for the index columns, transform to string so that rbind works
  #col <- indexColumns[1]
  isInconsistentFactor <- sapply( names(dsTarget),  function(col){
    (is.factor(dsTarget[[col]]) | is.factor(dsNew[[col]])) &&
       any(levels(dsTarget[[col]]) != levels(dsNew[[col]]))
  })
  if (sum(isInconsistentFactor)) warning(
    "releveling factors ", paste(names(dsTarget)[isInconsistentFactor], collapse = ","))
  #https://stackoverflow.com/questions/46876312/how-to-merge-factors-when-binding-two-dataframes-together/50503461#50503461
  for (col in names(dsTarget)[isInconsistentFactor]) {
    allLevels = lvls_union(list(dsTarget[[col]], dsNew[[col]]))
    dsTarget <- dsTarget %>% mutate(!!col := fct_expand(UQ(sym(col)), allLevels))
    dsNew <- dsNew %>% mutate(!!col := fct_expand(UQ(sym(col)), allLevels))
  }
  #group <- newGroups[1]
  for (group in newGroups) {
     dsNewGroup <- filter(dsNew, UQ(sym(".group")) == group)
     dates <- dsNewGroup[[dateColumn]]
     # dsTargetGroupOutside <- dsTarget %>%
     #   filter(UQ(sym(".group")) == group) %>%
     #   filter(UQ(sym(dateColumn)) < min(dates) | UQ(sym(dateColumn)) > max(dates))
     dsTargetGroupBefore <- dsTarget %>%
       filter(UQ(sym(".group")) == group) %>%
       filter(UQ(sym(dateColumn)) < min(dates))
     dsFillBefore <- data.frame()
     if (nrow(dsTargetGroupBefore)) {
       maxBefore <- max(dsTargetGroupBefore[[dateColumn]])
       minNew <- min(dates)
       gapSeconds = as.numeric(minNew) - as.numeric(maxBefore)
       nDiffs = as.integer(gapSeconds / diffDateSec)
       if (nDiffs != gapSeconds / diffDateSec) stop(
         "times of target are misaligned with times of new")
       if (nDiffs != 1) {
         dsFillBefore = cbind(setNames(data.frame(
           maxBefore + (1:(nDiffs - 1))*diffDateSec
           ), dateColumn)
           , dsTargetGroupBefore[1,indexColumns,drop = FALSE])
       }
     }
     dsTargetGroupAfter <- dsTarget %>%
       filter(UQ(sym(".group")) == group) %>%
       filter(UQ(sym(dateColumn)) > max(dates))
     dsFillAfter <- data.frame()
     if (nrow(dsTargetGroupAfter)) {
       minAfter <- min(dsTargetGroupAfter[[dateColumn]])
       maxNew <- max(dates)
       gapSeconds = as.numeric(minAfter) - as.numeric(maxNew)
       nDiffs = as.integer(gapSeconds / diffDateSec)
       if (nDiffs != gapSeconds / diffDateSec) stop(
         "times of target are misaligned with times of new")
       if (nDiffs != 1) {
         dsFillAfter = cbind(setNames(data.frame(
           maxNew + (1:(nDiffs - 1))*diffDateSec
         ), dateColumn)
         , dsTargetGroupAfter[1,indexColumns,drop = FALSE])
       }
     }
     dsTargetOtherGroups <- filter(dsTarget, UQ(sym(".group")) != group)
     dsTarget <- bind_rows(
       dsTargetOtherGroups
       , dsTargetGroupBefore
       , dsFillBefore
       , dsNewGroup
       , dsFillAfter
       , dsTargetGroupAfter
     )
  }
  ##value<< arguemt \code{dsTarget} with rows of \code{dsNew} updated.
  ans <- dsTarget %>% select(-!!sym(".group"))
  ans
}

.tmp.f <- function(){
  #https://stackoverflow.com/questions/46876312/how-to-merge-factors-when-binding-two-dataframes-together

  bind_rowsFactors <- function(
    ### bind_rows on two data.frames with merging factors levels
    a    ##<< first data.frame to bind
    , b  ##<< second data.frame to bind
    , ...  ##<< further arguments to \code{bind_rows}
  ){
    isInconsistentFactor <- sapply( names(a),  function(col){
      (is.factor(a[[col]]) | is.factor(b[[col]])) &&
        any(levels(a[[col]]) != levels(b[[col]]))
    })
    if (sum(isInconsistentFactor)) warning(
      "releveling factors ", paste(names(a)[isInconsistentFactor], collapse = ","))
    for (col in names(a)[isInconsistentFactor]) {
      a <- mutate(ungroup(a), !!col := as.character(!!rlang::sym(col)))
      b <- mutate(ungroup(b), !!col := as.character(!!rlang::sym(col)))
    }
    ans <- bind_rows(a, b, ...)
    # convert former factors form string back to factor
    for (col in names(ans)[isInconsistentFactor]) {
      ans <- mutate(ungroup(ans), !!col := factor(!!rlang::sym(col)))
    }
    ##value<< result of \code{bind_rows} with inconsistend factor columns still factors
    ans
  }

  #library(dplyr)
  a = data.frame(f = factor(c("a", "b")), g = c("a", "a"))
  b = data.frame(f = factor(c("a", "c")), g = c("a", "a"))
  a = a %>% group_by(g) %>% mutate(n = 1)
  b = b %>% group_by(g) %>% mutate(n = 2)
  #bind_rows(a,b)
  bind_rowsFactors(a,b)
}

#' @export
removeLastIncompleteRecord <- function(
  ### remove last row, if its timestep does not match (second - first row)
  data   ##<< the data.frame to check
  , colTimestamp = "timestamp"  ##<< scalar sting column name
  ## holding the time steps
){
  ##details<< Expects the timestamp column to hold end-of period timestamps
  ## with no missings.
  ## If the timestep is smaller than the first, this indicates that the
  ## data of the last time step was not complete yet.
  ## This method checks on this condition and removes an incomplete last row.
  fRemoveSingleGroup <- function(data, colTimestamp){
    if (nrow(data) <= 2) return(data)
    if (any(is.na(data[[colTimestamp]]))) stop(
      "no missings allowed in timestamp column ", colTimestamp)
    timestepFirst <- diff(head(data[[colTimestamp]], 2L))
    timestepLast <- diff(tail(data[[colTimestamp]], 2L))
    ##value<< \code{data} with last row removed, if the time step is incomplete
    if (timestepLast == timestepFirst) return(data)
    slice(data, -n())
  }
  .mapGroups(data, fRemoveSingleGroup, colTimestamp = colTimestamp)
}

# copied from lysiproc package
.mapGroups <- function(
  ### split-map-combine
  data  ##<< groped data.frame
  , FUN  ##<< function(data.frmae, ...) -> data.frame to apply to subsets
  , ...  ##<< further arguments to FUN
  , drop = TRUE  ##<< logical indicating if levels that do not occur should 
  ## be dropped. Set to FALSE if FUN returns a data.frame also 
  ## for zero-row inputs.
){
  # https://coolbutuseless.bitbucket.io/2018/03/03/split-apply-combine-my-search-for-a-replacement-for-group_by---do/
  groupVars <- group_vars(data)
  if (!length(groupVars)) return(FUN(data,...))
  data %>% 
    split(select(.,groupVars), drop = drop) %>% 
    map_dfr(FUN,...)
}

