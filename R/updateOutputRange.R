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
attr(removeLastIncompleteRecord, "ex") <- function(){
  nRec <- 10
  dsComplete <- data.frame(date = seq(
    ISOdatetime(2010,1,1,0,0,30, tz = "UTC"), by = "30 min", length.out = nRec))
  dsIncomplete <- dsComplete
  dsIncomplete$date[nRec] <- dsIncomplete$date[nRec] - 2*60
  nrow( removeLastIncompleteRecord(dsComplete, "date") )
  nrow( removeLastIncompleteRecord(dsIncomplete, "date") )
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

#--------- updateRData --------------
#' @export
updateRData <- function(
  ### update time-ordered data stored in RData file with new data
  newData         ##<< the updated data
  , fileName      ##<< scalar string path name of the RData file
  , objectName = file_path_sans_ext(basename(fileName)) ##<< scalar
  ## string: the name of the R-object. By default the basename of the
  , message = paste("updated", objectName, "in file", fileName) ##<< scalar
  ## string of a message to be displaced.
  , ...   ##<< further arguments to \code{\link{updateOutputRange}},
  ## such as \code{dateColumn} and \code{indexColumns}.
  , version = 2 ##<< RData version see \code{\link{save}}
){
  if (!file.exists(fileName)) {
    updatedData <- newData
  } else {
    ##details<< Assumes that fileName refers to an RData file with
    ## only one object inside
    origData <- local({load(fileName); get(ls()[1])})
    updatedData <- updateOutputRange(origData, newData, ...)
  }
  assign(objectName, updatedData)
  save( list = objectName, file = fileName, version = version)
  if (length(message))  base::message(message)
}

#' @export
updateOutputRange <- function(
  ### update part of the time-ordered data.frame with new values
  dsTarget              ##<< data.frame to update
  , dsNew               ##<< data.frame with new values
  , indexColumns = group_vars(dsNew)  ##<< other index columns beside date
  , dateColumn = "date" ##<< name of the column holding the dates/times
){
  ##details<< Update values of \code{dsNew} in \code{dsTarget}. Both
  ## data.frames must have the same columns. Each row is identified by
  ## data and values in the indexColumns.
  checkEqualColNames(dsTarget, dsNew)
  ##details<< The function requires that both data.frames have unique dates
  ## per index in equidistant time steps. The time steps must match.
  dsNew <- dsNew %>%
    create_group_column(indexColumns) %>%
    checkEquidistant(dateColumn, "Source")
  dsTarget <- dsTarget %>%
    create_group_column(indexColumns) %>%
    checkEquidistant(dateColumn, "Target")
  checkSameTimestep(dsTarget, dsNew, dateColumn)
  datasets <- expandFactorLevels(list(dsTarget, dsNew), ".group")
  dsTarget <- datasets[[1]]
  dsNew <- datasets[[2]]
  ##details<< Existing rows of the same index and time in the range of
  ## \code{dsNew}
  ## are dropped from \code{dsTarget}, and rows of \code{dsNew} are appended.
  newGroups <- unique(dsNew$.group)
  for (group in newGroups)
     dsTarget <- replaceGroup(dsTarget, dsNew, indexColumns, dateColumn, group)
  ##value<< argument \code{dsTarget} with rows of \code{dsNew} updated.
  ans <- dsTarget %>% ungroup() %>% select(-.data$.group)
  ans
}

checkEqualColNames <- function(dsTarget, dsNew) {
  iMissing <- which(is.na(match(names(dsNew), names(dsTarget))))
  if (length(iMissing)) stop(
    "The following columns do not exist in target: "
    , paste(names(dsNew)[iMissing], collapse = ","))
  iMissing <- which(is.na(match(names(dsTarget), names(dsNew))))
  if (length(iMissing)) stop(
    "The following columns do not exist in source: "
    , paste(names(dsTarget)[iMissing], collapse = ","))
}

create_group_column <- function(data, indexColumns) {
  nIndex <- length(indexColumns)
  data <- if (nIndex) {
    groupsVarsOrig <- group_vars(data)
    data %>%
      ungroup() %>%
      unite(".group", all_of(indexColumns), remove = FALSE) %>%
      mutate(.group = factor(.data$.group)) %>%
      group_by_at(vars(groupsVarsOrig))
  } else {
    data %>% mutate(.group = factor(1))
  }
  ##value<< \code{data} with new index column ".group"
  data
}

checkEquidistant <- function(data, dateColumn, dataName = "data") {
  data <- data %>% group_by(.data$.group)
  diffDate <- data %>%
    group_by(.data$.group) %>%
    arrange(.data[[dateColumn]]) %>%
    do(
      as.data.frame(table(diff(as.numeric(.[[dateColumn]]))))
    )
  if (any((diffDate %>%  count())$n != 1 )) stop(
    dataName, " has no equidistant time steps within groups")
  if (any( diffDate[[2]] != diffDate[[2]][1])) stop(
    dataName, "has no equidistant time across groups")
  ##value<< \code{data} grouped by index column ".group" and
  ## arranged ascending in dateColumn
  data
}

checkSameTimestep <- function(dsNew, dsTarget, dateColumn) {
  timestepNewSec <- diff(as.numeric(head(dsNew[[dateColumn]],2)))
  timestepTargetSec <- diff(as.numeric(head(dsTarget[[dateColumn]],2)))
  if (timestepNewSec != timestepTargetSec) stop(
    "Target has a different time step than source")
}

expandFactorLevels <- function(
  ### expand a factor in all dataset to encompass levels of all sets
  datasets    ##<< list of data.frames
  , varName   ##<< scalar string of variable holding the factor
){
  # copied from dplyrUtil
  #https://stackoverflow.com/questions/46876312/how-to-merge-factors-when-binding-two-dataframes-together/50503461#50503461
  groupLevels <- lvls_union(lapply(datasets, "[[", varName))
  force(varName)
  ans <- map(datasets, function(dss){
    dss[[varName]] <- factor(dss[[varName]], levels = groupLevels)
    dss
  })
  ##value<< list of datasets with each entries column releveled
  ans
}


replaceGroup <- function(
  ### replace a single group of new in target
  dsTarget, dsNew, indexColumns, dateColumn, group
) {
  dsNewGroup <- filter(dsNew, .data$.group == group)
  timestepSec <- diff(as.numeric(head(dsTarget[[dateColumn]],2)))
  dates <- dsNewGroup[[dateColumn]]
  # same group but before timestamp of new
  dsTargetGroupBefore <- dsTarget %>%
    filter(.data$.group == group) %>%
    filter(.data[[dateColumn]] < min(dates))
  # if new is after but not adjacent to target, need to fill empty lines
  dsFillBefore <- getFillBefore(
    dsTargetGroupBefore, dateColumn, min(dates), timestepSec, indexColumns)
  dsTargetGroupAfter <- dsTarget %>%
    filter(.data$.group == group) %>%
    filter(.data[[dateColumn]] > max(dates))
  # if fill is before target but not adjacent need to created fill lines
  dsFillAfter <- getFilledAfter(
    dsTargetGroupAfter, dateColumn, max(dates), timestepSec, indexColumns)
  dsTargetOtherGroups <- filter(dsTarget, .data$.group != group)
  dsTarget <- bind_rows(
    dsTargetOtherGroups
    , dsTargetGroupBefore
    , dsFillBefore
    , dsNewGroup
    , dsFillAfter
    , dsTargetGroupAfter
  )
}

getFillBefore <- function(
  ### create empty lines if new date is not adjacent after garget
  dsTargetGroupBefore, dateColumn, minDateNew, timestepSec, indexColumns
) {
  if (!nrow(dsTargetGroupBefore)) return(data.frame())
  maxBefore <- max(dsTargetGroupBefore[[dateColumn]])
  gapSeconds = as.numeric(minDateNew) - as.numeric(maxBefore)
  nDiffs = as.integer(gapSeconds / timestepSec)
  if (nDiffs != gapSeconds / timestepSec) stop(
    "times of target are misaligned with times of new")
  if (nDiffs == 1) return(data.frame())
  ##value<< data.frame with dateColumn and index columns or empty data.frame
  cbind.data.frame(setNames(data.frame(
    maxBefore + (1:(nDiffs - 1))*timestepSec
  ), dateColumn)
  , dsTargetGroupBefore[1,indexColumns,drop = FALSE])
}

getFilledAfter <- function(
  ### create empty lines if new date is not adjacent before garget
  dsTargetGroupAfter, dateColumn, maxDateNew, timestepSec, indexColumns
) {
  if (!nrow(dsTargetGroupAfter)) return(data.frame())
  minAfter <- min(dsTargetGroupAfter[[dateColumn]])
  gapSeconds = as.numeric(minAfter) - as.numeric(maxDateNew)
  nDiffs = as.integer(gapSeconds / timestepSec)
  if (nDiffs != gapSeconds / timestepSec) stop(
    "times of target are misaligned with times of new")
  if (nDiffs == 1) return(data.frame())
  ##value<< data.frame with dateColumn and index columns or empty data.frame
  cbind.data.frame(setNames(data.frame(
    maxDateNew + (1:(nDiffs - 1))*timestepSec
  ), dateColumn)
  , dsTargetGroupAfter[1,indexColumns,drop = FALSE])
}

.tmp.f <- function(){
  load("tmp/ETLys.RData")
  load("tmp/ETLysTmp.RData")
  ans <- updateOutputRange(ETLys, ETLysTmp, dateColumn = "timestamp")
}
