#' @export
readEquidistantCsv <- function(
  ### read subset of data from csv files where rows have and equidistant time step
  fileName   ##<< fileName to read
  , startTime = numeric(0)  ##<< POSIXct of minimum starting time to read
  , endTime = numeric(0)    ##<< POSIXct of maximum ending time to read
  , n_max = Inf             ##<< maximum number of rows to read, by default all.
  ## will be ignored if endTime is specified
  , nRowsHeader = 0L        ##<< number of header rows above column names
  , nRowsColumnHeader = 0L  ##<< number of header rows below column names
  ## e.g. with unit information for each column 
  , ...   ##<< further arguments to \code{\link{read_csv}} 
  , fCreateTimestamp = fSetTimestampTimezone ##<< a
    ## \code{function(data, timezone) -> data} 
    ## to update or create column timestamp.
    ## Argument timezone must have a default.
    ## The default assumes the properly formatted timestamp column already in
    ## file and sets the timezone if given to this function.
  , timezone = character()  ##<< if specified, this timezone 
  ## is provided to \code{fCreateTimestamp}, otherwise the functions default is
  ## used.
  , fReadCsv = read_csv     ##<< variant of \code{\link{read_csv}}
  ## used when reading the determined lines
  ## , e.g. \code{\link{read_csvDouble}}
  , col_types = NULL        ##<< see \code{\link{read_csv}}
){
  dfHeader <- read_csv(
    fileName, n_max = nRowsColumnHeader, skip = nRowsHeader
    , col_types = cols(.default = col_character())
    , ... )
  colNames <- names(dfHeader)
  # read the first two rows to determine timestep and initial time
  df1 <- suppressMessages(read_csv( 
    fileName, n_max = 2L
    , skip = nRowsHeader + 1L + nRowsColumnHeader
    , col_names = colNames, col_types = col_types, ... )) 
  df1 <- if (!length(timezone) || !nzchar(timezone)) {
    # use the default argumetn of the function
    df1 %>% fCreateTimestamp() 
  } else {
    df1 %>% fCreateTimestamp(timezone)
  }
  if (any(is.na(df1$timestamp))) stop("Could not create timestamp.")
  strideSec <- diff(as.numeric(df1$timestamp)[1:2])
  startTimeFile <- df1$timestamp[1]
  # determine rows to skip based on starting time
  if (!length(startTime)) startTime <- startTimeFile
  if (startTime < startTimeFile) startTime <- startTimeFile
  nRowsSkip = (as.numeric(startTime) - as.numeric(startTimeFile)) / strideSec
  if (nRowsSkip != as.integer(nRowsSkip)) stop(
    "startTime must be a multiple of timesteps (", strideSec, "s) after file's "
    , "start time ", startTimeFile, " but was ", startTime )
  # determine rows to read based on end time
  if (length(endTime)) {
    emptyResult = df1[FALSE,]
    if (endTime < startTime) return(emptyResult)
    n_max <- 1 + (as.numeric(endTime) - as.numeric(startTime)) /
      strideSec
    if (n_max != as.integer(n_max)) stop(
      "(endTime - startTime) must be a multiple of timesteps (", strideSec
      , "s) but was ", (as.numeric(endTime) - as.numeric(startTime)))
  }
  #df <- read.csv(fileName, header = T, sep = ",")
  df <- fReadCsv(
    fileName
    , skip = nRowsHeader + 1L + nRowsColumnHeader + nRowsSkip 
    , n_max = n_max, col_names = colNames, col_types = col_types, ... 
    ) 
  df <- if (!length(timezone) || !nzchar(timezone)) {
    # use the default argumetn of the function
    df %>% fCreateTimestamp() 
  } else {
    df %>% fCreateTimestamp(timezone)
  }
  if (as.POSIXct(round.POSIXt(df$timestamp[1])) != startTime) stop(
    "file ",fileName," has not all equidistant time steps.")
  ##value<< tibble read from file. If \code{nRowsColumnHeader > 0} then
  ## attribute 'columnHeader' contains a dataframe with those rows below
  ## the column names.
  ## If \code{nRowsHeader > 0} then
  ## attribute 'header' contains a string vector with those initial lines.
  if (nRowsColumnHeader > 0) attr(df, "columnHeader") <- dfHeader
  if (nRowsHeader > 0) attr(df, "header") <- read_lines(
    fileName, n_max = nRowsHeader)
  df
}

fSetTimestampTimezone <- function(
  ### set the tzone attribute of column timestamp
  data  ##<< data.frame with column timestamp
  ,timezone  ##<< the time zonen to set
){ 
  if (length(timezone)) 
    attr(data$timestamp, "tzone") <- timezone
  data
}

#' @export
read_csvDouble <- function(
  ### read_csv but read guessed integer columns as double
  ... ##<< further arguments to \code{\link{read_csv}}
  , n_max = Inf        ##<< see \code{\link{read_csv}}
  , col_types = cols(.default = col_guess()) ##<< see \code{\link{read_csv}}
  ## the default suppresses the type guessing messages
){
  ##details<< Sometimes, double columns are guessed as integer,  e.g. with
  ## runoff data where there are many zeros, an only occasionally 
  ## positive values that can be recognized as double.
  ## This functions modifies \code{read_csv} by changing guessed integer 
  ## columns to double columns.
  #https://stackoverflow.com/questions/52934467/how-to-tell-readrread-csv-to-guess-double-column-correctly
  colTypes <- read_csv(..., n_max = 3, col_types = col_types) %>% attr("spec")
  isIntCol <- map_lgl(colTypes$cols, identical, col_integer())
  colTypes$cols[isIntCol] <- replicate(sum(isIntCol), col_double())
  ##value<< tibble as returned by \code{\link{read_csv}}
  ans <- read_csv(..., n_max = n_max, col_types = colTypes)
  ans
}


