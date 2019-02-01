\name{readEquidistantCsv}
\alias{readEquidistantCsv}
\title{readEquidistantCsv}
\description{read subset of data from csv files where rows have and equidistant time step}
\usage{readEquidistantCsv(fileName, startTime = numeric(0), 
    endTime = numeric(0), n_max = Inf, nRowsHeader = 0L, 
    nRowsColumnHeader = 0L, ..., fCreateTimestamp = fSetTimestampTimezone, 
    timezone = character(), fReadCsv = read_csv, 
    col_types = NULL)}
\arguments{
  \item{fileName}{fileName to read}
  \item{startTime}{POSIXct of minimum starting time to read}
  \item{endTime}{POSIXct of maximum ending time to read}
  \item{n_max}{maximum number of rows to read, by default all.
will be ignored if endTime is specified}
  \item{nRowsHeader}{number of header rows above column names}
  \item{nRowsColumnHeader}{number of header rows below column names
e.g. with unit information for each column }
  \item{\dots}{further arguments to \code{\link{read_csv}}}
  \item{fCreateTimestamp}{a
\code{function(data, timezone) -> data} 
to update or create column timestamp.
Argument timezone must have a default.
The default assumes the properly formatted timestamp column already in
file and sets the timezone if given to this function.}
  \item{timezone}{if specified, this timezone
is provided to \code{fCreateTimestamp}, otherwise the functions default is
used.}
  \item{fReadCsv}{variant of \code{\link{read_csv}}
used when reading the determined lines
, e.g. \code{\link{read_csvDouble}}}
  \item{col_types}{see \code{\link{read_csv}}}
}

\value{tibble read from file. If \code{nRowsColumnHeader > 0} then
attribute 'columnHeader' contains a dataframe with those rows below
the column names.
If \code{nRowsHeader > 0} then
attribute 'header' contains a string vector with those initial lines.}

\author{Thomas Wutzler}





