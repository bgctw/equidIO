\name{removeLastIncompleteRecord}
\alias{removeLastIncompleteRecord}
\title{removeLastIncompleteRecord}
\description{remove last row, if its timestep does not match (second - first row)}
\usage{removeLastIncompleteRecord(data, colTimestamp = "timestamp")}
\arguments{
  \item{data}{the data.frame to check}
  \item{colTimestamp}{scalar sting column name
holding the time steps}
}
\details{Expects the timestamp column to hold end-of period timestamps
with no missings.
If the timestep is smaller than the first, this indicates that the
data of the last time step was not complete yet.
This method checks on this condition and removes an incomplete last row.}
\value{\code{data} with last row removed, if the time step is incomplete}

\author{Thomas Wutzler}




\examples{
nRec <- 10
dsComplete <- data.frame(date = seq(
  ISOdatetime(2010,1,1,0,0,30, tz = "UTC"), by = "30 min", length.out = nRec))
dsIncomplete <- dsComplete
dsIncomplete$date[nRec] <- dsIncomplete$date[nRec] - 2*60
nrow( removeLastIncompleteRecord(dsComplete, "date") )
nrow( removeLastIncompleteRecord(dsIncomplete, "date") )
}
