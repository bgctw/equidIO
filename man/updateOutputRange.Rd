\name{updateOutputRange}
\alias{updateOutputRange}
\title{updateOutputRange}
\description{update part of the time-ordered data.frame with new values}
\usage{updateOutputRange(dsTarget, dsNew, indexColumns = group_vars(dsNew), 
    dateColumn = "date")}
\arguments{
  \item{dsTarget}{data.frame to update}
  \item{dsNew}{data.frame with new values}
  \item{indexColumns}{other index columns beside date}
  \item{dateColumn}{name of the column holding the dates/times}
}
\details{Update values of \code{dsNew} in \code{dsTarget}. Both
data.frames must have the same columns. Each row is identified by
data and values in the indexColumns.

The function requires that both data.frames have unique dates
per index in equidistant time steps. The time steps must match.

Existing rows of the same index and time in the range of
\code{dsNew}
are dropped from \code{dsTarget}, and rows of \code{dsNew} are appended.}
\value{arguemt \code{dsTarget} with rows of \code{dsNew} updated.}

\author{Thomas Wutzler}





