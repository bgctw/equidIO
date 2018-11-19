\name{equidIO-package}

\alias{equidIO-package}
\alias{equidIO}

\title{Reading and writing equidistant data.}

\description{
Logger data often comes in equidistant time steps. 
This package provides functions that allow reading and updating parts
based on start and end times.
}

\details{

Reading logger files
\itemize{
\item Extended read_csv: \code{\link{readEquidistantCsv}}  
\item Prevent reading double variable as integer: \code{\link{read_csvDouble}}  
}%itemize

Updating result files and frames: \itemize{
\item Skip uncomple last records: \code{\link{removeLastIncompleteRecord}}  
\item Update time-ordered data stored in RData file with new data: 
  \code{\link{updateRData}}  
\item Update parts of a data.frame: \code{\link{updateOutputRange}}  
}%itemize

Also have a look at the \href{../doc/index.html}{package vignettes}.
}%details

\author{Thomas Wutzler}



\keyword{ package }





