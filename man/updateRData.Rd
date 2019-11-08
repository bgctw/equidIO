\name{updateRData}
\alias{updateRData}
\title{updateRData}
\description{update time-ordered data stored in RData file with new data}
\usage{updateRData(newData, fileName, objectName = file_path_sans_ext(basename(fileName)), 
    message = paste("updated", objectName, 
        "in file", fileName), ..., version = 2)}
\arguments{
  \item{newData}{the updated data}
  \item{fileName}{scalar string path name of the RData file}
  \item{objectName}{scalar
string: the name of the R-object. By default the basename of the }
  \item{message}{scalar
string of a message to be displaced.}
  \item{\dots}{further arguments to \code{\link{updateOutputRange}},
such as \code{dateColumn} and \code{indexColumns}.}
  \item{version}{RData version see \code{\link{save}}}
}
\details{Assumes that fileName refers to an RData file with 
only one object inside}


\author{Thomas Wutzler}





