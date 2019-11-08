\name{read_csvDouble}
\alias{read_csvDouble}
\title{read csvDouble}
\description{read_csv but read guessed integer columns as double}
\usage{read_csvDouble(..., n_max = Inf, col_types = cols(.default = col_guess()))}
\arguments{
  \item{\dots}{further arguments to \code{\link{read_csv}}}
  \item{n_max}{see \code{\link{read_csv}}}
  \item{col_types}{see \code{\link{read_csv}}
the default suppresses the type guessing messages}
}
\details{Sometimes, double columns are guessed as integer,  e.g. with
runoff data where there are many zeros, an only occasionally 
positive values that can be recognized as double.
This functions modifies \code{read_csv} by changing guessed integer 
columns to double columns.}
\value{tibble as returned by \code{\link{read_csv}}}

\author{Thomas Wutzler}





