\name{expandAllInconsistentFactorLevels}
\alias{expandAllInconsistentFactorLevels}
\title{expandAllInconsistentFactorLevels}
\description{expand a factor variables in all dataset to encompass levels of all sets}
\usage{expandAllInconsistentFactorLevels(..., .noWarningCols = character(0))}
\arguments{
  \item{\dots}{list of data.frames or several data.frames separated by comma}
  \item{.noWarningCols}{string vector: do not warn for the these
columns}
}

\value{\code{datasets} with updated factor columns}

\author{Thomas Wutzler}




\examples{
df1 <- data.frame(f = factor(c("D","D","C")))
df2 <- data.frame(f = factor(c("C","C","A"))
                  , desc = c("forC1","forC2","forA1"))
if (requireNamespace("dplyr"))
  dplyr::bind_rows(expandAllInconsistentFactorLevels(df1,df2))
left_joinFactors(df1,df2)
}
