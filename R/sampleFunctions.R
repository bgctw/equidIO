fSampleFunc <- function(
	### Adding two vectors
	a.n.v		##<< numeric vector: first term
	,b.n.v		##<< numeric vector: second term
){
	##seealso<< \code{\link{myPackageId}}
	#
	##details<<
	## If the two vectors are of different length, the shorter one
	## is recycled.
	#
	a.n.v + b.n.v
	### The componentwise sum of the two arguments
}
attr(fSampleFunc,"ex") <- function(){
	# This example appears in the generated help.
	fSampleFunc(1:10, 2)
  donttest({
    if (!interactive()) stop("should only be executed interactively.")
  })
}
