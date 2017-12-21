#' Read one or more TIFFs into 4D hyperstack
#'
#' @description \code{make_hyperstack} reads a single TIFF which may have 3-4
#'   dimensions.
#'
#' @details Where possible the hyperstack will include the \code{description}
#'   attribute from the original scanimage file. This can be used with
#'   \code{\link{scanimageinfo}} or \code{\link{parse_description}} to access
#'   useful information about the underlying image.
#'
#' @param x Path to (one or more) TIFFs
#' @param zslices Number of zlices in each TIFF
#' @param timepoints Number of timepoints in each TIFF
#' @param time.axis Which axis will end up as the time axis in the output
#'
#' @return a 4D array typically ordered x,y,z,t or x,y,t,file.
#' @export
#' @importFrom abind abind
make_hyperstack<-function(x, zslices, timepoints, time.axis=4, ...){
  if(is.factor(x)) x=as.character(x)
  if(is.character(x)) x=read.any.tiff(x, ...)
  if(missing(zslices)) zslices=length(x)/timepoints
  if(missing(timepoints)) timepoints=length(x)/zslices
  if(round(zslices*timepoints)!=length(x))
    stop("zlices * timepoints must equal total slices in stack")
  timepoint.stacks=list()
  for(i in 1:timepoints){
    # find raw slice indices for this time point
    idxs=seq(from=zslices*(i-1)+1,length.out = zslices)
    timepoint.stacks[[i]]=abind(x[idxs], along = 3)
  }
  res=abind(timepoint.stacks, along = time.axis)
  attr(res, 'description')=attr(x[[1]], 'description')
  res
}

#' @description \code{make_hyperstack_multifile} reads multiple TIFFs each of
#'   which will have 2-3 dimensions (usually x,y,t) and combines into a single
#'   hyperstack x,y,t,file
#' @param file.axis Which axis will end up as the file axis in the output when
#'   reading multiple files
#' @param ... Additional arguments passed to \code{make_hyperstack}, thence to
#'   \code{\link{read.any.tiff}} and eventually to \code{\link{read.scanimage}}.
#' @export
#' @rdname make_hyperstack
#' @importFrom abind abind
make_hyperstack_multifile <- function(x, file.axis=4, ...) {
  ll=lapply(x, make_hyperstack, zslices=1, time.axis=setdiff(3:4, file.axis), ...)
  res=abind(ll, along=file.axis)
  attr(res, 'description')=attr(ll[[1]], 'description')
  res
}
