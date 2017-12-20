#' Read one or more TIFFs into 4D hyperstack
#'
#' @description \code{make_hyperstack} reads a single TIFF which may have 3-4
#'   dimensions.
#'
#' @param x Path to (one or more) TIFFs
#' @param zslices Number of zlices in each TIFF
#' @param timepoints Number of timepoints in each TIFF
#' @param time.axis Which axis will end up as the time axis in the output
#'
#' @return a 4D array typically ordered x,y,z,t or x,y,t,file
#' @export
#' @importFrom abind abind
make_hyperstack<-function(x, zslices, timepoints, time.axis=4){
  if(is.character(x)) x=read.any.tiff(x)
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
  abind(timepoint.stacks, along = time.axis)
}

#' @description \code{make_hyperstack_multifile} reads multiple TIFFs each of
#'   which will have 2-3 dimensions (usually x,y,t) and combines into a single
#'   hyperstack x,y,t,file
#' @param file.axis Which axis will end up as the file axis in the output when
#'   reading multiple files.
#' @export
#' @rdname make_hyperstack
#' @importFrom abind abind
make_hyperstack_multifile <- function(x, file.axis=4) {
  ll=lapply(x, make_hyperstack, zslices=1, time.axis=setdiff(3:4, file.axis))
  abind(ll, along=file.axis)
}
