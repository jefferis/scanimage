#' Construct a mean image, blur and threshold to get
#'
#' @details This is currently targeted at single plane imaging data.
#'
#' @param x A hyperstack or path to one or more TIFF files (see
#'   \code{\link{make_hyperstack}})
#' @param blur The gaussian sigma to use
#' @param thr A thresholding specification (passed to
#'   \code{imager::\link[imager]{threshold}})
#' @param ... Additional arguments passed to \code{\link{make_hyperstack}}
#'
#' @return A 2D mask in the form of an imager::cimg object
#' @seealso \code{\link[imager]{cimg}}, \code{\link{t_profile}},
#'   \code{\link{make_hyperstack}}
#' @export
#'
#' @importFrom imager threshold as.cimg isoblur
make_roi_from_mean <- function(x, blur=5, thr="95%", ...) {
  if(is.character(x)) {
    x <- if(length(x)==1) make_hyperstack(x, ...) else make_hyperstack_multifile(x)
  }
  # make mean image
  mx=apply(x, 1:2, mean)

  cx=as.cimg(mx)
  bcx=isoblur(cx, sigma=blur)
  imager::threshold(bcx, thr)
}

#' Construct a temporal profile of voxels within an ROI
#'
#' @param x A hyperstack with imaging data
#' @param mask An ROI mask e.g. from \code{\link{make_roi_from_mean}}
#'
#' @return A matrix with the third and fourth dimensions of \code{x}
#' @export
#'
#' @seealso \code{\link{make_roi_from_mean}}
t_profile <- function(x, mask) {
  colMeans(apply(x, 3:4, "[", mask))
}
