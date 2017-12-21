#' Read scanimage tiffs into an R object
#'
#' @param source Either name of the file to read from or a raw vector
#'   representing the TIFF file content.
#' @param slices Which slices to return (default of Inf reads all available
#'   slices). A slice is a single channel 2D TIFF image.
#' @param frames Which frames to read (default of Inf reads all available
#'   frames). A frame is one or more 2D TIFF images acquired simultaneously.
#' @param channels which channels to read (default value of \code{Inf} implies
#'   all channels).
#' @param info if set to \code{TRUE} then the resulting image(s) will also
#'   contain information from TIFF tags as attributes
#' @param as.is Return original pixel values without rescaling where possible.
#'   This is usually what you want for 8 and 16 bit scanimage TIFFs (default
#'   \code{TRUE}).
#' @param ... additional arguments for \code{\link[tiff]{readTIFF}}.
#' @return A list of 2D matrices (height x width) except for the special case
#'   when the first slice has been requested.
#' @importFrom tiff readTIFF
#' @export
#' @examples
#' t=read.scanimage(system.file('extdata/Blank-IPA_1s_16r_032.tif', package='scanimage'))
read.scanimage<-function(source, slices=Inf, frames=Inf, channels=Inf, info=T, as.is=TRUE,
                         ...){
  # set all = FALSE if we just want slice 1
  all = !(is.finite(slices) && length(slices)==1 && slices==1)
  t=readTIFF(source, info=info, all=all, native=FALSE, as.is=as.is, ...)

  # figure out which subset of slices we want
  if(all(is.finite(frames)) || all(is.finite(channels))) {
    pd=parse_description(t)
    nchan=pd$state.acq.numberOfChannelsSave
    nframes=pd$state.acq.numberOfFrames
    if(nframes*nchan != length(t))
      stop("Mismatch between reported number of frames/channels and the number",
           " of raw slices read from TIFF")
    allchannels=seq_len(nchan)
    allframes=seq_len(nframes)
    if(all(is.finite(channels))) {
      if(!all(channels%in%allchannels)) stop("Bad channel number!")
    } else channels <- allchannels
    if(all(is.finite(frames))) {
      if(!all(frames%in%allframes)) stop("Bad frame number!")
    } else frames <- allframes
    # set up an index matrix to help figure out which slices we want
    # NB rows and columns
    index_mat=matrix(FALSE, nrow=nchan, ncol = nframes)
    index_mat[channels, frames]=T
    slices=which(index_mat)
  }

  if(all && all(is.finite(slices))) t[slices] else t
}

#' parse description of ScanImage TIFF file, converting it to key-value list
#'
#' @details ScanImage TIFF flies contain a single description field, which is a
#'   CR delimited values of the form \code{key=value}.
#' @param x Path to a TIFF file, one or more slices returned by
#'   \code{\link{read.scanimage}} or a raw data block.
#' @param raw Whether to return the raw description field as a single string or
#'   when \code{FALSE} (the default) to return it is as a list containing parsed
#'   R data types.
#' @return a named \code{list} or, when \code{raw=TRUE}, a character vector of
#'   length 1.
#' @export
#' @importFrom stringr str_split_fixed
#' @seealso \code{\link{read.scanimage}}
#' @examples
#' desc=parse_description(system.file(
#'   'extdata/Blank-IPA_1s_16r_032.tif',package='scanimage'))
#' desc$state.configName
#' # [1] "ajdm_piezo"
#' desc$state.acq.frameRate
#' # [1] 8.138021 (Hz)
parse_description<-function(x, raw=FALSE){
  if(is.character(x)) x=read.scanimage(x, slices=1)
  else if(is.list(x)) x=x[[1]]
  desc=attr(x, 'description')
  if(is.null(desc)) stop("No description!")
  if(raw) return(desc)
  lines=unlist(strsplit(desc, split = "(\r|\\\\r)"))
  cols=str_split_fixed(lines, pattern = "=", n = 2)
  keyvals=structure(cols[,2], .Names=cols[,1])
  lapply(keyvals, function(x) {t=try(eval(parse(text=x)), silent = T);
                               ifelse(inherits(t,'try-error'), x,t)})
}


#' Convenience function to read any TIFF image with attached information
#'
#' @param f Path to TIFF
#' @param ... Additional arguments passed to \code{\link{read.scanimage}} or
#'   \code{\link[tiff]{readTIFF}}.
#'
#' @return An array of the dimensions height x width x channels. If there is
#'   only one channel the result is a matrix. The values are integers.
#' @export
read.any.tiff<-function(f, ...){
  x=read.scanimage(f, slices=1)
  desc=attr(x,'description')
  is_scanimage=!is.null(desc) && grepl("state.configPath", desc,fixed = TRUE)
  if(is_scanimage) read.scanimage(f, ...) else tiff::readTIFF(f, all=T, info = T, ...)
}
