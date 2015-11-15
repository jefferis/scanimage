#' Read scanimage tiffs into an R object
#'
#' @param source Either name of the file to read from or a raw vector
#'   representing the TIFF file content.
#' @param slices Which slices to return (default of Inf reads all available
#'   slices)
#' @param channels which channels to read. FIXME only the default
#'   (\code{channels=Inf}) implying read all channels is presently implemented.
#' @param info if set to \code{TRUE} then the resulting image(s) will also
#'   contain information from TIFF tags as attributes
#' @param as.is Return original pixel values without rescaling where possible.
#'   This is usually what you want for 8 and 16 bit scanimage TIFFs (default
#'   \code{TRUE}).
#' @param ... additional arguments for \code{\link[tiff]{readTIFF}}.
#' @return An array of the dimensions height x width x channels. If there is
#'   only one channel the result is a matrix. The values are integers.
#' @importFrom tiff readTIFF
#' @export
#' @examples
#' t=read.scanimage(system.file('extdata/Blank-IPA_1s_16r_032.tif', package='scanimage'))
read.scanimage<-function(source, slices=Inf, channels=Inf, info=T, as.is=TRUE,
                         ...){
  if(is.finite(channels)) stop("channel selection not yet implemented!")
  # set all = FALSE if we just want slice 1
  all = !(is.finite(slices) && length(slices)==1 && slices==1)
  t=readTIFF(source, info=info, all=all, native=FALSE, as.is=as.is, ...)
  if(all && is.finite(slices)) t[slices] else t
}

#' parse description of ScanImage TIFF file, converting it to key-value list
#'
#' @details ScanImage TIFF flies contain a single description field, which is a
#'   CR delimited values of the form \code{key=value}.
#' @param x Path to a TIFF file or a raw data block
#' @param raw Whether to return the raw description field as a single string or
#'   when \code{FALSE} (the default) to return it is as a list containing parsed
#'   R data types.
#' @return a named \code{list} or, when \code{raw=TRUE}, a character vector of
#'   length 1.
#' @export
#' @importFrom stringr str_split_fixed
#' @examples
#' desc=parse_description(system.file(
#'   'extdata/Blank-IPA_1s_16r_032.tif',package='scanimage'))
#' desc$state.configName
#' # [1] "ajdm_piezo"
#' desc$state.acq.frameRate
#' # [1] 8.138021 (Hz)
parse_description<-function(x, raw=FALSE){
  if(is.character(x)) x=read.scanimage(x, slices=1)
  desc=attr(x, 'description')
  if(is.null(desc)) stop("No description!")
  if(raw) return(desc)
  lines=unlist(strsplit(desc, split = "(\r|\\\\r)"))
  cols=str_split_fixed(lines, pattern = "=", n = 2)
  keyvals=structure(cols[,2], .Names=cols[,1])
  lapply(keyvals, function(x) {t=try(eval(parse(text=x)), silent = T);
                               ifelse(inherits(t,'try-error'), x,t)})
}
