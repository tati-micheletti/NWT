#' prepInputStack is a wrapper to prepInput a stack of raster layers.
#'
#' @param targetFile Character string giving the path to the eventual file
#'   (raster, shapefile, csv, etc.) after downloading and extracting from a zip
#'   or tar archive. This is the file \emph{before} it is passed to
#'   \code{postProcess}. Currently, the internal checksumming does not checksum
#'   the file after it is \code{postProcess}ed (e.g., cropped/reprojected/masked).
#'   Using \code{Cache} around \code{prepInputs} will do a sufficient job in these cases.
#'   See table in \code{\link{preProcess}}.
#'
#' @param archive Optional character string giving the path of an archive
#'   containing \code{targetFile}, or a vector giving a set of nested archives
#'   (e.g., \code{c("xxx.tar", "inner.zip", "inner.rar")}). If there is/are (an) inner
#'   archive(s), but they are unknown, the function will try all until it finds
#'   the \code{targetFile}. See table in \code{\link{preProcess}}.
#'
#' @param url Optional character string indicating the URL to download from.
#'   If not specified, then no download will be attempted. If not entry
#'   exists in the \code{CHECKSUMS.txt} (in \code{destinationPath}), an entry
#'   will be created or appended to. This \code{CHECKSUMS.txt} entry will be used
#'   in subsequent calls to
#'   \code{prepInputs} or \code{preProcess}, comparing the file on hand with the ad hoc
#'   \code{CHECKSUMS.txt}. See table in \code{\link{preProcess}}.
#'
#' @param alsoExtract Optional character string naming files other than
#'   \code{targetFile} that must be extracted from the \code{archive}. If
#'   \code{NULL}, the default, then it will extract all files. Other options:
#'   \code{"similar"} will extract all files with the same filename without
#'   file extension as \code{targetFile}. \code{NA} will extract nothing other
#'   than \code{targetFile}. A character string of specific file names will cause
#'   only those to be extracted. See table in \code{\link{preProcess}}.
#'
#' @param destinationPath Character string of a directory in which to download
#'   and save the file that comes from \code{url} and is also where the function
#'   will look for \code{archive} or \code{targetFile}. NOTE (still experimental):
#'   To prevent repeated downloads in different locations, the user can also set
#'   \code{options("reproducible.inputPaths")} to one or more local file paths to
#'   search for the file before attempting to download. Default for that option is
#'   \code{NULL} meaning do not search locally.
#'
#' @param fun Function or character string indicating the function to use to load
#'   \code{targetFile} into an \code{R} object, e.g., in form with package name:
#'   \code{"raster::raster"}.
#'
#' @param quick Logical. This is passed internally to \code{\link{Checksums}}
#'   (the quickCheck argument), and to
#'   \code{\link{Cache}} (the quick argument). This results in faster, though
#'   less robust checking of inputs. See the respective functions.
#'
#' @param purge Logical or Integer. \code{0/FALSE} (default) keeps existing
#'    \code{CHECKSUMS.txt} file and
#'    \code{prepInputs} will write or append to it. \code{1/TRUE} will deleted the entire
#'    \code{CHECKSUMS.txt} file. Other options, see details.
#'
#' @param overwrite Logical. Should downloading and all the other actions occur
#'   even if they pass the checksums or the files are all there.
#'
#' @param ... Additional arguments passed to \code{fun} (i.e,. user supplied),
#'   \code{\link{postProcess}} and \code{\link[reproducible]{Cache}}.
#'  Since \code{...} is passed to \code{\link{postProcess}}, these will
#'  \code{...} will also be passed into the inner
#'  functions, e.g., \code{\link{cropInputs}}. See details and examples.
#'
#' @param useCache Passed to Cache in various places. Defaults to \code{getOption("reproducible.useCache")}
#' 
#' @return RasterStack
#'
#' @author Tati Micheletti
#' @export
#' @importFrom reproducible prepInputs postProcess
#' @importFrom raster nlayers stack
#' 
#' @rdname prepInputStack

prepInputStack <- function(...){
  
  dots <- list(...)
  message("prepInput a raster stack...")
  stackLayers <- reproducible::prepInputs(archive = dots$archive,
                                           url = dots$url,
                                          targetFile = dots$targetFile,
                                          alsoExtract = dots$alsoExtract,
                                           destinationPath = dots$destinationPath, 
                                           fun = "raster::stack")
  postProcessedLayers <- lapply(X = seq_len(nlayers(stackLayers)), FUN = function(layer){
    lay <- reproducible::postProcess(stackLayers[[layer]], studyArea = dots$studyArea, 
                       rasterToMatch = dots$rasterToMatch, destinationPath = dots$destinationPath,
                       filename2 = dots$filename2)
    names(lay) <- names(stackLayers[[layer]])
    return(lay)
  })
  postProcessedLayers <- raster::stack(postProcessedLayers)
  names(postProcessedLayers) <- names(stackLayers) # Added this one for computers that can't process in memory and have to write a temporary file

    return(postProcessedLayers)
}
