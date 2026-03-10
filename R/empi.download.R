#' Downloads \emph{Enhanced Matching Pursuit Implementation} program \href{https://github.com/develancer/empi}{empi}.
#'
#' empi is a fast implementation of Matching Pursuit algorithm. The program uses both CPU parallelization and GPU devices. See README.md for more details on this implementation.
#'
#' @return The function downloads the empi program in a version compatible with the operating system used (Windows, Linux, MacOS-x64, MacOS-arm64) and saves it in the current directory.
#'
#' @examples
#' ## Not run:
#' empi.download()
#' ## End(Not run)
#'
#' @export
#'
empi.download <- function() {
  out <-  locate.empi()
  dest <- file.path(tempdir(), out$fname)
  download.file(
    url = out$url,
    destfile = dest,
    mode = "wb"
  )
  unzip(dest)
}
