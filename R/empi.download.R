#' Downloading the required external program
#'
#' Downloads \emph{Enhanced Matching Pursuit Implementation} external program (or \emph{empi} for short).
#'
#' @details
#' \emph{empi} is a fast implementation of Matching Pursuit algorithm. The program uses both CPU
#' parallelization and GPU devices. See README.md for more details on this implementation.
#' The program (sources and binaries for different OSs) is available for download from
#' \url{https://github.com/develancer/empi}. Details are presented in a journal paper: Różański, P.T. (2024).
#' \emph{empi: GPU-Accelerated Matching Pursuit with Continuous Dictionaries}.
#' ACM Transactions on Mathematical Software, Volume 50, Issue 3, Article No. 17, pp. 1-17,
#' \doi{10.1145/3674832}.
#'
#' @return The function downloads the \emph{empi} program in a version compatible with the operating
#' system used (Windows, Linux, MacOS-x64, MacOS-arm64) and saves it in the current directory.
#'
#' @examples
#' ## Not run:
#' if (interactive()) {
#'   empi.download()
#' }
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
