#' Downloading the required software
#'
#' Downloads \emph{Enhanced Matching Pursuit Implementation} program \href{https://github.com/develancer/empi}{empi}.
#'
#' @details
#' \emph{empi} is a fast implementation of Matching Pursuit algorithm. The program uses both CPU parallelization and GPU
#' devices. See README.md for more details on this implementation.
#' The program (sources and binaries for different OSs) is available for download at
#' \href{https://github.com/develancer/empi}{empi}. Details are presented in a journal paper: Różański, P.T. (2024).
#' \emph{empi: GPU-Accelerated Matching Pursuit with Continuous Dictionaries}. ACM Transactions on Mathematical Software,
#'  vol.50, issue = 3, pp. 1-17, \url{https://doi.org/10.1145/3674832}.
#'
#' @return The function downloads the empi program in a version compatible with the operating system used (Windows, Linux, MacOS-x64, MacOS-arm64) and saves it in the current directory.
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
