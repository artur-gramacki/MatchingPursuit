#' Get required external software localization
#'
#' @description
#' Returns \strong{Enhanced Matching Pursuit Implementation} binary locations for
#' the following operating systems: Windows, Linux, macOS-arm64.
#'
#' @return
#' A list containing:
#' \itemize{
#'   \item \code{url}: URL of the EMPI binary archive,
#'   \item \code{fname}: archive file name.
#' }
#'
#' @seealso
#' \code{\link{empi_check}},
#' \code{\link{empi_install}},
#' \code{\link{empi_execute}},
#' \code{\link{plot.mp}}
#'
#' @export
#'
#' @examples
#' empi_locate()
#'
empi_locate <- function() {

  sys <- Sys.info()[["sysname"]]
  mach <- Sys.info()[["machine"]]

  if (sys == "Windows") {

    url <- "https://github.com/develancer/empi/releases/download/1.0.4/empi-1.0.4-windows-x64.zip"
    fname <- "empi-1.0.4-windows-x64.zip"

  } else if (sys == "Linux") {

    url <- "https://github.com/develancer/empi/releases/download/1.0.4/empi-1.0.4-linux-x64.zip"
    fname <- "empi-1.0.4-linux-x64.zip"

  } else if (sys == "Darwin") {

    if (mach != "arm64") {
      stop("Only Apple Silicon Macs (arm64) are supported. ", "Detected architecture: ", mach)
    }

    url <- "https://github.com/develancer/empi/releases/download/1.0.4/empi-1.0.4-macos-arm64.zip"
    fname <- "empi-1.0.4-macos-arm64.zip"

  } else {
    stop("Unsupported operating system: ", sys)
  }

  list(
    url = url,
    fname = fname
  )
}
