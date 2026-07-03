#' Get required external software localization
#'
#' @description
#' Returns \strong{Enhanced Matching Pursuit Implementation} binary locations for
#' the following operating systems: Windows, Linux, macOS-x64, macOS-arm64.
#'
#' @return List with URL of the EMPI binaries and zip file name.
#'
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

    url <- "https://github.com/develancer/empi/releases/download/1.0.4/empi-1.0.4-macos-arm64.zip"
    fname <- "empi-1.0.4-macos-arm64.zip"

  } else {
    stop("Sorry. Unsupported OS.")
  }

  list(
    url = url,
    fname = fname
  )
}
