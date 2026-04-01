#' Get required external software localization
#'
#' @description
#' Returns \strong{Enhanced Matching Pursuit Implementation} binary locations for
#' the following operation systems: Windows, Linux, MacOS-x64, MacOS-arm64.
#'
#' @return List with URL of the EMPI binaries and zip file name.
#'
#' @export
#'
#' @examples
#' empi.locate()
#'
empi.locate <- function() {
  sys <- Sys.info()[["sysname"]]
  mach <- Sys.info()[["machine"]]

  if (sys == "Windows") {
    url <- "https://github.com/develancer/empi/releases/download/1.0.3/empi-1.0.3-windows-x64.zip"
    fname <- "empi-1.0.3-windows-x64.zip"
  } else if (sys == "Linux") {
    url <- "https://github.com/develancer/empi/releases/download/1.0.3/empi-1.0.3-linux-x64.zip"
    fname <- "empi-1.0.3-linux-x64.zip"
  } else if (sys == "Darwin") {
    if (mach == "arm64") {
      url <- "https://github.com/develancer/empi/releases/download/1.0.3/empi-1.0.3-macos-arm64.zip"
      fname <- "empi-1.0.3-macos-arm64.zip"
    } else if (mach  == "x86_64") {
      url <- "https://github.com/develancer/empi/releases/download/1.0.3/empi-1.0.3-macos-x64.zip"
      fname <- "empi-1.0.3-macos-x64.zip"
    }
  } else {
    stop("Sorry. Unsupported OS.")
  }
  list(url = url, fname = fname)
}
