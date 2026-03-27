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
    url <- "https://drive.google.com/uc?export=download&id=1L1ZEBMW0dcb1-jCC3kur8VceijWogCJm"
    fname <- "empi-1.0.3-windows-x64.zip"
  } else if (sys == "Linux") {
    url <- "https://drive.google.com/uc?export=download&id=1_WPNnEeRGGzDQou763aWlQs3P1LBqzJk"
    fname <- "empi-1.0.3-linux-x64.zip"
  } else if (sys == "Darwin") {
    if (mach == "arm64") {
      url <- "https://drive.google.com/uc?export=download&id=1VAelJfQDghnKGyfiMCo5--uK3fIGF4WO"
      fname <- "empi-1.0.3-macos-arm64.zip"
    } else if (mach  == "x86_64") {
      url <- "https://drive.google.com/uc?export=download&id=1MKP7INoPglKvyLGPkkolFn0_juaspPy1"
      fname <- "empi-1.0.3-macos-x64.zip"
    }
  } else {
    stop("Sorry. Unsupported OS.")
  }
  list(url = url, fname = fname)
}
