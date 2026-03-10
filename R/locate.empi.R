#' Get required external software localization
#'
#' @description
#' Returns \emph{Enhanced Matching Pursuit Implementation} \href{https://github.com/develancer/empi}{empi} binary locations for the following operation systems: Windows, Linux, MacOS-x64, MacOS-arm64.
#'
#' @return URL links and zip file names.
#'
#' @importFrom utils download.file unzip
#'
#' @export
#'
#' @examples
#' locate.empi()
#'
locate.empi <- function() {
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
    stop("--> Sorry. Unsupported OS. <--")
  }
  list(url = url, fname = fname)
}
