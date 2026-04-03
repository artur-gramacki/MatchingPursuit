#' Installs the required external program
#'
#' Downloads \strong{Enhanced Matching Pursuit Implementation} external program (or EMPI for short)
#' and stores it in the cache directory.
#'
#' @return The function downloads the EMPI program in a version compatible with the operating
#' system used (Windows, Linux, MacOS-x64, MacOS-arm64) and stores it in the cache directory.
#'
#' @importFrom utils download.file unzip
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   empi.install()
#' }
#'
empi.install <- function() {

  out <-  empi.locate()

  dest.dir <- file.path(tools::R_user_dir("MatchingPursuit", "cache"), "empi")
  dir.create(dest.dir, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(dest.dir, out$fname)

  sys <- Sys.info()[["sysname"]]
  mach <- Sys.info()[["machine"]]

  files <- list.files(dest.dir)
  if (sys == "Windows") {
    if ("empi.exe" %in% files) {
      message("It looks like EMPI is already installed in the '", dest.dir, "' directory.")
      return(invisible(NULL))
    }
  } else {
    if ("empi" %in% files) {
      message("It looks like EMPI is already installed in the '", dest.dir, "' directory.")
      return(invisible(NULL))
    }
  }

  message("Downloading EMPI (third-party software) for ", sys, " ", mach, "...")

  download.file(url = out$url,  destfile = archive, mode = "wb")
  check.checksum(archive)

  message("Extracting EMPI to '", dest.dir, "' ...")
  unzip(zipfile = archive, exdir = dest.dir, junkpaths = TRUE)

  # chmod na Unix
  if (sys != "Windows") {
    exec <- file.path(dest.dir, "empi")
    Sys.chmod(exec, "0755")
  }

  message("Installation complete. EMPI program is in '", dest.dir, "'")

}

