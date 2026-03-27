#' Installs the required external program
#'
#' Downloads \emph{Enhanced Matching Pursuit Implementation} external program (or EMPI for short)
#' and stores it in the cache directory (if \code{dest.dir=NULL}) or in a directory specified by the user.
#'
#' @param dest.dir User-specified directory where the external EMPI program is to be stored.
#'
#' @return The function downloads the EMPI program in a version compatible with the operating
#' system used (Windows, Linux, MacOS-x64, MacOS-arm64) and stores it in the cache or user-specified directory.
#'
#' @importFrom utils download.file unzip
#'
#' @export
#'
#' @examples
#' ## Not run:
#' empi.install()
#' ## End(Not run)
#'
empi.install <- function(dest.dir = NULL) {

  out <-  empi.locate()

  if (is.null(dest.dir)) {
    dest.dir <- file.path(tools::R_user_dir("MatchingPursuit", "cache"))
    dest.dir <- normalizePath(dest.dir, winslash = "/")
    dir.create(dest.dir, recursive = TRUE, showWarnings = FALSE)
    archive <- paste(dest.dir, "/", out$fname, sep = "")
  } else {
    archive <- paste(dest.dir, "/", out$fname, sep = "")
  }

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

  message("Downloading 'EMPI' for ", sys, " ", mach, "...")

  download.file(url = out$url,  destfile = archive, mode = "wb")
  check.checksum(archive)

  message("Extracting 'EMPI' to '", dest.dir, "' ...")
  unzip(zipfile = archive, exdir = dest.dir, junkpaths = TRUE)

  # chmod na Unix
  if (sys != "Windows") {
    exec <- file.path(dest.dir, "empi")
    Sys.chmod(exec, "0755")
  }

  message("Installation complete. 'EMPI' program is in '", dest.dir, "'")

}

