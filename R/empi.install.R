#' Installs the EMPI external program
#'
#' Downloads \strong{Enhanced Matching Pursuit Implementation} external program
#' (or EMPI for short) binary compatible with the current operating system and
#' stores it in the package cache directory.
#'
#' The function detects the operating system (Windows, Linux, macOS x64,
#' macOS arm64), downloads the appropriate archive from the official
#' repository, verifies its integrity using a checksum, and extracts it.
#'
#' @return The function downloads the EMPI program in a version compatible
#' with the operating system used (Windows, Linux, MacOS-x64, MacOS-arm64)
#' and stores it in the package cache directory.
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

  bin.exists <- if (sys == "Windows") {
    file.exists(file.path(dest.dir, "empi.exe"))
  } else {
    file.exists(file.path(dest.dir, "empi"))
  }

  if (bin.exists) {
    message("EMPI already installed in: '", dest.dir, "' directory.")
    return(invisible(dest.dir))
  }

  message("Downloading EMPI (third-party software) for ", sys, " ", mach, "...")

  ok <- tryCatch({
    download.file(url = out$url, destfile = archive, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) {
        message("Download failed: ", e$message)
        FALSE
    }
  )

  if (!ok) stop("Installation aborted due to download failure.")

  if (!file.exists(archive)) {
    stop("Downloaded archive not found.")
  }

  check.checksum(archive)

  message("Extracting EMPI to '", dest.dir, "' ...")
  unzip(zipfile = archive, exdir = dest.dir, junkpaths = TRUE)

  # chmod na Unix
  if (sys != "Windows") {
    exec <- file.path(dest.dir, "empi")
    Sys.chmod(exec, "0755")
  }

  message("Installation complete. EMPI program is in '", dest.dir, "' directory.")

  invisible(dest.dir)

}

