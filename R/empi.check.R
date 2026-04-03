#' Checks if EMPI external software is installed
#'
#' The EMPI program is installed using the \code{empi.install()} function and is stored in the
#' cache directory. This function checks whether the EMPI program is still there (the user has
#' free access to the cache directory and can, for example, delete it at any time).
#'
#' @return
#' If the EMPI program is found, its full path is returned. Otherwise, a message is displayed,
#' prompting the user to install it using the \code{empi.install()} function.
#'
#' @export
#'
#' @examples
#' empi.check()
#'
empi.check <- function() {
  dest.dir <- file.path(tools::R_user_dir("MatchingPursuit", "cache"), "empi")

  sys <- Sys.info()[["sysname"]]

  exec.path <- if (sys == "Windows") {
    file.path(dest.dir, "empi.exe")
  } else {
    file.path(dest.dir, "empi")
  }

  if (file.exists(exec.path)) {
      return(exec.path)
  }

  message("The EMPI tool is not available. Run empi.install() to install it.")
}

