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
  cache_dir <- tools::R_user_dir("MatchingPursuit", "cache")

  sys <- Sys.info()[["sysname"]]

  exec.path <- if (sys == "Windows") {
    file.path(cache_dir, "empi.exe")
  } else {
    file.path(cache_dir, "empi")
  }

  if (file.exists(exec.path)) {
      return(exec.path)
  }

  # Moved this stop() to empi.execute()
  # stop(
  #   "The 'EMPI' tool is not available. ",
  #   "Run empi.install() to install it."
  # )
  return(NULL)
}

