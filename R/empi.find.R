#' Checks if EMPI external software is installed
#'
#' The EMPI program is installed using the \code{empi.install()} function and is loaded into the
#' cache directory. This function checks whether the EMPI program is still there (the user
#' has free access to the cache directory and can, for example, delete it).
#'
#' @return
#' If the EMPI program is found, it returns its full path. Otherwise, it displays a message
#' to the user asking them to install it using the \code{empi.install()} function.
#'
#' @export
#'
#' @examples
#' ## Not run:
#' empi.find()
#' ## End(Not run)
#'
empi.find <- function() {
  cache_dir <- file.path(tools::R_user_dir("MatchingPursuit", "cache"))
  cache_dir <- normalizePath(cache_dir, winslash = "/")

  sys <- Sys.info()[["sysname"]]

  exec.path <- if (sys == "Windows") {
    file.path(cache_dir, "empi.exe")
  } else {
    file.path(cache_dir, "empi")
  }

  if (file.exists(exec.path)) {
      return(exec.path)
  }

  stop(
    "\n\nThe 'EMPI' tool is not available.\n",
    "Run empi.install() to install it.\n\n"
  )
}

