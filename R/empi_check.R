#' Checks if EMPI external software is installed
#'
#' The EMPI program is installed using the \code{empi_install()} function and stored in the
#' cache directory. This function checks whether the EMPI program is still available there
#' (users have full access to the cache directory and may remove its contents at any time).
#'
#' @return
#' If the EMPI program is found, its full path is returned. Otherwise, a message is displayed,
#' prompting the user to install it using the \code{empi_install()} function.
#'
#' @export
#'
#' @examples
#' empi_check()
#'
empi_check <- function() {

  dest_dir <- file.path(tools::R_user_dir("MatchingPursuit", "cache"), "empi")

  sys <- Sys.info()[["sysname"]]

  exec_path <- if (sys == "Windows") {
    file.path(dest_dir, "empi.exe")
  } else {
    file.path(dest_dir, "empi")
  }

  if (file.exists(exec_path)) {
    return(exec_path)
  }

  message("The EMPI tool is not available. Run empi_install() to install it.")
}
