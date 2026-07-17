#' Checks if EMPI external software is installed
#'
#' The EMPI program is installed using the \code{empi_install()} function and stored in the
#' cache directory. This function checks whether the EMPI program is still available there
#' (users have full access to the cache directory and may remove its contents at any time).
#'
#' @return
#' The full path to the EMPI executable if it is found. Otherwise, returns \code{NULL}
#' and displays a message suggesting installation using \code{empi_install()}.
#'
#' @seealso
#' \code{\link{empi_install}},
#' \code{\link{empi_locate}},
#' \code{\link{empi_execute}},
#' \code{\link{plot.mp}}
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   empi_check()
#' }
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

  message(
    "The EMPI tool is not installed. ",
    "Run empi_install() if you want to install it."
  )

  return(invisible(NULL))
}
