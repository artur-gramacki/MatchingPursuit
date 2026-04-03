#' Clear MatchingPursuit Cache
#'
#' Deletes all files in the MatchingPursuit cache directory.
#'
#' @return Logical scalar. TRUE if all files were successfully removed, FALSE otherwise.
#' The return value is invisible.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   clear.cache()
#' }
clear.cache <- function() {

  cache.dir <- tools::R_user_dir("MatchingPursuit", "cache")

  if (!dir.exists(cache.dir)) {
    message("Cache directory does not exist: '", cache.dir, "'.")
    return(invisible(FALSE))
  }

  files <- list.files(cache.dir, full.names = TRUE, recursive = TRUE)

  if (length(files) == 0) {
    message("No files to remove in: '", cache.dir, "'.")
    return(invisible(TRUE))
  }

  ok <- file.remove(files)

  message("Removed ", sum(ok), " file(s) from cache.")
  if (any(!ok)) {
    warning("Some files could not be removed.")
  }

  return(invisible(all(ok)))
}
