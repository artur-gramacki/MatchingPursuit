#' Clear MatchingPursuit Cache
#'
#' Deletes all files in the MatchingPursuit cache directory.
#'
#' @return Logical scalar. Returns \code{TRUE} if all files were successfully removed,
#' and \code{FALSE} otherwise. The return value is invisible.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   clear_cache()
#' }
clear_cache <- function() {

  cache_dir <- tools::R_user_dir("MatchingPursuit", "cache")

  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist: '", cache_dir, "'.")
    return(invisible(FALSE))
  }

  files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
  files2 <- list.files(cache_dir, full.names = FALSE, recursive = TRUE)

  if (length(files) == 0) {
    message("No files to remove in: '", cache_dir, "'.")
    return(invisible(TRUE))
  }

  cat("Files found in cache:\n")
  cat(paste0(" - ", files2), sep = "\n")

  answer <- readline(prompt = paste0("Found ", length(files), " file(s) in cache. Delete them? [y/N]: "))


  if (!tolower(answer) %in% c("y", "yes")) {
    message("Operation cancelled by user.")
    return(invisible(FALSE))
  }

  ok <- file.remove(files)

  message("Removed ", sum(ok), " file(s) from cache.")
  if (any(!ok)) {
    warning("Some files could not be removed.")
  }

  return(invisible(all(ok)))
}
