#' @keywords internal
#'
#' @importFrom digest digest
#'
#' @noRd
#'
check_checksum <- function (dest) {

  checksums <- c(
    "empi-1.0.4-windows-x64.zip" = "8f232a33a73ea9f00f4fc92025f2aef1",
    "empi-1.0.4-linux-x64.zip"   = "1cabb00fe37db7e2e9fc175004e03871",
    "empi-1.0.4-macos-arm64.zip" = "f284baf481faabbceea4f0d44c8a749a"
  )

  fname <- basename(dest)

  if (!fname %in% names(checksums)) {
    stop("Unknown file name: cannot verify checksum.")
  }

  hash <- digest::digest(file = dest, algo = "md5")

  if (hash != checksums[[fname]]) {
    stop("Checksum does not match! The program file could not be downloaded.")
  }

  message("Checksum is correct.")

  invisible(TRUE)
}
