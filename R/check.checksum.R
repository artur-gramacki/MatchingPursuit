#' @keywords internal
#'
#' @importFrom digest digest
#'
#' @noRd
#'
check.checksum <- function (dest) {

  checksums <- c(
    "empi-1.0.3-windows-x64.zip" = "8bdb556d8f362cc3d4885ea203a620f1",
    "empi-1.0.3-linux-x64.zip"   = "b3ba3c6c6444d0358b74680cdfff8386",
    "empi-1.0.3-macos-x64.zip"   = "f77a9f6631bd006d876ba60fa572a089",
    "empi-1.0.3-macos-arm64.zip" = "f7321e57abed99076546762f104b2014"
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
