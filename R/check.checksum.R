#' @keywords internal
#' @noRd
#'
check.checksum <- function (dest) {

  digest::digest(file = dest, algo = "md5")


  md5_Windows =     "8bdb556d8f362cc3d4885ea203a620f1"
  md5_Linux =       "b3ba3c6c6444d0358b74680cdfff8386"
  md5_macos_x64 =   "f77a9f6631bd006d876ba60fa572a089"
  md5_macos_arm64 = "f7321e57abed99076546762f104b2014"

  hash <- digest(file = dest, algo = "md5")
  stop.txt <- "Checksum does not match! The program file could not be downloaded."

  if (out$fname == "empi-1.0.3-windows-x64.zip")
    if (hash != md5_Windows) stop(stop.txt)

  if (out$fname == "empi-1.0.3-linux-x64.zip")
    if (hash != md5_Linux) stop(stop.txt)

  if (out$fname == "empi-1.0.3-macos-x64.zip")
    if (hash != md5_macos_x64) stop(stop.txt)

  if (out$fname == "empi-1.0.3-macos-arm64.zip")
    if (hash != md5_macos_arm64) stop(stop.txt)

}
