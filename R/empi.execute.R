#' Launching the Matching Pursuit algorithm
#'
#' Runs the \emph{empi} program for the given data.
#'
#' @details
#' The \emph{empi} program (source and binary files for various operating systems) can be
#' downloaded from \url{https://github.com/develancer/empi}. Details are presented in the
#' journal paper: Różański, P.T. (2024). \emph{empi: GPU-Accelerated Matching Pursuit with
#' Continuous Dictionaries}.ACM Transactions on Mathematical Software, Volume 50, Issue 3,
#' Article No. 17, pp. 1-17, \doi{10.1145/3674832}.
#'
#' @param signal Must be a data frame: rows = samples for all channels, columns = channels.
#' The data frame must have column names (channel names).
#'
#' @param sampling.rate Sampling rate of the given signal (it is assumed to be the same for
#' each channel).
#'
#' @param empi.options If \code{NULL}, the \emph{empi} program runs with
#' \code{"-o local --gabor -i 50"} parameters. Otherwise, user can specify any command-line
#' options. See \code{README.md} file after downloading the \emph{empi} program using
#' \code{empi.download()} function.
#'
#' @param  write.to.file If \code{TRUE}, a SQLite database file \code{empi.db} will be created
#' and saved in the current directory. This file stores the results of signal decomposition
#' using the MP algorithm.
#'
#' @return Signal decomposition results using the MP algorithm. If \code{write.to.file=TRUE}
#' is specified, the results are additionally written to the \code{empi.db} file on disk
#' \code{empi.db} in the working directory.
#'
#' @export
#'
#' @examples
#' ## Not run:
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#' signal <- read.csv.signals(file)
#'
#' if (interactive()) {
#'   empi.out <- empi.execute (
#'     signal = signal,
#'     sampling.rate = 1024,
#'     empi.options = NULL,
#'     write.to.file = FALSE
#'   )
#' }
#' ## End(Not run)
#'
empi.execute <- function(signal, sampling.rate, empi.options = NULL, write.to.file = FALSE) {

  empi.loc <-  locate.empi()
  dir.name <- tools::file_path_sans_ext((empi.loc$fname))

  n.channels <- ncol(signal)

  signal.raw <- sig2bin(data = signal, write.to.file = FALSE)

  file.bin <- tempfile("file_for_empi_", fileext = ".bin")
  file.db <- tempfile("file__for_empi_", fileext = ".db")
  writeBin(signal.raw, file.bin)

  file.db <- tempfile("file__for_empi_", fileext = ".db")

  if (is.null(empi.options)) {
    options <-  "-o local --gabor -i 50"
  } else {
    options <- empi.options
  }

  if (!dir.exists(dir.name)) {
    if (interactive()) {
      cat(
        "###################### Important note ######################", "\n",
        "Downloading an external tool from the internet is required.", "\n",
        "Details can be found on the `empi.download `help page.", "\n",
        "###################### Important note ######################", "\n",
        sep = "")
      ans <- readline("Do you agree? (y/n): ")
      if (tolower(ans) != "y") {
        stop("\n--> Operation aborted by user. <--")
      } else {
        empi.download()
      }
    }
  }

  sys <- Sys.info()[["sysname"]]
  if (sys == "Windows") {
    exec = "empi.exe"
  } else {
    exec = "empi"
    Sys.chmod(paste(dir.name, "/empi", sep = ""), mode = "0755")
  }

  command <- paste(
    dir.name,
    "/",
    exec,
    " ",
    file.bin,
    " ",
    file.db,
    " ",
    "-f ",
    sampling.rate,
    " -c ",
    n.channels,
    " --channels 1-",
    n.channels,
    " ",
    options,
    sep = "")

  system(command)

  if (write.to.file) {
    file.copy(file.db, "empi.db", overwrite = TRUE)
    cat("\n--> Note: results were also saved in the 'empi.db' file in the current directory (in SQLite format). <--\n")
  }

  out <- read.empi.db.file(file.db)

  file.remove(file.bin, file.db)

  return(out)

}
