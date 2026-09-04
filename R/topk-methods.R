#' Methods for Top-k Objects
#'
#' Print and summarize objects returned by \code{topk_gabor_atoms()}.
#'
#' @name topk-methods
#' @seealso \code{\link{topk_gabor_atoms}}
#'
#' @examples
#' signal <- read_csv_signals(system.file("extdata", "sample1.csv", package = "MatchingPursuit"))
#' xml_file <- system.file("extdata", "sample1.xml", package = "MatchingPursuit")
#'
#' dictionary <- read_gabor_dict(
#'   xml_file = xml_file,
#'   sampling_frequency = signal$sampling_frequency,
#'   duration = max(signal$time)
#' )
#'
#' out_topk <- topk_gabor_atoms(
#'   atoms_dict = dictionary,
#'   signal = signal,
#'   topk = 100
#' )
#'
#' print(out_topk)
#' summary(out_topk)
NULL


#' @rdname topk-methods
#'
#' @param x An object of class \code{"topk"}.
#' @param ... Additional arguments, currently ignored.
#'
#' @return \code{print.topk()} returns \code{x} invisibly.
#'
#' @export
print.topk <- function(x, ...) {

  n_channels <- length(x$atoms)

  if (n_channels > 0L) {
    n_selected <- ncol(x$atoms[[1]])
    signal_length <- nrow(x$atoms[[1]])
  } else {
    n_selected <- 0L
    signal_length <- 0L
  }

  n_candidates <- nrow(x$inner_products)

  cat("Top-k Gabor atoms object (class 'topk')\n")
  cat("-----------------------------------------\n")
  cat("Candidate atoms:  ", n_candidates, "\n", sep = "")
  cat("Selected atoms:   ", n_selected, " per channel\n", sep = "")
  cat("Signal channels:  ", n_channels, "\n", sep = "")
  cat("Signal length:    ", signal_length, " samples\n", sep = "")

  invisible(x)
}

#' @rdname topk-methods
#'
#' @param object An object of class \code{"topk"}.
#'
#' @return
#' \code{summary.topk()} returns an object of class \code{"summary.topk"}
#' containing basic information about the selected atoms and ranges of their
#' Gabor parameters.
#'
#' @export
summary.topk <- function(object, ...) {

  n_channels <- length(object$atoms)

  if (n_channels > 0L) {
    n_selected <- ncol(object$atoms[[1]])
    signal_length <- nrow(object$atoms[[1]])
  } else {
    n_selected <- 0L
    signal_length <- 0L
  }

  n_candidates <- nrow(object$inner_products)

  safe_range <- function(x) {
    if (length(x) == 0L) return(c(NA_real_, NA_real_))
    x <- x[is.finite(x)]
    if (length(x) == 0L) return(c(NA_real_, NA_real_))
    range(x)
  }

  out <- list(
    n_candidates = n_candidates,
    n_selected = n_selected,
    n_channels = n_channels,
    signal_length = signal_length,
    frequency_range = safe_range(object$frequency),
    phase_range = safe_range(object$phase),
    scale_range = safe_range(object$scale),
    position_range = safe_range(object$position),
    atom_begin_range = safe_range(object$atom_begin),
    window_len_range = safe_range(object$window_len)
  )

  class(out) <- "summary.topk"
  out
}

#' @rdname topk-methods
#'
#' @param x An object of class \code{"summary.topk"}.
#'
#' @return \code{print.summary.topk()} returns \code{x} invisibly.
#'
#' @export
print.summary.topk <- function(x, ...) {

  cat("Summary of Top-k Gabor atoms object (class 'summary.topk')\n")
  cat("----------------------------------------------------------\n")
  cat("Candidate atoms:  ", x$n_candidates, "\n", sep = "")
  cat("Selected atoms:   ", x$n_selected, " per channel\n", sep = "")
  cat("Signal channels:  ", x$n_channels, "\n", sep = "")
  cat("Signal length:    ", x$signal_length, " samples\n", sep = "")

  if (all(is.finite(x$frequency_range))) {
    cat(
      "Frequency range:  ",
      format(x$frequency_range[1]),
      " - ",
      format(x$frequency_range[2]),
      " Hz\n",
      sep = ""
    )
  }

  if (all(is.finite(x$phase_range))) {
    cat(
      "Phase range:      ",
      format(x$phase_range[1]),
      " - ",
      format(x$phase_range[2]),
      " rad\n",
      sep = ""
    )
  }

  if (all(is.finite(x$scale_range))) {
    cat(
      "Scale range:      ",
      format(x$scale_range[1]),
      " - ",
      format(x$scale_range[2]),
      " s\n",
      sep = ""
    )
  }

  if (all(is.finite(x$position_range))) {
    cat(
      "Position range:   ",
      format(x$position_range[1]),
      " - ",
      format(x$position_range[2]),
      " s\n",
      sep = ""
    )
  }

  if (all(is.finite(x$atom_begin_range))) {
    cat(
      "Atom begin range: ",
      format(x$atom_begin_range[1]),
      " - ",
      format(x$atom_begin_range[2]),
      " s\n",
      sep = ""
    )
  }

  if (all(is.finite(x$window_len_range))) {
    cat(
      "Window length:    ",
      format(x$window_len_range[1]),
      " - ",
      format(x$window_len_range[2]),
      " s\n",
      sep = ""
    )
  }

  invisible(x)
}
