#' Resample a signal
#'
#' Resamples one or more numeric signals using
#' \code{signal::resample()}.
#'
#' The new sampling frequency is determined by the ratio \code{p/q}:
#'
#' \deqn{
#' f_{\mathrm{new}} = f_{\mathrm{old}} \frac{p}{q}.
#' }
#'
#' For matrices and data frames, resampling is performed independently for
#' each column. Rows are interpreted as time samples and columns as individual
#' signal channels.
#'
#' The function uses \code{\link[signal]{resample}} internally. The resampling
#' process includes interpolation, low-pass filtering, and decimation.
#'
#' @param signal A numeric vector, numeric matrix, or data frame containing
#'   only numeric columns. For two-dimensional objects, rows correspond to
#'   time samples and columns correspond to signal channels.
#' @param p A positive integer specifying the interpolation factor.
#' @param q A positive integer specifying the decimation factor.
#' @param d A positive integer specifying the filter delay. The default is 5.
#'
#' @return A numeric vector, matrix, or data frame containing the resampled
#'   signal. The output type matches the input type. Column names are preserved
#'   for matrices and data frames.
#'
#' @examples
#' # Numeric vector
#' signal <- sin(2 * pi * 5 * seq(0, 1, length.out = 400))
#' signal_resampled <- resample_signal(signal, p = 1, q = 4)
#'
#' old.par <- par("mfrow", "mai")
#' par(mfrow = c(2, 1))
#' par(mai = c(0.9, 0.5, 0.3, 0.4))
#'
#' plot(signal, type = "o")
#' plot(signal_resampled, type = "o")
#'
#' par(old.par)
#'
#' # Numeric matrix: samples in rows, channels in columns (256Hz, 10sec., 5 channels)
#' signal <- matrix(rnorm(2560 * 5), nrow = 2560, ncol = 5)
#' colnames(signal) <- paste0("channel_", seq_len(ncol(signal)))
#'
#' # Resample to 64Hz
#' signal_64 <- resample_signal(signal, p = 1, q = 4)
#' dim(signal_64)
#'
#' # Data frame
#' signal_df <- as.data.frame(signal)
#' signal_df_64 <- resample_signal(signal_df, p = 1, q = 4)
#' names(signal_df_64)
#'
#' @export
resample_signal <- function(signal, p, q, d = 5) {

  validate_positive_integer <- function(x, name) {
    if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x <= 0 || x != as.integer(x)) {
      stop(sprintf("`%s` must be a single positive integer.", name), call. = FALSE)
    }
  }

  validate_positive_integer(p, "p")
  validate_positive_integer(q, "q")
  validate_positive_integer(d, "d")

  p <- as.integer(p)
  q <- as.integer(q)
  d <- as.integer(d)

  # Numeric vector
  if (is.null(dim(signal)) && !is.data.frame(signal)) {
    if (!is.numeric(signal)) {
      stop("`signal` must be numeric.", call. = FALSE)
    }

    if (length(signal) == 0L) {
      stop("`signal` must not be empty.", call. = FALSE)
    }

    return(signal::resample(x = signal, p = p, q = q, d = d))
  }

  # Data frame
  if (is.data.frame(signal)) {
    if (nrow(signal) == 0L || ncol(signal) == 0L) {
      stop("`signal` must not be empty.", call. = FALSE)
    }

    numeric_columns <- vapply(signal, is.numeric, logical(1))

    if (!all(numeric_columns)) {
      stop("`signal` must contain only numeric columns.", call. = FALSE)
    }

    original_names <- names(signal)
    signal_matrix <- as.matrix(signal)

    resampled <- lapply(
      seq_len(ncol(signal_matrix)),
      function(j) {
        signal::resample(x = signal_matrix[, j], p = p, q = q, d = d)
      }
    )

    result <- as.data.frame(
      do.call(cbind, resampled),
      optional = TRUE
    )

    names(result) <- original_names
    rownames(result) <- NULL

    return(result)
  }

  # Numeric matrix
  if (is.matrix(signal)) {
    if (!is.numeric(signal)) {
      stop("`signal` must be a numeric matrix.", call. = FALSE)
    }

    if (nrow(signal) == 0L || ncol(signal) == 0L) {
      stop("`signal` must not be empty.", call. = FALSE)
    }

    original_colnames <- colnames(signal)

    resampled <- lapply(
      seq_len(ncol(signal)),
      function(j) {
        signal::resample(x = signal[, j], p = p, q = q, d = d)
      }
    )

    result <- do.call(cbind, resampled)
    colnames(result) <- original_colnames

    return(result)
  }

  stop("`signal` must be a numeric vector, matrix, or data frame.", call. = FALSE)
}
