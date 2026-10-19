#' Materialize a Gabor dictionary with fixed or atom-specific phases
#'
#' @param atoms_dict Parameter matrix returned by read_gabor_dict().
#'
#' @param sampling_frequency Sampling frequency in Hz.
#'
#' @param duration Signal duration in seconds.
#'
#' @param sigma_divisor Positive Gaussian-envelope scale divisor; default 3.
#'
#' @param phases Numeric vector of phases in radians. In "grid" mode each
#'   phase is used for every atom. In "per_atom" mode there must be exactly
#'   one phase per dictionary row.
#'
#' @param phase_mode Either "grid" (default, original behavior) or
#'   "per_atom" (one phase per atom).
#'
#' @return Dense numeric matrix with one column per materialized atom.
#'   In "grid" mode, degenerate columns are removed, as in the original
#'   implementation. In "per_atom" mode, a degenerate atom causes an error
#'   so that column positions always match rows of atoms_dict.
#'
#' @details
#' A Gabor atom is treated as degenerate when its cosine-modulated Gaussian
#' envelope is (numerically) zero before normalization. In particular, some
#' combinations of frequency, phase, and window length can make the sampled
#' cosine vanish at every sample. The implementation rejects an atom if its
#' norm is less than \code{1e-10 * sqrt(sum(w^2))}, where \code{w} is the
#' Gaussian envelope.
#'
#' A second, distinct case occurs when the atom has no usable samples within
#' the signal interval, for example when its entire support lies outside the
#' interval. Such an atom is rejected by the visible-fragment norm check.
#'
#' In \code{phase_mode = "grid"}, invalid atom columns are skipped; in
#' \code{phase_mode = "per_atom"}, an error is raised to preserve the
#' one-to-one correspondence between dictionary rows and output columns.
#'
#' @examples
#' fs <- 256
#' duration <- 2
#'
#' xml_file <- system.file(
#'   "extdata", "one_block.xml",
#'   package = "MatchingPursuit"
#' )
#'
#' atoms_dict <- read_gabor_dict(
#'   xml_file = xml_file,
#'   sampling_frequency = fs,
#'   duration = duration,
#'   full_atoms_in_signal = TRUE
#' )
#'
#' D <- materialize_gabor(
#'   atoms_dict = atoms_dict,
#'   sampling_frequency = fs,
#'   duration = duration,
#'   phases = 0,
#'   phase_mode = "grid"
#' )
#'
#' # Select five atoms with low frequencies, distributed across the signal
#' target_time <- seq(0.25, 1.75, length.out = 4)
#' target_freq <- c(4, 8, 12, 20)
#'
#' idx <- sapply(seq_along(target_time), function(i) {
#'   which.min(
#'     abs(atoms_dict[, "time_sec"] - target_time[i]) +
#'     abs(atoms_dict[, "freq_hz"] - target_freq[i]) / 10
#'   )
#' })
#'
#' matplot(
#'   (0:(nrow(D) - 1)) / fs,
#'    D[, idx, drop = FALSE],
#'    type = "l",
#'    lty = 1,
#'    xlab = "Time [s]",
#'    ylab = "Amplitude",
#'    main = "Four Gabor atoms"
#'   )
#'
#' @export
#'
materialize_gabor <- function(atoms_dict, sampling_frequency, duration,
                              sigma_divisor = 3, phases = 0,
                              phase_mode = c("grid", "per_atom")) {

  phase_mode <- match.arg(phase_mode)

  if (!is.matrix(atoms_dict) && !is.data.frame(atoms_dict)) {
    stop("'atoms_dict' must be a matrix or data frame.")
  }
  required <- c("window_len", "time_sample", "freq_hz")
  if (!all(required %in% colnames(atoms_dict))) {
    stop("'atoms_dict' must contain columns: ", paste(required, collapse = ", "))
  }
  if (length(sampling_frequency) != 1L || !is.finite(sampling_frequency) ||
      sampling_frequency <= 0 || length(duration) != 1L ||
      !is.finite(duration) || duration <= 0) {
    stop("'sampling_frequency' and 'duration' must be positive finite numbers.")
  }
  if (length(sigma_divisor) != 1L || !is.finite(sigma_divisor) ||
      sigma_divisor <= 0) {
    stop("'sigma_divisor' must be a positive finite number.")
  }
  if (!is.numeric(phases) || !length(phases) || any(!is.finite(phases))) {
    stop("'phases' must be a nonempty vector of finite numbers.")
  }

  N <- as.integer(round(sampling_frequency * duration))
  p <- nrow(atoms_dict)
  if (N < 1L || p < 1L) stop("Signal length and number of atoms must be positive.")

  if (phase_mode == "per_atom" && length(phases) != p) {
    stop("In 'per_atom' mode, length(phases) must equal nrow(atoms_dict).")
  }

  q <- if (phase_mode == "per_atom") 1L else length(phases)
  D <- matrix(0, nrow = N, ncol = p * q)
  valid <- rep(FALSE, p * q)

  for (j in seq_len(p)) {
    L <- as.integer(atoms_dict[j, "window_len"])
    t0 <- as.integer(atoms_dict[j, "time_sample"])
    freq <- as.numeric(atoms_dict[j, "freq_hz"])

    if (!is.finite(L) || L < 1L || !is.finite(t0) || !is.finite(freq)) {
      stop("Invalid Gabor parameters in row ", j, ".")
    }

    n <- seq.int(0L, L - 1L)
    sigma <- (L + 1) / sigma_divisor
    w <- exp(-pi * ((n - (L - 1) / 2) / sigma)^2)

    idx <- t0 + n
    inside <- idx >= 0 & idx < N
    rows <- idx[inside] + 1L

    phase_values <- if (phase_mode == "per_atom") phases[j] else phases

    for (h in seq_along(phase_values)) {
      col <- (j - 1L) * q + h
      atom <- w * cos(2 * pi * freq * n / sampling_frequency + phase_values[h])
      atom_norm <- sqrt(sum(atom^2))

      if (atom_norm < 1e-10 * sqrt(sum(w^2))) {
        if (phase_mode == "per_atom") stop("Degenerate atom in row ", j, ".")
        next
      }

      # Original convention: normalize complete atom first.
      atom <- atom / atom_norm
      atom_part <- atom[inside]
      norm <- sqrt(sum(atom_part^2))

      if (norm < 1e-12) {
        if (phase_mode == "per_atom") stop("No valid visible atom in row ", j, ".")
        next
      }

      # Original convention: normalize visible fragment again.
      D[rows, col] <- atom_part / norm
      valid[col] <- TRUE
    }
  }

  if (phase_mode == "grid") D <- D[, valid, drop = FALSE]
  D
}
