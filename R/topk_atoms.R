#' Select best Gabor atoms based on phase-invariant similarity
#'
#' This function constructs a sparse, signal-dependent Gabor dictionary by
#' selecting the most relevant atoms from a precomputed atom dictionary.
#'
#' In the first step, phase-invariant similarities between complex Gabor atoms
#' and the input signal are computed using cross-products. In the second step,
#' the top-ranked atoms are reconstructed with optimal phase alignment and
#' converted into real-valued time-domain signals. The resulting object is used
#' as input to \code{omp_core()} or to \code{omp_execute()}. This second function
#' is a wrapper around the first function. It is prepared in such a way that an
#' object of class \code{mp} is created as output. This allows it to be passed
#' to the \code{tf_map()} function, which creates a time-frequency map.
#'
#' @param atoms_dict A matrix describing Gabor atoms (e.g. output of
#'   \code{read_dict()}). Each row represents a candidate atom with fields:
#'   \code{time_sec}, \code{freq_hz}, \code{window_len}, etc.
#'
#' @param sig A numeric vector, matrix, or data frame representing the signal(s)
#'   to be analyzed. Each column is treated as a separate channel.
#'
#' @param sf Sampling frequency (Hz) of the signal.
#'
#' @param topk Number of best atoms to select per signal.
#'   If \code{NULL}, defaults to \code{ceiling(0.05 * nrow(atoms_dict))}.
#'
#' @param sigma_divisor Optional parameter controlling the width of the Gaussian
#'   window. Larger values produce narrower windows. If \code{NULL}, a default
#'   heuristic is used.
#'
#' @param verbose Logical; if \code{TRUE}, progress information is printed during
#'   both similarity computation and atom generation.
#'
#' @return An object of class \code{"topk"}, a list containing:
#'
#' \item{cross_products}{Matrix of phase-invariant similarities between atoms
#'   and signal(s).}
#' \item{topk_indices}{Indices of selected atoms for each signal channel.}
#' \item{atoms}{List of matrices containing reconstructed real-valued atoms
#'   (one matrix per signal).}
#' \item{frequency}{Frequency (Hz) of selected atoms.}
#' \item{phase}{Optimal phase values used for atom reconstruction.}
#' \item{scale}{Gaussian window scale (normalized sigma).}
#' \item{position}{Time position (center of atom in seconds).}
#' \item{atom_begin}{Start time of each atom (in seconds).}
#' \item{window_len}{Window length (in seconds).}
#'
#' @export
#'
#' @seealso
#' \code{\link{read_dict}},
#' \code{\link{omp_execute}}
#' \code{\link{omp_core}}
#'
#' @examples
#'
#' # +-------------------------------------------------------------+
#' # | Step 1: Read signal                                         |
#' # +-------------------------------------------------------------+
#' sig_file <- system.file(
#'   "extdata",
#'   "sample3.csv",
#'   package = "MatchingPursuit"
#' )
#'
#' sample3 <- read_csv_signals(
#'   sig_file,
#'   col_names_in_csv = TRUE
#' )
#'
#' sf <- sample3$sampling_frequency
#' signal <- sample3$signal
#' duration <- nrow(sample3$signal) / sf
#'
#' # +-------------------------------------------------------------+
#' # | Step 2: Read dictionary                                     |
#' # +-------------------------------------------------------------+
#' xml_file <- system.file(
#'   "extdata",
#'   "sample3_dict.xml",
#'   package = "MatchingPursuit"
#' )
#'
#' atoms_dict <- read_dict(
#'   xml_file,
#'   sf,
#'   duration,
#'   verbose = TRUE
#' )
#'
#' head(atoms_dict)
#' tail(atoms_dict)
#' nrow(atoms_dict)
#'
#'
#' # +-------------------------------------------------------------+
#' # | Step 3: Select top-k atoms most similar to the signal       |
#' # +-------------------------------------------------------------+
#' out_topk_atoms <- topk_atoms(
#'   atoms_dict = atoms_dict,
#'   sig = signal,
#'   sigma_divisor = NULL,
#'   sf = sf,
#'   topk = 5000
#' )
#'
#' class(out_topk_atoms)
#'
#' # +-------------------------------------------------------------+
#' # | Step 4.1                                                    |
#' # | Apply OMP to obtain a sparse representation of the signal   |
#' # +-------------------------------------------------------------+
#' # | Output: object of class 'mp'                                |
#' # | Processes: all signal channels (3 in this example)          |
#' # +-------------------------------------------------------------+
#' fit_1 <- omp_execute(
#'   dictionary = out_topk_atoms,
#'   signal = signal,
#'   sf = sf,
#'   n_nonzero_coefs = 50
#' )
#'
#' class(fit_1)
#'
#' # +-------------------------------------------------------------+
#' # | Step 4.2                                                    |
#' # | Apply OMP to obtain a sparse representation of the signal   |
#' # +-------------------------------------------------------------+
#' # | Output: list with atom parameters                           |
#' # | Processes: one selected channel                             |
#' # +-------------------------------------------------------------+
#' fit_2 <- omp_core(
#'   dictionary = out_topk_atoms,
#'   signal = signal,
#'   channel = 1,
#'   n_nonzero_coefs = 50
#' )
#'
#' # +-------------------------------------------------------------+
#' # | Step 5: Plot time-frequency representation                  |
#' # +-------------------------------------------------------------+
#' plot(fit_1, channel = 3)
#'
topk_atoms <- function(atoms_dict, sig, sf, topk = NULL, sigma_divisor = NULL, verbose = TRUE) {

  if (!is.matrix(sig)) {
    if (is.vector(sig) || is.data.frame(sig)) {
      sig <- as.matrix(sig)
    } else {
      stop("Parameter must be a matrix or convertible to a matrix")
    }
  }

  cp_mtx <- matrix(0, nrow = nrow(atoms_dict), ncol = ncol(sig))
  N <- nrow(sig)

  # By default select 5% best atoms
  if (is.null(topk)) {
    topk <- ceiling(0.05 * nrow(atoms_dict))
  }

  if (topk > nrow(cp_mtx)) {
    stop("'topk' cannot be greater than ", nrow(atoms_dict), ".")
  }

  if (verbose) cat("Precomputing objects, please wait a few seconds..." , "\n")

  # Calculates atoms for all possible combinations of "freq_hz", "window_len"
  # (so you don't have to calculate the same things multiple times later)
  unique_atoms <- unique(atoms_dict[, c("freq_hz", "window_len")])
  atoms_list <- list()

  for (i in 1:nrow(unique_atoms)) {
    ua2 <- unique_atoms[i, 2]
    ua1 <- unique_atoms[i, 1]
    # Local atom samples
    n <- 0:(ua2 - 1)
    # Gaussian window
    c <- (ua2 - 1) / 2
    if (is.null(sigma_divisor)) {
      sigma <- (ua2 + 1) / 3
    } else {
      sigma <- (ua2 + 1) / sigma_divisor
    }
    w <- exp(-pi * ((n - c) / sigma)^2)
    carrier_cplx <- exp(1i * 2 * pi * ua1 * n / sf)
    atom_cplx <- w * carrier_cplx
    atoms_list[[i]] <- atom_cplx
  }

  # Create a composite key from the first two columns of unique_atoms
  # and build a lookup table mapping each key to its index in atoms_list
  key <- paste(unique_atoms[,1], unique_atoms[,2], sep=":")
  lookup <- setNames(seq_along(atoms_list), key)

  #plot(w, type = "l")
  #plot(Re(atom_cplx), type = "l")
  #plot(Im(atom_cplx), type = "l")

  # Similar to above for all unique "window_len"
  wl <- unique(atoms_dict[, "window_len"])
  w_list <- list()

  for (i in 1:length(wl)) {
    n <- 0:(wl[i] - 1)
    c <- (wl[i] - 1) / 2
    # "(wl[i] + 1) / 3": heuristic default
    # We want the Gauss to be reasonably concentrated in the window
    # and not extend beyond its edges.
    #
    # sigma_divisor: divisor used to compute Gaussian width.
    # Larger values create narrower windows.
    if (is.null(sigma_divisor)) {
      sigma <- (wl[i] + 1) / 3
    } else {
      sigma <- (wl[i] + 1) / sigma_divisor
    }
    w_list[[i]] <- exp(-pi * ((n - c) / sigma)^2)
  }

  # Similar to above for all unique "freq_hz"
  fhz <- unique(atoms_dict[, "freq_hz"])
  fhz_vec <- c()

  for (j in 1:length(fhz)) {
    fhz_vec[j] <- exp(1i * 2 * pi * fhz[j] / sf)
  }

  # ------------------------------------------------------------------+
  # STEP 1 ----
  # Calculate phase-invariant similarities
  # using complex Gabor atoms
  # ------------------------------------------------------------------+
  #
  # In this step, we don't save all the generated atoms. With hundreds of
  # thousands of potential atoms, this would be very inefficient, especially
  # since the vast majority of these atoms won't be selected (crossprod
  # too small). We only create the cp_mtx matrix with the saved crossprod
  # values. In step 2, a matrix with the 'topk' atoms will be created.

  for (i in 1:nrow(atoms_dict)) {

    if (verbose) {
      if (i %% 1000 == 0) {
        cat("topk_atoms: step 1, calculating cross products: ", i, "/", nrow(atoms_dict), "\r", sep  = "")
        flush.console()
      }
    }

    time <- atoms_dict[i, "time_sec"]
    freq <- atoms_dict[i, "freq_hz"]
    window_len <- atoms_dict[i, "window_len"]

    # Convert time to sample index
    n0 <- as.integer(time * sf) + 1

    # Local atom samples
    n <- 0:(window_len - 1)

    # Gaussian window
    c <- (window_len - 1) / 2

    #idx <- match(window_len, wl)
    #w <- w_list[[idx]]

    # ------------------------------------------------------------------+
    # Complex Gabor atom ----
    # ------------------------------------------------------------------+
    # use the precomputed values
    atom_cplx <- atoms_list[[ lookup[[paste(freq, window_len, sep= ":")]] ]]

    # Full-length complex signal
    end_idx <- min(n0 + window_len - 1, N)
    valid_len <- end_idx - n0 + 1

    # Normalize
    atom_seg <- atom_cplx[1:valid_len]
    norm <- sqrt(sum(Mod(atom_seg)^2))
    if (norm > 0) {
       atom_seg <- atom_seg / norm
    }

    # ------------------------------------------------------------------+
    # Complex projection ----
    # ------------------------------------------------------------------+
    cp <- crossprod(atom_seg, sig[n0:end_idx, , drop = FALSE])

    # Phase-invariant similarity
    cp_mtx[i, ] <- Mod(cp)


  } ### for (i in 1:nrow(atoms_dict))

  if (verbose) {
    cat("topk_atoms: step 1, calculating cross products: ", i, "/", nrow(atoms_dict), "\r", sep = "")
    flush.console()
  }

  gc()

  # ------------------------------------------------------------------+
  # STEP 2 ----
  # Generate top-k atoms with optimal phase
  #                           ^^^^^^^^^^^^^
  # ------------------------------------------------------------------+
  atoms_list <- list()

  for (m in 1:ncol(sig)) {
    atoms_list[[m]] <- matrix(NA, nrow = N, ncol = topk)
  }
  names(atoms_list) <- paste0("signal_", 1:ncol(sig))

  times_mtx <- matrix(NA, nrow = topk, ncol = ncol(sig))
  times_center_mtx <- matrix(NA, nrow = topk, ncol = ncol(sig))
  freq_mtx <- matrix(NA, nrow = topk, ncol = ncol(sig))
  sigma_mtx <- matrix(NA,  nrow = topk, ncol = ncol(sig))
  window_len_mtx <- matrix(NA, nrow = topk, ncol = ncol(sig))
  topk_idx_mtx <- matrix(NA, nrow = topk, ncol = ncol(sig))
  phase_mtx <- matrix(NA, nrow = topk, ncol = ncol(sig))

  # "j" -  channel
  for (j in 1:ncol(sig)) {
    topk_idx <- order(cp_mtx[, j], decreasing = TRUE)[1:topk]
    topk_atoms_dict <- atoms_dict[topk_idx,]

    atoms_mtx <- matrix(NA, nrow = N, ncol = topk)
    times_vec <- numeric(topk)
    times_center_vec <- numeric(topk)
    freq_vec <- numeric(topk)
    sigma_vec <- numeric(topk)
    window_len_vec <- numeric(topk)
    phase_vec <- numeric(topk)

    m <- 1
    if (verbose) cat("\n")

    for (k in 1:nrow(topk_atoms_dict)) {
      time <- topk_atoms_dict[k, "time_sec"]
      freq <- topk_atoms_dict[k, "freq_hz"]
      window_len <- topk_atoms_dict[k, "window_len"]

      if (verbose) {
        if (m %% 100 == 0) {
          cat("topk_atoms: step 2, signal: ", j, ", generating atoms: ", m, "/", topk, "\r", sep = "")
          flush.console()
        }
      }

      n0 <- as.integer(time * sf) + 1
      n <- 0:(window_len - 1)
      c <- (window_len - 1) / 2

       if (is.null(sigma_divisor)) {
         sigma <- (window_len + 1) / 3
       } else {
         sigma <- (window_len + 1) / sigma_divisor
       }

      idx <- match(window_len, wl)
      w <- w_list[[idx]]

      # ------------------------------------------------------------------+
      # Complex atom for phase estimation ----
      #                 ^^^^^^^^^^^^^^^^^
      # ------------------------------------------------------------------+
      idx <- match(freq, fhz)
      carrier_cplx <- fhz_vec[idx]^n
      atom_cplx <- w * carrier_cplx

      end_idx <- min(n0 + window_len - 1, N)
      valid_len <- end_idx - n0 + 1

      # Normalize
      atom_seg <- atom_cplx[1:valid_len]
      norm <- sqrt(sum(Mod(atom_seg)^2))
      if (norm > 0) {
        atom_seg <- atom_seg / norm
      }

      # ------------------------------------------------------------------+
      # Optimal phase ----
      # ------------------------------------------------------------------+
      cp <- crossprod(atom_seg, sig[n0:end_idx, j, drop = FALSE])

      phi <- Arg(as.vector(cp))

      # ------------------------------------------------------------------+
      # Final real-valued atom with optimal phi ----
      #       ^^^^^^^^^^^^^^^^      ^^^^^^^^^^^
      # ------------------------------------------------------------------+
      carrier <- cos(2 * pi * freq * n / sf + phi)
      atom <- w * carrier
      x <- rep(0, N)
      x[n0:end_idx] <- atom[1:valid_len]

      # Normalize final atom
      norm <- sqrt(sum(x^2))

      if (norm > 0) {
        x <- x / norm
      }

      # ------------------------------------------------------------------+
      # Save ----
      # ------------------------------------------------------------------+
      atoms_mtx[, m] <- x
      times_vec[m] <- time
      times_center_vec[m] <- (time + (window_len / (2 * sf)))
      freq_vec[m] <- freq
      sigma_vec[m] <- sigma / sf
      window_len_vec[m] <- window_len / sf
      phase_vec[m] <- phi

      m <- m + 1
    } ### for (k in topk_idx)

    atoms_list[[j]] <- atoms_mtx
    times_mtx[, j] <- times_vec
    times_center_mtx[, j] <- times_center_vec
    freq_mtx[, j] <- freq_vec
    phase_mtx[, j] <- phase_vec
    sigma_mtx[, j] <- sigma_vec
    window_len_mtx[, j] <- window_len_vec
    topk_idx_mtx[, j] <- topk_idx


    if (verbose) {
      cat("topk_atoms: step 2, signal: ", j, ", generating atoms: ", m - 1, "/", topk, "\r", sep = "")
      flush.console()
    }
  } ###  for (j in 1:ncol(sig))

  # ------------------------------------------------------------------+
  # OUTPUT
  # ------------------------------------------------------------------+
  output <- list(
    cross_products = cp_mtx,
    topk_indices = topk_idx_mtx,
    atoms = atoms_list,
    frequency = freq_mtx,
    phase = phase_mtx,
    scale = sigma_mtx,
    position = times_center_mtx,
    atom_begin = times_mtx,
    window_len = window_len_mtx
  )

  class(output) <- "topk"

  return(output)
}
