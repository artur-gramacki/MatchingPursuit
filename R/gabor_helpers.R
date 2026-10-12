# Internal helpers shared by `mp_core_gabor()` and `omp_core_gabor()`.
#
# Candidate projections are computed blockwise with FFT, the optimal phase is
# obtained analytically, and the full time-domain waveform is constructed only
# for the winning atom in each iteration.


# Build a cache of Gaussian Gabor windows for all unique window lengths.
build_gabor_window_cache <- function(atoms_dict, sigma_div) {

  window_lengths <- unique(as.integer(atoms_dict[, "window_len"]))
  window_cache <- vector("list", length(window_lengths))
  names(window_cache) <- as.character(window_lengths)

  for (i in seq_along(window_lengths)) {
    window_len <- window_lengths[i]
    n <- 0:(window_len - 1L)
    center <- (window_len - 1) / 2
    sigma <- (window_len + 1) / sigma_div
    w <- exp(-pi * ((n - center) / sigma)^2)
    w_norm <- w / sqrt(sum(w^2))

    window_cache[[i]] <- list(n = n, w = w, w_norm = w_norm)
  }

  window_cache
}


# Precompute cosine-sine Gram matrix terms for one Gabor dictionary block.
precompute_gabor_gram_block <- function(
    block, ids, window_cache, signal_length) {

  window_len <- as.integer(block[1L, "window_len"])
  fft_size <- as.integer(block[1L, "fft_size"])

  cache <- window_cache[[as.character(window_len)]]
  n <- cache$n
  w_norm <- cache$w_norm

  time_sample <- block[, "time_sample"]

  if (is.unsorted(time_sample)) {
    ord <- order(time_sample)
    time_sorted <- time_sample[ord]
  } else {
    ord <- seq_along(time_sample)
    time_sorted <- time_sample
  }

  runs <- rle(time_sorted)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L

  gram_aa <- numeric(length(ids))
  gram_bb <- numeric(length(ids))
  gram_ab <- numeric(length(ids))

  # Atoms at the same time position have the same overlap range with the signal
  # regardless of frequency. Quantities depending only on this range can be reused.
  overlap_cache <- list()

  for (g in seq_along(runs$values)) {

    idx <- ord[seq.int(starts[g], ends[g])]
    t_sample <- runs$values[g]

    signal_idx <- t_sample + n
    inside <- signal_idx >= 0L & signal_idx < signal_length
    atom_idx <- which(inside)

    if (length(atom_idx) == 0L) next

    key <- paste0(atom_idx[1L], ":", atom_idx[length(atom_idx)])
    z <- overlap_cache[[key]]

    if (is.null(z)) {
      v <- numeric(fft_size)
      v[atom_idx] <- w_norm[atom_idx]^2

      z <- list(
        s0 = sum(v),
        fft_v = stats::fft(v)
      )

      overlap_cache[[key]] <- z
    }

    # Gram matrix elements contain cos(2*theta) and sin(2*theta) terms,
    # so use the FFT bin corresponding to twice the candidate frequency.
    freq_bins <- as.integer(block[idx, "freq_bin"])
    fft_idx_2f <- ((2L * freq_bins) %% fft_size) + 1L

    # stats::fft() uses exp(-i*theta), hence the complex conjugate gives:
    # q = sum w^2 exp(+2 i theta).
    q <- Conj(z$fft_v[fft_idx_2f])

    gram_aa[idx] <- 0.5 * (z$s0 + Re(q))
    gram_bb[idx] <- 0.5 * (z$s0 - Re(q))
    gram_ab[idx] <- 0.5 * Im(q)
  }

  list(
    ids = ids,
    gram_aa = gram_aa,
    gram_bb = gram_bb,
    gram_ab = gram_ab
  )
}


# Precompute cosine-sine Gram matrix terms for the complete Gabor dictionary.
precompute_gabor_gram <- function(
    atoms_dict, block_rows, window_cache, signal_length, verbose = FALSE) {

  p <- nrow(atoms_dict)
  gram_aa <- numeric(p)
  gram_bb <- numeric(p)
  gram_ab <- numeric(p)

  if (verbose) message("Precomputing Gabor normalization terms...")

  for (b in seq_along(block_rows)) {

    ids <- block_rows[[b]]
    block <- atoms_dict[ids, , drop = FALSE]

    gram_block <- precompute_gabor_gram_block(
      block = block,
      ids = ids,
      window_cache = window_cache,
      signal_length = signal_length
    )

    gram_aa[ids] <- gram_block$gram_aa
    gram_bb[ids] <- gram_block$gram_bb
    gram_ab[ids] <- gram_block$gram_ab

    if (verbose) {
      message("normalization: block ", b, "/", length(block_rows), " finished")
    }
  }

  if (verbose) message("Normalization terms ready.")

  list(
    gram_aa = gram_aa,
    gram_bb = gram_bb,
    gram_ab = gram_ab
  )
}


# Prepare all dictionary-dependent quantities reused by MP and OMP iterations.
prepare_gabor_dictionary <- function(
    atoms_dict,
    signal_length,
    sampling_frequency,
    sigma_divisor = NULL,
    verbose = FALSE) {

  sigma_div <- if (is.null(sigma_divisor)) 3 else sigma_divisor

  block_ids <- unique(atoms_dict[, "block"])
  block_rows <- lapply(
    block_ids,
    function(x) which(atoms_dict[, "block"] == x)
  )

  window_cache <- build_gabor_window_cache(
    atoms_dict = atoms_dict,
    sigma_div = sigma_div
  )

  gram <- precompute_gabor_gram(
    atoms_dict = atoms_dict,
    block_rows = block_rows,
    window_cache = window_cache,
    signal_length = signal_length,
    verbose = verbose
  )

  list(
    block_rows = block_rows,
    window_cache = window_cache,
    gram_aa = gram$gram_aa,
    gram_bb = gram$gram_bb,
    gram_ab = gram$gram_ab,
    signal_length = signal_length,
    sampling_frequency = sampling_frequency,
    sigma_divisor = sigma_div
  )
}


# Score all candidates in one Gabor dictionary block against the current residual.
score_gabor_block <- function(
    block, ids, residual,
    gram_aa, gram_bb, gram_ab,
    sigma_divisor = NULL) {

  projections <- gabor_projection_fft(
    block = block,
    signal = residual,
    sigma_divisor = sigma_divisor
  )

  # Each complex coefficient contains projections onto the cosine and sine
  # components of one candidate.
  z <- projections$fft_bin_mtx[, 1L]
  ca <- Re(z)
  cb <- -Im(z)

  # Gram matrix elements: A = <a,a>, B = <b,b>, C = <a,b>.
  A <- gram_aa[ids]
  B <- gram_bb[ids]
  C <- gram_ab[ids]

  detG <- A * B - C^2
  gram_scale <- pmax((A + B)^2, 1)

  regular <- is.finite(detG) &
    detG > 1e-12 * gram_scale &
    A > 0 & B > 0

  score2 <- rep(-Inf, length(ids))
  phase_vec <- rep(NA_real_, length(ids))

  # Regular case: G has full rank. For c = [ca, cb]^T and v = G^(-1)c,
  # the squared maximum normalized projection is c^T G^(-1)c.
  if (any(regular)) {

    rr <- which(regular)

    v1 <- (B[rr] * ca[rr] - C[rr] * cb[rr]) / detG[rr]
    v2 <- (A[rr] * cb[rr] - C[rr] * ca[rr]) / detG[rr]

    s2 <- ca[rr] * v1 + cb[rr] * v2
    s2 <- pmax(s2, 0)

    score2[rr] <- s2
    phase_vec[rr] <- atan2(-v2, v1)
  }

  # Singular cases include DC and Nyquist frequencies, where cosine and sine
  # do not provide two independent directions.
  singular <- which(!regular)

  if (length(singular) > 0L) {

    eps <- 1e-14

    score_a <- rep(-Inf, length(singular))
    score_b <- rep(-Inf, length(singular))

    ok_a <- A[singular] > eps
    ok_b <- B[singular] > eps

    score_a[ok_a] <- ca[singular][ok_a]^2 / A[singular][ok_a]
    score_b[ok_b] <- cb[singular][ok_b]^2 / B[singular][ok_b]

    use_a <- score_a >= score_b

    ia <- singular[use_a]
    ib <- singular[!use_a]

    if (length(ia) > 0L) {
      score2[ia] <- score_a[use_a]
      phase_vec[ia] <- ifelse(ca[ia] >= 0, 0, pi)
    }

    if (length(ib) > 0L) {
      score2[ib] <- score_b[!use_a]
      phase_vec[ib] <- ifelse(cb[ib] >= 0, -pi / 2, pi / 2)
    }
  }

  score <- sqrt(pmax(score2, 0))
  j <- which.max(score)

  if (length(j) == 0L || !is.finite(score[j])) {
    return(list(score = -Inf, atom_idx = NA_integer_, phase = NA_real_))
  }

  list(
    score = score[j],
    atom_idx = ids[j],
    phase = phase_vec[j]
  )
}


# Find the phase-optimized Gabor atom that best matches the current residual.
find_best_gabor_atom <- function(
    atoms_dict,
    residual,
    prep,
    exclude = integer(0)) {

  best_score <- -Inf
  best_atom_idx <- NA_integer_
  best_phase <- NA_real_

  for (b in seq_along(prep$block_rows)) {

    ids <- prep$block_rows[[b]]

    if (length(exclude) > 0L) {
      ids <- ids[!ids %in% exclude]
    }

    if (length(ids) == 0L) next

    block <- atoms_dict[ids, , drop = FALSE]

    block_best <- score_gabor_block(
      block = block,
      ids = ids,
      residual = residual,
      gram_aa = prep$gram_aa,
      gram_bb = prep$gram_bb,
      gram_ab = prep$gram_ab,
      sigma_divisor = prep$sigma_divisor
    )

    if (is.finite(block_best$score) && block_best$score > best_score) {
      best_score <- block_best$score
      best_atom_idx <- block_best$atom_idx
      best_phase <- block_best$phase
    }
  }

  if (is.na(best_atom_idx)) return(NULL)

  list(
    score = best_score,
    atom_idx = best_atom_idx,
    phase = best_phase
  )
}


# Construct and normalize the selected real-valued Gabor atom in the time domain.
build_gabor_atom <- function(
    atoms_dict, atom_idx, phase, prep) {

  window_cache <- prep$window_cache
  signal_length <- prep$signal_length
  sampling_frequency <- prep$sampling_frequency

  row <- atoms_dict[atom_idx, , drop = FALSE]

  t_sample <- as.integer(row[1L, "time_sample"])
  freq <- row[1L, "freq_hz"]
  window_len <- as.integer(row[1L, "window_len"])

  cache <- window_cache[[as.character(window_len)]]
  n <- cache$n
  w <- cache$w

  atom_full <- w * cos(2 * pi * freq * n / sampling_frequency + phase)

  # Preserve the normalization convention used in the current implementation:
  # normalize the complete atom first, then normalize the visible part again
  # after possible boundary truncation.
  full_norm <- sqrt(sum(atom_full^2))

  if (!is.finite(full_norm) || full_norm < 1e-12) {
    stop("Selected atom has zero or non-finite norm.")
  }

  atom_full <- atom_full / full_norm

  signal_idx <- t_sample + n
  inside <- signal_idx >= 0L & signal_idx < signal_length

  rows <- signal_idx[inside] + 1L
  atom_part <- atom_full[inside]

  visible_norm <- sqrt(sum(atom_part^2))

  if (!is.finite(visible_norm) || visible_norm < 1e-12) {
    stop("Selected atom has zero or non-finite visible norm.")
  }

  atom_part <- atom_part / visible_norm

  atom <- numeric(signal_length)
  atom[rows] <- atom_part

  list(
    atom = atom,
    atom_part = atom_part,
    rows = rows,
    row = row,
    t_sample = t_sample,
    freq = freq,
    window_len = window_len
  )
}

