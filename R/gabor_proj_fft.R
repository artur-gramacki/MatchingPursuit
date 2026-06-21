#' FFT-based fast computation of inner products between a signal and Gabor atoms
#'
#' This function computes inner products between a windowed signal and a set of Gabor
#' atoms using FFT-based frequency-domain operations. Instead of explicitly constructing
#' and shifting atoms in the time domain, it extracts selected Fourier coefficients
#' corresponding to Gabor frequencies. The resulting values provide both complex
#' projection coefficients and their magnitudes.
#'
#' @param block See the vignette (link below) for a description of the structure of blocks.
#'
#' @param signal A numeric vector, matrix, or data frame representing the signal(s)
#'   to be analyzed. Each column is treated as a separate channel.
#'
#' @seealso
#' \itemize{
#'   \item \emph{Introduction to the 'MatchingPursuit' package}
#'     (\url{https://cran.r-project.org/web/packages/MatchingPursuit/vignettes/MatchingPursuit.html})
#' }
#'
#' @note Users do not work directly with this function. It is used internally in the
#' \code{topk_atoms()} function. However, it can be used by users for their own experiments
#' and tests.
#'
#' @return A list containing two matrices computed from windowed FFT segments of the signal:
#' \item{proj_mod_mtx}{Magnitudes of selected Gabor atom inner products
#' (absolute values of projection coefficients).}
#' \item{fft_bin_mtx}{Complex Fourier coefficients used to compute inner products with Gabor atoms.}
#'
#' @importFrom stats mvfft
#'
#' @export
#'
#' @examples
#' signal <- as.matrix(rnorm(256))
#' sf <- 256
#' duration <- 1
#'
#' xml_file <- system.file("extdata", "one_block_dict.xml", package = "MatchingPursuit")
#' block <- read_dict(xml_file, sf, duration, verbose = TRUE)
#' my_list <- gabor_proj_fft(block, signal)
#'
#' pmm <- my_list$proj_mod_mtx
#' scm <- my_list$fft_bin_mtx
#'
#' head(scm)
#' head(pmm)
#'
#' # Of course it gives 'pmm'
#' head(Mod(scm))
#'
gabor_proj_fft <- function(block, signal) {

  N <- nrow(signal)
  K <- ncol(signal)
  proj_mod_mtx <- matrix(0, nrow = nrow(block), ncol = K)
  fft_bin_mtx <- matrix(0, nrow = nrow(block), ncol = K)

  unique_times <- unique(block[, "time_sample"])

  for (t_sample in unique_times) {

    idx_in_dict <- which(block[, "time_sample"] == t_sample)
    window_len <- block[idx_in_dict[1], "window_len"]
    fft_size   <- block[idx_in_dict[1], "fft_size"]

    n0 <- t_sample + 1
    n <- 0:(window_len - 1)
    c <- (window_len - 1) / 2
    sigma <- (window_len + 1) / 3
    w <- exp(-pi * ((n - c) / sigma)^2)
    w_norm <- w / sqrt(sum(w^2))

    end_idx <- min(n0 + window_len - 1, N)
    sig_segmented <- matrix(0, nrow = fft_size, ncol = K)
    sig_segmented[1:window_len, ] <- signal[n0:end_idx, , drop = FALSE] * w_norm

    fft_res <- mvfft(sig_segmented)

    freq_bins <- block[idx_in_dict, "freq_bin"]
    fft_indices <- freq_bins + 1

    proj_mod_mtx[idx_in_dict, ] <- Mod(fft_res[fft_indices, , drop = FALSE])
    fft_bin_mtx[idx_in_dict, ] <- fft_res[fft_indices, , drop = FALSE]
  }

  return(
    list(
      proj_mod_mtx = proj_mod_mtx,
      fft_bin_mtx = fft_bin_mtx)
  )
}
