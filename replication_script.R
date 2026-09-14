# +-------------------------------------------------------------+
# Replication script for the SoftwareX article
# https://www.sciencedirect.com/journal/softwarex
#
# MatchingPursuit: An R Package for Sparse Signal Decomposition
# Using Matching Pursuit and Orthogonal Matching Pursuit
#
# Artur Gramacki and Jarosław Gramacki<br>
# E-mail: a.gramacki@issi.uz.zgora.pl, j.gramacki@ck.uz.zgora.pl
# +-------------------------------------------------------------+

library(MatchingPursuit)

# +-------------------------------------------------------------+
# | 3.1. Basic decomposition workflow                           |
# +-------------------------------------------------------------+
sig_file <- system.file(
  "extdata",
  "sample1.csv",
  package = "MatchingPursuit"
)

signal <- read_csv_signals(sig_file)

# use mode = "omp" for OMP algorithm
out_mp <- mp_omp_execute(
  signal = signal,
  mode = "mp",
  n_nonzero_coefs = 50
)

plot(out_mp)

# +-------------------------------------------------------------+
# | 3.2. Decomposition with a custom dictionary                 |
# +-------------------------------------------------------------+
N <- 256
t <- seq(0, 1, length.out = N)

dictionary <- cbind(
  sin(2 * pi * 5 * t), cos(2 * pi * 5 * t),
  sin(2 * pi * 10 * t), cos(2 * pi * 10 * t)
)

signal <- sin(2 * pi * 5 * t) + 0.5 * cos(2 * pi * 10 * t)

# use mp_core() for MP; the result is similar
out <- omp_core(
  dictionary = dictionary,
  signal = signal,
  n_nonzero_coefs = 2
)

out$support
out$relative_residual_energy

# +-------------------------------------------------------------+
# |  3.3 Reconstruction accuracy and computational performance  |
# +-------------------------------------------------------------+
# SNR: Signal-to-Noise Ratio
# SNR = 20 dB -> the signal power is 100 times higher than the noise power
# SNR = 10 dB -> the signal power is 00 times higher than the noise power
# SNR = 0 dB -> the signal and noise have the same power
#
# EE: Explained_Energy
# RRE - Relative_Reconstruction_Error
#       (an RRE of 0 indicates perfect reconstruction;
#       lower values indicate better reconstruction quality)

fs <- 64
duration <- 2
samples <- fs * duration

clean <-
  1.00 * gabor_atom(samples, fs, 0.83, 0.2, 0.32,  3.7, FALSE)$gabor +
  0.85 * gabor_atom(samples, fs, 1.64, 1.0, 0.18,  8.3, FALSE)$gabor +
  0.70 * gabor_atom(samples, fs, 2.37, 0.6, 0.27, 14.6, FALSE)$gabor +
  1.10 * gabor_atom(samples, fs, 3.18, 1.4, 0.45,  5.4, FALSE)$gabor +
  0.65 * gabor_atom(samples, fs, 4.06, 0.3, 0.14, 19.2, FALSE)$gabor +
  0.90 * gabor_atom(samples, fs, 4.91, 2.0, 0.38, 10.7, FALSE)$gabor +
  0.75 * gabor_atom(samples, fs, 5.73, 0.8, 0.20, 23.1, FALSE)$gabor +
  1.05 * gabor_atom(samples, fs, 6.62, 1.7, 0.52,  6.8, FALSE)$gabor +
  0.60 * gabor_atom(samples, fs, 7.58, 2.4, 0.25, 16.4, FALSE)$gabor +
  0.80 * gabor_atom(samples, fs, 8.71, 0.5, 0.34, 11.9, FALSE)$gabor
clean <- clean / sqrt(sum(clean^2))

results <- data.frame()
n_rep <- 30
snr_values <- c(20, 10, 0)

for (rep in seq_len(n_rep)) {
  for (snr_dB in snr_values) {

    message("Run ", rep, "/", n_rep, " | SNR = ", snr_dB, " dB")

    set.seed(rep)

    noise <- rnorm(length(clean))
    noise <- noise / sqrt(sum(noise^2))
    noise <- noise * 10^(-snr_dB / 20)
    signal <- clean + noise

    sig <- as_sig(signal, fs)

    # "-o global" was used as the most complete optimization mode for this benchmark;
    # "-o local" is faster but uses only local optimization, whereas "-o none" disables
    # continuous atom optimization and provides the fastest, simplified variant.
    #
    # A very small residual threshold (-r 1e-11) was used to prevent early stopping
    # and ensure that exactly 10 atoms were selected.
    #
    # See the EMPI README.md documentation for details on optimization modes
    # and stopping criteria.
    time_empi <-  system.time({
    out_empi <- empi_execute(
        signal = sig,
        empi_options = paste0("-o global -i ", 10, " -r 0.00000000001 --gabor"),
        ignore.stdout = TRUE, ignore.stderr = TRUE)
    })["elapsed"]
    message("empi_execute(): channel = ", 1, " Successfully processed.")

    rre_empi <-  sqrt(sum((clean - out_empi$reconstruction)^2)) /  sqrt(sum(clean^2))
    signal <- as.matrix(out_empi$signal)
    reconstruction <- as.matrix(out_empi$reconstruction)
    signal_energy <- sum(signal^2, na.rm = TRUE)
    residual_energy <- sum((signal - reconstruction)^2, na.rm = TRUE)
    ee_empi <- 1 - residual_energy / signal_energy

    time_mp <-  system.time({
    out_mp <- mp_omp_execute(mode = "mp", signal = sig, n_nonzero_coefs = 10, topk = NULL, verbose = F)
    })["elapsed"]

    rre_mp <-  sqrt(sum((clean - out_mp$reconstruction)^2)) /  sqrt(sum(clean^2))
    signal <- as.matrix(out_mp$signal)
    reconstruction <- as.matrix(out_mp$reconstruction)
    signal_energy <- sum(signal^2, na.rm = TRUE)
    residual_energy <- sum((signal - reconstruction)^2, na.rm = TRUE)
    ee_mp <- 1 - residual_energy / signal_energy

    time_omp <-  system.time({
    out_omp <- mp_omp_execute(mode = "omp", signal = sig, n_nonzero_coefs = 10, topk = NULL, verbose = F)
    })["elapsed"]

    rre_omp <-  sqrt(sum((clean - out_omp$reconstruction)^2)) /  sqrt(sum(clean^2))
    signal <- as.matrix(out_omp$signal)
    reconstruction <- as.matrix(out_omp$reconstruction)
    signal_energy <- sum(signal^2, na.rm = TRUE)
    residual_energy <- sum((signal - reconstruction)^2, na.rm = TRUE)
    ee_omp <- 1 - residual_energy / signal_energy

    results <- rbind(
      results,
      data.frame(
        rep = rep,
        method = c("EMPI", "MP-R", "OMP-R"),
        snr_dB = snr_dB,
        rre_clean = c(rre_empi, rre_mp, rre_omp),
        explained_energy = c(ee_empi, ee_mp, ee_omp),
        elapsed_time = c(
          as.numeric(time_empi),
          as.numeric(time_mp),
          as.numeric(time_omp)
        )
      )
    )
  }
}

summary_rre <- aggregate(
  rre_clean ~ method + snr_dB,
  data = results,
  FUN = function(x) sprintf("%.3f ± %.3f", mean(x), sd(x))
)

summary_ee <- aggregate(
  explained_energy ~ method + snr_dB,
  data = results,
  FUN = function(x) sprintf("%.3f ± %.3f", 100 * mean(x), 100 * sd(x))
)

summary_time <- aggregate(
  elapsed_time ~ method + snr_dB,
  data = results,
  FUN = function(x) sprintf("%.3f ± %.3f", mean(x), sd(x))
)

table_results <- Reduce(
  function(x, y) merge(x, y, by = c("method", "snr_dB")),
  list(summary_rre, summary_ee, summary_time)
)

names(table_results) <- c(
  "Method",
  "SNR_dB",
  "RRE_mean_SD",
  "Explained_energy_mean_SD",
  "Time_mean_SD"
)

table_results$Method <- factor(
  table_results$Method,
  levels = c("MP-R", "OMP-R", "EMPI")
)

table_results <- table_results[
  order(table_results$SNR_dB, table_results$Method),
]

table_results

# +-------------------------------------------------------------+
# | 3.4. EEG analysis workflow                                  |
# +-------------------------------------------------------------+

# +------------------------+
# Import EDF recording     |
# +------------------------+
file <- system.file(
  "extdata", "EEG.edf",
  package = "MatchingPursuit"
)

sig <- read_edf_signals(file)

eeg <- sig$signal
sf <- sig$sampling_frequency

# +------------------------+
# Filtering                |
# +------------------------+
fc <- design_filters(
  sampling_frequency = sf,
  notch = c(49, 51),
  lowpass = 40,
  highpass = 1
)

eeg_f <- eeg

for (m in 1:ncol(eeg_f)) {
  eeg_f[, m] <- signal::filtfilt(fc$notch, eeg[, m])
  eeg_f[, m] <- signal::filtfilt(fc$lowpass, eeg_f[, m])
  eeg_f[, m] <- signal::filtfilt(fc$highpass, eeg_f[, m])
}

# +------------------------+
# | Downsampling (optional)|
# +------------------------+
sf_r <- 128
eeg_f_r <- resample_signal(
  signal = eeg_f, p = 1, q = 2
)

# +------------------------+
# | Bipolar montag         |
# +------------------------+
pairs <- list(
  c("Fp2","F4"), c("F4","C4"), c("C4","P4"), c("P4","O2"),
  c("Fp1","F3"), c("F3","C3"), c("C3","P3"), c("P3","O1"),
  c("Fp2","F8"), c("F8","T4"), c("T4","T6"), c("T6","O2"),
  c("Fp1","F7"), c("F7","T3"), c("T3","T5"), c("T5","O1"),
  c("Fz","Cz"), c("Cz","Pz")
)

eeg_f_r_m <- eeg_montage(
  eeg_f_r,
  montage_type = "bipolar",
  bipolar_pairs = pairs
)

# +------------------------+
# | Convert to "sig" object|
# +------------------------+
sig <- as_sig(eeg_f_r_m[,1], sf_r)

# +------------------------+
# |  MP decomposition      |
# +------------------------+
out_mp <- mp_omp_execute(
  mode = "mp",
  signal = sig,
  n_nonzero_coefs = 50,
  verbose = TRUE
)

# +------------------------+
# |  Time-Frequency map    |
# +------------------------+
plot(out_mp)







