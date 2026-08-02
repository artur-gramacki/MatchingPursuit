## Package and options ----
library("MASS")
options(prompt = "R> ", continue = "+  ", width = 90,  useFancyQuotes = FALSE, digits = 3)


# Filtering ----
file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
eeg <- read_edf_signals(file, resampling = FALSE, from = 0, to = 10)
signal_eeg <- eeg$signal
sampling_frequency <- eeg$sampling_frequency
sampling_frequency

pairs <- list(
  c("Fp2", "F4"), c("F4", "C4"), c("C4", "P4"), c("P4", "O2"), c("Fp1", "F3"), c("F3", "C3"),
  c("C3", "P3"), c("P3", "O1"), c("Fp2", "F8"), c("F8", "T4"), c("T4", "T6"), c("T6", "O2"),
  c("Fp1", "F7"), c("F7", "T3"), c("T3", "T5"), c("T5", "O1"), c("Fz", "Cz"), c("Cz", "Pz")
)

# Make the bipolar montage.
eeg_m <- eeg_montage(eeg, montage_type = c("bipolar"), bipolar_pairs = pairs)
signal_eeg_m <- eeg_m$signal

# Filter parameters that will be used (quite typical in filtering EEG signals).
fc <- filters_coeff(
   sampling_frequency = sampling_frequency,
   notch = c(49, 51),
   lowpass = 40,
   highpass = 1,
)

# Filtering input signals.
signal_eeg_m_f <- signal_eeg_m

for (m in 1:ncol(signal_eeg_m)) {
  signal_eeg_m_f[, m] = signal::filtfilt(fc$notch, signal_eeg_m[, m])      # 50Hz notch filter
  signal_eeg_m_f[, m] = signal::filtfilt(fc$lowpass, signal_eeg_m_f[, m])  # Low pass IIR Butterworth
  signal_eeg_m_f[, m] = signal::filtfilt(fc$highpass, signal_eeg_m_f[, m]) # High pass IIR Butterwoth
}


ch <- 1
par(mfrow = c(2, 1), pty = "m", mai = c(0.6, 0.5, 0.4, 0.5), mgp = c(1.5, 0.5, 0))

time <- seq(0, nrow(signal_eeg) - 1) / sampling_frequency
sr <- range(signal_eeg_m[, ch])

# Not-filtered signal (raw signal).
plot(
  time,
  signal_eeg_m[, ch],
  type = "l",
  panel_first = grid(),
  main = paste("Original EEG signal, channel #1 (", colnames(signal_eeg_m)[ch], ")", sep = ""),
  xlab = "Time (s)",
  ylab = "Amplitude",
  col = "blue",
  ylim = sr
)

# Signal after filtering.
plot(
  time,
  signal_eeg_m_f[, ch],
  type = "l",
  panel.first = grid(),
  main = paste("EEG signal after preprocessing, channel #1 (", colnames(signal_eeg_m)[ch], ")", sep = ""),
  xlab = "Time (s)",
  ylab = "Amplitude",
  col = "blue",
  ylim = sr
)

par(old_par)


# dict ----
xml_file <- tempfile(fileext = ".xml")
xml_dict <- generate_xml_dict(
  N = 256,
  file = xml_file
)

xml_dict

atoms_dict <- read_dict(
  xml_file = xml_file,
  sampling_frequency = 256,
  duration = 16
)

head(atoms_dict)
tail(atoms_dict)

# backends ----

file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
signal_list <- read_csv_signals(file)
dest_dir <- tools::R_user_dir("MatchingPursuit", "cache")

opts <- paste0(
  "-o none --gabor -i 50 --full-atoms-in-signal --dictionary-output ",
  dest_dir,
  "/dict.xml"
)

empi_out <- empi_execute(
 signal = signal_list,
 empi_options = opts
)

dict_xml_file <- readLines(paste0(dest_dir,"/dict.xml"), n = 20)
cat(dict_xml_file, sep = "\n")

atoms_dict <- read_dict(
  xml_file = paste0(dest_dir,"/dict.xml"),
  sampling_frequency = 1024,
  duration = 1,
  verbose = TRUE
)

################################-
file <- system.file(
  "extdata", "sample3.csv", package = "MatchingPursuit"
)

out_csv <- read_csv_signals(file, col_names_in_csv = TRUE)
signal <- out_csv$signal
sampling_frequency <- out_csv$sampling_frequency
duration <- nrow(signal) / sampling_frequency


out_empi <- empi_execute(signal = out_csv)
head(out_empi$atoms)
plot(out_empi, freq_divide = 4)

out_empi <- empi_execute(
  signal = out_csv,
  empi_options = "-o global --gabor -i 50"
)

# residuum
out_empi <- out_empi$signal - out_empi$reconstruction
plot(out_empi, type = "l")

################################-
file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
out_csv <- read_csv_signals(file, col_names_in_csv = FALSE)
signal <- out_csv$signal
sampling_frequency <- out_csv$sampling_frequency
duration <- nrow(signal) / sampling_frequency

xml_file <- system.file("extdata", "sample1_dict.xml", package = "MatchingPursuit")

atoms_dict <- read_dict(
  xml_file = xml_file,
  sampling_frequency = sampling_frequency,
  duration = duration)

topk_dict <- topk_atoms(
  atoms_dict = atoms_dict,
  signal = signal,
  sampling_frequency = sampling_frequency,
  topk = 5000
)

out_omp <- mp_omp_execute(
  mode = "omp",
  dictionary = topk_dict,
  signal = signal,
  sampling_frequency = sampling_frequency,
  n_nonzero_coefs = 75,
  verbose = TRUE
)

plot(out_omp, freq_divide = 4)

# residuum
res_omp <- out_omp$signal - out_omp$reconstruction
plot(res_omp, type = "l")
(nrmse <- sqrt(sum((out_omp$signal - out_omp$reconstruction)^2) / sum(out_omp$signal^2)))

out_mp <- mp_omp_execute(
  mode = "mp",
  dictionary = topk_dict,
  signal = signal,
  sampling_frequency = sampling_frequency,
  n_nonzero_coefs = 75,
  verbose = TRUE
)

plot(out_mp, freq_divide = 4)

# residuum
res_mp <- out_mp$signal - out_mp$reconstruction
plot(res_mp, type = "l")
(nrmse <- sqrt(sum((out_mp$signal - out_mp$reconstruction)^2) / sum(out_mp$signal^2)))

################################-
# nrmse ----

file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
out_csv <- read_csv_signals(file, col_names_in_csv = FALSE)
signal <- out_csv$signal
sampling_frequency <- out_csv$sampling_frequency
duration <- nrow(signal) / sampling_frequency
xml_file <- system.file("extdata", "sample1_dict.xml", package = "MatchingPursuit")

atoms_dict <- read_dict(
  xml_file = xml_file,
  sampling_frequency = sampling_frequency,
  duration = duration,
  verbose = TRUE
)

topk_dict <- topk_atoms(
  atoms_dict = atoms_dict,
  signal = signal,
  sampling_frequency = sampling_frequency,
  topk = 5000,
  verbose = TRUE
)

ncoefs <- c(25, 50, 75, 100, 125, 150, 175, 200)
nrmse <- matrix(nrow = length(ncoefs), ncol = 4)
i <- 0

for (n in ncoefs) {
  i <- i + 1
  cat(ncoefs[i], "\n")
  out_mp <- mp_omp_execute(
    mode = "mp",
    dictionary = topk_dict,
    signal = signal,
    sampling_frequency = sampling_frequency,
    n_nonzero_coefs = n,
    verbose = FALSE
  )
  nrmse[i, 1] <- sqrt(sum((out_mp$signal - out_mp$reconstruction)^2) / sum(out_mp$signal^2))

  out_omp <- mp_omp_execute(
    mode = "omp",
    dictionary = topk_dict,
    signal = signal,
    sampling_frequency = sampling_frequency,
    n_nonzero_coefs = n,
    verbose = FALSE
  )
  nrmse[i, 2] <- sqrt(sum((out_omp$signal - out_omp$reconstruction)^2) / sum(out_omp$signal^2))

  out_empi <- empi_execute(
    signal = out_csv,
    empi_options = paste0("-o none --full-atoms-in-signal -i ", n, " --gabor")
  )
  nrmse[i, 3] <- sqrt(sum((out_empi$signal - out_empi$reconstruction)^2) / sum(out_empi$signal^2))

  out_empi_2 <- empi_execute(
    signal = out_csv,
    empi_options = paste0("-o global -i ", n, " -r 0.00000000001 --gabor")
  )
  nrmse[i, 4] <- sqrt(sum((out_empi_2$signal - out_empi_2$reconstruction)^2) / sum(out_empi_2$signal^2))
}
nrmse

plot(
  ncoefs,
  nrmse[, 1],
  type = "o",
  pch = 15,
  lwd = 2,
  cex = 1.2,
  col = "blue",
  ylim = range(nrmse),
  xlab = "Number of selected atoms",
  ylab = "Relative reconstruction error (lower is better)",
  main = "Reconstruction accuracy of the available decomposition backends",
  panel.first = grid()
)

lines(
  ncoefs,
  nrmse[, 2],
  type = "o",
  pch = 16,
  lwd = 2,
  cex = 1.2,
  col = "red",
)

lines(
  ncoefs,
  nrmse[, 3],
  type = "o",
  pch = 17,
  lwd = 2,
  cex = 1.2,
  col = "darkgreen",
)

lines(
  ncoefs,
  nrmse[, 4],
  type = "o",
  pch = 18,
  lwd = 2,
  cex = 1.5,
  col = "darkorange",
)

legend(
  "topright",
  legend = c("MP-R", "OMP-R", "EMPI (discrete)", "EMPI (global)"),
  pch = c(15, 16, 17, 18),
  lwd = 2,
  bty = "n",
  col = c("blue", "red", "darkgreen", "darkorange"),
  pt.cex = c(1.2, 1.2, 1.2, 1.5)
)

# EEG workflow ----
## EEG read ----
file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
read_edf_params(file)

eeg <- read_edf_signals(file)
signal_eeg <- eeg$signal
sampling_frequency <- eeg$sampling_frequency
time <- seq(0, nrow(signal_eeg) - 1) / sampling_frequency

## EEG filter ----
fc <- filters_coeff(
  sampling_frequency = sampling_frequency,
  notch = c(49, 51),
  lowpass = 40,
  highpass = 1,
)

signal_eeg_f <- signal_eeg

for (m in 1:ncol(signal_eeg_f)) {
  signal_eeg_f[, m] = signal::filtfilt(fc$notch, signal_eeg[, m])
  signal_eeg_f[, m] = signal::filtfilt(fc$lowpass, signal_eeg_f[, m])
  signal_eeg_f[, m] = signal::filtfilt(fc$highpass, signal_eeg_f[, m])
}

# EEG resample ----
# 256 Hz --> 64 Hz
signal_eeg_f_r <- resample_signal(signal = signal_eeg_f, p = 1, q = 4)
time_64 <- seq(0, nrow(signal_eeg_f_r) - 1) / (sampling_frequency / 4)
sampling_frequency_r <- 64


# EEG double banana ----
pairs <- list(
  c("Fp2", "F4"), c("F4", "C4"), c("C4", "P4"),
  c("P4", "O2"), c("Fp1", "F3"), c("F3", "C3"),
  c("C3", "P3"), c("P3", "O1"), c("Fp2", "F8"),
  c("F8", "T4"), c("T4", "T6"), c("T6", "O2"),
  c("Fp1", "F7"), c("F7", "T3"), c("T3", "T5"),
  c("T5", "O1"), c("Fz", "Cz"), c("Cz", "Pz")
)

# Make the bipolar montage.
signal_eeg_f_r_m <- eeg_montage(
  signal_eeg_f_r,
  montage_type = c("bipolar"),
  bipolar_pairs = pairs
)

# Original signal (first 6 rows, first 6 channels).
signal_eeg_f_r[1:4, 1:6]

# Signal after banana montage (first 6 rows, first 6 channels).
signal_eeg_f_r_m[1:4, 1:6]

# EEG T-F ----
sig <- list(signal_eeg_f_r_m, sampling_frequency_r)
names(sig) <- c("signal", "sampling_frequency")

out_empi <- empi_execute (signal = sig)
plot(out_empi, channel = 2)

# EEG jeden rysunek ----
old_par <- par("mfrow", "mai", "mgp")
par(mfrow = c(3, 1), pty = "m", mai = c(0.35, 0.5, 0.4, 0.1), mgp = c(1.5, 0.5, 0))

rg <- range(c(signal_eeg[, 1], signal_eeg_f[, 1], signal_eeg_f_r[, 1]))

plot(
  time,
  signal_eeg[, 1],
  type = "l",
  xlab = "Time (s)",
  ylab = "Amplitude",
  main = "Original EEG signal, channel Fp1",
  col = "blue",
  ylim = rg,
  panel.first = grid()
)
abline(h = 0, col = "gray")

plot(
  time,
  signal_eeg_f[, 1],
  type = "l",
  xlab = "Time (s)",
  ylab = "Amplitude",
  main = "Filtered EEG signal, channel Fp1",
  col = "blue",
  ylim = rg,
  panel.first = grid()
)
abline(h = 0, col = "gray")

plot(
  time_64,
  signal_eeg_f_r[, 1],
  type = "l",
  xlab = "Time (s)",
  ylab = "Amplitude",
  main = "Filtered and downsampled EEG signal (64 Hz), channel Fp1",
  col = "blue",
  ylim = rg,
  panel.first = grid()
)
abline(h = 0, col = "gray")
par(old_par)

### EEG plot.edf ----
edf_processed <- structure(
  list(
    signal = as.data.frame(signal_eeg_f_r_m),
    sampling_frequency = sampling_frequency_r,
    time = time_64,
    signal_names = colnames(signal_eeg_f_r_m),
    record_name = basename(file)
  ),
  class = "edf"
)


plot(
  x = edf_processed,
  begin = 0,
  end = 10,
  panel_height = NULL,
  rainbow = FALSE,
  bg_colour = "white",
  txt_col = "blue",
  zero_line = TRUE,
  main = "EEG.edf file after filtering, resampling and banana montage"
)


plot(
  x = eeg,
  begin = 0,
  end = 10
  #main = "EEG.edf file before preprocessing"
)
