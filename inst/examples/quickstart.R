library(MatchingPursuit)

# +-------------------------------------------------------------+
# Clear the cache directory, install the 'empi' program,
# and verify that the installation completed successfully.
# +-------------------------------------------------------------+
clear_cache()
empi_install()
empi_check()

# +-------------------------------------------------------------+
# EMPI workflow
# +-------------------------------------------------------------+
# STEP 1 - Read sample data.
file_sample1_csv <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
signal_sample1_csv <- read_csv_signals(file_sample1_csv, col_names = "ch1")

# STEP 2 - Run the MP algorithm.
empi_class <- empi_execute(
  signal = signal_sample1_csv
)

# STEP 2.bis - Run the MP algorithm.
# Default EMPI options have been changed. For details, see the EMPI README.md file.
empi_class <- empi_execute(
  signal = signal_sample1_csv,
  empi_options = "-o none --full-atoms-in-signal -i 50 --gabor"
)

# STEP 3 - Plot the time-frequency map based on MP atoms.
# plot_empi() is the S3 method for the generic plot() function.
# It requires an object of class empi, created with empi_execute().
plot(empi_class, freq_divide = 4)

# STEP 3.bis — This call gives the same result as plot(empi_class).
out <- tf_map(
  x = empi_class,
  channel = 1,
  increase_factor = 8,
  out_mode = "plot"
)

# +-------------------------------------------------------------+
# Orthogonal Matching Pursuit (OMP) workflow
# +-------------------------------------------------------------+
# STEP 1 - Read sample data.
sig_file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
sample1 <- read_csv_signals(sig_file, col_names_in_csv = FALSE)

signal <- sample1$signal
sf <- sample1$sampling_frequency
duration <- nrow(sample1$signal) / sf

# STEP 2 - Read dictionary.
xml_file <- system.file("extdata", "sample1_dict.xml", package = "MatchingPursuit")

atoms_dict <- read_dict(
  xml_file,
  sf,
  duration,
  verbose = TRUE)

# STEP 3 - Preselection of candidate atoms.
dict_topk <- topk_atoms(
  atoms_dict = atoms_dict,
  signal = signal,
  sf = sf,
  topk = 5000,
  verbose = TRUE
)

# STEP 4 - OMP decomposition.
fit <- mp_omp_execute(
  mode = "omp",
  dictionary = dict_topk,
  signal = signal,
  sf = sf,
  n_nonzero_coefs = 50,
  verbose = TRUE
)

# STEP 4 - Plot the time-frequency map based on PMP atoms.
plot(fit, channel = 1, freq_divide = 4)

# Combine steps 1–4 into a single pipeline.
# The final result is identical as in step 4.
out <- mp_omp_run_pipeline(
  mode = "omp",
  sig_file = sig_file,
  col_names_in_csv = FALSE,
  xml_file = xml_file,
  topk = 5000,
  n_nonzero_coefs = 50,
  verbose = TRUE
)

class(out)
plot(out, channel = 1, freq_divide = 4)

# +-------------------------------------------------------------+
# Matching Pursuit (MP) workflow
# +-------------------------------------------------------------+
# We're using the same functions here as in the above workflow.
# The "mode" parameter is now set to "mp." Technically, this calls
# the "mp_core()" function instead of "opm_core()".
out <- mp_omp_run_pipeline(
  mode = "mp",
  sig_file = sig_file,
  col_names_in_csv = FALSE,
  xml_file = xml_file,
  topk = 5000,
  n_nonzero_coefs = 50,
  verbose = TRUE
)

plot(out, channel = 1, freq_divide = 4)

# +-------------------------------------------------------------+
# Data stored in an EDF file (EEG data).
# EMPI is used.
# +-------------------------------------------------------------+
# STEP 1 - Read a sample EEG file.
file_EEG_edf <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
out_EEG <- read_edf_signals(file_EEG_edf)
signal_EEG <- out_EEG$signal
sampling_frequency <- out_EEG$sampling_frequency

# STEP 2 - Create the EEG montage.
# This step is typically required during EEG processing.
# Signal pairs used to construct the bipolar montage
# (the so-called "double banana" montage).
pairs <- list(
  c("Fp2", "F4"), c("F4", "C4"), c("C4", "P4"), c("P4", "O2"), c("Fp1", "F3"), c("F3", "C3"),
  c("C3", "P3"), c("P3", "O1"), c("Fp2", "F8"), c("F8", "T4"), c("T4", "T6"), c("T6", "O2"),
  c("Fp1", "F7"), c("F7", "T3"), c("T3", "T5"), c("T5", "O1"), c("Fz", "Cz"), c("Cz", "Pz")
)

bip_montage <- eeg_montage(out_EEG, montage_type = c("bipolar"), bipolar_pairs = pairs)

# STEP 3 - Filter the data.
# This step is typically required during EEG processing.
# Define commonly used EEG filtering parameters.
fc <- filters_coeff(
  sf = sampling_frequency,
  notch = c(49, 51),
  lowpass = 40,
  highpass = 1,
)

sig_filt <- bip_montage$signal

for (m in 1:ncol(sig_filt)) {
  # 50Hz notch filter
  sig_filt[, m] = signal::filtfilt(fc$notch, sig_filt[, m])
  # Low pass IIR Butterworth
  sig_filt[, m] = signal::filtfilt(fc$lowpass, sig_filt[, m])
  # High pass IIR Butterwoth
  sig_filt[, m] = signal::filtfilt(fc$highpass, sig_filt[, m])
}

signal_EEG_montage_filt <- list(sig_filt, sampling_frequency)
names(signal_EEG_montage_filt) <- c("signal", "sampling_frequency")

# STEP 4 - Run the MP algorithm.
empi_class <- empi_execute(signal = signal_EEG_montage_filt)

# STEP 5 - Plot the time-frequency map based on MP atoms.
plot(empi_class, channel = 2)

# STEP 6 - Plot EEG signals after applying the bipolar montage
# and the filtering procedure.
temp <- bip_montage
temp$signal <- sig_filt

plot(
  x = temp,
  begin = 0,
  end = 10,
  panel_height = NULL,
  rainbow = FALSE,
  bg_colour = "white",
  txt_col = "blue",
  zero_line = TRUE,
  main = "EEG.edf file after banana montage and after filtering"
)

# +-------------------------------------------------------------+
# Data stored in WFDB (WaveForm DataBase) format.
# EMPI is used.
# +-------------------------------------------------------------+
# STEP 1 - Read sample data.
file_00001_lr_hea <- system.file("extdata", "00001_lr.hea", package = "MatchingPursuit")
out_ecg <- read_ecg_signals(file_00001_lr_hea)

# Create a list compatible with the empi.execute() function.
signal_ecg <- list(
  signal = data.frame(out_ecg$signal),
  sampling_frequency = out_ecg$sampling_frequency
)

# STEP 2 - Run the MP algorithm.
empi_class <- empi_execute(signal = signal_ecg)

# STEP 3 - Plot the time-frequency map based on MP atoms.
plot(empi_class)

# STEP 4 - Display ECG signals using a layout corresponding
# to standard ECG paper.
# A typical ECG paper layout was used, with a small grid
# of 0.04 s × 0.1 mV and a large grid of 0.20 s × 0.5 mV.
plot(
  x = out_ecg,
  begin = 0,
  end = 10,
  panel_height = 1,
  zero_line = FALSE,
  small_squares = TRUE
)

# +-------------------------------------------------------------+
# Save empi_execute() results to a SQLite file.
# +-------------------------------------------------------------+
# Run the MP algorithm and save the results to a SQLite file.
empi_class <- empi_execute(
  signal = signal_sample1_csv,
  write_to_file = TRUE,
  path = ".",
  file_name = "sample1.db")

# Read atom parameters.
atoms <- atom_params(db_file = "sample1.db")
print(atoms)

# +-------------------------------------------------------------+
# Some useful options in tf_map() function.
# +-------------------------------------------------------------+
# Save the time-frequency map as a PNG file
# with the specified dimensions (in pixels).
out <- tf_map(
  x = empi_class,
  channel = 1,
  increase_factor = 8,
  out_mode = "file",
  path = ".",
  file_name = "sample1.png",
  size = c(512, 512)
)

# Save the time-frequency map as an RData file
# containing a matrix with the specified dimensions.
out <- tf_map(
  x = empi_class,
  channel = 1,
  out_mode = "RData",
  path = ".",
  file_name = "sample1.RData",
  size = c(128, 128)
)

# Unlike plot_empi(), tf_map() can also accept
# a path to a SQLite file created by empi_execute().
file_sample1_db <- system.file("extdata", "sample1.db", package = "MatchingPursuit")

out <- tf_map(
  x = file_sample1_db,
  channel = 1,
  increase_factor = 8,
  out_mode = "plot"
)
