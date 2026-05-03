if (interactive()) {
###############################################################################
# Clear the cache directory, install the 'empi' program and
# check if the 'empi' program is installed.
###############################################################################
clear.cache()
empi.install()
empi.check()

###############################################################################
# Workflow 1. Data are stored in a CSV file.
###############################################################################
# STEP 1 - read sample data.
file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
signal <- read.csv.signals(file, col.names = "ch1")

# STEP 2 - execute the MP algorithm.
empi.out <- empi.execute(signal = signal)

# STEP 3 - plot a time-frequency map based on MP atoms.
plot(empi.out)

###############################################################################
# Workflow 2. Data are stored in a EDF file (EEG results).
###############################################################################
# STEP 1 - reading a sample EEG file.
file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
out.EEG <- read.edf.signals(file)
signal.EEG <- out.EEG$signals
sampling.rate <- out.EEG$sampling.rate

# STEP 2 - create the EEG montage.
# This step is usually required while processing EEG data.
# Pairs of signals for bipolar montage (so called "double banana").
pairs <- list(
  c("Fp2", "F4"), c("F4", "C4"), c("C4", "P4"), c("P4", "O2"), c("Fp1", "F3"), c("F3", "C3"),
  c("C3", "P3"), c("P3", "O1"), c("Fp2", "F8"), c("F8", "T4"), c("T4", "T6"), c("T6", "O2"),
  c("Fp1", "F7"), c("F7", "T3"), c("T3", "T5"), c("T5", "O1"), c("Fz", "Cz"), c("Cz", "Pz")
)

bip.montage <- eeg.montage(signal.EEG, montage.type = c("bipolar"), bipolar.pairs = pairs)

# STEP 3 - filter the data
# This step is usually required while processing EEG data.
# Filter parameters that will be used (quite typical in filtering EEG signals).
fc <- filters.coeff(
  fs = sampling.rate,
  notch = c(49, 51),
  lowpass = 40,
  highpass = 1,
)

bip.montage.filt <- bip.montage

for (m in 1:ncol(bip.montage)) {
  # 50Hz notch filter
  bip.montage.filt[, m] = signal::filtfilt(fc$notch, bip.montage[, m])
  # Low pass IIR Butterworth
  bip.montage.filt[, m] = signal::filtfilt(fc$lowpass, bip.montage.filt[, m])
  # High pass IIR Butterwoth
  bip.montage.filt[, m] = signal::filtfilt(fc$highpass, bip.montage.filt[, m])
}

signal.EEG.montage.filt <- list(bip.montage.filt, sampling.rate)
names(signal.EEG.montage.filt) <- c("signal", "sampling.rate")

# STEP 4 - execute the MP algorithm.
empi.out <- empi.execute(signal = signal.EEG.montage.filt)

# STEP 5 - plot a time-frequency map based on MP atoms.
plot(empi.out, channel = 2)

###############################################################################
# Saving the empi.execute() results to a SQLite file.
###############################################################################
# Execute the MP algorithm and save the result to a SQLite file.
empi.out <- empi.execute(
  signal = signal,
  write.to.file = TRUE,
  path = ".",
  file.name = "sample1.db")

# Read the atoms parameters.
atom.params(db.file = "sample1.db")

###############################################################################
# empi2tf() function.
###############################################################################
# This function call produces the same effect as plot(empi.out).
out <- empi2tf(
  x = empi.out,
  channel = 1,
  increase.factor = 8,
  out.mode = "plot"
)

# Save the T-F map as a PNG file with the specified dimensions (in pixels).
out <- empi2tf(
  x = empi.out,
  channel = 1,
  increase.factor = 8,
  out.mode = "file",
  path = ".",
  file.name = "sample1.png",
  size = c(512, 512)
)

# Save the T-F map as an RData file containing a matrix with the specified dimensions.
out <- empi2tf(
  x = empi.out,
  channel = 1,
  out.mode = "RData",
  path = ".",
  file.name = "sample1.RData",
  size = c(128, 128)
)

}

