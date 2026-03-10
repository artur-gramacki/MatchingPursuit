# empi download ----
empi.download()

#  csv file --> signal --> empi.execute --> mp2df ----
file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
signal <- read.csv(file, header = TRUE)

empi.out <- empi.execute (
  signal = signal,
  sampling.rate = 1024,
  empi.options = NULL,
  write.to.file = TRUE)

out <- empi2tf(
  db.file = 'empi.db',
  #db.list = empi.out,
  channel = 1,
  mode = "sqrt",
  freqDivide = 4,
  increaseFactor= 4,
  shorteningFactor.x = 2,
  shorteningFactor.y = 2,
  displayCrosses = TRUE,
  displayAtomNumbers = FALSE,
  outMode = "plot"
)

file <- system.file("extdata", "sample2.csv", package = "MatchingPursuit")
signal <- read.csv(file, header = TRUE)

empi.out <- empi.execute (
  signal = signal,
  sampling.rate = 128,
  empi.options = "-o local --gabor -i 50",
  write.to.file = TRUE)

out <- empi2tf(
  #db.file = 'empi.db',
  db.list = empi.out,
  channel = 1,
  mode = "sqrt",
  freqDivide = 1,
  increaseFactor= 4,
  shorteningFactor.x = 2,
  shorteningFactor.y = 2,
  displayCrosses = TRUE,
  displayAtomNumbers = FALSE,
  plotAtoms = TRUE,
  outMode = "plot"
)

# EEG 1 ----
file <- system.file("extdata", "EEG_data_10sec.edf", package = "MatchingPursuit")

# Read signal main parameters.
pars <- read.edf.params(file)
pars

# Extract signals from EDF file.
signal <- read.edf.signals(file, resampling = FALSE, from = 2, to = 6)
head(signal)

sampling.rate <- pars$frequency[1]
sampling.rate

empi.out <- empi.execute (
  signal = signal,
  sampling.rate = sampling.rate,
  empi.options = NULL,
  write.to.file = TRUE)

out <- empi2tf(
  #db.file = 'empi.db',
  db.list = empi.out,
  channel = 1,
  mode = "sqrt",
  freqDivide = 4,
  increaseFactor= 4,
  shorteningFactor.x = 2,
  shorteningFactor.y = 2,
  displayCrosses = TRUE,
  displayAtomNumbers = FALSE,
  outMode = "plot"
)

# EEG 2 (montage + filtering) ----
file <- system.file("extdata", "EEG_data_10sec.edf", package = "MatchingPursuit")

# Read signal main parameters.
pars <- read.edf.params(file)

# Extract signals from EDF file.
signal <- read.edf.signals(file, resampling = FALSE, from = 2, to = 6)

sampling.rate <- pars$frequency[1]

# Pairs of signals for bipolar montage (so called "double banana").
pairs <- list(
  c("Fp2", "F4"),
  c("F4",  "C4"),
  c("C4",  "P4"),
  c("P4",  "O2"),
  c("Fp1", "F3"),
  c("F3",  "C3"),
  c("C3",  "P3"),
  c("P3",  "O1"),
  c("Fp2", "F8"),
  c("F8",  "T4"),
  c("T4",  "T6"),
  c("T6",  "O2"),
  c("Fp1", "F7"),
  c("F7",  "T3"),
  c("T3",  "T5"),
  c("T5",  "O1"),
  c("Fz",  "Cz"),
  c("Cz",  "Pz")
)

# Channel that will be analyzed.
ch <- 1

par(mfrow = c(2, 1), pty = "m")

# Make the bipolar montage.
bip.montage <- eeg.montage(signal, montage.type = c("bipolar"), bipolar.pairs = pairs)
plot(
  bip.montage[, ch],
  type = "l",
  panel.first = grid(),
  main = paste(colnames(bip.montage)[ch], " (raw signal)", sep = "")
)

# Filter parameters that will be used (quite typical in filtering EEG signals).
fc <- filters.coeff(
   fs = sampling.rate,
   notch = c(49, 51),
   lowpass = 40,
   highpass = 1,
)

# Filtering input signals.
for (m in 1:ncol(bip.montage)) {
  bip.montage[, m] = signal::filtfilt(fc$bf.notch, bip.montage[, m]); # 50Hz notch filter
  bip.montage[, m] = signal::filtfilt(fc$bf.low, bip.montage[, m]); # Low pass IIR Butterworth
  bip.montage[, m] = signal::filtfilt(fc$bf.high, bip.montage[, m]); # High pass IIR Butterwoth
}
plot(
  bip.montage[, ch],
  type = "l",
  panel.first = grid(),
  main = paste(colnames(bip.montage)[ch], " (filtered signal)", sep = "")
)

empi.out <- empi.execute (
  signal = bip.montage,
  sampling.rate = sampling.rate,
  empi.options = NULL,
  write.to.file = TRUE)

out <- empi2tf(
  db.file = NULL,
  db.list = empi.out,
  channel = 1,
  mode = "sqrt",
  freqDivide = 4,
  increaseFactor= 4,
  shorteningFactor.x = 2,
  shorteningFactor.y = 2,
  displayCrosses = TRUE,
  displayAtomNumbers = FALSE,
  outMode = "plot"
)

n.channnels <- ncol(empi.out$original.signal)
original.signal <- empi.out$original.signal
reconstruction <- empi.out$reconstruction
t <- empi.out$t
f <- empi.out$f

for (k in (1:n.channnels)) {
  par(mfrow = c(2, 1), pty = "m")
  par(mai = c(0.9, 0.5, 0.2, 0.4))
  plot(
    original.signal[,k], type="l", col = "blue",
    main = paste("channel: ", k, " / " , n.channnels, sep = ""),
    xaxt = "n", ylab = "", xlab = "time [sec]"
  )

  len <- length(original.signal[, 1])
  lab <- seq(t[1], t[len] + 1 / f, length.out = 11)

  axis(side = 1, las = 1, cex.axis = 0.9, at = seq(0, len, length.out = 11), labels = lab)
  plot(reconstruction[,k], type="l", col = "blue", xaxt = "n", ylab = "", xlab = "time [sec]")
  axis(side = 1, las = 1, cex.axis = 0.9, at = seq(0, len, length.out = 11), labels = lab)
}

