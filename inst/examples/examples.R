if (interactive()) {
  ###############################################################################
  # Clear the cache directory, install the 'empi' program,
  # and verify that the installation completed successfully.
  ###############################################################################
  clear.cache()
  empi.install()
  empi.check()

  ###############################################################################
  # Workflow 1. Data stored in a CSV file.
  ###############################################################################
  # STEP 1 - Read sample data.
  file.sample1.csv <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
  signal.sample1.csv <- read.csv.signals(file.sample1.csv, col.names = "ch1")

  # STEP 2 - Run the MP algorithm.
  empi.class <- empi.execute(signal = signal.sample1.csv)

  # STEP 3 - Plot the time-frequency map based on MP atoms.
  plot(empi.class)

  ###############################################################################
  # Workflow 2. Data stored in an EDF file (EEG data).
  ###############################################################################
  # STEP 1 - Read a sample EEG file.
  file.EEG.edf <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
  out.EEG <- read.edf.signals(file.EEG.edf)
  signal.EEG <- out.EEG$signal
  sampling.rate <- out.EEG$sampling.rate

  # STEP 2 - Create the EEG montage.
  # This step is typically required during EEG processing.
  # Signal pairs used to construct the bipolar montage
  # (the so-called "double banana" montage).
  pairs <- list(
    c("Fp2", "F4"), c("F4", "C4"), c("C4", "P4"), c("P4", "O2"), c("Fp1", "F3"), c("F3", "C3"),
    c("C3", "P3"), c("P3", "O1"), c("Fp2", "F8"), c("F8", "T4"), c("T4", "T6"), c("T6", "O2"),
    c("Fp1", "F7"), c("F7", "T3"), c("T3", "T5"), c("T5", "O1"), c("Fz", "Cz"), c("Cz", "Pz")
  )

  bip.montage <- eeg.montage(out.EEG, montage.type = c("bipolar"), bipolar.pairs = pairs)

  # STEP 3 - Filter the data.
  # This step is typically required during EEG processing.
  # Define commonly used EEG filtering parameters.
  fc <- filters.coeff(
    fs = sampling.rate,
    notch = c(49, 51),
    lowpass = 40,
    highpass = 1,
  )

  sig.filt <- bip.montage$signal

  for (m in 1:ncol(bip.montage$signal)) {
    # 50Hz notch filter
    sig.filt[, m] = signal::filtfilt(fc$notch, bip.montage$signal[, m])
    # Low pass IIR Butterworth
    sig.filt[, m] = signal::filtfilt(fc$lowpass, sig.filt[, m])
    # High pass IIR Butterwoth
    sig.filt[, m] = signal::filtfilt(fc$highpass, sig.filt[, m])
  }

  signal.EEG.montage.filt <- list(sig.filt, sampling.rate)
  names(signal.EEG.montage.filt) <- c("signal", "sampling.rate")

  # STEP 4 - Run the MP algorithm.
  empi.class <- empi.execute(signal = signal.EEG.montage.filt)

  # STEP 5 - Plot the time-frequency map based on MP atoms.
  plot(empi.class, channel = 2)

  # STEP 6 - Plot EEG signals after applying the bipolar montage
  # and the filtering procedure.
  temp <- bip.montage
  temp$signal <- sig.filt

  plot(
    x = temp,
    begin = 0,
    end = 10,
    panel.height = NULL,
    rainbow = FALSE,
    bg.colour = "white",
    txt.col = "blue",
    zero.line = TRUE,
    main = "EEG.edf file after banana montage and after filtering"
  )

  ###############################################################################
  # Workflow 3. Data stored in WFDB (WaveForm DataBase) format.
  ###############################################################################
  # STEP 1 - Read sample data.
  file.00001_lr.hea <- system.file("extdata", "00001_lr.hea", package = "MatchingPursuit")
  out.ecg <- read.ecg.signals(file.00001_lr.hea)

  # Create a list compatible with the empi.execute() function.
  signal.ecg <- list(
    signal = data.frame(out.ecg$signal),
    sampling.rate = out.ecg$sampling.rate
  )

  # STEP 2 - Run the MP algorithm.
  empi.class <- empi.execute(signal = signal.ecg)

  # STEP 3 - Plot the time-frequency map based on MP atoms.
  plot(empi.class)

  # STEP 4 - Display ECG signals using a layout corresponding
  # to standard ECG paper.
  # A typical ECG paper layout was used, with a small grid
  # of 0.04 s × 0.1 mV and a large grid of 0.20 s × 0.5 mV.
  plot(
    x = out.ecg,
    begin = 0,
    end = 10,
    panel.height = 1,
    zero.line = FALSE,
    small.squares = TRUE
  )

  ###############################################################################
  # Save empi.execute() results to a SQLite file.
  ###############################################################################
  # Run the MP algorithm and save the results to a SQLite file.
  empi.class <- empi.execute(
    signal = signal.sample1.csv,
    write.to.file = TRUE,
    path = ".",
    file.name = "sample1.db")

  # Read atom parameters.
  atoms <- atom.params(db.file = "sample1.db")
  print(atoms)

  ###############################################################################
  # empi2tf() function.
  ###############################################################################
  # This call produces the same result as plot(empi.class).
  out <- empi2tf(
    x = empi.class,
    channel = 1,
    increase.factor = 8,
    out.mode = "plot"
  )

  # Save the time-frequency map as a PNG file
  # with the specified dimensions (in pixels).
  out <- empi2tf(
    x = empi.class,
    channel = 1,
    increase.factor = 8,
    out.mode = "file",
    path = ".",
    file.name = "sample1.png",
    size = c(512, 512)
  )

  # Save the time-frequency map as an RData file
  # containing a matrix with the specified dimensions.
  out <- empi2tf(
    x = empi.class,
    channel = 1,
    out.mode = "RData",
    path = ".",
    file.name = "sample1.RData",
    size = c(128, 128)
  )

  # Unlike plot.empi(), empi2tf() can also accept
  # a path to a SQLite file created by empi.execute().
  file.sample1.db <- system.file("extdata", "sample1.db", package = "MatchingPursuit")

  out <- empi2tf(
    x = file.sample1.db,
    channel = 1,
    increase.factor = 8,
    out.mode = "plot"
  )

}

