file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
out  <- read_edf_signals(file, resampling = FALSE)
signal <- as_sig(out$signal, out$sampling_frequency)
sampling_frequency <- out$sampling_frequency
duration <- length(out$time) / sampling_frequency

out_empi <- empi_execute(
  signal = signal,
  empi_options = "-o local --full-atoms-in-signal -i 50 --gabor",
  #empi_options = "-o local -i 50 --gabor --dictionary-output EEG_empi.xml"
)
plot(out_empi, channel = 1)

dict <- generate_xml_dict(N = duration, file = 'EEG.xml')

atoms_dict <- read_gabor_dict(
  xml_file = 'EEG_empi.xml',
  sampling_frequency = sampling_frequency,
  duration = duration,
  verbose = TRUE,
  full_atoms_in_signal = FALSE
)

out_empi <- empi_execute(
   signal = signal,
 #empi_options = "-o none -i 10 --gabor",
 #empi_options = "-o local -i 10 --gabor",
 #empi_options = "-o global -i 10 --gabor",
 empi_options = "-o local -i 50 --gabor",
   write_to_file = FALSE,
   path = NULL,
   file_name = "my_decomposition.db"
 )
 plot(out_empi, freq_divide = 8)
 summary(out_empi)

 out_empi$atoms$frequency
 out_empi$atoms$phase
 out_empi$atoms$scale
 out_empi$atoms$position


file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
out  <- read_edf_signals(file, resampling = FALSE)
signal <- as_sig(out$signal[,1], out$sampling_frequency)

system.time({
fit_mp <- mp_omp_execute(
   mode = "mp",
   signal = signal,
   n_nonzero_coefs = 50,
   topk = 5000,
   verbose = TRUE
 )
})
plot(fit_mp, freq_divide = 8)
summary(fit_mp)

# test ----

library(MatchingPursuit)

eeg_file <- system.file("extdata","EEG.edf", package = "MatchingPursuit")
out <- read_edf_signals(eeg_file)
sampling_frequency <- out$sampling_frequency
duration <- nrow(out$signal) / sampling_frequency
xml_file <- "EEG.xml"
signal <- as_sig(out$signal, out$sampling_frequency)

sig_file <- system.file("extdata","sample2.csv",package = "MatchingPursuit")
signal <- read_csv_signals(sig_file,col_names_in_csv = F)
sampling_frequency <- signal$sampling_frequency
duration <- nrow(signal$signal) / sampling_frequency
xml_file <- system.file("extdata",  "sample2.xml",  package = "MatchingPursuit")

system.time({
out_mp <- mp_omp_execute(
  mode = "mp",
  signal = signal,
  n_nonzero_coefs = 50,
  topk = 1000,
  sparse = TRUE,
  verbose = TRUE
)})
plot(out_mp, freq_divide = 1)
out <- tf_map(out_mp, channel = 1, freq_divide = 8)
summary(out_mp)

system.time({
out_empi <- empi_execute(
  signal = signal,
  empi_options = "-o local -i 50 --gabor -r 0.00000000001",
  ignore.stdout = TRUE,
  ignore.stderr = TRUE
)})
plot(out_empi, freq_divide = 1)
out <- tf_map(out_empi, channel = 1, freq_divide = 8)
summary(out_empi)


atoms_dict <- read_gabor_dict(
  xml_file = xml_file,
  sampling_frequency = sampling_frequency,
  duration = duration,
  verbose = TRUE,
  full_atoms_in_signal = FALSE
)

system.time({
out_topk <- topk_gabor_atoms(
  atoms_dict = atoms_dict,
  signal = signal,
  topk = 40000,
  verbose = TRUE
)
})

system.time({
  out_topk_m <- topk_gabor_atoms_materialized(
    atoms_dict = atoms_dict,
    signal = signal,
    topk = 40000,
    verbose = TRUE
  )
})


system.time({D_sparse <- gabor_atoms_matrix(out_topk)})
system.time({D_dense <- gabor_atoms_matrix(out_topk, sparse = FALSE)})

identical(out_topk_m$atoms[[1]], as.matrix(D_sparse))
identical(out_topk_m$atoms[[1]], as.matrix(D_dense))

object.size(D_dense)
object.size(D_sparse)
as.numeric(object.size(D_sparse) * 100 / object.size(D_dense))
mean(D_dense == 0)


for (i in 39900:40000) {
  txt = paste(
    "atom:", i, ", ",
    "f=", format(out_topk_m$frequency[i,1], digits = 2),
    ", phase=", format(out_topk_m$phase[i,1], digits = 2),
    ", scale=", format(out_topk_m$scale[i,1], digits = 2),
    ", pos=", format(out_topk_m$position[i,1], digits = 2),
    ", begin=", format(out_topk_m$atom_begin[i,1], digits = 2),
    ", len=", format(out_topk_m$window_len[i,1], digits = 2), sep = ""
    )
  plot(out_topk_m$atoms[[1]][,i], type = "l", main = txt)
}

for (i in 39900:40000) {
  txt = paste("atom:", i, sep = "")
  plot(D_dense[,i], type = "l", main = txt)
}





source("R/materialize_gabor_atoms_optimized2.R", keep.source = TRUE)
Rprof("materialize_dense2.out", interval = 0.001, line.profiling = TRUE)
D_dense_opt2 <- materialize_gabor_atoms_optimized2(out_topk, sparse = FALSE)
Rprof(NULL)
summaryRprof("materialize_dense2.out", lines = "show")$by.self


system.time({
for (ch in seq_len(ncol(sig))) {

  signal_one_ch <- as_sig(sig[, ch], sampling_frequency)

  topk_atoms <- topk_gabor_atoms(
    atoms_dict = atoms_dict,
    signal = signal_one_ch,
    topk = topk,
    verbose = F
  )
}
})

system.time({
    topk_atoms_N <- topk_gabor_atoms(
      atoms_dict = atoms_dict,
      signal = signal,
      topk = topk,
      verbose = F
    )
})









