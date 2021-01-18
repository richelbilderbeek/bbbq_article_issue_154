library(testthat)
expect_true(pureseqtmr::is_pureseqtm_installed())
fasta_filename <- tempfile(fileext = "_UP000005640.fasta")
download.file(
  url = "https://www.richelbilderbeek.nl/UP000005640.fasta",
  destfile = fasta_filename
)
expect_true(file.exists(fasta_filename))
# We know this reference proteome has 75004 proteins
expect_equal(
  75004,
  nrow(pureseqtmr::load_fasta_file_as_tibble_cpp(fasta_filename))
)
tmhmm_filename <- tempfile(fileext = "_UP000005640.tmhmm")

if (1 == 2) {
  # The original file does not work, takes minutes for this to find out
  expect_error(
    tmhmm::run_tmhmm_to_file(
      fasta_filename = fasta_filename,
      tmhmm_filename = tmhmm_filename
    ),
    "Character 'U' not allowed in alphabet 'ACDEFGHIKLMNPQRSTVWYBXZ'."
  )

  # Remove all proteins with a selenocysteine
  fasta_filename <- "UP000005640.fasta"
  t <- pureseqtmr::load_fasta_file_as_tibble_cpp(fasta_filename)
  # Remove the Us
  t <- t[ -stringr::str_which(string = t$sequence, pattern = "U"), ]
  nrow(t)
  pureseqtmr::save_tibble_as_fasta_file(t = t, fasta_filename = fasta_filename)
}

tmhmm::run_tmhmm_to_file(
  fasta_filename = fasta_filename,
  tmhmm_filename = tmhmm_filename
)

expect_true(file.exists(tmhmm_filename))
expect_equal(
  nrow(pureseqtmr::load_fasta_file_as_tibble_cpp(fasta_filename)),
  nrow(pureseqtmr::load_fasta_file_as_tibble_cpp(tmhmm_filename))
)

