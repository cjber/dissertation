# timer
ptm <- proc.time()

# delete old derived data
unlink("../data/derived/*/*", recursive = FALSE)
unlink("../data/final_data/*", recursive = FALSE)
# scripts
source("00_clean.r")
source("01_samples.r")
source("02_lm.r")
source("03_width.r")

# end timer
proc.time() - ptm
