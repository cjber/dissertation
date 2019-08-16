# timer
ptm <- proc.time()

# delete old derived data
unlink("../data/derived/*/*", recursive = FALSE)
unlink("../data/final_data/*", recursive = FALSE)
# scripts
source("00_clean.r")
gc()
print("clean done")
source("01_samples.r")
gc()
print("samples done")
source("02_lm.r")
gc()
print("lm done")
source("03_width.r")
gc()
print("width done")
source("04_cents.r")
print("cents done")

# end timer
proc.time() - ptm
