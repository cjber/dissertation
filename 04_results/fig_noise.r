source("../scripts/functions.r")

road_lm <- fread("../data/derived/model_data/linearmodels.csv") %>%
  as.data.frame() %>%
  st_as_sf(coords = c("X", "Y"), crs = 27700)
sample_lines <- st_read("../data/derived/roads/sample_lines.gpkg", quiet = TRUE)

road_lm1 <- road_lm[road_lm$lm1_dum == 1, ]
road_lm90 <- road_lm[road_lm$lm1_dum90 == 1, ]
road_lm80 <- road_lm[road_lm$lm1_dum80 == 1, ]

# remove noise
cent_lm1 <- fread("../data/final_data/cent_lm.csv") %>% 
  as.data.frame() %>%
  st_as_sf(coords = c("X", "Y"), crs = 27700)
cent_lm1 <- cent_lm1[cent_lm1$lm1_dum == 1 & cent_lm1$road_id == "road_6", ]
sample_lines <- sample_lines[sample_lines$road_id == "road_6", ]

rd_split <- split(cent_lm1, cent_lm1$sample_id)

rd_split <- rd_split %>% compact()
rd_split <- lapply(rd_split, filter_samples)

rd_fil <- do.call(rbind, rd_split)
