source("../scripts/functions.r")

road_lm <- fread("../data/derived/model_data/lm_u.csv") %>%
  as.data.frame() %>%
  st_as_sf(coords = c("X", "Y"), crs = 27700)
sample_lines <- st_read("../data/derived/roads/sample_lines.gpkg", quiet = TRUE)

road_lm <- road_lm[road_lm$lm1_dum == 1, ]

# remove noise
# chosen example
rd <- road_lm[road_lm$road_id == "road_6", ]
sample_lines <- sample_lines[sample_lines$road_id == "road_6", ]

rd_split <- split(rd, rd$sample_id)

rd_split <- rd_split %>% compact()
rd_split <- lapply(rd_split, filter_samples)

rd_fil <- do.call(rbind, rd_split)
