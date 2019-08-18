source("../scripts/functions.r")

cent1 <- st_read("../data/derived/roads/cent_iteration1.gpkg", quiet = TRUE)
cent1_las <- fread("../data/final_data/cent_lm.csv") %>% 
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)

cent1_las <- cent1_las[cent1_las$road_id == "road_6" & cent1_las$lm1_dum == 1, ]
cent1 <- cent1[cent1$road_id == "road_6", ]

cent1 <- cent1[cent1$road_id == rd_f, ] %>%
    st_crop(sample_lines)

cent1_las <- split(cent1_las, cent1_las$sample_id)

cent1_las <- cent1_las %>% compact()
cent1_las <- lapply(cent1_las, filter_samples)

cent1_las <- do.call(rbind, cent1_las)
