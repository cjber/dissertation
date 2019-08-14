source("../scripts/functions.r")

cent1 <- st_read("../data/final_data/samples_cent1.gpkg")
cent1_las <- fread("../data/final_data/sampled_cent1.csv") %>% 
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)

cent1_las <- cent1_las[cent1_las$road_id == "road_6" & cent1_las$dum == 1, ]
