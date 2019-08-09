source("../scripts/functions.r")

filter_las <- fread("../data/derived/model_data/lm_f.csv")

road_lm_filter <- fread("../data/derived/model_data/lm1_las_f.csv") %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)

road_lm_nofilter <- fread("../data/derived/model_data/lm_u.csv") %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)

roads <- st_read("../data/derived/roads/roads_line.gpkg")
centrelines <- st_read("../data/derived/roads/roads_line.gpkg") %>%
    st_set_crs(27700)

