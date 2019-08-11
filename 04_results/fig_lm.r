source("../scripts/functions.r")

filter_las <- fread("../data/derived/model_data/lm_f.csv")

road_lm_filter <- fread("../data/derived/model_data/lm_f.csv") %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)

road_lm_nofilter <- fread("../data/derived/model_data/lm_u.csv") %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)

roads <- st_read("../data/derived/roads/roads_line.gpkg", quiet = TRUE)
centrelines <- st_read("../data/derived/roads/roads_line.gpkg", quiet = TRUE) %>%
    st_set_crs(27700)

# example road section
rd_f <- "road_6"

road_lm_filter <- road_lm_filter[road_lm_filter$road_id == rd_f, ]
road_lm_nofilter <- road_lm_nofilter[road_lm_nofilter$road_id == rd_f, ]

jpgs <- Sys.glob("../data/aerial/*.jpg")
jpgs <- lapply(jpgs, brick)
aerial <- lapply(jpgs, function(x){
return(tryCatch(crop(x, road_lm_nofilter), error=function(e) NULL))
})
aerial <- compact(aerial)
aerial <- do.call(merge, aerial)
