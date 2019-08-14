source("../scripts/functions.r")

sampled_las <- fread("../data/derived/model_data/lm_u.csv")
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

sample_lines <- st_read("../data/derived/roads/sample_lines.gpkg", quiet = TRUE)

# example road section
rd_f <- "road_6"

road_lm_filter <- road_lm_filter[road_lm_filter$road_id == rd_f, ]
road_lm_nofilter <- road_lm_nofilter[road_lm_nofilter$road_id == rd_f, ]
sample_lines <- sample_lines[sample_lines$road_id == rd_f, ]
centrelines <- centrelines[centrelines$road_id == rd_f, ] %>% 
    st_crop(sample_lines)

jpgs <- Sys.glob("../data/aerial/*.jpg")
jpgs <- lapply(jpgs, brick)
aerial <- lapply(jpgs, function(x){
return(tryCatch(crop(x, sample_lines), error=function(e) NULL))
})

aerial <- aerial %>% 
    compact() %>% 
    brick()
