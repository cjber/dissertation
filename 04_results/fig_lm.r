source("../scripts/functions.r")

sampled_las <- fread("../data/derived/model_data/linearmodels.csv")

road_lm <- fread("../data/derived/model_data/linearmodels.csv") %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)

roads <- st_read("../data/derived/roads/roads_line.gpkg", quiet = TRUE)
centrelines <- st_read("../data/derived/roads/roads_line.gpkg", quiet = TRUE) %>%
    st_set_crs(27700)

sample_lines <- st_read("../data/derived/roads/sample_lines.gpkg", quiet = TRUE)

# example road section
rd_f <- "road_6"

road_lm <- road_lm[road_lm$road_id == rd_f, ]
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
