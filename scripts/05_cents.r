source("./functions.r")
cent1 <- st_read("../data/final_data/cent_iteration1.gpkg") %>% 
    st_transform(27700)
sampled_las <- fread("../data/derived/model_data/sampled_las.csv") %>% 
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)
aerial <- raster("../data/derived/aerial/aerial_crop.tif")

roads <- cent1 %>%
    st_buffer(2)

roads_df <- roads %>% st_drop_geometry()

joined_output <- merge(sampled_las, roads_df, by = "road_id")

int <- st_contains(roads, joined_output, sparse = FALSE) %>%
    colSums()

joined_output$road <- int

# turn to binary, some road buffers overlap
joined_output$road <- as.numeric(joined_output$road > 0)

# crop aerial data
lum <- raster::extract(aerial, joined_output)
joined_output$lum <- as.numeric(lum)


# find dists from centrelines
joined_output <- split(joined_output, f = joined_output$road_id)
centrelines <- split(cent1, cent1$road_id)

centrelines <- centrelines[names(joined_output)]

dists <- mapply(
    find_dists,
    joined_output,
    centrelines
)

joined_output <- do.call(rbind, joined_output)
dists <- do.call(rbind, dists)
joined_output$dists <- dists

coords <- joined_output %>%
    st_coordinates()
cent1_las <- joined_output %>%
    st_drop_geometry() %>%
    mutate(
        X = coords[, 1],
        Y = coords[, 2]
    )

f1 <- as.formula("road ~ Intensity + lum + dists + Z")
lm1 <- lm(data = cent1_las, formula = f1)
pred <- predict(lm1, cent1_las, type = "response")

cent1_las$pred <- pred
cent1_las$dum <- ifelse(cent1_las$pred >
  quantile(cent1_las$pred, .95), 1, 0)

fwrite(cent1_las, "../data/final_data/sampled_cent1.csv")
