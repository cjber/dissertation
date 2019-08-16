source("./functions.r")
cent1 <- st_read("../data/derived/roads/cent_iteration1.gpkg") %>%
    st_transform(27700)
sampled_las <- fread("../data/derived/model_data/sampled_las.csv") %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)
aerial <- raster("../data/derived/aerial/aerial_crop.tif")

# improved roads centrelines
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

# individual linear probability model: has to filter out canopy
cent1_las <- split(cent1_las, cent1_las$sample_id)
f1 <- as.formula("road ~ Intensity + lum + dists + Z + NumberOfReturns")
cent1_las <- lapply(cent1_las, lm_compute, f = f1)
cent1_las <- do.call(rbind, cent1_las)

fwrite(cent1_las, "../data/final_data/final_cent1.csv")

lmc <- cent1_las[cent1_las$I_dum == 1, ] %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)
lmc <- list(lmc)


road_buff <- st_read("../data/derived/roads/roads_buff.gpkg")

centrelines <- do.call(rbind, centrelines)
# includes all filtering, max dist points
lmc <- lapply(lmc, max_lines, cents = centrelines)
lmc <- lmc[[1]]

lmc <- lmc[lmc$length < 8 & lmc$length  >2, ]

lmc <- lmc[!is.na(lmc$road_id), ]

# save lines for comparison
st_write(lmc,"../data/derived/model_data/widths_IND.gpkg",
         layer_options = "OVERWRITE=YES")

####
rds <- unique(lmc$road_id)
road_lm <- split(lmc, f = lmc$road_id)

samp <- Filter(function(x) dim(x)[1] > 0, road_lm)
cent <- centrelines[centrelines$road_id %in% rds, ]
cent <- split(cent, f = cent$road_id)
cent <- Filter(function(x) dim(x)[1] > 0, cent)

# here
widths <- mapply(opposite_length, samp, cent)
widths <- do.call(rbind, widths)
widths <- as.data.frame(widths)

widths$opposite <- as.numeric(unfactor(widths$opposite))

widths <- widths[widths$opposite > 2 & widths$opposite < 8, ]

widths_lmc <- widths %>%
    group_by(V2) %>%
    select(road_id = V2, opposite) %>%
    summarise(
        mean_width = mean(opposite)
    )

names(widths_lmc) <- c(
    "road_id",
    "lm_IND_cent1"
)
roads <- fread("../data/final_data/final.csv")

roads <- merge(roads, widths_lmc, by = "road_id")
fwrite(roads, "../data/final_data/final.csv")
