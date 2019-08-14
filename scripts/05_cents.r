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

lmc <- cent1_las[cent1_las$dum == 1, ] %>% 
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)

# to reuse apply function
lmc <- list(lmc)
lmc <- lapply(lmc, max_lines)

linear_widths <- lapply(lmc, model_comparison)

linear_widths <- linear_widths %>%
    reduce(left_join, by = "road_id")

names(linear_widths) <- c(
    "road_id",
    "lmc_mean"
)

roads <- merge(roads, linear_widths, by = "road_id")

## ---- angles
roads_split <- st_read("../data/derived/roads/roads_line.gpkg") %>%
    st_cast("POINT") %>%
    st_set_crs(27700)

roads_split <- split(roads_split, roads_split$road_id)

angles <- lapply(roads_split, road_angles)
angles <- do.call(rbind, angles)
row.names(angles) <- NULL
angles <- angles %>%
    as.data.frame()
names(angles) <- c("angle", "road_id")
angles$angle <- as.numeric(unfactor(angles$angle))

angles <- angles %>%
    group_by(road_id) %>%
    summarise(
        meanAngle = mean(angle),
        maxAngle = max(angle)
    )

roads <- merge(roads, angles, by = "road_id")

## ---- heights
# Non-normalised las files
sample_lines <- st_read("../data/derived/roads/sample_lines.gpkg") %>%
    st_set_crs(27700)
roads_1m <- st_read("../data/derived/roads/roads.gpkg")
ctg <- catalog("../data/derived/ctg_notnorm/")
opt_chunk_size(ctg) <- 500
plan(multisession, workers = 6L)
set_lidr_threads(12L)

# remove points outside samples
las_rds <- catalog_apply(ctg, clip_samples, sample_lines)
las_rds <- do.call(rbind, las_rds)

las_rds <- las_rds[las_rds$NumberOfReturns == 1 &
    las_rds$Classification == 2, ]

rds <- st_read("../data/derived/roads/roads.gpkg") %>%
    st_transform(27700)

roads_df <- rds %>% st_drop_geometry()

las_rds <- las_rds %>%
    st_transform(27700)

las_rds <- merge(las_rds, roads_df, by = "road_id")

int <- st_contains(roads_1m, las_rds, sparse = FALSE) %>%
    colSums()

las_rds$road <- int

# remove overlapping road points
las_rds <- las_rds[las_rds$road < 2, ]
# turn to binary (might not be needed)
las_rds$road <- as.numeric(las_rds$road > 0)

las_rds <- las_rds[las_rds$road == 1, ]

las_rds <- split(las_rds, las_rds$sample_id)

las_rds <- lapply(las_rds, filter_returns)

las_rds <- las_rds %>%
    compact()

las_rds <- do.call(rbind, las_rds)

las_height <- split(las_rds, las_rds$road_id)

las_height <- lapply(las_height, height_change)

las_height <- do.call(rbind, las_height)
las_height <- as.data.frame(las_height)

names(las_height) <- c("road_id", "Z")
las_height <- las_height %>%
    group_by(road_id) %>%
    summarise(
        maxZ = max(as.numeric(unfactor(Z))),
        meanZ = mean(as.numeric(unfactor(Z))),
    ) %>%
    drop_na()

roads <- merge(roads, las_height, by = "road_id")

## ---- surface_qual
las_qual <- las_rds %>%
    group_by(road_id) %>%
    summarise(
        meanInt = mean(Intensity),
        rangeInt = max(Intensity) - min(Intensity)
    ) %>%
    drop_na() %>%
    select(c(road_id, meanInt, rangeInt))

roads <- merge(roads, las_qual, by = "road_id") %>%
    st_drop_geometry()

write.csv(roads, "../data/final_data/final.csv")
