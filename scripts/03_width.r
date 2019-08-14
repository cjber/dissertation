source("./functions.r")

## ---- widths
road_lm <- fread("../data/derived/model_data/lm_u.csv") %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)

road_fil <- fread("../data/derived/model_data/lm_f.csv") %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)

roads <- st_read("../data/derived/roads/roads_line.gpkg")
road_buff <- st_read("../data/derived/roads/roads_buff.gpkg") %>%
    st_set_crs(27700)

road_lm1 <- road_lm[road_lm$lm1_dum == 1, ]
road_lm15 <- road_lm[road_lm$lm1_dum5 == 1, ]
road_lm2 <- road_lm[road_lm$lm2_dum == 1, ]
road_lm3 <- road_lm[road_lm$lm3_dum == 1, ]
road_glm <- road_lm[road_lm$glm1_dum == 1, ]
road_lmi <- road_fil[road_fil$lmI_dum == 1, ]

linear_models <- list(road_lm1, road_lm15, road_lm2, road_lm3, road_glm, road_lmi)
# includes all filtering, max dist points
linear_models <- lapply(linear_models, max_lines)

# save lines for comparison
for (i in 1:length(linear_models)) {
    st_write(linear_models[[i]], 
             paste0("../data/final_data/road_slines_", i, ".gpkg"),
             layer_options = "OVERWRITE=YES")
}

centrelines <- st_read("../data/derived/roads/roads.gpkg") %>%
    st_set_crs(27700)
####
linear_widths <- lapply(linear_models, model_comparison)

linear_widths <- linear_widths %>%
    reduce(left_join, by = "road_id")

names(linear_widths) <- c(
    "road_id",
    "lm1_mean",
    "lm2_mean",
    "lm3_mean",
    "glm_mean",
    "lmi_mean"
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
