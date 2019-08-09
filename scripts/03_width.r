source("./functions.r")
## ---- widths
road_lm <- fread("../data/derived/model_data/lm_f.csv") %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)

roads <- st_read("../data/derived/roads/roads_line.gpkg")
centrelines <- st_read("../data/derived/roads/roads_line.gpkg") %>%
    st_set_crs(27700)

road_lm1 <- road_lm[road_lm$lm1_pred == 1, ]
ggplot() +
    geom_point(data = road_lm, aes(x = index(road_lm), y = ))

road_lm1 <- split(road_lm1, f = road_lm1$sample_id)
# remove noise
s <- road_lm1[[56]]
aerial <- raster("../data/derived/aerial/aerial_crop.tif")
a_crop <- crop(aerial, road_lm[road_lm$road_id == unique(s$road_id), ])

distances <- s %>%
    st_distance(by_element = FALSE) %>%
    unclass() %>%
    "[<-"(lower.tri(., diag = TRUE), NA) %>%
    as_tibble() %>%
    rowid_to_column() %>%
    gather(colid, distance, starts_with("V"),
        na.rm = TRUE
    ) %>%
    arrange(distance)

distances <- distances[!duplicated(distances$rowid), ] %>% 
    arrange(desc(distance))

distances <- distances[distances$distance < 1, ]

s <- s %>% mutate(rowid = row_number())

s <- s[s$rowid %in% distances$rowid, ]

plot(abs(diff(s$gpstime)))
max(abs(diff(s$gpstime)))
plot(s$gpstime)

plot(a_crop)
plot(s, add = TRUE, colour = "red")


road_lm1 <- lapply(road_lm1, max_dist)
road_lm1 <- do.call(rbind, road_lm1)

# find intersecting centrelines
road_lm1 <- st_join(road_lm1, centrelines, join = st_crosses)
# remove non intersecting lines
road_lm1 <- road_lm1[!is.na(road_lm1$roadFunction), ]

rds <- unique(road_lm1$road_id)

road_lm1 <- split(road_lm1, f = road_lm1$road_id)

# remove empty dataframes
samp <- Filter(function(x) dim(x)[1] > 0, road_lm1)
centrelines <- centrelines[centrelines$road_id %in% rds, ]
cent <- split(centrelines, f = centrelines$road_id)
cent <- Filter(function(x) dim(x)[1] > 0, cent)

widths <- mapply(opposite_length, samp, cent)
widths <- do.call(rbind, widths)
widths <- as.data.frame(widths)

names(widths) <- c("width", "road_id")
widths <- widths %>%
    as.data.frame() %>%
    group_by(road_id) %>%
    summarise(width = mean(as.numeric(unfactor(width))))

roads <- merge(roads, widths, by = "road_id")

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
        mean_angle = mean(angle),
        max_angle = max(angle)
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

las_rds <- lapply(las_rds, function(x) {
    if (max(x$NumberOfReturns) == 1) {
        return(x)
    }
})
las_rds <- las_rds %>%
    compact()
las_rds <- do.call(rbind, las_rds)

las_height <- split(las_rds, las_rds$road_id)

las_height <- lapply(las_height, function(x) {
    elev <- c()
    samples <- split(x, x$sample_id)
    if (length(samples) > 2) {
        for (s in 2:length(samples) - 1) {
            pair <- samples[c(s, s + 1)]
            n1 <- mean(pair[[1]]$Z)
            n2 <- mean(pair[[2]]$Z)
            e <- abs(n1 - n2)
            e <- cbind(
                as.character(unique(samples[[s]]$road_id)), e
            )
            elev <- rbind(elev, e)
        }
    }
    return(elev)
})

las_height <- do.call(rbind, las_height)
las_height <- as.data.frame(las_height)

names(las_height) <- c("road_id", "Z")
las_height <- las_height %>%
    group_by(road_id) %>%
    summarise(
        max_Z = max(as.numeric(unfactor(Z))),
        mean_Z = mean(as.numeric(unfactor(Z))),
    ) %>%
    drop_na()

roads <- merge(roads, las_height, by = "road_id")

## ---- surface_qual

las_qual <- las_rds %>%
    group_by(road_id) %>%
    summarise(
        mean_int = mean(Intensity),
        range_int = max(Intensity) - min(Intensity)
    ) %>%
    drop_na() %>%
    select(c(road_id, mean_int, range_int))

roads <- merge(roads, las_qual, by = "road_id") %>%
    st_drop_geometry()

write.csv(roads, "../data/final_data/final.csv")
