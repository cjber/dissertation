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
road_lm2 <- road_lm[road_lm$lm2_dum == 1, ]
road_lm3 <- road_lm[road_lm$lm3_dum == 1, ]
road_glm <- road_lm[road_lm$glm1_dum == 1, ]
road_lmi <- road_fil[road_fil$lmI_dum == 1, ]

linear_models <- list(road_lm1, road_lm2, road_lm3, road_glm, road_lmi)

linear_models <- lapply(linear_models, function(x) {
    road_lm <- split(x, f = x$sample_id)

    road_lm <- lapply(road_lm, function(s) {
        # assume 2m 4 pts per m
        if (nrow(s) > 8) {
            # remove outlier points
            distances <- s %>%
                st_distance() %>%
                apply(1, FUN = function(y) {
                    min(y[y > 0])
                }) %>%
                as.data.frame() %>%
                mutate(rowid = row_number()) %>%
                select(min_dist = ".", rowid)

            # above 1m from any other point
            distances <- distances[distances$min_dist < 1, ]

            s <- s %>% mutate(rowid = row_number())

            s <- s[s$rowid %in% distances$rowid, ]
            return(s)
        }
    })
    road_lm <- road_lm %>% compact()

    # find two farthest points
    road_lm <- lapply(road_lm, max_dist)
    road_lm <- do.call(rbind, road_lm)
    # find intersecting buffers
    road_lm <- st_join(road_lm, road_buff)

    return(road_lm)
})

road_polys <- lapply(linear_models, function(x) {
    # road polygons
    road_pts <- x %>% st_cast("POINT")
    road_pts <- split(road_pts, road_pts$road_id)
    road_poly <- lapply(road_pts, function(x) {
        x <- x %>%
            st_union() %>%
            st_convex_hull()
        if (class(x)[1] == "sfc_POLYGON") {
            return(x)
        }
    })

    # remove na
    road_poly <- road_poly %>% compact()
    # do.call doesn't work not sure why
    road_poly <- purrr::reduce(road_poly, c)

    return(road_poly)
})

# save polygons
for (i in 1:length(road_polys)) {
    st_write(linear_models[[i]], paste0("../data/final_data/road_polys_", i, ".gpkg"), layer_options = "OVERWRITE=YES")
}

centrelines <- st_read("../data/derived/roads/roads.gpkg") %>%
    st_set_crs(27700)
####
linear_widths <- lapply(linear_models, function(model) {
    road_lm <- model[!is.na(model$roadFunction), ]
    rds <- unique(model$road_id)
    road_lm <- split(road_lm, f = road_lm$road_id)

    samp <- Filter(function(x) dim(x)[1] > 0, road_lm)
    cent <- centrelines[centrelines$road_id %in% rds, ]
    cent <- split(cent, f = cent$road_id)
    cent <- Filter(function(x) dim(x)[1] > 0, cent)

    ## ---- find true widths
    widths <- mapply(opposite_length, samp, cent)
    widths <- do.call(rbind, widths)
    widths <- as.data.frame(widths)

    widths$opposite <- as.numeric(unfactor(widths$opposite))

    widths <- widths[widths$opposite > 2 & widths$opposite < 8, ]

    widths <- widths %>%
        group_by(V2) %>%
        select(road_id = V2, opposite) %>%
        summarise(
            mean_width = mean(opposite)
        )

    return(widths)
})

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
