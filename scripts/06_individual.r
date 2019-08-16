# global linear model: filtered test
source("./functions.r")
sampled_las <- fread("../data/derived/model_data/sampled_las.csv")
sampled_las <- sampled_las[sampled_las$Classification == 2, ]
samples <- split(sampled_las, f = sampled_las$sample_id)

# remove samples with road outcome that have more than 1 return
filter_las <- lapply(samples, filter_returns)

filter_las <- filter_las %>%
  compact()

filter_las <- do.call(rbind, filter_las)

# remove any NA values in chosen pred + outcomes
filter_las <- filter_las[!is.na(filter_las$Intensity) &
  !is.na(filter_las$lum) & !is.na(filter_las$road) & !is.na(filter_las$Z), ]

filter_las <- split(filter_las, f = filter_las$sample_id)

filter_las <- plyr::compact(filter_las)
filter_las <- do.call(rbind, filter_las)

f1 <- as.formula("road ~ Intensity + lum + dists + Z")
lm1 <- lm(data = filter_las, formula = f1)
lm1_sum <- summary(lm1)
lm1_pred <- predict(lm1, filter_las, type = "response")
filter_las$filter_pred <- lm1_pred
filter_las$filter_dum <- ifelse(filter_las$filter_pred >
  quantile(filter_las$filter_pred, .95), 1, 0)



road_lm <- fread("../data/derived/model_data/linearmodels.csv") %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)

roads <- st_read("../data/final_data/cent_iteration1.gpkg")

road_lm <- filter_las[filter_las$filter_dum == 1, ]
linear_models <- list(
    road_lm,
)

source("./functions.r")
cent1 <- st_read("../data/final_data/cent_iteration1.gpkg") %>%
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

f1 <- as.formula("road ~ Intensity + lum + dists + Z + NumberOfReturns")
lm1 <- lm(data = cent1_las, formula = f1)
pred <- predict(lm1, cent1_las, type = "response")

cent1_las$pred <- pred
cent1_las$dum <- ifelse(cent1_las$pred >
    quantile(cent1_las$pred, .95), 1, 0)

fwrite(cent1_las, "../data/final_data/final_cent1.csv")

lmc <- cent1_las[cent1_las$dum == 1, ] %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)


road_buff <- st_read("../data/derived/roads/roads_buff.gpkg")
# to reuse apply function
coords <- lmc %>%
    st_coordinates()
lmc <- list(lmc)

# includes all filtering, max dist points
centrelines <- st_read("../data/derived/roads/roads_line.gpkg")
lmc <- lapply(lmc, max_lines, cents = centrelines)

lmc <- lmc[[1]]

# save lines for comparison
st_write(lmc,
    paste0("../data/final_data/road_slines_lmc.gpkg"),
    layer_options = "OVERWRITE=YES"
)
####
road_lm <- lmc[!is.na(lmc$roadFunction), ]
rds <- unique(lmc$road_id)
road_lm <- split(road_lm, f = road_lm$road_id)

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

lmc <- widths %>%
    group_by(V2) %>%
    select(road_id = V2, opposite) %>%
    summarise(
        mean_width = mean(opposite)
    )

names(lmc) <- c(
    "road_id",
    "lm1_mean_cent1"
)
roads <- fread("../data/final_data/final.csv")

roads <- merge(roads, lmc, by = "road_id")
fwrite(roads, "../data/final_data/final.csv")

# includes all filtering, max dist points
linear_models <- lapply(linear_models, max_lines, cents = roads)

st_write(linear_models[[1]], "../data/derived/model_data/widths_IND.gpkg")

####
linear_widths <- lapply(linear_models, model_comparison)
linear_widths <- linear_widths %>%
    reduce(left_join, by = "road_id")

names(linear_widths) <- c(
    "road_id",
    "lm IND mean"
)
roads <- merge(roads, linear_widths, by = "road_id")

write.csv(roads, "../data/final_data/final_IND.csv")
