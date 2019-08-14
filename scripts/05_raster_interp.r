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
x <- linear_models[[1]]
x <- x[x$road_id == "road_6", ] %>% 
    as_Spatial()


# Create a raster, give it the same extent as the points
# and define rows and columns:

rast <- raster()
extent(rast) <- extent(x) # this might be unnecessary

# And then ... rasterize it! This creates a grid version 
# of your points using the cells of rast, values from the IP field:
rast2 <- rasterize(x, rast, x$lm1_pred, fun=mean) 
interpolate(rast2)
