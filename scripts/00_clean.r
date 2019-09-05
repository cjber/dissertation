# Source Scripts
source("./functions.r")

# Create las catalog with all .laz files
ctg <- catalog("../data/point/")
opt_chunk_size(ctg) <- 500
opt_chunk_buffer(ctg) <- 20

# create lax file to index + speed up process
plan(multisession, workers = 6L)
set_lidr_threads(12L)
# speed up lax computation time
lidR:::catalog_laxindex(ctg)

# ctg to points csv
las <- catalog_apply(ctg, ctg_to_df)
las <- do.call(rbind, las)
las <- las %>%
    select(-c(
        Synthetic_flag,
        Keypoint_flag,
        Withheld_flag
    ))

fwrite(las, "../data/point/points.csv")

# very very slow to read in full gpkg, don't run unless new data added
#roads <- st_read("../data/osroads/oproad_gpkg_gb/data/oproad_gb.gpkg",
#   layer = "RoadLink",
#   query =
#        "SELECT * FROM RoadLink WHERE
#         formOfWay = \"Single Carriageway\" AND
#         roadFunction <> \"Restricted Local Access Road\" "
#) %>%
#st_zm() # remove z axis

#roads <- st_crop(roads, st_bbox(extent(ctg)))
#st_write(roads, "../data/osroads/oproad_crop.gpkg", layer_options = "OVERWRITE=yes")

roads <- st_read("../data/osroads/oproad_crop.gpkg") %>% 
    drop_na(roadNameTOID) %>% 
    group_by(roadNameTOID) %>%
    summarise(len = sum(length)) %>% 
    filter_at(.vars = vars(roadNameTOID), .vars_predicate = any_vars(!is.na(.))) %>% 
    distinct(roadNameTOID, .keep_all = TRUE)

roads$road_id <- paste0("road_", seq.int(nrow(roads)))
roads <- roads %>% 
    ungroup() %>% 
    select(road_id, len, geom, roadNameTOID)

# keep line polys
roads_line <- roads

# one buffer to include non road points, 1m buffer to show only road points
roads_buff <- st_buffer(roads, 30)
roads <- st_buffer(roads, 1)
roads_buff_union <- st_union(roads_buff)

# write all outputs to files
st_write(roads, "../data/derived/roads/roads.gpkg",
    delete_layer = TRUE
)
st_write(roads_line, "../data/derived/roads/roads_line.gpkg",
    delete_layer = TRUE
)
st_write(roads_buff, "../data/derived/roads/roads_buff.gpkg",
    delete_layer = TRUE
)

st_write(roads_buff_union, "../data/derived/roads/roads_buff_diss.gpkg",
    delete_layer = TRUE
)

roads_buff <- st_read("../data/derived/roads/roads_buff.gpkg") %>%
    as_Spatial()
plot(roads_buff)

ctg <- catalog("../data/point/")
opt_output_files(ctg) <- "../data/derived/ctg_clean/{ID}_clean"
opt_chunk_size(ctg) <- 500
opt_chunk_buffer(ctg) <- 20
catalog_apply(ctg, lidr_clean)

ctg <- catalog("../data/derived/ctg_clean/")
opt_output_files(ctg) <- "../data/derived/ctg_buff/{ID}_tile"
opt_chunk_size(ctg) <- 500
opt_chunk_buffer(ctg) <- 20
catalog_apply(ctg, extract_buff, roads_buff)

ctg <- catalog("../data/derived/ctg_buff/")
opt_output_files(ctg) <- "../data/derived/ctg/{ID}_tile"
opt_chunk_size(ctg) <- 500
opt_chunk_buffer(ctg) <- 20
catalog_apply(ctg, las_filter_noise, sensitivity = 1.2)

# non normalised ctg
ctg_notnorm <- catalog("../data/point/")
opt_output_files(ctg_notnorm) <- "../data/derived/ctg_notnorm/{ID}_tile"
opt_chunk_size(ctg_notnorm) <- 500
opt_chunk_buffer(ctg_notnorm) <- 20
catalog_apply(ctg_notnorm, extract_buff, roads_buff)

# read in written roads file
roads <- read_sf("../data/derived/roads/roads.gpkg")

# find roads extent shows study area + used for aerial imagery from digimaps
extent <- st_as_sfc(st_bbox(roads))

# Write extent shapefile
st_write(extent, "../data/derived/extent/extent.gpkg", delete_layer = TRUE)
