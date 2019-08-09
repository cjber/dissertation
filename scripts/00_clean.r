# Source Scripts
source("./functions.r")

# Create las catalog with all .laz files
ctg <- catalog("../data/point/")
opt_chunk_size(ctg) <- 500

# create lax file to index + speed up process
plan(multisession, workers = 6L)
set_lidr_threads(12L)
lidR:::catalog_laxindex(ctg)

# run once
las <- catalog_apply(ctg, ctg_to_df)
las <- do.call(rbind, las)

fwrite(las, "../data/point/points.csv")

# filter using sql expressions why not
roads <- st_read("../data/osroads/oproad_gpkg_gb/data/oproad_gb.gpkg",
    layer = "RoadLink", query =
        "SELECT * FROM RoadLink WHERE
         formOfWay = \"Single Carriageway\" AND
         roadFunction <> \"Restricted Local Access Road\" "
) %>%
    select(c(roadFunction, geom)) %>%
    st_zm() # remove z axis

roads <- as_Spatial(roads)
roads <- raster::crop(roads, as.matrix(extent(ctg))) %>%
    st_as_sf() %>%
    mutate(road_id = paste0("road_", row_number()))


# keep line polys
roads_line <- roads

# one buffer to include non road points, 1m buffer to show only road points
roads_buff <- st_buffer(roads, 30)
roads <- st_buffer(roads, 1)

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

roads_buff <- st_read("../data/derived/roads/roads_buff.gpkg") %>%
    as_Spatial()

ctg_ <- catalog("../data/point/")
opt_output_files(ctg) <- "../data/derived/ctg_clean/{ID}_clean"
opt_chunk_size(ctg) <- 500
catalog_apply(ctg, lidr_clean)

ctg <- catalog("../data/derived/ctg_clean/")
opt_output_files(ctg) <- "../data/derived/ctg_buff/{ID}_tile"
opt_chunk_size(ctg) <- 500
catalog_apply(ctg, extract_buff, roads_buff)

ctg_notnorm <- catalog("../data/point/")
opt_output_files(ctg_notnorm) <- "../data/derived/ctg_notnorm/{ID}_tile"
opt_chunk_size(ctg_notnorm) <- 500
catalog_apply(ctg_notnorm, extract_buff, roads_buff)

# read in written roads file
roads <- read_sf("../data/derived/roads/roads.gpkg")

# find roads extent shows study area + used for aerial imagery from digimaps
extent <- st_as_sfc(st_bbox(roads))

# Write extent shapefile
st_write(extent, "../data/derived/extent/extent.gpkg", delete_layer = TRUE)
