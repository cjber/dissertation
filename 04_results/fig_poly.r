source("../scripts/functions.r")

road_poly <- Sys.glob("../data/final_data/*.gpkg")
road_poly <- lapply(road_poly, st_read)

centrelines <- st_read("../data/derived/roads/roads_line.gpkg", quiet = TRUE)
