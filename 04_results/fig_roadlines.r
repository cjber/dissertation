source("../scripts/functions.r")

road_slines <- Sys.glob("../data/final_data/*.gpkg")

road_slines <- lapply(road_slines, st_read, quiet = T)
