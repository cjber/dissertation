source("../scripts/functions.r")
widths_1 <- st_read("../data/derived/model_data/widths_1.gpkg") # lm1
widths_2 <- st_read("../data/derived/model_data/widths_2.gpkg") # glm1
widths_3 <- st_read("../data/derived/model_data/widths_3.gpkg") # lm190
widths_IND <- st_read("../data/derived/model_data/widths_IND.gpkg") # ind lm with new cents

road_est <- st_read("../data/osroads/roads_estwidth.gpkg", quiet = TRUE) %>% 
    st_drop_geometry() %>%
    na.omit()

# width comparison
widths <- fread("../data/final_data/final.csv") %>%
    select(-c(V1,
              meanAngle,
              maxAngle,
              maxZ,
              meanZ,
              meanInt,
              rangeInt)) %>% 
    merge(road_est, by = c("road_id", "roadFunction"))

normalise_widths <- function(x){
    x <- x / widths$estWidth * 100
}

#normalised comparison relative to known
norm_widths <- widths %>%
    mutate_at(vars(3:length(widths)), normalise_widths) %>%
    summarise_at(vars(3:length(widths)), funs(mean), na.rm = T) %>%
    t()

# widths and other variables
# choose most accurate width
final_data <- fread("../data/final_data/final.csv") %>%
    select(-c(V1,
              maxAngle,
              maxZ,
              meanInt,
              lm1_mean,
              lm2_mean,
              glm_mean,
              )) %>%
    merge(road_est, by = c("road_id", "roadFunction"))
