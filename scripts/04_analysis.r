source("../scripts/functions.r")

road_est <- st_read("../data/osroads/roads_estwidth.gpkg", quiet = TRUE) %>% 
    st_drop_geometry() %>%
    na.omit()



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

norm_widths <- widths %>%
    mutate_at(vars(3:8), normalise_widths) %>%
    summarise_at(vars(3:8), funs(mean), na.rm = T) %>% 
    t()

final_data <- fread("../data/final_data/final.csv") %>%
    select(-c(V1,
              maxAngle,
              maxZ,
              meanInt,
              lm1_mean,
              lm2_mean,
              lm3_mean,
              glm_mean)) %>% 
    merge(road_est, by = c("road_id", "roadFunction"))
