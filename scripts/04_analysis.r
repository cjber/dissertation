source("../scripts/functions.r")

final_data <- fread("../data/final_data/final.csv") %>% 
    select(-c(V1, id, roadFunction, mean_angle, mean_Z, mean_int)) %>% 
    column_to_rownames(var = "road_id")
head(final_data)
cor(final_data, method = "pearson", use = "complete.obs")

chart.Correlation(final_data, method = "pearson")
