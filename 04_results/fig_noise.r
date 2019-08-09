source("../scripts/functions.r")
road_lm <- fread("../data/derived/model_data/lm1_las_f.csv") %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"), crs = 27700)
sample_lines <- st_read("../data/derived/roads/sample_lines.gpkg")

road_lm1 <- road_lm[road_lm$lm1_pred == 1, ]

# remove noise
# chosen example

rd <- road_lm1[road_lm1$road_id == "road_15", ]
sample_lines <- sample_lines[sample_lines$road_id == "road_15", ]


jpgs <- Sys.glob("../data/aerial/*.jpg")
jpgs <- lapply(jpgs, brick)
jpgs <- do.call(merge, jpgs)
aerial <- crop(jpgs, rd)

extent <- st_as_sfc(st_bbox(rd))
sample_lines <- sample_lines %>% st_intersection(extent)
plot(sample_lines)

rd_split <- split(rd, rd$sample_id)

rd_split <- lapply(rd_split, function(x) {
    distances <- x %>%
        st_distance() %>%
        as.data.frame()

    distances <- sapply(distances, as.numeric)

    distances[distances == 0] <- 99

    distances <- distances %>% 
        apply(2, min) %>%
        as.data.frame() %>% 
        mutate(rowid = row_number())
    names(distances) <- c("distance", "rowid")

    distances <- distances[distances$distance < 1, ]

    x_fil <- x %>% mutate(rowid = row_number())

    x_fil <- x_fil[x_fil$rowid %in% distances$rowid, ]

    return(x_fil)
})

rd_fil <- do.call(rbind, rd_split)
