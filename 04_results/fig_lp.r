source("/home/cjber/drive/uni/envs492/main/scripts/functions.r")
las <- catalog("/home/cjber/drive/uni/envs492/main/data/point")
roads <- read_sf("/home/cjber/drive/uni/envs492/main/data/derived/roads/roads_buff.gpkg", quiet = TRUE)
aerial <- raster("/home/cjber/drive/uni/envs492/main/data/derived/aerial/aerial_crop.tif")

las <- lasclip(las, extent(roads[6, ]))
aerial <- raster::crop(aerial, extent(roads[6, ]))
aerial <- aggregate(aerial, fact=8)

aerial <- as(aerial, "SpatialPixelsDataFrame")
aerial <- as.data.frame(aerial)
colnames(aerial) <- c("value", "x", "y")

chm <- grid_canopy(las, res = 2, p2r())

chm <- as(chm, "SpatialPixelsDataFrame")
chm <- as.data.frame(chm)
colnames(chm) <- c("value", "x", "y")

las_lp <- lasfilter(las, NumberOfReturns == ReturnNumber)
chm_lp <- grid_canopy(las_lp, res = 2, p2r())

chm_lp <- as(chm_lp, "SpatialPixelsDataFrame")
chm_lp <- as.data.frame(chm_lp)
colnames(chm_lp) <- c("value", "x", "y")

las_lp <- lasground(las_lp, csf())

#### Create Point DSM
# interpolate ground points to create raster dtm. Uses Classification
# very large number of points, therefore idw used as opposed to kriging
dsm_lp <- grid_terrain(las_lp, 2, knnidw(k = 10, p = 2))
# normalise heights using dtm
norm_lp <- lasnormalize(las_lp, dsm_lp)

dsm_lp <- grid_canopy(norm_lp, res = 2, p2r())

dsm_lp <- as(dsm_lp, "SpatialPixelsDataFrame")
dsm_lp <- as.data.frame(dsm_lp)
colnames(dsm_lp) <- c("value", "x", "y")

las_lp <- grid_canopy(norm_lp, res = 2, p2r())

las_lp <- as(las_lp, "SpatialPixelsDataFrame")
las_lp <- as.data.frame(las_lp)
colnames(las_lp) <- c("value", "x", "y")

# Define your own new metrics
myMetrics <- function(z, i) {
    metrics <- list(
        zmean = mean(z), # Mean products of z by intensity
        imean = mean(i)
    )
}

metrics <- grid_metrics(norm_lp, ~ myMetrics(Z, Intensity), res = 2)

metrics <- as(metrics, "SpatialPixelsDataFrame")
metrics <- as.data.frame(metrics)
colnames(metrics) <- c("zmean", "imean", "x", "y")
