## ---- libs
library(pacman)

pkgs <- c(
  "devtools",
  "Hmisc",
  "PerformanceAnalytics",
  "RStoolbox",
  "ggthemes",
  "RStoolbox",
  "broom",
  "viridis",
  "cowplot",
  "viridisLite",
  "cowplot",
  "ggpubr",
  "magrittr",
  "sf",
  "data.table",
  "kableExtra",
  "scales",
  "lidR",
  "raster",
  "sp",
  "knitr",
  "nvimcom",
  "tidyverse",
  "varhandle",
  "future",
  "rgdal",
  "pbapply",
  "cowplot"
)

pacman::p_load(pkgs, character.only = T)

## ---- home_dir
dir <- "/home/cjber/drive/uni/envs492/main/"

## ---- make_table
make_table <- function(df, cap, dig = 2, ...) {
  require(kableExtra)
  require(tidyverse)

  options(knitr.kable.NA = "")
  kable(df,
    digits = dig, caption = cap,
    linesep = "",
    longtable = FALSE, booktabs = TRUE,
    format = "latex",
    escape = F
  ) %>%
    kable_styling(font_size = 8)  %>% 
    row_spec(0, bold = TRUE)
}

## ---- ctg_to_df
ctg_to_df <- function(cluster) {
  las <- readLAS(cluster)
  if (is.empty(las)) {
    return(NULL)
  }
  las <- as.spatial(las)
  las <- as.data.frame(las)
  return(las)
}

## ---- clip_samples
clip_samples <- function(cluster, x) {
  las <- readLAS(cluster)
  if (is.empty(las)) {
    return(NULL)
  }
  las <- las %>%
    as.spatial() %>%
    st_as_sf(las) %>%
    st_set_crs(27700) %>%
    st_join(x)

  las <- las[is.na(las$sample_id) == FALSE, ]
  return(las)
}

## ---- lidr_clean
lidr_clean <- function(cluster) {
  las <- readLAS(cluster)
  if (is.empty(las)) {
    return(NULL)
  }
  epsg(las) <- 27700
  # remove all but last return
  las <- lasfilter(las, NumberOfReturns == ReturnNumber)

  # find ground points
  las <- lasground(las, csf())

  ## Create Point DEM
  # interpolate ground points to create raster dtm. Uses Classification = 2
  # very large number of points, therefore idw used as opposed to kriging
  dtm <- grid_terrain(las, 1, knnidw(k = 10, p = 2))
  # normalise heights using dtm
  las <- lasnormalize(las, dtm)
  return(las)
}

## ---- extract_buff
extract_buff <- function(cluster, clip_input) {
  las <- readLAS(cluster)

  if (is.empty(las)) {
    return(NULL)
  }

  if (!is.null(clip_input)) {
    las <- lasclip(las, clip_input)

    if (length(las) > 1) {
      for (i in 1:length(las)) {
        if (!is.empty(las[[i]])) {
          las <- do.call(rbind, las)
          return(las)
        }
      }
    }
  }
}

## ---- find_dists
find_dists <- function(x, y) {
  d <- st_distance(x, y)
  return(d)
}

## ---- euc
# Function to calculate Euclidean distance between 2 points
euclidean_distance <- function(p1, p2) {
  return(sqrt((p2[1] - p1[1])**2 + (p2[2] - p1[2])**2))
}

## ---- perp
# Function to calculate 2 points on a line perpendicular to another defined by 2 points p1,p2
# For point at interval, which can be a proportion of the segment length, or a constant
# At distance n from the source line
calc_perp <- function(p1, p2, n, interval = 0.5, proportion = TRUE) {
  # Calculate x and y distances
  x_len <- p2[1] - p1[1]
  y_len <- p2[2] - p1[2]

  # If proportion calculate reference point from tot_length
  if (proportion) {
    point <- c(p1[1] + x_len * interval, p1[2] + y_len * interval)
  }
  # Else use the constant value
  else {
    tot_len <- euclidean_distance(p1, p2)
    point <- c(
      p1[1] + x_len / tot_len * interval,
      p1[2] + y_len / tot_len * interval
    )
  }

  # Calculate the x and y distances from reference point
  # to point on line n distance away
  ref_len <- euclidean_distance(point, p2)
  xn_len <- (n / ref_len) * (p2[1] - point[1])
  yn_len <- (n / ref_len) * (p2[2] - point[2])

  # Invert the x and y lengths and add/subtract from the refrence point
  ref_points <- rbind(
    point,
    c(point[1] + yn_len, point[2] - xn_len),
    c(point[1] - yn_len, point[2] + xn_len)
  )

  # Return the reference points
  return(ref_points)
}

## ---- comb_ctg
comb_ctg <- function(x) {
  las <- readLAS(x)
  if (is.empty(las)) {
    return(NULL)
  }
  return(las)
}

## ---- compute_samples
sample_lines <- c()
compute_samples <- function(x) {
  if (nrow(x) > 1) {
    road_node <- st_coordinates(x)
    tot_len <- 0
    len_inc <- 10
    len_ofs <- len_inc
    for (i in 2:nrow(road_node) - 1) {
      n1 <- road_node[i, ]
      n2 <- road_node[i + 1, ]

      len_seg <- euclidean_distance(n1, n2)
      len_ofs <- len_ofs + len_inc

      while (len_ofs <= tot_len + len_seg) {
        len_ofs <- len_ofs + len_inc

        # Add results to output vector
        perp_segments <- calc_perp(
          n1, n2, 30,
          len_ofs - tot_len,
          proportion = FALSE
        )

        multipoints <- st_multipoint(matrix(perp_segments, ncol = 2))
        pts <- st_cast(st_geometry(multipoints), "POINT")
        n <- length(pts)

        pair <- st_combine(c(pts[1], pts[2], pts[3]))
        linestring <- st_cast(pair, "LINESTRING") %>%
          st_buffer(2) %>%
          st_sf() %>%
          mutate(road_id = as.character(unique(x$road_id)))
        sample_lines <- rbind(sample_lines, linestring)
      }
      tot_len <- tot_len + len_seg
    }
  }
  return(sample_lines)
}
## ---- greyscale
greyscale <- function(x) {
  x <- (x[[1]] + x[[2]] + x[[3]]) / 3
}

## ---- lm_compute
lm_compute <- function(x, f) tryCatch({
    m <- lm(formula = f, data = x)

    p <- m %>%
      tidy() %>%
      dplyr::select(p = p.value)

    pred_m <- predict(m, x, type = "response")

    if (sum(p) / nrow(p) < 0.05) {
      x$lm <- pred_m
    } else {
      x$lm <- NA
    }

    x$lmI_dum <- ifelse(x$lm > quantile(x$lm, .95), 1, 0)
    x$lmI_dum <- ifelse(x$lm > quantile(x$lm, .95), 1, 0)
    x$p_val <- sum(p)

    return(x)
  }, error = function(e) NULL)

## ---- filter_returns
filter_returns <- function(x) {
  road <- x[x$road == 1, ]
  if (max(road$NumberOfReturns) == 1) {
    return(x)
  }
}

## ---- filter_samples
filter_samples <- function(s) {
  # find rows far below mean
  if (nrow(s) > mean(nrow(s)) / 4) {
    # remove outlier points
    distances <- s %>%
      st_distance() %>%
      apply(1, FUN = function(y) {
        min(y[y > 0])
      }) %>%
      as.data.frame() %>%
      mutate(rowid = row_number()) %>%
      select(min_dist = ".", rowid)

    # above 1m from any other point
    distances <- distances[distances$min_dist < 1, ]

    s <- s %>% mutate(rowid = row_number())

    s <- s[s$rowid %in% distances$rowid, ]
    return(s)
  }
}

## ---- max_dist
max_dist <- function(x) {
  tot_dists <- c()
  distances <- x %>%
    st_distance(by_element = FALSE) %>%
    unclass() %>%
    "[<-"(lower.tri(., diag = TRUE), NA) %>%
    as_tibble() %>%
    rowid_to_column() %>%
    gather(colid, distance, starts_with("V"),
      na.rm = TRUE
    ) %>%
    arrange(desc(distance))

  if (nrow(distances) > 0) {
    distances$colid <- gsub("[^0-9.-]", "", distances$colid)
    tot_dists <- rbind(tot_dists, max(distances$distance))

    distances <- as.list(distances[1, 1:2]) %>%
      unlist() %>%
      as.numeric()

    x <- x[distances, ] %>%
      st_combine() %>%
      st_sf() %>%
      st_cast("LINESTRING")
    return(x)
  }
}

# consider optimisation?
## ---- max_lines
max_lines <- function(x) {
  road_lm <- split(x, f = x$sample_id)

  road_lm <- road_lm %>% compact()

  road_lm <- lapply(road_lm, filter_samples)
  road_lm <- lapply(road_lm, max_dist)
  road_lm <- do.call(rbind, road_lm)
  # find intersecting buffers
  road_lm <- st_join(road_lm, road_buff)

  return(road_lm)
}

## -- model_comparison
model_comparison <- function(model) {
  road_lm <- model[!is.na(model$roadFunction), ]
  rds <- unique(model$road_id)
  road_lm <- split(road_lm, f = road_lm$road_id)

  samp <- Filter(function(x) dim(x)[1] > 0, road_lm)
  cent <- centrelines[centrelines$road_id %in% rds, ]
  cent <- split(cent, f = cent$road_id)
  cent <- Filter(function(x) dim(x)[1] > 0, cent)

  ## ---- find true widths
  widths <- mapply(opposite_length, samp, cent)
  widths <- do.call(rbind, widths)
  widths <- as.data.frame(widths)

  widths$opposite <- as.numeric(unfactor(widths$opposite))

  widths <- widths[widths$opposite > 2 & widths$opposite < 8, ]

  widths <- widths %>%
    group_by(V2) %>%
    select(road_id = V2, opposite) %>%
    summarise(
      mean_width = mean(opposite)
    )

  return(widths)
}

## ---- opposite_length
opposite_length <- function(samp, cent) {
  tot_width <- c()
  cent <- cent %>% st_cast("POINT")
  n <- nrow(cent) - 1
  nodelines <- lapply(X = 1:n, FUN = function(i) {
    pair <- cent[c(i, i + 1), ] %>%
      st_combine()
    line <- st_cast(pair, "LINESTRING")
    return(line)
  })

  samp <- samp %>%
    mutate(row_id = row_number())
  samp <- split(samp, samp$row_id)
  for (n in nodelines) {
    for (s in samp) {
      int <- as.numeric(st_crosses(n, s))
      int[is.na(int)] <- 0
      if (int == 1) {
        n1 <- st_coordinates(n)[1, ]
        n2 <- st_coordinates(n)[2, ]
        x <- n1[1] - n2[1]
        y <- n1[2] - n2[2]
        ang_rad <- atan2(x, y)
        ang_deg <- ang_rad * 180 / pi
        if (ang_rad < 0) {
          ang_deg <- ang_deg + 180
        }

        n1 <- st_coordinates(s)[1, ]
        n2 <- st_coordinates(s)[2, ]
        x <- n1[1] - n2[2]
        y <- n1[2] - n2[2]

        ang_rad <- atan2(x, y)
        ang_deg_c <- ang_rad * 180 / pi
        if (ang_rad < 0) {
          ang_deg_c <- ang_deg + 180
        }

        theta <- ang_deg - ang_deg_c
        theta <- theta - 45 # position relation to perp line

        c1_len <- st_length(s)
        opposite <- abs(as.numeric(c1_len) * cos(as.numeric(theta)))
        opposite <- cbind(
          opposite, as.character(unique(cent$road_id)),
          as.character(unique(cent$sample_id))
        )
        tot_width <- rbind(tot_width, opposite)
      }
    }
  }
  return(tot_width)
}

## ---- road_angles
road_angles <- function(rd) {
  coords <- rd %>% st_coordinates()
  angle <- c()
  if (nrow(coords) > 1) {
    for (i in 1:(nrow(rd) - 1)) {
      n1 <- coords[i, ]
      n2 <- coords[i + 1, ]
      x <- n1[1] - n2[1]
      y <- n1[2] - n2[2]
      ang_rad <- atan2(x, y)
      ang_deg <- ang_rad / pi * 180

      if (ang_rad < 0) {
        ang_deg <- ang_deg + 180
      }
      angle <- append(angle, ang_deg)
    }
  }

  normal_ang <- c()
  for (i in 2:length(angle)) {
    # here i - 1 is theta 1, i is theta 2
    normal <- abs(angle[i] - (angle[i - 1]))
    normal_ang <- rbind(normal_ang, normal)
  }
  normal_ang <- cbind(
    normal_ang,
    as.character(rep(unique(rd$road_id), nrow(normal_ang)))
  )
  return(normal_ang)
}

## ---- height_change
height_change <- function(x) {
  elev <- c()
  samples <- split(x, x$sample_id)
  if (length(samples) > 2) {
    for (s in 2:length(samples) - 1) {
      pair <- samples[c(s, s + 1)]
      n1 <- mean(pair[[1]]$Z)
      n2 <- mean(pair[[2]]$Z)
      e <- abs(n1 - n2)
      e <- cbind(
        as.character(unique(samples[[s]]$road_id)), e
      )
      elev <- rbind(elev, e)
    }
  }
  return(elev)
}
