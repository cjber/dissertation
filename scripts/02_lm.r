source("./functions.r")
sampled_las <- fread("../data/derived/model_data/sampled_las.csv")
samples <- split(sampled_las, f = sampled_las$sample_id)

# remove non ground and returns above 1
# remove sample line if road centrelines show multiple returns
# i.e. ignore samples with partial canopy obstruction

# global linear model: filtered
# for this section see social survey + ss assessment 2
f1 <- as.formula("road ~ Intensity + lum + dists + Z")
f2 <- as.formula("road ~ Intensity + lum + dists")
f3 <- as.formula("road ~ Intensity + dists")
lm1 <- lm(data = sampled_las, formula = f1)
lm1_sum <- summary(lm1) %>% tidy()
lm1_pred <- predict(lm1, sampled_las, type = "response")

lm2 <- lm(data = sampled_las, formula = f2)
lm2_sum <- summary(lm2) %>% tidy()
lm2_pred <- predict(lm2, sampled_las, type = "response")

lm3 <- lm(data = sampled_las, formula = f3)
lm3_sum <- summary(lm3) %>% tidy()
lm3_pred <- predict(lm3, sampled_las, type = "response")

glm1 <- glm(data = sampled_las, formula = f1, family = "binomial")
glm1_sum <- summary(glm1)
glm1_pred <- predict(glm1, sampled_las, type = "response")

sampled_las$lm1_pred <- lm1_pred
sampled_las$lm1_dum <- ifelse(sampled_las$lm1_pred >
  quantile(sampled_las$lm1_pred, .95), 1, 0)

sampled_las$lm2_pred <- lm2_pred
sampled_las$lm2_dum <- ifelse(sampled_las$lm2_pred >
  quantile(sampled_las$lm2_pred, .95), 1, 0)

sampled_las$lm3_pred <- lm3_pred
sampled_las$lm3_dum <- ifelse(sampled_las$lm3_pred >
  quantile(sampled_las$lm3_pred, .95), 1, 0)

sampled_las$glm1_pred <- glm1_pred
sampled_las$glm1_dum <- ifelse(sampled_las$glm1_pred >
  quantile(sampled_las$glm1_pred, .95), 1, 0)

# global linear model: filtered test
filter_las <- lapply(samples, filter_returns)

filter_las <- filter_las %>%
  compact()

filter_las <- do.call(rbind, filter_las)

# remove any NA values in chosen pred + outcomes
filter_las <- filter_las[!is.na(filter_las$Intensity) &
  !is.na(filter_las$lum) & !is.na(filter_las$road) & !is.na(filter_las$Z), ]

filter_las <- split(filter_las, f = filter_las$sample_id)

filter_las <- plyr::compact(filter_las)
filter_las <- do.call(rbind, filter_las)

f1 <- as.formula("road ~ Intensity + lum + dists + Z")
lm1 <- lm(data = filter_las, formula = f1)
lm1_sum <- summary(lm1)
lm1_pred <- predict(lm1, filter_las, type = "response")
filter_las$filter_pred <- lm1_pred
filter_las$filter_dum <- ifelse(filter_las$filter_pred >
  quantile(filter_las$filter_pred, .95), 1, 0)

# individual linear probability model: has to filter out canopy
filter_las <- split(filter_las, filter_las$sample_id)
f1 <- as.formula("road ~ Intensity + lum + dists + Z")
filter_las <- lapply(filter_las, lm_compute, f = f1)
filter_las <- do.call(rbind, filter_las)

fwrite(sampled_las, "../data/derived/model_data/lm_u.csv")
fwrite(filter_las, "../data/derived/model_data/lm_f.csv")
