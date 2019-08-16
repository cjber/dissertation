source("./functions.r")
sampled_las <- fread("../data/derived/model_data/sampled_las.csv")

# ground pts only
sampled_las <- sampled_las[sampled_las$Classification == 2, ]

# global linear model: unfiltered
# for this section see social survey + ss assessment 2
f1 <- as.formula("road ~ Intensity + lum + dists + Z + NumberOfReturns")
lm1 <- lm(data = sampled_las, formula = f1)
lm1_pred <- predict(lm1, sampled_las, type = "response")

f2 <- as.formula("road ~ Intensity + dists + Z + NumberOfReturns")
lm2 <- lm(data = sampled_las, formula = f1)
lm2_pred <- predict(lm2, sampled_las, type = "response")

glm1 <- glm(data = sampled_las, formula = f1, family = "binomial")
glm1_sum <- summary(glm1)
glm1_pred <- predict(glm1, sampled_las, type = "response")

sampled_las$lm1_pred <- lm1_pred
sampled_las$lm1_dum <- ifelse(sampled_las$lm1_pred >
  quantile(sampled_las$lm1_pred, .95), 1, 0)

sampled_las$lm1_pred <- lm1_pred
sampled_las$lm1_dum90 <- ifelse(sampled_las$lm1_pred >
  quantile(sampled_las$lm1_pred, .90), 1, 0)

sampled_las$lm2_pred <- lm2_pred
sampled_las$lm2_dum90 <- ifelse(sampled_las$lm2_pred >
  quantile(sampled_las$lm2_pred, .95), 1, 0)

sampled_las$glm1_pred <- glm1_pred
sampled_las$glm1_dum <- ifelse(sampled_las$glm1_pred >
  quantile(sampled_las$glm1_pred, .95), 1, 0)

fwrite(sampled_las, "../data/derived/model_data/linearmodels.csv")
