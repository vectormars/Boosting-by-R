rm(list=ls())

library("mboost")  ## load package
data("bodyfat", package = "TH.data")    ## load data
head(bodyfat)


## Reproduce formula of Garcia et al., 2005
lm1 <- lm(DEXfat ~ hipcirc + kneebreadth + anthro3a, data = bodyfat)
coef(lm1)

## Estimate same model by glmboost
glm1 <- glmboost(DEXfat ~ hipcirc + kneebreadth + anthro3a, data = bodyfat)
coef(glm1, off2int=TRUE) ## off2int adds the offset to the intercept

preds <- names(bodyfat[, names(bodyfat) != "DEXfat"]) ## names of predictors
fm <- as.formula(paste("DEXfat ~", paste(preds, collapse = "+"))) ## build formula
fm


glm2a <- glmboost(fm, data = bodyfat)
glm2b <- glmboost(DEXfat ~ ., data = bodyfat)
identical(coef(glm2a), coef(glm2b))


coef(glm2b, ## usually the argument 'which' is used to specify single base-
      which = "") ## learners via partial matching; With which = "" we select all.

plot(glm2b, off2int = TRUE) ## default plot, offset added to intercept
## now change ylim to the range of the coefficients without intercept (zoom-in)
plot(glm2b, ylim = range(coef(glm2b, which = preds)))







