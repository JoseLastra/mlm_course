library(openxlsx)
library(emmeans)

subsampling <- read.xlsx("pesticides.xlsx", sheet="Sheet1")
subsampling$method <- as.factor(subsampling$method)
subsampling$batch <- as.factor(subsampling$batch)
subsampling$subsample <- as.factor(subsampling$subsample)

aovo <- aov(pesticide ~ method + Error(batch), data=subsampling)
summary(aovo)

emmeans(aovo, pairwise ~ method)
