library(lavaan)
white_hall <- read.csv("data/Whitehall_NA.csv")

mh_fac2 <- 'pos =~ nervous + down + sad
            neg =~ happy + peace
            pos ~~ neg '

fit_mh_fac2 <- cfa(mh_fac2, data = white_hall, meanstructure = TRUE)
summary(fit_mh_fac2, fit.measures=TRUE, standardized = TRUE)
# good: cfi = 0.985 >=0.95; srmr = 0.02 <= 0.08
# poor: rmsea = 0.077 >= 0.05
lavResiduals(fit_mh_fac2)

mh_fac <- 'mh =~ nervous + down + peace + sad + happy'
fit_mh_fac <- cfa(mh_fac, data = white_hall,
                  meanstructure = TRUE)
summary(fit_mh_fac, fit.measures=TRUE, standardized = TRUE)
anova(fit_mh_fac, fit_mh_fac2)
#p-value < 2.2e-16 < 0.05 which indicated that the two-factor model is better
