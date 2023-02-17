##
#
#   Factor structure of the SF36 "Mental Health" scale
#                Part 2
##

##
#  Data: Whitehall_NA.csv. 
#  See "Codebook for Whitehall_NA.csv.docx" for variable descriptions
#    and the hypothesised latent structure of the mental health scale
rm(list = ls()) # clear the environment
setwd()
getwd()
## load the libraries
install.packages("lavaan") ## in case you haven't done it yet
library(lavaan)

## Read the data
whitehall <- read.csv("Whitehall_NA.csv")


##  Carry out the tasks below and answer the questions
#   (see https://lavaan.ugent.be/tutorial/cfa.html for help)
#
#  Task 1: Fit a two-factor model of the "mental health" items, 
#   ("nervous", "down", "peace", "sad", "happy"), with one
#   factor indicated by the positive items ("happy", "peace"), 
#   and one factor indicated by the negative items ("nervous", "down", 
#   "sad") 
# 
#  Q1: What results suggest that this model is a good model? 
#  Q2: What results suggest that this model is a poor model? 
###### Task 1: Fit a Two factor model

mh_fac2 <- 'pos =~ nervous + down + sad   # negative mood
            neg =~ happy + peace          # positive mood 
            pos ~~ neg '                  # factor correlation


fit_mh_fac2 <- cfa(mh_fac2, data = whitehall, meanstructure = TRUE)

summary(fit_mh_fac2, fit.measures=TRUE, standardized = TRUE)

lavResiduals(fit_mh_fac2)   # LavResiduals() gives us more than residuals()

#  Q3: Which model is preferred by a Likelihood Ratio Test? 

#  To compare the one-factor model with the two-factor model - First, fit the one-factor model.
mh_fac <- 'mh =~ nervous + down + peace + sad + happy'
fit_mh_fac <- cfa(mh_fac, data = whitehall, meanstructure = TRUE)
summary(fit_mh_fac, fit.measures=TRUE, standardized = TRUE)

## Then run the Likelihood Ratio Test of 1- vs. 2-factor models 
anova(fit_mh_fac, fit_mh_fac2)

#  Task 2. Fit a CFA model assuming a single latent factor for the  
#   SF-36 scale "vitality". (Items: "life", "energy", "wornout", "tired")
#
#  Q4: Interpret the results. Is this a good model?

# First, fit a single-dimension model for the four vitality items

vit_fac <- 'vit =~ life + energy + wornout + tired'
fit_vit_fac <- cfa(vit_fac, data = whitehall, meanstructure = TRUE)
summary(fit_vit_fac, fit.measures=TRUE, standardized = TRUE)
lavResiduals(fit_vit_fac)


#  Task 3. Fit a series of CFA models to evaluate whether the items   
#   from the "vitality" factor ("life", "energy", "wornout", "tired")
#   and the items from the "Positive mood" factor ("peace", "happy")
#   are adequately described by a single latent factor, or are better
#   represented by a model with two latent factors.
#
#  Q5: Evaluate the model fits and compare the models. Is latent "positive
#    mood" synonymous with latent "vitality", or are they separate constructs?

# First, fit a single-dimension model for all items
vitpos_fac <- 'vit =~ life + energy + wornout + tired + happy + peace'
fit_vitpos_fac <- cfa(vitpos_fac, data = whitehall, meanstructure = TRUE)
summary(fit_vitpos_fac, fit.measures=TRUE, standardized = TRUE)
lavResiduals(fit_vitpos_fac)

# Next, fit a two factor model, with "vitality" and "positive mood" factors
#  and compare the models

vitpos2_fac <- 'vit =~ life + energy + wornout + tired
                pos =~ happy + peace
                vit ~~ pos'

fit_vitpos2_fac <- cfa(vitpos2_fac, data = whitehall, meanstructure = TRUE)
summary(fit_vitpos2_fac, fit.measures=TRUE, standardized = TRUE)
lavResiduals(fit_vitpos2_fac)
anova(fit_vitpos_fac, fit_vitpos2_fac)


