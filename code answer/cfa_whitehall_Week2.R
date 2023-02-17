##
#
#   Factor structure of the SF36 "Mental Health" scale
#
##

##
#  Data: whitehall_NA.csv. Survey of 10,000 London-based
#   civil servants, collected in 2002.  
##### https://www.ucl.ac.uk/epidemiology-health-care/research/epidemiology-and-public-health/research/whitehall-ii
#
#   See "Codebook for Whitehall_NA.csv.docx" for variable descriptions
#    and the hypothesised latent structure of the mental health scale
#
#   Summary
#   Ware (2000) claims that the five items of the SF36 health 
#   screening questionaire are indicators of a single "mental 
#   health" latent factor. The five items are "nervous", "down", 
#   "peace", "sad", "happy". Assume that these variables can be
#   treated as continuous scale scores.
#### https://journals.lww.com/spinejournal/Fulltext/2000/12150/SF_36_Health_Survey_Update.8.aspx

## 

## set the directory, install package (if you haven't done yet) and load the library
getwd()
install.packages("lavaan")
library(lavaan)

## Read the data
whitehall <- read.csv("Whitehall_NA.csv")


##  Carry out the tasks below and answer the questions
#
#   (see https://lavaan.ugent.be/tutorial/cfa.html for help)
#
#  Task 1. Fit a CFA model assuming a single latent factor that is 
#     indicated by items "nervous", "down", "peace", "sad", "happy"
#
#  Q1: Interpret the factor loadings: what do these tell us? 
#     (Interpret the unstandardized and standardized loadings)
#
#  Q2: Interpret the item intercepts: What do these tell us?
#
#  Q3: Interpret the correlation residuals: What do these tell us? 
#    (See https://lavaan.ugent.be/tutorial/inspect.html)   
#
#  Q4: Interpret the model fit indices (CFI, SRMR, RMSEA): what do 
#    these tell us?
#
#  Q5: Overall, do you support Ware's (2000) claim that these five
#     items plausibly reflect a single, latent factor?

# specify the model
mh_fac <- 'mh =~ nervous + down + peace + sad + happy'

# fit the model to the data
fit_mh_fac <- cfa(mh_fac, data = whitehall, meanstructure = TRUE)
#
# View the parameters estimates and fit indices 
summary(fit_mh_fac, fit.measures=TRUE, standardized = TRUE)
#
# View the residuals
residuals(fit_mh_fac)


