####################################################################
#
#    Introduction to lavaan - why do wet fingers wrinkle?
#
####################################################################

# We're going to run some linear regression models.
# We are going to run them using the familiar lm() library
# Then we're going to run them in lavaan.

# Preparation ----
rm(list = ls())  # clear the environment
getwd()

# load required libraries (Run once)
install.packages("lavaan")
install.packages("fastDummies")
install.packages("janitor") 
install.packages("tidyverse")

library(lavaan)
library(fastDummies)
library(janitor)            
library(tidyverse)


# Load the data ----
wrinkle <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/Stat2Data/Wrinkle.csv")
summary(wrinkle)

# Get to know the data ----
dim(wrinkle) 
names(wrinkle)
View(wrinkle)

# How many variables are there? What are they?
head(wrinkle)
tail(wrinkle)



# These data are from: ----
# Kareklas, et al. (2013) "Water-induced finger wrinkles improve 
# handling of wet objects", Biology Letters, 
# http://dx.doi.org/10.1098/rsbl.2012.0999 
#
# 20 participants picked up items with the right hand thumb and index finger,
# passed them through a small hole and grabbed them with the left hand, 
# and then put them into a box that had a hole in the lid. 
# Sometimes the participants' fingers were wrinkled (because they were wet). 
# Sometimes the items were wet.

## Codebook (abridged)
#
# Participant: Participant ID
# Time:     Time to move objects (seconds)
# Fingers:  Status of fingers (non or wrinkled)
# Objects:  Status of objects (dry or wet)

# General research question ----
#  Why do our fingers go wrinkly when wet? Perhaps it helps us handle
#  wet objects. 
# Specific research question: 
#  Is handling time for wet objects with wet fingers less than 
#   handling time for wet objects with dry fingers?

# Causal model ----
# This was an experiment. The values of Finger and Objects were decided
# by the experimenters, so they are uncorrelated causes of handling Time.

# Statistical model ----
# The hypothesis is not simply that wet fingers will increase handling
# time, and that wet objects will increase handling time, and that these  
# two factors will operate independently. It is that wet fingers will reduce  
# the slowing effect of handling wet objects. This implies an interaction 
# between Fingers and Objects.

# Data preparation ----
# First we'll create numerical dummy variables for having wet(wrinkled) 
# fingers and wet objects. 

# Get to know the data 
wrinkle %>% tabyl(Fingers)
wrinkle %>% tabyl(Objects)

# dummy variable creation ----
wrinkle <- dummy_cols(wrinkle, select_columns = "Fingers")
wrinkle <- dummy_cols(wrinkle, select_columns = "Objects")

# We now have new dummy variables: Fingers_wrinkled and Objects_wet
head(wrinkle)
View(wrinkle)

# lavaan won't create interactions automatically, so we'll have to
# do it manually

wrinkle$interaction <- wrinkle$Fingers_wrinkled*wrinkle$Objects_wet


######################################### lm()
#
##  First, we use lm() to fit the model

# SK: fit the model ----
fit_lm <- lm(Time ~ Fingers_wrinkled * Objects_wet,   # lm() will create interactions 
             data = wrinkle)
summary(fit_lm)
plot(fit_lm)



## check the average object handling time
summary(wrinkle)

########## Answer these questions:
# 
# Q1: Interpret the b-coefficients. Do they support our specific research 
#     question?
# Q2: Are the statistical assumptions of the model met?


######################################### LAVAAN
##
##  now we use lavaan to fit the same model
##

# In lavaan we define the statistical model as a data object.
# The object specification is enclosed in single quotes.

model <- 'Time ~ Fingers_wrinkled + Objects_wet + interaction'

# Next we fit the model to the data

fit_lav_linear <- sem(model, data=wrinkle, meanstructure = TRUE)

# lavaan does not include intercepts ("meanstructure") in the model
#  by default, so we have to ask for it explicitly

summary(fit_lav_linear)
 
########## Answer this question:
# 
# Q3: Are there any differences between the two sets of results?


########## Model assumptions
#
# An assumption of both lm() and lavaan models above is that the
# observations are independent. Let's see:

View(wrinkle)

# There are only 20 participants, but there are 80 rows of data: Each
# participant did the task four times. These observations are therefore
# not independent - they are clustered within participants. This violates
# the independence assumption of the residuals in these simple linear
# models. What can we do? 
# We can relax the assumption in lavaan by allowing for the clustering 
# when computing the model SEs (the independence assumption only
# affects the SEs, not the b-values themselves.)

fit_lav_linear_clus <- sem(model, data=wrinkle, meanstructure = TRUE, 
                      cluster = "Participant")

# The "cluster" command tells lavaan that the observations (rows)
# are clustered within values of "Participant". It then computes "robust" 
# SEs, taking this non-independence into account.

summary(fit_lav_linear_clus)


########## Answer these questions:
# 
# Q4: How have the results changed? Would your answer to the 
#     research question change?
#
# Q5: Can you think of any other potential problems with the experiment
#     that might weaken our faith in the results?



# Extra time to play - Plot the interaction

library(ggplot2)

# check and fix the data types
str(wrinkle)
wrinkle$Objects <- as.factor(wrinkle$Objects)
wrinkle$Fingers <- as.factor(wrinkle$Fingers)
wrinkle$Time <- as.numeric(wrinkle$Time)


# plot the data
ggplot(data = wrinkle, aes(x = Fingers, y = Time)) +
  geom_boxplot() +
  facet_wrap( ~ Objects)
