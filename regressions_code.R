rm(list = ls())

# Running the Regression
library(tidyverse)
library(fixest)

setwd("C:\\Users\\andre\\OneDrive\\Documents\\Homework\\Emp Meth\\Regressions")

test <- read.csv("df.csv")

agr_countries <- test[test$laggedGDP2 > 30, ]
agr_countries <- na.omit(agr_countries)

ind_countries <- test[test$laggedGDP2 < 5, ]
ind_countries <- na.omit(ind_countries)

hot_countries <- test[test$GID_0 %in% c("BEN", "BFA", "CAF", "CIV",
                                        "DJI", "ERI", "GHA", "GIN",
                                        "GMB",
                                        "GNB", "MLI", "MRT", "NER",
                                        "NGA", "SEN", "SLE", "SOM",
                                        "SYC", "TCD", "TGO"),]
hot_countries <- na.omit(hot_countries)

rainy_countries <- test[test$GID_0 %in% c("SLE", "LBR", "GNQ", "GAB",
                                          "COM", "STP", "GIN", "CMR",
                                          "COG", "COD"),]
rainy_countries <- na.omit(rainy_countries)

mod_all_countries <- feols(stunting_rate ~ hot_months1 + hot_months2 + rainy_months1 + rainy_months2 | GID_0 + year,
                 data=test)
mod_agr <- feols(stunting_rate ~ hot_months1 + hot_months2 + rainy_months1 + rainy_months2 | GID_0 + year,
              data=agr_countries)
mod_ind <- feols(stunting_rate ~ hot_months1 + hot_months2 + rainy_months1 + rainy_months2 | GID_0 + year, 
             data=ind_countries)
mod_hot <- feols(stunting_rate ~ hot_months1 + hot_months2 + rainy_months1 + rainy_months2 | GID_0 + year, 
             data=hot_countries)
mod_rain <- feols(stunting_rate ~ hot_months1 + hot_months2 + rainy_months1 + rainy_months2 | GID_0 + year, 
             data=rainy_countries)

mod_all_countries$coefficients
mod_agr$coefficients
mod_ind$coefficients
mod_hot$coefficients
mod_rain$coefficients

summary(mod_agr)
summary(mod_ind)
summary(mod_hot)
summary(mod_rain)



