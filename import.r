# ================================================================== #
#   Program: 2010ACS-RUC
#   Author:  F.H. Dobkin
#   Descrip: Create Master file
#            
# ================================================================== #


#Load packages
library(readr)
library(tidyverse)
setwd("~/Desktop/")

#Upload rural file
county_rural <- read_excel("county_rural.xlsx", 
col_types = c("numeric", "text", "text", 
"numeric"))


#Upload 2010 file
county_pov_inc <- read_excel("county_pov_inc.xlsx", 
col_types = c("numeric", "text", "numeric", 
"numeric"))


#Upload 2009 file
county_pov_inc_2009 <- read_excel("~saipe.xlsx", 
col_types = c("numeric", "numeric", "numeric", 
"text", "numeric", "numeric"))


#Merge files and drop repeat columns
county_2010 <- merge(county_pov_inc, county_rural, by="geoid")
counties <- merge(county_2010, county_pov_inc_2009, by="geoid")


#Place in standard format
order <- c("geoid", "state", "county.y", "percent_rural", "percent_poverty", "median_hh_income", "All Ages in Poverty Percent", "Median Household Income in Dollars")
counties[,order]
counties <- counties[,-c(5:6)]
rename(
      "county" = "county.y",
      "poverty_2010" = "percent_poverty",
      "income_2010" = "median_hh_income",
      "poverty_2009" = "All Ages in Poverty Percent",
       "income_2009" = "Median Household Income in Dollars"
      )
counties <- counties %>%
  mutate_if(is.numeric, round, digits=2)


#Join independent variables 
indvar <- read_excel("indvar.xlsx")
county_final <- left_join(counties, indvar, by = "geoid")


#Growth Poverty Elasticity 
county_test <- county_final %>%
  group_by(poverty_2010, income_2010, poverty_2009, income_2009) %>%
  mutate(per_change_pov = (poverty_2010 - poverty_2009)/poverty_2009) %>%
  mutate(per_change_inc = (income_2010 - income_2009)/income_2009) %>%
  mutate(gpe = per_change_pov/per_change_inc)
county_test <- county_test %>%
  group_by(gpe) %>%
  mutate(gpe_percentile = ntile(desc(gpe, 100)))

# ================================================================== #

#   END OF FILE
