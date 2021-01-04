# ================================================================== #
#   Program: Missingness
#   Author:  F.H. Dobkin
#   Descrip: Identify and correct missingness
#            
# ================================================================== #


#Load packages
library(naniar)
library(mice)


#Identify patterns in areas of concern
ggplot(county_test, aes(x=violent_crime_rate, y=percent_rural)) +
  geom_miss_point() +
  facet_wrap(~state) +
  theme_dark()


#Visualize
gg_miss_var(county_test, show_pct=TRUE, facet=state)
ggplot(county_test, aes(x=violent_crime_rate, y=percent_rural)) +
  geom_miss_point() +
  facet_wrap(~state) +
  theme_dark()


#Preprocess data
str(county_test)
lapply(county_test$debt_to_income, as.numeric)
lapply(county_test$state, as.factor)
lapply(county_test$geoid, as.factor)
lapply(county_test$county.x, as.factor)


#Zeroes to NA
for(i in 1:ncol(county_test)) {
  county_test[,i] <- ifelse(county_test[,i] <= 0, NA, county_test[,i])
}


#Identify pattern type
pattern_missing <- unlist(lapply(county_test, function(x) sum(is.na(x))))
sort(pattern_missing[pattern_missing > 0], decreasing=TRUE)

                                                               
#Deselect nonimpute variables
county_test <- county_test %>%
  select(-geoid, -state, -county.x, -gpe) 
county_test$soc_em_support <- as.numeric(county_test$soc_em_support)
                                 

#Run mice
imp <- mice(county_test, maxit=0)
matrix = imp$predictorMatrix
method = imp$method
imped <- mice(county_test, maxit=1, predictorMatrix = matrix,
              method=method, print=FALSE)
fit_imp <- with(imped, lm(soc_em_support ~ percent_rural + debt_to_income + low_bw + teen_birth + hs_graduation +
                          violent_crime_rate)

                      

# ================================================================== #

#   END OF FILE
