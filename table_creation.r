library(stargazer)
library(tidyverse)
library(quantreg)

#Mobility and County Rurality
model1 <- lm(mobile ~ tax + govt_exp + hs_drop + lfpr + teen_birth + rural_cat, data = income2)

#Mobility and Proximity 
model2 <- lm(mobile ~ tax + govt_exp + hs_drop + lfpr + teen_birth + non_met + non_met_adj, data = income2)

#GPE and County Rurality
model3 <- rq(gpe ~ tax + govt_exp + hs_drop + lfpr + teen_birth + rural_cat, data = income2)

#GPE and RUCC
model4 <- rq(gpe ~ tax + govt_exp + hs_drop + lfpr + teen_birth + non_met + non_met_adj, data = income2)

#Semi GPE and County Rurality
model5 <- rq(semi_gpe ~ tax + govt_exp + hs_drop + lfpr + teen_birth + rural_cat, data = income2)

#Semi GPE and RUCC
model6 <- rq(semi_gpe ~ tax + govt_exp + hs_drop + lfpr + teen_birth + non_met + non_met_adj, data = income2)

#Table
stargazer(model1, model2, model3, model4, model5, model6,  title="Table 1. Regression Results",
          dep.var.labels = c("Absolute Mobility", "Growth Poverty Elasticity", "Semi Growth Poverty Elasticity"),
          column.labels = c("Rural Level", "Rural-Urban Continuum Code", "Rural Level",
                            "Rural-Urban Continuum Code", "Rural Level", "Rural-Urban Continuum Code"),
          style = "io", align=TRUE, 
          covariate.labels = c("Local Tax Rate", "Government Expenditures", "High School Dropout Rate",
                               "Labor Force Participation Rate", "Teen Birth Rate","Rural Level", "RUCC 4-6",
                               "RUCC 7-9"), ci= FALSE, ci.level = 0.95, single.row=FALSE, 
          type = "latex", out = "regression.html")
