# ================================================================== #
#   Program: Fixed Effect
#   Author:  F.H. Dobkin
#   Descrip: Code fixed effect
#            
# ================================================================== #


#Create region columns
county_test$pacific <- NA
county_test$mountain <- NA
county_test$wn_central <- NA
county_test$en_central <- NA
county_test$ws_central <- NA
county_test$es_central <- NA
county_test$s_atlantic <- NA
county_test$m_atlantic <- NA
county_test$n_england <- NA


#Pacific 
lapply(income_ct$pacific, as.factor)
income_ct$pacific <- ifelse(income_ct$state %in% 
                                c("CA", "WA", "OR", "AK", "HI"), 1, 0)


#Mountain
lapply(income_ct$mountain, as.factor)
income_ct$mountain <- ifelse(income_ct$state %in% 
                                  c("AZ", "NM", "UT", "NV", "CO", "WY", "ID", "MT"), 1, 0)


#West North Central
lapply(income_ct$wn_central, as.factor)
income_ct$wn_central <- ifelse(income_ct$state %in%
                                   c("KS", "MO", 
                                    "NE", "IA", "SD", "ND", "MN"), 1, 0)


#East North Central
lapply(income_ct$en_central, as.factor)
income_ct$en_central <- ifelse(income_ct$state %in%
                                   c("IL", "IN", "OH", "MI", "WI"), 1, 0)


#West South Central
lapply(income_ct$ws_central, as.factor)
income_ct$ws_central <- ifelse(income_ct$state %in%
                                   c("TX", "LA", "AR", "OK"), 1, 0)

#East South Central
lapply(income_ct$es_central, as.factor)
income_ct$es_central <- ifelse(income_ct$state %in%
                                   c("AL", "MS", "TN", "KY"), 1, 0)


#South Atlantic
lapply(income_ct$s_atlantic, as.factor)
income_ct$s_atlantic <- ifelse(income_ct$state %in%
                                   c("FL", "GA", "SC", "NC", "VA", "WV",
                                     "DE", "DC", "MD"), 1, 0)


#Middle Atlantic
lapply(income_ct$m_atlantic, as.factor)
income_ct$m_atlantic <- ifelse(income_ct$state %in% 
                                   c("PA", "NY", "NJ"), 1, 0)


#New England
lapply(income_ct$n_england, as.factor)
income_ct$n_england <- ifelse(income_ct$state %in%
                                  c("CT", "RI", "MA", "NH", "VT", "ME"), 1, 0)


#Majority or Minority Rural 
income_ct <- income %>%
  mutate(rural_cat = case_when(
    perc_rural >= 0 & perc_rural <= 50 ~ 0,
    perc_rural >= 50.1 ~ 1
  ))


#RUCC Fixed Effects
income_ct$non_met <- NA
income_ct$non_met <- ifelse(income_ct$rucc %in% 4:6, 1, 0)

income_ct$non_met_adj <- NA
income_ct$non_met_adj <- ifelse(income_ct$rucc %in% 7:9, 1, 0)
# ================================================================== #

#   END OF FILE
