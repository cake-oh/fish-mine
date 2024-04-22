## calculate GDP metrics: normalized percent contribution, annual rates of change, avg GDP percent for every country ##

#### [0] prep ####
library(dplyr)
library(tidyr)

## load data
df_fish_og <- read.csv("output/df_fishing_rejoin.csv")
df_mine_og <- read.csv("output/df_mining_rejoin.csv")

#### [1] combine fishing and mining + select relevant variables ####
df_comb <- bind_rows(df_fish_og,df_mine_og) %>% # combine
  select(Country,Industry,Industry_simple,Aggregate_level,GVA_total,Currency_ISO,
         Metric_type,Year,Base_year,Effective_year_GDP,GDP_annual)


#### [2] calculate GDP metrics ####
## normalized GDP for annual percent contribution, annual growth rates: absolute + relative, average contribution over all years
df_calcs <- df_comb %>%
  group_by(Country,Industry_simple,Aggregate_level,Metric_type) %>% # for every country's industries
  arrange(Year) %>%
  mutate(GDP_perc_norm=GVA_total/GDP_annual*100, # normalized GDP percent contribution
         GDP_change_absolute = GDP_perc_norm - lag(GDP_perc_norm), # absolute growth
         GDP_change_relative = (GDP_perc_norm - lag(GDP_perc_norm))/lag(GDP_perc_norm) * 100,
         GDP_perc_norm_mean=mean(GDP_perc_norm,na.rm = TRUE),
         GDP_change_abs_mean = mean(GDP_change_absolute,na.rm = TRUE),
         GDP_change_rel_mean = mean(GDP_change_relative,na.rm=TRUE),
         GDP_annual_mean=mean(GDP_annual,na.rm=TRUE),
         GVA_total_mean=mean(GVA_total,na.rm=TRUE),
         GVA_total_allyears=sum(GVA_total,na.rm=TRUE)) %>% # relative growth
  ungroup() %>% 
  mutate(Country_iso=countrycode::countrycode(Country,"country.name","iso3c"),  # iso abbreviation
         Country_short = if_else(stringr::str_detect(Country, " "),Country_iso, Country), # iso if country is 2 words
         Aggregate_level = as.character(Aggregate_level)) %>%
  arrange(Country,Industry_simple,Aggregate_level,Metric_type,Year) 
  
#### save output ####
write.csv(df_calcs,"output/df_gdp_calcs.csv",row.names=FALSE)


#### next: 3_make_gdp_plots.R ####
