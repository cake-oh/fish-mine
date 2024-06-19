#### [0] prep ###
## goal: start with oecd data, then pull google doc data to fill in gaps

library(tidyverse)

## load oecd data from 4_new_oecd_data.R (funky file paths) ##
df_gdp_total_og <- read_csv("/Users/keikonomura/GitHub/fish-mine/data/total_finaladd.csv")
df_fish_og <- read_csv("/Users/keikonomura/GitHub/fish-mine/data/fish_finaladd.csv")
df_mine_og <- read_csv("/Users/keikonomura/GitHub/fish-mine/data/mining_finaladd.csv")

## load google sheets data ##
df_googledoc_og <- read.csv("output/df_gdp_calcs.csv")




#### [1] clean and join fishing data ####
fish_code <- distinct(df_fish_og[,c("ACTIVITY","Economic.activity")]) # A03 is wanted
## 1 filter oecd data for A03 / rename columns to match for join later
a03_oecd <- filter(df_fish_og,ACTIVITY=="A03") %>%
  rename(Activity.code=ACTIVITY,
         Industry=Economic.activity,
         Country=Reference.area,Country_iso=REF_AREA,
         Year=TIME_PERIOD,
         GVA_total=OBS_VALUE,Currency_iso=CURRENCY) %>%
  mutate(Country_short = if_else(stringr::str_detect(Country, " "),Country_iso, Country)) # iso if country is 2 words

## 2 find missing countries
# all countries in og google doc data
gd_og_fishing <- filter(df_googledoc_og,Industry_simple=="Fishing",
                        !Aggregate_level==1)
a <- unique(gd_og_fishing$Country)
# all countries in oecd data
b <- unique(a03_oecd$Country)
# find missing ones
c <- setdiff(a,b)

## 3 grab missing countries from OG
a03_missing <- filter(gd_og_fishing,Country %in% c,
                      Metric_type=="Constant")

## 4 do calculations for oecd data
# calculate percent change from oecd data
a03_oecd_formatted <- a03_oecd %>% arrange(Country,Year) %>% 
  group_by(Country) %>% mutate(GDP_change_relative = 100*(GVA_total - lag(GVA_total))/lag(GVA_total),
                               GDP_change_rel_mean=mean(GDP_change_relative,na.rm = TRUE))
# calculate overall gdp perc contribution
df_gdp_total_og_formatted <- df_gdp_total_og %>% rename(Activity.code=ACTIVITY, # grab total gdp's
                                                        Country=Reference.area,Country_iso=REF_AREA,
                                                        Year=TIME_PERIOD,
                                                        GDP_total=OBS_VALUE) %>%
  select(Country,Year,GDP_total)

a03_oecd_formatted2 <- left_join(a03_oecd_formatted,df_gdp_total_og_formatted) %>% # join to gva, calculate perc contribution
  group_by(Country,Year) %>% mutate(GDP_perc_norm=GVA_total/GDP_total*100) %>% ungroup() %>%
  group_by(Country) %>% mutate(GDP_perc_norm_mean=mean(GDP_perc_norm,na.rm = TRUE),
                               GDP_annual_mean=mean(GDP_total,na.rm=TRUE),
                               GVA_total_mean=mean(GVA_total,na.rm=TRUE),
                               GVA_total_allyears=sum(GVA_total,na.rm=TRUE))


## 5 align columns
a03_oecd_formatted3 <- a03_oecd_formatted2 %>%
  select(-c("...1","OBS_STATUS","Observation.status","Currency","Activity.code")) %>%
  rename(Industry_specific=Industry) %>%
  mutate(Industry_simple="Fishing")

a03_missing_formatted <- a03_missing %>%
  select(-c("Industry_simple","Aggregate_level","Base_year","Effective_year_GDP","GDP_change_absolute",
            "Metric_type","GDP_change_abs_mean")) %>%
  rename(Currency_iso=Currency_ISO,GDP_total=GDP_annual,
         Industry_specific=Industry) %>%
  mutate(Industry_simple="Fishing")


## 6 join _missing and _oecd

a03_all <- bind_rows(a03_oecd_formatted3,a03_missing_formatted)



#####



#### [2] clean and join mining data ####
mine_code <- distinct(df_mine_og[,c("ACTIVITY","Economic.activity")]) # A03 is wanted
## 1 filter oecd data for A03 / rename columns to match for join later
b_oecd <- filter(df_mine_og,ACTIVITY=="B") %>%
  rename(Activity.code=ACTIVITY,
         Industry=Economic.activity,
         Country=Reference.area,Country_iso=REF_AREA,
         Year=TIME_PERIOD,
         GVA_total=OBS_VALUE,Currency_iso=CURRENCY) %>%
  mutate(Country_short = if_else(stringr::str_detect(Country, " "),Country_iso, Country)) # iso if country is 2 words

## 2 find missing countries
# all countries in og google doc data
gd_og_mining <- filter(df_googledoc_og,Industry_simple=="Mining",
                        Aggregate_level==1)
d <- unique(gd_og_mining$Country)
# all countries in oecd data
e <- unique(b_oecd$Country)
# find missing ones
f <- setdiff(d,e)

## 3 grab missing countries from OG
b_missing <- filter(gd_og_mining,Country %in% f,
                      Metric_type=="Constant")

## 4 do calculations for oecd data
# calculate percent change from oecd data
b_oecd_formatted <- b_oecd %>% arrange(Country,Year) %>% 
  group_by(Country) %>% mutate(GDP_change_relative = 100*(GVA_total - lag(GVA_total))/lag(GVA_total),
                               GDP_change_rel_mean=mean(GDP_change_relative,na.rm = TRUE))
# calculate overall gdp perc contribution
df_gdp_total_og_formatted <- df_gdp_total_og %>% rename(Activity.code=ACTIVITY, # grab total gdp's
                                                        Country=Reference.area,Country_iso=REF_AREA,
                                                        Year=TIME_PERIOD,
                                                        GDP_total=OBS_VALUE) %>%
  select(Country,Year,GDP_total)

b_oecd_formatted2 <- left_join(b_oecd_formatted,df_gdp_total_og_formatted) %>% # join to gva, calculate perc contribution
  group_by(Country,Year) %>% mutate(GDP_perc_norm=GVA_total/GDP_total*100) %>% ungroup() %>%
  group_by(Country) %>% mutate(GDP_perc_norm_mean=mean(GDP_perc_norm,na.rm = TRUE),
                               GDP_annual_mean=mean(GDP_total,na.rm=TRUE),
                               GVA_total_mean=mean(GVA_total,na.rm=TRUE),
                               GVA_total_allyears=sum(GVA_total,na.rm=TRUE))


## 5 align columns
b_oecd_formatted3 <- b_oecd_formatted2 %>%
  select(-c("...1","OBS_STATUS","Observation.status","Currency","Activity.code")) %>%
  rename(Industry_specific=Industry) %>%
  mutate(Industry_simple="Mining")

b_missing_formatted <- b_missing %>%
  select(-c("Industry_simple","Aggregate_level","Base_year","Effective_year_GDP","GDP_change_absolute",
            "Metric_type","GDP_change_abs_mean")) %>%
  rename(Currency_iso=Currency_ISO,GDP_total=GDP_annual,
         Industry_specific=Industry) %>%
  mutate(Industry_simple="Mining")


## 6 join _missing and _oecd

b_all <- bind_rows(b_oecd_formatted3,b_missing_formatted)



#### [3] join fishing and mining ####
fishmine_all <- bind_rows(a03_all,b_all)
write.csv(fishmine_all,"output/fishmine_updated.csv",row.names = FALSE)

# check what's not in both industries (just curious)
x <- unique(b_all$Country)
y <- unique(a03_all$Country)
setdiff(x,y)
