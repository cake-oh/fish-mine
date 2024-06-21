#### Load required packages ####
library(tidyverse)

# Load datasets
df_gdp_total_og <- read_csv("/Users/keikonomura/GitHub/fish-mine/data/total_finaladd.csv")
df_fish_og <- read_csv("/Users/keikonomura/GitHub/fish-mine/data/fish_finaladd.csv")
df_mine_og <- read_csv("/Users/keikonomura/GitHub/fish-mine/data/mining_finaladd.csv")
df_googledoc_og <- read_csv("/Users/keikonomura/GitHub/fish-mine/output/df_gdp_calcs.csv")
df_gdp <- read_csv("/Users/keikonomura/GitHub/fish-mine/output/df_annualgdp_clean.csv")
df_rates <- read_csv("/Users/keikonomura/GitHub/fish-mine/data/SNA_TABLE4_20062024043645017.csv")
df_wb_og <- read_csv("data/P_Data_Extract_From_World_Development_Indicators/c2f6b414-d729-47d1-b3ed-1a5e9c766c03_Data.csv")



#### Filter OECD data for A03 and rename columns ####
# set timeframe
start_year <- 2017
end_year <- 2023

a03_oecd <- df_fish_og %>%
  filter(ACTIVITY == "A03", TIME_PERIOD > start_year & TIME_PERIOD < end_year) %>%
  rename(Activity.code = ACTIVITY, Industry = Economic.activity, Country = Reference.area, Country_iso = REF_AREA, Year = TIME_PERIOD, GVA_total = OBS_VALUE, Currency_iso = CURRENCY) %>%
  mutate(Country_short = if_else(str_length(Country) > 6, Country_iso, Country))

# Find missing countries from Google Doc data
gd_og_fishing <- df_googledoc_og %>%
  filter(Industry_simple == "Fishing", Aggregate_level != 1,Metric_type == "Constant")
missing_countries_fishing <- setdiff(unique(gd_og_fishing$Country), unique(a03_oecd$Country))

# Grab missing countries from OG data
a03_missing <- gd_og_fishing %>%
  filter(Country %in% missing_countries_fishing)

# Format OECD data
a03_oecd_formatted <- a03_oecd %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  select(-c("...1", "OBS_STATUS", "Observation.status", "Currency", "Activity.code")) %>%
  rename(Industry_specific = Industry) %>%
  mutate(Industry_simple = "Fishing") %>%
  ### TEST TO TROUBLESHOOT ###
  mutate(GVA_total=GVA_total*1e+06) 

# Format missing data
a03_missing_formatted <- a03_missing %>%
  rename(Currency_iso = Currency_ISO, GDP_total = GDP_annual, Industry_specific = Industry) %>%
  select(-c("Industry_simple", "Aggregate_level", "Base_year", "Effective_year_GDP", "GDP_change_absolute", "Metric_type", "GDP_change_abs_mean", "GDP_total", "GDP_perc_norm", "GDP_perc_norm_mean", "GDP_annual_mean", "GVA_total_mean", "GVA_total_allyears", "GDP_change_relative", "GDP_change_rel_mean")) %>%
  mutate(Industry_simple = "Fishing") %>%
  mutate(Country_short = if_else(str_length(Country) > 6, Country_iso, Country))
  

# Join OECD and missing data
a03_all <- bind_rows(a03_oecd_formatted, a03_missing_formatted)



#### Filter OECD data for B and rename columns #### 
b_oecd <- df_mine_og %>%
  filter(ACTIVITY == "B", TIME_PERIOD > start_year & TIME_PERIOD < end_year) %>%
  rename(Activity.code = ACTIVITY, Industry = Economic.activity, Country = Reference.area, Country_iso = REF_AREA, Year = TIME_PERIOD, GVA_total = OBS_VALUE, Currency_iso = CURRENCY) %>%
  mutate(Country_short = if_else(str_length(Country) > 6, Country_iso, Country))

# Find missing countries from Google Doc data
gd_og_mining <- df_googledoc_og %>%
  filter(Industry_simple == "Mining", Aggregate_level == 1, Metric_type == "Constant")
missing_countries_mining <- setdiff(unique(gd_og_mining$Country), unique(b_oecd$Country))

# Grab missing countries from OG data
b_missing <- gd_og_mining %>%
  filter(Country %in% missing_countries_mining)

# Format OECD data
b_oecd_formatted <- b_oecd %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  select(-c("...1", "OBS_STATUS", "Observation.status", "Currency", "Activity.code")) %>%
  rename(Industry_specific = Industry) %>%
  mutate(Industry_simple = "Mining") %>%
  ### TEST TO TROUBLESHOOT ###
  mutate(GVA_total=GVA_total*1e+06)

# Format missing data
b_missing_formatted <- b_missing %>%
  rename(Currency_iso = Currency_ISO, GDP_total = GDP_annual, Industry_specific = Industry) %>%
  select(-c("Industry_simple", "Aggregate_level", "Base_year", "Effective_year_GDP", "GDP_change_absolute", "Metric_type", "GDP_change_abs_mean", "GDP_total", "GDP_perc_norm", "GDP_perc_norm_mean", "GDP_annual_mean", "GVA_total_mean", "GVA_total_allyears", "GDP_change_relative", "GDP_change_rel_mean")) %>%
  mutate(Industry_simple = "Mining") %>%
  mutate(Country_short = if_else(str_length(Country) > 6, Country_iso, Country))

# Join OECD and missing data
b_all <- bind_rows(b_oecd_formatted, b_missing_formatted)


#### join fishing and mining ####
fishmine_all <- bind_rows(a03_all, b_all) %>%
  filter(Year > start_year & Year < end_year) %>% arrange(Country,Industry_simple,Year)




#### [4] add annual GDP's ####
df_gdp <- df_wb_og %>%
  filter(`Series Name` == "GDP (constant LCU)", `Country Name` %in% unique(fishmine_all$Country) | `Country Name` %in% c("Russian Federation", "Korea, Rep.")) %>%
  select(-`Series Name`, -`Series Code`) %>%
  mutate(across(3:26, ~ ifelse(. == "..", NA, as.numeric(as.character(.))))) %>%
  pivot_longer(cols = 3:26, names_to = "Year", values_to = "GDP_annual") %>%
  mutate(Year = as.numeric(sub(".*?(\\d{4}).*", "\\1", Year))) %>%
  rename(Country = `Country Name`, Country_iso = `Country Code`) %>%
  mutate(Country = recode(Country, "Russian Federation" = "Russia"), Country = recode(Country, "Korea, Rep." = "South Korea"))

# Join with fishmine data
df_join <- left_join(fishmine_all, df_gdp, by = c("Country", "Year", "Country_iso"))



#### [5] convert to usd using exchange rates ####
# load data
df_rates <- read_csv("data/SNA_TABLE4_20062024043645017.csv") # oecd world bank data
other_gdp <- read_csv("output/other_gdp.csv") # compiled data from '4b_extract_other_gdp.R'

# Format df_rates
df_rates_formatted <- df_rates %>%
  filter(Transaction == "Exchange rates, period-average") %>%
  select(c("Year", "Value", "Country")) %>%
  rename(Exch_rate = Value)

# Combine with other_gdp
combined_rates <- bind_rows(df_rates_formatted, other_gdp)

df_rates_merged <- left_join(df_join, combined_rates)

# Step 1: Remove countries with all NA values in Exch_rate for each Industry_simple
data_filtered <- df_rates_merged %>%
  group_by(Country, Industry_simple) %>%
  filter(!all(is.na(Exch_rate))) %>%
  ungroup()

# Step 2: Fill remaining NA values with the mean Exch_rate for each Country and Industry_simple
data_filled <- data_filtered %>%
  group_by(Country, Industry_simple) %>%
  mutate(Exch_rate = ifelse(is.na(Exch_rate), mean(Exch_rate, na.rm = TRUE), Exch_rate)) %>%
  ungroup()

df_rates_convert <- data_filled %>%
  mutate(
    GVA_annual_usd = GVA_total / Exch_rate,
    GDP_annual_usd = GDP_annual / Exch_rate
  ) %>%
  arrange(desc(GDP_annual_usd)) %>%
  rename(GVA_total_lcu = GVA_total, GDP_annual_lcu=GDP_annual) # differentiate local currency from usd




#### [6] calculate gdp metrics ####
df_rates_final <- df_rates_convert %>%  
  group_by(Country,Year,Industry_simple) %>% arrange(Country,Year,Industry_simple) %>% 
  mutate(GVA_perc_GDP_usd = (GVA_annual_usd / GDP_annual_usd) * 100,
         GVA_perc_GDP_lcu = (GVA_total_lcu / GDP_annual_lcu) * 100) %>% ungroup()

df_rates_final2 <- df_rates_final %>% group_by(Country,Industry_simple) %>%
  arrange(Year) %>%
  mutate(GVA_change_relative = (GVA_annual_usd - lag(GVA_annual_usd)) / lag(GVA_annual_usd) * 100,
         GVA_change_rel_mean=mean(GVA_change_relative,na.rm = TRUE),
         GVA_mean_usd=mean(GVA_annual_usd,na.rm=TRUE),
         GVA_mean_lcu=mean(GVA_total_lcu,na.rm=TRUE),
         GVA_perc_GDP_usd_mean=mean(GVA_perc_GDP_usd,na.rm=TRUE),
         GVA_perc_GDP_lcu_mean=mean(GVA_perc_GDP_lcu,na.rm=TRUE),
         national_industry_gva_usd = sum(GVA_annual_usd,na.rm = TRUE)) %>% ungroup()

df_rates_final3 <- df_rates_final2 %>% group_by(Country) %>%
  mutate(GDP_mean_usd=mean(GDP_annual_usd,na.rm=TRUE),
         GDP_mean_lcu=mean(GDP_annual_lcu,na.rm=TRUE)) %>% arrange(Country,Industry_simple,Year) %>% ungroup()

df_rates_final4 <- df_rates_final3 %>% group_by(Industry_simple) %>%
  mutate(industry_total_gva_usd = sum(GVA_annual_usd,na.rm=TRUE)) %>% ungroup()

df_rates_final5 <- df_rates_final4 %>% group_by(Industry_simple,Year) %>%
  mutate(industry_annual_gva_sum_usd = sum(GVA_annual_usd,na.rm = TRUE),
         industry_annual_gva_mean_usd = mean(GVA_annual_usd,na.rm = TRUE),
         n_country = n_distinct(Country)) %>% ungroup() 



# check what needs to be addressed w/ missing GDP's or exchange rates (for troubleshooting)
missing <- filter(df_rates_final5,is.na(Exch_rate)) %>% select(Country) %>% distinct()




# df_gdp_metrics <- df_rates_final %>%
#   group_by(Country,Year,Industry_simple) %>% arrange(Country,Year,Industry_simple) %>% 
#   mutate(GDP_perc_norm=GVA_total_usd/GDP_annual_usd*100) %>% 
#     ungroup() %>% group_by(Country,Industry_simple) %>% 
#     mutate(GDP_perc_norm_mean=mean(GDP_perc_norm,na.rm = TRUE),
#                                GVA_total_mean=mean(GVA_total_usd,na.rm=TRUE),
#                                GVA_total_sum=sum(GVA_total_usd,na.rm=TRUE),
#                                GDP_change_relative = 100*(GVA_total_usd - lag(GVA_total_usd))/lag(GVA_total_usd),
#                                GDP_change_rel_mean=mean(GDP_change_relative,na.rm = TRUE)) %>%
#   ungroup() %>% group_by(Country) %>%
#   mutate(GDP_annual_mean=mean(GDP_annual_usd,na.rm=TRUE)) %>% # only by country
#   arrange(Country,Industry_simple,Year) 



## save output
# write.csv(df_rates_final5,paste0("output/fishmine_updated_usd_",start_year+1,end_year-1,".csv"),row.names = FALSE)



