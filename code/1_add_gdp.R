## join GDPs from World Bank to economic accounts data ##

#### [0] prep ####
library(googlesheets4)
library(tidyverse)

## load data 
# economic accounts data (EA)
# load from google docs
# df_fishing_og <- read_sheet("https://docs.google.com/spreadsheets/d/1ro3LL3v-KByzsWK5Jwem4FLGH9uGR5anq2FRhpafoYM/edit?usp=sharing")
# 1 or 2
# df_mining_og <- read_sheet("https://docs.google.com/spreadsheets/d/1ro3LL3v-KByzsWK5Jwem4FLGH9uGR5anq2FRhpafoYM/edit?usp=sharing", sheet=2)
# save to computer
# write.csv(df_fishing_og,"data/df_fishing_og.csv",row.names = FALSE)
# write.csv(df_mining_og,"data/df_mining_og.csv",row.names = FALSE)

# load recent local folder ^
df_fishing_og <- read_csv("data/df_fishing_og.csv")
df_mining_og <- read_csv("data/df_mining_og.csv")

# World Bank data (WB): https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.MKTP.CN&country=AUS#
df_wb_og <- read.csv("data/worldbank_current_lcu.csv")


#### [1] process data ####
## EA: grab countries' year and base year + convert to numeric / replace "Current" with 999999
year_fishing <- df_fishing_og %>%
  select(Country,Metric_type,Year, Base_year) %>%
  mutate(across(Base_year, ~ifelse(is.na(as.numeric(as.character(.))), 999999, as.numeric(as.character(.))))) 

year_mining <- df_mining_og %>%
  select(Country,Metric_type,Year, Base_year) %>%
  mutate(across(Base_year, ~ifelse(is.na(as.numeric(as.character(.))), 999999, as.numeric(as.character(.)))))

# combine mining + fishing 
year_comb <- bind_rows(year_fishing,year_mining) %>%
  mutate(Effective_year_GDP = ifelse(Metric_type == "Constant", Base_year, Year))


## WB: clean + rearrange to long format
# 2 NA's because of missing data for 2022 Palau and Tonga
df_gdp <- filter(df_wb_og,Series.Name=="GDP (current LCU)", # filter blank rows
                 Country.Name %in% unique(year_comb$Country) |
                   Country.Name %in% c("Russian Federation","Korea, Rep.")) %>% # filter countries in our data
  select(-Series.Name,-Series.Code) %>%
  mutate(across(3:26, ~ifelse(. == "..", NA, as.numeric(as.character(.))))) %>% # change ".." to numeric
  pivot_longer(cols = 3:26, 
               names_to = "Year", 
               values_to = "GDP_annual") %>% # make long
  mutate(Year = as.numeric(sub(".*?(\\d{4}).*", "\\1", Year))) %>% # convert col names to useful years
  rename(Country=Country.Name) %>% # rename for join
  mutate(Country = recode(Country, "Russian Federation" = "Russia"),
         Country = recode(Country, "Korea, Rep." = "South Korea"))
  

#### [2] join gdp to economic accounts by country and effective year ####
df_join <- left_join(year_comb,df_gdp, 
                  by = c("Country", "Effective_year_GDP" = "Year"))


#### [3] re-join to original dataframes ####
## fishing
df_fishing_rejoin <- mutate(df_fishing_og,
                            Effective_year_GDP = as.numeric(ifelse(Metric_type == "Constant", Base_year, Year))) %>% # add key column 'effective_year_gdp'
  left_join(distinct(df_join[,c("Country","Effective_year_GDP","GDP_annual")]),
            by=c("Country","Effective_year_GDP")) %>%
  select(-GDP_annual.x) %>% rename(GDP_annual=GDP_annual.y) %>%
  relocate(GDP_annual,.after=Base_year) %>%
  relocate(Effective_year_GDP, .after=Base_year) 
  
## mining
df_mining_rejoin <- mutate(df_mining_og,
                            Effective_year_GDP = as.numeric(ifelse(Metric_type == "Constant", Base_year, Year))) %>% # add key column 'effective_year_gdp'
  left_join(distinct(df_join[,c("Country","Effective_year_GDP","GDP_annual")]),
            by=c("Country","Effective_year_GDP")) %>%
  select(-GDP_annual.x) %>% rename(GDP_annual=GDP_annual.y) %>%
  relocate(GDP_annual,.after=Base_year) %>%
  relocate(Effective_year_GDP, .after=Base_year) 

#### save output ####
## convert list variables
sapply(df_fishing_rejoin,mode)
# df_fishing_rejoin$Base_year <- as.character(df_fishing_rejoin$Base_year)
# df_fishing_rejoin$GVA_abbrev <- as.character(df_fishing_rejoin$GVA_abbrev)
# df_fishing_rejoin$Code_num <- as.character(df_fishing_rejoin$Code_num)

write.csv(df_fishing_rejoin,"output/df_fishing_rejoin.csv",row.names = FALSE)


sapply(df_mining_rejoin,mode)
# df_mining_rejoin$Code_num <- as.character(df_mining_rejoin$Code_num)
# df_mining_rejoin$Base_year <- as.character(df_mining_rejoin$Base_year)
# df_mining_rejoin$GVA_abbrev <- as.character(df_mining_rejoin$GVA_abbrev)

write.csv(df_mining_rejoin,"output/df_mining_rejoin.csv",row.names = FALSE)


#### next: 2_get_gdp_calcs.R ####


                  
                  
                  
                  
                  