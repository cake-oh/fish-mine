library(tidyverse)
library(RColorBrewer)

# load data
df_gdp <- read.csv("data/total_finaladd.csv")
df_fish <- read.csv("data/fish_finaladd.csv")
df_mine <- read.csv("data/mining_finaladd.csv")

## reformat
# grab annual GDPs
country_gdp <- select(df_gdp,Reference.area,TIME_PERIOD,OBS_VALUE) %>%
  rename(GDP_total=OBS_VALUE)

# join with mining
df_mine2 <- full_join(df_mine,country_gdp) %>%
  filter(Economic.activity=="Mining and quarrying")%>%
  arrange(Reference.area,TIME_PERIOD)

df_fish2 <- full_join(df_fish,country_gdp) %>%
  filter(Economic.activity=="Fishing and aquaculture")%>%
  arrange(Reference.area,TIME_PERIOD)

# merge the two
df_both <- bind_rows(df_fish2,df_mine2)

# rename the columns to match
df_gdp_calcs_og <- read.csv("output/df_gdp_calcs.csv")
dfc<-colnames(df_gdp_calcs_og)


df_both_renamed <- select(df_both,-c("X","OBS_STATUS","Observation.status")) %>%
  rename(Country_iso=REF_AREA, Country=Reference.area,
         Activity_code=ACTIVITY, Activity=Economic.activity,
         Year=TIME_PERIOD,GVA=OBS_VALUE,
         Currency_iso=CURRENCY) %>%
  group_by(Country,Activity) %>%
  mutate(GDP_perc = GVA/GDP_total*100, # % contribution
         time_diff = Year - lag(Year), 
         GDP_change_relative = ((GDP_perc - lag(GDP_perc)) / lag(GDP_perc)) / time_diff * 100,
         GDP_perc_mean=mean(GDP_perc,na.rm = TRUE), # mean % contr
         GDP_change_rel_mean = mean(GDP_change_relative,na.rm=TRUE), # mean % rel change
         GDP_annual_mean=mean(GDP_total,na.rm=TRUE) # mean national gdp
         ) %>%
  ungroup() %>%
  mutate(Country_short = if_else(stringr::str_detect(Country, " "),Country_iso, Country)) # iso if country is 2 words

# select countries from google sheets
# google sheets goal country list 
c <- c("AUS", "BEL", "BLZ", "BGR", "CAN", "CHL", "FJI",
       "FIN", "FRA", "DEU", "IND", "IDN", "JAM", "JPN",
       "KIR", "NRU", "NZL", "NOR", "PLW", "PAN", "PNG",
       "POL", "PRT", "WSM", "TON", "GBR", "USA", "VUT")

df_both_clean <- df_both_renamed %>%
  filter(Country_iso %in% c)

# what's left in the dataset
b <- unique(df_both_clean$Country_iso)

# what's missing (what we need to get data for)
setdiff(c,b)


#### plot ####

### [2] plots

### 2.1 Time Series Analysis of GDP

## relative change rates
ggplot(df_both_clean,
       aes(x = Year, y = GDP_change_relative,
           color=Country)) +
  geom_line()+
  geom_point(size=.7,alpha=0.6)+
  facet_wrap(~Activity,scales="free_y")+
  labs(title = paste0("Rates of Change in Relative GDP Contributions"),
       x = NULL,
       y = "Annual Change (%)",
       color="Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10),
        axis.text.y = element_text(size=10),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        plot.title = element_text(size=15),
        legend.position = "none")
# ggsave("plots/line_relchange.jpg")

## gdp contributions
ggplot(df_both_clean,
       aes(x = Year, y = GDP_perc,
           color=Country)) +
  geom_line()+
  geom_point(size=.7,alpha=0.6)+
  facet_wrap(~Activity,scales="free_y")+
  labs(title = paste0("Relative GDP Contributions"),
       x = NULL,
       y = "% GDP",
       color="Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10),
        axis.text.y = element_text(size=10),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        plot.title = element_text(size=15),
        legend.position = "none")
# ggsave("plots/line_relgdp.jpg")


### 2.2 boxplots of GDP and changes
## relative change rates
ggplot(df_both_clean,
       aes(x = Country_short, y = GDP_change_relative,color=Activity)) +
  # geom_abline(slope = 0, intercept = mean(df_gdp_calcs$GDP_change_relative[df_gdp_calcs$Activity == "Fishing"], na.rm = TRUE),
  #             color = brewer.pal(5,"Dark2")[1], linetype = "dashed") + 
  # geom_abline(slope = 0, intercept = mean(df_gdp_calcs$GDP_change_relative[df_gdp_calcs$Activity == "Mining"], na.rm = TRUE),
  #             color = brewer.pal(5,"Dark2")[2], linetype = "dashed") + 
  geom_boxplot(alpha=0.8)+
  geom_point(size=.7,alpha=0.6)+
  scale_color_brewer(palette="Dark2")+
  # facet_wrap(~Activity,scales="free_y")+
  labs(title = paste0("Rates of Change in Relative GDP Contributions"),
       x = NULL,
       y = "Annual Change (%)",
       color="Industry") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10),
        axis.text.y = element_text(size=10),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        plot.title = element_text(size=15))
# ggsave("plots/box_relchange.jpg")

##
ggplot(df_both_clean,
       aes(x = Country_short, y = GDP_perc,color=Activity)) +
  # geom_abline(slope = 0, intercept = mean(df_gdp_calcs$GDP_perc_norm[df_gdp_calcs$Activity == "Fishing"], na.rm = TRUE),
  #             color = brewer.pal(5,"Dark2")[1], linetype = "dashed") + 
  # geom_abline(slope = 0, intercept = mean(df_gdp_calcs$GDP_perc_norm[df_gdp_calcs$Activity == "Mining"], na.rm = TRUE),
  #             color = brewer.pal(5,"Dark2")[2], linetype = "dashed") + 
  geom_boxplot()+
  geom_point(size=.7,alpha=0.6)+
  # facet_wrap(~Activity, scales = "free_y") +
  labs(title = paste0("Relative GDP Contributions"),
       x = NULL,
       y = "% GDP",
       color="Industry") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10),
        axis.text.y = element_text(size=10),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        plot.title = element_text(size=15))
# ggsave("plots/box_relgdp.jpg")



### 2.3 stacked bar chart GDP contributions - aggregate

# prep data - remove any annual timeseries variables
gdp_contr <- group_by(df_both_clean,Country) %>% 
  mutate(both_rel_mean=sum(unique(GDP_perc_mean))) %>% ungroup() %>%
  arrange(desc(both_rel_mean)) %>%
  mutate(across(where(is.character), ~factor(., levels = unique(.)))) %>%
  select("Country_iso","Country","Country_short","Activity","GDP_annual_mean","GDP_perc_mean","GDP_change_rel_mean") %>%
  distinct()

# percentage (%)
ggplot(gdp_contr,
       aes(x = Country_short, y = GDP_perc_mean, fill=Activity)) +
  geom_bar(stat = "identity") +
  labs(title = "Economic Contribution of Fishing and Mining Sectors",
       x = NULL,
       y = "% GDP",
       fill = NULL) +
  scale_fill_brewer(palette = "Set2") + 
  # scale_y_continuous(limits=c(0,50),labels=seq(0,50,5),
  #                    breaks=seq(0,50,5),expand=c(0,0))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12),
        axis.text.y = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        plot.title = element_text(size=15),
        legend.position = c(.85,.85), legend.background = element_rect(fill="white",size=0.2))
# ggsave(paste0("plots/plot_barchart_percgdp_",gdp_type, ".jpg"))


### 2.3 stacked bar chart GDP contributions - individual

## plot 2 individual sectors
# process data
gdp_contr_fish <- filter(df_both_clean,Activity=="Fishing and aquaculture") %>%
  arrange(desc(GDP_perc_mean)) %>%   mutate(Country = factor(Country, levels = unique(Country))) %>%
  select(Country,Country_short,GDP_perc_mean,Activity) %>% distinct()
fish_range<- range(gdp_contr_fish$GDP_perc_mean)

gdp_contr_mine <- filter(df_both_clean,Activity=="Mining and quarrying") %>%
  arrange(desc(GDP_perc_mean)) %>%   mutate(Country = factor(Country, levels = unique(Country))) %>%
  select(Country,Country_short,GDP_perc_mean,Activity) %>% distinct()
mine_range<- range(gdp_contr_mine$GDP_perc_mean)

# plot
p_fish <- ggplot(gdp_contr_fish, aes(x = reorder(Country_short, -GDP_perc_mean), y = GDP_perc_mean, fill = Activity)) +
  geom_bar(stat = "identity", show.legend = FALSE,fill=brewer.pal(8, "Set2")[1]) +
  labs(title = paste0(unique(gdp_contr_fish$Activity)),
       x = NULL,y = "% GDP") +
  # scale_y_continuous(limits = c(floor(fish_range[1] / 5) * 5,ceiling(fish_range[2] / 5) * 5),expand=c(0,0))+
  scale_y_continuous(limits=c(0,2),expand=c(0,0))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size = 10),
        strip.text = element_text(size = 10, face = "bold"))
p_mine <- ggplot(gdp_contr_mine, aes(x = reorder(Country_short, -GDP_perc_mean), y = GDP_perc_mean, fill = Activity)) +
  geom_bar(stat = "identity", show.legend = FALSE,fill=brewer.pal(8, "Set2")[2]) +
  labs(title = paste0(unique(gdp_contr_mine$Activity)),
       x = NULL, y = NULL) +
  scale_y_continuous(limits = c(floor(mine_range[1] / 5) * 5,ceiling(mine_range[2] / 5) * 5),expand=c(0,0))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size = 10),
        strip.text = element_text(size = 10, face = "bold"))

cowplot::plot_grid(p_fish,p_mine) # combine em
# ggsave("plots/plot_barchart_percgdp_separate.jpg")



### 2.4 scatterplots fishing vs. mining contributions

fishing_data <- filter(df_both_clean,Activity=="Fishing and aquaculture") %>%
  rename(Fishing_Contribution = GDP_perc_mean,
         Fishing_Rel = GDP_change_rel_mean) %>%
  select(Country, Country_short, Fishing_Contribution,Fishing_Rel) %>% distinct()
mining_data <- filter(df_both_clean,Activity=="Mining and quarrying") %>%
  rename(Mining_Contribution = GDP_perc_mean,
         Mining_Rel = GDP_change_rel_mean) %>%
  select(Country,Country_short,Mining_Contribution,Mining_Rel) %>% distinct()

# Perform the left join, grab relevant columns, omit na's
combined_data <- full_join(fishing_data, mining_data) %>%
  # distinct() %>% 
  mutate(across(where(is.numeric), ~ coalesce(., 0))) # replace missing values with 0

# run clustering (4_makeclusters.R) to add membership
data_scaled_contr <- scale(combined_data[,c("Fishing_Contribution","Mining_Contribution")])
set.seed(123)  # Setting a random seed for reproducibility
k <- 4
kmeans_result_contr <- kmeans(data_scaled_contr, centers = k, nstart = 25)
# add cluster assignments
combined_data$cluster_contr <- as.factor(kmeans_result_contr$cluster)

## plot ##
# plot mean gdp %
scat_gdp_mean <- ggplot(data = combined_data, 
                        aes(x = Fishing_Contribution, y = Mining_Contribution,
                            color=cluster_contr)) + # for kmeans clusering
  geom_abline(slope = 1, intercept = 0, color = "lightgrey", linetype = "dashed") + # Add a line with slope of 1
  geom_point() +
  geom_text(aes(label = Country_short), size=4,
            nudge_x = 0, nudge_y = .8, check_overlap = FALSE) + # Add labels
  # scale_x_continuous(limits = c(0,30),labels=c(seq(0,30,5)),breaks=c(seq(0,30,5)))+
  # scale_y_continuous(limits = c(0,30),labels=c(seq(0,30,5)),breaks=c(seq(0,30,5)))+
  scale_color_brewer(palette = "Dark2") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text = element_text(size=14),
        axis.title = element_text(size=12),
        plot.title = element_text(size=12)) +
  labs(x = "Fishing", y = "Mining",title="Contribution to GDP (%)",
       subtitle=paste0(k," k-means clusters"))
plot(scat_gdp_mean)
# ggsave(paste0("plots/scatter_gdp_",gdp_type,".jpg"))



## plot mean relative growth
data_scaled_rel <- scale(combined_data[,c("Fishing_Rel","Mining_Rel")])
set.seed(123)  # Setting a random seed for reproducibility
k <-4
kmeans_result_rel <- kmeans(data_scaled_rel, centers = k, nstart = 25)
# add cluster assignments
combined_data$cluster_rel <- as.factor(kmeans_result_rel$cluster)

mine_range_rel <- range(combined_data$Mining_Rel) # make ranges
fish_range_rel <- range(combined_data$Fishing_Rel) 

scat_gdp_rel <- ggplot(data = combined_data, 
                       aes(x = Fishing_Rel, y = Mining_Rel,
                           color=cluster_rel)) +
  geom_hline(yintercept=0, color = "gray60") + 
  geom_vline(xintercept=0, color = "gray60") + 
  geom_point() + 
  geom_text(aes(label = Country_short), size=4,
            nudge_x = 0, nudge_y = 2, check_overlap = FALSE) + # Add labels
  scale_x_continuous(limits = c(floor(fish_range_rel[1] / 5) * 5,ceiling(fish_range_rel[2] / 10) * 10))+
  scale_y_continuous(limits = c(floor(mine_range_rel[1] / 5) * 5,ceiling(mine_range_rel[2] / 10) * 10))+
  scale_color_brewer(palette = "Dark2") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text = element_text(size=10),
        axis.title = element_text(size=14),
        plot.title = element_text(size=15)) +
  labs(x = "Fishing", y = "Mining",title="Average Annual Relative GDP Change (%)",
       subtitle=paste0(k," k-means clusters"))
plot(scat_gdp_rel)
# ggsave(paste0("plots/scatter_relchange_",gdp_type,".jpg"))
