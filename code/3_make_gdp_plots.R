## make gdp plots: annual growth rates ##

#### [0] prep ####
library(tidyverse)
library(RColorBrewer)

## load data
df_gdp_calcs_og <- read.csv("output/df_gdp_calcs.csv")

#### [1] prep data ####
df_gdp_calcs <- filter(df_gdp_calcs_og, ## pick aggregate level and gdp type
                       Metric_type == "Constant")
df_gdp_calcs_1 <- filter(df_gdp_calcs,Aggregate_level==1)

# union(setdiff(a,b), setdiff(b,a)) # check which countires aren't in both


#### [2] plots ####

#### 2.1 :) timeseries of GDP and changes ####
## gdp contributions
ggplot(df_gdp_calcs,
       aes(x = Year, y = GDP_perc_norm,
           color=Country)) +
  geom_line()+
  geom_point(size=.7,alpha=0.6)+
  facet_wrap(~Industry_simple,scales="free_y")+
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

## relative change rates
ggplot(df_gdp_calcs,
       aes(x = Year, y = GDP_change_relative,
           color=Country)) +
  geom_line()+
  geom_point(size=.7,alpha=0.6)+
  facet_wrap(~Industry_simple,scales="free_y")+
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



#### 2.2 :) boxplots of GDP and changes ####
##
ggplot(df_gdp_calcs,
       aes(x = Country_iso, y = GDP_perc_norm,color=Industry_simple)) +
  geom_abline(slope = 0, intercept = mean(df_gdp_calcs$GDP_perc_norm[df_gdp_calcs$Industry_simple == "Fishing"], na.rm = TRUE),
              color = brewer.pal(5,"Dark2")[1], linetype = "dashed") + 
  geom_abline(slope = 0, intercept = mean(df_gdp_calcs$GDP_perc_norm[df_gdp_calcs$Industry_simple == "Mining"], na.rm = TRUE),
              color = brewer.pal(5,"Dark2")[2], linetype = "dashed") + 
  geom_boxplot()+
  geom_point(size=.7,alpha=0.6)+
  # facet_wrap(~Industry_simple, scales = "free_y") +
  labs(title = paste0("Relative GDP Contributions"),
       x = NULL,
       y = "Percentage (%)",
       color="Industry") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10),
        axis.text.y = element_text(size=10),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        plot.title = element_text(size=15))


## relative change rates
ggplot(df_gdp_calcs,
       aes(x = Country_iso, y = GDP_change_relative,color=Industry_simple)) +
  geom_abline(slope = 0, intercept = mean(df_gdp_calcs$GDP_change_relative[df_gdp_calcs$Industry_simple == "Fishing"], na.rm = TRUE),
              color = brewer.pal(5,"Dark2")[1], linetype = "dashed") + 
  geom_abline(slope = 0, intercept = mean(df_gdp_calcs$GDP_change_relative[df_gdp_calcs$Industry_simple == "Mining"], na.rm = TRUE),
              color = brewer.pal(5,"Dark2")[2], linetype = "dashed") + 
  geom_boxplot(alpha=0.8)+
  geom_point(size=.7,alpha=0.6)+
  scale_color_brewer(palette="Dark2")+
  # facet_wrap(~Industry_simple,scales="free_y")+
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


#### 2.3 :) stacked bar chart GDP contributions - aggregate ####
# prep data - remove any annual timeseries variables
gdp_contr <- group_by(df_gdp_calcs,Country) %>% 
  mutate(both_rel_mean=sum(unique(GDP_perc_norm_mean))) %>% ungroup() %>%
  group_by(Country,Industry_simple,Aggregate_level) %>%
  mutate(industry_total_gva=sum(GVA_total)) %>% ungroup() %>%
  select(-c("Industry","Year","GVA_total","Base_year","GDP_change_abs_mean","GDP_change_rel_mean", "Effective_year_GDP","GDP_annual","GDP_perc_norm","GDP_change_absolute","GDP_change_relative")) %>%
  distinct() %>%
  arrange(desc(both_rel_mean)) %>%
  mutate(across(where(is.character), ~factor(., levels = unique(.)))) %>%
  group_by(Country, Industry_simple) %>%
  mutate(percent_weight = industry_total_gva / sum(industry_total_gva[Aggregate_level == 1])) %>% ungroup() %>%
  mutate(agg1_yn = ifelse(Aggregate_level==1,as.character(1),as.character(0)))
# write.csv(gdp_contr,"output/gdp_contr.csv",row.names = FALSE)


gva_level1 <- gdp_contr %>%
  filter(Aggregate_level == 1) %>%
  select(Country, Industry_simple, GVA_total_allyears) %>%
  rename(GVA_level1 = GVA_total_allyears)
data_merged <- gdp_contr %>%
  left_join(gva_level1, by = c("Country", "Industry_simple")) %>%
  mutate(Proportion = case_when(
    Aggregate_level == 1 ~ 1,
    TRUE ~ GVA_total_allyears / GVA_level1
  ))

# Assuming 'data_merged' is already your working dataset
# We need to adjust the data for plotting. Aggregate levels will be adjusted to plot on top of level 1
data_for_plot <- data_merged %>%
  group_by(Country, Industry_simple) %>%
  mutate(percent_weight = Weight / sum(Weight) * 100)

# Plotting
# cols <- brewer.pal(4,"Set2")
p <- ggplot(gdp_contr, 
            aes(x = Country_short, y = percent_weight, 
                fill = interaction(agg1_yn,Industry_simple))) +
  facet_wrap(~Industry_simple)+
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "National Importance of Various Sectors",
       subtitle = "Percent contribution to real GDP, proportionally adjusted by GVA",
       x = NULL,
       y = "% of GDP",
       fill = "Industry and Level") +
  scale_fill_brewer(palette = rev("Set2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 15),
        legend.position = "bottom",
        legend.background = element_rect(fill="white", size=0.2))
plot(p)

# percentage (%)
p<-ggplot(gdp_contr %>% filter(Aggregate_level==1), 
          aes(x = Country_short, y = GDP_perc_norm_mean, 
                      fill=Industry_simple)) +
  geom_bar(stat = "identity") +
  labs(title = "National Importance of Fishing and Mining Sectors",
       subtitle = paste0("Percent contribution to real GDP"),
       x = NULL,
       y = "%",
       fill = "Industry") +
  scale_fill_brewer(palette = "Set2") + 
  # scale_y_continuous(limits = c(0,50),expand=c(0,0))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10),
        axis.text.y = element_text(size=10),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        plot.title = element_text(size=15),
        legend.position = c(.85,.85), legend.background = element_rect(fill="white",size=0.2))
plot(p)
# ggsave(paste0("output/plot_barchart_percgdp_",gdp_type, ".jpg"))

p + geom_bar_pattern(data = subset(gdp_contr, Aggregate_level %in% c(2)),
                     aes(x = Country_short, y = GDP_perc_norm_mean, fill = Industry_simple),
                     stat = "identity", position = "stack",
                     pattern = "stripe", pattern_angle = 45,
                     pattern_density = .4, pattern_spacing = 0.1,
                     pattern_color = "black", alpha = 0.5)


#### 2.3 :) stacked bar chart GDP contributions - individual ####
# individual sectors
gdp_contr_fish <- filter(gdp_contr,Industry_simple=="Fishing") %>%
  arrange(desc(GDP_perc_norm_mean)) %>%   mutate(Country = factor(Country, levels = unique(Country)))
fish_range<- range(gdp_contr_fish$GDP_perc_norm_mean)
gdp_contr_mine <- filter(gdp_contr,Industry_simple=="Mining") %>%
  arrange(desc(GDP_perc_norm_mean)) %>%   mutate(Country = factor(Country, levels = unique(Country)))
mine_range<- range(gdp_contr_mine$GDP_perc_norm_mean)

p_fish <- ggplot(subset(gdp_contr_fish,Aggregate_level==1), 
                 aes(x = reorder(Country_iso, -GDP_perc_norm_mean), y = GDP_perc_norm_mean, fill = Industry_simple)) +
  geom_bar(stat = "identity", show.legend = FALSE,fill=brewer.pal(8, "Set2")[1]) +
  labs(title = paste0(unique(gdp_contr_fish$Industry_simple)),
       x = NULL,y = "% GDP") +
  scale_y_continuous(limits = c(floor(fish_range[1] / 5) * 5,ceiling(fish_range[2] / 5) * 5),expand=c(0,0))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"))
p_fish
p_mine <- ggplot(gdp_contr_mine, aes(x = reorder(Country_iso, -GDP_perc_norm_mean), y = GDP_perc_norm_mean, fill = Industry_simple)) +
  geom_bar(stat = "identity",position="stack", show.legend = FALSE,fill=brewer.pal(8, "Set2")[2]) +
  facet_wrap(~Aggregate_level, scales = "free_x") + # Separate plots for each Aggregate_level
  labs(title = paste0(unique(gdp_contr_mine$Industry_simple)),
       x = NULL, y = NULL) +
  scale_y_continuous(limits = c(floor(mine_range[1] / 5) * 5,ceiling(mine_range[2] / 5) * 5),expand=c(0,0))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"))

cowplot::plot_grid(p_fish,p_mine) # combine em


p_fish + geom_bar_pattern(data = subset(gdp_contr_fish, Aggregate_level %in% c(4)),
                     aes(x = Country_short, y = GDP_perc_norm_mean, fill = Industry_simple),
                     stat = "identity", position = "stack",
                     pattern = "stripe", pattern_angle = 45,
                     pattern_density = .4, pattern_spacing = 0.1,
                     pattern_color = "black", alpha = 0.5)





#### 2.4 :) scatterplots fishing vs. mining contributions ####
# df <- select(df_gdp_calcs,Country,Industry_simple,GDP_perc_norm_mean,GDP_change_rel_mean)
# df2 <- df %>% group_by(GDP_perc_norm_mean,GDP_change_rel_mean) %>% summarise()
fishing_data <- filter(df_gdp_calcs, Industry_simple == "Fishing") %>%
  rename(Fishing_Contribution = GDP_perc_norm_mean,
         Fishing_Rel = GDP_change_rel_mean) 
mining_data <- filter(df_gdp_calcs, Industry_simple == "Mining") %>%
  rename(Mining_Contribution = GDP_perc_norm_mean,
         Mining_Rel = GDP_change_rel_mean) 

# Perform the left join, grab relevant columns, omit na's
combined_data <- full_join(fishing_data, mining_data, by = c("Country","Year")) %>%
  select(Country, Country_iso.y,Fishing_Contribution, Mining_Contribution, Fishing_Rel, Mining_Rel) %>%
  distinct() %>% rename(Country_iso=Country_iso.y) %>%
  filter(rowSums(is.na(.)) == 0)

# run clustering (4_makeclusters.R) to add membership
data_scaled_contr <- scale(combined_data[,c("Fishing_Contribution","Mining_Contribution")])
set.seed(123)  # Setting a random seed for reproducibility
k <- 4
kmeans_result_contr <- kmeans(data_scaled_contr, centers = k, nstart = 25)
# add cluster assignments
combined_data$cluster_contr <- as.factor(kmeans_result_contr$cluster)

## plot ##
# establish ranges for axes
# m_range <- range(combined_data$Mining_Rel)
# f_range <- range(combined_data$Fishing_Rel)

# plot mean gdp %
scat_gdp_mean <- ggplot(data = combined_data, 
                        aes(x = Fishing_Contribution, y = Mining_Contribution,
                            color=cluster_contr)) + # for kmeans clusering
  geom_abline(slope = 1, intercept = 0, color = "lightgrey", linetype = "dashed") + # Add a line with slope of 1
  geom_point() +
  geom_text(aes(label = Country_iso), size=4,
            nudge_x = 0, nudge_y = .8, check_overlap = FALSE) + # Add labels
  scale_x_continuous(limits = c(0,30),labels=c(seq(0,30,5)),breaks=c(seq(0,30,5)))+
  scale_y_continuous(limits = c(0,30),labels=c(seq(0,30,5)),breaks=c(seq(0,30,5)))+
  scale_color_brewer(palette = "Dark2") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text = element_text(size=10),
        axis.title = element_text(size=14),
        plot.title = element_text(size=15)) +
  labs(x = "Fishing", y = "Mining",title="Contribution to GDP (%)",
       subtitle=paste0(k," k-means clusters"))
plot(scat_gdp_mean)
# ggsave(paste0("output/scatter_gdp_",gdp_type,".jpg"))

## plot mean relative growth
data_scaled_rel <- scale(combined_data[,c("Fishing_Rel","Mining_Rel")])
set.seed(123)  # Setting a random seed for reproducibility
k <- 4
kmeans_result_rel <- kmeans(data_scaled_rel, centers = k, nstart = 25)
# add cluster assignments
combined_data$cluster_rel <- as.factor(kmeans_result_rel$cluster)


scat_gdp_rel <- ggplot(data = combined_data, 
                       aes(x = Fishing_Rel, y = Mining_Rel,
                           color=cluster_rel)) +
  geom_hline(yintercept=0, color = "gray60") + 
  geom_vline(xintercept=0, color = "gray60") + 
  geom_point() + 
  geom_text(aes(label = Country_iso), size=4,
            nudge_x = 0, nudge_y = 2, check_overlap = FALSE) + # Add labels
  scale_x_continuous(limits = c(-15,20),labels=c(seq(-15,20,5)),breaks=c(seq(-15,20,5)))+
  scale_y_continuous(limits = c(-15,75),labels=c(seq(-15,75,15)),breaks=c(seq(-15,75,15)))+
  scale_color_brewer(palette = "Dark2") + 
  theme_minimal() + 
  theme(legend.position="none",
        axis.text = element_text(size=10),
        axis.title = element_text(size=14),
        plot.title = element_text(size=15)) +
  labs(x = "Fishing", y = "Mining",title="Average Annual Relative GDP Change (%)",
       subtitle=paste0(k," k-means clusters"))
plot(scat_gdp_rel)
# ggsave(paste0("output/scatter_relchange_",gdp_type,".jpg"))



#### :) 2.5 facet_wrap data completion by aggregate level ####
ggplot(gdp_contr, 
       aes(x = Country_short, y = GDP_perc_norm_mean,fill=Industry_simple),
       show.legend=FALSE) +
  geom_bar(stat = "identity",position="stack")+
  scale_fill_brewer(palette = "Dark2") + 
  labs(title = "Data availability: aggregate levels",
       x = NULL, y = "% GDP",
       fill = "Industry") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.85,0.3)) +
  facet_wrap(~Aggregate_level, scales = "free")  # Separate plots for each Aggregate_level
# ggsave("output/barchart_aggregates.jpg")  



#### percent GDP contributions ####
# Group by Industry_simple and Country, summarize to find maximum GDP_perc_norm for each country
top_countries_GDP_perc_norm <- df_gdp_calcs %>%
  group_by(Industry_simple, Country) %>%
  summarise(max_GDP_perc_norm = max(GDP_perc_norm)) %>%
  arrange(desc(max_GDP_perc_norm)) %>%
  top_n(5)

# Filter the original dataset to include only the top countries
df_top_GDP_perc_norm <- df_gdp_calcs %>%
  filter(Country %in% top_countries_GDP_perc_norm$Country &
           Industry_simple %in% top_countries_GDP_perc_norm$Industry_simple)

## plots
# top 5
top5_perc_gdp <- ggplot(df_top_GDP_perc_norm, aes(x = Year, y = GDP_perc_norm, color = Country)) +
  geom_line() + geom_point()+
  facet_wrap(~Industry_simple, scales = "free_y") +
  labs(title = "Top 5 Countries with Highest Percent GDP Contribution",
       subtitle = paste(df_gdp_calcs$Metric_type, "GDP"),
       x = "Year",
       y = "Percent GDP")
plot(top5_perc_gdp)
# ggsave("output/plot_top5_percgdp.jpg")

# all countries
all_perc_gdp <- ggplot(df_gdp_calcs, aes(x = Year, y = GDP_perc_norm, color = Country)) +
  geom_line() + geom_point()+
  facet_wrap(~Industry_simple, scales = "free_y") +
  labs(title = "All Countries Percent GDP Contribution",
       subtitle = paste(df_gdp_calcs$Metric_type, "GDP"),
       x = "Year",
       y = "Percent GDP")
plot(all_perc_gdp)

#### plot relative rate of GDP change ####
## relative GDP contributions
# Group by Industry_simple and Country, summarize to find maximum GDP_perc_norm for each country
top_countries_GDP_change_relative <- df_gdp_calcs %>%
  group_by(Industry_simple, Country) %>%
  summarise(max_GDP_change_relative = max(GDP_change_relative,na.rm=TRUE)) %>%
  arrange(desc(max_GDP_change_relative)) %>%
  top_n(5)

# Filter the original dataset to include only the top countries
df_top_GDP_change_relative <- df_gdp_calcs %>%
  filter(Country %in% top_countries_GDP_change_relative$Country &
           Industry_simple %in% top_countries_GDP_change_relative$Industry_simple)






