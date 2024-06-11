# Required Packages: 
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(lubridate)
library("readxl")
library(ggpubr)

# Call data: 
Temp <- read_excel("D:/AIMS/Master Thesis/Data/Temperature/ChickenDaviesSiteTemp.xlsx")
Temp$Temperature <- as.numeric(Temp$Temperature) # Make sure that Temperature is numeric
Temp$Time <- ymd_hms(Temp$Time, tz = "Australia/Brisbane") #Turn data from character to date

## DATA ANALYSIS ##

# Statistics on data set
qqnorm(Temp$Temperature) # Normality assessment (not normal)
kruskal.test(Temp$Temperature~Temp$Site) # Difference between sites

# Data aggregation
    # Per day
Temp_DayAggregated <- Temp %>%
  mutate(Date = floor_date(Time, unit = "day")) %>%
  group_by(Site, Date) %>%
  summarise(
    MeanTemperature = mean(Temperature),
    MaxTemperature = max(Temperature),
    MinTemperature = min(Temperature),
    DiurnalVariation = max(Temperature)-min(Temperature)
  )
Temp_DayAggregated <- Temp_DayAggregated %>%
  pivot_longer(cols = c(MinTemperature, MeanTemperature, MaxTemperature, DiurnalVariation), 
               names_to = "Type", 
               values_to = "Temperature") %>%
  mutate(Type = case_when(
    Type == "MaxTemperature" ~ "Daily Maximum",
    Type == "MinTemperature" ~ "Daily Minimum",
    Type == "MeanTemperature" ~ "Mean daily",
    Type == "DiurnalVariation" ~ "Diurnal variation"
  ))
Temp_DayAggregated 

    # Per month
Temp_MonthAggregated <- Temp_DayAggregated |> 
  mutate(Month = floor_date(Date, unit = "month"))  |>
  group_by(Site, Month) |> 
  summarise(MeanDiurnalVariation = mean(DiurnalVariation))
Temp_MonthAggregated

    # Wilcoxon tests #

# Compare Mean Diurnal Variation per Month
Temp_MonthAggregated <- Temp_MonthAggregated |> 
  pivot_wider(names_from = Site, values_from = MeanDiurnalVariation) 
  wilcox.test(Temp_MonthAggregated$`Chicken Reef`, Temp_MonthAggregated$`Davies Reef`)

# Compare Daily Minimum Temperature
Temp_MinDay <- subset(Temp_DayAggregated, Type=="Daily Minimum")
Temp_MinDay <- Temp_MinDay %>%
  pivot_wider(names_from = Site, values_from = Temperature)
wilcox.test(Temp_MinDay$`Chicken Reef`, Temp_MinDay$`Davies Reef`)

# Compare Daily Maximum Temperature
Temp_MaxDay <- subset(Temp_DayAggregated, Type=="Daily Maximum")
Temp_MaxDay <- Temp_MaxDay %>%
  pivot_wider(names_from = Site, values_from = Temperature)
wilcox.test(Temp_MinDay$`Chicken Reef`, Temp_MinDay$`Davies Reef`)

# Compare Daily Temperature Mean
Temp_MeanDay <- subset(Temp_DayAggregated, Type=="Mean daily")
Temp_MeanDay <- Temp_MeanDay |> 
  pivot_wider(names_from = Site, values_from = Temperature)
  wilcox.test(Temp_MeanDay$`Chicken Reef`, Temp_MeanDay$`Davies Reef`)

# Compare Diurnal temperature variation
Temp_DayVar <- subset(Temp_DayAggregated, Type=="Diurnal variation")
Temp_DayVar <- Temp_DayVar |> 
  pivot_wider(names_from = Site, values_from = Temperature)
  wilcox.test(Temp_DayVar$`Chicken Reef`, Temp_DayVar$`Davies Reef`)

# Other stats
summary(Temp_DayVar$`Chicken Reef`) # Get mean, min and max
sd(Temp_DayVar$`Chicken Reef`) / sqrt(length(Temp_DayVar$`Chicken Reef`)) #Standard error
summary(Temp_DayVar$`Davies Reef`)
sd(Temp_DayVar$`Davies Reef`) / sqrt(length(Temp_DayVar$`Davies Reef`))

    ## DATA VIZUALISATION ##

  ## Temperature profile throughout the year ##

p <- ggplot(Temp, aes(x=Time, y=Temperature, color=Site)) +
  geom_line(linewidth=0.5) +
  xlab("Date") +
  ylab("Temperature per day (°C)")+
  scale_color_manual(values = c("#A994F0", "#EDD263"))+
  scale_x_datetime(date_labels = "%B %Y",date_breaks = "2 month") + # axis appears Month Year
  theme(axis.text.x=element_text(family="Product Sans"),
        axis.text.y=element_text(family = "Imprima"),
        axis.title = element_text(family = "Product Sans"),
        legend.title = element_text(family="Product Sans"),
        legend.text = element_text(family="Product Sans Light")) +
  ylim(22.5,31.5)
p

# Arrange
Temp_DayAggregated$Type <- factor(Temp_DayAggregated$Type, levels = c("Daily Minimum","Daily Maximum","Mean daily","Diurnal variation"))

    ## Boxplots ##

m <- ggplot(data=Temp_DayAggregated, aes(x=Site, y=Temperature, fill=Site)) +
  geom_boxplot()+
  scale_fill_manual(values = c("#A994F0", "#EDD263"))+
  facet_wrap(~Type, scale="free_y", ncol=4)+
  stat_compare_means(label =  "p.signif", label.x = 1.5, vjust=0.5)+
  theme(axis.text.y=element_text(family = "Imprima"),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(family="Product Sans"),
        axis.title = element_text(family = "Product Sans"),
        legend.title = element_text(family="Product Sans"),
        legend.text = element_text(family="Product Sans Light"))
m

ggarrange(p, m, ncol = 1,
          labels = c("A","B"),
          nrow = 2, 
          heights = c(3,2))
