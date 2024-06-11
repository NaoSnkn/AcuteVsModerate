# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(lubridate)
library("readxl")
library(ggpubr)

Temp <- read_excel("D:/AIMS/Master Thesis/Data/Temperature/ChickenDaviesSiteTemp.xlsx")
str(Temp) # Show you data properties
Temp$Temperature <- as.numeric(Temp$Temperature) # Make sure that Temperature is numeric
Temp$Time <- ymd_hms(Temp$Time, tz = "Australia/Brisbane") #Turn data from character to date

qqnorm(Temp$Temperature)
kruskal.test(Temp$Temperature~Temp$Site)

# Most basic bubble plot
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
?scale_x_datetime()
# Change axis to enflish
# Male axis start
# Change background grid
# Customize fonts and size

original_locale <- Sys.getlocale("LC_TIME")

# Changer la locale pour l'anglais
Sys.setlocale("LC_TIME", "en_US.UTF-8")
Sys.setlocale("LC_TIME", original_locale)


# Agrégation des données par jour et par site
Temp_DayAggregated <- Temp %>%
  mutate(Date = floor_date(Time, unit = "day")) %>%
  group_by(Site, Date) %>%
  summarise(
    MeanTemperature = mean(Temperature),
    MaxTemperature = max(Temperature),
    MinTemperature = min(Temperature),
    DiurnalVariation = max(Temperature)-min(Temperature)
  )

Temp_MonthAggregated <- Temp_DayAggregated |> 
  mutate(Month = floor_date(Date, unit = "month"))  |>
  group_by(Site, Month) |> 
  summarise(MeanDiurnalVariation = mean(DiurnalVariation))

Temp_MonthAggregated

Temp_MonthAggregated <- Temp_MonthAggregated |> 
  pivot_wider(names_from = Site, values_from = MeanDiurnalVariation) 
  wilcox.test(Temp_MonthAggregated$`Chicken Reef`, Temp_MonthAggregated$`Davies Reef`)

# Affichage des résultats

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

shapiro.test(Temp_DayAggregated$Temperature)

Temp_MinDay <- subset(Temp_DayAggregated, Type=="Daily Minimum")
Temp_MinDay <- Temp_MinDay %>%
  pivot_wider(names_from = Site, values_from = Temperature)
wilcox.test(Temp_MinDay$`Chicken Reef`, Temp_MinDay$`Davies Reef`)

Temp_MaxDay <- subset(Temp_DayAggregated, Type=="Daily Maximum")
Temp_MaxDay <- Temp_MaxDay %>%
  pivot_wider(names_from = Site, values_from = Temperature)
wilcox.test(Temp_MinDay$`Chicken Reef`, Temp_MinDay$`Davies Reef`)

Temp_MeanDay <- subset(Temp_DayAggregated, Type=="Mean daily")
Temp_MeanDay <- Temp_MeanDay |> 
  pivot_wider(names_from = Site, values_from = Temperature)
  wilcox.test(Temp_MeanDay$`Chicken Reef`, Temp_MeanDay$`Davies Reef`)
  
Temp_DayVar <- subset(Temp_DayAggregated, Type=="Diurnal variation")
Temp_DayVar <- Temp_DayVar |> 
  pivot_wider(names_from = Site, values_from = Temperature)
  wilcox.test(Temp_DayVar$`Chicken Reef`, Temp_DayVar$`Davies Reef`)
  
summary(Temp_DayVar$`Chicken Reef`) # Get mean, min and max
sd(Temp_DayVar$`Chicken Reef`) / sqrt(length(Temp_DayVar$`Chicken Reef`)) #Standard error
summary(Temp_DayVar$`Davies Reef`)
sd(Temp_DayVar$`Davies Reef`) / sqrt(length(Temp_DayVar$`Davies Reef`))


# Calculate standard error



Temp_DayAggregated$Type <- factor(Temp_DayAggregated$Type, levels = c("Daily Minimum","Daily Maximum","Mean daily","Diurnal variation"))

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


#Make grid
#Customize background
#Customize fonts

ggarrange(p, m, ncol = 1,
          labels = c("A","B"),
          nrow = 2, 
          heights = c(3,2))
?ggarrange()
