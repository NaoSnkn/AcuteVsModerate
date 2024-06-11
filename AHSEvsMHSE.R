library("readxl")
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(extrafont)

# Load fonts
font_import(paths ="D:/AIMS/Master Thesis/Fonts") # Fonts have to be TrueType # Change fonts
loadfonts()

# Call data
MHSE <- read_excel("D:/AIMS/Master Thesis/Data/Moderate/MetadataModerateAhya_Final.xlsx") # Call Moderate data
AHSE <- read_excel("D:/AIMS/Master Thesis/Data/Acute/Compiled_Ahya_Acute_Final.xlsx") # Call Acute data

# Remove genotype that cannot be used (x2 MMM7 missing because of F missing value)
AHSE <- subset(AHSE, AHSE$Genotype !="292" & AHSE$Genotype !="355" & AHSE$Genotype !="1839"& AHSE$Genotype !="1840" & AHSE$Genotype !="270")
# Remove genotype that cannot be used (Genotype not available for both treatments)
MHSE <- subset(MHSE, MHSE$Genotype !="301" & MHSE$Genotype !="305" & MHSE$Genotype !="335")

#------------------------#
### Models analysis ####
#------------------------#

Required packages:
library(emmeans) # for paired comparison
library("DHARMa") # to see residuals
library(glmmTMB) # generalized, different distribution
library(car) # Do ANOVA on glmm model
library(fitdistrplus) # To identify distribution of data
library(univariateML) # Confirm data distribution

#-----# AHSE ANALYSIS #--------#

    ## Statistics ##

Acute <- AHSE |> mutate(fTemp=factor(Temperature))
Acute$FvFm[Acute$FvFm == 0] <- 0.0000000000000001 # beta do not include 0

# Identify FvFm distribution 
descdist(Acute$FvFm, discrete = FALSE)
model_select(Acute$FvFm) # Skew cannot be modelled with classic functions

# Try different models

control_mod = glmmTMBControl(optCtrl=list(iter.max = 10000, eval.max = 10000)) 
A_mod1 <- glmmTMB(FvFm ~ fTemp*Genotype + (1|Tank)+(1|Reef), 
                dispformula = ~fTemp,
                data = Acute,
                family = "beta_family",# Only consider treatment
                control=control_mod)
resid <- simulateResiduals(A_mod1, plot=TRUE) # Residuals diagnostic
summary(A_mod1)
Anova(A_mod1)

a <- glmmTMB(FvFm ~ fTemp*Genotype + (1|Tank)+(1|Reef), dispformula = ~fTemp,data = Acute,family = "beta_family",control=control_mod)
b <- glmmTMB(FvFm ~ Genotype+fTemp + (1|Tank)+(1|Reef), dispformula = ~fTemp,data = Acute,family = "beta_family",control=control_mod)
c <- glmmTMB(FvFm ~ fTemp + (1|Tank)+(1|Reef), dispformula = ~fTemp,data = Acute,family = "beta_family",control=control_mod)
d <- glmmTMB(FvFm ~ Genotype + (1|Tank)+(1|Reef), dispformula = ~fTemp,data = Acute,family = "beta_family",control=control_mod)
anova(a,b,c,d) # Compare models

A_mod1.em <- emmeans(A_mod1, ~Genotype|fTemp, type="response") |> 
  pairs() |> 
  summary(infer=TRUE) 
A_mod1.em # Pairwise comparison between genets

    ## Vizualisation ##

# Pairwise comparison vizualisation between genets

# At 29°C
A_mod1.em |> as.data.frame() |> 
  filter(fTemp=='29') |> 
  tidyr::separate(contrast,into=c('Genotype1','Genotype2'), sep='/') |> 
  filter(!is.na(odds.ratio)) |> # only wthout na
  droplevels() |> 
  mutate(col=ifelse(asymp.UCL<1, 'Negative', ifelse(asymp.LCL>1, 'Positive','Neutral'))) |> 
  ggplot(aes(x=odds.ratio, y=Genotype2))+ # Show significant differences between genotypes for day
  geom_hline(yintercept=1)+
  geom_pointrange(aes(xmin=asymp.UCL,xmax=asymp.LCL, colour=col))+
  facet_wrap(Genotype1~., scales='free_y', nrow=3)+
  scale_x_continuous(trans = scales::log2_trans())+
  scale_color_manual('',breaks=c('Negative','Neutral','Positive'),values=c("#F66060","black","#87C43D"))+
  theme(axis.title = element_text(family="Nunito"),
        axis.text.y = element_text(family="Imprima"),
        axis.text.x = element_text(family="Product Sans", angle=90, vjust=0.5, hjust=1),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Nunito"),
        legend.text = element_text(family="Product Sans"), 
        panel.grid.minor=element_blank()
  )

# At 33°C
A_mod1.em |> as.data.frame() |> 
  filter(fTemp=='33') |> 
  tidyr::separate(contrast,into=c('Genotype1','Genotype2'), sep='/') |> 
  filter(!is.na(odds.ratio)) |> # only wthout na
  droplevels() |> 
  mutate(col=ifelse(asymp.UCL<1, 'Negative', ifelse(asymp.LCL>1, 'Positive','Neutral'))) |> 
  ggplot(aes(x=odds.ratio, y=Genotype2))+ # Show significant differences between genotypes for day
  geom_hline(yintercept=1)+
  geom_pointrange(aes(xmin=asymp.UCL,xmax=asymp.LCL, colour=col))+
  facet_wrap(Genotype1~., scales='free_y', nrow=3)+
  scale_x_continuous(trans = scales::log2_trans())+
  scale_color_manual('',breaks=c('Negative','Neutral','Positive'),values=c("#F66060","black","#87C43D"))+
  theme(axis.title = element_text(family="Nunito"),
        axis.text.y = element_text(family="Imprima"),
        axis.text.x = element_text(family="Product Sans", angle=90, vjust=0.5, hjust=1),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Nunito"),
        legend.text = element_text(family="Product Sans"), 
        panel.grid.minor=element_blank()
  )

# At 36°C
A_mod1.em |> as.data.frame() |> 
  filter(fTemp=='36') |> 
  tidyr::separate(contrast,into=c('Genotype1','Genotype2'), sep='/') |> 
  filter(!is.na(odds.ratio)) |> # only wthout na
  droplevels() |> 
  mutate(col=ifelse(asymp.UCL<1, 'Negative', ifelse(asymp.LCL>1, 'Positive','Neutral'))) |> 
  ggplot(aes(x=odds.ratio, y=Genotype2))+ # Show significant differences between genotypes for day
  geom_hline(yintercept=1)+
  geom_pointrange(aes(xmin=asymp.UCL,xmax=asymp.LCL, colour=col))+
  facet_wrap(Genotype1~., scales='free_y', nrow=3)+
  scale_x_continuous(trans = scales::log2_trans())+
  scale_color_manual('',breaks=c('Negative','Neutral','Positive'),values=c("#F66060","black","#87C43D"))+
  theme(axis.title = element_text(family="Nunito"),
        axis.text.y = element_text(family="Imprima"),
        axis.text.x = element_text(family="Product Sans", angle=90, vjust=0.5, hjust=1),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Nunito"),
        legend.text = element_text(family="Product Sans"), 
        panel.grid.minor=element_blank()
  )

# At 39°C
A_mod1.em |> as.data.frame() |> 
  filter(fTemp=='39') |> 
  tidyr::separate(contrast,into=c('Genotype1','Genotype2'), sep='/') |> 
  filter(!is.na(odds.ratio)) |> # only wthout na
  droplevels() |> 
  mutate(col=ifelse(asymp.UCL<1, 'Negative', ifelse(asymp.LCL>1, 'Positive','Neutral'))) |> 
  ggplot(aes(x=odds.ratio, y=Genotype2))+ # Show significant differences between genotypes for day
  geom_hline(yintercept=1)+
  geom_pointrange(aes(xmin=asymp.UCL,xmax=asymp.LCL, colour=col))+
  facet_wrap(Genotype1~., scales='free_y', nrow=3)+
  scale_x_continuous(trans = scales::log2_trans())+
  scale_color_manual('',breaks=c('Negative','Neutral','Positive'),values=c("#F66060","black","#87C43D"))+
  theme(axis.title = element_text(family="Nunito"),
        axis.text.y = element_text(family="Imprima"),
        axis.text.x = element_text(family="Product Sans", angle=90, vjust=0.5, hjust=1),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Nunito"),
        legend.text = element_text(family="Product Sans"), 
        panel.grid.minor=element_blank()
  )

# Vizualisation of Fv/Fm decrease with temperature
ggplot(Acute, aes(x=Temperature, y=FvFm, color=Temperature))+
  geom_smooth(aes(color=..x..), linewidth=2)+
  geom_point(size=2)+
  scale_x_continuous(expression(Temperature~("°C")), limits=c(28.5,39.5), breaks=c(29,33,36,39))+
  scale_y_continuous(expression(Photosynthetic~efficiency~(Fv/Fm)), limits=c(0.15,0.7), breaks=c(0.2,0.4,0.6),expand = c(0.005, 0.005))+
  scale_colour_gradient(low="#30D4F1", high="#FF5353")+
  theme_grey(base_size=18)+
  theme(axis.title = element_text(family="Nunito"),
        axis.text = element_text(family="Imprima"),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Nunito"),
        legend.text = element_text(family="Product Sans"), 
        panel.grid.minor=element_blank(),
        legend.position = 'none')

#-----# MHSE ANALYSIS #--------#

    ## Statistics ##

# Call file with Fv/Fm
Moderate <- subset(MHSE, MHSE$PAR =="0") # Don't consider columns without actual IPAM measurements
Moderate <- Moderate |> mutate(fDays=factor(Days))
Moderate <- Moderate |> mutate(fTemp=factor(Temperature))

# Try different models:
a <- glmmTMB(FvFm ~ fDays*Genotype*fTemp + (1|Tank), dispformula = ~fDays, data = Moderate, family="beta_family",REML=TRUE)
b <- glmmTMB(FvFm ~ Genotype*fTemp + (1|Tank), dispformula = ~fDays, data = Moderate, family="beta_family",REML=TRUE)
c <- glmmTMB(FvFm ~ fDays*fTemp + (1|Tank), dispformula = ~fDays, data = Moderate, family="beta_family",REML=TRUE)
d <- glmmTMB(FvFm ~ fDays*Genotype + (1|Tank), dispformula = ~fDays, data = Moderate, family="beta_family",REML=TRUE)
e <- glmmTMB(FvFm ~ fDays + (1|Tank), dispformula = ~fDays, data = Moderate, family="beta_family",REML=TRUE)
f <- glmmTMB(FvFm ~ Genotype + (1|Tank), dispformula = ~fDays, data = Moderate, family="beta_family",REML=TRUE)
g <- glmmTMB(FvFm ~ fTemp + (1|Tank), dispformula = ~fDays, data = Moderate, family="beta_family",REML=TRUE)
h <- glmmTMB(FvFm ~ fDays+Genotype+fTemp + (1|Tank), dispformula = ~fDays, data = Moderate, family="beta_family",REML=TRUE)
i <- glmmTMB(FvFm ~ Genotype+fTemp + (1|Tank), dispformula = ~fDays, data = Moderate, family="beta_family",REML=TRUE)
j <- glmmTMB(FvFm ~ fDays+fTemp + (1|Tank), dispformula = ~fDays, data = Moderate, family="beta_family",REML=TRUE)
k <- glmmTMB(FvFm ~ fDays+Genotype + (1|Tank), dispformula = ~fDays, data = Moderate, family="beta_family",REML=TRUE)
AIC(a, b, c, d, e, f, g, h, i,j, k) # Compare models

# Check time & temperature effect :
M_mod1 <- glmmTMB(FvFm ~ fDays*fTemp + (1|Tank)+(1|Reef), 
                dispformula = ~fDays, # What gives the main variation
                data = Moderate,
                family="beta_family", # This distribution is for "limited" values, (having a min and a max)
                REML=TRUE) 
resid <- simulateResiduals(M_mod1, plot=TRUE)
summary(M_mod1)
Anova(M_mod1)

M_mod1.em <- emmeans(M_mod1, ~fTemp|fDays, type="response") |> 
  pairs() |> 
  summary(infer=TRUE) 
M_mod1.em # Pairwise comparison

# Vizualisation of pairwise comparison
M_mod1.em |> as.data.frame() |> 
  filter(!is.na(odds.ratio)) |> # only without na
  droplevels() |> 
  mutate(col=ifelse(asymp.UCL<1, 'Negative', ifelse(asymp.LCL>1, 'Positive','Neutral'))) |> 
  ggplot(aes(x=odds.ratio, y=contrast))+ 
  geom_vline(xintercept=1)+
  geom_pointrange(aes(xmin=asymp.UCL,xmax=asymp.LCL, colour=col))+
  facet_grid(fDays~., space='free', scales='free_y')+
  scale_x_continuous(trans = scales::log2_trans())+
  scale_color_manual('',breaks=c('Negative','Neutral','Positive'),values=c("#F66060","black","#A3E059"))+
  theme(axis.title = element_text(family="Work Sans Medium"),
        axis.text = element_text(family="Product Sans"),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Helvetica Neue eText Pro Medium"),
        legend.text = element_text(family="Helvetica Neue eText Pro Medium"), 
        panel.grid.minor=element_blank()
  )

# Effect of Temperature, Time and Genet on Fv/Fm
M_mod2 <- glmmTMB(FvFm ~ fDays*Genotype*fTemp + (1|Tank), 
                dispformula = ~fDays, # What gives the main variation
                data = Moderate, # Only consider treatment
                family="beta_family", # This distribution is for "limited" values, (having a min and a max)
                REML=TRUE) #Restricted (or Residual) Maximum Likelihood, more accurate estimates of the random effects variances
resid <- simulateResiduals(M_mod2, plot=TRUE) # Look at the residuals
  # Highly sensitive test
  # Looking at how dots fit normality
  # Lines should be parallel to one another
  # Adjust the model and get as close as possible to assumptions
summary(M_mod2)
Anova(M_mod2)

M_mod2.em <- emmeans(M_mod2, ~Genotype|fDays|fTemp, type="response") |> 
  pairs() |> 
  summary(infer=TRUE) 
M_mod2.em # odds.ratio tell the type of variation around, p-value tells confidence in the given variation
        # Variation cannot be assessed confidently if possible value =1

       ## Visualisation ##

# Visualize pairwise comparison at day 20
M_mod2.em |> as.data.frame() |> 
  filter(fDays=='20') |> # Look at day 20 only
  tidyr::separate(contrast,into=c('Genotype1','Genotype2'), sep='/') |> 
  filter(!is.na(odds.ratio)) |> # only wthout na
  droplevels() |> 
  mutate(col=ifelse(asymp.UCL<1, 'Negative', ifelse(asymp.LCL>1, 'Positive','Neutral'))) |> 
  ggplot(aes(x=odds.ratio, y=Genotype2))+ # Show significant differences between genotypes for day
  geom_vline(xintercept=1)+
  geom_pointrange(aes(xmin=asymp.UCL,xmax=asymp.LCL, colour=col))+
  facet_grid(Genotype1~., space='free', scales='free_y')+
  scale_x_continuous(trans = scales::log2_trans())+
  scale_color_manual('',breaks=c('Negative','Neutral','Positive'),values=c("#F66060","black","#87C43D"))+
  theme(axis.title = element_text(family="Source Sans Pro"),
        axis.text = element_text(family="Imprima"),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Source Sans Pro"),
        legend.text = element_text(family="Product Sans"), 
        panel.grid.minor=element_blank()
  )

# Visualize pairwise comparison at day 14
M_mod2.em |> as.data.frame() |> 
  filter(fDays=='14') |> filter(fTemp!='28') |> # Look at day 14 only
  tidyr::separate(contrast,into=c('Genotype1','Genotype2'), sep='/') |> 
  filter(!is.na(odds.ratio)) |> # only wthout na
  droplevels() |> 
  mutate(col=ifelse(asymp.UCL<1, 'Negative', ifelse(asymp.LCL>1, 'Positive','Neutral'))) |> 
  ggplot(aes(x=odds.ratio, y=Genotype2))+ # Show significant differences between genotypes for day
  geom_vline(xintercept=1)+
  geom_pointrange(aes(xmin=asymp.UCL,xmax=asymp.LCL, colour=col))+
  facet_wrap(Genotype1~., scale="free_y", ncol=6, strip.position = "top")+
  scale_x_continuous(trans = scales::log2_trans())+
  scale_color_manual('',breaks=c('Negative','Neutral','Positive'),values=c("#F66060","black","#87C43D"))+
  theme(axis.title = element_text(family="Source Sans Pro"),
        axis.text = element_text(family="Imprima"),
        axis.text.y = element_text(family="Product Sans", size=6),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Source Sans Pro"),
        legend.text = element_text(family="Product Sans"), 
        panel.grid.minor=element_blank()
  )

# Vizualisation of Fv/Fm variation with temperature
ggplot(Moderate, aes(x=Days, y=FvFm, color=Temperature))+
  geom_smooth(linewidth=1)+
  geom_point(size=2)+
  scale_color_manual(values = c("#30D4F1", "#FF5353"))+
  scale_x_continuous(expression(Time~(Days)), limits=c(0,21), breaks=c(1,14,20),expand = c(0.005, 0.005))+
  scale_y_continuous(expression(Photosynthetic~efficiency~(Fv/Fm)), limits=c(0,0.7), breaks=c(0,0.2,0.4,0.6),expand = c(0.005, 0.005))+
  theme_grey(base_size = 18)+
  theme(axis.title = element_text(family="Nunito"),
        axis.text = element_text(family="Imprima"),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Nunito", size=16),
        legend.text = element_text(family="Product Sans", size=13), 
        panel.grid.minor=element_blank()
  )

#------------------------#
### Survival analysis ####
#------------------------#

# Required packages:
library(survival)
library(coxme)
library(survminer)
library(ggquickeda)

# Call data
survivorship <-read_excel("D:/AIMS/Master Thesis/Data/Moderate/Survivorship/SurvivorshipModerateFinal.xlsx")
survivorship$Tank <- as.character(survivorship$Tank)
survivorship$TotalDayAlive <- as.numeric(survivorship$TotalDayAlive)
survivorship$Temperature <- as.factor(survivorship$Temperature)

#--------### ANALYSIS ###---------#

died1=Surv(survivorship$TotalDayAlive, survivorship$Dead);died # Survorship variable
survivorship$Temperature <- as.character(survivorship$Temperature)

        ## Modelisation ##

# Identify a model that accounts for full effects and tests for potential interaction :
survivorship <- filter(survivorship, survivorship$Genotype!="14" & survivorship$Genotype!="329" & survivorship$Genotype!="271qm" )
    # Remove genotype with less than 2 replicate
died=Surv(survivorship$TotalDayAlive, survivorship$Dead);died # Survorship variable # Use this new variable for model only

control<-coxme.control(iter.max=300) # Maximum times the model tries to adjust 

m1<-coxme(died~Temperature*Genotype+(1|Tank)+(1|Reef), data=survivorship, control=control)
LogLik1 <- m1$loglik[1]*m1$loglik[3] # NULL*Penalized
summary(m1)
test.m1 <- cox.zph(m1) ; test.m1 # cox.zph() works for both coxme and coxph
Anova(m1)

m2<-coxme(died~Temperature+Genotype+(1|Tank)+(1|Reef), data=survivorship)
LogLik2 <- m2$loglik[1]*m2$loglik[3] # = 227536.3
test.m2 <- cox.zph(m2) ; test.m2 # cox.zph() works for both coxme and coxph
m2
m3<-coxme(died~Temperature+(1|Tank)+(1|Reef), data=survivorship)
LogLik3 <- m3$loglik[1]*m3$loglik[3] # = 268680.2 
test.m3 <- cox.zph(m3); test.m3
m3
m4<-coxme(died~Genotype+(1|Tank)+(1|Reef), data=survivorship)
LogLik4 <- m4$loglik[1]*m4$loglik[3] # 226353.6
test.m4 <- cox.zph(m4); test.m4
m4

anova(m1,m2,m3,m4) # Compare models using anova or looking at smallest LogLik value

      ## Survivorship and ranking assessement ##

# Select 31.5°C and establish survfit
subset_treatment <- survivorship[survivorship$Temperature == "31.5", ]
survival_curve_Treatment<- survfit(Surv(subset_treatment$TotalDayAlive, 
                                        subset_treatment$Dead)~Genotype,
                                        data=subset_treatment)

# Ranking

SurvRank <- summary(survival_curve_Treatment)$table[,5:6]
      # 5th column = mean and 6th column = standard error of
SurvRank <- as.table(SurvRank) # 1st step. Transform data into data frame.
SurvRank <- as.data.frame(SurvRank) # 2nd step. Transform data into data frame.
SurvRank <- pivot_wider(SurvRank, names_from = Var2, values_from = Freq) # Pivot table
SurvRank$Genotype <- SurvRank$Var1 ; SurvRank <- subset(SurvRank, select = -Var1)
SurvRank$Survival <- SurvRank$rmean ; SurvRank <- subset(SurvRank, select = -rmean)
SurvRank$Genotype <- gsub("^Genotype=", "", SurvRank$Genotype) # Adjust names
SurvRank$SRank <- rank(-SurvRank$Survival) # Rank from higher to lower
print(SurvRank, n=31) # Show values, standard error and ranks

#--------### VIZUALISATION ###---------#

# Treatment comparison
ggplot(survivorship, aes(time = TotalDayAlive, status = Dead, color=Temperature, group=Temperature)) +
  geom_km(linewidth=1.5)+
  ylab("Photosynthetic efficiency (Fv/Fm)") +
  xlab("Time (Days)")+
  scale_color_manual(values = c("#30D4F1", "#FF5353"))+
  theme_grey(base_size = 17)+
  theme(axis.title = element_text(family="Nunito"),
        axis.text = element_text(family="Imprima"),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Product Sans"),
        legend.text = element_text(family="Product Sans"), 
        panel.grid.minor=element_blank()
  )

# Arrange data
SurvRank <- SurvRank %>%
  arrange(-Survival) # Arrange according to ranks
Geno_order <- as.factor(SurvRank$Genotype) # Create a factor of genotype ordered in desired ranking
survivorship$Genotype <- factor(survivorship$Genotype, levels = Geno_order) # Reorder data according to this factor

# Survivorship per genotype according to time
ggplot(survivorship, aes(time = TotalDayAlive, status = Dead, color=Temperature, group=Temperature)) +
  geom_km()+
  facet_wrap(~ Genotype, ncol=5)+
  ylab("Photosynthetic efficiency (Fv/Fm)") +
  xlab("Time (Days)")+
  theme_grey(base_size = 15)+
  scale_color_manual(values = c("#30D4F1", "#FF5353"))+
  theme(axis.title = element_text(family="Nunito"),
        axis.text = element_text(family="Imprima"),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Product Sans"),
        legend.text = element_text(family="Product Sans"), 
        panel.grid.minor=element_blank()
        )

#------------------------------#
### Delta Fv/Fm of the MHSE ####
#------------------------------#

# Call data
deltaFvFm <- subset(MHSE, MHSE$PAR =="0") # Don't consider columns without IPAM measurements

  #--------### ANALYSIS ###---------#

deltaFvFm_treat <- subset(deltaFvFm, deltaFvFm$Temperature =="31.5") # Only consider treatment

        ## Calculate deltas of Fv/Fm ##

# Get one data frame per day
T0 <- subset(deltaFvFm, deltaFvFm$MeasureDate =="20240320"); T1 <- subset(deltaFvFm, deltaFvFm$MeasureDate =="20240402"); T2 <- subset(deltaFvFm, deltaFvFm$MeasureDate =="20240408")

# Merge data sets according to position and calculate delta for T0-T1, T0-T2 and T1-T2. 
T0T1 <- left_join(T0, T1[,c("Position","FvFm")], by = "Position") 
T0T1$DeltaFvFm = T0T1$FvFm.y-T0T1$FvFm.x # Calculate Deltas
T0T1$Delta = "T0 to T1" # Add columns with type of delta.
T0T2 <- left_join(T0, T2[,c("Position","FvFm")], by = "Position")
T0T2$DeltaFvFm = T0T2$FvFm.y-T0T2$FvFm.x # Need to just keep 1, 14, 1841, 279, 298, 314, 328, 350, 355, 8, 3qm2tagea
T0T2$Delta= "T0 to T2"
T1T2 <- left_join(T1, T2[,c("Position","FvFm")], by = "Position")
T1T2$DeltaFvFm = T1T2$FvFm.y-T1T2$FvFm.x
T1T2$Delta = "T1 to T2"
Deltas <- rbind(T0T1, T1T2, T0T2) # Make one big data frame.

        ## Delta Fv/Fm Ranking ##

dT0T1 <- T0T1 |> 
  group_by(Genotype) |>  
  filter(!is.na(DeltaFvFm)) |> # exclude NAs
  summarize(Delta=mean(DeltaFvFm), # Mean of slopes, deltas for each genotypes
            Delta_SE = sd(DeltaFvFm, na.rm = TRUE) / sqrt(sum(!is.na(DeltaFvFm)))) 
dT0T1 <- as.data.frame(dT0T1) # Turn results into data frame to be able to rank
dT0T1$DRank <- rank(-dT0T1$Delta)
print(dT0T1) # Shows values, standard error and ranks

 #--------### VIZUALISATION ###---------#    

# Arrange data
dT0T1 <- dT0T1 %>%
  arrange(-Delta) # Arrange rank data frame according to ranks
Geno_order <- as.factor(dT0T1$Genotype) # Create a factor of genotype ordered in desired ranking
Deltas$Genotype <- factor(Deltas$Genotype, levels = Geno_order) # Reorder data according to this factor
deltaFvFm$Genotype <- factor(deltaFvFm$Genotype, levels = Geno_order) # Reorder data according to this factor
deltaFvFm_treat$Genotype <- factor(deltaFvFm_treat$Genotype, levels = Geno_order) # Reorder data according to this factor

        ## Box plots ##

ggplot(Deltas, aes(x=Genotype, y=DeltaFvFm, fill=Genotype))+
  geom_boxplot()+
  facet_wrap(~Delta, nrow=3)+
  ylab("Delta of photosynthetic efficiency (Fv/Fm)") +
  xlab("Genet")+
  theme(axis.title = element_text(family="Nunito"),
        axis.text = element_text(family="Imprima"),
        axis.text.x=element_text(family="Product Sans", angle = 55, hjust = 1),
        strip.text = element_text(family="Product Sans"),
        legend.position="none", 
        panel.grid.minor=element_blank()
  ) # Have to remove either x axis or genotype legend

       ## Slopes per Genet ##

# Fv/Fv according to temperature per genotype with adjusted linear model
ggplot(deltaFvFm, aes(x=Days, y=FvFm, fill=Temperature, color=Temperature))+
  geom_smooth(method=lm, se=TRUE)+ # adjusts the model linearly
  facet_wrap(~Genotype, ncol=6)+
  geom_point()+
  scale_color_manual(values = c("#30D4F1", "#FF5353")) +
  scale_fill_manual(values = c("#30D4F1", "#FF5353"))+
  scale_x_continuous(expression(Time~(Days)), limits=c(0,21), breaks=c(1,14,20),expand = c(0.005, 0.005))+
  scale_y_continuous(expression(Photosynthetic~efficiency~(Fv/Fm)), limits=c(0.1,0.7), breaks=c(0,0.2,0.4,0.6),expand = c(0.005, 0.005))+
  theme_grey(base_size = 15)+
  theme(axis.title = element_text(family="Nunito"),
        axis.text = element_text(family="Imprima"),
        strip.text = element_text(family="Product Sans", size=11),
        legend.title = element_text(family="Nunito"),
        legend.text = element_text(family="Product Sans"), 
        panel.grid.minor=element_blank()
  )

# Fv/Fm through days per genotype for each position
ggplot(deltaFvFm_treat, aes(x=Days, y=FvFm, color=Genotype))+
  geom_smooth(method=lm, se=TRUE)+
  facet_wrap(~Genotype, ncol=6)+
  geom_point()+
  scale_x_continuous(expression(Time~(Days)), limits=c(0,21), breaks=c(1,14,20),expand = c(0.005, 0.005))+
  scale_y_continuous(expression(Photosynthetic~efficiency~(Fv/Fm)), limits=c(0.1,0.7), breaks=c(0,0.2,0.4,0.6),expand = c(0.005, 0.005))+
  theme_grey(base_size = 16)+
  theme(axis.title = element_text(family="Nunito"),
        axis.text = element_text(family="Imprima"),
        strip.text = element_text(family="Product Sans", size=11),
        legend.title = element_text(family="Nunito"),
        legend.text = element_text(family="Product Sans"), 
        panel.grid.minor=element_blank(),
        legend.position = "none"
  )

#-----------------------------#
### Fv/Fm ED50 of the AHSE ####
#-----------------------------#

# Required packages:
library(drc)

# Call data
AHSE <- subset(AHSE, AHSE$PAR =="0") # Don't consider columns without IPAM measurements

      #----------# ANALYSIS #------------#

            ## Modelisation ##

# drm() creates a model, mselect(,icfct = AIC) calculate AIC of models for each fonctions 
EDAcute <- drm(FvFm ~ Temperature, data=AHSE, curveid=Genotype, fct=LL.3());EDAcute 
mselect(EDAcute, list(LL.3(), LL.2(), LL.4(), LL.5(), LL2.2(), LL2.3(), LL2.4(), LL2.5(), AR.2(), AR.3(), EXD.2(), EXD.3()), icfct = AIC)
        # Best model will have the smallest AIC
summary(EDAcute)

            ## Effective-Dose at 50% ranking ##

ED50Acute <- data.frame(ED(EDAcute, 50));ED50Acute # Put ED50 into data frame
ED50Acute_rownames <- gsub("^e:([^:]+):[0-9]+$", "\\1", rownames(ED50Acute))
rownames(ED50Acute) <- ED50Acute_rownames
ED50Acute$Genotype <- ED50Acute_rownames;rownames(ED50Acute)=NULL # Assign corresponding genotype to row names
ED50Acute$ED50 <- ED50Acute$Estimate ; ED50Acute <- subset(ED50Acute, select = -Estimate) # Rename Estimate column into ED50
ED50Acute$ERank= rank(-ED50Acute$ED50) # Add Rank column
ED50Acute # Shows estimated ED50, Standard error and ranking

        #----------# VIZUALISATION #------------#

# Arrange Data
ED50Acute <- ED50Acute %>%
  arrange(ERank) # Arrange rank data frame according to ranks
Geno_order <- as.factor(ED50Acute$Genotype) # Create a factor of genotype ordered in desired ranking
AHSE$Genotype <- factor(AHSE$Genotype, levels = Geno_order) # Reorder data according to this factor
ED50Acute$Genotype <- factor(ED50Acute$Genotype, levels = Geno_order) # Have to reorder this one because facet wrap is using it

# Merged Fv/Fm DRC and ED50s
ggplot(AHSE, aes(y=FvFm, x=Temperature, color=Genotype, fill=Genotype))+ 
  geom_smooth(method=drm, #tells type of model
              method.args=list(fct = LL.3()), se=F)+ # tells how to smooth and what function to use
  geom_vline(data=ED50Acute, aes(xintercept=ED50, color=Genotype), linetype=1, linewidth=0.5)+ # Shows ED50s
  geom_point(alpha=0.3,position=position_jitterdodge(0.05))+ # Shows Fv/Fm values as points
  scale_x_continuous(expression(Temperature~(degree~C)), limits=c(29,40),breaks=c(29.5, 33.5, 36.5, 39.5),expand = c(0.005, 0.005))+
  scale_y_continuous(expression(Photosynthetic~efficiency~(Fv/Fm)), limits=c(0,0.7), breaks=c(0,0.2,0.4,0.6),expand = c(0.005, 0.005))+
  theme_grey(base_size = 17)+
  labs(color="Genets", fill="Genets")+
  theme(axis.title = element_text(family="Nunito", size=14),
        axis.text = element_text(family="Imprima"),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Nunito", size=12),
        legend.text = element_text(family="Product Sans", size=11), 
        panel.grid.minor=element_blank()
  )

# Fv/Fm DRC and ED50s per Genet
ggplot(AHSE, aes(y=FvFm, x=Temperature, color=Genotype, fill=Genotype))+ 
  geom_smooth(method=drm, method.args=list(fct = LL.3()), se=F)+
  geom_vline(data=ED50Acute, aes(xintercept=ED50, color=Genotype), linetype=2, linewidth=0.75)+
  geom_point(alpha=0.3,position=position_jitterdodge(0.05))+
  scale_x_continuous(expression(Temperature~(degree~C)), limits=c(29,40),breaks=c(29, 33, 36, 39),expand = c(0.005, 0.005))+
  scale_y_continuous(expression(Photosynthetic~efficiency~(Fv/Fm)), limits=c(0,0.7), breaks=c(0,0.2,0.4,0.6),expand = c(0.005, 0.005)) +
  facet_wrap(~ Genotype, nrow=6)+
  theme_grey(base_size = 16)+
  theme(axis.title = element_text(family="Nunito", size=14),
        axis.text = element_text(family="Imprima"),
        strip.text = element_text(family="Product Sans", size=10),
        panel.grid.minor=element_blank(),
        legend.position="none"
  )

#------------------------------#
### Rankings comparison ####
#------------------------------#

Required packages:
library(gridExtra)

        #---------# ANALYSIS #-----------#

        ## Clean and process data ##

# Remove genets that can't be compared
SurvRank <- subset(SurvRank,Genotype!="1840" & Genotype!="292" & Genotype!="355" 
                   & Genotype!="270" & Genotype!="uk2" & Genotype!="329")
ED50Acute <- filter(ED50Acute, Genotype!="5" & Genotype!="301" & Genotype!="305" &
                    Genotype!="320" & Genotype!="335" & Genotype!="uk2" & Genotype!="329" )
dT0T1 <- subset(dT0T1, Genotype!="1840" & Genotype!="292" & Genotype!="355" 
                    & Genotype!="270" & Genotype!="5")

# Ranks that consider values distances #

#Order data
SurvRank <- SurvRank[order(-SurvRank$Survival),]
SurvRank$SRank <- rank(-SurvRank$Survival)
# Calculate deltas for Survival
SurvRank$deltas <- c(0, -diff(SurvRank$Survival)) # Difference between each adjacent value
a <- max(SurvRank$Survival) - min(SurvRank$Survival) # Max range
n <- nrow(SurvRank)-1 # number of observation - 1
# Make true ranks for survival
SurvRank$deltas <- SurvRank$deltas * n / a # Relative difference between supposed ranks
SurvRank$S_Rank <- cumsum(SurvRank$deltas)+1

# Order data
ED50Acute <- ED50Acute[order(-ED50Acute$ED50),] 
ED50Acute$ERank <- rank(-ED50Acute$ED50)
# Calculate deltas for Survival
ED50Acute$deltas <- c(0, -diff(ED50Acute$ED50))
a <- max(ED50Acute$ED50) - min(ED50Acute$ED50)
n <- nrow(ED50Acute)-1
# Make true ranks for ED50
ED50Acute$deltas <- ED50Acute$deltas * n / a
ED50Acute$E_Rank <- cumsum(ED50Acute$deltas)+1

# Order data
dT0T1$Delt <- -dT0T1$Delta 
dT0T1$DRank <- rank(-dT0T1$Delta)
dT0T1 <- dT0T1[order(-dT0T1$Delta),] # Ranger dans l'ordre
# Calculate deltas for Survival
dT0T1$deltas <- c(0, -diff(dT0T1$Delta))
a <- max(dT0T1$Delta) - min(dT0T1$Delta)
n <- nrow(dT0T1)-1
# Make true ranks for Delta
dT0T1$deltas <- dT0T1$deltas * n / a
dT0T1$D_Rank <- cumsum(dT0T1$deltas)+1

# Merge ranks
Ranking <- full_join(SurvRank, ED50Acute, by = "Genotype"); Ranking <- full_join(Ranking, dT0T1, by = "Genotype")

    ## Statistical Test ##

# Spearman correlation tests, pairwise
cor.test(rank(-(Ranking$Survival)),rank(-(Ranking$ED50)), method='spearman')
cor.test(rank(-(Ranking$Survival)),rank(Ranking$Delta),method='spearman')
cor.test(rank(Ranking$Delta),rank(-Ranking$ED50), method='spearman')

        #---------# VIZUALISATION #-----------#

        ## Correlation plots ##

One <- ggplot(Ranking, aes(x=E_Rank,y=D_Rank, color=Genotype)) + 
  geom_point(size=3)+
  geom_smooth(se=TRUE)+
  xlab("Fv/Fm ED50 of the acute")+
  ylab(expression(Delta*"Fv/Fm of the moderate"))+
  theme_grey(base_size=15)+
  theme(legend.position = 'none',
        axis.title = element_text(family="Product Sans", size=12),
        axis.text = element_text(family="Imprima"),
        legend.title = element_text(family="Product Sans"),
        legend.text = element_text(family="Product Sans"),
        panel.grid = element_blank())

Two <- ggplot(Ranking, aes(x=E_Rank,y=S_Rank, color=Genotype)) + 
  geom_point(size=3)+
  geom_smooth(se=TRUE)+
  xlab("Fv/Fm ED50 of the acute")+
  ylab("Survival trait of the moderate")+
  theme_grey(base_size=15)+
  theme(legend.position = 'none',
        axis.title = element_text(family="Product Sans", size=12),
        axis.text = element_text(family="Imprima"),
        legend.title = element_text(family="Product Sans"),
        legend.text = element_text(family="Product Sans"),
        panel.grid = element_blank())

Three <- ggplot(Ranking, aes(x=S_Rank,y=D_Rank, color=Genotype)) + 
  geom_point(size=3)+
  xlab("Survival trait of the moderate")+
  ylab(expression(Delta*"Fv/Fm of the moderate"))+ 
  labs(color="Genet")+
  theme_grey(base_size=15)+
  theme(axis.title = element_text(family="Product Sans", size=13),
        axis.text = element_text(family="Imprima"),
        legend.title = element_text(family="Product Sans", size=13),
        legend.text = element_text(family="Product Sans"),
        panel.grid = element_blank())

grid.arrange(arrangeGrob(One,Two, ncol=2), Three, ncol = 1) 

      ## Heatmap ##

# Make a total table for ranking and call data
SurvRank$Rank=SurvRank$SRank;ED50Acute$Rank=ED50Acute$ERank;dT0T1$Rank=dT0T1$DRank
SurvRank$Metric="Survivorship";ED50Acute$Metric="ED50 of acute";dT0T1$Metric="Fv/FM decrease in moderate"
Sranking <- SurvRank[, c("Genotype", "Rank", "Metric")];EDranking <- ED50Acute[, c("Genotype", "Rank", "Metric")];Sloperanking <- dT0T1[, c("Genotype", "Rank", "Metric")]
Rankings_merged <- rbind(Sranking, EDranking, Sloperanking)

# Choose a color gradient for heatmap
color_gradient <- colorRampPalette(c("#06d6a0", "#fee440","#ef476f"))

# Ranking according to the Fv/Fm ED50 of the acute

Heat1 <- Rankings_merged %>%
  mutate(Genotype = fct_relevel(Genotype, 
                          "8","1","ukchicken","3qm2tageat",
                          "302","298","293", "333","279","275","14",
                          "1841","314", "350","271qm",
                          "1842","328","272","318","360","7","239",
                          "11","317")) %>% # Genotype ordering according to Acute ED50
  mutate(Metric = fct_relevel(Metric, "ED50 of acute", "Fv/FM decrease in moderate", "Survivorship")) %>%
  ggplot(aes(x = Metric, y = Genotype, fill = Rank)) +
  geom_tile() +
  scale_fill_gradientn(colors=color_gradient(100)) + # Green is top of rank
  theme(axis.title = element_text(family="Source Sans Pro"),
        axis.text.x = element_text(family="Product Sans"),
        axis.text.y = element_text(family="Product Sans"),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Source Sans Pro"),
        legend.text = element_text(family="Imprima"), 
        panel.grid=element_blank(), panel.background = element_blank())
Heat1

# Ranking according to survival :
custom_labels <- c("Fv/Fm ED50 \n of acute test", "Survival\nof moderate test","Delta Fv/Fm of\nmoderate test")
Heat2 <- Rankings_merged %>%
  mutate(Genotype = fct_relevel(Genotype,"271qm","14","298","3qm2tageat","1841","350",
                                "279","1","314","328","360","7","318","333",
                                "1842","293","302","272","ukchicken","239","317","8",
                                "11","275")) %>% # Genotype ordering according to Acute ED50
  mutate(Metric = fct_relevel(Metric, "ED50 of acute","Survivorship","Fv/FM decrease in moderate" )) %>%
  ggplot(aes(x = Metric, y = Genotype, fill = Rank)) +
  geom_tile() +
  ylab("Genet")+
  xlab("")+
  scale_fill_gradientn(colors=color_gradient(100))+ # Green is top of rank
  scale_x_discrete(labels = custom_labels)+ 
  theme_grey(base_size=13)+
  theme(axis.title = element_text(family="Source Sans Pro"),
        axis.text.x = element_text(family="Product Sans", vjust=-2),
        axis.text.y = element_text(family="Product Sans"),
        strip.text = element_text(family="Product Sans"),
        legend.title = element_text(family="Source Sans Pro"),
        legend.text = element_text(family="Imprima"), 
        panel.grid=element_blank(), panel.background = element_blank()
  )
Heat2

ggarrange(Heat1, Heat2, ncol = 2,
          labels = c("A","B"))

#------------------------#
### Models analysis ####
#------------------------#
