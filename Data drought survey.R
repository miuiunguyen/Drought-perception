###########################################################
############# IMPORT DATA AND PACKAGE USED #################
###########################################################
rm(list=ls())
setwd("C:/Users/Ms Mui/OneDrive - Victoria University of Wellington - STAFF/Data")
getwd()
Drought <- read_excel("survey_export-v2-.xlsx")
library(haven)
library(dplyr)
library(tidyr)
library(tibble)
library(XLConnect)
library(XLConnectJars)
library(lme4)
library(car)
library(rJava)
library(xlsx)
library(readxl)
library(foreign)
library(MASS)
library(nnet)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(fmsb)
library(gcookbook)

##################################################
############# RECODE VARIABLES ##################
################################################
Drought$Province <- factor(Drought$q22_region)
rought$Cc_impact_to_drought <- Drought$q113_drought
Drought$Gender <- Drought$q152_own
Drought$Age <-Drought$q153_age_own
Drought$On_farm_Experience <- Drought$q154_own
Drought$Generation <- Drought$q155_own
Drought$Ethnic_European <- Drought$q156_nz_european_own
Drought$Education <- Drought$q157
Drought$Qualification_farming <- Drought$q158_farming_own
Drought$Qualification_business <- Drought$q158_business_own
Drought$Prod_decline_drought_2013 <- Drought$q119_2013
Drought$Prod_decline_drought_2014 <- Drought$q119_2014
Drought$Prod_decline_drought_2015 <- Drought$q119_2015
Drought$Prod_decline_drought_2016 <- Drought$q119_2016
Drought$Prod_decline_drought_2017 <- Drought$q119_2017
Drought$Prod_decline_drought_2018 <- Drought$q119_2018
## Recoding drought experience variables
Drought <- Drought %>% 
 mutate(Drought_Experience_2018 = case_when(
  Drought_2018 == 0 ~ 0,
  Drought_2018 >0 ~ 1)) %>%  
 mutate(Drought_Experience_2017 = case_when(
  Drought_2017 == 0 ~ 0,
  Drought_2017 >0 ~ 1)) %>% 
 mutate(Drought_Experience_2016 = case_when(
  Drought_2016 == 0 ~ 0,
  Drought_2016 >0 ~ 1)) %>% 
 mutate(Drought_Experience_2015 = case_when(
  Drought_2015 == 0 ~ 0,
  Drought_2015 >0 ~ 1)) %>% 
 mutate(Drought_Experience_2014 = case_when(
  Drought_2014 == 0 ~ 0,
  Drought_2014 >0 ~ 1)) %>% 
 mutate(Drought_Experience_2013 = case_when(
  Drought_2013 == 0 ~ 0,
  Drought_2013 >0 ~ 1)) %>%
 mutate(Drought_Experience_2012 = case_when(
  Drought_2012 == 0 ~ 0,
  Drought_2012 >0 ~ 1)) %>% 
 mutate(Drought_Experience_2011 = case_when(
  Drought_2011 == 0 ~ 0,
  Drought_2011 >0 ~ 1)) %>% 
 mutate(Drought_Experience_2010 = case_when(
  Drought_2010 == 0 ~ 0,
  Drought_2010 >0 ~ 1)) %>%
 mutate(Drought_Experience_2009 = case_when(
  Drought_2009 == 0 ~ 0,
  Drought_2009 >0 ~ 1)) %>% 
 mutate(Drought_Experience_2008 = case_when(
  Drought_2008 == 0 ~ 0,
  Drought_2008 >0 ~ 1)) %>% 
 mutate(Drought_Experience_2007 = case_when(
  Drought_2007 == 0 ~ 0,
  Drought_2007 >0 ~ 1))
### Recoding Age and On farm experience
Drought <- Drought %>% 
 mutate(Age_factor = case_when(
  Age < 40 ~ "<40 years old",
  Age <= 50 ~ "40-50 years old",
  Age <= 60 ~ "51-60 years old",
  Age <= 70 ~ "61-70 years old",
  Age > 70 ~ "70+")) %>% 
 mutate(On_farm_Experience_factor = case_when(
  On_farm_Experience <= 5 ~ "0-5 years",
  On_farm_Experience <= 10 ~ "6-10 years",
  On_farm_Experience <= 15 ~ "11-15 years",
  On_farm_Experience <= 20 ~ "16-20 years",
  On_farm_Experience > 20 ~ "Over 20 years")) %>% 
 mutate(Education = case_when(
    Education == "Some secondary school" ~ 0,
    Education == "Secondary school" ~ 0,
    Education == "Certificate (level 1-6)" ~ 0,
    Education == "Prefer not to answer" ~ 0,
    Education == "Other" ~ 0,
    Education == "Master's degree" ~ 1,
    Education == "Post-graduate diploma or certificate / Bachelor's honour's degree" ~ 1,
    Education == "Diploma (level 5-7)" ~ 1,
    Education == "Doctoral degreer" ~ 1,
    Education == "Bachelor's degree" ~ 1))

###########################################################
############### PART 1: PLOT FIGURES ######################
###########################################################

## PLOT PERCEPTION OF FREQUENCY AND INTENSITY BAR CHART

a <- Drought %>%dplyr::select(q114_drought_freq, Province)
aa <- na.omit(a)
data_frequency <- aa %>%
 group_by(q114_drought_freq) %>%
 summarise(total=n(),.groups = 'drop') %>% 
 mutate(percent = total/sum(total))

data1 <- data_frequency %>% 
 ggplot(aes(q114_drought_freq, percent, fill = as.factor(q114_drought_freq),width=0.75)) +
 scale_fill_brewer(palette = "YlOrBr",labels=c("Highly decrease",
                                               "Slightly decrease",
                                               "No change",
                                               "Slightly increase",
                                               "Highly increase"))+
 theme(legend.title = element_blank()) +
 scale_y_continuous(limits = c(0, 0.6),labels=scales::percent) +
 geom_col() + 
 theme_hc()+ 
 ylab("") + xlab("Drought Frequency")
data1

b <- Drought %>%dplyr::select(q114_drought_int, Province)
bb <- na.omit(b)
data2 <- bb %>%
 group_by(q114_drought_int) %>%
 summarise(total=n(),.groups = 'drop') %>% 
 mutate(percent = total/sum(total)) %>% 
 ggplot(aes(q114_drought_int, percent, fill = as.factor(q114_drought_int),width=0.75)) +
 scale_y_continuous(limits = c(0, 0.6),labels=scales::percent) +
 scale_fill_brewer(palette = "YlOrBr") +
 geom_col(show.legend = FALSE) + 
 geom_text(aes(label = total), position = position_dodge(0.9), vjust = -0.3, size = 3) +
 theme_hc()+ scale_colour_hc() +
 ylab("") + xlab("Drought Intensity")
data2

library(cowplot) # arrange the three plots in a single row
prow <- plot_grid( data1 + theme(legend.position="none"),
                   data2 + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 1)
# extract the legend from one of the plots
legend_b <- get_legend(data1 + theme(legend.position="bottom"))
p <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1,0.1))
p

########## PLOT PERCEPTION DATA BY REGIONS #########
data_frequency2 <- aa %>%
 group_by(Province,q114_drought_freq) %>%
 summarise(total=n()) %>% 
 mutate(percent = total/sum(total))

data3 <- data_frequency2 %>% 
 ggplot(aes(Province, percent,fill=as.factor(q114_drought_freq),width=0.75)) +
 scale_fill_brewer(palette = "YlOrBr", labels=c("Highly decrease",
                                                "Slightly decrease",
                                                "No change",
                                                "Slightly increase",
                                                "Highly increase")) +
 geom_col(width = 0.6, position = "fill") +
 coord_flip() + 
 scale_y_continuous(limits = c(0, 1),labels=scales::percent) +
 labs(x = NULL, y = "Drought Frequency") + 
 theme_hc() + theme(legend.title = element_blank())
data3

########## PLOT INTENSITY DATA BY REGIONS #########
data_intensity2 <- bb %>%
 group_by(Province,q114_drought_int) %>%
 summarise(total=n()) %>% 
 mutate(percent = total/sum(total))

data4 <- data_intensity2 %>% 
 ggplot(aes(Province, percent,fill=as.factor(q114_drought_int),width=0.75)) +
 scale_fill_brewer(palette = "YlOrBr", labels=c("Highly decrease",
                                                "Slightly decrease",
                                                "No change",
                                                "Slightly increase",
                                                "Highly increase")) +
 geom_col(width = 0.6, position = "fill") +
 coord_flip() + 
 scale_y_continuous(limits = c(0, 1),labels=scales::percent) +
 labs(x = NULL, y = "Drought Intensity") + 
 theme_hc() + theme(legend.title = element_blank())
data4

prow_2 <- plot_grid( data3 + theme(legend.position="none"),
                     data4 + theme(legend.position="none"),
                     align = 'vh',
                     hjust = -1,
                     nrow = 1)
# extract the legend from one of the plots
legend_b2 <- get_legend(data3 + theme(legend.position="bottom"))
p2 <- plot_grid(prow_2, legend_b2, ncol = 1, rel_heights = c(1,0.1))
p2

#####################################################
########### PART 2: REGRESSION MODEL ################
#####################################################
###PREPARE DATA#######

Drought_freq_int1 <- Drought %>% 
 dplyr::select(q114_drought_freq, q114_drought_int, Aweighted2,
               CC_impact_belief, Drought_2014_2018, Drought_2009_2018, 
               Drought_2018,Drought_2017, Drought_2016, Drought_2015, Drought_2014, Drought_2013, 
               Drought_2012, Drought_2011, Drought_2010, Drought_2009, Drought_2008, Drought_2007,
               Age, On_farm_Experience,Age_factor, Gender, Education, Total_area, Province,
               diff_focus_climate,diff_focus_ghgs,diff_focus_water_efficient,diff_focus_quality_efficient, q22_ta)
Drought_freq_clean <- na.omit(Drought_freq_int1)

#plot correlation between age and on farm exp
plot(Drought_freq_clean$Age, Drought_freq_clean$On_farm_Experience,
     xlab = "Age", ylab = "On farm Experience")

##############################################################################
######## PART 2.1: PERCEPTION OF DROUGHT FREQ AND INTENSITY ##################
##############################################################################
#MODELS FOR MULTIPLE YEARS
olr_Perception_freq4 <- polr(factor(q114_drought_freq) ~ Age_factor + On_farm_Experience + Gender + Education+ Total_area+Drought_2009_2018,
                             data = Drought_freq_clean, Hess = TRUE)
olr_Perception_freq5 <- polr(factor(q114_drought_freq) ~ Age_factor + On_farm_Experience + Gender + Education+ Total_area+Drought_2014_2018, 
                             data = Drought_freq_clean, Hess = TRUE)
olr_Perception_freq3 <- polr(factor(q114_drought_freq) ~ Age_factor + On_farm_Experience + Gender + Education+ Total_area+Drought_2010+Drought_2011+Drought_2013+
                              Drought_2014+Drought_2015+Drought_2017+Drought_2018,
                             data = Drought_freq_clean, Hess = TRUE)

olr_Perception_int4 <- polr(factor(q114_drought_int) ~ 
                             Age_factor + On_farm_Experience + Gender + Education+ Total_area+Drought_2009_2018,
                            data = Drought_freq_clean, Hess = TRUE)
olr_Perception_int5 <- polr(factor(q114_drought_int) ~ 
                             Age_factor + On_farm_Experience + Gender + Education+ Total_area+Drought_2014_2018, 
                            data = Drought_freq_clean, Hess = TRUE)
olr_Perception_int3 <- polr(factor(q114_drought_int) ~ 
                             Age_factor + On_farm_Experience + Gender + Education+ Total_area+Drought_2010+Drought_2011+Drought_2013+
                             Drought_2014+Drought_2015+Drought_2017+Drought_2018,
                            data = Drought_freq_clean, Hess = TRUE)
library(brant)
a <- brant(olr_Perception_freq3, by.var=F)
b <- brant(olr_Perception_freq4, by.var=F)
c <- brant(olr_Perception_freq5, by.var=F)
d <- brant(olr_Perception_int3, by.var=F)
e <- brant(olr_Perception_int4, by.var=F)
f <- brant(olr_Perception_int5, by.var=F)
stargazer(a,b,c,d,e,f,type="text",align=F, out="brant test perception")
## check p-value manually, to compare with the OR table
(ctable <- coef(summary(olr_Perception_freq3)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
ci <- confint.default(olr_Perception_freq3) # CIs asszuming normality
exp(cbind(OR = coef(olr_Perception_freq3), ci))

library(stargazer)
models <- list(olr_Perception_freq3,olr_Perception_freq4,olr_Perception_freq5,
               olr_Perception_int3, olr_Perception_int4,olr_Perception_int5)
stargazer(models, apply.coef=exp, t.auto=F, p.auto=F, type = "text",
          out="OLR Relative risk ratios of Drought Perception & Intensity",align=TRUE)

#Check models without age control - show in Appendix in the paper
olr_Perception_freq0 <- polr(factor(q114_drought_freq) ~On_farm_Experience + Gender + Education+ Total_area+Drought_2009_2018,
                             data = Drought_freq_clean, Hess = TRUE)
olr_Perception_freq1 <- polr(factor(q114_drought_freq) ~ On_farm_Experience + Gender + Education+ Total_area+Drought_2014_2018, 
                             data = Drought_freq_clean, Hess = TRUE)
olr_Perception_freq2 <- polr(factor(q114_drought_freq) ~ On_farm_Experience + Gender + Education+ Total_area+Drought_2010+Drought_2011+Drought_2013+
                              Drought_2014+Drought_2015+Drought_2017+Drought_2018,
                             data = Drought_freq_clean, Hess = TRUE)

olr_Perception_int0 <- polr(factor(q114_drought_int) ~ 
                             On_farm_Experience + Gender + Education+ Total_area+Drought_2009_2018,
                            data = Drought_freq_clean, Hess = TRUE)
olr_Perception_int1 <- polr(factor(q114_drought_int) ~ 
                             On_farm_Experience + Gender + Education+ Total_area+Drought_2014_2018, 
                            data = Drought_freq_clean, Hess = TRUE)
olr_Perception_int2 <- polr(factor(q114_drought_int) ~ 
                             On_farm_Experience + Gender + Education+ Total_area+Drought_2010+Drought_2011+Drought_2013+
                             Drought_2014+Drought_2015+Drought_2017+Drought_2018,
                            data = Drought_freq_clean, Hess = TRUE)
library(brant)
a1 <- brant(olr_Perception_freq0, by.var=F)
b1 <- brant(olr_Perception_freq1,by.var=F)
c1 <- brant(olr_Perception_freq2, by.var=F)
d1 <- brant(olr_Perception_int0, by.var=F)
e1 <- brant(olr_Perception_int1, by.var=F)
f1 <- brant(olr_Perception_int2, by.var=F)
stargazer(a1,b1,c1,d1,e1,f1,type="text",align=F, out="brant test perception")

models2 <- list(olr_Perception_freq2,olr_Perception_freq0,olr_Perception_freq1,
                olr_Perception_int2, olr_Perception_int0,olr_Perception_int1)
stargazer(models2, apply.coef=exp, t.auto=F, p.auto=F, type = "text",
          out="OLR without age Relative risk ratios of Drought Perception & Intensity",align=TRUE)

#Check weighted models - with using years as weighted average value - not show in the paper
olr_Perception_freq6 <- polr(factor(q114_drought_freq) ~ Aweighted2 + On_farm_Experience + Gender + Education+ Total_area+Drought_2009_2018,
                             data = Drought_freq_clean, Hess = TRUE)
olr_Perception_freq7 <- polr(factor(q114_drought_freq) ~ Aweighted2 + On_farm_Experience + Gender + Education+ Total_area+Drought_2014_2018, 
                             data = Drought_freq_clean, Hess = TRUE)
olr_Perception_freq8 <- polr(factor(q114_drought_freq) ~ Aweighted2 + On_farm_Experience + Gender + Education+ Total_area+Drought_2010+Drought_2011+Drought_2013+
                              Drought_2014+Drought_2015+Drought_2017+Drought_2018,
                             data = Drought_freq_clean, Hess = TRUE)

olr_Perception_int6 <- polr(factor(q114_drought_int) ~ Aweighted2 + On_farm_Experience + Gender + Education+ Total_area+Drought_2009_2018,
                            data = Drought_freq_clean, Hess = TRUE)
olr_Perception_int7 <- polr(factor(q114_drought_int) ~ Aweighted2 + On_farm_Experience + Gender + Education+ Total_area+Drought_2014_2018, 
                            data = Drought_freq_clean, Hess = TRUE)
olr_Perception_int8 <- polr(factor(q114_drought_int) ~ Aweighted2 + On_farm_Experience + Gender + Education+ Total_area+Drought_2010+Drought_2011+Drought_2013+
                             Drought_2014+Drought_2015+Drought_2017+Drought_2018,
                            data = Drought_freq_clean, Hess = TRUE)

library(brant)
a2 <- brant(olr_Perception_freq6, by.var=F)
b2 <- brant(olr_Perception_freq7, by.var=F)
c2 <- brant(olr_Perception_freq8, by.var=F)
d2 <- brant(olr_Perception_int6, by.var=F)
e2 <- brant(olr_Perception_int7, by.var=F)
f2 <- brant(olr_Perception_int8, by.var=F)
stargazer(a2,b2,c2,d2,e2,f2,type="text",align=F, out="brant test perception")

models3 <- list(olr_Perception_freq6,olr_Perception_freq7,olr_Perception_freq8,
                olr_Perception_int6, olr_Perception_int7,olr_Perception_int8)
stargazer(models3, apply.coef=exp, t.auto=F, p.auto=F, type = "text",
          out="OLR Weighted Relative risk ratios of Drought Perception & Intensity",align=TRUE)

#Check weighted models - with using years as weighted average value - show in the appendix and rObustness
olr_Perception_freq9 <- polr(factor(q114_drought_freq) ~ Aweighted2 + On_farm_Experience + Gender + Education+ Total_area,
                             data = Drought_freq_clean, Hess = TRUE)
olr_Perception_freq10 <- polr(factor(q114_drought_freq) ~ Aweighted2 + On_farm_Experience + Gender + Education+ Total_area, 
                              data = Drought_freq_clean, Hess = TRUE)
olr_Perception_freq11 <- polr(factor(q114_drought_freq) ~ Aweighted2 + On_farm_Experience + Gender + Education+ Total_area,
                              data = Drought_freq_clean, Hess = TRUE)

olr_Perception_int9 <- polr(factor(q114_drought_int) ~ Aweighted2 + On_farm_Experience + Gender + Education+ Total_area,
                            data = Drought_freq_clean, Hess = TRUE)
olr_Perception_int10 <- polr(factor(q114_drought_int) ~ Aweighted2 + On_farm_Experience + Gender + Education+ Total_area, 
                             data = Drought_freq_clean, Hess = TRUE)
olr_Perception_int11 <- polr(factor(q114_drought_int) ~ Aweighted2 + On_farm_Experience + Gender + Education+ Total_area,
                             data = Drought_freq_clean, Hess = TRUE)

library(brant)
a3 <- brant(olr_Perception_freq9, by.var=F)
b3 <- brant(olr_Perception_freq10, by.var=F)
c3 <- brant(olr_Perception_freq11, by.var=F)
d3 <- brant(olr_Perception_int9, by.var=F)
e3 <- brant(olr_Perception_int10, by.var=F)
f3 <- brant(olr_Perception_int11, by.var=F)
stargazer(a3,b3,c3,d3,e3,f3,type="text",align=F, out="brant test perception")

models4 <- list(olr_Perception_freq9,olr_Perception_freq10,olr_Perception_freq11,
                olr_Perception_int9, olr_Perception_int10,olr_Perception_int11)
stargazer(models4, apply.coef=exp, t.auto=F, p.auto=F, type = "text",
          out="OLR Weighted with out years Relative risk ratios of Drought Perception & Intensity",align=TRUE)

################################################################
########## PART 2.2. DIFFERENT FOCUS NEXT 5 YEARS AND LAST 5 YEARS #######
################################################################
#recoding the data variables of 3 different focus
Drought_freq_clean <- Drought_freq_clean %>% 
 mutate(diff_focus_climate1 = case_when(
  diff_focus_climate == "-3" ~ "0",
  diff_focus_climate == "-2" ~ "0",
  diff_focus_climate == "-1" ~ "0",
  diff_focus_climate == "0" ~ "1",
  diff_focus_climate == "1" ~ "2",
  diff_focus_climate == "2" ~ "3",
  diff_focus_climate == "3" ~ "3")) %>% 
 mutate(diff_focus_ghgs1 = case_when(
  diff_focus_ghgs == "-3" ~ "0",
  diff_focus_ghgs == "-2" ~ "0",
  diff_focus_ghgs == "-1" ~ "0",
  diff_focus_ghgs == "0" ~ "1",
  diff_focus_ghgs == "1" ~ "2",
  diff_focus_ghgs == "2" ~ "3",
  diff_focus_ghgs == "3" ~ "3")) %>% 
 mutate(diff_focus_water_efficient1 = case_when(
  diff_focus_water_efficient == "-3" ~ "0",
  diff_focus_water_efficient == "-2" ~ "0",
  diff_focus_water_efficient == "-1" ~ "0",
  diff_focus_water_efficient == "0" ~ "1",
  diff_focus_water_efficient == "1" ~ "2",
  diff_focus_water_efficient == "2" ~ "3",
  diff_focus_water_efficient == "3" ~ "3")) %>% 
 mutate(diff_focus_quality_efficient1 = case_when(
  diff_focus_quality_efficient == "-3" ~ "0",
  diff_focus_quality_efficient == "-2" ~ "0",
  diff_focus_quality_efficient == "-1" ~ "0",
  diff_focus_quality_efficient == "0" ~ "1",
  diff_focus_quality_efficient == "1" ~ "2",
  diff_focus_quality_efficient == "2" ~ "3",
  diff_focus_quality_efficient == "3" ~ "3")) 

####################################
####### START REGRESSION ###########
###################################3
## 1. MODELS PART 1 WITH TOTAL YEARS FROM 2009 TO 2018
olr_focus_climate1 <- polr(factor(diff_focus_climate1) ~ q114_drought_freq + Drought_2009_2018+
                            Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                           data = Drought_freq_clean, Hess = TRUE)
olr_focus_ghgs1 <- polr(factor(diff_focus_ghgs1) ~ q114_drought_freq + Drought_2009_2018+
                         Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                        data = Drought_freq_clean, Hess = TRUE)
olr_focus_water_efficient1 <- polr(factor(diff_focus_water_efficient1) ~ q114_drought_freq + Drought_2009_2018+
                                    Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                                   data = Drought_freq_clean, Hess = TRUE)

olr_focus_climate2 <- polr(factor(diff_focus_climate1) ~ q114_drought_int + Drought_2009_2018+
                            Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                           data = Drought_freq_clean, Hess = TRUE)
olr_focus_ghgs2 <- polr(factor(diff_focus_ghgs1) ~ q114_drought_int + Drought_2009_2018+
                         Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                        data = Drought_freq_clean, Hess = TRUE)
olr_focus_water_efficient2 <- polr(factor(diff_focus_water_efficient1) ~ q114_drought_int + Drought_2009_2018+
                                    Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                                   data = Drought_freq_clean, Hess = TRUE)
library(brant)
a4 <- brant(olr_focus_climate1, by.var=F)
b4 <- brant(olr_focus_ghgs1, by.var=F)
c4 <- brant(olr_focus_water_efficient1, by.var=F)
d4 <-brant(olr_focus_climate2, by.var=F)
e4 <-brant(olr_focus_ghgs2, by.var=F)
f4 <-brant(olr_focus_water_efficient2, by.var=F)
stargazer(a4,b4,c4,d4,e4,f4,type="html",align=F,out="brant test focus 1")

models5 <- list(olr_focus_climate1,olr_focus_ghgs1,olr_focus_water_efficient1,
                olr_focus_climate2,olr_focus_ghgs2,olr_focus_water_efficient2)
stargazer(models5, apply.coef=exp, t.auto=F, p.auto=F, type = "text",
          out="OLR Relative risk ratios of focus",align=TRUE)

##MODELS PART 2 WITH MULTIPLE YEARS FROM 2010 TO 2018
olr_focus_climate3 <- polr(factor(diff_focus_climate1) ~ q114_drought_freq + Drought_2010+Drought_2011+Drought_2013+
                            Drought_2014+Drought_2015+Drought_2017+Drought_2018+
                            Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                           data = Drought_freq_clean, Hess = TRUE)
olr_focus_ghgs3 <- polr(factor(diff_focus_ghgs1) ~ q114_drought_freq + Drought_2010+Drought_2011+Drought_2013+
                         Drought_2014+Drought_2015+Drought_2017+Drought_2018+
                         Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                        data = Drought_freq_clean, Hess = TRUE)
olr_focus_water_efficient3 <- polr(factor(diff_focus_water_efficient1) ~ q114_drought_freq + Drought_2010+Drought_2011+Drought_2013+
                                    Drought_2014+Drought_2015+Drought_2017+Drought_2018+
                                    Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                                   data = Drought_freq_clean, Hess = TRUE)

olr_focus_climate4 <- polr(factor(diff_focus_climate1) ~ q114_drought_int + Drought_2010+Drought_2011+Drought_2013+
                            Drought_2014+Drought_2015+Drought_2017+Drought_2018+
                            Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                           data = Drought_freq_clean, Hess = TRUE)
olr_focus_ghgs4 <- polr(factor(diff_focus_ghgs1) ~ q114_drought_int + Drought_2010+Drought_2011+Drought_2013+
                         Drought_2014+Drought_2015+Drought_2017+Drought_2018+
                         Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                        data = Drought_freq_clean, Hess = TRUE)
olr_focus_water_efficient4 <- polr(factor(diff_focus_water_efficient1) ~ q114_drought_int + Drought_2010+Drought_2011+Drought_2013+
                                    Drought_2014+Drought_2015+Drought_2017+Drought_2018+
                                    Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                                   data = Drought_freq_clean, Hess = TRUE)

a5 <- brant(olr_focus_climate1, by.var=F)
b5 <- brant(olr_focus_ghgs1, by.var=F)
c5 <- brant(olr_focus_water_efficient1, by.var=F)
d5 <-brant(olr_focus_climate2, by.var=F)
e5 <-brant(olr_focus_ghgs2, by.var=F)
f5 <-brant(olr_focus_water_efficient2, by.var=F)
stargazer(a5,b5,c5,d5,e5,f5,type="html",align=F,out="brant test focus 2")

models6 <- list(olr_focus_climate3,olr_focus_ghgs3,olr_focus_water_efficient3,
                olr_focus_climate4,olr_focus_ghgs4,olr_focus_water_efficient4)
stargazer(models6, apply.coef=exp, t.auto=F, p.auto=F, type = "text",
          out="OLR Relative risk ratios of focus in multiple years",align=TRUE)

##MODELS PART 3 WITH NO YEARS
olr_focus_climate5 <- polr(factor(diff_focus_climate1) ~ q114_drought_freq +
                            Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                           data = Drought_freq_clean, Hess = TRUE)
olr_focus_ghgs5 <- polr(factor(diff_focus_ghgs1) ~ q114_drought_freq + 
                         Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                        data = Drought_freq_clean, Hess = TRUE)
olr_focus_water_efficient5 <- polr(factor(diff_focus_water_efficient1) ~ q114_drought_freq + 
                                    Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                                   data = Drought_freq_clean, Hess = TRUE)

olr_focus_climate6 <- polr(factor(diff_focus_climate1) ~ q114_drought_int + 
                            Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                           data = Drought_freq_clean, Hess = TRUE)
olr_focus_ghgs6 <- polr(factor(diff_focus_ghgs1) ~ q114_drought_int + 
                         Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                        data = Drought_freq_clean, Hess = TRUE)
olr_focus_water_efficient6 <- polr(factor(diff_focus_water_efficient1) ~ q114_drought_int + 
                                    Age_factor + On_farm_Experience + Gender + Education+ Total_area, 
                                   data = Drought_freq_clean, Hess = TRUE)

a6 <- brant(olr_focus_climate3, by.var=F)
b6 <- brant(olr_focus_ghgs3, by.var=F)
c6 <- brant(olr_focus_water_efficient3, by.var=F)
d6 <-brant(olr_focus_climate4, by.var=F)
e6 <-brant(olr_focus_ghgs4, by.var=F)
f6 <-brant(olr_focus_water_efficient4, by.var=F)
stargazer(a5,b5,c6,d6,e6,f6,type="html",align=F,out="brant test focus 3 without years")

models7 <- list(olr_focus_climate5,olr_focus_ghgs5,olr_focus_water_efficient5,
                olr_focus_climate6,olr_focus_ghgs6,olr_focus_water_efficient6)
stargazer(models7, apply.coef=exp, t.auto=F, p.auto=F, type = "text")

