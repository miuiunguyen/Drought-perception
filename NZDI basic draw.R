###########################################################
##-----MERGE FILES INTO 1 FILE----------
#--------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/Ms Mui/OneDrive - Victoria University of Wellington - STAFF/Data/data NZDI/draw_R_data")
getwd()
library(haven)
library(dplyr)
library(tidyr)
library(tibble)
library(multcomp)
library(nlme)
library(XLConnect)
library(XLConnectJars)
library(lme4)
library(car)
library(rJava)
library(xlsx)
library(readxl)
library(foreign)
library(car)
library(MASS)
library(nnet)
drought_days <- read_excel("drought days by province.xlsx", sheet = 1)
drought_frequency <- read_excel("drought days by province.xlsx", sheet = 3)
dist_frequency <- read_excel("drought days by province.xlsx", sheet = 2)
total_drought_days <- read_excel("drought days by province.xlsx", sheet = 5)

##1. PLOT DROUGHT FREQUENCY
library(ggplot2)
library(ggthemes)
drought_frequency$Month = factor(drought_frequency$Month, levels = month.abb)
drought_frequency2 <- drought_frequency %>%
        group_by(Type) %>%
        mutate(percent = index/sum(index)) %>% 
        ggplot(aes(Month, percent, fill = Type,width=0.6)) +
        #scale_fill_brewer(labels=c("Drought", "Severe Drought")) +
        theme(legend.title = element_blank()) +
        scale_y_continuous(limits = c(0, 0.35),labels=scales::percent) +
        scale_x_discrete(limits = month.abb) +
        geom_col(position = "dodge") + 
        #geom_text(aes(label = index), position = position_dodge(0.9), vjust = -0.3, size = 3)+
        labs(x = NULL, y = "Drought Frequency")+
        theme_classic() +scale_fill_manual(labels=c("Drought", "Severe Drought"),
                                      values=c("#f4bc64", "#b77345"))
drought_frequency2

#PLOT DISTRICTS EXPERIENCING DROUGHT IN NZ OVER YEARS
#78 is the number of districts extract from NZDI data in NZ
dist_frequency2 <- dist_frequency %>%
        group_by(type) %>%
        mutate(percent = districts/78) %>% 
        ggplot(aes(as.factor(year), percent, fill = type,width=0.6)) +
        #scale_fill_brewer(labels=c("Drought", "Severe Drought")) +
        theme(legend.title = element_blank()) +
        scale_y_continuous(limits = c(0, 0.35),labels=scales::percent) +
        geom_col(position = "dodge") + 
        #geom_text(aes(label = index), position = position_dodge(0.9), vjust = -0.3, size = 3)+
        labs(x = NULL, y = "Drought Frequency")+
        theme_classic() +scale_fill_manual(labels=c("Drought", "Severe Drought"),
                                           values=c("#f4bc64", "#b77345"))
dist_frequency2

#Drought day by district by years
library(ggplot2)
total_drought_days_1 <- total_drought_days %>%
        ggplot(aes(as.factor(year), drought_days, width=0.6)) +
        geom_bar(stat="identity", width=0.6, fill = "#b77345") +
        geom_text(aes(label = drought_days), position = position_dodge(0.9), vjust = -0.3, size = 3)+
        labs(y = "Total drought days", x = NULL) +theme_classic()
        #scale_color_manual(values="#f4bc64") +
        #theme_hc()
total_drought_days_1
####END OF DRAWINGS

