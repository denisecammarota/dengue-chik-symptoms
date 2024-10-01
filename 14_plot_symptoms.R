# Plotting distribution of symptoms per state


library(tidyverse)
library(dplyr)
library(splitstackshape)
library(sjPlot)
library(pROC)
library(data.table)
library(gtsummary)
library(gt)
library(finalfit)
library(gridExtra)
source('theme.publication.R')
set.seed(42) 


setwd('C:/Users/denis/Documents/dengue-chik-symptoms')

df_denv <- read.csv('denv_2024_processed.csv')
df_chik <- read.csv('chik_2024_processed.csv')
df_denv['CHIK'] <- 0
df_chik['CHIK'] <- 1
df_denv[is.na(df_denv)] <- 2
df_chik[is.na(df_chik)] <- 2
df_total <- rbind(df_denv, df_chik)
df_total[df_total == 2] <- 0
df_total[df_total == 2] <- 0
rm(df_denv, df_chik)

list_symptoms <- c('FEBRE','MIALGIA', 'CEFALEIA', 'EXANTEMA',
                   'VOMITO', 'NAUSEA', 'DOR_COSTAS', 'ARTRITE',
                   'ARTRALGIA', 'PETEQUIA_N', 'LEUCOPENIA', 'DOR_RETRO')

i = 1
for(symptom in list_symptoms){
  name <- list_symptoms[i]
  df_total['symptom'] <- df_total[name]
  df_summ <- df_total %>% group_by(SG_UF, symptom) %>% summarise(n = n())
  p <- ggplot(df_summ, aes(x = as.factor(SG_UF), y = n, fill = symptom)) + 
    geom_bar(stat = "identity", position="fill") + xlab('State') + ylab('Proportion') +
    theme_bw() + labs(fill = name)
  print(p)
  i = i + 1
}











