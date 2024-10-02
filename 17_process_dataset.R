# Extracting dataset for SP for ML ###################################

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

# Reading data from dengue and chikungunya and separating 
df_denv <- read.csv('denv_2024_processed.csv')
df_chik <- read.csv('chik_2024_processed.csv')

# Assigning whether dengue or chik
df_denv['CHIK'] <- 0
df_chik['CHIK'] <- 1

# Filling NAs with 2s 
df_denv[is.na(df_denv)] <- 2
df_chik[is.na(df_chik)] <- 2

# Changing criteria
df_denv <- df_denv %>% filter(CRITERIO != 3)
df_chik <- df_chik %>% filter(CRITERIO != 3)

# Changing criteria by names
df_denv <- df_denv %>% mutate(CRITERIO = case_when(
  CRITERIO == 1 ~ 'Laboratorial',
  CRITERIO == 2 ~ 'EpiClinico'
))

df_chik <- df_chik %>% mutate(CRITERIO = case_when(
  CRITERIO == 1 ~ 'Laboratorial',
  CRITERIO == 2 ~ 'EpiClinico'
))

# Total dataframe
df_total <- rbind(df_denv,df_chik)

# Replacing 2s by 0s
df_total[df_total == 2] <- 0


# Saving dataset
write_csv(df_total, 'df_total.csv')
