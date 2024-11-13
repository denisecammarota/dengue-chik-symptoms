# Preparing data for XGBoost in Python

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
library(xgboost)
source('theme.publication.R')
set.seed(42) 

setwd('C:/Users/denis/Documents/dengue-chik-symptoms')

# Reading data from dengue and chikungunya and separating 
df_denv <- read.csv('denv_2024_processed_3.csv')
df_chik <- read.csv('chik_2024_processed_3.csv')

df_denv['CHIK'] <- 0
df_chik['CHIK'] <- 1

df_denv[is.na(df_denv)] <- 2
df_chik[is.na(df_chik)] <- 2

df_denv_lab <- df_denv %>% filter(CRITERIO == 1)
df_chik_lab <- df_chik %>% filter(CRITERIO == 1)

df_denv_epi <- df_denv %>% filter(CRITERIO == 2)
df_chik_epi <- df_chik %>% filter(CRITERIO == 2)


# Separating by lab/epidemiological

df_total <- rbind(df_denv_lab,df_chik_lab)
df_epi <- rbind(df_denv_epi, df_chik_epi)

# Getting together all values possible of SG_UF
sg_values <- unique(df_total$SG_UF)

# Eliminating the rest
rm(df_chik, df_chik_epi, df_chik_lab, df_denv, df_denv_epi, df_denv_lab)


# Creating age groups for lab and epi
df_total <- df_total %>% mutate(fx_etaria = case_when(
  NU_IDADE_N >= 0 & NU_IDADE_N <= 4 ~ '0 a 4',
  NU_IDADE_N >= 5 & NU_IDADE_N <= 9 ~ '5 a 9',
  NU_IDADE_N >= 10 & NU_IDADE_N <= 14 ~ '10 a 14',
  NU_IDADE_N >= 15 & NU_IDADE_N <= 19 ~ '15 a 19',
  NU_IDADE_N >= 20 & NU_IDADE_N <= 29 ~ '20 a 29',
  NU_IDADE_N >= 30 & NU_IDADE_N <= 39 ~ '30 a 39',
  NU_IDADE_N >= 40 & NU_IDADE_N <= 49 ~ '40 a 49',
  NU_IDADE_N >= 50 & NU_IDADE_N <= 59 ~ '50 a 59',
  NU_IDADE_N >= 60 & NU_IDADE_N <= 69 ~ '60 a 69',
  NU_IDADE_N >= 70 & NU_IDADE_N <= 79 ~ '70 a 79',
  NU_IDADE_N >= 80 ~ '80 e +'
)) %>% select(!NU_IDADE_N)


df_epi <- df_epi %>% mutate(fx_etaria = case_when(
  NU_IDADE_N >= 0 & NU_IDADE_N <= 4 ~ '0 a 4',
  NU_IDADE_N >= 5 & NU_IDADE_N <= 9 ~ '5 a 9',
  NU_IDADE_N >= 10 & NU_IDADE_N <= 14 ~ '10 a 14',
  NU_IDADE_N >= 15 & NU_IDADE_N <= 19 ~ '15 a 19',
  NU_IDADE_N >= 20 & NU_IDADE_N <= 29 ~ '20 a 29',
  NU_IDADE_N >= 30 & NU_IDADE_N <= 39 ~ '30 a 39',
  NU_IDADE_N >= 40 & NU_IDADE_N <= 49 ~ '40 a 49',
  NU_IDADE_N >= 50 & NU_IDADE_N <= 59 ~ '50 a 59',
  NU_IDADE_N >= 60 & NU_IDADE_N <= 69 ~ '60 a 69',
  NU_IDADE_N >= 70 & NU_IDADE_N <= 79 ~ '70 a 79',
  NU_IDADE_N >= 80 ~ '80 e +'
)) %>% select(!NU_IDADE_N)


# Adding latlong
load('latlong.RData')
municipalities <- municipalities %>% mutate(ID_MN_RESI = as.numeric(ID_MN_RESI))
df_total <- df_total %>% left_join(municipalities, by = join_by(ID_MN_RESI))
df_total <- df_total %>% drop_na()
df_epi <- df_epi %>% left_join(municipalities, by = join_by(ID_MN_RESI))
df_epi <- df_epi %>% drop_na()

# Eliminating criteria, changing symptoms to 1 or 0
df_total <- df_total %>% select(!CRITERIO)
df_total[df_total == 2] <- 0
df_total[df_total == 2] <- 0
df_epi <- df_epi %>% select(!CRITERIO)
df_epi[df_epi == 2] <- 0
df_epi[df_epi == 2] <- 0

# Changing hospitalization character
df_total <- df_total %>% mutate(HOSPITALIZ = ifelse(HOSPITALIZ == 1, 1, 0))
df_epi <- df_epi %>% mutate(HOSPITALIZ = ifelse(HOSPITALIZ == 1, 1, 0))

rm(municipalities)

for(sg_mun in sg_values){
  print(sg_mun)
  df_total_sg <- df_total %>% filter(SG_UF == sg_mun)
  df_epi_sg <- df_epi %>% filter(SG_UF == sg_mun)
  df_total_sg <- df_total_sg %>% select(!SG_UF)
  df_epi_sg <- df_epi_sg %>% select(!SG_UF)
  name_lab <- paste0(paste0('data/lab_',sg_mun),'.csv')
  name_epi <- paste0(paste0('data/epi_',sg_mun),'.csv')
  write_csv(df_total_sg, name_lab)
  write_csv(df_epi_sg, name_epi)
}












