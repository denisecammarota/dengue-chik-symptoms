library(tidyverse)
library(dplyr)
library(splitstackshape)
library(sjPlot)
library(pROC)
library(data.table)

setwd('C:/Users/denis/Documents/dengue-chik-symptoms')

# Initial analysis, number of cases of chik and denv per state

df_denv <- read.csv('denv_2024_processed.csv')
df_chik <- read.csv('chik_2024_processed.csv')
df_denv['CHIK'] <- 0
df_chik['CHIK'] <- 1
df_denv_lab <- df_denv %>% filter(CRITERIO == 1)
df_chik_lab <- df_chik %>% filter(CRITERIO == 1)

df <- rbind(df_denv_lab, df_chik_lab)

table(df$SG_UF,df$CHIK)
table(df$CHIK)

# Analysing the number of symptoms/filling out the forms per state

df[df == 2] <- 0
df[df == 2] <- 0

df['n'] <- df$FEBRE + df$MIALGIA + 
  df$CEFALEIA + df$EXANTEMA + df$VOMITO + df$NAUSEA + 
  df$DOR_COSTAS + df$CONJUNTVIT + df$ARTRITE + 
  df$ARTRALGIA + df$PETEQUIA_N + df$LEUCOPENIA + df$LACO + df$DOR_RETRO

table(df$n)
table(df$SG_UF,df$n)

