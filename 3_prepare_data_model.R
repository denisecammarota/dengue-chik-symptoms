# Script to separate chik and denv data in order to train ML models
# Code developed by Denise Cammarota

library(tidyverse)
library(dplyr)
library(read.dbc)
library(data.table)

# Loading data ######################################################
df <- fread('denv_2024.csv') # é mais rápido
df_chik <- fread('chik_2024.csv')

# Separating confirmed cases #########################################

df <- df %>% filter(CLASSI_FIN %in% c(10,11,12))
df_chik <- df_chik %>% filter(CLASSI_FIN %in% c(13))

# Filtering the ones with at least one symptom ########################

df_chik <- df_chik %>% filter(!(
  is.na(FEBRE) & is.na(MIALGIA) & is.na(CEFALEIA) & is.na(EXANTEMA) & is.na(VOMITO) &
    is.na(NAUSEA) & is.na(DOR_COSTAS) & is.na(CONJUNTVIT) & is.na(ARTRITE) & is.na(ARTRALGIA) &
    is.na(PETEQUIA_N) & is.na(LEUCOPENIA) & is.na(LACO) & is.na(DOR_RETRO)
))

df <- df %>% filter(!(
  is.na(FEBRE) & is.na(MIALGIA) & is.na(CEFALEIA) & is.na(EXANTEMA) & is.na(VOMITO) &
    is.na(NAUSEA) & is.na(DOR_COSTAS) & is.na(CONJUNTVIT) & is.na(ARTRITE) & is.na(ARTRALGIA) &
    is.na(PETEQUIA_N) & is.na(LEUCOPENIA) & is.na(LACO) & is.na(DOR_RETRO)
))


# Filtering for the state of SP #############################################
# in order to use few data for model training 

# df <- df %>% filter(SG_UF == 35)

# df_chik <- df_chik %>% filter(SG_UF == 35)


df <- df %>% select("SG_UF","NU_IDADE_N" ,"CRITERIO", "FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                        "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                        "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO") 

df_chik <- df_chik %>% select("SG_UF","NU_IDADE_N" ,"CRITERIO", "FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                             "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                             "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO") 

df <- df %>% replace(is.na(.), 2)
df_chik <- df_chik %>% replace(is.na(.), 2)


write_csv(df, 'denv_2024_processed.csv')
write_csv(df_chik, 'chik_2024_processed.csv')








