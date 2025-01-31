library(tidyverse)
library(geobr)

df_res <- data.frame()
states <- c(31,35,33)
load('latlong.RData')

for(state in states){
  print(state)
  path_lab <- paste0('data/lab_',paste0(state,'.csv'))
  path_epi <- paste0('results/epi_corr_',paste0(state,'.csv'))
  df_lab <- read.csv(path_lab)
  df_epi <- read.csv(path_epi)
  municipalities_tmp <- municipalities %>% filter(substr(ID_MN_RESI,1,2) == state)
  df_lab_res <- df_lab %>% group_by(ID_MN_RESI,CHIK) %>% summarise(lab = n())
  df_epi_res <- df_epi %>% group_by(ID_MN_RESI, CHIK) %>% summarise(epi = n())
  df_epi_corr_res <- df_epi %>% group_by(ID_MN_RESI, CHIK_CORR) %>% summarise(epi_corr = n())
  df_epi_corr_res <- df_epi_corr_res %>% rename(CHIK = CHIK_CORR)
  df_epi_res <- df_epi_res %>% mutate(ID_MN_RESI = as.numeric(ID_MN_RESI))
  df_epi_corr_res <- df_epi_corr_res %>% mutate(ID_MN_RESI = as.numeric(ID_MN_RESI))
  df_res_tmp <- municipalities_tmp %>% select(ID_MN_RESI)
  df_res_tmp <- expand.grid(df_res_tmp$ID_MN_RESI, c(0,1))
  colnames(df_res_tmp) <- c('ID_MN_RESI', 'CHIK')
  df_res_tmp <- df_res_tmp %>% mutate(ID_MN_RESI = as.numeric(as.character(ID_MN_RESI)),
                                      CHIK = as.numeric(as.character(CHIK)))
  df_res_tmp <- df_res_tmp %>% left_join(df_lab_res, by = join_by(ID_MN_RESI, CHIK))
  df_res_tmp <- df_res_tmp %>% left_join(df_epi_res, by = join_by(ID_MN_RESI, CHIK))
  df_res_tmp <- df_res_tmp %>% left_join(df_epi_corr_res, by = join_by(ID_MN_RESI, CHIK))
  df_res_tmp[is.na(df_res_tmp)] <- 0
  df_res <- rbind(df_res, df_res_tmp)
}

# Doing a summary of the corrections
df_resumo <- df_res %>% mutate(SG_UF = substr(ID_MN_RESI, 1, 2))
df_resumo <- df_resumo %>% group_by(SG_UF, CHIK) %>% summarise(lab = sum(lab), epi = sum(epi), epi_corr = sum(epi_corr))


# Loading Brazil map

# Plotting spatial distribution of dengue - Total

## Before correction


## After correction

# Plotting spatial distribution of chik - Total

## Before correction


## After correction
