# Fitting logistic regression in R for dengue/chikungunuya

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

# Initialize result dataframe
result_df <- data.frame()
symptom_df <- data.frame()

for(sg_mun in sg_values){
  print(sg_mun)
  # Filtrating
  df_total_sg <- df_total %>% filter(SG_UF == sg_mun)
  df_epi_sg <- df_epi %>% filter(SG_UF == sg_mun)
  
  
  # Doing split and stratification, dividing into train and test
  cols <- c('SG_UF')
  df_total_sg[cols] <- lapply(df_total_sg[cols], as.factor)
  df_total_sg$row <- 1:nrow(df_total_sg)
  df_train <- stratified(df_total_sg, c('CHIK'), 0.7)
  df_test <- df_total_sg[!(df_total_sg$row %in% df_train$row),]
  df_train <- df_train %>% select(!row)
  df_test <- df_test %>% select(!row)
  df_train <- df_train %>% mutate(lat_m = (lat - mean(lat))/sd(lat),
                                  long_m = (long - mean(long))/sd(long))
  df_test <- df_test %>% mutate(lat_m = (lat - mean(df_train$lat))/sd(df_train$lat),
                                  long_m = (long - mean(df_train$long))/sd(df_train$long))
  # Train most simple model
  if(sg_mun == 53){
    model <- glm(CHIK ~ fx_etaria  + FEBRE + MIALGIA + CEFALEIA + EXANTEMA + VOMITO + NAUSEA + DOR_COSTAS + ARTRITE + ARTRALGIA + PETEQUIA_N + LEUCOPENIA + DOR_RETRO,
                 family=binomial(link='logit'), data = df_train)
  }else{
    model <- glm(CHIK ~ lat_m + long_m  + fx_etaria +  FEBRE + MIALGIA + CEFALEIA + EXANTEMA + VOMITO + NAUSEA + DOR_COSTAS + ARTRITE + ARTRALGIA + PETEQUIA_N + LEUCOPENIA + DOR_RETRO,
               family=binomial(link='logit'), data = df_train)
  }
  
  # Summary model
  summary(model)
  
  # Exponential coefficients
  exp(model$coefficients)
  
  # Measuring performance
  res1 <- roc(CHIK ~ fitted(model),
              data = df_train
  ) 
  res1$auc
  predicted_prob <- predict(model, newdata = df_test, type = "response")
  predicted_prob_train <- predict(model, newdata = df_train, type = "response")
  res2 <- roc(df_test$CHIK, predicted_prob)
  res2$auc
  
  
  # Choosing optimal threshold
  coords <- coords(res2, "best", best.method = "closest.topleft")
  specificity_test <- coords$specificity
  sensitivity_test <- coords$sensitivity
  best_threshold <- coords$threshold 
  cat("Optimal threshold based on ROC curve:", best_threshold, "\n")
  
  
  # Results as a whole
  df_epi_sg <- df_epi_sg %>% mutate(lat_m = (lat - mean(df_train$lat))/sd(df_train$lat),
                                    long_m = (long - mean(df_train$long))/sd(df_train$long))
  df_epi_pred <- ifelse(predict(model, newdata = df_epi_sg, type = 'response') >= best_threshold, 1, 0)
  
  vec <- c(sg_mun, res1$auc[1], res2$auc[1], specificity_test, sensitivity_test, sum(df_total_sg$CHIK == 0), sum(df_total_sg$CHIK == 1),
        sum(df_epi_sg$CHIK == 0), sum(df_epi_sg$CHIK == 1), sum(df_epi_pred == 0),
        sum(df_epi_pred == 1))
  df_tmp <- as.data.frame(t(unlist(vec)))
  result_df <- rbind(result_df, df_tmp)
  #symptom_df <- rbind(symptom_df,as.data.frame(t(unlist(exp(model$coefficients)))))

}

colnames(result_df) <- c('SG_UF', 'AUC Train', 'AUC Test',
                         'Spec Test', 'Senst Test', 'Dengue Lab', 'Chik Lab',
                         'Dengue Epi', 'Chik Epi', 'Dengue Epi Corr', 'Chik Epi Corr')

#symptom_mean <- data.frame(name = colnames(symptom_df)[15:26], mean = colMeans(symptom_df, na.rm = TRUE)[15:26])

