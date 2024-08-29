# Fitting logistic regression in R for dengue/chikungunuya

library(tidyverse)
library(dplyr)
library(splitstackshape)
library(sjPlot)
library(pROC)
library(data.table)
set.seed(42) 

setwd('C:/Users/denis/Documents/dengue-chik-symptoms')

# Reading data from dengue and chikungunya and separating 
df_denv <- read.csv('denv_2024_processed.csv')
df_chik <- read.csv('chik_2024_processed.csv')

df_denv <- df_denv %>% mutate(SG_11 = ifelse(SG_UF == 11,1,0),
                              SG_12 = ifelse(SG_UF == 12,1,0),
                              SG_13 = ifelse(SG_UF == 13,1,0),
                              SG_14 = ifelse(SG_UF == 14,1,0),
                              SG_15 = ifelse(SG_UF == 15,1,0),
                              SG_16 = ifelse(SG_UF == 16,1,0),
                              SG_17 = ifelse(SG_UF == 17,1,0),
                              SG_21 = ifelse(SG_UF == 21,1,0),
                              SG_22 = ifelse(SG_UF == 22,1,0),
                              SG_23 = ifelse(SG_UF == 23,1,0),
                              SG_24 = ifelse(SG_UF == 24,1,0),
                              SG_25 = ifelse(SG_UF == 25,1,0),
                              SG_26 = ifelse(SG_UF == 26,1,0),
                              SG_27 = ifelse(SG_UF == 27,1,0),
                              SG_28 = ifelse(SG_UF == 28,1,0),
                              SG_29 = ifelse(SG_UF == 29,1,0),
                              SG_31 = ifelse(SG_UF == 31,1,0),
                              SG_32 = ifelse(SG_UF == 32,1,0),
                              SG_33 = ifelse(SG_UF == 33,1,0),
                              SG_35 = ifelse(SG_UF == 35,1,0),
                              SG_41 = ifelse(SG_UF == 41,1,0),
                              SG_42 = ifelse(SG_UF == 42,1,0),
                              SG_43 = ifelse(SG_UF == 43,1,0),
                              SG_50 = ifelse(SG_UF == 50,1,0),
                              SG_51 = ifelse(SG_UF == 51,1,0),
                              SG_52 = ifelse(SG_UF == 52,1,0),
                              SG_53 = ifelse(SG_UF == 53,1,0)
                              )

df_chik <- df_chik %>% mutate(SG_11 = ifelse(SG_UF == 11,1,0),
                              SG_12 = ifelse(SG_UF == 12,1,0),
                              SG_13 = ifelse(SG_UF == 13,1,0),
                              SG_14 = ifelse(SG_UF == 14,1,0),
                              SG_15 = ifelse(SG_UF == 15,1,0),
                              SG_16 = ifelse(SG_UF == 16,1,0),
                              SG_17 = ifelse(SG_UF == 17,1,0),
                              SG_21 = ifelse(SG_UF == 21,1,0),
                              SG_22 = ifelse(SG_UF == 22,1,0),
                              SG_23 = ifelse(SG_UF == 23,1,0),
                              SG_24 = ifelse(SG_UF == 24,1,0),
                              SG_25 = ifelse(SG_UF == 25,1,0),
                              SG_26 = ifelse(SG_UF == 26,1,0),
                              SG_27 = ifelse(SG_UF == 27,1,0),
                              SG_28 = ifelse(SG_UF == 28,1,0),
                              SG_29 = ifelse(SG_UF == 29,1,0),
                              SG_31 = ifelse(SG_UF == 31,1,0),
                              SG_32 = ifelse(SG_UF == 32,1,0),
                              SG_33 = ifelse(SG_UF == 33,1,0),
                              SG_35 = ifelse(SG_UF == 35,1,0),
                              SG_41 = ifelse(SG_UF == 41,1,0),
                              SG_42 = ifelse(SG_UF == 42,1,0),
                              SG_43 = ifelse(SG_UF == 43,1,0),
                              SG_50 = ifelse(SG_UF == 50,1,0),
                              SG_51 = ifelse(SG_UF == 51,1,0),
                              SG_52 = ifelse(SG_UF == 52,1,0),
                              SG_53 = ifelse(SG_UF == 53,1,0)
                            
)

df_denv <- df_denv %>% select(!SG_UF)
df_chik <- df_chik %>% select(!SG_UF)

df_denv['CHIK'] <- 0
df_chik['CHIK'] <- 1

df_denv[is.na(df_denv)] <- 2
df_chik[is.na(df_chik)] <- 2

df_denv_lab <- df_denv %>% filter(CRITERIO == 1)
df_chik_lab <- df_chik %>% filter(CRITERIO == 1)

df_denv_epi <- df_denv %>% filter(CRITERIO == 2)
df_chik_epi <- df_chik %>% filter(CRITERIO == 2)

# Train and test split
df_total <- rbind(df_denv_lab,df_chik_lab)
df_total <- df_total %>% select(!CRITERIO)
df_total[df_total == 2] <- 0
df_total[df_total == 2] <- 0
df_total['n'] <- df_total$FEBRE + df_total$MIALGIA + 
  df_total$CEFALEIA + df_total$EXANTEMA + df_total$VOMITO + df_total$NAUSEA + 
  df_total$DOR_COSTAS + df_total$CONJUNTVIT + df_total$ARTRITE + 
  df_total$ARTRALGIA + df_total$PETEQUIA_N + df_total$LEUCOPENIA + df_total$LACO + df_total$DOR_RETRO

df_total$row <- 1:nrow(df_total)
df_train <- stratified(df_total, c('CHIK'), 0.7)
df_test <- df_total[!(df_total$row %in% df_train$row),]

# Getting the relative weights for the logistic regression
dim(df_denv_lab)[1]/dim(df_chik_lab)[1]
dim(df_denv_epi)[1]/dim(df_chik_epi)[1]
dim(df_denv)[1]/dim(df_chik)[1]

# Eliminating the row index
df_train <- df_train %>% select(!row)
df_train <- df_train %>% mutate(weight = ifelse(CHIK == 1, 1, 1/30))
df_test <- df_test %>% select(!row)

# Doing the weighted logistic regression
model <- glm(CHIK ~ FEBRE + MIALGIA + CEFALEIA + EXANTEMA + VOMITO + NAUSEA + DOR_COSTAS + CONJUNTVIT + ARTRITE + ARTRALGIA + PETEQUIA_N + LEUCOPENIA + LACO + DOR_RETRO,
             family=binomial(link='logit'), data = df_train, weight = weight)
model2 <- glm(CHIK ~ SG_11 + SG_12 + SG_13 + SG_14 + SG_15 + SG_16+ SG_17 + SG_21 + SG_22 + SG_23 + SG_24 + SG_25 + SG_26 + SG_27 + SG_28 + SG_29 + SG_31 + SG_32 + SG_33 + SG_35 + SG_41 + SG_42 +
              SG_43 + SG_50 + SG_51 + SG_52  + SG_53 + FEBRE + MIALGIA + CEFALEIA + EXANTEMA + VOMITO + NAUSEA + DOR_COSTAS + ARTRITE + ARTRALGIA + PETEQUIA_N + LEUCOPENIA + DOR_RETRO,
             family=binomial(link='logit'), data = df_train, weight = weight)
model3 <- glm(CHIK ~ NU_IDADE_N + FEBRE + MIALGIA + CEFALEIA + EXANTEMA + VOMITO + NAUSEA + DOR_COSTAS + ARTRITE + ARTRALGIA + PETEQUIA_N + LEUCOPENIA + DOR_RETRO,
              family=binomial(link='logit'), data = df_train, weight = weight)
model4 <- glm(CHIK ~ SG_11 + SG_12 + SG_13 + SG_14 + SG_15 + SG_16+ SG_17 + SG_21 + SG_22 + SG_23 + SG_24 + SG_25 + SG_26 + SG_27 + SG_28 + SG_29 + SG_31 + SG_32 + SG_33 + SG_35 + SG_41 + SG_42 + 
              SG_43 + SG_50 + SG_51 + SG_52  + SG_53 + NU_IDADE_N + FEBRE + MIALGIA + CEFALEIA + EXANTEMA + VOMITO + NAUSEA + DOR_COSTAS + ARTRITE + ARTRALGIA + PETEQUIA_N + LEUCOPENIA + DOR_RETRO,
              family=binomial(link='logit'), data = df_train, weight = weight)

# summary
summary(model)
summary(model2)
summary(model3)
summary(model4)

# model selection
model$aic
model2$aic
model3$aic
model4$aic

# coefficients
coef(model4)

# odds ratio
exp(coef(model4))


# confidence intervals
exp(confint(model))

# summary from the plots
tab_model(model,
          show.ci = FALSE, # remove CI
          show.aic = TRUE, # display AIC
          p.style = "numeric_stars" # display p-values and stars
)

tab_model(model4,
          show.ci = FALSE, # remove CI
          show.aic = TRUE, # display AIC
          p.style = "numeric_stars" # display p-values and stars
)


# calculating the auc and roc curves for the train data
res1 <- roc(CHIK ~ fitted(model),
           data = df_train
)

res2 <- roc(CHIK ~ fitted(model2),
            data = df_train
)

res3 <- roc(CHIK ~ fitted(model3),
            data = df_train
)

res4 <- roc(CHIK ~ fitted(model4),
            data = df_train
)


res1$auc
res2$auc
res3$auc
res4$auc



plot(res1)
lines(res1, col = 1)
lines(res2, col = 2)
lines(res3, col = 3)
lines(res4, col = 4)
text(locator(), labels = c('SINT','SINT + UF', 'SINT + IDADE + UF'))

