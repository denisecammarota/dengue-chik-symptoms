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
df_denv <- read.csv('denv_2024_processed.csv')
df_chik <- read.csv('chik_2024_processed.csv')

#df_denv <- df_denv %>% select(!SG_UF)
#df_chik <- df_chik %>% select(!SG_UF)

df_denv['CHIK'] <- 0
df_chik['CHIK'] <- 1

df_denv[is.na(df_denv)] <- 2
df_chik[is.na(df_chik)] <- 2

df_denv_lab <- df_denv %>% filter(CRITERIO == 1)
df_chik_lab <- df_chik %>% filter(CRITERIO == 1)

df_denv_epi <- df_denv %>% filter(CRITERIO == 2)
df_chik_epi <- df_chik %>% filter(CRITERIO == 2)

# Getting all data together
df_total <- rbind(df_denv_lab,df_chik_lab)

# Creating faixas etarias
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

# Eliminating criteria, changing symptoms to 1 or 0
df_total <- df_total %>% select(!CRITERIO)
df_total[df_total == 2] <- 0
df_total[df_total == 2] <- 0

# Turning into factors
#cols <- colnames(df_total)[colnames(df_total) != 'CHIK']
cols <- c('SG_UF')
df_total[cols] <- lapply(df_total[cols], as.factor)


# Train and test split
df_total$row <- 1:nrow(df_total)

df_train <- stratified(df_total, c('CHIK'), 0.7)
df_test <- df_total[!(df_total$row %in% df_train$row),]

# Eliminating the row index
df_train <- df_train %>% select(!row)
df_test <- df_test %>% select(!row)

# Train the best and most simple model

model <- glm(CHIK ~ SG_UF + + FEBRE + MIALGIA + CEFALEIA + EXANTEMA + VOMITO + NAUSEA + DOR_COSTAS + ARTRITE + ARTRALGIA + PETEQUIA_N + LEUCOPENIA + DOR_RETRO,
             family=binomial(link='logit'), data = df_train)

# Summary model
summary(model)

# Exponential coefficients
exp(model$coefficients)

# ROC and AUC on train data

res1 <- roc(CHIK ~ fitted(model),
            data = df_train
) 

res1$auc

plot(res1)

# ROC and AUC on test data
predicted_prob <- predict(model, newdata = df_test, type = "response")
predicted_prob_train <- predict(model, newdata = df_train, type = "response")
res2 <- roc(df_test$CHIK, predicted_prob)

res2$auc

plot(res2)
ggroc(res2,legacy.axes = TRUE) + theme_bw()


# Uniting both plots - model generalises well
plot(res1)
lines(res2, col = 2)

# Uniting plots - better format
p <- ggroc(res1,legacy.axes = TRUE) +  geom_abline(intercept = 0, slope = 1,
                                                   color = "darkgrey", linetype = "dashed") +
  xlab('Sensitivity') + ylab('1 - Specificity') + ggtitle(paste0('Train Data AUC: ',round(res1$auc,3))) +
  theme_Publication(base_size = 10, base_family = 'Roboto') + theme(plot.margin = margin(6, 12,6,6,"pt"))

p

q <- ggroc(res2,legacy.axes = TRUE) +  geom_abline(intercept = 0, slope = 1,
                                                   color = "darkgrey", linetype = "dashed") +
  xlab('Sensitivity') + ylab('1 - Specificity') + ggtitle(paste0('Test Data AUC: ',round(res2$auc,3))) +
  theme_Publication(base_size = 10, base_family = 'Roboto') + theme(plot.margin = margin(6, 12,6,6,"pt"))

q

grid.arrange(p,q,nrow=1)

# Choosing the optimal threshold

## Using the testing data 
coords <- coords(res2, "best", best.method = "closest.topleft")
best_threshold <- coords$threshold # 0.56

cat("Optimal threshold based on ROC curve:", best_threshold, "\n")

## Using the training data
coords <- coords(res1, "best", best.method = "closest.topleft")
best_threshold <- coords$threshold

cat("Optimal threshold based on ROC curve:", best_threshold, "\n") # 0.56

# Using the basic threshold of 0.5
# Calculating sensitivity and specificity on test data and train data
tab_test <- table(df_test$CHIK, ifelse(predicted_prob >= 0.5, 1, 0))
tab_train <- table(df_train$CHIK, ifelse(predicted_prob_train >= 0.5, 1, 0))

# sensivity TP/(TP + FN)
tab_test[2, 2]/(tab_test[2, 2] + tab_test[2, 1]) # 0.8574673
tab_train[2, 2]/(tab_train[2, 2] + tab_train[2, 1]) # 0.8586115

# specificity TN/(TN + FP)
tab_test[1, 1]/(tab_test[1, 1] + tab_test[1, 2]) # 0.7663109
tab_train[1, 1]/(tab_train[1, 1] + tab_train[1, 2]) # 0.7659559


# accuracy
sum(diag(tab_test))/sum(tab_test) # 0.7692536
sum(diag(tab_train))/sum(tab_train) # 0.7689468


# Using optimal theshold of 0.56
# Calculating sensitivity and specificity on test data and train data
tab_test <- table(df_test$CHIK, ifelse(predicted_prob >= 0.04, 1, 0))
tab_train <- table(df_train$CHIK, ifelse(predicted_prob_train >= 0.04, 1, 0))

# sensivity TP/(TP + FN)
tab_test[2, 2]/(tab_test[2, 2] + tab_test[2, 1]) # 0.8186351
tab_train[2, 2]/(tab_train[2, 2] + tab_train[2, 1]) # 0.8196258

# specificity TN/(TN + FP)
tab_test[1, 1]/(tab_test[1, 1] + tab_test[1, 2]) # 0.8057719
tab_train[1, 1]/(tab_train[1, 1] + tab_train[1, 2]) # 0.8054217


# accuracy
sum(diag(tab_test))/sum(tab_test) # 0.7692536
sum(diag(tab_train))/sum(tab_train) # 0.7689468


# Calculating the total number of chik and dengue cases (mean)

# real number
dim(df_denv_lab)[1] + dim(df_denv_epi)[1] # 1.803.806 + 2.791.601 = 4.595.407 dengue cases
dim(df_chik_lab)[1] + dim(df_chik_epi)[1] # 60.170 + 92.257 = 152.427 chik cases


# laboratorial test - we take this as ground truth
df_denv_lab %>% group_by(SG_UF) %>% summarise(n = n()) # total 1.803.806
df_chik_lab %>% group_by(SG_UF) %>% summarise(n = n()) # total 60.170

# epidemiological test - only based on this
df_denv_epi %>% group_by(SG_UF) %>% summarise(n = n()) # total 2.791.601
df_chik_epi %>% group_by(SG_UF) %>% summarise(n = n()) # total 92.257

# epidemiological - we correct this, since this is the doubt in the literature
df_denv_epi[df_denv_epi == 2] <- 0
df_chik_epi[df_chik_epi == 2] <- 0
df_epi <- rbind(df_denv_epi, df_chik_epi)
cols <- c('SG_UF')
df_epi[cols] <- lapply(df_epi[cols], as.factor)
df_epi_pred <- ifelse(predict(model, newdata = df_epi, type = 'response') >= best_threshold, 1, 0)
df_epi <- cbind(df_epi, df_epi_pred)
df_epi <- data.frame(df_epi)

df_epi %>% group_by(SG_UF, df_epi_pred) %>% summarise(n = n())
table(df_epi$CHIK, df_epi$df_epi_pred)
table(df_epi$CHIK, df_epi$SG_UF)
table(df_epi$df_epi_pred) # 1.892.520 denv and 991.338 chik 2883858
a <- df_epi %>% group_by(SG_UF, CHIK, df_epi_pred) %>% summarise(n = n())

# Calculating errors - country level predictions 
hist_pred <- data.frame(p = predict(model, newdata = df_epi, type = 'response'))

ggplot(hist_pred, aes(x=p)) +  geom_histogram(aes(y = ..density..), colour = 1, fill = "red", bins = 20) + 
  theme_bw() + xlab('Predicted Probability') + ylab('Probability Density of CHIK') +
  geom_vline(xintercept = best_threshold) + ggtitle('Brazil') +
  theme_Publication(base_size = 10, base_family = 'Roboto') + theme(plot.margin = margin(6, 12,6,6,"pt"))

phist <- hist(hist_pred$p, freq = FALSE, breaks = 30)

n_chik <- list()
best_threshold <- 0.04

for(i in seq(1,10000,1)){
  samplesize <- 2883858
  bins=with(phist,sample(length(mids),samplesize,p=density,replace=TRUE)) # choose a bin
  result=runif(length(bins),phist$breaks[bins],phist$breaks[bins+1]) # sample a uniform in it
  result <- data.frame(r = result)
  result <- result %>% mutate(r = ifelse(r >= best_threshold, 1, 0))
  #n_chik <- append(n_chik, sum(result$r))
  n_chik <- append(n_chik,as.numeric(sum(result$r)))
}

n_chik <- as.numeric(n_chik)
n_chik <- data.frame(n_chik)
mean_data <- mean(n_chik$n_chik)
sd_data <- sd(n_chik$n_chik)
conf_int <- mean_data + c(-1.96, 1.96) * sd_data/sqrt(length(data)) 

ggplot(data = n_chik, aes(x = n_chik)) + 
  geom_histogram(
    fill = "red", color = "black") + 
  geom_vline(xintercept = mean_data, 
             color = "black", linetype = "dashed") + 
  geom_ribbon(aes(ymin = 0, ymax = Inf, 
                  xmin = conf_int[1], 
                  xmax = conf_int[2]), 
              fill = "gray80", alpha = 0.3) +
  xlab("Data") + 
  ylab("Density") + ggtitle('Estimated Chikungunya Cases for Brazil') +
  theme_Publication(base_size = 10, base_family = 'Roboto') + theme(plot.margin = margin(6, 12,6,6,"pt"))

# Calculating errors - state level predictions
sg <- unique(df_epi$SG_UF)

for(i in seq(1,27,1)){
  sg_tmp <- sg[i]
  print(sg_tmp)
  df_epi_tmp <- df_epi %>% filter(SG_UF == sg_tmp)
  sample_size <- df_epi_tmp %>% nrow()
  hist_pred <- data.frame(p = predict(model, newdata = df_epi_tmp, type = 'response'))
  phist <- hist(hist_pred$p, freq = FALSE, breaks = 30)
  n_chik <- list()
  
  for(i in seq(1,10000,1)){
    samplesize <- sample_size
    bins=with(phist,sample(length(mids),samplesize,p=density,replace=TRUE)) # choose a bin
    result=runif(length(bins),phist$breaks[bins],phist$breaks[bins+1]) # sample a uniform in it
    result <- data.frame(r = result)
    result <- result %>% mutate(r = ifelse(r >= best_threshold, 1, 0))
    n_chik <- append(n_chik,as.numeric(sum(result$r)))
  }
  n_chik <- as.numeric(n_chik)
  n_chik <- data.frame(n_chik)
  mean_data <- mean(n_chik$n_chik)
  sd_data <- sd(n_chik$n_chik)
  conf_int <- mean_data + c(-1.96, 1.96) * sd_data/sqrt(length(data)) 
  print(paste0('Mean: ',mean_data))
  print(paste0('CI: ',conf_int))
  print(paste0('Original: ',df_epi_tmp %>% filter(CHIK == 1) %>% nrow()))
  print(paste0('Total Arbo Epi: ', df_epi_tmp %>% nrow()))
  
  p <- ggplot(data = n_chik, aes(x = n_chik)) + 
    geom_histogram(
      fill = "red", color = "black") + 
    geom_vline(xintercept = mean_data, 
               color = "black", linetype = "dashed") + 
    geom_ribbon(aes(ymin = 0, ymax = Inf, 
                    xmin = conf_int[1], 
                    xmax = conf_int[2]), 
                fill = "gray80", alpha = 0.3) + 
    ggtitle(paste0('Histogram for SG_UF =', sg_tmp)) + 
    xlab("Data") + 
    ylab("Density") + theme_bw()
  print(p)
  
}




