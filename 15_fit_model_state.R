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
sg_num <- 35

# Reading data from dengue and chikungunya and separating 
df_denv <- read.csv('denv_2024_processed.csv')
df_chik <- read.csv('chik_2024_processed.csv')

#df_denv <- df_denv %>% select(!SG_UF)
#df_chik <- df_chik %>% select(!SG_UF)

# Filtering the state in question
df_denv <- df_denv %>% filter(SG_UF == sg_num)
df_chik <- df_chik %>% filter(SG_UF == sg_num)

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

df_total$row <- 1:nrow(df_total)
df_train <- stratified(df_total, c('CHIK','SG_UF'), 0.7)
df_test <- df_total[!(df_total$row %in% df_train$row),]

df_train <- df_train %>% select(!SG_UF)
df_test <- df_test %>% select(!SG_UF)

# Eliminating the row index
df_train <- df_train %>% select(!row)
df_test <- df_test %>% select(!row)

# Train the best and most simple model

model <- glm(CHIK ~  FEBRE + MIALGIA + CEFALEIA + EXANTEMA + VOMITO + NAUSEA + DOR_COSTAS + ARTRITE + ARTRALGIA + PETEQUIA_N + LEUCOPENIA + DOR_RETRO,
             family=binomial(link='logit'), data = df_train)

# Summary model
summary(model)

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

# Using optimal theshold 
# Calculating sensitivity and specificity on test data and train data
tab_test <- table(df_test$CHIK, ifelse(predicted_prob >= best_threshold, 1, 0))
tab_train <- table(df_train$CHIK, ifelse(predicted_prob_train >= best_threshold, 1, 0))

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
df_epi_pred <- ifelse(predict(model, newdata = df_epi, type = 'response') >= best_threshold, 1, 0)
df_epi <- cbind(df_epi, df_epi_pred)
df_epi <- data.frame(df_epi)

df_epi %>% group_by(SG_UF, df_epi_pred) %>% summarise(n = n())
table(df_epi$CHIK, df_epi$df_epi_pred)
table(df_epi$CHIK, df_epi$SG_UF)
table(df_epi$df_epi_pred) # 1.892.520 denv and 991.338 chik
a <- df_epi %>% group_by(SG_UF, CHIK, df_epi_pred) %>% summarise(n = n())

# Calculating errors - country level predictions 
hist_pred <- data.frame(p = predict(model, newdata = df_epi, type = 'response'))

ggplot(hist_pred, aes(x=p)) +  geom_histogram(aes(y = ..density..), colour = 1, fill = "red", bins = 20) + 
  theme_bw() + xlab('Predicted Probability') + ylab('Probability Density of CHIK') +
  geom_vline(xintercept = best_threshold) + ggtitle('SP') +
  theme_bw()

phist <- hist(hist_pred$p, freq = FALSE, breaks = 30)

n_chik <- list()

for(i in seq(1,100,1)){
  samplesize <- 903984
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

