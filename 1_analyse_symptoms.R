library(tidyverse)
library(dplyr)
library(read.dbc)
library(ggplot2)
library(igraph)

#df <- read_csv('D:/Code/dengue_2024.csv')
df <- read.dbc('D:/DENGBR24.dbc') # é mais rápido
df_chik <- read.dbc(
  'D:/CHIKBR24.dbc')

# Casos de dengue e de chik ##################################################################

# Confirmados de dengue
table(df$CLASSI_FIN)
print(paste0('Casos de dengue: ', 4605140 + 87030 +  7360))
casos_dengue <- 4605140 + 87030 +  7360

# Confirmados de chik
table(df_chik$CLASSI_FIN)
print(paste0('Casos de chik: ' ,164991))
casos_chik <- 164991


# Filtrando os casos de fato
df <- df %>% filter(CLASSI_FIN %in% c(10,11,12))
df_chik <- df_chik %>% filter(CLASSI_FIN %in% c(13))


# Filtrando os que tem pelo menos um sintoma 
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

table(df$CRITERIO)
# total sem filtrar 4699530
# total 4699509
# laboratorial 1803806
# clinico 2791601
# em inv 104102

table(df_chik$CRITERIO)
# total sem filtrar 164991
# total 164990
# laboratorial  60170
# clinico 92257
# em inv 12563



# Sinais e sintomas #############################################################################
df_tmp <- df
df_chik_tmp <- df_chik


#df_tmp <- df %>% filter(CRITERIO == 2)
#df_chik_tmp <- df_chik %>% filter(CRITERIO == 2)

casos_dengue <- 4699509
casos_chik <- 164990

sintomas <- c("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
              "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
              "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")

df_tmp <- df_tmp %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                            "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                            "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")

df_chik_tmp <- df_chik_tmp %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                                      "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                                      "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")

df_tmp <- df_tmp %>% mutate(doenca = 'Dengue')
df_chik_tmp <- df_chik_tmp %>% mutate(doenca = 'Chikungunya')

df_sint <- rbind(df_tmp,df_chik_tmp)

df_sint <- df_sint %>% pivot_longer(cols = FEBRE:DOR_RETRO,
                                    names_to = 'sintoma', values_to = "Y")

df_sint <- df_sint %>% mutate(Y = ifelse(Y == 1, 1, 0))

df_sint <- df_sint %>% filter(!is.na(Y))

df_sint <- df_sint %>% group_by(doenca, sintoma) %>% summarise(n = sum(Y, na.rm = TRUE))

# eu ja rodei até aqui HOJE AGORA 11/7/24

df_sint <- df_sint %>% mutate(total = case_when(
  doenca == 'Dengue' ~ casos_dengue,
  T ~ casos_chik
))

df_sint <- df_sint %>% mutate(p = 100*n/total)

ggplot(df_sint, aes(doenca, sintoma, fill= p)) + 
  geom_tile() + scale_fill_distiller(palette = "RdPu") +
  geom_text(aes(label = paste0(round(p,2),'%')), color = "black", size = 4)




# Redes de sintomas #####################################################

## Dengue ###############################################################

sintomas <- c("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
              "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
              "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")
df_red_dengue <- df %>% filter(CRITERIO == 1) %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                                                         "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                                                         "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")


red_dengue <- matrix(, nrow = 14, ncol = 14)
diag_dengue <- matrix(, nrow = 1, ncol = 14)

for(i in seq(1,14,1)){
  for(j in seq(1,14,1)){
    if(i == j){
      casos_dengue <- df_red_dengue %>% nrow()
      casos_par <- sum(df_red_dengue[colnames(df_red_dengue)[i]] == 1)
      diag_dengue[1,i] = casos_par/casos_dengue
      red_dengue[i,j] = 0
    }else{
      casos_dengue <- df_red_dengue %>% nrow()
      casos_par <- sum(df_red_dengue[colnames(df_red_dengue)[i]] == 1 & df_red_dengue[colnames(df_red_dengue)[j]] == 1)
      red_dengue[i,j] = casos_par/casos_dengue
    }
  }
}

colnames(red_dengue) <- sintomas
rownames(red_dengue) <- sintomas




## Chikungunya ###################################################################################################

sintomas <- c("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
              "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
              "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")

df_red_chik <- df_chik %>% filter(CRITERIO == 1) %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                                                            "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                                                            "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")


red_chik <- matrix(, nrow = 14, ncol = 14)
diag_chik <- matrix(, nrow = 1, ncol = 14)

for(i in seq(1,14,1)){
  for(j in seq(1,14,1)){
    if(i == j){
      casos_chik <- df_red_chik %>% nrow()
      casos_par <- sum(df_red_chik[colnames(df_red_chik)[i]] == 1)
      diag_chik[1,i] = casos_par/casos_chik
      red_chik[i,j] = 0
    }else{
      casos_chik <- df_red_chik %>% nrow()
      casos_par <- sum(df_red_chik[colnames(df_red_chik)[i]] == 1 & df_red_chik[colnames(df_red_chik)[j]] == 1)
      red_chik[i,j] = casos_par/casos_chik
    }
  }
}

colnames(red_chik) <- sintomas
rownames(red_chik) <- sintomas


# COISAS EXTRAS #############################################################################

# Obitos de dengue e de chik #################################################################
df %>% filter(EVOLUCAO == 2) %>% nrow()

df_chik  %>% filter(EVOLUCAO == 2) %>% nrow()

# Positividade ###############################################################################

## Positividade PCR ##########################################################################

testes_pcr_den <- df %>% filter(RESUL_PCR_ %in% c(1,2,3)) %>% nrow()
testes_pcr_chik <- df_chik %>% filter(RESUL_PCR_ %in% c(1,2,3)) %>% nrow()
testes_pcr <- testes_pcr_den + testes_pcr_chik 

testes_pcr_pos_den <- df %>% filter(RESUL_PCR_ %in% c(1)) %>% nrow()
testes_pcr_pos_chik <- df_chik %>% filter(RESUL_PCR_ %in% c(1)) %>% nrow()
testes_pcr_pos <- testes_pcr_pos_den + testes_pcr_pos_chik 

# positividade para dengue
100*testes_pcr_pos_den/testes_pcr_den

# postividade para chik
100*testes_pcr_pos_chik/testes_pcr_chik

## IGM ######################################################################################

testes_igm_den <- df %>% filter(RESUL_SORO %in% c(1,2,3)) %>% nrow()
testes_igm_chik <- df_chik %>% filter(RES_CHIKS1 %in% c(1,2,3) | RES_CHIKS2 %in% c(1,2,3)) %>% nrow()
testes_igm <- testes_igm_den + testes_igm_chik 


testes_igm_pos_den <- df %>% filter(RESUL_SORO %in% c(1)) %>% nrow()
testes_igm_pos_chik <- df_chik %>% filter(RES_CHIKS1 %in% c(1) | RES_CHIKS2 %in% c(1)) %>% nrow()
testes_igm_pos <- testes_igm_pos_den + testes_igm_pos_chik 

# positividade para dengue
100*testes_igm_pos_den/testes_igm_den

# postividade para chik
100*testes_igm_pos_chik/testes_igm_chik

## Total ####################################################################################
testes_den = testes_igm_den  + testes_pcr_den
testes_chik = testes_igm_chik + testes_pcr_chik

testes_pos_den = testes_igm_pos_den  + testes_pcr_pos_den
testes_pos_chik = testes_igm_pos_chik  + testes_pcr_pos_chik


# positividade para dengue
100*testes_pos_den/testes_den

# postividade para chik
100*testes_pos_chik/testes_chik


# Hospitalizações #########################################################################
table(df$HOSPITALIZ)

table(df_chik$HOSPITALIZ)

# Critério de encerramento dos casos #############################################################
table(df$CRITERIO)
100*1803812/casos_dengue # laboratorial
100*2791615/casos_dengue # clinico epidemiologico
100*104103/casos_dengue # em investigação

table(df_chik$CRITERIO)
100*60171/casos_chik # laboratorial
100*92257/casos_chik # clinico epidemiologico
100*12563/casos_chik # em investigação




# numero de sintomas - isso pode fazer depois

# numero de sinais e sintomas

df <- df %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                    "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                    "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO") %>%
  mutate(doenca = 'Dengue') %>%
  mutate(FEBRE = ifelse(FEBRE == 1,1,0),
         MIALGIA = ifelse(MIALGIA == 1,1,0),
         CEFALEIA = ifelse(CEFALEIA == 1,1,0),
         EXANTEMA = ifelse(EXANTEMA == 1, 1, 0),
         VOMITO = ifelse(VOMITO == 1,1,0),
         NAUSEA = ifelse(NAUSEA == 1,1,0),
         DOR_COSTAS = ifelse(DOR_COSTAS == 1,1,0),
         CONJUNTVIT = ifelse(CONJUNTVIT == 1,1,0),
         ARTRITE = ifelse(ARTRITE == 1,1,0),
         ARTRALGIA = ifelse(ARTRALGIA == 1,1,0),
         PETEQUIA_N = ifelse(PETEQUIA_N == 1,1,0),
         LEUCOPENIA = ifelse(LEUCOPENIA == 1,1,0),
         LACO = ifelse(LACO == 1,1,0),
         DOR_RETRO = ifelse(DOR_RETRO == 1,1,0))

df_chik <- df_chik %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                              "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                              "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO") %>%
  mutate(doenca = 'Chikungunya') %>%
  mutate(FEBRE = ifelse(FEBRE == 1,1,0),
         MIALGIA = ifelse(MIALGIA == 1,1,0),
         CEFALEIA = ifelse(CEFALEIA == 1,1,0),
         EXANTEMA = ifelse(EXANTEMA == 1, 1, 0),
         VOMITO = ifelse(VOMITO == 1,1,0),
         NAUSEA = ifelse(NAUSEA == 1,1,0),
         DOR_COSTAS = ifelse(DOR_COSTAS == 1,1,0),
         CONJUNTVIT = ifelse(CONJUNTVIT == 1,1,0),
         ARTRITE = ifelse(ARTRITE == 1,1,0),
         ARTRALGIA = ifelse(ARTRALGIA == 1,1,0),
         PETEQUIA_N = ifelse(PETEQUIA_N == 1,1,0),
         LEUCOPENIA = ifelse(LEUCOPENIA == 1,1,0),
         LACO = ifelse(LACO == 1,1,0),
         DOR_RETRO = ifelse(DOR_RETRO == 1,1,0))


df_total <- rbind(df, df_chik)

df_total <- df_total %>% mutate(total_sintomas =
                                  rowSums(df_total[, colnames(df_total)[1:14]], na.rm = TRUE))
df_total <- df_total %>% select(doenca, total_sintomas)

df_total <- df_total %>% group_by(doenca, total_sintomas) %>% summarise(n = n())

df_doenca <- df_total %>% group_by(doenca) %>% summarise(n = sum(n))

df_total <- df_total %>% left_join(df_doenca, join_by(doenca == doenca))

df_total <- df_total %>% mutate(p = 100*n.x/n.y)

df_total

ggplot(df_total, aes(doenca, total_sintomas, fill= p)) + 
  geom_tile() + scale_fill_distiller(palette = "RdPu") +
  geom_text(aes(label = paste0(round(p,2),'%')), color = "black", size = 4)
