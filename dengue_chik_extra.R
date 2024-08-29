## Contando os casos con pelo menos um sintoma

# Processando total 

# casos total dengue
df %>% nrows()

# casos total chik
df_chik %>% nrows()


# Fazendo o resto para tabular por sintomas

df <- df %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                    "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                    "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO", "CRITERIO") %>%
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
                              "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO", "CRITERIO") %>%
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
df_total <- df_total %>% select(doenca, CRITERIO, total_sintomas)

df_total <- df_total %>% group_by(doenca, CRITERIO, total_sintomas) %>% summarise(n = n())
