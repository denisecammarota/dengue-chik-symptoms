df_lab <- df %>% filter(CRITERIO == 1) %>% 
  select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                                                       "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                                                       "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")

df_epi <- df %>% filter(CRITERIO == 2)  %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                                                   "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                                                   "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")


df_chik_lab <- df_chik %>% filter(CRITERIO == 1)  %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                                                             "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                                                             "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")

df_chik_epi <- df_chik %>% filter(CRITERIO == 2)  %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                                                             "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                                                             "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")


# Laboratorial total
df_lab <- rbind(df_lab, df_chik_lab)

# Epidemiologico total
df_epi <- rbind(df_epi, df_chik_epi)


# Totais
total_lab <- dim(df_lab)[1]
total_epi <- dim(df_epi)[1]

df_lab <- df_lab %>% pivot_longer(cols = FEBRE:DOR_RETRO,
                 names_to = 'sintoma', values_to = "Y") %>% mutate(Y = ifelse(Y == 1, 1, 0)) %>%
  filter(!is.na(Y)) %>% group_by(sintoma) %>% summarise(n = sum(Y, na.rm = TRUE))


df_epi <- df_epi %>% pivot_longer(cols = FEBRE:DOR_RETRO,
                                  names_to = 'sintoma', values_to = "Y") %>% mutate(Y = ifelse(Y == 1, 1, 0)) %>%
  filter(!is.na(Y)) %>% group_by(sintoma) %>% summarise(n = sum(Y, na.rm = TRUE))


df_lab <- df_lab %>% mutate(p = 100*(n/total_lab))
df_epi <- df_epi %>% mutate(p = 100*(n/total_epi))

df_lab['tipo'] <- 'Lab'
df_epi['tipo'] <- 'Epi'

df_lab <- df_lab %>% select(sintoma, p, tipo)
df_epi <- df_epi %>% select(sintoma, p, tipo)

df_total <- rbind(df_lab, df_epi)


ggplot(df_total, aes(tipo, sintoma, fill= p)) + 
  geom_tile() + scale_fill_distiller(palette = "RdPu") +
  geom_text(aes(label = paste0(round(p,2),'%')), color = "black", size = 4) +
  theme_bw() + labs(x = 'Criterio', y = 'Sintoma', fill = 'Proportion')
