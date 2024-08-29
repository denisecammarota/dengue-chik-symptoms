library(geobr)

pop <- read.csv2('pop_estados.csv')


# Plotting chikungunya incidence total ########################################

## laboratorial ##################################################

map_br <- read_state()
map_aux <- map_br %>% left_join(pop, by = join_by(code_state==SG_UF))

df_chik_lab_resumo <- df_chik_lab %>% group_by(SG_UF) %>% summarise(n = n())
df_chik_lab_resumo
map_aux <- map_aux %>% left_join(df_chik_lab_resumo, by = join_by(code_state == SG_UF))
map_aux['loginc'] <- ((10**5)*(map_aux$n/map_aux$pop))


ggplot(data = map_aux) +
  geom_sf(aes(fill = loginc)) +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 10, base_family = 'Roboto') + theme(plot.margin = margin(6, 12,6,6,"pt")) +
  theme_map() + labs(fill = "Incidence per \n100.000 inhabitants") + ggtitle('Chikungunya - Laboratorial')

## epidemiological original ######################################

map_br <- read_state()
map_aux <- map_br %>% left_join(pop, by = join_by(code_state==SG_UF))

df_chik_epi_resumo <- df_chik_epi %>% group_by(SG_UF) %>% summarise(n = n())
df_chik_epi_resumo
map_aux <- map_aux %>% left_join(df_chik_epi_resumo, by = join_by(code_state == SG_UF))
map_aux['loginc'] <- (10**5)*(map_aux$n/map_aux$pop)


ggplot(data = map_aux) +
  geom_sf(aes(fill = loginc)) +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 10, base_family = 'Roboto') + theme(plot.margin = margin(6, 12,6,6,"pt")) +
  theme_map() + labs(fill = "Incidence per \n100.000 inhabitants") + ggtitle('Chikungunya - Epidemiological')


## epidemiological estimated #####################################
sg_list <- sort(unique(df_chik_lab$SG_UF))
case_list <- c(1567, 1535, 41, 2, 1427, 2145, 1891,
               5216,4364,4125,3559,7051,4579,1967,371,77992,
               667271,44,55649,21367,
               1,0,0,
               3565,21823,102500,1295)

df_chik_epi_resumo_est <- data.frame(SG_UF = sg_list, n = case_list)

map_br <- read_state()
map_aux <- map_br %>% left_join(pop, by = join_by(code_state==SG_UF))
map_aux <- map_aux %>% left_join(df_chik_epi_resumo_est, by = join_by(code_state == SG_UF))
map_aux['loginc'] <- log((10**5)*(map_aux$n/map_aux$pop))


ggplot(data = map_aux) +
  geom_sf(aes(fill = loginc)) +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 10, base_family = 'Roboto') + theme(plot.margin = margin(6, 12,6,6,"pt")) +
  theme_map() + labs(fill = "Incidence per \n100.000 inhabitants") + ggtitle('Chikungunya - Estimated Epidemiological')

## total chikungunya incidence ####################################
df_chik_epi_resumo <- df_chik_epi_resumo %>% ungroup()
df_chik_lab_resumo <- df_chik_lab_resumo %>% ungroup()
df_total <- df_chik_lab_resumo %>% left_join(df_chik_epi_resumo, by = join_by(SG_UF))
df_total[is.na(df_total)] <- 0
df_total['n'] <- df_total['n.x'] + df_total['n.y']

map_br <- read_state()
map_aux <- map_br %>% left_join(pop, by = join_by(code_state==SG_UF))
map_aux <- map_aux %>% left_join(df_total, by = join_by(code_state == SG_UF))
map_aux['loginc'] <- (10**5)*(map_aux$n/map_aux$pop)

ggplot(data = map_aux) +
  geom_sf(aes(fill = loginc)) +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 10, base_family = 'Roboto') + theme(plot.margin = margin(6, 12,6,6,"pt")) +
  theme_map() + labs(fill = "Incidence per \n100.000 inhabitants") + ggtitle('Chikungunya')

## estimated chikungunya incidence ####################################
df_chik_epi_resumo_est <- df_chik_epi_resumo_est %>% ungroup()
df_total <- df_chik_lab_resumo %>% left_join(df_chik_epi_resumo_est, by = join_by(SG_UF))
df_total[is.na(df_total)] <- 0
df_total['n'] <- df_total['n.x'] + df_total['n.y']

map_br <- read_state()
map_aux <- map_br %>% left_join(pop, by = join_by(code_state==SG_UF))
map_aux <- map_aux %>% left_join(df_total, by = join_by(code_state == SG_UF))
map_aux['loginc'] <- (10**5)*(map_aux$n/map_aux$pop)

ggplot(data = map_aux) +
  geom_sf(aes(fill = loginc)) +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 10, base_family = 'Roboto') + theme(plot.margin = margin(6, 12,6,6,"pt")) +
  theme_map() + labs(fill = "Incidence per \n100.000 inhabitants") + ggtitle('Chikungunya - Estimated')


df_total



