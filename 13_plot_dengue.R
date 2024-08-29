library(geobr)

pop <- read.csv2('pop_estados.csv')


# Plotting denvungunya incidence total ########################################

## laboratorial ##################################################

map_br <- read_state()
map_aux <- map_br %>% left_join(pop, by = join_by(code_state==SG_UF))

df_denv_lab_resumo <- df_denv_lab %>% group_by(SG_UF) %>% summarise(n = n())
df_denv_lab_resumo
map_aux <- map_aux %>% left_join(df_denv_lab_resumo, by = join_by(code_state == SG_UF))
map_aux['loginc'] <- log(((10**5)*(map_aux$n/map_aux$pop)))


ggplot(data = map_aux) +
  geom_sf(aes(fill = loginc)) +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 10, base_family = 'Roboto') + theme(plot.margin = margin(6, 12,6,6,"pt")) +
  theme_map() + labs(fill = "Incidence per \n100.000 inhabitants") + ggtitle('Dengue - Laboratorial')

## epidemiological original ######################################

map_br <- read_state()
map_aux <- map_br %>% left_join(pop, by = join_by(code_state==SG_UF))

df_denv_epi_resumo <- df_denv_epi %>% group_by(SG_UF) %>% summarise(n = n())
df_denv_epi_resumo
map_aux <- map_aux %>% left_join(df_denv_epi_resumo, by = join_by(code_state == SG_UF))
map_aux['loginc'] <- log((10**5)*(map_aux$n/map_aux$pop))


ggplot(data = map_aux) +
  geom_sf(aes(fill = loginc)) +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 10, base_family = 'Roboto') + theme(plot.margin = margin(6, 12,6,6,"pt")) +
  theme_map() + labs(fill = "Incidence per \n100.000 inhabitants") + ggtitle('Dengue - Epidemiological')

## total Dengue incidence ####################################
df_denv_epi_resumo <- df_denv_epi_resumo %>% ungroup()
df_denv_lab_resumo <- df_denv_lab_resumo %>% ungroup()
df_total <- df_denv_lab_resumo %>% left_join(df_denv_epi_resumo, by = join_by(SG_UF))
df_total[is.na(df_total)] <- 0
df_total['n'] <- df_total['n.x'] + df_total['n.y']

map_br <- read_state()
map_aux <- map_br %>% left_join(pop, by = join_by(code_state==SG_UF))
map_aux <- map_aux %>% left_join(df_total, by = join_by(code_state == SG_UF))
map_aux['loginc'] <- log((10**5)*(map_aux$n/map_aux$pop))

ggplot(data = map_aux) +
  geom_sf(aes(fill = loginc)) +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 10, base_family = 'Roboto') + theme(plot.margin = margin(6, 12,6,6,"pt")) +
  theme_map() + labs(fill = "Incidence per \n100.000 inhabitants") + ggtitle('Dengue')

