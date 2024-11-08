library(geobr)
library(tidyverse)
library(sf)

municipalities <- read_municipality(year = 2020)

# Calculate centroids
municipalities <- municipalities %>%
  mutate(centroid = st_centroid(geom)) %>%
  mutate(lat = st_coordinates(centroid)[,2],
         long = st_coordinates(centroid)[,1])
municipalities <- municipalities %>% mutate(ID_MN_RESI = substr(code_muni,1,6))
municipalities <- municipalities %>% select(ID_MN_RESI, lat, long)
municipalities <- municipalities %>% ungroup()
municipalities <- municipalities %>% select(!geom)
municipalities <- municipalities %>%  st_set_geometry(NULL)
save(municipalities, file = 'latlong.RData')
