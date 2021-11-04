library(sf)
library(tidyverse)

nc = read_sf("data/gis/nc_counties/")
air = read_sf("data/gis/airports//")
hwy = read_sf("data/gis/us_interstates/")


mapview::mapview(nc)




# How many counties in North Carolina are within 5, 10, 20, or 50 km of an interstate highway?

nc_utm  = st_transform(nc, "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")
hwy_utm  = st_transform(hwy, "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")
hwy_nc = st_intersection(hwy_utm, nc_utm)

hwy_nc_buffer = hwy_nc %>%
  st_buffer(10000) %>%
  st_union() %>%
  st_sf()

ggplot() + 
  geom_sf(data=nc_utm) +
  geom_sf(data=hwy_nc, color='red') +
  geom_sf(data=hwy_nc_buffer, fill='red', alpha=0.3)


ct = st_intersects(
  nc_utm,
  hwy_nc_buffer
)


nc %>%
  mutate(
    is_within_10km = map_lgl(ct, ~length(.x) > 0)
  ) %>%
  filter(is_within_10km) %>%
  ggplot() +
  geom_sf(data = nc_utm) +
  geom_sf(fill="blue", alpha=0.5) +
  geom_sf(data=hwy_nc_buffer, fill='red', alpha=0.3)
