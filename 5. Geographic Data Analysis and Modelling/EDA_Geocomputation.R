library(raster)
require(raster)
r <- raster("30n000e_20101117_gmted_mea300.tif")
class(r)
plot(r)

library(sf)
aoi <- st_read("hw5/GRC_ADM2/GRC_ADM2.shp")
st_geometry_type(aoi)
ggplot() + 
  geom_sf(data = aoi, size = 0.5, color = "black") + 
  ggtitle("Greece") + 
  coord_sf()

help("raster-package")

# 1. Create new raster file for Greece
aoi = st_transform(aoi, projection(r))
class(aoi)
r_cropped = crop(r, aoi)
r_masked = mask(r, aoi)
class(r_masked)
plot(r_masked)

# 2a. Find mean & sd for greek capitals
poleis <- st_read("hw5/poleis/poleis.shp")
st_geometry_type(poleis)
ggplot() + 
  geom_sf(data = poleis, size = 1, color = "black") + 
  ggtitle("Greek Capitals") + 
  coord_sf()

# 2b. Plot map of Greece based on city elevation
poleis$elevation = raster::extract(r, poleis)
# poleis$elevation = raster::extract(r, poleis, buffer = 1000)  # selected area of 1000 meters around point
ggplot() + 
  geom_sf(data = aoi, size = 0.5, color = "black") + 
  geom_sf(data = poleis["elevation"], size = log(poleis$elevation), color = "black") + 
  ggtitle("Greek Capitals") + 
  coord_sf()

# 3a. Find mean & sd for every municipality
library(dplyr)
aoi_r_values = raster::extract(x = r, y = aoi, df = TRUE)
aoi_r_values = aoi_r_values %>% 
  rename(
    r = X30n000e_20101117_gmted_mea300
  )

grouped_aoi = group_by(aoi_r_values, ID) %>% 
  summarize_at(vars(r), list(~mean(.), ~sd(.)))

# 3b. Choropleth maps
library(tmap)   
aoi_elevation = aoi
aoi_elevation$mean_elevation = grouped_aoi$mean
aoi_elevation$sd_elevation = grouped_aoi$sd
(aoi)
(aoi_elevation)
tm_shape(aoi_elevation) + tm_polygons(col = "mean_elevation")
tm_shape(aoi_elevation) + tm_polygons(col = "sd_elevation")

#  4. Find top 10 municipalities for mean & sd values
mean_sort = aoi_elevation[order(-aoi_elevation$mean_elevation),][1:10,]
(mean_sort$NAME)
sd_sort = aoi_elevation[order(-aoi_elevation$sd_elevation),][1:10,]
(sd_sort$NAME)

# 5a. Find elevation for places.shp points
places <- st_read("hw5/places/places.shp")
ggplot() + 
  geom_sf(data = places, size = 0.5, color = "black") + 
  ggtitle("Greek Locations") + 
  coord_sf()
places$elevation = raster::extract(r, places)
places_sorted = places[order(-places$elevation),]

# 5b. Generate map of places where elevation is greater than 1500
places_sorted <- filter(places_sorted, elevation > 1500)
ggplot() + 
  geom_sf(data = places_sorted, size = 0.5, color = "black") + 
  ggtitle("Greek Locations with over 1500m elevation") + 
  coord_sf()
pal = colorNumeric("RdYlBu", domain = places_sorted$population)
leaflet(data = places_sorted) %>% 
  addTiles() %>%
  addCircles(col = ~pal(population), opacity = 1) %>% 
  addPolygons(data = aoi, fill = FALSE , opacity = 0.1) %>% 
  addLegend(pal = pal, values = ~elevation)

# 6. Reclassify original raster
rcl = matrix(c(0, 500, 1, 500, 1000, 2, 1000, 1500, 3, 1500, 2000, 4, 2000, 2500, 5, 2500, 3000, 6), ncol = 3, byrow = TRUE)
rc <- reclassify(r_masked, rcl)
plot(rc)

