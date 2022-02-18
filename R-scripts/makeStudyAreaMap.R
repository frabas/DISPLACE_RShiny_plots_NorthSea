library(rnaturalearth)
library(sf)
library(ggplot2) # with support for geom_sf

# all countries at scale 10m
#ctrys <- ne_countries(scale = 10, type = "countries", returnclass = "sf")
# unable to install....so,
# replaced by:
# a quick map to color
library(rgdal);  library(raster); ctrys <- readOGR(file.path("D:","FBA","BENTHIS_2020", "FAO_AREAS", "FAO_AREAS.shp"))
ctrys <- st_as_sf(ctrys) # convert to sf type


crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"



# the bouding box polygon in long/lat projection, i.e. axis-aligned
studyarea <- st_sfc(
  st_polygon(list(cbind(
    c(-5, 11, 11, -5, -5), # x-coordinates (longitudes) of points A,B,C,D
    c(50, 50, 62, 62, 50)     # y-coordinates (latitudes) of points A,B,C,D
  ))),
  crs = crsLONGLAT)

bb <- st_sfc(
  st_polygon(list(cbind(
    c(-6, 12, 12, -6, -6), # x-coordinates (longitudes) of points A,B,C,D
    c(49, 49, 63, 63, 49)     # y-coordinates (latitudes) of points A,B,C,D
  ))),
  crs = crsLONGLAT)


# now in in LAEA projection
laeabb <- st_transform(bb, crs = crsLAEA)

# the extent of the bounding box in the new projection
b <- st_bbox(laeabb)
b

gg2 <- ggplot(data =ctrys) +
  geom_sf(fill = "grey", colour = "black") +
  geom_sf(data = studyarea, fill = "#12345678") +
  coord_sf(crs = crsLAEA, xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_bw()

setwd(file.path("D:","FBA","DISPLACE_RShiny_plots_NorthSea","www"))
ggsave(gg2, filename = "studyAreaMap.png", pointsize = 30)
