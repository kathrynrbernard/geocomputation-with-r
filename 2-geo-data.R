
# https://geocompr.robinlovelace.net/spatial-class.html#spatial-class

# Load Packages -----------------------------------------------------------

library(sf) # classes and functions for vector data
library(terra) # classes and functions for raster data
library(spData) # load geographic data
library(spDataLarge) # load larger geographic data
library(tidyverse)

# Vector Data  -------------------------------------------------------------

# dataset loaded by spData
class(world) 
names(world)

# this is a 'simple feature' (sf) object
# sf objects are dataframes with special geometry (geom) columns

plot(world)
# results in a map for each variable in the dataset

summary(world)
world[1:2, 1:3]

# basic plot
plot(world["pop"])

 # adding layers to plot
world_asia = world[world$continent == "Asia", ]
# st_union combines all Asian countries into a multipolygon
# so they can be plotted as a single continent with no lines for countries
asia = st_union(world_asia) 
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

# plotting country populations
plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
# st_centroid converts polygons to points
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)
# cex: a numerical vector giving the amount by which plotting characters and symbols 
# should be scaled relative to the default.

# expand BB
india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(st_geometry(world_asia), add = TRUE)


# sfg = simple feature geometries
# point, linestring, polygon, multipoint, multilinestring, multipolygon, geometry collection
# sfc = simple feature columns
# - list of sfg objects, usually has CRS info
# sf objects are dataframes with a "spatial extension"


# Raster Data -------------------------------------------------------------

# Packages
# terra focuses on regular raster grids and can handle one or multi-layered rasters
# stars can handle other grid models and can also store raster data cubes
# (many bands, time series, sensor attributes)
# stars is closely related to sf; terra has its own vector data model

# Read in data from spDataLarge packages
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
class(my_rast)
my_rast

# basic plot
plot(my_rast)

# create raster from scratch
new_raster = rast(nrows = 6, ncols = 6, resolution = 0.5, 
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
                  vals = 1:36)
plot(new_raster)

# multi-layer raster
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)
multi_rast
nlyr(multi_rast)
multi_rast3 = subset(multi_rast, 3)
multi_rast4 = subset(multi_rast, "landsat_4")
multi_rast34 = c(multi_rast3, multi_rast4)


# Geographic and projected CRS --------------------------------------------

# geographic coordinate reference systems = lat/long; distance measured in degrees, not meters
# datum: what elipsoid to use; relationship between Cartesian coordinates and location on Earth's surface
# projected coordinate reference system: covert 3D surface into 2D
# some disortions are inevitable - can preserve some but not all of area, direction, distance, and shape
# types of projections: conic, cylindrical, planar (azimuthal


# Units -------------------------------------------------------------------
luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg) # units are m^2
units::set_units(st_area(luxembourg), km^2)

# terra package does not explicitly specify units - need to base it off CRS


# Exercises ---------------------------------------------------------------

# E1.
summary(world$geom)
# geom type = multipolygon

# E2.
# cex: a numerical vector giving the amount by which plotting characters and symbols 
# should be scaled relative to the default.

# E3.
nigeria = world[world$name_long == "Nigeria", ]
world_africa = world[world$continent == "Africa", ]
plot(st_geometry(nigeria), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(st_geometry(world_africa), add = TRUE)
#text(world$name_long)

# E4.
new_raster = rast(nrows = 10, ncols = 10, resolution = 0.5, 
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
                  vals = 1:10)
plot(new_raster)

# E5.
nlcd_filepath = system.file("raster/nlcd.tif", package = "spDataLarge")
nlcd = rast(raster_filepath)
nlcd

# E6.
crs(nlcd)
# WGS84