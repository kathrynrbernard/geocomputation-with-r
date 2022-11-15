# https://geocompr.robinlovelace.net/spatial-operations.html


# Load Packages -----------------------------------------------------------
library(sf)
library(terra)
library(dplyr)
library(spData)


# Read in Data ------------------------------------------------------------
elev = rast(system.file("raster/elev.tif", package = "spData"))
grain = rast(system.file("raster/grain.tif", package = "spData"))


# Spatial Operations on Vector Data ---------------------------------------
## Spatial Subsetting
# using nz and nz_height datasets from spData package
canterbury = nz |> filter(Name == "Canterbury")
# subsetting syntax: x[y, op]; default op = intersects
canterbury_height = nz_height[canterbury, ]

# disjoint = opposite of intersect
nz_height[canterbury, op = st_disjoint]

# sgbp - sparse geometry binary predicate
sel_sgbp = st_intersects(x = nz_height, y = canterbury)
class(sel_sgbp)
sel_logical = lengths(sel_sgbp) > 0
canterbury_height2 = nz_height[sel_logical, ]

# st_filter
canterbury_height3 = nz_height |>
  st_filter(y = canterbury, .predicate = st_intersects)

## Topological Relations
# binary topological relationships: logical statements about spatial relationships between 2 objects
# binary predicates: sf functions for testing different types of topological relationships

# create polygon
polygon_matrix = cbind(
  x = c(0, 0, 1, 1,   0),
  y = c(0, 1, 1, 0.5, 0)
)
polygon_sfc = st_sfc(st_polygon(list(polygon_matrix)))

# create line
line_sfc = st_sfc(st_linestring(cbind(
  x = c(0.4, 1),
  y = c(0.2, 0.5)
)))

# create points
point_df = data.frame(
  x = c(0.2, 0.7, 0.4),
  y = c(0.1, 0.2, 0.8)
)
point_sf = st_as_sf(point_df, coords = c("x", "y"))

# which points intersect with the polygon?
st_intersects(point_sf, polygon_sfc)
st_intersects(point_sf, polygon_sfc, sparse = FALSE)

# other types of relationships
st_within(point_sf, polygon_sfc)
st_touches(point_sf, polygon_sfc) # touches border
st_disjoint(point_sf, polygon_sfc, sparse = FALSE)[, 1] # [,1] turns result into a vector
st_is_within_distance(point_sf, polygon_sfc, dist = 0.2, sparse = FALSE)[, 1]

## DE-9IM Strings
# Dimensionally Extended 9-Intersection Model
# used to create custom biary spatial predicates

## Spatial Joining
# joining adds new columns to the target object (x) from a source object (y)

# imagine you have ten points randomly distributed across the Earth’s surface and you ask, 
# for the points that are on land, which countries are they in?
set.seed(2018) # set seed for reproducibility
(bb = st_bbox(world)) # the world's bounds
random_df = data.frame(
  x = runif(n = 10, min = bb[1], max = bb[3]),
  y = runif(n = 10, min = bb[2], max = bb[4])
)
random_points = random_df |> 
  st_as_sf(coords = c("x", "y")) |> # set coordinates
  st_set_crs("EPSG:4326") # set geographic CRS
world_random = world[random_points, ]
nrow(world_random)
random_joined = st_join(random_points, world["name_long"]) # default is left join
plot(random_joined)

## Non-Overlapping Joins
# used when two geographic datasets don't touch but have a strong geographic relationship
plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")
any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))
sel = st_is_within_distance(cycle_hire, cycle_hire_osm, dist = 20)
summary(lengths(sel) > 0)
z = st_join(cycle_hire, cycle_hire_osm, join = st_is_within_distance, dist = 20)
nrow(cycle_hire)
nrow(z)

# the number of rows in the joined result is greater than the target.
# This is because some cycle hire stations in cycle_hire have multiple matches in cycle_hire_osm
z = z |> 
  group_by(id) |> 
  summarize(capacity = mean(capacity))
nrow(z) == nrow(cycle_hire)

plot(cycle_hire_osm["capacity"])
plot(z["capacity"])

## Spatial Aggregation
nz_agg = aggregate(x = nz_height, by = nz, FUN = mean)
nz_agg2 = st_join(x = nz, y = nz_height) |>
  group_by(Name) |>
  summarize(elevation = mean(elevation, na.rm = TRUE))

## Joining Incongruent Layers
# an aggregating object (y) is congruent with the target object (x) if they have shared borders

# incongruent aggregating objects do not share common borders with the target
# incongruence is a problem: aggregating the centroid of each subzone won't be accurate

# areal interpolation transfers values from one set of areal units to another (range of algorithms)

# area weighted spatial interpolation: simplest method
# it transfers values from the incongruent object to a new column in the target in proportion with the
# area of overlap - larger spaital intersection = larger corresponding value

iv = incongruent["value"] # keep only the values to be transferred
agg_aw = st_interpolate_aw(iv, aggregating_zones, extensive = TRUE)
agg_aw$value

## Distance Relations
# topological relations are binary - features either intersect or don't
# distance relations are continuous

nz_highest = nz_height |> slice_max(n = 1, order_by = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_highest, canterbury_centroid)

# st_distance can return distance matrices between all combinations of features in x and y
co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)
plot(st_geometry(co)[2])
plot(st_geometry(nz_height)[2:3], add = TRUE)


# Spatial Operations on Raster Data ---------------------------------------

## Spatial Subsetting
# subset with cell numbers
id = cellFromXY(elev, xy = matrix(c(0.1, 0.1), ncol = 2))
elev[id]
# the same as
terra::extract(elev, matrix(c(0.1, 0.1), ncol = 2))

# subset with another raster
clip = rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45,
            resolution = 0.3, vals = rep(1, 9))
elev[clip]
# we can also use extract
terra::extract(elev, ext(clip))

# returns the first two cells of elev as a raster object 
elev[1:2, drop = FALSE]    # spatial subsetting with cell IDs

# raster masking
rmask = elev
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)
# spatial subsetting
elev[rmask, drop = FALSE]  # with [ operator
mask(elev, rmask)  # with mask()
elev[elev < 20] = NA

## Map Algebra
# map algebra: operations that modify or summarize raster cell values with reference to other cells
# local: per-cell operations
# focal: neighborhood
# zonal: similar to focal but surrounding pixel grid can have irregular shape/size
# global: per-raster operations

## Local Operations
# raster algebra
elev + elev
elev^2
log(elev)
elev > 5

# classification
# reclassification matrix:
# first column = lower and the second column = upper end of the class
# third column = the new value for the specified ranges in column one and two
rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
rcl
recl = classify(elev, rcl = rcl)

# other raster operations - app(), tapp(), lapp()
# app(): applies a function to each cell of a raster and is used to summarize
# (e.g., calculating the sum) the values of multiple layers into one layer

# tapp(): select a subset of layers to perform the operation on

# lapp(): apply a function to each cell using layers as arguments

# calculating NDVI on multispectral satellite image
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)
ndvi_fun = function(nir, red){
  (nir - red) / (nir + red)
}
ndvi_rast = lapp(multi_rast[[c(4, 3)]], fun = ndvi_fun)

## Focal Operations
# take into account a central (focal) cell and its neighbors
# applies an aggregation to all cells in the neighborhood and uses the output as the new value
# for the focal cell
# returns a raster object
r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)
# w = weights (shape of moving window)

## Zonal Operations
# apply an aggregation function using zonal filters (defined by a second raster)
# not necessarily neighborhoods
# returns a summary table grouped by zone
z = zonal(elev, grain, fun = "mean")
z

## Global Operations and Distances
# entire raster represents a single zone
# descriptive stats like min/max for entire raster

# also useful for computing distance and weight rasters
# distance: calculate distance from each cell to target cell
# weight the distance with elevation

## Map Algebra Counterparts in Vector Processing
# computing distance raster while only considering a maximum distance = buffer
# reclassifying raster data = dissolving vector data

## Merging Rasters
aut = geodata::elevation_30s(country = "AUT", path = tempdir())
ch = geodata::elevation_30s(country = "CHE", path = tempdir())
aut_ch = merge(aut, ch)

# Exercises ---------------------------------------------------------------

# E1.
canterbury = nz |> filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]
nrow(canterbury_height)

# E2.


# E3.
# desired output: table with row for each region and column for region name and number of points
heights <- matrix(ncol = 2, nrow = nrow(nz))
heights <- data.frame(heights)
colnames(heights) <- c("Region", "Sum")
for(i in 1:nrow(nz)){
  row <- nz[i, ]
  row_height <- nz_height[row, ]
  heights[i, "Region"] <- row$Name
  heights[i, "Sum"] <- nrow(row_height)
}

heights %>% arrange(desc(Sum))

# E4.
colorado <- filter(us_states, NAME == "Colorado")
plot(st_geometry(us_states))
plot(st_geometry(colorado), col = "red", add = TRUE)

# Create a new object representing all the states that geographically intersect with Colorado
# and plot the result
intersection <- us_states[st_intersects(us_states, colorado, sparse = FALSE), ]
plot(st_geometry(intersection[!intersection$NAME == "Colorado", ]), col = "yellow", add = TRUE)

# E5.
dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))
# reclassify the elevation in three classes: low (<300), medium and high (>500)
recl_mat <- matrix(c(0, 300, 1,
                     300, 500, 2,
                     500, 9999, 3),
                   ncol = 3, byrow = TRUE)
dem_class <- classify(dem, rcl= recl_mat)

ndvi = rast(system.file("raster/ndvi.tif", package = "spDataLarge"))
# compute the mean NDVI and the mean elevation for each altitudinal class
zonal(rast(ndvi), dem_class, fun = "mean")

# E6.
# Apply a line detection filter to rast(system.file("ex/logo.tif", package = "terra")).
# Plot the result. Hint: Read ?terra::focal()

data <- rast(system.file("ex/logo.tif", package = "terra"))
plot(data)
r_focal = focal(data, w = matrix(1, nrow = 3, ncol = 3), fun = min)
plot(r_focal)

# E7.
image_file <- system.file("raster/landsat.tif", package = "spDataLarge")
image <- rast(image_file)
# Calculate the Normalized Difference Water Index (NDWI; (green - nir)/(green + nir))

ndwi_fun <- function(green, nir){
  (green - nir) / (green + nir)
}
ndwi_rast = lapp(image[[c(1, 4)]], fun = ndwi_fun)
plot(ndwi_rast)

# calculate a correlation between NDVI and NDWI for this area
# (hint: you can use the layerCor() function
layerCor(x = ndvi_rast, fun = "pearson", w= ndwi_rast)

# E8.
# A StackOverflow post shows how to compute distances to the nearest coastline using 
# raster::distance(). Try to do something similar but with terra::distance(): 
# retrieve a digital elevation model of Spain, and compute a raster which represents 
# distances to the coast across the country (hint: use geodata::elevation_30s()). 
# Convert the resulting distances from meters to kilometers.
# Note: it may be wise to increase the cell size of the input raster to reduce compute time

# E9.
