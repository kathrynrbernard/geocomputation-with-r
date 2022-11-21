# https://geocompr.robinlovelace.net/geometry-operations.html


# Load Packages -----------------------------------------------------------
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)


# Geometric Operations on Vector Data -------------------------------------

## Simplification
# generalizing vector objects for use in smaller scale maps or to reduce the amount of memory consumed
# reducing the vertex count

# simplifying lines
seine_simp = st_simplify(seine, dTolerance = 2000)  # 2000 m
object.size(seine)
object.size(seine_simp)

# simplifying polygons
us_states2163 = st_transform(us_states, "EPSG:2163")
# st_simplify loses the topology and can result in overlaps or gaps in the polygons
us_states_simp1 = st_simplify(us_states2163, dTolerance = 100000)  # 100 km
# ms_simplify overcomes those issues
# proportion of points to retain (0-1; default 0.05)
us_states_simp2 = rmapshaper::ms_simplify(us_states2163, keep = 0.01,
                                          keep_shapes = TRUE)
# smoothr smooths the boundaries of the polygons; doesn't preserve topology
us_states_simp3 = smoothr::smooth(us_states2163, method = 'ksmooth', smoothness = 6)

## Centroids
# different ways to define the geographic center of an object
# geographic centroid: center of mass in a spatial object
nz_centroid = st_centroid(nz)
seine_centroid = st_centroid(seine)

# point on surface: guarantees that the centroid will be inside the object (ex: donut)
nz_pos = st_point_on_surface(nz)
seine_pos = st_point_on_surface(seine)

## Buffers
seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)

## Affine transformation
# any transformation that preserves lines and parallelism but not necessary angles or length
# examples: shifting (translation), scaling, rotation
nz_sfc = st_geometry(nz)
# shifting: move every point by the same distance in map units
nz_shift = nz_sfc + c(0, 100000)

# scaling: enlarges or shrinks the object by a factor (globally or locally)
nz_centroid_sfc = st_centroid(nz_sfc)
nz_scale = (nz_sfc - nz_centroid_sfc) * 0.5 + nz_centroid_sfc

# rotating: requires rotation matrix; rotates points in a clockwise direction
rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 
nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(30) + nz_centroid_sfc

# replace old geometries with new
nz_scale_sf = st_set_geometry(nz, nz_scale)

## Clipping
b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
plot(b, border = "grey")
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"), cex = 3) # add text
# select the intersection of x and y
x = b[1]
y = b[2]
x_and_y = st_intersection(x, y)
plot(b, border = "grey")
plot(x_and_y, col = "lightgrey", border = "grey", add = TRUE) # intersecting area

# other options: st_difference (directional), st_union, st_sym_difference (opposite of intersection)

## Subsetting and Clipping
bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2017)
p = st_sample(x = box, size = 10) # 10 points within the bounding boxes
x_and_y = st_intersection(x, y)

# select the point that intersects with both x and y
p_xy1 = p[x_and_y]
p_xy2 = st_intersection(p, x_and_y)
sel_p_xy = st_intersects(p, x, sparse = FALSE)[, 1] &
  st_intersects(p, y, sparse = FALSE)[, 1]
p_xy3 = p[sel_p_xy]

## Geometry Unions
# aggregate US states into 4 regions
regions = aggregate(x = us_states[, "total_pop_15"], by = list(us_states$REGION),
                    FUN = sum, na.rm = TRUE)
regions2 = us_states |> 
  group_by(REGION) |>
  summarize(pop = sum(total_pop_15, na.rm = TRUE))

# aggregate western states
us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)
# add Texas
texas = us_states[us_states$NAME == "Texas", ]
texas_union = st_union(us_west_union, texas)

plot(st_geometry(us_states))
plot(st_geometry(texas_union), add = TRUE, col = "yellow")

## Type Transformations
# st_cast() behaves differently for single simple feature geometry (sfg) objects, simple feature geometry
# columns (sfc), and simple features objects

# sfg
multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1), ncol = 2))
linestring = st_cast(multipoint, "LINESTRING")
polyg = st_cast(multipoint, "POLYGON")

# reversed
multipoint_2 = st_cast(linestring, "MULTIPOINT")
multipoint_3 = st_cast(polyg, "MULTIPOINT")
all.equal(multipoint, multipoint_2)


# Geometric Operations on Raster Data -------------------------------------

## Geometric Intersections
elev = rast(system.file("raster/elev.tif", package = "spData"))
clip = rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45,
            resolution = 0.3, vals = rep(1, 9))
elev[clip, drop = FALSE]

## Extent and Origin
# when aligning  two images, you need to make sure their resolution, projection, origin, and/or extent match

# setting extent
elev = rast(system.file("raster/elev.tif", package = "spData"))
elev_2 = extend(elev, c(1, 2))
elev_3 = elev + elev_2 # error
# align extents with extend()
elev_4 = extend(elev, elev_2)
origin(elev_4)
origin(elev_4) = c(0.25, 0.25)

## Aggregation and Disaggregation
# to make resolutions match, you can either decrease (aggregate) or increase (disaggregate) the resolution of one raster
# aggregating
dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))
dem_agg = aggregate(dem, fact = 5, fun = mean)

# disaggregating
# two options: near and bilinear
# near = all output cells get the value of the input cell -> blocky output
# bilinear = four nearest pixel centers of the input are put into a weighted average to determine the output
dem_disagg = disagg(dem_agg, fact = 5, method = "bilinear")
identical(dem, dem_disagg)

## Resampling
# when you have 2+ rasters with different resolutions and origins
# resampling: process of computing values for new pixel locations
# nearest neighbor
# bilinear interpolation
# cubic interpolation
# cubic spline interpolation
# lanczos windowed sinc resampling

# only nearest neighbor can be used for categorical rasters
# all can be used for continuous

target_rast = rast(xmin = 794600, xmax = 798200, 
                   ymin = 8931800, ymax = 8935400,
                   resolution = 150, crs = "EPSG:32717")
dem_resampl = resample(dem, y = target_rast, method = "bilinear")


# Exercises ---------------------------------------------------------------


