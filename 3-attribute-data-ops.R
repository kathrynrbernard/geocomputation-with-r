# https://geocompr.robinlovelace.net/attr.html#attr

# Load Packages -----------------------------------------------------------

library(sf)      # vector data package introduced in Chapter 2
library(terra)   # raster data package introduced in Chapter 2
library(dplyr)   # tidyverse package for data frame manipulation
library(spData)  # spatial data package introduced in Chapter 2
library(tidyverse)

# Vector Attribute Manipulation -------------------------------------------

# sf is compatbile with tidyverse stuff

dim(world)
# drop geom column - processing can be faster without it (usually is better to keep it though)
world_df <- st_drop_geometry(world)

# subsetting with base R
small_countries = world[world$area_km2 < 10000, ]
small_countries = subset(world, area_km2 < 10000)

# subsetting with dplyr
world1 = dplyr::select(world, name_long, pop)
names(world1) # geom column is still there too ("sticky")

# all columns between name_long and pop (inclusive)
world2 = dplyr::select(world, name_long:pop)

# all columns except subregion and area_km2 (inclusive)
world3 = dplyr::select(world, -subregion, -area_km2)

# subset and rename column
world4 = dplyr::select(world, name_long, population = pop)

# extract a single column as a vector
pull(world, pop) # dplyr
world$pop # ase R
world[["pop"]] # base R

# slice
slice(world, 1:6) # row equivalent of select

# filter
world7 = filter(world ,area_km2 < 10000) # countries with a small area
world7 = filter(world, lifeExp > 82)      # with high life expectancy

# aggregation
# non spatial
world_agg1 = aggregate(pop ~ continent, FUN = sum, data = world,
                       na.rm = TRUE)
class(world_agg1)
# spatial
world_agg2 = aggregate(world["pop"], list(world$continent), FUN = sum, 
                       na.rm = TRUE)
class(world_agg2)

# dplyr 
world_agg3 = world |>
  group_by(continent) |>
  summarize(pop = sum(pop, na.rm = TRUE))
class(world_agg3)

# joins
# start with spatial, join non-spatial = keep spatial info
# left join - all rows in world are kept even if they don't have coffee data
world_coffee = left_join(world, coffee_data) # coffee_data is from spData
# shared column is name_long
class(world_coffee)
nrow(world_coffee)
names(world_coffee)
plot(world_coffee["coffee_production_2017"])
plot(world_coffee["coffee_production_2016"])

# inner join - only keep rows from world that have coffee data
world_coffee_inner = inner_join(world, coffee_data)
nrow(world_coffee_inner)
# resulting df is missing 2 rows from coffee; dem republic of congo was abbreviated and therefore missed
setdiff(coffee_data$name_long, world$name_long)
coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] = drc
world_coffee_match = inner_join(world, coffee_data)
nrow(world_coffee_match)

# create new attributes
# mutate - adds new columns
world |> 
  mutate(pop_dens = pop / area_km2)
# transmute - drops all other existing columns except geom
world |> 
  transmute(pop_dens = pop / area_km2)

# unite - pastes together existing columns
world_unite = world |>
  tidyr::unite("con_reg", continent:region_un, sep = ":", remove = TRUE)
# separate
world_separate = world_unite |>
  tidyr::separate(con_reg, c("continent", "region_un"), sep = ":")

# Manipulating Raster Objects ---------------------------------------------

# create raster from scratch
# raster with numeric cell values
elev = rast(nrows = 6, ncols = 6, resolution = 0.5, 
            xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
            vals = 1:36)
plot(elev)

# raster with categorical values
grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = rast(nrows = 6, ncols = 6, resolution = 0.5, 
             xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
             vals = grain_fact)

levels(grain) = data.frame(value = c(0, 1, 2), wetness = c("wet", "moist", "dry"))
levels(grain)
plot(grain)

# subsetting
# return value of top left pixel in the raster object
elev[1, 1]
elev[1]

# subsetting multi-layered raster objects returns the cell values for each layer
two_layers = c(grain, elev) 
two_layers[1] = cbind(c(1), c(4))
two_layers[] # retrieve all values

# summary stats
summary(elev)
# custom stats
global(elev, sd)
# frequency
freq(elev)
hist(elev)


# Exercises ---------------------------------------------------------------

data(us_states)
data(us_states_df)

# E1.