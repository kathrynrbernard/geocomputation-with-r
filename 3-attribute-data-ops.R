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
us_states_names <- select(us_states, NAME)
class(us_states_names)

# E2. 
select(us_states, c(total_pop_10, total_pop_15))

# E3.
midwest <- filter(us_states, REGION == "Midwest")
plot(midwest)

west <- filter(us_states, REGION == "West" &
               as.numeric(AREA) <= 250000 &
                 total_pop_15 > 5000000)
plot(west)

south <- filter(us_states, REGION == "South" &
                  (as.numeric(AREA) > 150000 |
                  total_pop_15 > 7000000))
plot(south)

# E4. 
sum(us_states$total_pop_15)
min(us_states$total_pop_15)
max(us_states$total_pop_15)

# E5.
group_by(us_states, REGION) %>% count()

# E6.
group_by(us_states, REGION) %>% summarize(total = sum(total_pop_15),
                                          min = min(total_pop_15),
                                          max = max(total_pop_15))

# E7. 
colnames(us_states)
colnames(us_states_df)

join <- left_join(us_states, us_states_df, by = c("NAME" = "state"))
class(join)

# E8.
dplyr::anti_join(us_states_df, us_states, by = c("state" = "NAME"))

# E9.
# population density = population/area
us_states$pop_density_15 <- us_states$total_pop_15/us_states$AREA
us_states$pop_density_10 <- us_states$total_pop_10/us_states$AREA
# us_states |> mutate(pop_dens_15 = total_pop_15 / area)
# us_states |> mutate(pop_dens_10 = total_pop_10 / area)

# E10.
us_states$pop_den_dif <- us_states$pop_density_15-us_states$pop_density_10
# us_states |> mutate(pop_dens_dif = total_pop_15 - total_pop_10)
plot(us_states)

# E11.
colnames(us_states) <- tolower(colnames(us_states))

# E12.
us_states_sel <- cbind(select(us_states, geometry), select(us_states_df,median_income_15))

# E13.
us_states_df %>% mutate(pov_dif = poverty_level_15 - poverty_level_10)
# todo - percentage

# E14.

# E15.
rast <- rast(nrows = 9, ncols = 9, resolution = 0.5, 
             xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
             vals = 1:81)

new_raster = rast(nrows = 9, ncols = 9, resolution = 0.5, 
                  xmin = -2.25, xmax = 2.25, ymin = -2.25, ymax = 2.25,
                  vals = 1:81)
rast[1] # upper left
rast[9] # upper right
rast[]

# E16.
summary(grain)

# E17.
path  <- system.file("raster/dem.tif", package = "spDataLarge")
dem <- rast(path)
hist(dem)
boxplot(dem)
