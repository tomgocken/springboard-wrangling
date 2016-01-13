# Step 1: Load datasets
# Data for counties of interest was queried from:
# http://wonder.cdc.gov/EnvironmentalData.html
# Each environmental data type was downloaded in its own tab-delimited file
# and a dataset was created for each file.
airtemp <- read.delim("data/Air Temperature.txt")
precip <- read.delim("data/Precipitation.txt")
sunlight <- read.delim("data/Sunlight.txt")
surfacetemp <- read.delim("data/Surface Temperature.txt")

# Step 2: Manage NA's
# **dplyr** package loaded for next steps:
library(dplyr)

# Missing numeric values from certain columns in original files were populated 
# with the string "Missing".
# "Missing" strings were converted to NA using **type.convert** function.
airtemp <- mutate(airtemp, heat_index = 
                    type.convert(as.character(Avg.Daily.Max.Heat.Index..F.), 
                                 na.strings = "Missing")
)                                          
surfacetemp <- mutate(surfacetemp, day_surface_temp = 
                        type.convert(as.character(Avg.Day.Land.Surface.Temperature..F.), 
                                     na.strings = "Missing"),
                      night_surface_temp =
                        type.convert(as.character(Avg.Night.Land.Surface.Temperature..F.), 
                                     na.strings = "Missing")
)
# Note: The same result could have been accomplished using **gsub** function:
# airtemp <- mutate(airtemp, heat_index = as.numeric(gsub("Missing", NA, 
#                   as.character((airtemp$Avg.Daily.Max.Heat.Index..F.)))))

# Step 3: Join data into a single tidy dataset
# Rows from individual datasets represent unique county/date observations. 
# To ensure that all rows from every dataset were included in the combined 
# dataset, full joins were used. Except for environmental data columns, 
# variables were named consistently in downloaded files. As a result, natural
# joins on all variables with common names were possible without the need for
# a "by" argument.
joindat <- full_join(airtemp, precip)
joindat <- full_join(joindat, sunlight)
joindat <- full_join(joindat, surfacetemp)

# Date variable was created by concatenating year, month, and day columns and 
# coverting to date class.
joindat <- mutate(joindat, date = as.Date(paste(joindat$Year.Code, 
                                                joindat$Month.Code, joindat$Day.of.Month.Code, sep="-"))
)

# Select statement used to assign consise variable names in common format to columms of interest.
envdat <- select(joindat,
                 county = County,
                 year = Year,
                 day_of_yr = Day.of.Year,
                 date,
                 max_air_temp = Avg.Daily.Max.Air.Temperature..F.,
                 min_air_temp = Avg.Daily.Min.Air.Temperature..F.,
                 heat_index,
                 precip = Avg.Daily.Precipitation..mm.,
                 sunlight = Avg.Daily.Sunlight..KJ.m².,
                 day_surface_temp,
                 night_surface_temp
)

# Growing degree days (gdd) and accumulated growing degree days (agdd) created from 
# min and max air temps.
envdat <- mutate(envdat, gdd = ifelse(max_air_temp < 50, 0, # exclude negative values
                                        (((ifelse(max_air_temp > 86, 86, max_air_temp) 
                                           + ifelse(min_air_temp < 50, 50, min_air_temp)) / 2) - 50)))
envdat <- transform(envdat, agdd = ave(gdd, paste(county, year), FUN = cumsum))

# Step 4: Summarize and view data (UNDER CONSTRUCTION)
summary(envdat)
# write.csv(envdat, "envdat.csv", row.names=FALSE, na="")
# ...add some descriptive graphs...
library(ggplot2)
g <- filter(envdat, year > 2008)
g$year <- as.factor(g$year)
qplot(county, max_air_temp, data = g, geom = "boxplot", facets = year ~ .)
