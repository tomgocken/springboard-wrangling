# Script to create tidy envdat dataset and export as txt file
# Data for counties of interest was queried from http://wonder.cdc.gov/EnvironmentalData.html
airtemp <- read.delim("data/Air Temperature.txt")
precip <- read.delim("data/Precipitation.txt")
sunlight <- read.delim("data/Sunlight.txt")
surfacetemp <- read.delim("data/Surface Temperature.txt")
particulate <- read.delim("data/Particulate Matter.txt")

# Replace "Missing" string with NA
library(dplyr)
airtemp <- mutate(airtemp, heat_index = 
                    type.convert(as.character(Avg.Daily.Max.Heat.Index..F.), 
                                 na.strings = "Missing"))                                          

surfacetemp <- mutate(surfacetemp, day_surface_temp = 
                        type.convert(as.character(
                          Avg.Day.Land.Surface.Temperature..F.), 
                          na.strings = "Missing"),
                      night_surface_temp = type.convert(as.character(
                        Avg.Night.Land.Surface.Temperature..F.), 
                        na.strings = "Missing"))

joindat <- full_join(airtemp, precip)
joindat <- full_join(joindat, sunlight)
joindat <- full_join(joindat, surfacetemp)
joindat <- full_join(joindat, particulate)
joindat <- mutate(joindat, date = as.Date(paste(joindat$Year.Code, 
                                                joindat$Month.Code, 
                                                joindat$Day.of.Month.Code, 
                                                sep="-")))

envdat <- select(joindat,
                 county = County,
                 year = Year,
                 month = Month.Code,
                 day_of_yr = Day.of.Year,
                 date,
                 max_air_temp = Avg.Daily.Max.Air.Temperature..F.,
                 min_air_temp = Avg.Daily.Min.Air.Temperature..F.,
                 heat_index,
                 precip = Avg.Daily.Precipitation..mm.,
                 sunlight = Avg.Daily.Sunlight..KJ.m².,
                 day_surface_temp,
                 night_surface_temp,
                 particulate_matter = Avg.Fine.Particulate.Matter..µg.m³.
)

# Growing degree units (GDUs) = ((T(max) + T(min)) / 2) - T(base)
envdat <- mutate(envdat, gdu = ifelse(max_air_temp < 50, 0,
                                      (((ifelse(max_air_temp > 86, 86, max_air_temp) 
                                         + ifelse(min_air_temp < 50, 50, min_air_temp)) / 2) - 50)))
# Accumulated GDUs (AGDUs)
envdat <- transform(envdat, agdu = ave(gdu, paste(county, year), 
                                       FUN = cumsum))

write.table(envdat, "C:/Projects/data/envdat.txt", sep = "\t")
