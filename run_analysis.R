# Utilized dplyr package for mutate, full_join, and select functions
suppressMessages(library(dplyr))

# Step 1: Load datasets
# Data for counties of interest was queried from: http://wonder.cdc.gov/EnvironmentalData.html, 
# with each environmental data type downloaded in its own tab-delimited file.

airtemp <- read.delim("data/Air Temperature.txt")
precip <- read.delim("data/Precipitation.txt")
sunlight <- read.delim("data/Sunlight.txt")
surfacetemp <- read.delim("data/Surface Temperature.txt")

# Step 2: Handling NAs
# Missing numeric values from certain columns in original files were populated with the string "Missing".
# "Missing" strings were converted to NA using type.convert function.
# The same result could be accomplished using gsub function:
# airtemp <- mutate(airtemp, heat_index = as.numeric(gsub("Missing", NA,
#                   as.character((airtemp$Avg.Daily.Max.Heat.Index..F.)))))

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

# Step 3: Joining data into single tidy dataset
# Each row of each environmental dataset represented an observation on a unique county/date combination. To ensure that all rows from every dataset were included in the combined dataset, full joins were used.
# Except for environmental data columns, columns were named consistently in all downloaded files, making the joins of their datasets straightforward and without the need for a "by" clause.

j <- full_join(airtemp, precip, by = c("Notes", "County", "County.Code", "Year", "Year.Code", 
                                       "Month", "Month.Code", "Day.of.Month", "Day.of.Month.Code", 
                                       "Day.of.Year", "Day.of.Year.Code")) # by not needed
j <- full_join(j, sunlight)
j <- full_join(j, surfacetemp)

# A column with mode date was created by concatenating year, month, and day columns.
j <- mutate(j, Date = as.Date(paste(j$Year.Code, j$Month.Code, j$Day.of.Month.Code, sep="-")))

# Select statement used to assign consise variable names in common format to columms of interest
k <- select(j,
            county = County,
            year = Year,
            day_of_yr = Day.of.Year,
            date = Date,
            max_air_temp = Avg.Daily.Max.Air.Temperature..F.,
            min_air_temp = Avg.Daily.Min.Air.Temperature..F.,
            heat_index,
            precip = Avg.Daily.Precipitation..mm.,
            sunlight = Avg.Daily.Sunlight..KJ.m².,
            day_surface_temp,
            night_surface_temp
)
k <- mutate(k, gdd = ifelse(max_air_temp < 50, 0, # exclude negative values
                   (((ifelse(max_air_temp > 86, 86, max_air_temp) 
                      + ifelse(min_air_temp < 50, 50, min_air_temp)) / 2) - 50))
                   )

l <- filter(k, year > 2008)
l$year <- as.factor(l$year)
qplot(county, max_air_temp, data = l, geom = "boxplot", facets = year ~ .)