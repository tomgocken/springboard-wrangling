# Data for counties of interest was queried from http://wonder.cdc.gov/EnvironmentalData.html
setwd("C:/Projects/springboard-wrangling")
airtemp <- read.delim("../data/Air Temperature.txt")
precip <- read.delim("../data/Precipitation.txt")
sunlight <- read.delim("../data/Sunlight.txt")
surfacetemp <- read.delim("../data/Surface Temperature.txt")
particulate <- read.delim("../data/Particulate Matter.txt")

library(dplyr)

# Replace "Missing" string with NA
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

# Load and reshape monthly ERSST data measuring El Nino / La Nina effects
# http://www.cpc.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml
el_nino <- read.csv("../data/el_nino.csv")
el_nino <- dplyr::rename(el_nino, year = Year, "1" = DJF, "2" = JFM, "3" = FMA,
                         "4" = MAM, "5" = AMJ, "6" = MJJ, "7" = JJA, "8" = JAS,
                         "9" = ASO, "10" = SON, "11" = OND, "12" = NDJ)
el_nino_tidy <- tidyr::gather(el_nino, "month", "ersst", 2:13)
el_nino_tidy$month <- as.integer(el_nino_tidy$month)
# Add 3 and 6 month lag variables
el_nino_lag <- dplyr::mutate(el_nino_tidy,
                             yr_lag3mo = as.integer(ifelse(month < 10, year, year + 1)),
                             mo_lag3mo = as.integer(ifelse(month < 10, month + 3, month - 9)),
                             yr_lag6mo = as.integer(ifelse(month < 7, year, year + 1)),
                             mo_lag6mo = as.integer(ifelse(month < 7, month + 6, month - 6)))
el_nino_lag3mo <- dplyr::select(el_nino_lag, year = yr_lag3mo,
                                month = mo_lag3mo, ersst_lag3mo = ersst)
el_nino_lag6mo <- dplyr::select(el_nino_lag, year = yr_lag6mo,
                                month = mo_lag6mo, ersst_lag6mo = ersst)
el_nino_all <- dplyr::left_join(el_nino_tidy, el_nino_lag3mo)
el_nino_all <- dplyr::left_join(el_nino_all, el_nino_lag6mo)
el_nino_all <- dplyr::arrange(el_nino_all, year, month)

# Join data into a single tidy dataset
joindat <- left_join(airtemp, precip)
joindat <- left_join(joindat, sunlight)
joindat <- left_join(joindat, surfacetemp)
joindat <- left_join(joindat, particulate)
joindat <- left_join(joindat, el_nino_all, by = c("Year" = "year", "Month.Code" = "month"))

# Date variable created by concatenating year, month, and day columns and coverting to date class.
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
                 particulate_matter = Avg.Fine.Particulate.Matter..µg.m³.,
                 ersst,
                 ersst_lag3mo,
                 ersst_lag6mo
)

# Growing degree units (GDUs) = ((T(max) + T(min)) / 2) - T(base)
envdat <- mutate(envdat, gdu = ifelse(max_air_temp < 50, 0,
                                      (((ifelse(max_air_temp > 86, 86, max_air_temp) 
                                         + ifelse(min_air_temp < 50, 50, min_air_temp)) / 2) - 50)))

# Accumulated GDUs (AGDUs)
envdat <- transform(envdat, agdu = ave(gdu, paste(county, year), 
                                       FUN = cumsum))

# Save results for future use
write.table(envdat, "../data/envdat.txt", sep = "\t")

# Summarize and view data
summary(envdat)
library(ggplot2)

# Differences in max air temp by year and county (2009-2011):
envdat_3yr <- filter(envdat, year %in% c(2009, 2010, 2011))
envdat_3yr$year <- as.factor(envdat_3yr$year)
qplot(county, max_air_temp, data = envdat_3yr, geom = "boxplot", 
      facets = year ~ .)

# Differences in accumulated GDUs by county, across years:
county_means <- envdat %>%
  filter(day_of_yr != 366) %>% # exclude extra leap year day
  group_by(county, day_of_yr) %>%
  summarize(agdu_mean = mean(agdu))

qplot(day_of_yr, agdu_mean, data = county_means, geom = "line", color = county, 
      xlab = "Day of Year", ylab = "Mean Accumulated GDUs") + geom_line(size = 1.0)

# Differences in accumulated GDUs by 5-year means, across counties:
envdat <- mutate(envdat, yr_group = ifelse(year < 1997, "1992-1996",
                                           ifelse(year < 2002, "1997-2001",
                                                  ifelse(year < 2007, "2002-2006",
                                                         "2007-2011"))))
yr_means <- envdat %>%
  filter(day_of_yr != 366) %>% # exclude extra leap year day
  group_by(yr_group, day_of_yr) %>%
  summarize(agdu_mean = mean(agdu))

qplot(day_of_yr, agdu_mean, data = yr_means, geom = "line", 
      color = yr_group, xlab = "Day of Year", 
      ylab = "Mean Accumulated GDUs") + geom_line(size = 1.0)