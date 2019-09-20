# setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(here)

# wrangle bio -------------------------------------------------------------

biodatraw <- read_excel(here('data/raw', 'Refined Biological Results.xlsx'))

# selecting lat, lon, date, time, parameter, value, result, unit
# taking unique values (making some assumptions)
biodat <- biodatraw %>%
  select(
    station = MonitoringLocationIdentifier,
    latitude = `ActivityLocation/LatitudeMeasure`,
    longitude = `ActivityLocation/LongitudeMeasure`,
    date = ActivityStartDate,
    time = `ActivityStartTime/Time`,
    param = `CharacteristicName`,
    val = ResultMeasureValue,
    unit = `ResultMeasure/MeasureUnitCode`
    ) %>%
  unique

# filtering out parameters with few observations, include microcystin and chlorophyll
biodat <- biodat %>%
  group_by(param) %>%
  mutate(
    n = n()
  ) %>%
  ungroup %>%
  filter(n > 1000 | grepl('^Microcystin|^Chloroph', param)) %>%
  select(-n)

table(biodat[, c('param', 'unit')])

# standardizing names for units
biodat <- biodat %>%
  mutate(
    unit = case_when(
      unit == '%' ~ '% saturatn',
      unit == 'ft/sec' ~ 'ft',
      unit == 'PSS' ~ 'ppt',  # these are equivalent
      unit == 'std units' ~ 'None', # for ph
      unit == 'uS/cm @25C' ~ 'uS/cm',
      grepl('Microcystin', param) & unit == 'ppb' ~ 'ug/l',
      T ~ unit
    )
  )

# remove turbidy as FTU, JTU, don't know what this is
# remove do sat as g/l
# remove conductance as degree c
biodat <- biodat %>%
  filter(!(unit %in% c('FTU', 'JTU') & param %in% 'Turbidity')) %>%
  filter(!(unit %in% 'g/l' & param %in% 'Dissolved oxygen saturation')) %>%
  filter(!(unit %in% 'deg C' & param %in% 'Specific conductance'))

# table(biodat[, c('param', 'unit')])

# standardizing values for different units
biodat <- biodat %>%
  mutate(
    val = case_when(
      val %in% c('ND', 'TRACE') ~ 0,
      T ~ as.numeric(val)
      ),
    val = case_when(
      param == 'Depth, Secchi disk depth' & unit == 'ft' ~ val * 0.3048, # to m
      param == 'Iron' & unit == 'ug/l' ~ val * 0.001, # to mg/l
      param == 'Specific conductance' & unit == 'uS/cm' ~ val * 0.001, # to ms/cm
      param == 'deg F' ~ (val - 32) * 5 / 9 , # to deg C
      param == 'Total dissolved solids' & unit == 'tons/ac ft' ~ val * 0.735468, # to g/l
      param == 'Total dissolved solids' & unit == 'mg/l' ~ val * 0.001, # to g/l
      T ~ val
    ),
    unit = case_when(
      unit == 'ft' ~ 'm',
      param == 'Iron' & unit == 'ug/l' ~ 'mg/l',
      param == 'Specific conductance' & unit == 'uS/cm' ~ 'mS/cm',
      unit == 'deg F' ~ 'deg C',
      unit == 'tons/ac ft' ~ 'g/l',
      param == 'Total dissolved solids' & unit == 'mg/l' ~ 'g/l',
      T ~ unit
    )
  )

# final minor edits
# rename some values in param
# take daily avg for multiples
biodat <- biodat %>%
  mutate(
    param = case_when(
      param == 'Chlorophyll a (probe)' ~ 'Chlorophyll a',
      T ~ param
    ),
    date = ymd(date)
  ) %>%
  select(-time) %>%
  group_by(station, date, param, unit) %>%
  summarise_all(mean, na.rm = T) %>%
  filter(!is.na(val))

# wrangle phy -------------------------------------------------------------

phydatraw <- read_excel(here('data/raw', 'Refined Physical Result.xlsx'))

# selecting station, date, time, parameter, value, result, unit
# taking unique values (making some assumptions)
phydat <- phydatraw %>%
  select(
    station = MonitoringLocationIdentifier,
    date = ActivityStartDate,
    time = `ActivityStartTime/Time`,
    param = `CharacteristicName`,
    val = ResultMeasureValue,
    unit = `ResultMeasure/MeasureUnitCode`
    ) %>%
  unique

# filtering out parameters with few observations, include microcystin and chlorophyll
phydat <- phydat %>%
  group_by(param) %>%
  mutate(
    n = n()
  ) %>%
  ungroup %>%
  filter(n > 1000 | grepl('^Microcystin|^Chloroph', param)) %>%
  select(-n)

# table(phydat[, c('param', 'unit')])

# standardizing names for units
phydat <- phydat %>%
  mutate(
    unit = case_when(
      unit == '%' ~ '% saturatn',
      unit == 'ft/sec' ~ 'ft',
      unit == 'PSS' ~ 'ppt',  # these are equivalent
      unit == 'std units' ~ 'None', # for ph
      unit == 'uS/cm @25C' ~ 'uS/cm',
      grepl('Microcystin', param) & unit == 'ppb' ~ 'ug/l',
      T ~ unit
    )
  )

# remove turbidy as FTU, JTU, don't know what this is
# remove do sat as g/l
# remove conductance as degree c
phydat <- phydat %>%
  filter(!(unit %in% c('FTU', 'JTU') & param %in% 'Turbidity')) %>%
  filter(!(unit %in% 'g/l' & param %in% 'Dissolved oxygen saturation')) %>%
  filter(!(unit %in% 'deg C' & param %in% 'Specific conductance'))

# table(biodat[, c('param', 'unit')])

# standardizing values for different units
phydat <- phydat %>%
  mutate(
    val = case_when(
      val %in% c('ND', 'TRACE') ~ 0,
      T ~ as.numeric(val)
      ),
    val = case_when(
      param == 'Depth, Secchi disk depth' & unit == 'ft' ~ val * 0.3048, # to m
      param == 'Iron' & unit == 'ug/l' ~ val * 0.001, # to mg/l
      param == 'Specific conductance' & unit == 'uS/cm' ~ val * 0.001, # to ms/cm
      param == 'deg F' ~ (val - 32) * 5 / 9 , # to deg C
      param == 'Total dissolved solids' & unit == 'tons/ac ft' ~ val * 0.735468, # to g/l
      param == 'Total dissolved solids' & unit == 'mg/l' ~ val * 0.001, # to g/l
      T ~ val
    ),
    unit = case_when(
      unit == 'ft' ~ 'm',
      param == 'Iron' & unit == 'ug/l' ~ 'mg/l',
      param == 'Specific conductance' & unit == 'uS/cm' ~ 'mS/cm',
      unit == 'deg F' ~ 'deg C',
      unit == 'tons/ac ft' ~ 'g/l',
      param == 'Total dissolved solids' & unit == 'mg/l' ~ 'g/l',
      T ~ unit
    )
  )

# final minor edits
# rename some values in param
# take daily avg for multiples
phydat <- phydat %>%
  mutate(
    param = case_when(
      param == 'Chlorophyll a (probe)' ~ 'Chlorophyll a',
      param == 'Microcystins' ~ 'Microcystin',
      T ~ param
    ),
    date = ymd(date)
  ) %>%
  select(-time) %>%
  group_by(station, date, param, unit) %>%
  summarise_all(mean, na.rm = T) %>%
  filter(!is.na(val))


# join phydat with biodat -------------------------------------------------

# these seem to have the same data

# join both
alldat <- full_join(biodat, phydat, c('station', 'date', 'param', 'unit'))

save(alldat, file = 'data/alldat.RData', compress = 'xz')
write.csv(alldat, file = here('data', 'alldat.csv'), row.names = F)

# lake discharge ----------------------------------------------------------

disdatraw <- read_excel(here('data/raw', 'Lake discharge.xlsx'), skip = 11)

disdat <- disdatraw %>% 
  mutate(
    date = as.Date(Date)
  ) %>%  
  select(
    date, 
    discharge_cfs = `Dischage (cubic feet per second)`
  ) %>% 
  group_by(date) %>% 
  summarise(discharge_cfs = mean(discharge_cfs, na.rm = T))

save(disdat, file = 'data/disdat.RData', compress = 'xz')
write.csv(disdat, file = here('data', 'disdat.csv'), row.names = F)
