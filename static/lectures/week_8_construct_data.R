library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(readxl)

variables <- load_variables(2020, 'pl')
ethnicity <- get_decennial('block', table='P2', year=2020, state=17, county=31, output='wide')
housing <- get_decennial('block', table='H1', year=2020, state=17, county=31, output='wide')

hmini <- housing %>% select(GEOID, total_units=H1_001N, occupied=H1_002N, vacant=H1_003N)
ethmini <- ethnicity %>% select(GEOID, total_pop=P2_001N,
                                total_hisp=P2_002N, total_nonhisp=P2_003N, white_alone=P2_005N,
                                black_alone=P2_006N, aian_alone=P2_007N, nhpi_alone=P2_009N,
                                asian_alone=P2_008N, other_alone=P2_010N, two_or_more=P2_011N)

precincts <- read_sf('https://data.cityofchicago.org/api/geospatial/6piy-vbxa?accessType=DOWNLOAD&method=export&format=GeoJSON')

#https://chicagoelections.gov/en/election-results-specifics.asp
#re-saved as xlsx manually
results <- read_excel('~/../Downloads/dataexport.xls.xlsx', skip = 9, col_names = FALSE) 

results_cleaned <- results %>% filter(!is.na(...1), ...1 != 'Total') %>% 
  mutate(ward = if_else(str_detect(...1, 'Ward '),
                        str_extract(...1, '\\d+'),
                        NA_character_)) %>% 
  fill(ward) %>%
  filter(!is.na(...2)) %>%
  select(precinct = ...1,
         ward,
         Green = ...3,
         King = ...5, 
         Buckner = ...7,
         Wilson = ...9,
         Johnson = ...11,
         Vallas = ...13,
         Lightfoot = ...15, 
         Sawyer = ...17,
         Garcia = ...19) %>%
  filter(precinct != 'Precinct') %>%
  readr::type_convert()

cook <- blocks(state=17, county=31)
precincts_pro <- st_transform(st_make_valid(precincts), st_crs(cook))

joined <- precincts_pro %>% select(precinct, ward) %>%
  st_join(
    cook %>% select(GEOID20)
  ) %>%
  mutate(precinct = as.numeric(precinct),
         ward = as.numeric(ward)) %>%
  st_centroid() %>%
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  left_join(hmini, by=c('GEOID20'='GEOID')) %>%
  left_join(ethmini, by=c('GEOID20'='GEOID')) %>%
  group_by(precinct, ward, geometry, long, lat) %>%
  summarize(across(total_units:two_or_more, sum))


joined %>%
  left_join(results_cleaned) %>%
  tibble() %>%
  select(-geometry) %>%
  write_csv('static/lectures/week_8_data.csv')

