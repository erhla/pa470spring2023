#https://divvy-tripdata.s3.amazonaws.com/index.html
#download 2021
library(tidyverse)
library(lubridate)
file_ls <- list.files('../../../Downloads/', pattern='divvy', full.names = TRUE)

divvy <- tibble()
for(file in file_ls){
  df <- read_csv(unz(file, paste0(str_sub(file, 20, 40), '.csv')))
  divvy <- divvy %>% bind_rows(df)
}

all_hours <- data.frame(
  started_hour = seq(ymd_hms('2021-01-01 00:00:00'), ymd_hms('2021-12-31 23:59:59'), by='1 hour')
)

divvy_hours <- divvy %>% count(started_hour = floor_date(started_at, 'hour'))


divvy_hours %>% mutate(dif = lead(started_hour) - started_hour) %>%
  filter(dif >= 2)

beach <- read_csv("https://data.cityofchicago.org/api/views/k7hf-8y75/rows.csv?accessType=DOWNLOAD")

beach_hour <- beach %>% mutate(time = mdy_hms(`Measurement Timestamp`),
                 started_hour = floor_date(time, 'hour')) %>%
  filter(started_hour >= ymd('2021-05-01')) %>%
  group_by(started_hour) %>%
  summarize(temp = median(`Air Temperature`),
            wind = median(`Wind Speed`),
            humidity = median(Humidity),
            solar_rad = median(`Solar Radiation`),
            interval_rain = median(`Interval Rain`),
            )

segment <- read_csv('../../../Downloads/Chicago_Traffic_Tracker_-_Historical_Congestion_Estimates_by_Region_-_2018-Current.csv')

segment_hour <- segment %>% mutate(TIME = mdy_hms(TIME)) %>% 
  filter(TIME >= ymd('2021-01-01')) %>%
  group_by(time_hour = floor_date(TIME, 'hour')) %>%
  summarize(
    avg_speed = sum(SPEED * BUS_COUNT) / sum(BUS_COUNT)
  )

joined_data <- all_hours %>%
  left_join(divvy_hours %>% rename(rides = n)) %>%
  left_join(segment_hour %>% rename(started_hour = time_hour)) %>%
  left_join(beach_hour)

joined_data %>% write_csv('static/lectures/week_3_data.csv')

#segment <- read_csv("https://data.cityofchicago.org/api/views/kf7e-cur8/rows.csv?accessType=DOWNLOAD")
