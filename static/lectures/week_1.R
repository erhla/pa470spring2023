library(tidyverse)

file_loc <- '~/../Downloads/'

ccao <- read_csv(str_glue(file_loc, 
                          'Cook_County_Assessor_s_Residential_Modeling_Characteristics__Chicago_.csv'))

str(ccao)

# count property classes

ccao %>% count(`Property Class`)

ccao %>% group_by(`Property Class`) %>%
  summarize(n = n())

ccao %>% count(`Property Class`, sort=TRUE)

# top five classes

ccao %>% count(`Property Class`) %>%
  slice_max(order_by = `n`, n=5)


# keep only townships with Chicago in the name

ccao %>% distinct(`Township Name`)

ccao %>% filter(str_detect(`Township Name`, 'Chicago'))


# average rooms by property class

ccao %>% group_by(`Property Class`) %>%
  summarize(avg_rooms = mean(Rooms),
            count = n()) %>% view()

# lubridate

library(lubridate)

ccao %>% select(`Sale Date`)

ccao %>% select(contains('Date'))

ccao %>% count(`Sale Date`, sort=TRUE)

ccao <- ccao %>%
  mutate(
    `Sale Date` = mdy_hms(`Sale Date`)
  )

ccao %>% count(`Sale Date`)

# count sales in January 2019

ccao %>% filter(
  month(`Sale Date`) == 1 & year(`Sale Date`) == 2019
)

# count sales by month

ccao %>% group_by(
  month = floor_date(`Sale Date`, 'month')
) %>% summarize(
  count = n()
)

# ggplot2
library(ggplot2)

theme_set(theme_bw())

# sales by week

gdata <- ccao %>% 
  filter(!is.na(`Sale Date`)) %>%
  group_by(week = floor_date(`Sale Date`, 'week')) %>%
  summarize(count = n())

ggplot(gdata, aes(x=week, y=count)) +
  geom_point() +
  geom_line()

ggplot(gdata, aes(x=week, y=count)) +
  geom_point(size=2) + geom_line(size=1.5) +
  scale_x_datetime(date_breaks = '6 months', date_labels='%m/%d/%y') +
  labs(x='Week of', y='Sales', title='Sales in Chicago by Week')

# sale price density

library(scales)

ggplot(ccao, aes(x=`Sale Price`)) +
  geom_density(fill='navy')

ggplot(ccao, aes(x=`Sale Price`)) +
  geom_density(fill='navy') +
  scale_x_log10(labels=dollar_format())

ggplot(ccao, aes(x=`Township Name`, y=`Sale Price`)) +
  geom_boxplot(fill='navy') +
  scale_y_log10(labels=dollar_format()) +
  coord_flip()





