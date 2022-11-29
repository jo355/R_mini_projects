install.packages("tidyverse")
library("nycflights13")
library("tidyverse")
filter(flights,arr_delay >= 120)
filter(flights, dest == "IAH" | dest == "HOU")
select(flights,carrier)
filter(flights,carrier %in% c("AA","DL","UA"))
colnames(flights)
str(flights)
filter(flights,month %in% c(7,8,9))
filter(flights,arr_delay>=120 & dep_delay<=0)
filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)
summary(flights$dep_time)
filter(flights, dep_time %% 2400 <= 600)
filter(flights, between(month, 7, 9))
filter(flights, is.na(dep_time))
summary(flights)
arrange(flights, dep_time) %>% tail()
arrange(flights, desc(is.na(dep_time)), dep_time)
arrange(flights, desc(dep_delay))#delay
arrange(flights, dep_delay)
head(arrange(flights, desc(distance / air_time)))
select(flights, starts_with("dep_"), starts_with("arr_"))
flights_times <- mutate(flights,dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100)
                        %% 1440,sched_dep_time_mins = (sched_dep_time %/% 100 * 60 +
                                                         sched_dep_time %% 100) %% 1440)
select(flights_times, dep_time, dep_time_mins, sched_dep_time,sched_dep_time_mins)
flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest, origin, carrier, flight) %>%
  summarise(arr_delay = sum(arr_delay)) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_prop = arr_delay / sum(arr_delay)
  ) %>%
  arrange(dest, desc(arr_delay_prop)) %>%
  select(carrier, flight, origin, dest, arr_delay_prop)