# Analysis of events in Rotterdam
library(plyr)
library(ggplot2)
library(data.table)

df.events <- read.csv(paste0("/Users/ulifretzen/Swaggathon/providedData/ds.urban.events.csv"))
View(df.events)
sum(complete.cases(df.events))

# Visitors per year
vis.per.year <- ddply(df.events, .(year), summarise, nr_visitors = sum(nr_visitors, na.rm = T))

ggplot(vis.per.year, aes(x= year, y = nr_visitors)) +
  geom_line() +
  geom_smooth()


# Number new events per year
df.events.new <- df.events[!is.na(df.events$initial_year & df.events$year),
                           c("initial_year", "year")]

df.events.new$new <- df.events.new$initial_year == df.events.new$year

df.events.new.by.year <- ddply(df.events.new, .(year), summarise, n_new_events = sum(new))
ggplot(df.events.new.by.year, aes(x= year, y = n_new_events)) +
  geom_line() +
  geom_smooth()


# # Number of visitors involved in size of events
# df.event.seasonality <- df.events[!is.na(df.events$start_date & df.events$nr_visitors), 
#                                   c("start_date", "year", "nr_visitors")]
# 
# df.event.seasonality$month <- substr(df.event.seasonality$start_date, 6, 7)
# 
# dt.event.seasonality <- data.table(df.event.seasonality)
# 
# dt.test <- dt.event.seasonality[, visitors := sum(nr_visitors), by = c("year", "month")]
# 
# head(dt.test)
# dt.event.seasonality <- ddply(df.event.seasonality,
#                               c("year", "month"),
#                               summarise,
#                               nr_visitors = sum("nr_visitors"))


# Get number of people per size of event
df.events.people.per.event.size <- df.events[, 12:13]
df.events.people.per.event.size.complete <- df.events.people.per.event.size[complete.cases(
  df.events.people.per.event.size), ]

setDT(df.events.people.per.event.size.complete)

ds.testyear <- df.events.people.per.event.size.complete


ds.testyear[, mean_nr := mean(nr_visitors), by = "year"]
ds.testyear[, count_event := .N, by = "year"]
ds.testyear <- ds.testyear[, c(2:4)]

ggplot(ds.testyear, aes(x = year, y = mean_nr)) + geom_line()
ggplot(ds.testyear, aes(x = year, y = count_event)) +geom_line()
ggplot(ds.test, aes(x = size)) + geom_histogram() + facet_wrap(~year) +scale_x_log10()

# save script as pdf
knitr::stitch('Event-Analysis.R')




