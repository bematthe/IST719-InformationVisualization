library(dplyr)
library(lubridate)
library(ggplot2)

fname <- 'C:/Users/marmesto/Documents/Cuse/visualization/vizathon/BuoyData_2_2.csv'
data <- read.csv(fname, stringsAsFactors = FALSE)

dim(data)
str(data)
is.na(data)
head(data)

data## data cleaning
buoy <- data %>%
  rename(datetime = 1) %>%
  mutate(datetime = as_datetime(datetime, format = '%m/%d/%Y %H:%M')) %>%
  mutate(date = as_date(datetime)) %>%
  select(datetime, date, everything())

## aggregate all averages by day
buoy_day <- buoy %>%
  select(-datetime) %>%
  group_by(date) %>%
  summarize(across(everything(), mean))

## aggregate by depth
## didn't use this
buoy_depth <- buoy %>%
  select(-datetime, -date)
group_by(DEPTH_m) %>%
  summarize(across(everything(), mean))

## time series of average ph each day
## colored by temp
ggplot(buoy_day, aes(x = date, y = pH)) +
  geom_point(aes(col = T_DEGC)) +
  scale_x_date(date_labels = '%b %Y',
               date_breaks = '4 months') +
  theme_bw() +
  xlab('') + ylab('pH') +
  labs(col = 'Temperature (C)')

## time series of average oxygen each day
## colored by temp
ggplot(buoy_day, aes(x = date, y = Dox_mg_L)) +
  geom_point(aes(col = T_DEGC)) +
  scale_x_date(date_labels = '%b %Y',
               date_breaks = '4 months') +
  theme_bw() +
  xlab('') + ylab('Dissolved oxygen (mg/L)') +
  labs(col = 'Temperature (C)')

## histogram of oxygen levels
ggplot(buoy_day, aes(x = Dox_mg_L)) + geom_histogram() +
  theme_bw())

#Becky Code

ggplot(mydata, aes(x = pH, y = Chl_ug_L)) +
  geom_hex() +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  theme_bw()


ggplot(mydata, aes(x = pH, y = Dox_mg_L)) +
  geom_hex() +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  theme_bw()


ggplot(mydata, aes(x = Dox_mg_L, y = Chl_ug_L)) +
  geom_hex() +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  theme_bw()

#Jose Code

ggplot(data = data, aes(x = DATE_TIME, y = DEPTH_m))+
  geom_line(color = "#00AFBB", size = .1)

ggplot(data) + aes(x=pH, y = DEPTH_m) + geom_point() + stat_smooth(method=lm)
ggplot(data) + aes(x=pH, y = PRECIP_in) + geom_point() + stat_smooth(method=lm)
ggplot(data) + aes(x=pH, y = T_DEGC) + geom_point() + stat_smooth(method=lm)

df2 <- aggregate(data$Dox_mg_L, list(data$DATE_TIME), mean)
df3 <- aggregate(data$Chl_ug_L, list(data$DATE_TIME), mean)
df4 <- aggregate(data$T_DEGC, list(data$DATE_TIME), mean)


ggplot(data = df2, aes(x = df2$Group.1, y = x))+
  geom_line(color = "#00AFBB", size = 2) +
  labs(title = "Oxygen Level Time Series", x = "Date", y = "Oxygen Level") +
  stat_smooth(method=lm)

ggplot(data = df3, aes(x = df2$Group.1, y = x))+
  geom_line(color = "#00AFBB", size = 2) +
  labs(title = "Algae Content Time Series", x = "Date", y = "Algae Content") +
  stat_smooth(method=lm)

ggplot(data = df4, aes(x = df2$Group.1, y = x))+
  geom_line(color = "#00AFBB", size = 2) +
  labs(title = "Algae Content Time Series", x = "Date", y = "Algae Content") +
  stat_smooth(method=lm)

ggplot(data) + aes(x = Chl_ug_L) +
  geom_histogram()

ggplot(data) + aes(x = pH) +
  geom_histogram()

ggplot(data) + aes(x = Dox_mg_L) +
  geom_histogram() +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  labs(x = "Oxygen Content", y = "Count") +
  theme_bw()

#N'Dea Code

barplot(table(vizathon$pH))







