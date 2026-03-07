install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("plotly")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)

library(readxl)
hotel_data <- read_excel("C:/Users/HUAWEI/Downloads/hotel_bookings.xlsx")

str(hotel_data)
head(hotel_data)
colnames(hotel_data)
summary(hotel_data)

hotel_clean <- hotel_data[, c("hotel",
                         "arrival_date_month",
                         "arrival_date_year",
                         "arrival_date_week_number",
                         "arrival_date_day_of_month",
                         "stays_in_weekend_nights",
                         "stays_in_week_nights",
                         "adults",
                         "children",
                         "babies",
                         "country",
                         "reserved_room_type",
                         "assigned_room_type",
                         "adr")]
View(hotel_clean)

dim(hotel_clean)
colSums(is.na(hotel_clean))

mean_children <- mean(hotel_clean$children, na.rm = TRUE)
mean_children <- round(mean_children)
hotel_clean$children[is.na(hotel_clean$children)] <- mean_children
colSums(is.na(hotel_clean))
