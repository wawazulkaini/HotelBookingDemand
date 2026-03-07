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

sum(duplicated(hotel_data))
hotel_data[duplicated(hotel_data), ]

hotel_clean <- hotel_data %>%
     distinct()

sum(duplicated(hotel_clean))

hotel_clean_2 <- hotel_clean[, c("hotel",
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
View(hotel_clean_2)

dim(hotel_clean_2)

colSums(is.na(hotel_clean_2))

mean_children <- mean(hotel_clean_2$children, na.rm = TRUE)
mean_children <- round(mean_children)
hotel_clean_2$children[is.na(hotel_clean_2$children)] <- mean_children

colSums(is.na(hotel_clean_2))

hotel_clean_2$total_guests <- hotel_clean_2$adults + hotel_clean_2$children + hotel_clean_2$babies
hotel_clean_2$total_nights <- hotel_clean_2$stays_in_weekend_nights + hotel_clean_2$stays_in_week_nights

hotel_clean_3 <- hotel_clean_2[hotel_clean_2$total_guests != 0, ]
View(hotel_clean_3)

sum(hotel_clean_3$total_guests == 0)

table(hotel_clean_3$hotel)
ggplot(hotel_clean_3, aes(x = hotel)) +
       geom_bar(fill = "skyblue") +
       labs(title = "Hotel Type Distribution",
            x = "Hotel Type",
            y = "Number of Bookings")

hotel_clean_3$arrival_date_month <- factor(
  hotel_clean_3$arrival_date_month,
  levels = c("January","February","March","April","May","June",
             "July","August","September","October","November","December")
)

monthly_bookings <- hotel_clean_3 %>%
  group_by(arrival_date_month) %>%
  summarise(total_bookings = n())
monthly_bookings

ggplot(monthly_bookings,
       aes(x = arrival_date_month, y = total_bookings)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Hotel Booking Demand by Month",
       x = "Month",
       y = "Number of Bookings") +
  theme_minimal()

monthly_bookings[which.max(monthly_bookings$total_bookings), ]
monthly_bookings[which.min(monthly_bookings$total_bookings), ]

ggplot(hotel_clean_3, aes(x = arrival_date_month, fill = hotel)) +
  geom_bar(position = "dodge") +
  labs(title = "Monthly Booking Demand by Hotel Type",
       x = "Month",
       y = "Number of Bookings",
       fill = "Hotel Type") +
  theme_minimal()
