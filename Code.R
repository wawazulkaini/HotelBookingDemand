#Install needed packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("plotly")

#Call packages library
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)

#Read dataset excel file
library(readxl)
hotel_data <- read_excel("C:/Users/HUAWEI/Downloads/hotel_bookings.xlsx")

str(hotel_data)
head(hotel_data)
colnames(hotel_data)
summary(hotel_data)

#hotel_clean <- hotel_data
#hotel_removed <- data.frame()
#rows_to_remove <- c()
#for (i in 2:nrow(hotel_data)) {
#     for (j in 1:(i-1)) {
#          if (hotel_data[i, 2] == hotel_data[j, 2]) {
#               rows_to_remove <- c(rows_to_remove, i)
#          }
#     }
#}
#hotel_clean <- hotel_clean[-rows_to_remove, ]
#View(hotel_clean)

#Check for dupicated data
sum(duplicated(hotel_data))
hotel_data[duplicated(hotel_data), ]

#Create new dataset with non-duplicated data
hotel_clean <- hotel_data %>%
     distinct()

sum(duplicated(hotel_clean))

#Remove unneeded data and transfer needed data to new dataset
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

#Change NULL in dataset to NA
hotel_clean_2[hotel_clean_2 == "NULL"] <- NA

#Check for missing values
colSums(is.na(hotel_clean_2))

#Calculate mean for children column
mean_children <- mean(hotel_clean_2$children, na.rm = TRUE)
mean_children <- round(mean_children)

#Change NA values in children column to mean
hotel_clean_2$children[is.na(hotel_clean_2$children)] <- mean_children

#Remove remaining rows with NA values
hotel_clean_2 <- na.omit(hotel_clean_2)

#Check for missing values again
colSums(is.na(hotel_clean_2))

#Create new columns total_guests, total_nights & Date
hotel_clean_2$total_guests <- hotel_clean_2$adults + hotel_clean_2$children + hotel_clean_2$babies
hotel_clean_2$total_nights <- hotel_clean_2$stays_in_weekend_nights + hotel_clean_2$stays_in_week_nights
hotel_clean_2$Date <- paste(hotel_clean_2$arrival_date_year,hotel_clean_2$arrival_date_month, hotel_clean_2$arrival_date_day_of_month)

#Format column Date
hotel_clean_2$Date <- as.Date(hotel_clean_2$Date, format = "%Y %B %d")

#Remove all rows where total_guests == 0
hotel_clean_3 <- hotel_clean_2[hotel_clean_2$total_guests != 0, ]
View(hotel_clean_3)

#Check for total_guests == 0
sum(hotel_clean_3$total_guests == 0)

europe_codes <- c("ALB", "AND", "AUT", "BEL", "BIH", "BGR", "HRV", "CYP", "CZE", "DNK",
                  "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ITA", "LVA",
                  "LIE", "LTU", "LUX", "MLT", "MNE", "NLD", "NOR", "POL", "PRT", "ROU",
                  "RUS", "SMR", "SRB", "SVK", "SVN", "ESP", "SWE", "CHE", "UKR", "GBR")


hotel_clean_3 <- hotel_clean_3 %>% filter(country %in% europe_codes)

#Create boxplot of adr
boxplot(hotel_clean_3$adr,
         main = "Boxplot of ADR",
         ylab = "ADR",
         col = "lightblue")

#Remove adr outliers
hotel_clean_3 <- hotel_clean_3 %>% filter(adr < 500)

#Create boxplot of total_guests
boxplot(hotel_clean_3$total_guests,
        main = "Boxplot of Total Guests",
        ylab = "Number of Guests",
        col = "lightgreen")

#Create boxplot of total_nights
boxplot(hotel_clean_3$total_nights,
        main = "Boxplot of Total Nights",
        ylab = "Number of Nights",
        col = "lightpink")

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
View(monthly_bookings)

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

ggplot(hotel_clean_3, aes(x = total_nights)) +
     geom_histogram(binwidth = 1) +
     labs(title = "Distribution of Length of Stay",
          x = "Total Nights",
          y = "Frequency")

avg_stay <- mean(hotel_clean_3$total_nights, na.rm = TRUE)
avg_stay

ggplot(hotel_clean_3, aes(x = total_guests)) +
    geom_histogram(binwidth = 1) +
    labs(title = "Guest Count Distribution",
         x = "Number of Guests",
         y = "Frequency")

write.csv(hotel_clean_3, "hotel_cleaned_data.csv", row.names = FALSE)
