#Load data
Crash_data <- read.csv("C:/Users/jones/Downloads/Crash_Analysis_System_(CAS)_data.csv")
#population data from stats.govt.nz 
Population_nz <- read_excel("C:/Users/jones/OneDrive/Documents/Pop_NZ.xlsx")
#factors contributing to crashed percentages

#nz_2022_region <- read.csv("C:/Users/jones/Downloads/regional_council_2022_csv.csv")

#regional data as shapefile
nz_2022_region <- st_read("C:/Users/jones/Downloads/statsnz-regional-council-2022-generalised-SHP/regional-council-2022-generalised.shp")


#data info - investigating data
names(Crash_data)
unique(Crash_data$trafficControl)
Crash_data %>% tabyl(speedLimit) %>% adorn_pct_formatting(affix_sign = T)
Crash_data %>% tabyl(crashSeverity) %>% adorn_pct_formatting(affix_sign = T)
Crash_data %>% tabyl(holiday)
Crash_data %>% tabyl(region)

#road conditions 
Crash_data %>% tabyl(light)
Crash_data %>% tabyl(roadSurface)
Crash_data %>% tabyl(streetLight)
Crash_data %>% tabyl(weatherA)
Crash_data %>% tabyl(weatherB)
Crash_data %>% tabyl(tree)
Crash_data %>% tabyl(strayAnimal)
Crash_data %>% tabyl(slipOrFlood)

Crash_data %>% tabyl(roadworks)
Crash_data %>% tabyl(guardRail)
Crash_data %>% tabyl(trafficIsland)


# Load necessary libraries
library(ggplot2)
library(sf) 
library(leaflet)
library(janitor)
library(magrittr)
library(dplyr)
library(tidyr)
library(readxl)
library(stats)
library(MASS)
library(tidyverse)
library(RColorBrewer)

# plot with XY coordinates
Crash_data %>% filter(crashYear == "2022") %>% ggplot(aes(x = X, y = Y)) +
 geom_point(pch = 10, col = "blue") +
 labs(title = "XY Coordinate Plot")
#too much data

#fatal.. over years etc.


# Leaflet map
leaflet(Crash_data) %>%
  addTiles() %>% 
  addMarkers(~X, ~Y, popup = ~paste("Latitude:", Y, "<br>Longitude:", X))  

#total crashes - different regions. deaths per 100,00 people colored map
#data 2022
pop_2022 <- Population_nz %>% dplyr::select(Area, `2022`) %>% rename("region" = "Area", "population" = "2022")

fatal_2022 <- Crash_data %>% filter(crashYear == "2022", crashSeverity == "Fatal Crash", (!(region == ""))) %>% 
  tabyl(region) %>% left_join(pop_2022) %>% mutate(deaths_per_100k = n/population*100000) %>% data.frame() 

#nice table 
#region and Fatal Crashes per 100,000 people 


# Merge shapefile with fatal crashes data by region name
fatal_2022 <- merge(nz_2022_region, fatal_2022, by.x = "REGC2022_1", by.y = "region", all.x = TRUE)
fatal_2022 <- st_as_sf(fatal_2022)
fatal_2022_geographic <- st_transform(fatal_2022, crs = st_crs(4326))

#map
color_palette <- colorRampPalette(brewer.pal(9, "Blues"))

leaflet_object <- leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(data = fatal_2022_geographic, 
              fillColor = ~color_palette(100)[cut(deaths_per_100k, breaks = 100)], 
              fillOpacity = 0.8,
              weight = 1,
              color = "black",
              popup = ~paste("Region:", REGC2022_1, "<br>",
                             "Deaths per 100,000:", deaths_per_100k)) #%>% 
addLegend(position = "bottomright", 
            pal = colorNumeric(palette = color_palette(100), 
                               domain = fatal_2022_geographic$deaths_per_100k),
            values = ~deaths_per_100k,
            title = "Deaths per 100,000",
            opacity = 0.8)


#total crashes by year 
#removing 2023 data as it looks incomplete 
Year_count <- Crash_data %>% filter(!(crashYear == "2023")) %>% tabyl(crashYear)

ggplot(Year_count, aes(x = crashYear, y = n)) +
  geom_line(color = "lightblue", linewidth = 2) +
  geom_point(color = "lightblue", size = 4) +
  labs(title = "Total crashes by year", x = "Year", y = "Number of crashes") +
 theme_minimal() +
  theme(
    plot.title = element_text(family = "sans", size = 16, face = "bold"),  
    axis.title = element_text(family = "sans", size = 12), 
    axis.text = element_text(family = "sans", size = 10)  
  ) 
  

#Total crashes by region
#removing 2023 data as it looks incomplete 
Region_count <- Crash_data %>% filter(!(crashYear == "2023"), (!(region == ""))) %>% 
  tabyl(region, crashYear) %>% 
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "n")

ggplot(Region_count, aes(x = Year, y = n, group = region, color = region)) +
  geom_line(linewidth = 2) +
  geom_point(size = 4) +
  labs(title = "Total crashes per region by year", x = "Year", y = "Number of crashes") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "sans", size = 16, face = "bold"),  
    axis.title = element_text(family = "sans", size = 12), 
    axis.text = element_text(family = "sans", size = 10)  
  ) 




#total fatal by year


#number of crashes, fatalities, injuries, and types of crashes (e.g., single-vehicle, multi-vehicle) for each region in New Zealand.




#total fatal crashes by speed


#North and south island 


#Deaths and serious injury by region per 100 000 people 


#vehicle type / pedestrians (car, bike, train, motorcycle, bus, pedestrian, passenger, moped, other)

#infrastructure - road markings, road works

#regression model to predict crash severity based on factors like advisory speed, road surface, weather conditions


#Regression analysis

# Check for missing values
sum(is.na(Crash_data$crashSeverity))
sum(is.na(Crash_data$speedLimit))

#remove NA, make crash severity a factor (ordinal data)
cleaned_df <- Crash_data %>%  select(speedLimit, crashSeverity)  %>% na.omit()
cleaned_df$crashSeverity <- as.factor(cleaned_df$crashSeverity)
levels(cleaned_df$crashSeverity) <- c("Non-Injury Crash", "Minor Crash", "Serious Crash", "Fatal Crash")

# Fit an ordinal logistic regression model
model <- polr(crashSeverity ~ speedLimit, data = cleaned_df, Hess = TRUE)

# Summary of the ordinal logistic regression model
summary(model) #Crash severity as a function of speedlimits


######------------Creating a graph of this model--------
# Generate sequence of values for the predictor variable
Speed_seq <- seq(min(cleaned_df$speedLimit), max(cleaned_df$speedLimit), length.out = 100)

# Predict probabilities for each category of crash severity
predicted_probs <- predict(model, newdata = data.frame(speedLimit = Speed_seq), type = "probs")

# Plot the predicted probabilities
plot(Speed_seq, predicted_probs[,1], type = "l", col = "blue", ylim = c(0, 1), 
     xlab = "Speed Limit", ylab = "Predicted Probability", 
     main = "Predicted Probabilities of Crash Severity")
lines(advisorySpeed_seq, predicted_probs[,2], type = "l", col = "blue")
lines(advisorySpeed_seq, predicted_probs[,3], type = "l", col = "green")
lines(advisorySpeed_seq, predicted_probs[,4], type = "l", col = "purple")
legend("topright", legend = levels(model$y), col = c("red", "orange", "Yellow", "green"), lty = 1)

#Note this was not a useful model. 


#speed limit on number of crashes - as above with speed limit + number of crashes
speed_count <- Crash_data %>% filter(!(crashYear == "2023")) %>% 
  tabyl(speedLimit) %>% na.omit() %>% filter(!(speedLimit %in% c(2, 6, 51, 61))) #removing odd/ only one speedlimit 
model_2 <- lm(n ~ speedLimit, data = speed_count)
summary(model_2) 


#graph
options(scipen = 999)
plot(speed_count$speedLimit, speed_count$n, main = "Crash Number as a function of Speed Limit 2020 - 2022", ylab = "Number of Crashes", xlab = "Speed Limit", pch = 10, col = "blue") +
abline(model_2, col = "red") +
text(40, 100000, paste("Adjusted R-squared = -0.055"))
#par(font.axis = 2, cex.axis = 1.2, family = "Arial", las = 1)


#the greatest number of crashes occurring at 50Km/h, followed by 100km/h
#split analysis/ graphs into over and under 50km/h for clarity 


#same as above but split by crash severity 
speed_count_2 <- Crash_data %>% filter(!(crashYear == "2023")) %>% 
  tabyl(speedLimit, crashSeverity) %>% na.omit() %>% filter(!(speedLimit %in% c(2, 6, 51, 61))) %>% 
  pivot_longer(cols = c("Non-Injury Crash", "Minor Crash", "Serious Crash", "Fatal Crash"), 
               names_to = "Crash Severity", values_to = "n")

ggplot(speed_count_2, aes(x = speedLimit, y = n, fill = `Crash Severity`)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("darkgreen", "yellow", "orange", "darkred"), 
                    limits = c("Non-Injury Crash", "Minor Crash", "Serious Crash", "Fatal Crash")) +
  labs(title = "Total crashes as a function of speed, by severity", x = "Speed Limit", y = "Number of crashes")
 


#regression but for fatal and serious crashes only 
#speed limit on number of crashes - as above with speed limit + number of crashes
speed_count_fatal <- Crash_data %>% filter(!(crashYear == "2023"), crashSeverity == c("Serious Crash", "Fatal Crash")) %>% 
  tabyl(speedLimit) %>% na.omit() %>% filter(!(speedLimit %in% c(2, 6, 51, 61))) #removing odd/ only one speedlimit 
model_3 <- lm(n ~ speedLimit, data = speed_count_fatal)
summary(model_3) 


#graph
plot(speed_count_fatal$speedLimit, speed_count_fatal$n, main = "Fatal and serious crashes as a function of Speed Limit 2020 - 2022", ylab = "Number of Crashes", xlab = "Speed Limit", pch = 10, col = "blue") +
abline(model_3, col = "red") +
text(30, 2000, paste("Adjusted R-squared = 0.033"))


