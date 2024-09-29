library(dplyr)
library(lubridate)
library(ggplot2)

# Question 1: Make a graph that communicates about emissions from any 
#               countries of your choice. Explain how you considered 
#               principles of visualization in making your graph.

non_countries <- c("World", "Asia", "Europe", "North America",
        "Asia (excl. China & India)","EU-28","EU-27","Europe (excl. EU-27)", 
        "Europe (excl. EU-28)", "North America (excl. USA)", "Africa", 
        "South America", "International transport", "Oceania")

datCO2_countries <- datCO2 %>% filter(!(Entity %in% non_countries))
datCO2_countries$Entity <- as.character(datCO2_countries$Entity)
top_ten_since_1980 <- datCO2_countries %>% filter(Year >= 1980) %>%
  group_by(Entity) %>%
  summarise(tot_CO2 = sum(CO2)) %>%
  arrange(-tot_CO2) %>%
  top_n(10) %>% 
  select(Entity)  %>% pull()

datCO2_ten <- datCO2 %>% filter(Entity %in% top_ten_since_1980) %>% filter(Year >= 1980)
datCO2_ten <- datCO2_ten %>% group_by(Entity) %>% arrange(Year) %>% mutate(cumCO2 = cumsum(CO2))
library(randomcoloR)
pallette <- distinctColorPalette(10)

ggplot(datCO2_ten, aes(x = Year, y = cumCO2/1000000000, colour = Entity)) +
  geom_line() +
  labs(title = "Ten Biggest Emitters in the World Since 1980",
       y = bquote("Cumulative emissions (billions of tons "~CO[2]~")")) +
  scale_color_manual(values = pallette)+
  theme_classic()

# Question 2: You are tasked with communicating the change in world air 
#               temperatures and CO2 emissions to a broad audience in 
#               visually appealing graphs. Make two graphs to present in 
#               your word document side by side. Plot world CO2 emissions 
#               on one graph and world air temperature anomalies on the 
#               other graph.

worldCO2 <- datCO2 %>% filter(Entity == "World") %>% filter(Year >= 1880)

wpCO2 <- ggplot(worldCO2, aes(x = Year, y = CO2/1000000000,)) +
  geom_area(fill = "orchid") +
  labs(title =bquote("Year-Over_Year"~CO[2]~"Emissions"),
       y = bquote("Annual"~CO[2]~"Emissions (trillions of tons)")) +
  scale_color_manual(values = pallette)+
  theme_classic()

worldTemp <- hemCO2 %>% filter(Entity == "World")
worldTemp$Year <- year(worldTemp$Day)
worldTemp <- worldTemp %>% group_by(Year) %>% summarise(
  temp_anom = sum(temperature_anomaly)
)

wpT <- ggplot(worldTemp, aes(x = Year, y = temp_anom)) +
  geom_area(fill = "orchid", show.legend = F) +
  labs(title = "Year-Over-Year World Air Temperature",
       x = "Date",
       y = "Temperature Anomaly (C)") +
  scale_color_manual(values = pallette)+
  ylim(-13,13)+
  theme_classic()

#install.packages("ggpubr")
library(ggpubr)
ggarrange(wpCO2, wpT)
# Question 3: Look up any type of environmental data of your interest in 
#               our world in data (link in tutorial). Download the csv 
#               and upload it to RStudio Cloud. Remake the graph. 
#               You may make the graph exactly as it is or alter it to 
#               present the data in a different format. Explain how you 
#               considered principles of visualization in making your graph. 
#               Explain the main conclusion of the graph.

# Question 4: Copy the URL to your R script here.