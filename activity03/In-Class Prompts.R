#install.packages(c("dplyr","lubridate","ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
colnames(datCO2)[4] <- "CO2" 
datCO2$Entity <- as.factor(datCO2$Entity)
name.Ent <- levels(datCO2$Entity)

# Prompt 1: Make a plot of air temperature anomalies in the Northern and 
#             Southern Hemisphere in base R and in ggplot2.

hemCO2 <- read.csv("/cloud/project/activity03/climate-change.csv")
hemCO2$Day <- as.Date(hemCO2$Day)

NHem <- hemCO2 %>% filter(Entity == "Northern Hemisphere")
SHem <- hemCO2 %>% filter(Entity == "Southern Hemisphere")
# Base R
plot(NHem$Day, # x data
     NHem$temperature_anomaly, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     col = "darkred",
     ylab = "Temperature Anomaly", #y axis label
     xlab = "Year", #x axis label
)

points(SHem$Day, # x data
       SHem$temperature_anomaly, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkblue")

legend("topleft",
       c("Northern Hemisphere", "Southern Hemisphere"),
       col=c("darkred", "darkblue"),
       pch=19, bty= "n")

# ggplot
hemispheres <- hemCO2 %>% filter(Entity != "World")

ggplot(hemispheres, aes(x = Day, y = temperature_anomaly, colour = Entity)) +
  geom_point(alpha = 0.75) +
  labs(x = "Date",
       y = "Temperature Anomaly (C)")

# Prompt 2: Plot the total all time emissions for the United States, Mexico, 
#             and Canada.

options(scipen = 9999)
NA_nations <- datCO2 %>% filter(Entity %in% c("United States", "Mexico","Canada"))


ggplot(NA_nations, aes(x = Year, ymin = 0, ymax = CO2/1000000000, fill = Entity)) +
  geom_ribbon(alpha = 0.5) +
  labs(title = "Historic Emissions for North American Nations",
       y = bquote("Annual emissions (billions of tons "~CO[2]~")"))
 
# Optional: Try and find an answer through your search engine: how would you 
#             add subscripts for CO2 in your plot axes label?
  
bquote("Annual emissions (millions of tons "~CO[2]~")")
