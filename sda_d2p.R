library(sf)
library(rgdal)
library(RColorBrewer)

# Load Data---------------------------------------------------------------------

obesity_data <- read.csv("~/Desktop/Spatial Data/D2P/childhood_obesity_2014_2016.csv")

fn <- file.path(tempdir(), "~/Desktop/statistical_neighborhoods.shp")

utils::unzip("~/Desktop/Spatial Data/D2P/statistical_neighborhoods.zip")
shp <- readOGR("~/Desktop/Spatial Data/D2P/statistical_neighborhoods.shp")

utils::unzip("~/Desktop/Spatial Data/D2P/obesityplot.zip")
shp1 <- readOGR("~/Desktop/Spatial Data/D2P/obesityplot/childhood_obesity_2014_2016.shp")

# Correct formatting-----------------------------------------------------------

obesity_data$NUMBER_OBESE[obesity_data$NUMBER_OBESE == "NULL"] <- NA

# Combine statistical neighborhoods with obesity data--------------------------

shp@data <- merge(shp@data, obesity_data, by = "NBHD_ID")

# Correct formatting-----------------------------------------------------------

obesity_data$PERCENT_OBESE[obesity_data$PERCENT_OBESE == "None"] <- NA
shp@data$PERCENT_OBESE <- as.double(obesity_data$PERCENT_OBESE)
shp@data$NUMBER_OBESE <- as.integer(obesity_data$NUMBER_OBESE)

# Use st_read for choropleth---------------------------------------------------
data1 <- st_read("~/Desktop/Spatial Data/D2P/obesityplot/")
plot(data1["PERCENT_OB"], pal = viridisLite::cividis, 
     main = "Choropleth Map of Obesity Percentage")

data1$NUM_OBESE <- data1$PERCENT_OB * data1$COUNT_CHIL
plot(data1["NUM_OBESE"], pal = viridisLite::cividis, 
     main = "Choropleth Map of Obesity Counts")
