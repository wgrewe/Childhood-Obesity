# D2P Project - Statistical Tests

library(sf)
library(rgdal)
library(smerc)
library(spdep)
install.packages("rgeos")
library(rgeos)
library(RColorBrewer)
library(plotrix)

getwd()
setwd("~/Desktop/Spatial Data/D2P")

# Load Data

obesity_data <- read.csv("childhood_obesity_2014_2016.csv")

utils::unzip("statistical_neighborhoods.zip")
shp <- readOGR("statistical_neighborhoods.shp")

# Correct "NULL"

obesity_data$NUMBER_OBESE[obesity_data$NUMBER_OBESE == "NULL"] <- NA
obesity_data$PERCENT_OBESE[obesity_data$PERCENT_OBESE == "None"] <- NA
obesity_data$COUNT_CHILDREN_INREGISTRYBMI[obesity_data$COUNT_CHILDREN_INREGISTRYBMI == "None"] <- NA

obesity_data$PERCENT_OBESE <- as.double(obesity_data$PERCENT_OBESE)
obesity_data$NUMBER_OBESE <- as.double(obesity_data$NUMBER_OBESE)
obesity_data$COUNT_CHILDREN_INREGISTRYBMI <- as.double(obesity_data$COUNT_CHILDREN_INREGISTRYBMI)

# Extract Relevant Info from SHP file

trueCentroids = gCentroid(shp,byid=TRUE)
coords <- trueCentroids@coords

NBHD_ID <- shp@data$NBHD_ID

filename <- system.file("D2P/statistical_neighborhoods.shp", package="sf")
shapes <- read_sf(dsn = '.', layer = "statistical_neighborhoods")

shp_data <- data.frame(NBHD_ID, coords, shapes$geometry)

# Filter and Combine Datasets
obesity_data_filter <- obesity_data[c("NBHD_ID", "COUNT_CHILDREN_INREGISTRYBMI","NUMBER_OBESE")]
data <- merge(shp_data, obesity_data_filter, by = "NBHD_ID", sort = TRUE)

data$COUNT_CHILDREN_INREGISTRYBMI <- as.double(data$COUNT_CHILDREN_INREGISTRYBMI)
data$NUMBER_OBESE <- as.double(data$NUMBER_OBESE)
data[is.na(data)] <- 0

save(data, file = "obesity_data_processed.RData")




