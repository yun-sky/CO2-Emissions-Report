library(dplyr)
data <- read.csv("data/CO2 Emissions_Canada.csv", header=TRUE)

colnames(data) <- c('Make', 'Model', 'veh_class', '')

