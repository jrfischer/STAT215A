# STAT 215A Lab 2
# Jonathan Fischer. Oct 7, 2014

# Load some necessary packages
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(animation)

# Load the cleaned data from previous lab folder
setwd("/Users/jonathanfischer/Desktop/STAT215A/Lab 1")
redwood.data <- read.csv('redwoods_data.csv', header = TRUE)

# Make a histogram to look at the temperature distribution
hist(redwood.data$humid_temp)

# Calculate the default, "optimal" bandwidth used by the function so we can use adjust to control bandwidth
bandwidth_optimal <- bw.nrd0(redwood.data$humid_temp)

# Specialized Function that creates the kernel density estimate plots for this particular data
density_bandwidth_comparisons <- function(kernel_name, title){
  # Outputs a plot with the kernel density estimates of temperature for different bandwidths for a given
  # kernel. Inputs are kernel_name for desired kernel and title for plot title
  density_plot <- ggplot(redwood.data, aes(x = humid_temp)) + xlim(c(0,35)) + xlab("Air Temperature (C)") + 
    ylab("Density") + ggtitle(title)+  # Set temperature as the variable and choose appropriate limits. Label axes
    geom_density(kernel = kernel_name, adjust = .1, aes(colour=".1"), size=.5)+  # Generates KDEs for bandwidths
    geom_density(kernel = kernel_name, adjust = .2, aes(colour=".2"), size=.5)+
    geom_density(kernel = kernel_name,adjust = 1, aes(colour="1"), size=.5)+
    geom_density(kernel = kernel_name,adjust = 5, aes(colour="5"), size=.5)+
    geom_density(kernel = kernel_name,adjust = 10, aes(colour="10"), size=.5) + 
    scale_colour_manual(values=c(".1"="blue", ".2"="green", "1"="black", "5"="orange", "10"="red"),
                        name="Adjustments")  # Make a legend corresponding to bandwidth
  
  return(density_plot)
}

# Generates and stores comparison plots for four different kernels - gaussian, rectangular, e'kov, biweight
gaussian_temperature_densities <- density_bandwidth_comparisons("gaussian", "Gaussian Kernel Density Estimates")
rectangular_temperature_densities <- density_bandwidth_comparisons("rectangular", "Rectangular Kernel Density Estimates")
epanechnikov_temperature_densities <- density_bandwidth_comparisons("epanechnikov", "Epanechnikov Kernel Density Estimates")
biweight_temperature_densities <- density_bandwidth_comparisons("biweight", "Biweight Kernel Density Estimates")

# Prints plots
gaussian_temperature_densities 
rectangular_temperature_densities 
epanechnikov_temperature_densities
biweight_temperature_densities

# Quick and dirty plot to visualize data
plot(redwood.data$humid_temp, redwood.data$humidity, pch='.', col='blue')

# Which time of day to choose? Counts number of data points at each time and sorts in descending order
redwood.data$time <- redwood.data$epoch %% 288 # Converts epochs to one daily value
time.multiplicity <- group_by(redwood.data, "time") %>% summarise(n=n()) # Counts up data 
time.sorted <- arrange(time.multiplicity, -n) # Sorting process

# Choose a specific time of day
time.of.day <- 7
# Restrict our data to humidity and temperature at this time
redwood.data.common.time <- select(filter(redwood.data, time == time.of.day), humid_temp, humidity)

# Base scatterplot of chosen data
temp.humidity.plot <- ggplot(redwood.data.common.time, aes(x=humid_temp, y=humidity)) + 
  geom_point(color='black')+xlab("Air Temperature (C)")+ylab("Relative Humidity")+
  ggtitle("Humidity vs Temperature at 10:05 AM")

# Specialized Function that plots 6 loess smoothers with different spans of given degree
loess_span_comparisons <- function(scatterplot, deg){
  # Outputs a plot with loess smoothers for 6 different spans and given degree. Scatterplot is the plot of
  # the two variables in question and deg is the desired degree of the smoother. Options are 0, 1, 2.
  loess_plot <- scatterplot +  # start with scatterplot and add smoothers with specified parameters
    stat_smooth(method = "loess", formula = y ~ x, se = F, aes(colour=".25"), span = .25, degree = deg) +
    stat_smooth(method = "loess", formula = y ~ x, se = F, aes(colour=".5"), span=.5, degree = deg) +
    stat_smooth(method = "loess", formula = y ~ x, se = F, aes(colour=".75"), span = .75, degree = deg) +
    stat_smooth(method = "loess", formula = y ~ x, se = F, aes(colour="1"), span = 1, degree = deg) +
    stat_smooth(method = "loess", formula = y ~ x, se = F, aes(colour="2"), span = 2, degree = deg) +
    stat_smooth(method = "loess", formula = y ~ x, se = F, aes(colour="5"), span = 5, degree = deg) +
    scale_colour_manual(values=c(".25"="red", ".5"="orange", ".75"="yellow", "1"="green", "2"="blue", "5"="purple"),
                        name="Span") # add legend
  
  return(loess_plot)
}

# Create plots with loess smoothers added
loess.plot.degree.0 <- loess_span_comparisons(temp.humidity.plot, 0)
loess.plot.degree.1 <- loess_span_comparisons(temp.humidity.plot, 1)
loess.plot.degree.2 <- loess_span_comparisons(temp.humidity.plot, 2)

# Print these plots
loess.plot.degree.0 
loess.plot.degree.1 
loess.plot.degree.2 



