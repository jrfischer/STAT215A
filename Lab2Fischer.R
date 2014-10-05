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
setwd("/Users/jonathanfischer/Desktop/STAT215A/Lab 2")

# Make a histogram to look at the temperature distribution
hist(redwood.data$humid_temp)

# Calculate the default, "optimal" bandwidth used by the function so we can use adjust to control bandwidth
bandwidth_optimal <- bw.nrd0(redwood.data$humid_temp)

# Specialized Function that creates the kernel density estimate plots for this particular data
density_bandwidth_comparisons <- function(kernel_name, title){
  # Outputs a plot with the kernel density estimates of temperature for different bandwidths for a given
  # kernel. Inputs are kernel_name for desired kernel and title for plot title
  density_plot <- ggplot(redwood.data, aes(x = humid_temp)) + xlim(c(0,35)) + xlab("Air Temperature (ºC)") + 
    ylab("Density") + ggtitle(title)+  # Set temperature as the variable and choose appropriate limits. Label axes
    geom_density(kernel = kernel_name, adjust = .1, aes(colour=".1"), size=.5)+  # Generates KDEs for bandwidths
    geom_density(kernel = kernel_name, adjust = .2, aes(colour=".2"), size=.5)+
    geom_density(kernel = kernel_name,adjust = 1, aes(colour="1"), size=.5)+
    geom_density(kernel = kernel_name,adjust = 5, aes(colour="5"), size=.5)+
    geom_density(kernel = kernel_name,adjust = 10, aes(colour="10"), size=.5) + 
    scale_colour_manual(values=c(".1"="blue", ".2"="green", "1"="black", "5"="orange", "10"="red"),
                        name="Adjustments") +  # Make a legend corresponding to bandwidth
    geom_rug(alpha = .005)
  
  return(density_plot)
}

# Generates and stores comparison plots for four different kernels - gaussian, rectangular, e'kov, biweight
gaussian_temperature_densities <- density_bandwidth_comparisons("gaussian", "Gaussian Kernel Density Estimates")
rectangular_temperature_densities <- density_bandwidth_comparisons("rectangular", "Rectangular Kernel Density Estimates")
epanechnikov_temperature_densities <- density_bandwidth_comparisons("epanechnikov", "Epanechnikov Kernel Density Estimates")
biweight_temperature_densities <- density_bandwidth_comparisons("biweight", "Biweight Kernel Density Estimates")

# Prints plots
gaussian_temperature_densities 
dev.copy(png,'Temperature_Density_Guassian_Kernel.png')
dev.off()
rectangular_temperature_densities 
dev.copy(png,'Temperature_Density_Rectangular_Kernel.png')
dev.off()
epanechnikov_temperature_densities
dev.copy(png,'Temperature_Density_Epanechnikov_Kernel.png')
dev.off()
biweight_temperature_densities
dev.copy(png,'Temperature_Density_Biweight_Kernel.png')
dev.off()

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
  geom_point(color='black')+xlab("Air Temperature (ºC)")+ylab("Relative Humidity (%)")+
  ggtitle("Humidity vs Temperature at 10:05 AM")

# Specialized Function that plots 4 loess smoothers with different spans of given degree
loess_span_comparisons <- function(scatterplot, deg){
  # Outputs a plot with loess smoothers for 4 different spans and given degree. Scatterplot is the plot of
  # the two variables in question and deg is the desired degree of the smoother. Options are 0, 1, 2.
  loess_plot <- scatterplot +  # start with scatterplot and add smoothers with specified parameters
    stat_smooth(method = "loess", formula = y ~ x, se = F, aes(colour=".25"), span = .25, degree = deg) +
    stat_smooth(method = "loess", formula = y ~ x, se = F, aes(colour=".5"), span=.5, degree = deg) +
    stat_smooth(method = "loess", formula = y ~ x, se = F, aes(colour=".75"), span = .75, degree = deg) +
    stat_smooth(method = "loess", formula = y ~ x, se = F, aes(colour="1"), span = 1, degree = deg) +
    scale_colour_manual(values=c(".25"="red", ".5"="orange", ".75"="green", "1"="blue"),
                        name="Span") # add legend
  
  return(loess_plot)
}

# Create plots with loess smoothers added
loess.plot.degree.0 <- loess_span_comparisons(temp.humidity.plot, 0)
loess.plot.degree.1 <- loess_span_comparisons(temp.humidity.plot, 1)
loess.plot.degree.2 <- loess_span_comparisons(temp.humidity.plot, 2)

# Print these plots
loess.plot.degree.0 
dev.copy(png,'Humidity_Temperature_Loess_0.png')
dev.off()
loess.plot.degree.1 
dev.copy(png,'Humidity_Temperature_Loess_1.png')
dev.off()
loess.plot.degree.2 
dev.copy(png,'Humidity_Temperature_Loess_2.png')
dev.off()






lingData <- read.table('lingData.txt', header = T)
lingLocation <- read.table('lingLocation.txt', header = T)
load("question_data.RData")

sum(is.na(lingData[,5:73]))
sum(is.na(lingLocation))
no.na.in.row <- !rowSums(is.na(lingData[,5:73]))
lingData2 <- lingData[no.na.in.row,]
nrow(lingData2)+sum(!no.na.in.row)-nrow(lingData)
ling.data.stripped <- lingData2[,-c(1:4)]

# lingData has a column for each question, and lingLocation has a column
# for each question x answer.  Sorry the columns in lingLocation are not usefully named,
# but it's not too tricky to figure out which is which.
# Note that you still need to clean this data (check for NA's, missing location data, etc.)
names(lingData)
names(lingLocation)
state.df <- map_data("state")

blank.theme <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

############
# Make a plot similar to the website for the second person plural answers.
# You may want to join these data sets more efficiently than this.
plural.second.person <- filter(lingData, Q050 %in% c(6), long > -125)
answers.q50 <- all.ans[['50']]

# Make the column to join on.  They must be the same type.
answers.q50$Q050 <- rownames(answers.q50)
plural.second.person$Q050 <- as.character(plural.second.person$Q050)
plural.second.person <- inner_join(plural.second.person, answers.q50, by="Q050")

# Plot!
ggplot(data=NULL) +
  geom_point(data=plural.second.person, aes(x=long, y=lat, color=ans), size=3, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme



###############
# Plot the lingLocation data (which lives on a grid).  Note that this doesn't look great with
# state outlines.  You can probably do better!
ggplot(data=NULL) +
  geom_tile(data=filter(lingLocation, Longitude > -125),
            aes(x=Longitude, y=Latitude, color=log10(V12), fill=log10(V12))) +
  geom_polygon(data=state.df, colour = "gray", fill = NA, aes(x=long, y=lat, group=group)) +
  blank.theme


