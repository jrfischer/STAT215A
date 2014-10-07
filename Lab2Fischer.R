# STAT 215A Lab 2
# Jonathan Fischer. Oct 7, 2014

# Load some necessary packages
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(animation)
library(rgl)

# Part 1: Redwood Data Smoothing

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

# Part 2: Linguistic Data Analysis

# Load question and response data. Look at questions
lingData <- read.table('lingData.txt', header = T)
lingLocation <- read.table('lingLocation.txt', header = T)
load("question_data.RData")
View(quest.use)

# Remove rows with missing latitude and longitude. For the working dataset, 
# also remove ID, city, state, and zip since we use lat and long
sum(is.na(lingData[,5:73]))
no.na.in.row <- !rowSums(is.na(lingData[,5:73]))
lingData2 <- lingData[no.na.in.row,]
nrow(lingData2)+sum(!no.na.in.row)-nrow(lingData)
is.0 <- lingData2[,5:71] == 0
count.0.in.row <- rowSums(is.0)
lingData2 <- lingData2[count.0.in.row < 20,]
lat.and.long <- lingData2[,72:73]
ling.data.stripped <- lingData2[,-c(1:4, 72:73)]

# See how much of the original data we lose by removing rows in this manner
1-nrow(ling.data.stripped)/nrow(lingData)

binary.matrix <- apply(ling.data.stripped, 2, function(x) model.matrix(~factor(x)))
binary.matrix <- data.frame(binary.matrix)
intercept_indices <- which(colSums(binary.matrix)==nrow(binary.matrix))
binary.matrix <- binary.matrix[,-intercept_indices]

language_pca <- prcomp(binary.matrix, center = TRUE, scale. = FALSE)
summary(language_pca)

principal_components <- data.matrix(binary.matrix) %*% language_pca$rotation[,1:3]
principal_components <- data.frame(principal_components)

cumulative.var <- cumsum(language_pca$sdev ^ 2) / sum(language_pca$sdev ^ 2)
ggplot(data=NULL, aes(x=1:length(cumulative.var), y=cumulative.var)) +
  geom_point(size=1) + geom_line()+xlab('Number of Principal Components')+ylab('Cumulative Percentage of Variance')+
  ggtitle('Cumulative Percentage of Variance vs Number of Principal Components')

# Plot pairwise projections to PC

pca.plot.12 <- ggplot(principal_components, aes(x=PC1, y=PC2)) + 
  geom_point(color='black', size = 1, alpha = .5)+xlab("First Principal Component")+ylab("Second Principal Component")+
  ggtitle("Projection via Principal Components 1 and 2")
pca.plot.12

pca.plot.13 <- ggplot(principal_components, aes(x=PC1, y=PC3)) + 
  geom_point(color='black', size = 1, alpha = .5)+xlab("First Principal Component")+ylab("Third Principal Component")+
  ggtitle("Projection onto Principal Components 1 and 3")
pca.plot.13

pca.plot.23 <- ggplot(principal_components, aes(x=PC2, y=PC3)) + 
  geom_point(color='black', size = 1, alpha = .5)+xlab("Second Principal Component")+ylab("Third Principal Component")+
  ggtitle("Projection onto Principal Components 2 and 3")
pca.plot.23

plot3d(principal_components$PC1, principal_components$PC2, principal_components$PC3, col="blue", size=1)

state.df <- map_data("state")

blank.theme <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

k.means.3.clusters <- kmeans(principal_components, 3)
principal_components_clustered_3 <- cbind(principal_components,k.means.3.clusters$cluster, lat.and.long)
colnames(principal_components_clustered_3) <- c(colnames(principal_components), "cluster", colnames(lat.and.long))
principal_components_clustered_3 <- principal_components_clustered_3[principal_components_clustered_3$long>-125,]

ggplot(data=NULL) +
  geom_point(data = principal_components_clustered_3, aes(x=long, y=lat, color=factor(cluster)), size=2, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group = group)) +
  blank.theme 

pca.plot.12.clustered <- ggplot(principal_components_clustered_3, aes(x=PC1, y=PC2, color=factor(cluster))) + 
  geom_point(size = 1, alpha = .5)+xlab("First Principal Component")+ylab("Second Principal Component")+
  ggtitle("Projection via First and Second Principal Components")
pca.plot.12.clustered

pca.plot.13.clustered <- ggplot(principal_components_clustered_3, aes(x=PC1, y=PC3, color=factor(cluster))) + 
  geom_point(size = 1, alpha = .5)+xlab("First Principal Component")+ylab("Third Principal Component")+
  ggtitle("Projection via First and Third Principal Components")
pca.plot.13.clustered

pca.plot.23.clustered <- ggplot(principal_components_clustered_3, aes(x=PC2, y=PC3, color=factor(cluster))) + 
  geom_point(size = 1, alpha = .5)+xlab("Second Principal Component")+ylab("Third Principal Component")+
  ggtitle("Projection via Second and Third Principal Components")
pca.plot.23.clustered

plot3d(principal_components_clustered_3$PC1, principal_components_clustered_3$PC2, principal_components_clustered_3$PC3, size=1, xlab= 'First Principal Component'
       , ylab='Second Principal Component', zlab = 'Third Principal Component', col = principal_components_clustered_3$cluster)

k.means.4.clusters <- kmeans(principal_components, 4)
principal_components_clustered_4 <- cbind(principal_components,k.means.4.clusters$cluster, lat.and.long)
colnames(principal_components_clustered_4) <- c(colnames(principal_components), "cluster", colnames(lat.and.long))
principal_components_clustered_4 <- principal_components_clustered_4[principal_components_clustered_4$long>-125,]

ggplot(data=NULL) +
  geom_point(data = principal_components_clustered_4, aes(x=long, y=lat, color=factor(cluster)), size=2, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group = group)) +
  blank.theme 

subsample_size <- 10000
subsample <- sample_n(cbind(binary.matrix, lat.and.long), subsample_size)
latlongsub <- subsample[,469:470]
subsample <- subsample[,-c(469:470)]
pca_sub <- prcomp(subsample, center = TRUE, scale. = FALSE)
summary(pca_sub)

principal.components.sub <- data.matrix(subsample) %*% pca_sub$rotation[,1:3]
principal.components.sub <- data.frame(principal.components.sub)

kmeans.3.clusters.subsample <- kmeans(principal.components.sub, 3)
language_subsample <- cbind(principal.components.sub, kmeans.3.clusters.subsample$cluster, latlongsub)
colnames(language_subsample) <- c(colnames(principal.components.sub), "cluster", colnames(latlongsub))
language_subsample <- language_subsample[language_subsample$long>-125,]

ggplot(data=NULL) +
  geom_point(data = language_subsample, aes(x=long, y=lat, color=factor(cluster)), size=2, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group = group)) +
  blank.theme 

cutoff <- .3
absolute.loadings <- data.frame(abs(language_pca$rotation[,1:3]))
absolute.loadings$importance <- rowSums(absolute.loadings)
sum(absolute.loadings$importance > cutoff)
questions_to_examine <- which(absolute.loadings$importance > cutoff)

# Repeats PCA and K-means clustering for data containing only questions 50, 73, and 103
# Q050 - you/y'all, Q073 - tennis shoes/sneakers, Q103 - water/drinking fountain
cross.question.comparison <- filter(lingData2, Q050 %in% c(1,4,7,9), Q073 %in% c(1,6), Q103 %in% c(3,4),  long > -125)
cross.question.comparison <- select(cross.question.comparison, Q050, Q073, Q103, lat, long)
not.yall <- which(cross.question.comparison$Q050 !=9)
cross.question.comparison[c(not.yall), 1] <- 1
lat.long.cross <- cross.question.comparison[,c(4,5)]
cross.question.comparison <- cross.question.comparison[,-c(4,5)]
cross.matrix <- apply(cross.question.comparison, 2, function(x) model.matrix(~factor(x)-1))
cross.matrix <- data.frame(cross.matrix)
cross_pca <- prcomp(cross.matrix, center = TRUE, scale. = FALSE)
principal_components_cross <- data.matrix(cross.matrix) %*% cross_pca$rotation[,1:3]
principal_components_cross <- data.frame(principal_components_cross)
cumulative.var <- cumsum(cross_pca$sdev ^ 2) / sum(cross_pca$sdev ^ 2)
ggplot(data=NULL, aes(x=1:length(cumulative.var), y=cumulative.var)) +
  geom_point(size=1) + geom_line()+xlab('Number of Principal Components')+ylab('Cumulative Percentage of Variance')
k.means.cross <- kmeans(principal_components_cross, 3)
principal_components_cross_cluster <- cbind(principal_components_cross,k.means.cross$cluster, lat.long.cross)
colnames(principal_components_cross_cluster) <- c(colnames(principal_components_cross), "cluster", colnames(lat.and.long))
principal_components_cross_cluster <- principal_components_cross_cluster[principal_components_cross_cluster$long>-125,]
ggplot(data=NULL) +
  geom_point(data = principal_components_cross_cluster, aes(x=long, y=lat, color=factor(cluster)), size=2, alpha=0.5) +
  geom_polygon(data=state.df, colour = "black", fill = NA, aes(x=long, y=lat, group = group)) +
  blank.theme 

observations <- nrow(cross.question.comparison)
notyall.percentage <- sum(cross.question.comparison$Q050 == 1)/observations
yall.percentage <- sum(cross.question.comparison$Q050 == 9)/observations
sneakers.percentage <- sum(cross.question.comparison$Q073 == 1)/observations
tennis.shoes.percentage <- sum(cross.question.comparison$Q073 == 6)/observations
drinking.fountain.percentage <- sum(cross.question.comparison$Q103 == 3)/observations
water.fountain.percentage <- sum(cross.question.comparison$Q103 == 4)/observations

you.or.yall <- c(notyall.percentage, yall.percentage)
sneakers.or.tennis.shoes <- c(sneakers.percentage, tennis.shoes.percentage)
drinking.or.water <- c(drinking.fountain.percentage, water.fountain.percentage)

notyall.sneakers <- sum(cross.question.comparison$Q050 == 1 & cross.question.comparison$Q073 == 1)/observations
notyall.tennis.shoes <- sum(cross.question.comparison$Q050 == 1 & cross.question.comparison$Q073 == 6)/observations
notyall.drinking.fountain <- sum(cross.question.comparison$Q050 == 1 & cross.question.comparison$Q103 == 3)/observations
notyall.water.fountain <- sum(cross.question.comparison$Q050 == 1 & cross.question.comparison$Q103 == 4)/observations
yall.sneakers <- sum(cross.question.comparison$Q050 == 9 & cross.question.comparison$Q073 == 1)/observations
yall.tennis.shoes <- sum(cross.question.comparison$Q050 == 9 & cross.question.comparison$Q073 == 6)/observations
yall.drinking.fountain <- sum(cross.question.comparison$Q050 == 9 & cross.question.comparison$Q103 == 3)/observations
yall.water.fountain <- sum(cross.question.comparison$Q050 == 9 & cross.question.comparison$Q103 == 4)/observations
sneakers.drinking.fountain <- sum(cross.question.comparison$Q073 == 1 & cross.question.comparison$Q103 == 3)/observations
sneakers.water.fountain <- sum(cross.question.comparison$Q073 == 1 & cross.question.comparison$Q103 == 4)/observations
tennis.shoes.drinking.fountain <- sum(cross.question.comparison$Q073 == 6 & cross.question.comparison$Q103 == 3)/observations
tennis.shoes.water.fountain <- sum(cross.question.comparison$Q073 == 6 & cross.question.comparison$Q103 == 4)/observations

notyall.sneakers.drinking.fountain <- sum(cross.question.comparison$Q050 == 1 & cross.question.comparison$Q073 == 1 & cross.question.comparison$Q103 == 3)/observations
notyall.sneakers.water.fountain <- sum(cross.question.comparison$Q050 == 1 & cross.question.comparison$Q073 == 1 & cross.question.comparison$Q103 == 4)/observations
notyall.tennis.shoes.drinking.fountain <- sum(cross.question.comparison$Q050 == 1 & cross.question.comparison$Q073 == 6 & cross.question.comparison$Q103 == 3)/observations
notyall.tennis.shoes.water.fountain <- sum(cross.question.comparison$Q050 == 1 & cross.question.comparison$Q073 == 6 & cross.question.comparison$Q103 == 4)/observations
yall.sneakers.drinking.fountain <- sum(cross.question.comparison$Q050 == 9 & cross.question.comparison$Q073 == 1 & cross.question.comparison$Q103 == 3)/observations
yall.sneakers.water.fountain <- sum(cross.question.comparison$Q050 == 9 & cross.question.comparison$Q073 == 1 & cross.question.comparison$Q103 == 4)/observations
yall.tennis.shoes.drinking.fountain <- sum(cross.question.comparison$Q050 == 9 & cross.question.comparison$Q073 == 6 & cross.question.comparison$Q103 == 3)/observations
yall.tennis.shoes.water.fountain <- sum(cross.question.comparison$Q050 == 9 & cross.question.comparison$Q073 == 6 & cross.question.comparison$Q103 == 4)/observations

joint.all <- array(c(notyall.sneakers.drinking.fountain, notyall.sneakers.water.fountain, notyall.tennis.shoes.drinking.fountain, notyall.tennis.shoes.water.fountain,
                     yall.sneakers.drinking.fountain, yall.sneakers.water.fountain, yall.tennis.shoes.drinking.fountain, yall.tennis.shoes.water.fountain), dim = c(2,2,2)
                   )

joint.you.shoes <- matrix(c(notyall.sneakers, notyall.tennis.shoes, yall.sneakers, yall.tennis.shoes), nrow=2, ncol=2)
joint.you.fountain <- matrix(c(notyall.drinking.fountain, notyall.water.fountain, yall.drinking.fountain, yall.water.fountain), nrow=2, ncol=2)
joint.shoes.fountain <- matrix(c(sneakers.drinking.fountain, sneakers.water.fountain, tennis.shoes.drinking.fountain, tennis.shoes.water.fountain), nrow=2, ncol=2)
rownames(joint.you.shoes) <- c('Sneakers', 'Tennis Shoes')
colnames(joint.you.shoes) <- c('You, You guys, You all', 'Yall')
rownames(joint.you.fountain) <- c('Drinking fountain', 'Water fountain')
colnames(joint.you.fountain) <- c('You, You guys, You all', 'Yall')
rownames(joint.shoes.fountain) <- c('Sneakers', 'Tennis Shoes')
colnames(joint.shoes.fountain) <- c('Drinking fountain', 'Water fountain')

shoes.conditioned.on.you <- rbind(joint.you.shoes[,1]/you.or.yall[1], joint.you.shoes[,2]/you.or.yall[2])
you.conditioned.on.shoes <- cbind(joint.you.shoes[1,]/sneakers.or.tennis.shoes[1], joint.you.shoes[2,]/sneakers.or.tennis.shoes[2])

