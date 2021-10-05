#### Chapter 1 ####

#### Packages ####

pacman::p_load(matrixstats, tidyverse)

#### Data ####

urlfile <-  "https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/state.csv"

state <- read.csv(urlfile)

###########################################################
#### Location Estimates of Population and Murder rates ####
###########################################################

# Mean, trimmed mean and median 

mean(state$Population)

mean(state$Population, trim= 0.1)

median(state$Population)

# Weighted mean  and median (with matrixstats)

weighted.mean(state$Murder.Rate, w=state$Population)

matrixStats::weightedMedian(state$Murder.Rate, w=state$Population)

###########################################################
###### Variability Estimates of State Population ##########
###########################################################

# Sd, IQR, MAD

sd(state$Population)

IQR(state$Population)

mad(state$Population)

#As we can see, the sd is twice the mad (MAD is adjusted to be on the same scale as the mean), therefore MAD is more robust than sd. 

###########################################################
########## Exploring the Data Distribution ################
###########################################################

# Percentiles and Box plots

quantile(state$Murder.Rate, p = c(.05, .25, .50, .75, .95))

boxplot(state$Population / 1000000, ylab = "Population (millions")

# Frequency Tables and Histograms

breaks <- seq(
  from = min(state$Population),
  to = max(state$Population), 
  length = 11)

pop_freq <- cut(
  state$Population, 
  breaks = breaks,
  right = T, 
  include.lowest = T)

table(pop_freq)

# Histogram

hist(state$Population, breaks = breaks, xlab = "Population", main = NULL)

# Density Plots and Estimates

hist(state$Murder.Rate, freq = F, xlab = "Murder Rate (per 100,000)", ylab = "Density",  main = NULL) #Note freq = F, therefore histogram is plotted as a
lines(density(state$Murder.Rate), lwd = 3, col = "blue")

# Tidyverse approach

ggplot(state, aes(x = Murder.Rate)) +
  geom_histogram(aes(y=..density..), colour="black", fill="lightgrey", bins = 11) +
  geom_density(color = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0,11), breaks = seq(from= 0, to = 10, by = 2)) +
  scale_y_continuous(breaks = seq(from= 0., to= 0.20, by= 0.05)) +
  xlab("Murder Rate (per 100,000)") +
  ylab("Density") +
  labs(title = NULL) +
  theme_minimal()

###########################################################
#########  Exploring binary and categorical data###########
###########################################################

urlfile2 <- "https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/dfw_airline.csv"

dfw <- read.csv(urlfile2)


