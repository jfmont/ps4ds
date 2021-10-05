#### Chapter 1 ####

#### Packages ####

pacman::p_load(matrixstats)

#### Data ####

state <- read.csv("data/state.csv")

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

# Percentiles and Boxplots

quantile(state$Murder.Rate, p = c(.05, .25, .50, .75, .95))

boxplot(state$Population / 1000000, ylab = "Population (millions")

# Frecuency Tables and Histograms

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


       