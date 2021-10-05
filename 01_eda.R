#### Chapter 1 ####

#### Packages ####

pacman::p_load(matrixstats)

#### Data ####

state <- read.csv("data/state.csv")

##########################################################
#### Location stimates of Population and Murder rates ####
##########################################################

# Mean, trimmed mean and median 

mean(state$Population)

mean(state$Population, trim= 0.1)

median(state$Population)

# Weighted mean  and median (with matrixstats)

weighted.mean(state$Murder.Rate, w=state$Population)

matrixStats::weightedMedian(state$Murder.Rate, w=state$Population)

