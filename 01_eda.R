#### Chapter 1 ####

#### Packages ####

pacman::p_load(matrixstats, tidyverse, corrplot)

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

###########################################################
####################  Correlation  ########################
###########################################################

#Database contained at github repo did not match the book, therefore here is
#an adaptation from the available databases.

sp500_px <- read.csv("data/sp500_data.csv") %>% 
  
  pivot_longer(cols = !X, 
               names_to = "symbol") %>% 
  
  mutate(X = as.Date(X, format = "%Y-%m-%d")) %>% 
  
  filter(X > "2012-07-02")

url <- "https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/sp500_sectors.csv"

etfs <- read.csv(url) %>% 
  
  filter(sector == "etf") %>% 
  
  left_join(sp500_px, by = "symbol") %>% 
  
  pivot_wider(id_cols = "X", names_from = "symbol", values_from = "value") %>% 
  
  select(!X)

corrplot::corrplot(cor(etfs), method = "ellipse")


# Scatterplots

sp500_w <- read.csv("data/sp500_data.csv") %>% 
  
  pivot_longer(cols = !X, 
               names_to = "symbol") %>% 
  
  mutate(X = as.Date(X, format = "%Y-%m-%d")) %>% 
  
  filter(X > "2012-07-02") %>% 
  
  pivot_wider(id_cols = "X", names_from = "symbol", values_from = "value") %>% 
  
  select(!X)

plot(sp500_w$T, sp500_w$VZ, xlab = "ATT (T)", ylab = "Verizon (VZ)" )

# ggplot approach

ggplot(sp500_w, aes(x = T, y = VZ)) +
  
  geom_point(alpha = .2, size = 2.5) +
  
  scale_y_continuous(breaks = seq(from= -1.5, to= 1.5, by= .5)) +
  
  xlab("ATT (T)") +
  
  ylab("Verizon (VZ)") +
  
  theme_bw()

###########################################################
########### Exploring two or more variables  ##############
###########################################################

# Hexagonal Binning 

kc_tax <- read.csv("data/kc_tax.csv") 

kc_tax0 <- kc_tax %>% 
                  
  filter(TaxAssessedValue < 750000 & SqFtTotLiving > 100 & SqFtTotLiving < 3500)

nrow(kc_tax0)

ggplot(kc_tax0, aes(x=SqFtTotLiving, y= TaxAssessedValue)) +
  
  stat_binhex(color="white") +
  
  theme_bw() +
  
  scale_fill_gradient(low= "white", high = "black") +
  
  labs(x = "Finished Square Feet", y = "Tax-Assesed Value")

# Contours

ggplot(kc_tax0, aes(x = SqFtTotLiving, TaxAssessedValue)) +
  
  theme_bw() +
  
  geom_point(alpha = 0.1) +
  
  geom_density2d(color = "white") +
  
  labs(x = "Finished Square Feet", y = "Tax-Assessed Value")

# Contingency tables

#desc::CrossTable is recommended, I rather use janitor::tabyl

# Categorical and Numeric Data

air <- "https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/airline_stats.csv"

airline_stats <- read.csv(air) 

# Boxplot

boxplot(pct_carrier_delay ~ airline, data = airline_stats, ylim=c(0,50), xlab = "Airline", ylab= "Daily % of Delayed Flights")

# ggplot approach

ggplot(airline_stats, aes(x= airline, y = pct_carrier_delay)) +
  
  geom_boxplot(alpha = .2) +
  
  scale_y_continuous(breaks= seq(from= 0, to=50, by=10),limits = c(0,50)) +
  
  xlab("Airline") +
  
  ylab("Daily % of Delayed Flights")

# Violin plot

ggplot(airline_stats, aes(x= forcats::as_factor(airline), y = pct_carrier_delay)) +
  
  geom_violin(draw_quantiles = c(.25, .50, .75), linetype = "dashed", size = .75) +
  
  geom_violin(size = 1.75, alpha = 0) +
  
  scale_y_continuous(breaks= seq(from= 0, to=50, by=10),limits = c(0,50)) +
  
  xlab("") +
  
  ylab("Daily % of Delayed Flights") +
  
  theme_bw()

# Visualizing Multiple Variables

kc_tax0 %>% 
  
  filter(ZipCode == c(98188, 98105, 98108, 98126)) %>% 

ggplot(aes(x=SqFtTotLiving, y= TaxAssessedValue)) +
  
  stat_binhex(color="white") +
  
  theme_bw() +
  
  scale_fill_gradient(low= "white", high = "black") +
  
  labs(x = "Finished Square Feet", y = "Tax-Assesed Value") + 
  
  facet_wrap(~ZipCode) +
  
  theme(strip.text.x = element_text(
      size = 11))


