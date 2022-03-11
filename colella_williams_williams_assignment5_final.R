# import necessary libraries
library(bayesm)
library(corrplot)
library(MASS)
library(ggplot2)
library(tidyverse)
library(caTools)
library(corrplot)
library(psych)
library(vioplot)
library(DataExplorer)
library(lmtest)
library(rstan)
library(rstanarm)

# to prevent scientific notation, you can use a large value like 999
options(scipen=999)

# suppress warnings globally
options(warn=-1)


# helper functions for log price manipulations
n <- 7
# own brand: (1-1/n)*log(price)
own_brand <-function(logprice){
  (1-(1/n)) * logprice
  }

# competitive brand: -1/n*log(price)
compet_brand <- function(logprice){
  (-1/n) * logprice
  }


# load data
data(tuna)

# explore data
head(tuna) # preview data
names(tuna) # explore column names only
plot_str(tuna) # explore all the columns available and their types
plot_missing(tuna) # check for missing values
plot_histogram(tuna) # represent continuous variables through histograms to examine distribution of values
plot_density(tuna) # view instead via density plot
plot_correlation(tuna, type = 'continuous') # quick correlation plot across all variables
write.csv(tuna, 'tuna_original.csv', row.names=FALSE) # write full dataset to csv

# variables:
# WEEK
# MOVE:	unit sales of brand
# NSALE: display activity of brand
# LPRICE:	log price of brand
# FULLCUST:	total customer visits
# LWHPRIC: log wholesale price of brand

# brands:
# 1) Star Kist 6 oz.
# 2) Chicken of the Sea 6 oz.
# 3) Bumble Bee Solid 6.12 oz.
# 4) Bumble Bee Chunk 6.12 oz.
# 5) Geisha 6 oz.
# 6) Bumble Bee Large Cans.
# 7) H Chunk Lite 6.5 oz.

# plot all unit vs. log-price relationships -- as price increases, units should trend downward

# Star Kist 6 oz. units vs. price
ggplot(data = tuna, aes(y = MOVE1, x = LPRICE1))  + geom_point(col = 'blue') + geom_smooth(method = 'lm', col = 'red', size = 0.4)
# Chicken of the Sea 6 oz. units vs. price
ggplot(data = tuna, aes(y = MOVE2, x = LPRICE2))  + geom_point(col = 'blue') + geom_smooth(method = 'lm', col = 'red', size = 0.4)
# Bumble Bee Solid 6.12 oz.units vs. price
ggplot(data = tuna, aes(y = MOVE3, x = LPRICE3))  + geom_point(col = 'blue') + geom_smooth(method = 'lm', col = 'red', size = 0.4)
# Bumble Bee Chunk 6.12 oz. units vs. price
ggplot(data = tuna, aes(y = MOVE4, x = LPRICE4))  + geom_point(col = 'blue') + geom_smooth(method = 'lm', col = 'red', size = 0.4)
# Geisha 6 oz. units vs. price
ggplot(data = tuna, aes(y = MOVE5, x = LPRICE5))  + geom_point(col = 'blue') + geom_smooth(method = 'lm', col = 'red', size = 0.4)
# Bumble Bee Large Cans. units vs. price
ggplot(data = tuna, aes(y = MOVE6, x = LPRICE6))  + geom_point(col = 'blue') + geom_smooth(method = 'lm', col = 'red', size = 0.4)
# H Chunk Lite 6.5 oz. units vs. price
ggplot(data = tuna, aes(y = MOVE7, x = LPRICE7))  + geom_point(col = 'blue') + geom_smooth(method = 'lm', col = 'red', size = 0.4)

# plot unit sales over time to uncover trends
par(mfrow=c(2,2))
plot(tuna$WEEK,tuna$MOVE1,main = 'Star Kist 6 oz',type = 'l')
plot(tuna$WEEK,tuna$MOVE2,main = 'Chicken of the Sea 6 oz',type = 'l')
plot(tuna$WEEK,tuna$MOVE3,main = 'Bumble Bee Solid 6.12 oz',type = 'l')
plot(tuna$WEEK,tuna$MOVE4,main = 'Bumble Bee Chunk 6.12 oz',type = 'l')
plot(tuna$WEEK,tuna$MOVE5,main = 'Geisha 6 oz',type = 'l')
plot(tuna$WEEK,tuna$MOVE6,main = 'Bumble Bee Large Cans',type = 'l')
plot(tuna$WEEK,tuna$MOVE7,main = 'HH Chunk Lite 6.5 oz',type = 'l')

# violin plots to check spread of data for each brand
# violin plot is hybrid of box plot & a kernel density plot, which shows peaks in the data
par(mfrow=c(1,1))
x1 <- tuna$MOVE1
x2 <- tuna$MOVE2
x3 <- tuna$MOVE3
x4 <- tuna$MOVE4
x5 <- tuna$MOVE5
x6 <- tuna$MOVE6
x7 <- tuna$MOVE7
vioplot(x1, x2, x3, x4, x5, x6, x7, names=c("Star Kist 6 oz", "Chicken of the Sea 6 oz", 
                                            "Bumble Bee Solid 6.12 oz", "Bumble Bee Chunk 6.12 oz",
                                            "Geisha 6 oz", "Bumble Bee Large Cans", "HH Chunk Lite 6.5 oz"),
        col="green")
title("Violin Plots of Unit Sales by Brand")

# test normality of each column in dataset with the Shapiro-Wilk test
# p-value > 0.05 implies distribution of data are not significantly different from normal distribution, so can assume
# normality when this occurs
# it's based on the correlation between the data & the normal distribution
lshap <- lapply(tuna, shapiro.test)
lshap

# get summary of data
describe(tuna)

# EDA plot to view how many points in each bin
# 2D binning; improvement from histogram -- fill color to display how many points fall into each bin
ggplot(data = tuna) +
  geom_hex(mapping = aes(x = MOVE7, y = FULLCUST)) # example with units sales of star kist

# create log price dataframe
log_prices_df <- data.matrix(tuna[,grepl("LPRICE",names(tuna))]) 
head(log_prices_df)
summary(log_prices_df) # examine spread of price changes

# create a wholesale only dataframe
wholesale_df <- data.matrix(tuna[,grepl("LWHPRIC",names(tuna))])
head(wholesale_df)

whlsl_corr_df <- cor(log_prices_df, wholesale_df)
corrplot(whlsl_corr_df, order="hclust", type="upper")


# subset data to exclude wholesale data as wholesale is considerably different from traditional 
# channels and wholesale price & log price show strong correlation; also not interested in total 
# customer visits column
tuna <- tuna[1:22]
names(tuna)

# create log units dataframe
log_units_df <- apply(tuna[,grepl("MOVE",names(tuna))],2,log)
head(log_units_df)


# create display only dataframe
display_df <- data.matrix(tuna[,grepl("NSALE",names(tuna))])
head(display_df)


# plot unit sales trends
modeling_df <- cbind(x=tuna$WEEK, y=tuna[,grepl("MOVE",names(tuna))])

ggplot(modeling_df, aes(x, y = value, color = product)) + 
  geom_smooth(aes(y = y.MOVE1, col = "star_kist_units")) + 
  geom_smooth(aes(y = y.MOVE2, col = "chkn_sea_units")) +
  geom_smooth(aes(y = y.MOVE3, col = "bumble_solid_units")) +
  geom_smooth(aes(y = y.MOVE4, col = "bumble_chunk_units")) +
  geom_smooth(aes(y = y.MOVE5, col = "geisha_units")) +
  geom_smooth(aes(y = y.MOVE6, col = "bumble_large_units")) +
  geom_smooth(aes(y = y.MOVE7, col = "h_chunk_units")) +
  scale_x_continuous(name="week", limits=c(0, 400)) +
  scale_y_continuous(name="units", limits=c(0, 50000))


# rename original dataframe columns
names(tuna)[names(tuna) == "MOVE1"] <- "star_kist_units"
names(tuna)[names(tuna) == "MOVE2"] <- "chkn_sea_units"
names(tuna)[names(tuna) == "MOVE3"] <- "bumble_solid_units"
names(tuna)[names(tuna) == "MOVE4"] <- "bumble_chunk_units"
names(tuna)[names(tuna) == "MOVE5"] <- "geisha_units"
names(tuna)[names(tuna) == "MOVE6"] <- "bumble_large_units"
names(tuna)[names(tuna) == "MOVE7"] <- "h_chunk_units"
names(tuna)[names(tuna) == "NSALE1"] <- "star_kist_display"
names(tuna)[names(tuna) == "NSALE2"] <- "chkn_sea_display"
names(tuna)[names(tuna) == "NSALE3"] <- "bumble_solid_display"
names(tuna)[names(tuna) == "NSALE4"] <- "bumble_chunk_display"
names(tuna)[names(tuna) == "NSALE5"] <- "geisha_display"
names(tuna)[names(tuna) == "NSALE6"] <- "bumble_large_display"
names(tuna)[names(tuna) == "NSALE7"] <- "h_chunk_display"
names(tuna)[names(tuna) == "LPRICE1"] <- "star_kist_price"
names(tuna)[names(tuna) == "LPRICE2"] <- "chkn_sea_price"
names(tuna)[names(tuna) == "LPRICE3"] <- "bumble_solid_price"
names(tuna)[names(tuna) == "LPRICE4"] <- "bumble_chunk_price"
names(tuna)[names(tuna) == "LPRICE5"] <- "geisha_price"
names(tuna)[names(tuna) == "LPRICE6"] <- "bumble_large_price"
names(tuna)[names(tuna) == "LPRICE7"] <- "h_chunk_price"

# write cleaned dataframe to csv
write.csv(tuna, 'tuna_filtered.csv', row.names=FALSE)


# examine correlations between price & display activity
# due to high collinearity, we can ignore display activity of a product itself when including its 
# price in the same elasticity model; we can, however, include the display activity of competing 
# products
corr_df <- cor(log_prices_df, display_df)
corrplot(corr_df, order="hclust", type="upper")
corr_df

# examine correlations between price & units -- this is the relationship we aim to better understand
corr_df <- cor(log_prices_df, log_units_df)
corrplot(corr_df, order="hclust", type="upper")
corr_df

# log unit sales price elasticity: multiple regression
# ***ordered by complexity***

# summary is used repeatedly and will provide results of the regression analysis, including estimates for each of the beta values
# as you add predictors, multiple r-squared will always increase since a predictor will always explain some portion of variance
# adjusted r-squared controls for this, even adding penalties for # of predictors in model
# thus, adjusted Rrsquared can be (-) but is usually not; it's always lower than the r-squared

# key to the analysis are the parameter estimates: the values for b0 (intercept) and b (coefficients)
# generally, they will tell us that a one-unit increase in each respective predictor results in a respective beta coefficient
# increase/decrease in log-units
# so, coefficients all represent change in dependent variable y w/ respect to x

# simple linear models: just using product's units and that product's price

# Star Kist 6 oz.
summary(lm(log_units_df[,c(1)]~log_prices_df[,c(1)]))
# Chicken of the Sea 6 oz.
summary(lm(log_units_df[,c(2)]~log_prices_df[,c(2)]))
# Bumble Bee Solid 6.12 oz.
summary(lm(log_units_df[,c(3)]~log_prices_df[,c(3)]))
# Bumble Bee Chunk 6.12 oz.
summary(lm(log_units_df[,c(4)]~log_prices_df[,c(4)]))
# Geisha 6 oz.
summary(lm(log_units_df[,c(5)]~log_prices_df[,c(5)]))
# Bumble Bee Large Cans.
summary(lm(log_units_df[,c(6)]~log_prices_df[,c(6)]))
# H Chunk Lite 6.5 oz.
summary(lm(log_units_df[,c(7)]~log_prices_df[,c(7)]))


# slightly more complex: product's units, product's price & display of other product's since there
# is high collinearity betw. a product's price and it's own display activity -- adjusted R squared 
# increases universally

# Star Kist 6 oz.
summary(lm(log_units_df[,c(1)]~log_prices_df[,c(1)]+display_df[,c(2,3,4,5,6,7)]))
# Chicken of the Sea 6 oz.
summary(lm(log_units_df[,c(2)]~log_prices_df[,c(2)]+display_df[,c(1,3,4,5,6,7)]))
# Bumble Bee Solid 6.12 oz.
summary(lm(log_units_df[,c(3)]~log_prices_df[,c(3)]+display_df[,c(1,2,4,5,6,7)]))
# Bumble Bee Chunk 6.12 oz.
summary(lm(log_units_df[,c(4)]~log_prices_df[,c(4)]+display_df[,c(1,2,3,5,6,7)]))
# Geisha 6 oz.
summary(lm(log_units_df[,c(5)]~log_prices_df[,c(5)]+display_df[,c(1,2,3,4,6,7)]))
# Bumble Bee Large Cans.
summary(lm(log_units_df[,c(6)]~log_prices_df[,c(6)]+display_df[,c(1,2,3,4,5,7)]))
# H Chunk Lite 6.5 oz.
summary(lm(log_units_df[,c(7)]~log_prices_df[,c(7)]+display_df[,c(1,2,3,4,5,6)]))


# slightly more complex: using product's units and all product prices -- adjusted R squared 
# increases universally

# Star Kist 6 oz.
summary(lm(log_units_df[,c(1)]~log_prices_df))
# Chicken of the Sea 6 oz.
summary(lm(log_units_df[,c(2)]~log_prices_df))
# Bumble Bee Solid 6.12 oz.
summary(lm(log_units_df[,c(3)]~log_prices_df))
# Bumble Bee Chunk 6.12 oz.
summary(lm(log_units_df[,c(4)]~log_prices_df))
# Geisha 6 oz.
summary(lm(log_units_df[,c(5)]~log_prices_df))
# Bumble Bee Large Cans.
summary(lm(log_units_df[,c(6)]~log_prices_df))
# H Chunk Lite 6.5 oz.
summary(lm(log_units_df[,c(7)]~log_prices_df))


# more complex: using product's units, all product prices & just display activity for competing products
# again, ignoring display of a product in its own model given that its
# price is already included & that price is already highly correlated with its display activity
# adjusted r-squared does improve, albeit marginally in some cases

# Star Kist 6 oz.
summary(lm(log_units_df[,c(1)]~log_prices_df+display_df[,c(2,3,4,5,6,7)]))
# Chicken of the Sea 6 oz.
summary(lm(log_units_df[,c(2)]~log_prices_df+display_df[,c(1,3,4,5,6,7)]))
# Bumble Bee Solid 6.12 oz.
summary(lm(log_units_df[,c(3)]~log_prices_df+display_df[,c(1,2,4,5,6,7)]))
# Bumble Bee Chunk 6.12 oz.
summary(lm(log_units_df[,c(4)]~log_prices_df+display_df[,c(1,2,3,5,6,7)]))
# Geisha 6 oz.
summary(lm(log_units_df[,c(5)]~log_prices_df+display_df[,c(1,2,3,4,6,7)]))
# Bumble Bee Large Cans.
summary(lm(log_units_df[,c(6)]~log_prices_df+display_df[,c(1,2,3,4,5,7)]))
# H Chunk Lite 6.5 oz.
summary(lm(log_units_df[,c(7)]~log_prices_df+display_df[,c(1,2,3,4,5,6)]))


# final refinement: same model as above but removing display info. for products 3 & 5 as well
# b/c they are both >= .7 in terms of correlation betw. price & display; they are the most highly 
# correlated

# the below decreases in adj. r-squared are so small that they almost don't matter; no material negative impact by removing highly collinear features

# Star Kist 6 oz.
summary(lm(log_units_df[,c(1)]~log_prices_df+display_df[,c(2,4,6,7)])) # adj. r-squared increases
star_kist_lm <- lm(log_units_df[,c(1)]~log_prices_df+display_df[,c(2,4,6,7)])
star_kist_lm$coefficients

plot(x = log_units_df[,c(1)],                         
     y = star_kist_lm$fitted.values,
     xlab = "True Values",
     ylab = "Fitted Values",
     main = "Regression fits of Log Units")
abline(b = 1, a = 0)

plot(star_kist_lm$residuals)

# Chicken of the Sea 6 oz.
summary(lm(log_units_df[,c(2)]~log_prices_df+display_df[,c(1,4,6,7)])) # adj. r_squared decreases by 0.0001
chkn_sea_lm <- lm(log_units_df[,c(2)]~log_prices_df+display_df[,c(1,4,6,7)])
chkn_sea_lm$coefficients

plot(x = log_units_df[,c(2)],                         
     y = chkn_sea_lm$fitted.values,
     xlab = "True Values",
     ylab = "Fitted Values",
     main = "Regression fits of Log Units")
abline(b = 1, a = 0)        

plot(chkn_sea_lm$residuals)

# Bumble Bee Solid 6.12 oz.
summary(lm(log_units_df[,c(3)]~log_prices_df+display_df[,c(1,2,4,6,7)])) # adj. r-squared increases but still low
bumble_solid_lm <- lm(log_units_df[,c(3)]~log_prices_df+display_df[,c(1,2,4,6,7)])
bumble_solid_lm$coefficients

plot(x = log_units_df[,c(3)],                         
     y = bumble_solid_lm$fitted.values,
     xlab = "True Values",
     ylab = "Fitted Values",
     main = "Regression fits of Log Units")
abline(b = 1, a = 0)        

plot(bumble_solid_lm$residuals)

# Bumble Bee Chunk 6.12 oz.
summary(lm(log_units_df[,c(4)]~log_prices_df+display_df[,c(1,2,6,7)])) # adj. r-squared increases
bumble_chunk_lm <- lm(log_units_df[,c(4)]~log_prices_df+display_df[,c(1,2,6,7)])
bumble_chunk_lm$coefficients

plot(x = log_units_df[,c(4)],                         
     y = bumble_chunk_lm$fitted.values,
     xlab = "True Values",
     ylab = "Fitted Values",
     main = "Regression fits of Log Units")
abline(b = 1, a = 0)     

plot(bumble_chunk_lm$residuals)

# Geisha 6 oz.
summary(lm(log_units_df[,c(5)]~log_prices_df+display_df[,c(1,2,4,6,7)])) # adj. r-squared decreases by 0.0035
geisha_lm <- lm(log_units_df[,c(5)]~log_prices_df+display_df[,c(1,2,4,6,7)])
geisha_lm$coefficients

plot(x = log_units_df[,c(5)],                         
     y = geisha_lm$fitted.values,
     xlab = "True Values",
     ylab = "Fitted Values",
     main = "Regression fits of Log Units")
abline(b = 1, a = 0)     

plot(geisha_lm$residuals)

# Bumble Bee Large Cans.
summary(lm(log_units_df[,c(6)]~log_prices_df+display_df[,c(1,2,4,7)])) # adj. r-squared dereases by 0.00708 but still low
bumble_large_lm <- lm(log_units_df[,c(6)]~log_prices_df+display_df[,c(1,2,4,7)])
bumble_large_lm$coefficients

plot(x = log_units_df[,c(6)],                         
     y = bumble_large_lm$fitted.values,
     xlab = "True Values",
     ylab = "Fitted Values",
     main = "Regression fits of Log Units")
abline(b = 1, a = 0)     

plot(bumble_large_lm$residuals)

# H Chunk Lite 6.5 oz.
summary(lm(log_units_df[,c(7)]~log_prices_df+display_df[,c(1,2,4,6)])) # adj. r-squared decreases by 0.0117 but still low
h_chunk_lm <- lm(log_units_df[,c(7)]~log_prices_df+display_df[,c(1,2,4,6)])
h_chunk_lm$coefficients

plot(x = log_units_df[,c(7)],                         
     y = h_chunk_lm$fitted.values,
     xlab = "True Values",
     ylab = "Fitted Values",
     main = "Regression fits of Log Units")
abline(b = 1, a = 0)     

plot(h_chunk_lm$residuals)

# the above items with the smallest adj. r-squared have flatter price change trends, which is no surprise


# ***FINAL MODELS***
# *** same models as above but with removal of items with elasticities that do not make sense***
# for Breush-Pagan test for heteroscedasticity, null hypothesis = variance of residuals is constant

# Star Kist 6 oz.
summary(lm(log_units_df[,c(1)]~log_prices_df[,c(1,2,4,5,7)]+display_df[,c(2,4,6)])) # adj. r-squared increases slightly
star_kist_lm <- lm(log_units_df[,c(1)]~log_prices_df[,c(1,2,4,5,7)]+display_df[,c(2,4,6)]) 
plot(star_kist_lm$residuals)
par(mfrow=c(2,2))
plot(star_kist_lm)
lmtest::bptest(star_kist_lm) # Breush-Pagan test for heteroscedasticity; reject null that variance of residuals is constant 

# Chicken of the Sea 6 oz.
summary(lm(log_units_df[,c(2)]~log_prices_df[,c(1,2,4,5)]+display_df[,c(1,4,6,7)])) # adj. r-squared decreases slightly
chkn_sea_lm <- lm(log_units_df[,c(2)]~log_prices_df[,c(1,2,4,5)]+display_df[,c(1,4,6,7)])
plot(chkn_sea_lm$residuals)
par(mfrow=c(2,2))
plot(chkn_sea_lm)
lmtest::bptest(chkn_sea_lm) # Breush-Pagan test for heteroscedasticity; reject null that variance of residuals is constant 

# Bumble Bee Solid 6.12 oz.
summary(lm(log_units_df[,c(3)]~log_prices_df[,c(1,3,5,6)]+display_df[,c(2,7)])) # adj. r-squared decreases by 0.10
bumble_solid_lm <- lm(log_units_df[,c(3)]~log_prices_df[,c(2,7)])
plot(bumble_solid_lm$residuals)
par(mfrow=c(2,2))
plot(bumble_solid_lm)
lmtest::bptest(bumble_solid_lm) # Breush-Pagan test for heteroscedasticity; reject null that variance of residuals is constant 

# Bumble Bee Chunk 6.12 oz.
summary(lm(log_units_df[,c(4)]~log_prices_df[,c(1,2,4)]+display_df[,c(1,2,6,7)])) # adj. r-squared increases slightly
bumble_chunk_lm <- lm(log_units_df[,c(4)]~log_prices_df[,c(1,2,4)]+display_df[,c(1,2,6,7)])
plot(bumble_chunk_lm$residuals)
par(mfrow=c(2,2))
plot(bumble_chunk_lm)
lmtest::bptest(bumble_chunk_lm) # Breush-Pagan test for heteroscedasticity; reject null that variance of residuals is constant 

# Geisha 6 oz.
summary(lm(log_units_df[,c(5)]~log_prices_df[,c(3,5)]+display_df[,c(1,2,4,6)])) # adj. r-squared decreases slightly
geisha_lm <- lm(log_units_df[,c(5)]~log_prices_df[,c(3,5)])
plot(geisha_lm$residuals)
par(mfrow=c(2,2))
plot(geisha_lm)
lmtest::bptest(geisha_lm) # Breush-Pagan test for heteroscedasticity; fail to reject null that variance of residuals is constant 

# Bumble Bee Large Cans.
summary(lm(log_units_df[,c(6)]~log_prices_df[,c(1,5,6)]+display_df[,c(7)])) # adj. r-squared decreases by 0.08
bumble_large_lm <- lm(log_units_df[,c(6)]~log_prices_df[,c(1,2,5,6)]+display_df[,c(7)])
plot(bumble_large_lm$residuals)
par(mfrow=c(2,2))
plot(bumble_large_lm)
lmtest::bptest(bumble_large_lm) # Breush-Pagan test for heteroscedasticity; fail to reject null that variance of residuals is constant 

# H Chunk Lite 6.5 oz.
summary(lm(log_units_df[,c(7)]~log_prices_df[,c(1,5,7)]+display_df[,c(1,2,4,6)])) # adj. r-squared decreases by 0.05
h_chunk_lm <- lm(log_units_df[,c(7)]~log_prices_df[,c(1,3,4,5,7)]+display_df[,c(1,2,4,6)])
plot(h_chunk_lm$residuals)
par(mfrow=c(2,2))
plot(h_chunk_lm)
lmtest::bptest(h_chunk_lm) # Breush-Pagan test for heteroscedasticity; reject null that variance of residuals is constant 


# build total unit sales df
unit_sales_df <- tuna[,grepl("_units",names(tuna))]
unit_sales_df['total_units'] <- apply(unit_sales_df,1,sum)
head(unit_sales_df)


# build market share dataframe
share_df <- unit_sales_df[,grepl("_units",names(unit_sales_df))]/unit_sales_df[,'total_units']
share_df['geometric_mean'] <- apply(share_df, 1, geometric.mean) # geometric mean is sometimes used to average ratios and percent changes
head(share_df)


# build log-centered shares dataframe
log_cent_shares <- log(share_df[,grepl("_units",names(share_df))]/share_df[,'geometric_mean'])
head(log_cent_shares)


# price elasticity for market share

# build dataframes with log price manipulations

# first for each brand as itself
brand_price_df <- own_brand(log_prices_df)
colnames(brand_price_df) <- c("star_kist_price","chkn_sea_price","bumble_solid_price", "bumble_chunk_price",
                              "geisha_price", "bumble_large_price","h_chunk_price")
head(brand_price_df)

# next for each brand as a competitor
competitor_price_df <- compet_brand(log_prices_df)
colnames(competitor_price_df) <- c("star_kist_price","chkn_sea_price","bumble_solid_price", "bumble_chunk_price",
                              "geisha_price", "bumble_large_price","h_chunk_price")
head(competitor_price_df)


# Star Kist 6 oz.
centered_share <- log_cent_shares[,1]
log_price <- brand_price_df[, 1]
competitor_prices <- competitor_price_df[,2:7]
summary(lm(centered_share ~ log_price + competitor_prices))

# Chicken of the Sea 6 oz.
centered_share <- log_cent_shares[,2]
log_price <- brand_price_df[, 2]
competitor_prices <- competitor_price_df[,-2] 
summary(lm(centered_share ~ log_price + competitor_prices))

# Bumble Bee Solid 6.12 oz.
centered_share <- log_cent_shares[,3]
log_price <- brand_price_df[, 3]
competitor_prices <- competitor_price_df[,-3] 
summary(lm(centered_share ~ log_price + competitor_prices))

# Bumble Bee Chunk 6.12 oz.
centered_share <- log_cent_shares[,4]
log_price <- brand_price_df[, 4]
competitor_prices <- competitor_price_df[,-4] 
summary(lm(centered_share ~ log_price + competitor_prices))

# Geisha 6 oz.
centered_share <- log_cent_shares[,5]
log_price <- brand_price_df[, 5]
competitor_prices <- competitor_price_df[,-5] 
summary(lm(centered_share ~ log_price + competitor_prices))

# Bumble Bee Large Cans.
centered_share <- log_cent_shares[,6]
log_price <- brand_price_df[, 6]
competitor_prices <- competitor_price_df[,-6] 
summary(lm(centered_share ~ log_price + competitor_prices))

# H Chunk Lite 6.5 oz.
centered_share <- log_cent_shares[,7]
log_price <- brand_price_df[, 7]
competitor_prices <- competitor_price_df[,-7] 
summary(lm(centered_share ~ log_price + competitor_prices))


# create a nice markdown of the analysis
setwd("/Users/colella2/Google Drive/Graduate School/MScA/Courses/Marketing Analytics/colella_docs/assignments/assignment_5")

create_report(tuna, output_file = "assignment_5_report.html",  output_dir = getwd(), 
              report_title = "Tuna Data Profiling Report")

# Given the various models in this analysis, it is best to use the "final refinement" sales price elasticity models POST removal of
# elasticities that do not make sense. The reasoning behind this is that the cross-price elasticities as well as price elasticities & display related 
# elasticties MUST make sense. In other words, a one unit increase in price should lead to a decrease in units and a one unit increase in a competitor's 
# price should lead to an increase in target brand's units. Not all of the coefficients makes complete sense across all models, so it might warrant 
# further investigation as to true competitors. Among sales price elasticity models, the "final refinement" models have
# the highest adj. r-squared values while accounting for highly collinear features. Also, adj. r-squared in these models is competitive 
# as compared to respective market share versions of the models, if not better. As a reminder, the models labeled "***FINAL MODELS**
# are the best ones