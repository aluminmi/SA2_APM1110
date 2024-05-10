library(DataCombine)
library(anytime)
library(plyr)
library(dplyr)
library(dgof)
library(fitdistrplus)
library(VGAM)
library(DataCombine)
library(data.table)
library(EnvStats)
library(ggplot2)
library(tsallisqexp)
library(poweRlaw)
library(fitur)
library(fitdistrplus)

df_btc <- read.csv("C:/Users/naomi/Documents/SA2_Cuerdo_Morillo/1day_BTC_USD.csv")
summary(df_btc)

x_limits <- c(-0.3, 0.3)
#histogram
df_btc$Change<-as.numeric(gsub("%", "",df_btc$Change)) / 100
hist(df_btc$Change, 
     main = "Histogram of Percent Change", 
     xlab = "Percentage Change", 
     ylab = "Returns", 
     col = alpha("purple", 0.5),  # Adjust transparency
     breaks = 400,  # Reduce breaks for better visibility
     xlim = x_limits)

#Normal Distri Curve
shifted_normal_pdf <- function(x) {
  dnorm((x) / 0.04) * length(df_btc$Change) * diff(range(df_btc$Change)) / 10
}

curve(shifted_normal_pdf(x), from = x_limits[1], to = x_limits[2], add = TRUE, col = "blue", lwd = 2)

#T distri curve
curve(dt(x, df = df_est), 
      from = x_limits[1], to = x_limits[2], 
      add = TRUE, col = "orange", lwd = 2)
#laplace Curve
laplace_pdf <- function(x, mu, b) {
  (1/(2*b)) * exp(-abs(x - mu)/b)
}
mu_est <- mean(df_btc$Change)
b_est <- sd(df_btc$Change) / sqrt(2)
curve(laplace_pdf(x, mu_est, b_est), 
      from = x_limits[1], to = x_limits[2], 
      add = TRUE, col = "green", lwd = 2)
df_est <- length(df_btc$Change) - 1

#T sallis
tsallis_pdf <- function(x, q, beta) {
  if (q == 1) {
    return((1 / beta) * exp(-x / beta))
  } else {
    return((1 / beta) * (1 - (1 - q * beta * x)^((1 / (1 - q)))))
  }
}
q <- 1.5 
beta <- sd(df_btc$Change) 
curve(tsallis_pdf(x, q, beta), 
      from = min(x_limits), to = max(x_limits), 
      add = TRUE, col = "red", lwd = 2)

fit <- fitdist(df_btc$Change, "norm")
# Perform the Kolmogorov-Smirnov test by creating random samples to compare which distribution fits BTC
cleaned_data <- na.omit(df_btc$Change)
fit <- fitdist(cleaned_data, "norm")

#getting a sample from a normal distribution
random_sample <- rnorm(length(cleaned_data), mean = fit$estimate[1], sd = fit$estimate[2])
cleaned_data <- jitter(cleaned_data)
ks_result <- ks.test(cleaned_data, random_sample)
print(ks_result)

#comparing to a t-distribution
random_sample <- rt(length(cleaned_data), length(cleaned_data)-1)
kst_result <- ks.test(cleaned_data, random_sample)
print(kst_result)

#comparing to a Laplace distribution 
lp_test <-rlaplace(length(na.omit(df_btc$Change)), 
                   location = mean(na.omit(df_btc$Change)), 
                   scale = sd(na.omit(df_btc$Change)))
jittered_data <- jitter(na.omit(df_btc$Change))
ks_result <- ks.test(jittered_data, lp_test)
print(ks_result)
#comparing to a Tsallis a-exponential distribution
rtsal <- function(n, q, beta) {
  u <- runif(n)
  if (q == 1) {
    return(-beta * log(1 - u))
  } else {
    return(sign(q) * beta * (1 - (1 + q * beta * u)^(1/q)))
  }
}

df_test<- rtsal(length(na.omit(df_btc$Change)), mean(na.omit(df_btc$Change)), sd(na.omit(df_btc$Change)))
jittered_data <- jitter(na.omit(df_btc$Change))
ks_result <- ks.test(jittered_data, df_test)
print(ks_result)

#comparing to a Powerlaw distribution
#rounded_data <- round(df_btc$Change)
#pl_fit <- displ$new(df_btc$Change)
#pl_fit$setXmin()
#df_teste <- rpldis(length(df_btc$Change), xmin = pl_fit$getXmin(), alpha = pl_fit$getParameter("alpha"))
#ks_result <- ks.test(df_btc$Change, df_teste)
#print(ks_result)
# not possible, the data needs to be discrete 

# D STATS:
# Normal: 0.19537
# T- Distribution: 0.44041
# Laplace: 0.19202
# TSALLIS : 0.53108
# winner: laplace

