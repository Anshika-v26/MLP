createsample(201641632)
save(mysample, file = "mysample.RData")
mean(mysample$AgeFirstKill) #mean of age of first kill
#counting no of rows
nrow(mysample) 

#DATA CLEANING
#to remove NA values,rows with missing observations for AgeFirstKill
#removing rows who first killed before the year 1900
nrow(mysample)
mysample<-na.omit(mysample) 
nrow(mysample)
mysample<-subset(mysample, AgeFirstKill!=99999) 
nrow(mysample)
mysample<-subset(mysample, AgeFirstKill+YearBorn >=1900)

#calculating and adding career duration column
CareerDuration<-mysample$AgeLastKill-mysample$AgeFirstKill 
mysample<-cbind(mysample,CareerDuration) 
#removing negative or unrealistically long durations
mysample<-subset(mysample, CareerDuration>=0) 
nrow(mysample)
duplicated(mysample) #no duplicates

#DATA EXPLORATION
table(mysample$Motive) #tables for my use maybe
table(mysample$Sex)
table(mysample$Race)
table(mysample$Sex, mysample$Race)
table(mysample$Sex, mysample$Motive)
table(mysample$Race, mysample$Motive)

#making box plots for motives vs age at first kill
par(mfrow = c(1, 1))
plot(y=mysample$AgeFirstKill,x=mysample$Motive,ylab="Age at first kill",xlab="Motives")

x <- seq(from = min(mysample$AgeFirstKill), to = max(mysample$AgeFirstKill), by = 1)
hist(mysample$AgeFirstKill, freq = F, xlab="Age")
lines(x, dnorm(x, mean = mean(mysample$AgeFirstKill), sd = sd(mysample$AgeFirstKill)), lwd = 2, col = "maroon")
summary(mysample$AgeFirstKill)

#Motive - Enjoyment or power
firstmotive<-mysample[mysample$Motive=="Enjoyment or power",c("AgeFirstKill","Motive")]
mean(firstmotive$AgeFirstKill)
sd(firstmotive$AgeFirstKill)
var(firstmotive$AgeFirstKill)
summary(firstmotive$AgeFirstKill)
quantile(firstmotive$AgeFirstKill,type=1)
hist(v<-firstmotive$AgeFirstKill,xlab="Age",main="Enjoyment or power - Age at first kill")
# Density curve for checking if it's normally distributed
x<-firstmotive$AgeFirstKill
plot(x, dnorm(x, mean =30.57571, sd =8.432645), type = "l",xlab="Age",ylab="Density", col = "blue",main="Enjoyment or power - Age at first kill")
hist(firstmotive$AgeFirstKill, freq =  FALSE,xlab="Age",main="Enjoyment or power - Age at first kill")
x <- seq(from = min(firstmotive$AgeFirstKill), to = max(firstmotive$AgeFirstKill), by =0.1)
lines(x, dnorm(x, mean = 30.57571 , sd =  8.432645), lwd = 2, col = "blue")

#CDF - Normal distribution
Fn <- ecdf(firstmotive$AgeFirstKill)
Fn(30.57571)
sum(firstmotive$AgeFirstKill <= 30.57571)/length(firstmotive$AgeFirstKill)
#G(x)
# Estimates of mu and sigma:
mu <- mean(firstmotive$AgeFirstKill) #mean
sigma <- sd(firstmotive$AgeFirstKill) #standard deviation
# CDF of a normal distribution with the estimated parameters:
G <- function(x){return(pnorm(x, mean = mu, sd = sigma))}
G(30.57571)
plot(Fn, verticals = TRUE, pch = NA, xlab="Age",main="Enjoyment or power - Age at first kill")
# A set of equally spaced points at which to plot the curve G(x):
x <- 1:500   
# We use the lines() function to connect the plotted points:
lines(x, G(x), col = "blue")
#The vertical distances between the two curves generally appear to be quite small, suggesting close agreement between the sample CDF and the normal CDF.

# QQ Plots - Normal distribution
sort(firstmotive$AgeFirstKill)
z <- (sort(firstmotive$AgeFirstKill) - mu)/sigma
n <- length(firstmotive$AgeFirstKill)
r <- (1:n)
# Quantiles of N(0, 1):
q <- qnorm(p = r/(n + 1), mean = 0, sd = 1)
# Normal Q-Q plot:
plot(q, z)
# Line with intercept 0 and slope 1:
abline(a = 0, b = 1, col = "blue")

#CLT
samplemeans <- rep(NA, 100)# An empty vector to store our 100 sample means.
n <- 667
for(i in 1:100){
    simulatedsample <- rnorm(n, 30.57571)     
    samplemeans[i] <- mean(simulatedsample)
}
qqnorm(samplemeans)

#estimating mu
mu      <- mean(firstmotive$AgeFirstKill)
sigma   <- sd(firstmotive$AgeFirstKill)
muhat1  <- rep(NA, 1000)
muhat2  <- rep(NA, 1000)
for(i in 1:1000){
  x <- rnorm(n = 667, mean = mu, sd = sigma)
  muhat1[i] <- mean(x)
  muhat2[i] <- quantile(x, type = 1)[3]   
}
par(mfrow = c(1, 2))
hist(muhat1, xlim = range(c(muhat1, muhat2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(muhat1), col = "blue", lty = 2, lwd = 3)
hist(muhat2, xlim = range(c(muhat1, muhat2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(muhat2), col = "blue", lty = 2, lwd = 3)

#estimating sigma
mu      <- mean(firstmotive$AgeFirstKill)
sigma   <- sd(firstmotive$AgeFirstKill) 
sigma2hat1 <- rep(NA, 1000)
sigma2hat2 <- rep(NA, 1000)
for(i in 1:1000){
  x <- rnorm(n = 10, mean = mu, sd = sigma)
  sigma2hat1[i] <- sd(x)^2
  sigma2hat2[i] <- (666/667)*sd(x)^2   
}
par(mfrow = c(1, 2))
hist(sigma2hat1, xlim = range(c(sigma2hat1, sigma2hat2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(sigma2hat1), col = "blue", lty = 2, lwd = 3)
hist(sigma2hat2, xlim = range(c(sigma2hat1, sigma2hat2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(sigma2hat2), col = "blue", lty = 2, lwd = 3)


#Motive - Revenge or vigilante justice
secondmotive<-mysample[mysample$Motive=="Revenge or vigilante justice",c("AgeFirstKill","Motive")]
mean(secondmotive$AgeFirstKill)
sd(secondmotive$AgeFirstKill)
var(secondmotive$AgeFirstKill)
quantile(secondmotive$AgeFirstKill,type=1)
summary(secondmotive$AgeFirstKill)
par(mfrow = c(1, 1))
hist(v<-secondmotive$AgeFirstKill,xlab="Age",main="Revenge or vigilante justice - Age at first kill")
#histogram
x<-secondmotive$AgeFirstKill
plot(x, dnorm(x, mean =30.41818, sd =9.870815), type = "l",xlab="Age",ylab="Density", col = "green",main="Revenge or vigilante justice - Age at first kill")
#normal distribution curve
hist(secondmotive$AgeFirstKill, freq =  FALSE,xlab="Age",main="Revenge or vigilante justice - Age at first kill")
x <- seq(from = min(secondmotive$AgeFirstKill), to = max(secondmotive$AgeFirstKill), by =0.1)
lines(x, dnorm(x, mean=30.41818 ,sd=9.870815), lwd=2, col = "green")

#CDF
Fn <- ecdf(secondmotive$AgeFirstKill)
Fn(30.41818)
sum(secondmotive$AgeFirstKill <= 30.41818)/length(secondmotive$AgeFirstKill)
#G(x)
# Estimates of mu and sigma:
mu <- mean(secondmotive$AgeFirstKill) #mean
sigma <- sd(secondmotive$AgeFirstKill) #standard deviation
# CDF of a normal distribution with the 
# estimated parameters:
G <- function(x){return(pnorm(x, mean = mu, sd = sigma))}
# Check the value at x = 30.41818:
G(30.41818)
plot(Fn, verticals = TRUE, pch = NA, xlab="Age", main="Revenge or vigilante justice - Age at first kill")
# A set of equally spaced points at which to plot the curve G(x):
x <- 1:500   
# We use the lines() function to connect the plotted points:
lines(x, G(x), col = "green")
#The vertical distances between the two curves generally appear to be quite small, suggesting close agreement between the sample CDF and the normal CDF.

#Q-Q plot
sort(secondmotive$AgeFirstKill)
# Standardised order statistics:
z <- (sort(secondmotive$AgeFirstKill) - mu)/sigma
n <- length(secondmotive$AgeFirstKill)
r <- (1:n)
# Quantiles of N(0, 1):
q <- qnorm(p = r/(n + 1), mean = 0, sd = 1)
# Normal Q-Q plot:
plot(q, z)
# Line with intercept 0 and slope 1:
abline(a = 0, b = 1, col = "green")

#CLT
samplemeans <- rep(NA, 100)# An empty vector to store our 100 sample means.
n <- 55
for(i in 1:100){
  simulatedsample <- rnorm(n, 30.41818)     
  samplemeans[i] <- mean(simulatedsample)
}
qqnorm(samplemeans)

#estimating mu
mu      <- mean(secondmotive$AgeFirstKill)
sigma   <- sd(secondmotive$AgeFirstKill)
muhat1  <- rep(NA, 1000)
muhat2  <- rep(NA, 1000)
for(i in 1:1000){
  x <- rnorm(n = 55, mean = mu, sd = sigma)
  muhat1[i] <- mean(x)
  muhat2[i] <- quantile(x, type = 1)[3]   
}
par(mfrow = c(1, 2))
hist(muhat1, xlim = range(c(muhat1, muhat2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(muhat1), col = "blue", lty = 2, lwd = 3)
hist(muhat2, xlim = range(c(muhat1, muhat2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(muhat2), col = "blue", lty = 2, lwd = 3)

#estimating sigma
mu      <- mean(secondmotive$AgeFirstKill)
sigma   <- sd(secondmotive$AgeFirstKill) 
sigma2hat1 <- rep(NA, 1000)
sigma2hat2 <- rep(NA, 1000)
for(i in 1:1000){
  x <- rnorm(n = 10, mean = mu, sd = sigma)
  sigma2hat1[i] <- sd(x)^2
  sigma2hat2[i] <- (54/55)*sd(x)^2   
}
par(mfrow = c(1, 2))
hist(sigma2hat1, xlim = range(c(sigma2hat1, sigma2hat2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(sigma2hat1), col = "blue", lty = 2, lwd = 3)
hist(sigma2hat2, xlim = range(c(sigma2hat1, sigma2hat2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(sigma2hat2), col = "blue", lty = 2, lwd = 3)


#Motive - Robbery or financial gain
thirdmotive<-mysample[mysample$Motive=="Robbery or financial gain",c("AgeFirstKill","Motive")]
median(thirdmotive$AgeFirstKill)
mean(thirdmotive$AgeFirstKill)
sd(thirdmotive$AgeFirstKill)
var(thirdmotive$AgeFirstKill)
summary(thirdmotive$AgeFirstKill)
par(mfrow = c(1, 1))
hist(v<-thirdmotive$AgeFirstKill,xlab="Age",main="Robbery or financial gain - Age at first kill")
#histogram
x<-thirdmotive$AgeFirstKill
plot(x, dnorm(x, mean =29.38126, sd =8.966736), type = "l", col = "red",xlab="Age",ylab="Density",main="Robbery or financial gain - Age at first kill")
#normal distribution curve
hist(thirdmotive$AgeFirstKill, freq =  FALSE,xlab="Age",ylab="Density",main="Robbery or financial gain - Age at first kill")
x <- seq(from = min(thirdmotive$AgeFirstKill), to = max(thirdmotive$AgeFirstKill), by =0.1)
lines(x, dnorm(x, mean = 29.38126 , sd =  8.966736), lwd = 2, col = "red")

#CDF
Fn <- ecdf(thirdmotive$AgeFirstKill)
Fn(29.38126)
sum(thirdmotive$AgeFirstKill <= 29.38126)/length(thirdmotive$AgeFirstKill)
#G(x)
# Estimates of mu and sigma:
mu <- mean(thirdmotive$AgeFirstKill) #mean
sigma <- sd(thirdmotive$AgeFirstKill) #standard deviation
# CDF of a normal distribution with the 
# estimated parameters:
G <- function(x){return(pnorm(x, mean = mu, sd = sigma))}
G(29.38126)
plot(Fn, verticals = TRUE, pch = NA,xlab="Age",main="Robbery or financial gain - Age at first kill")
# A set of equally spaced points at which to plot the curve G(x):
x <- 1:500   
# We use the lines() function to connect the plotted points:
lines(x, G(x), col = "red")

#QQ plot
sort(thirdmotive$AgeFirstKill)
# Standardised order statistics:
z <- (sort(thirdmotive$AgeFirstKill) - mu)/sigma
n <- length(thirdmotive$AgeFirstKill)
r <- (1:n)
# Quantiles of N(0, 1):
q <- qnorm(p = r/(n + 1), mean = 0, sd = 1)
# Normal Q-Q plot:
plot(q, z)
# Line with intercept 0 and slope 1:
abline(a = 0, b = 1, col = "red")

#CLT
samplemeans <- rep(NA, 100)# An empty vector to store our 100 sample means.
n <- 459
for(i in 1:100){
  simulatedsample <- rnorm(n, 29.38126)     
  samplemeans[i] <- mean(simulatedsample)
}
qqnorm(samplemeans)

#estimating mu
mu      <- mean(thirdmotive$AgeFirstKill)
sigma   <- sd(thirdmotive$AgeFirstKill)
muhat1  <- rep(NA, 1000)
muhat2  <- rep(NA, 1000)
for(i in 1:1000){
  x <- rnorm(n = 459, mean = mu, sd = sigma)
  muhat1[i] <- mean(x)
  muhat2[i] <- quantile(x, type = 1)[3]   
}
par(mfrow = c(1, 2))
hist(muhat1, xlim = range(c(muhat1, muhat2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(muhat1), col = "blue", lty = 2, lwd = 3)
hist(muhat2, xlim = range(c(muhat1, muhat2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(muhat2), col = "blue", lty = 2, lwd = 3)

#estimating sigma
mu      <- mean(thirdmotive$AgeFirstKill)
sigma   <- sd(thirdmotive$AgeFirstKill) 
sigma2hat1 <- rep(NA, 1000)
sigma2hat2 <- rep(NA, 1000)
for(i in 1:1000){
  x <- rnorm(n = 10, mean = mu, sd = sigma)
  sigma2hat1[i] <- sd(x)^2
  sigma2hat2[i] <- (458/459)*sd(x)^2   
}
par(mfrow = c(1, 2))
hist(sigma2hat1, xlim = range(c(sigma2hat1, sigma2hat2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(sigma2hat1), col = "blue", lty = 2, lwd = 3)
hist(sigma2hat2, xlim = range(c(sigma2hat1, sigma2hat2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(sigma2hat2), col = "blue", lty = 2, lwd = 3)



#Z-test function (since sample > 30)
z_test <- function(x, mu, sd, n){
  z <- (x - mu) / sqrt ((sd^2)/n)
  c <- qnorm(0.975)
  p <- 2 * pnorm(-abs(z))
  test <- p * 100
  val <- (z >  c) | (z < - c)
  # 97.5% quantile:
  alpha=0.05
  xbar=mean(mysample$AgeFirstKill)
  z_0.025 <- qnorm(1 - alpha/2, mean = 0, sd = 1)
  CI <- c( xbar - z_0.025 * sqrt(sd^2/n), xbar + z_0.025 * sqrt(sd^2/n) )  
  if (val == "TRUE" & p < 5){
    print (cat("Confidence interval", CI, "\n"))
    print("p -value is less than 5 %, so we reject the null hypothesis")
  }
  else {
    print ("p -value is greater than 5 %, so we fail reject the null hypothesis")
  }
  return (cat("Z-Score -", z, "\np-value -", p) )
}

z_test(mean(firstmotive$AgeFirstKill), 27, sd(firstmotive$AgeFirstKill), length(firstmotive$AgeFirstKill))
z_test(mean(secondmotive$AgeFirstKill), 27, sd(secondmotive$AgeFirstKill), length(secondmotive$AgeFirstKill))
z_test(mean(thirdmotive$AgeFirstKill), 27, sd(thirdmotive$AgeFirstKill), length(thirdmotive$AgeFirstKill))


#Confidence interval plot
# Analysis labels for the left side:
analysis = c("First Motive", 
             "Second Motive", 
             "Third Motive")
# Results of each test (estimated mean, 
# upper CI limit, lower CI limit, p-value):
estimate  =  c(30.57571,30.41818,29.38126 )             
upper     =  c(30.7441,32.71282,30.92446 )
#upper=c(31.21683,33.08664,30.20374)
lower     =  c(29.46419,27.49548,29.28384)
#lower=c(29.93459,27.74973,28.55878)
pval      =  c(6.557408e-28,0.01022375,1.273598e-08)
#pval=c(2.2e-16,0.01302,2.275e-08)
# Set the margin widths:
par(mar = c(6,6,1,6))
# Create an empty plot of a suitable 
# size (considering the width of your
# confidence intervals):
plot(x = 0,                                  # One point at (0,0).
     xlim = c(26, 34), ylim=c(0, 5),        # Axis limits.
     type = "n", xaxt = "n", yaxt="n",       # No points, no axes drawn.
     xlab = NULL, ylab= NULL, ann = FALSE,   # No axis labels or numbers.
     bty="n")                                # No box.
# Add a horizontal (side = 1) axis:
axis(side = 1, cex.axis = 1) 
# Add an axis label 4 lines below the axis:
mtext("Estimated difference in means for 3 motives, with 95% confidence interval", 
      side = 1, line = 4) 
# Add some grid lines, preferably lined up
# with the numbers on the horizontal axis:
for(i in c(28, 29, 30, 31)){
  lines(c(i, i), c(0, 5), lty = 2, col = "gray53")
}
# Add labels for each analysis on the left (side = 2) 
# at vertical heights of 1, 2, 3:
verticalpos = 1:3
mtext(text = analysis,  at = verticalpos, 
      side = 2, line = 5, outer = FALSE, las = 1, adj = 0)
points(estimate, verticalpos, pch = 16)
# Plot the four interval estimates:
for(i in 1:3 ){
  lines(c(lower[i], upper[i]), c(verticalpos[i], verticalpos[i]))
  lines(c(lower[i], lower[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))
  lines(c(upper[i], upper[i]), c(verticalpos[i] + 0.2, verticalpos[i] - 0.2))
}
abline(v = 27, col = "red3", lwd = 3)
est <- formatC(estimate, format='f', digits = 0)
P <- formatC(pval , format = 'f', digits = 3) 
pval <- paste("p =", P)    # Type pval to see what this does.
L <- formatC(lower, format = 'f', digits = 0)
U <- formatC(upper, format = 'f', digits = 0)
interval <- paste("(", L, ", ", U, "),", sep = "")   # Type interval to check.
# Putting it all together:
results <- paste(est, interval, pval)
# Add to the plot:
mtext(text = results, at = verticalpos,side = 4, line = 4, outer = FALSE, las = 1, adj = 1)
box("inner")

