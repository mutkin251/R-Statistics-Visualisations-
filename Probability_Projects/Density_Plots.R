#b Create function studio03_problem_2b(rate, nsamples, n_to_average, bin_width) which does the following:
#creates n_to_average exponential with parameter rate samples of size nsamples
#creates sample of averages of corresponding values from created samples
#plots a density histogram of the created sample
#overlays graph of pdf of normal distribution with parameters
mean_of_average = 1/rate  
std_dev_of_average = (1/rate)/sqrt(n_to_average)

#to do so adapt previosly used code

#After having finished check if and to what extent histogram and pdf are alike


#Supplementary tasks (optional):
#1)check if the observed phenomenon occurs for different distributions
#namely try this for uniform distributions
#hint: use runif(nsamples,min,max)

#mixture of normal distributions (50% of the sample of one normal distribution, 50% of the other normal distribution)
#regular normal distributions
#hint: use rnorm(nsamples,mean,sd)

#2)play around with different values of n_to_average to find the breaking point where histogram and pdf start to look alike
#hint: depending on the studied distribution (exponential, uniform, normal mixture,regular normal)
#for which distribution does the phenomenon occur the fastest?





#Problem 1
#a Create function studio03_problem_1a(rate, nsamples) which does the following:
#1)generates sample of size nsamples of exponential distribution with parameter rate
#2)plots a FREQUENCY histogram with a specified bin size of the created sample

studio03_problem_1a <- function(rate, nsamples){
  # generate sample
  x <- rexp(nsamples, rate=rate)
  bin_width <- 0.2
  bins <- seq(min(x), max(x) + bin_width, bin_width)
  
  # plot frequency histogram
  hist(x, breaks=bins, col='orange', freq=TRUE,
       main=paste("Frequency Histogram, rate =", rate),
       xlab="Value", ylab="Frequency")
}
studio03_problem_1a(1, 1000)

#b Create function studio03_problem_1b(rate, nsamples) which does the following:
#1)generates sample of size nsamples of exponential distribution with parameter rate
#2)plots a DENSITY histogram with a specified bin size of the created sample
#3)adds pdf of the underlying exponential distribution to the previously created plot

studio03_problem_1b <- function(rate, nsamples){
  # generate sample
  x <- rexp(nsamples, rate=rate)
  bin_width <- 0.2
  bins <- seq(min(x), max(x) + bin_width, bin_width)

  hist(x, breaks=bins, col='blue', freq=FALSE,
       main=paste("Density Histogram with PDF, rate =", rate),
       xlab="Value", ylab="Density")

  curve(dexp(x, rate=rate), col="red", lwd=2, add=TRUE)
  legend("right", legend=c("Exponential PDF"), col="red", lwd=2)
}
studio03_problem_1b(1, 1000)

#Problem 2:In this problem we will draw density histograms of the average of many
#exponential samples. The goal is to see how the density of the averages changes as the
#number averaged increases. We will see graphically how it approaches a normal distribution
#as the number of terms in the average increases.

#a: Create function studio03_problem_2a(rate, nsamples) which does the following:
#generates 2 samples of size nsamples of exponential distribution with parameter rate
#creates a sample of means of sample 1 and sample 2
#plots a histogram of those means

studio03_problem_2a <- function(rate, nsamples){
  s1 <- rexp(nsamples, rate=rate)
  s2 <- rexp(nsamples, rate=rate)
  s_mean <- (s1 + s2) / 2
  
  bin_width <- 0.2
  bins <- seq(min(s_mean), max(s_mean) + bin_width, bin_width)
  
  hist(s_mean, breaks=bins, color='green', freq=FALSE,
       main=paste("Density Histogram of Averages (2 samples), rate =", rate),
       xlab="Value", ylab="Density")
}
studio03_problem_2a(0.1, 1000)

#b Create function studio03_problem_2b(rate, nsamples, n_to_average, bin_width) which does the following:
#creates n_to_average exponential with parameter rate samples of size nsamples
#creates sample of averages of corresponding values from created samples
#plots a density histogram of the created sample
#overlays graph of pdf of normal distribution with parameters
# mean_of_average = 1/rate  
# std_dev_of_average = (1/rate)/sqrt(n_to_average)

studio03_problem_2b <- function(rate, nsamples, n_to_average, bin_width){
  
  samples <- matrix(rexp(nsamples * n_to_average, rate=rate), ncol=n_to_average)
  s_mean <- rowMeans(samples)
  
  bins <- seq(min(s_mean), max(s_mean) + bin_width, bin_width)
  hist(s_mean, breaks=bins, col='grey', freq=FALSE,
       main=paste("Density Histogram of Averages,", n_to_average, "samples"),
       xlab="Value", ylab="Density")
  
  mean_of_average <- 1 / rate
  std_dev_of_average <- (1 / rate) / sqrt(n_to_average)
  
  curve(dnorm(x, mean=mean_of_average, sd=std_dev_of_average),col="blue", lwd=2, add=TRUE)
  
  legend("topright", legend=c("Normal Approximation"), col="blue", lwd=2)
}
studio03_problem_2b(1, 1000, 10, 0.1)
