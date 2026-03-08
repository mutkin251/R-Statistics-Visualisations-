library(tidyverse)
# #empirichno ozinuemo
# X~binom -> C(n,k) Sample Binom(n,p) -> filter samplespace
######################################  1 a
problem_1a <- function(ntosses, phead, k, ntrials) {
  s <- rbinom(ntrials, ntosses, phead)
  pmf <- sum(s == k) / ntrials
  cdf <- sum(s <= k) / ntrials  
  
  cat("Estimated probability P(Y =", k,"):", pmf, "\n")
  cat("Estimated probability P(Y ≤", k,"):", cdf, "\n")
}
problem_1a(10,0.6,6,1000)

 ########################################## 1 b

problem_1b <- function(ntosses, phead, k) {
  P_Y_equals_k_exact <- choose(ntosses, k) * (phead^k) * ((1 - phead)^(ntosses - k))
  cat("Exact P(Y = k):", P_Y_equals_k_exact, "\n")
}
problem_1b(10,0.6,6)

#P(Binom(n,k)=k)=C(n,k)*p^k * (1-p)^n-k
# pbinom(4,10,0.5)
# dbinom(4,10,0.5)

################################################# 2a

# v<-0:10
# prize<-v^2-7*v
# prize
# plot(x=v,y=prize)
problem_2a <- function() {
  v <- 0:10
  prize <- v^2 - 7 * v
  plot(x = v, y = prize, type = "b", col = "orange", xlab = "Outcomes", ylab = "Payoff")
}
problem_2a()
################################################## 2b

problem_2b <- function() {
  ntosses <- 10
  phead <- 0.6
  prize <- (0:10)^2 - 7*(0:10)
  
  expect <- 0
  for(i in 0:10) {
    expect <- expect + prize[i + 1] * choose(ntosses, i) * (phead^i) * ((1 - phead)^(ntosses - i))
  }
  
  cat("Expected value of the game:", expect, "\n")
  if (expect > 0) {
    cat("The game is a good bet.\n")
  } else {
    cat("The game is a bad bet.\n")
  }
}
problem_2b()

################################################## 2c

problem_2c <- function(ntrials) {
  ntosses <- 10
  phead <- 0.6
  tosses <- rbinom(ntrials, ntosses, phead)
  prizes <- tosses^2 - 7 * tosses
  avg_payoff <- mean(prizes)
  cat("Simulated average payoff:", avg_payoff, "\n")
}
problem_2c(5000)

# tosses <- rbinom(10000, 10,0.6)    
# prizes<-tosses^2-7*tosses
# mean(prizes)

############################################### 3
# n<-5
# start_position<-1:n
# sample(start_position,n)
# count<-rep(0,10000)
# for (i in 1:10000){
#   count[i]<-sum(start_position==sample(start_position,n))
# }
# mean(count)

problem_3 <- function(n, ntrials) {
  start_position <- 1:n
  count <- 0
  
  for (i in 1:ntrials) {
    perm <- sample(start_position, n)
    if (sum(start_position == perm) == 0) {
      count <- count + 1
    }
  }
  
  derangement_fraction <- count / ntrials
  cat("Fraction of derangements:", derangement_fraction, "\n")
}
problem_3(10,1000)

