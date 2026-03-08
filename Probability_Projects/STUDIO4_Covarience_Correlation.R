#Problem 1a: Create a function studio04_problem_1a(n_together,n_Barto_alone,ntrials)
#which does the following: simulate Barto's and Axel's daily winnings, estimate means and variances
#of those quantitites. Additionally, function should calculate covariance and correlation
#Barto's and Axel's daily winnings.

#One trial consists of a simulation of one day of gambling.
#• Run ntrials trials.
#• Use the cat statements provided to print the sample means, variances, covariance and
#correlation.
#• In the cat statements, you will need to provide the variables using the names you
#chose for the various quantities.

#To do so you might find usifeul studying and adapting the following code:

# Будемо працювати на основі цього:
# але в заваданні там всього треба буде росписувати, жах короче

# 1. Test the 0.68, 0.95 rules
# s = rnorm(10000, 0, 1)
# m1 = mean(abs(s) < 1)
# m2 = mean(abs(s) < 2)
# cat('Mean m1 should be near 0.68: m1 = ', m1, '\n')
# cat('Mean m2 should be near 0.95: m2 = ', m2, '\n')

# Problem1 <- function(n_tries,n_togather,n_alone){
#   wintogather <- rbinom(n_tries,n_togather,18/38)
#   incometogether <- wintogather-(n_togather-wintogather)
#   winalone <- rbinom(n_tries, n_alone, 18/38)
#   incomealone <- winalone-(n_alone-winalone)
#   incomeA<-incometogether
#   incomeB<-incometogether+incomealone
#   return(cor(incomeA,incomeB))
# }
# Problem1(1000,100,10)

# v = var(s)
# cat('Variance should be near 1: v = ', v, '\n')
# cv = cov(s,2*s)
# cat('Covariance should be positive: cv = ', cv, '\n')
# cr = cor(s,2*s)
# cat('Covariance should be near 1: cr = ', cr, '\n')


studio04_problem_1a <- function(n_together, n_Barto_alone, ntrials) {
  p_red <- 18 / 38

  axel_winnings <- numeric(ntrials)
  barto_winnings <- numeric(ntrials)
  
  for (i in 1:ntrials) {
    # Axel_Barto_together
    win_together <- rbinom(1, n_together, p_red)
    together_losses <- n_together - win_together
    axel_together <- win_together - together_losses
    barto_together <- axel_together
    # Barto_alone 
    barto_alone_wins <- rbinom(1, n_Barto_alone, p_red) #хоча я і пишу ред, по-суті шанс чорний=червоний, тому сенсу вводити змінну для приколу - не хочу
    barto_alone_losses <- n_Barto_alone - barto_alone_wins
    barto_alone_income <- barto_alone_wins - barto_alone_losses
    # Total_win
    axel_winnings[i] <- axel_together
    barto_winnings[i] <- barto_together + barto_alone_income
  }
  # Ну і понову виводимо цю всю приколюху
  mean_axel <- mean(axel_winnings)
  mean_barto <- mean(barto_winnings)
  var_axel <- var(axel_winnings)
  var_barto <- var(barto_winnings)
  covar <- cov(axel_winnings, barto_winnings)
  corr <- cor(axel_winnings, barto_winnings)
  
  cat("Mean Axel:", mean_axel,'\n')
  cat("Mean Barto:", mean_barto,'\n')
  cat("Variance Axel:", var_axel,'\n')
  cat("Variance Barto:", var_barto,'\n')
  cat("Covariance:", covar,'\n')
  cat("Correlation:", corr,'\n')
  
  return(list(covar = covar, corr = corr)) #ця приколюха нам знадобиться в 1.б бо по простому не виходить
}

studio04_problem_1a(10, 20, 1000)

#Problem 1b: Create a function studio04_problem_1b()
#which does the following: applies studio04_problem_1a() with n_together = 10 and 
#various values of n_Barto_alone. As output should show how sample covariance and
#correlation change as n_Barto_alone increases.

# 2. 
# Воно до речі чогось не працює, не знаю чому, але графік який виводився був не тим який був у вас на прктиці
# corr<-rep(0,200)
# for (i in seq(10,200,10)){
#   corr[i/10]<-Problem1(1000,100,i)
# }
# plot(corr)

n_together <- 10
n_Barto_values <- seq(0, 200, 10)
cov_values <- numeric(length(n_Barto_values)) # це по-суті пусті 20 комірочок для заповнення даними 
cor_values <- numeric(length(n_Barto_values))
    
for (i in seq_along(n_Barto_values)) {
  res <- studio04_problem_1a(n_together, n_Barto_values[i], ntrials=1000)
  cov_values[i] <- res$covar
  cor_values[i] <- res$corr
}
    
# Plot correlation vs n_Barto_alone
plot(n_Barto_values, cor_values, type='b', col='blue',
    main='Correlation vs n_Barto_alone',
    xlab='Number of Barto Alone Bets', ylab='Correlation')

#Problem 2:Create a function studio04_problem_2(n_bets_per_trial, ntrials)
#which does the following: simulates ntrials of a player betting 1 dollar on red
#n_bets_per_trial and calculates their total winning, plots histogram of those winnings,
#plots corresponding normal density over the histogram.
#Hint: the task is very similar to studio 3, you can try to adapt your code from it


studio04_problem_2 <- function(n_bets_per_trial, ntrials) {
  p_red <- 18 / 38
  winnings <- numeric(ntrials)
  
  for (i in 1:ntrials) {
    results <- rbinom(n_bets_per_trial, 1, p_red) #ну тут по ідеї біном тому-що результат - так\ні 
    income <- sum(ifelse(results == 1, 1, -1)) #я пару раз використовув цю пиколюху і ніколи не пояснював - 
    # тут прикол в функції 1)експеримент 2)якщо так 3)якщо ні. і на основі цього дає разовий результат
    winnings[i] <- income
  }
  hist(winnings, freq=FALSE, col='blue', breaks=40,
       main=paste('Distribution of Total Winnings (', n_bets_per_trial, ' bets)'),
       xlab='Total winnings')
  
  mean_theor <- n_bets_per_trial * (p_red - (1 - p_red))
  sd_theor <- sqrt(n_bets_per_trial * 4 * p_red * (1 - p_red))
  
  curve(dnorm(x, mean=mean_theor, sd=sd_theor),
        add=TRUE, col='red', lwd=2) #тут add відповідає за те щоб накладатися, бо чогось у мене line не працює
}

studio04_problem_2(100, 10000)

#Supplementary problem: Responsible or Irreseponsible?
#Suppose Barto and Axel both have 500 dollars and have a huge debt to mafia of size 2000
#Barto, being paralyzed by fear, bets 1 dollar or red each game, untill he either runs out of money
#or reaches target sum of 2000
#Axel is affected by fear differently, he bets all he has each time, untill he either runs out of money
#or reaches target sum of 2000

#Who of those 2 is more likely to reach target sum?
#Why is that so?
#If time allows, play around with other possible strategies (e.g. bet half of money one has each time)

#щодо теоретичних питань, я не знаю чи вони обов'язкові але нехай. Тут для мене все більш однозначно.Чого так - 
# якщо бетити усі свої гроші разом, то тобі має повезти 2 рази підряд, так, шанс невеликий, але порівняно з
# тим, як людина ставить по 1 баксу кожен раз, при тому що шанс перемогти нижче 50% - шанс того що він 
# так заробить 2000 баксів - майже неможливий. Тому я вірю в Акселя більше в цьому питанні. Щодо питання про 
# ставити половину - я б бетив по 1\8 від загального банку, напевно. Так я не дуже буду просідати одразу 
# після 2-3 програшів і буде шанс відігратися, але сума достатня для того щоб щось отримати, а не 1 бакс.