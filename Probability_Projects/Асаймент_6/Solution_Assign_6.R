data <- read.csv("D://RProjects//Probability_Projects//Асаймент_6//studio6_test_data_frame.csv")

# Problem 1: Normal Distribution Averaging

studio6_problem_1 = function() { 
  cat("Problem 1: Averaging normal distributions")
  source('D://RProjects//Probability_Projects//Асаймент_6//studio6_problem0_draw_plot.r')
  studio6_problem0_draw_plot(10, 6, 9, 10000)
  
  cat('1. COMPARISON OF HISTOGRAMS:')
  cat('The first histogram shows single draws from N(10, 6^2). The second histogram shows averages of 
      9 draws from the same distribution.The averaged histogram is much narrower - this demonstrates
      the Central Limit Theorem. Standard deviation of the average is sigma/sqrt(n) = 6/sqrt(9) = 2, which
      is 3 times smaller than the original standard deviation of 6. The average is more concentrated
      around the mean.')
}

studio6_problem_1()
# Problem 2 Info about Cauchy distribution

# 2 a
# https://en.wikipedia.org/wiki/Cauchy_distribution

# 2 b
studio6_problem_2b_plot = function() {
  cat("Problem 2b: Comparing Normal and Cauchy distributions")
  x = seq(-10, 10, by=0.01)
  normal_density = dnorm(x, mean=0, sd=1)
  cauchy_density = dcauchy(x, location=0, scale=1)
  plot(x, normal_density, type='l', col='blue', lwd=2,
       ylim=c(0, max(normal_density)),
       main='Standard Normal vs Cauchy Distribution',
       xlab='x', ylab='Density',
       las=1)
  lines(x, cauchy_density, col='red', lwd=2)
  legend('topright', legend=c('Normal(0,1)', 'Cauchy(0,1)'),
         col=c('blue', 'red'), lwd=2)
  
  cat('Both distributions are symmetric around 0.')
  cat('Normal has lighter tails, Cauchy has heavier tails.')
}

studio6_problem_2b_plot()

# 2 c

studio6_problem_2c = function() {
  cat("Problem 2c: Fat tails assessment")
  
  cat('YES, Cauchy distribution has extremely fat tails. In fact, the Cauchy distribution has NO 
      defined mean or variance. The tails are so heavy that integrals for mean and variance diverge. This
      is even more extreme than the t-distribution.')
}

studio6_problem_2c()

# 2 d

studio6_problem_2d = function() {
  cat("Problem 2d: Average of Cauchy distributions")
  
  source('D://RProjects//Probability_Projects//Асаймент_6//studio6_problem_1d.r')
  
  cat('OBSERVATION:')
  cat('The phenomenon from Problem 1 does NOT persist Averaging Cauchy random variables does NOT
      make the distribution narrower. Both histograms look similar - the CLT does not apply to Cauchy
      This is because Cauchy has no defined mean or variance.')
}

studio6_problem_2d()

# Problem 3: Lighthouse Problem - Bayesian Inference

# 3 a

studio6_problem_3a = function(data) {
  cat("Problem 3a: Plot lighthouse data")
  
  position_data = data[,'position']
  
  plot(1:length(position_data), position_data, 
       type='p', pch=19, col='blue',
       main='Lighthouse Beam Positions on Shore',
       xlab='Observation Number', ylab='Position (km)',
       las=1)
  abline(h=0, col='red', lty=2)
  
  cat(sprintf('Number of observations: %d', length(position_data)))
  cat(sprintf('Data range: [%.2f, %.2f]', min(position_data), max(position_data)))
}

studio6_problem_3a(data)
# 3 b
studio6_problem_3b = function(data) {
  cat("Problem 3b: Bayesian updating for lighthouse position")
  
  theta_min = -10
  theta_max = 10
  dtheta = 0.02
  
  theta = seq(theta_min, theta_max, by=dtheta)
  n_theta = length(theta)

  position_data = data[,'position']
  n_data = length(position_data)

  prior = rep(1, n_theta)
  prior = prior / sum(prior * dtheta)  # Normalize
  
  posteriors = matrix(0, nrow=n_theta, ncol=n_data+1)
  posteriors[,1] = prior

  map_estimates = rep(0, n_data+1)
  map_estimates[1] = theta[which.max(prior)]

  current_posterior = prior
  for (i in 1:n_data) {
    likelihood = dcauchy(position_data[i], location=theta, scale=1)
    
    # Posterior = likelihood * prior
    unnormalized_posterior = likelihood * current_posterior
    
    current_posterior = unnormalized_posterior / (sum(unnormalized_posterior * dtheta))
    posteriors[,i+1] = current_posterior
    map_estimates[i+1] = theta[which.max(current_posterior)]
  }
  
  # Prior and all posteriors
  par(mfrow=c(1,2))
  
  plot(theta, posteriors[,1], type='l', col='black', lwd=2,
       main='Prior and Posteriors',
       xlab='Theta (lighthouse position)', ylab='Density',
       ylim=c(0, max(posteriors)),
       las=1)
  
  colors = rainbow(n_data)
  for (i in 1:n_data) {
    lines(theta, posteriors[,i+1], col=colors[i], lwd=1)
  }
  legend('topright', legend=c('Prior', sprintf('After %d obs', n_data)),
         col=c('black', colors[n_data]), lwd=2, cex=0.8)
  
  plot(0:n_data, map_estimates, type='b', pch=19, col='blue',
       main='MAP Estimates Over Time',
       xlab='Number of Observations', ylab='MAP Estimate of Theta',
       las=1)
  abline(h=map_estimates[n_data+1], col='red', lty=2)
  
  par(mfrow=c(1,1))

  plot(theta, posteriors[,n_data+1], type='l', col='darkblue', lwd=3,
       main='Final Posterior Distribution',
       xlab='Theta (lighthouse position)', ylab='Posterior Density',
       las=1)
  abline(v=map_estimates[n_data+1], col='red', lwd=2, lty=2)
  legend('topright', 
         legend=c('Final Posterior', sprintf('MAP = %.2f', map_estimates[n_data+1])),
         col=c('darkblue', 'red'), lwd=2, cex=0.9)
  
  # Print results
  cat(sprintf('Final MAP estimate: %.3f km', map_estimates[n_data+1]))
  cat(sprintf('Given the final posterior, look for the obscure path at %.3f km mark.', 
              map_estimates[n_data+1]))
  cat('This is our best estimate of the lighthouse location.')
  
  return(list(theta=theta, posteriors=posteriors, map_estimates=map_estimates))
}

studio6_problem_3b(data)



