studio6_problem0_draw_plot = function(mu, sigma, n_draws, ntrials) { 
  # Simulate averaging data from a normal distribution
  # Draw the histogram of the results.
  
  # Arguments:
  #  mu = mean of normal distribution
  #  sigma = standard deviation of normal distribution
  #  n_draws = number of data points drawn from the normal distribution for each trial.
  #  ntrials = number of trials to simulate

  # This function does the following:
  #   Divide the screen so you can show 2 plots side-by-side 
  #   Run ntrials trials of a single draw from this distribution
  #   Plot a density histogram of the data
  #   Run ntrials trials of averaging n_draws draws from this distribution. 
  #   Plot a density histogram of the averaged data.

  # Set the screen to hold 2 plots
  opar = par(mfrow=c(1,2))

  # Generate normal data
  data_1 = rnorm(ntrials, mu, sigma)

  # Plot the histogram
  plottitle = "Hist. of one draw"
  xmin = min(mu-4*sigma, min(data_1))
  xmax = max(mu+4*sigma, max(data_1))
  hist(data_1, freq=FALSE, xlim=c(xmin, xmax), breaks=15, col='orange', main=plottitle)

  # Generate an average normal data
  x = rnorm(ntrials*n_draws, mu, sigma)
  data_n = matrix(x, nrow=n_draws, ncol=ntrials)
  data_ave = colMeans(data_n)

  # Plot the histogram
  plottitle = "Hist. of average"
  sig_ave = sigma/sqrt(n_draws)
  # Use the same scale for the average to see the changes
  hist(data_ave, freq=FALSE, xlim=c(xmin, xmax), breaks=15, col='blue', main=plottitle)

  # Restore the old parameter settings
  par(opar)
}
