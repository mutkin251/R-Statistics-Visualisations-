
# Source code for Studio 6 problem 1d
#---------------------------

# This draws a histogram for a trials consisting of a single draw from a Cauchy distribution and trials which are the average of ndraws2 draws from the same distribution

# Set the parameters
loc = 0
scale = 1
ntrials = 10000
ndraws2 = 9

# Give the limits for the plotting range
range_min = -10
range_max = 10

# Plot 2 graphs on the screen. Save the old parameter settings
opar = par(mfrow=c(1,2))

# Compute the Cauchy density for plotting on top of the histograms
xpdf = seq(range_min, range_max, 0.01)
ypdf = dcauchy(xpdf, location=loc, scale=scale)

# Generate Cauchy data for part
data1 = rcauchy(ntrials, location=loc, scale=scale)
# Cauchy data is concentrated near the location, but has occasional very large values. This makes the histogram hard to draw. We fix this by removing all the values outside our range of range_min to range_max
data1r = data1[(data1<range_max) & (data1>range_min)]

# Plot the histogram  
plot_title = "1d(i). Hist. of one draw"
hist(data1r, freq=FALSE, ylim=c(0,max(ypdf)), breaks=40, col=rgb(1, 0.7, 0.4), main=plot_title)
# Add the graph of the Cauchy pdf
lines(xpdf, ypdf, col='blue', lwd=3)

# Generate and average Cauchy data
x = rcauchy(ntrials*ndraws2, location=loc, scale=scale)
x2 = matrix(x, nrow=ndraws2, ncol=ntrials)
data2 = colMeans(x2)
# As above, remove values outside the specified range
data2r = data2[(data2<range_max) & (data2>range_min)]

# Plot the histogram
plot_title = "1d(ii). Hist. of average"
hist(data2r, freq=FALSE, ylim=c(0,max(ypdf)), breaks=40, col=rgb(0.6, 0.6, 1), main=plot_title)
# Add the graph of the Cauchy pdf
lines(xpdf, ypdf, col='orange', lwd=2)

# Restore the old parameter settings
par(opar)

