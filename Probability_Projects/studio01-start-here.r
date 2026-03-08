
# This is a quick test to make sure R Studio is running properly

# Choose 'Source' from the 'Code' menu and look at the graph produced

#----------------------------
# Check that the working directory is set correctly
source('studio01-start-here-include.r')

# Set exact limits on the plot and axis labels
# If this is left off R will determine limits for you when you call plot.
# This can be a problem if you add another graph to the plot that needs bigger limits

#Type = 'n' means 'none', i.e. don't actually draw anything
xlabel = expression(theta)
plot(0, 0, type='n', xlim=c(-2,9), ylim=c(-2.5,2.5), xlab=xlabel, ylab='')

#add some graphs with different colors to the plot
x = seq(-2, 9, 0.01)
y = sin(x)
lines(x, y, col='red', lwd=2)

y = cos(x)
lines(x, y, col='green', lwd=2)

#You can use rgb() to get other colors.
#You can use the parameter lwd to get a thicker line
col = rgb(1, 0.6, 0)
y = 2*cos(x)
lines(x, y, col=col, lwd=4)

# add a horizontal line at y = 0
abline(h=0)

# add a vertical line at x = 0
abline(v=0)

#Finally we add the graph of an exponential density
# Note the exponential density has range 0 to infinity so R gives it the
# value of 0 for x < 0
y = dexp(x, 1.5)
lines(x, y, col='blue', lwd=3)

#---------------------------------------------------------
#
# MIT OpenCourseWare: https://ocw.mit.edu
#
#---------------------------------------------------------
