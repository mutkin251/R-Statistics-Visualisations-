#Preparation

#don't forget to go into Session menu and set current directory as the working one
#after defining function call it explicidly as in the following code
test<-function(x){
  return(x^2)
}
test(4)

#Problem 0:
#Load attached file "studio6_test_data_frame.csv"
data<-read.csv("studio6_test_data_frame.csv")

#Problem 1:
#a 
#Run the following function and compare observed histograms
#justify observed phenomenon
studio6_problem_0 = function() { 
cat("\n----------------------------------\n")
cat("Problem 0: Averaging normal distributions \n")

# This will draw the pair of histograms needed for this problem
source('studio6_problem0_draw_plot.r')
studio6_problem0_draw_plot(10, 6, 9, 10000)

# Do not change the above code.
# ********* YOUR CODE HERE ***********

cat('0. PUT YOUR COMPARISON OF THE HISTOGRAMS HERE.\n')
}
#Problem 2:
#a
#find info about Cauch distribution on wikipedia

#b
#Write a function that plots densities of standard normal distribution and
#Cauchy with  parameters theta = 0 and scale=1

#to do so use code from previous studios and function
dcauchy(0,1,1)

#c
#asses if cauchy distribution has fat tails as t-distribution from your problem set did

#d
#run the following function
studio6_problem_2d = function() {
  cat("-----\n")
  cat("2d. Average of Cauchy distributions\n")
  
  # Be sure the working directory is set to source file location
  # No code to write. Just run this code
  source('studio6_problem_1d.r')
  
  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  
  }
studio6_problem_2d()
#Observe if phenomenon from task 1 persists?

#Task 2:
#Scenario: A lighthouse stands one kilometer offshore. 
#Its bi-directional beam rotates at a constant
#rate, so that at any moment there is exactly one spot
#on the straight shoreline illuminated by the beam. 
#All along the shore are kilometer markers.

#we suppose that lighthouse location is Cauchy distributed with unknown theta
#and scale=1

#Due to nature of Cauchy distribution, even though theta sorta describes the centre of
#the distribution in is not the mean and thus
#just averaging observation to find location of lighthouse doesn't work

#so we have to use bayesian updating to infer value of theta

#a
#plot data from problem 0

#b Modify function studio6_problem_2b(data) to do the following:
#1) Discretize the prior: use the value dtheta given in the function as the discretization stepsize.

#2) Use a for loop to go through the data one value at a time and do Bayesian updating.
#Save each posterior as you go. (Before the loop, you will need to initialize a matrix to hold the posteriors.)

#3) On one plot graph the prior and each of the posteriors
#4) The maximum a posteriori (MAP) estimate is the value of 𝜃 that maximizes the
# posterior function. Find the MAP estimate for each of the posteriors. Save these in an
# array and plot them.
# Make a separate plot showing the final posterior pdf and its MAP estimate.
# In the cat statement at the end of the function, say where, given the final posterior,
# you would look for the obscure path.



# Problem 2b:. Discretized Bayesian updates----
studio6_problem_2b = function(data) {
  # We give the unknown parameter theta a prior that follows a Uniform(theta_min, theta_max) distribution.
  # Use these values
  theta_min = -10
  theta_max = 10
  dtheta = 0.02 # Stepsize for discretizing the prior.
  
  position_data = data[,'position']
}
