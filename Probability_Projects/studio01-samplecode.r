
# Sample code from Studio 1
# You can find more at the R tutorials
#---------------------------------

# IMPORTANT: You should use the Session menu in R-studio to set the working directory to
# 'To Source File Location'

#---------------------------
# Section 1. print and cat statements
# to display answers
x = 5
y = 10
print(x) # Shows the value of x
cat(x, "\n") # Shows the value of x
# cat doesn't automatically start a
# new line. The "\n" makes it start
# a new line.
# The following shows values of x and y
cat("x =", x, "y =", y, "\n") 

#----------------------------------
# Section 2: choose, factorial, sample functions

# To find n choose k use choose(n, k)
x = choose(4, 2)
print(x)
x = choose(10, 3)
print(x)
x = choose(100, 42)
print(x)

# Compute the number of ways to get 1 pair and the
# probability of one pair
x = choose(13, 1)*choose(4, 2)*choose(12, 3)*4*4*4
print(x)
y = choose(13, 1)*choose(4, 2)*choose(12, 3)*4*4*4/choose(52, 5)
print(y)

# factorial does what it says
x = factorial(3)
print(x)
x = factorial(4)
print(x)
x = factorial(22)
print(x)

#sample picks random values from a list
x = sample(1:5, 3)  #pick 3 elements from 1,2,3,4,5 --no repeats allowed
print(x)

x = sample(1:5, 5)  #Permutes the 5 elements
print(x)

# Uncomment out the next line and run it to see it fail. Then comment it back out so it doesn't cause problems.
#sample(1:5, 7)  #Fails because repeats are not allowed

x = sample(1:20, 8)
print(x)

x = sample(1:5, 4, replace=TRUE) #repeats are now allowed
print(x)
x = sample(1:5, 5, replace=TRUE)
print(x)
x = sample(1:5, 10, replace=TRUE)
print(x)

# Section 3: making arrays
#very often we want to arrange our data in rows and columns
#The matrix command does this for us.
x = matrix(1:30, nrow=5) #If you only set nrow, matrix will figure out ncol
print(x)
# N.B. matrix breaks the list into columns --this is opposite what you might expect. As is typical in R there is a parameter in the matrix() function to change this. See if you can figure it out.

# Section 4: colSums function
x=sample(0:1, 30, replace=TRUE)
print(x)
y=matrix(x, nrow=5, ncol=6)
print(y)
#colSums() sums each of the columns of y. 
print(colSums(y))

#---------------------------
# Section 5: DICE SIMULATION: 
# Simulating rolling a 20-sided die 
# 8 times and checking for matches ----

# source() is the way of loading source 
# code in other files
source('colMatches.r')
colMatchesHelp()  #we'll use this syntax to write help functions for code.

# First we'll do 1 trial of nrolls 
# of an nsides-sided die
# Give values understandable names.
# In this code an initial 'n' means 'number of'
nsides = 20
nrolls = 8
ntrials = 10
size_match = 2
die = 1:nsides

trial =  sample(die, nrolls, replace=TRUE)
#display the values in trials
print(trial)
# colMatches checks the vector trials to see if size_match of the entries are the same.
# Look for two rolls the same
print(colMatches(trial, size_match))

# Now we'll run ntrials at once
nsides = 20
nrolls = 8
ntrials = 10
size_match = 2
die = 1:nsides

# ntrials of nrolls each requires nrolls*ntrials random samples
# Generate nrolls*ntrials random samples
y =  sample(die, nrolls*ntrials, replace=TRUE)
# Arrange the samples into one nrolls trial per column of an array
# Note the named arguments are 'nrow' NOT 'nrows' and 'ncol' NOT 'ncols'
trials = matrix(y, nrow=nrolls, ncol=ntrials)
print(trials)
# The function colMatches checks each column of a matrix to see if size_match entries match. It returns a vector (list) of 1's and 0's
# In this case each column of the matrix trials represents one trial.
w = colMatches(trials, size_match)
# Use sum to count the number of 1's in w
print(sum(w))
# Divide by ntrials to get the fraction of trials with a match
print(sum(w)/ntrials)
# A simpler way to get the fraction in one command is with mean()
print(mean(w))

# Here's the same code without comments so selecting and
# running in RStudio is easier. We also remove  the
# line that displayed trials and up ntrials to 1000
source('colMatches.r')
nsides = 20
nrolls = 8
ntrials = 1000
size_match = 2
die = 1:nsides
y =  sample(die, nrolls*ntrials, replace=TRUE)
trials = matrix(y, nrow=nrolls, ncol=ntrials)
w = colMatches(trials, size_match)
print(mean(w))

#------------------------------
# Section 6: SIMPLE PLOTTING
# 1. seq(start, stop, increment) makes a list of numbers from start to stop in steps of increment
# 2. sin(x) applies sin to every element in x
# 3. plot has lots of ways to control the plot type='l' says to connect the points in (x,y) by lines, lwd is the line width
x = seq(0, 6*pi, 0.01)
y = sin(x)
plot(x,y,type='l', col='red',lwd=3)

# You can look at studio01-sampleplot.r for more plotting examples

#-------------------------
# Section 7: MAKING GRAPHS OF THE RESULTS
# Here's code to plot the (estimated) probability of a match vs nrolls in a trial
# It uses a 'for loop' to run the simulation for various values of nrolls

# If you don't know about 'for loops' don't worry about it. We'll learn more about them soon.
source('colMatches.r')

nsides = 20
ntrials = 1000
size_match = 2
die = 1:nsides
# Run experiments with nrolls from 1 to 60
nrolls = 1:60

# length(all_nrolls) gives the length of the vector all_nrolls
N = length(nrolls)

# rep(value, n) makes a vector with n copies of value.
# We start by creating the probability vector. 
# We let all values be 0. We'll fill in our actual computed values in the loop
prob = rep(0, N)

# This is how you make a for loop
for (j in 1:N) {
    nr = nrolls[j]
    y =  sample(die, nr*ntrials, replace=TRUE)
    trials = matrix(y, nrow=nr, ncol=ntrials)
    w = colMatches(trials, size_match)
    prob[j] = mean(w)
}
plot(nrolls, prob, type='l', col="red", lwd=2)

# We do the same plot for size_match = 3
size_match = 3

prob = rep(0, N)
for (j in 1:N) {
    nr = nrolls[j]
    y =  sample(die, nr*ntrials, replace=TRUE)
    trials = matrix(y, nrow=nr, ncol=ntrials)
    w = colMatches(trials, size_match)
    prob[j] = mean(w)
}
# lines adds the plot of all_nrolls,prob to the existing plot instead of making a new plot
lines(nrolls, prob, col="green", lwd=2) #New color

# Same thing with size_match = 4
size_match = 4

prob = rep(0, N)
for (j in 1:N) {
    nr = nrolls[j]
    y =  sample(die, nr*ntrials, replace=TRUE)
    trials = matrix(y, nrow=nr, ncol=ntrials)
    w = colMatches(trials, size_match)
    prob[j] = mean(w)
}
lines(nrolls,prob, col="blue", lwd=2) #New color



#---------------------------------------------------------
#
# MIT OpenCourseWare: https://ocw.mit.edu
#
#---------------------------------------------------------
