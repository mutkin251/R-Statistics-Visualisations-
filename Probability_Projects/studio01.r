
#---------------------------------------------------------
# Studio 1 ----

# 1. Be sure to read the instructions file: studio01-instructions.pdf

# 2. The instruction file gives instructions for testing your code. You can see the output of the tests in studio01-test-answers.html. You should use this to verify your code is giving correct output in the format asked for.


#--------------------------------------
# Problem 1: Tutorial ----
# See the problem 1 part of studio01-instructions.pdf
#
# Summary: Look at sections 1-5 of studio01-samplecode.r.
studio01_problem_1 = function() {
  cat("----------------------------------\n")
  cat("Problem 1: Tutorial.\n")

  cat("No question here.")
}

#--------------------------------------
# Problem 2: Birthdays. ----
# 2a. See the problem 2a part of studio01-instructions.pdf
#
# Summary: Use a simulation to estimate the probability that among n people in a room at least one pair of them shares a birthday.
#
# See studio01-test-answers.html for the output of test calls to this function.
studio01_problem_2a = function(ndays_in_year, npeople, ntrials) {
  cat("----------------------------------\n")
  cat("Problem 2a: Birthdays\n")
  # ndays_in_year=365
  # npeople = 15
  # ntrials =2000
  # source('colMatches.r')
  estimated_prob = 0 # This variable is for your simulated probability

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  count=0
  for (i in 1:ntrials){
    birthdays <- sample(1:ndays_in_year, npeople, replace = TRUE)
    
    if (any(duplicated(birthdays))) {
      count <- count + 1
    }
  }
  
  # Step 3: estimated probability
  estimated_prob <- count / ntrials

  # These lines print out your answers
  cat('Problem 2a simulation results:','\n')
  cat('  Number of days in year =', ndays_in_year, '\n')
  cat('  Number of people =', npeople, '\n')
  cat('  Number of trials =', ntrials, '\n')
  cat('  Simulated probability of a match =', estimated_prob, '\n')
}
studio01_problem_2a(365, 15, 2000)
#------
# 2b. See the problem 2b part of studio01-instructions.pdf
#
# Summary: Use the function in 2a to find the minimum number of people to have a 0.50 probability that at least one pair shares a birthday
#
# See studio01-test-answers.html for the output of test calls to this function.
studio01_problem_2b = function() {
  cat("----------------------------------\n")
  cat("Problem 2b.\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  ndays_in_year <- 365
  ntrials <- 5000   # large enough for stability
  for (n in 23:60) {
    prob <- studio01_problem_2a(ndays_in_year, n, ntrials)
    if (prob >= 0.5) {
      npeople = n
      break
    }
  }
  
  
  
  npeople = 23  # You need to set this to the correct value.
  cat('Smallest number of people needed for a 0.5 probability:', npeople, "\n")
}
studio01_problem_2b()
#--------------------------------------
# OPTIONAL Problem 3: Plot birthdays. ----
# ONLY IF YOU HAVE TIME.
# 3. See the problem 3 part of studio01-instructions.pdf
#
# Summary: Make a plot of the probability of a shared birthday as a function of npeople.
#
# See studio01-test-answers.html for the output of test calls to this function.
studio01_problem_3 = function(ndays_in_year, ntrials) {
  cat("----------------------------------\n")
  cat("Problem 3: Plot birthdays.\n")

  source('colMatches.r')

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********


  # The output is a graph
  cat('See plot\n')
}

#--------------------------------------
# MIT OpenCourseWare: https://ocw.mit.edu
