# Problem 1: Confident Coin Analysis
n <- 250
observed_heads <- 140
observed_tails <- 110
theta_0 <- 0.5  # null hypothesis
# Part a: P-values
# One-sided p-value (testing if theta > 0.5)
# P(X >= 140 | theta = 0.5)
p_value_one_sided <- 1 - pbinom(observed_heads - 1, n, theta_0)
# Two-sided p-value
# P(X <= 110 or X >= 140 | theta = 0.5)
p_value_two_sided <- binom.test(observed_heads, n, p = 0.5,alternative = "two.sided")$p.value
cat("Problem 1a01. One-sided p-value (exact binomial):",p_value_one_sided)
cat("Problem 1a02. Two-sided p-value (exact binomial):",p_value_two_sided)
# Part b: Hypothesis testing at different significance levels
alpha_1 <- 0.1
alpha_2 <- 0.05
cat("Using two-sided test (as in part a):")
cat("P-value:", p_value_two_sided)
cat("At alpha = 0.1:", ifelse(p_value_two_sided < alpha_1, "REJECT H0","FAIL TO REJECT H0"))
cat("At alpha = 0.05:", ifelse(p_value_two_sided < alpha_2, "REJECT H0","FAIL TO REJECT H0"))
# Part c: Critical values for alpha = 0.01 (two-sided test)
alpha_3 <- 0.01
# For two-sided test, we need both tails
# Lower tail: P(X <= k) = alpha/2
# Upper tail: P(X >= k) = alpha/2
# Find lower critical value
k_lower <- qbinom(alpha_3/2, n, theta_0)
# Find upper critical value
# We want the smallest k such that P(X >= k) <= alpha/2
# This is equivalent to P(X <= k-1) >= 1 - alpha/2
k_upper <- qbinom(1 - alpha_3/2, n, theta_0) + 1
# Verify these values
p_lower <- pbinom(k_lower, n, theta_0)
p_upper <- 1 - pbinom(k_upper - 1, n, theta_0)
cat("For a two-sided test at alpha = 0.01:")
cat("Problem 1c01. Reject H0 if heads <= ", k_lower)
cat("Problem 1c02. Reject H0 if heads >= ", k_upper)
cat("Verification:")
cat("P(X <= ", k_lower, ") =", p_lower)
cat("P(X >= ", k_upper, ") =", p_upper)
cat("Total Type I error rate:", p_lower + p_upper)

# Part d: Power analysis for different alternatives

alpha_power <- 0.05
theta_alt_1 <- 0.55
theta_alt_2 <- 0.6
# Find critical values for two-sided test at alpha = 0.05
k_lower_05 <- qbinom(alpha_power/2, n, theta_0)
k_upper_05 <- qbinom(1 - alpha_power/2, n, theta_0) + 1
cat("Critical values for alpha = 0.05 (two-sided):")
cat("Lower: <=", k_lower_05, "heads")
cat("Upper: >=", k_upper_05, "heads")
# Power calculation for theta = 0.55
# Power = P(reject H0 | HA is true)
# Power = P(X <= k_lower | theta = 0.55) + P(X >= k_upper | theta = 0.55)
power_055 <- pbinom(k_lower_05, n, theta_alt_1) + 
  (1 - pbinom(k_upper_05 - 1, n, theta_alt_1))
# Power calculation for theta = 0.6
power_060 <- pbinom(k_lower_05, n, theta_alt_2) + 
  (1 - pbinom(k_upper_05 - 1, n, theta_alt_2))
cat("Problem 1d01. Power when theta = 0.55:", power_055)
cat("Problem 1d02. Power when theta = 0.6:", power_060)
par(mfrow = c(1, 2))
# PMF comparison
x_plot <- 100:175
prob_h0 <- dbinom(x_plot, n, theta_0)
prob_ha1 <- dbinom(x_plot, n, theta_alt_1)
prob_ha2 <- dbinom(x_plot, n, theta_alt_2)
plot(x_plot, prob_h0, type = "h", col = "black", lwd = 2,
     xlab = "Number of Heads", ylab = "Probability Mass",
     main = "PMF under H0 and Alternative Hypotheses")
lines(x_plot + 0.3, prob_ha1, type = "h", col = "blue", lwd = 2)
lines(x_plot + 0.6, prob_ha2, type = "h", col = "red", lwd = 2)
# Add critical values
abline(v = k_lower_05, col = "green", lwd = 2, lty = 2)
abline(v = k_upper_05, col = "green", lwd = 2, lty = 2)
# Highlight rejection region for HA1
segments(x_plot[x_plot >= k_upper_05], 0, 
         x_plot[x_plot >= k_upper_05], prob_ha1[x_plot >= k_upper_05],
         col = rgb(0, 0, 1, 0.5), lwd = 3)
segments(x_plot[x_plot <= k_lower_05], 0, 
         x_plot[x_plot <= k_lower_05], prob_ha1[x_plot <= k_lower_05],
         col = rgb(0, 0, 1, 0.5), lwd = 3)
legend("topright", 
       c("H0: theta = 0.5", "HA: theta = 0.55", "HA: theta = 0.6", "Critical values"),
       col = c("black", "blue", "red", "green"), 
       lwd = 2, lty = c(1, 1, 1, 2), cex = 0.8)
# Power regions illustrated
plot(x_plot, prob_ha1, type = "h", col = "blue", lwd = 2,
     xlab = "Number of Heads", ylab = "Probability Mass",
     main = "Power Illustration for theta = 0.55")
abline(v = k_lower_05, col = "green", lwd = 2, lty = 2)
abline(v = k_upper_05, col = "green", lwd = 2, lty = 2)
# Shade rejection regions
segments(x_plot[x_plot >= k_upper_05], 0, 
         x_plot[x_plot >= k_upper_05], prob_ha1[x_plot >= k_upper_05],
         col = "red", lwd = 4)
segments(x_plot[x_plot <= k_lower_05], 0, 
         x_plot[x_plot <= k_lower_05], prob_ha1[x_plot <= k_lower_05],
         col = "red", lwd = 4)
text(k_upper_05 + 5, max(prob_ha1) * 0.8, 
     paste0("Power = ", power_055), pos = 4, col = "red", cex = 1.2)
legend("topright", 
       c("HA: theta = 0.55", "Critical values", "Rejection region (Power)"),
       col = c("blue", "green", "red"), 
       lwd = c(2, 2, 4), lty = c(1, 2, 1), cex = 0.8)
par(mfrow = c(1, 1))

# Part e: Sample size for desired power

target_power <- 0.9
theta_alt_e <- 0.55
alpha_e <- 0.05
# Function to calculate power for given n (two-sided test)
calculate_power_twosided <- function(sample_size, theta_null = 0.5,theta_alt = 0.55, alpha = 0.05) {
  k_lower <- qbinom(alpha/2, sample_size, theta_null)
  k_upper <- qbinom(1 - alpha/2, sample_size, theta_null) + 1
  power <- pbinom(k_lower, sample_size, theta_alt) +(1 - pbinom(k_upper - 1, sample_size, theta_alt))
  return(power)
}
# Search for minimum n
n_values <- seq(300, 1500, by = 1)
powers <- sapply(n_values, calculate_power_twosided, theta_alt = theta_alt_e)
n_required <- n_values[which(powers >= target_power)[1]]
cat("Problem 1e. Minimum sample size for power = 0.9 when theta = 0.55:", 
    n_required)
cat("Actual power achieved:", powers[which(powers >= target_power)[1]])

# Visualization for part e

par(mfrow = c(1, 2))
# Power curve
n_plot <- seq(200, 1500, by = 10)
power_plot <- sapply(n_plot, calculate_power_twosided, theta_alt = theta_alt_e)
plot(n_plot, power_plot, type = "l", lwd = 2, col = "blue",
     xlab = "Sample Size (n)", ylab = "Power",
     main = "Power vs Sample Size\n(theta = 0.55, alpha = 0.05, two-sided)")
abline(h = target_power, col = "red", lty = 2, lwd = 2)
abline(v = n_required, col = "red", lty = 2, lwd = 2)
points(n_required, target_power, col = "red", pch = 19, cex = 1.5)
text(n_required, target_power - 0.05, 
     paste0("n = ", n_required), pos = 4, col = "red", cex = 1.1)
grid()
# Distribution comparison at n=250 vs n_required
# Show why larger n gives more power
k_lower_250 <- qbinom(alpha_e/2, 250, theta_0)
k_upper_250 <- qbinom(1 - alpha_e/2, 250, theta_0) + 1
k_lower_req <- qbinom(alpha_e/2, n_required, theta_0)
k_upper_req <- qbinom(1 - alpha_e/2, n_required, theta_0) + 1
# Convert to proportions for comparison
x_250 <- seq(100, 170, by = 1)
x_req <- seq(n_required * 0.4, n_required * 0.7, by = 1)
prop_250 <- x_250 / 250
prop_req <- x_req / n_required
prob_250 <- dbinom(x_250, 250, theta_alt_e)
prob_req <- dbinom(x_req, n_required, theta_alt_e)
plot(prop_250, prob_250, type = "h", col = "blue", lwd = 1.5,
     xlab = "Proportion of Heads", ylab = "Probability Mass",
     main = "Effect of Sample Size on Power",
     xlim = c(0.48, 0.62))
lines(prop_req + 0.002, prob_req, type = "h", col = "red", lwd = 1.5)
abline(v = 0.5, col = "black", lty = 2, lwd = 1)
abline(v = k_upper_250/250, col = "blue", lty = 2, lwd = 1)
abline(v = k_upper_req/n_required, col = "red", lty = 2, lwd = 1)
legend("topright", 
       c(paste0("n = 250 (power = ", power_055, ")"), 
         paste0("n = ", n_required, " (power = 0.9)"), 
         "H0: theta = 0.5",
         "Critical values"),
       col = c("blue", "red", "black", "black"),
       lwd = c(1.5, 1.5, 1, 1), lty = c(1, 1, 2, 2), cex = 0.7)
par(mfrow = c(1, 1))

# Part f: Bayesian posterior probability

theta_h0_f <- 0.5
theta_ha_f <- 0.55
prior_h0 <- 0.5
prior_ha <- 0.5
# Likelihood under each hypothesis
likelihood_h0 <- dbinom(observed_heads, n, theta_h0_f)
likelihood_ha <- dbinom(observed_heads, n, theta_ha_f)
# Posterior using Bayes rule
# P(HA | data) = P(data | HA) * P(HA) / P(data)
# where P(data) = P(data | H0) * P(H0) + P(data | HA) * P(HA)
marginal_likelihood <- likelihood_h0 * prior_h0 + likelihood_ha * prior_ha
posterior_ha <- (likelihood_ha * prior_ha) / marginal_likelihood
posterior_h0 <- (likelihood_h0 * prior_h0) / marginal_likelihood
# Bayes factor
bayes_factor <- likelihood_ha / likelihood_h0
cat("Likelihood under H0 (theta = 0.5):", likelihood_h0)
cat("Likelihood under HA (theta = 0.55):", likelihood_ha)
cat("Bayes Factor (HA/H0):", bayes_factor)
cat("Prior probability of H0:", prior_h0)
cat("Prior probability of HA:", prior_ha)
cat("Problem 1f. Posterior probability of theta = 0.55:",posterior_ha)
cat("Posterior probability of theta = 0.5:", posterior_h0)

# Part g: Personal probability assessment

# Using a Beta-Binomial model with uniform prior
# Prior: Beta(1, 1) - uniform
# Posterior: Beta(1 + 140, 1 + 110) = Beta(141, 111)
alpha_post <- 1 + observed_heads
beta_post <- 1 + observed_tails
# Probability that theta > 0.5
prob_biased_toward_heads <- 1 - pbeta(0.5, alpha_post, beta_post)
# Posterior mean and credible interval
posterior_mean <- alpha_post / (alpha_post + beta_post)
credible_interval <- qbeta(c(0.025, 0.975), alpha_post, beta_post)
cat("Posterior distribution: Beta(", alpha_post, ",", beta_post,")")
cat("Posterior mean of theta:", posterior_mean)
cat("95% Credible Interval: [", credible_interval[1], 
    ",", credible_interval[2], "]")
cat("P(theta > 0.5 | data):",prob_biased_toward_heads)

cat("Interpretation:")
cat("There is approximately a",prob_biased_toward_heads * 100, "% probability that")
cat("the coin is biased toward heads (theta > 0.5).")
cat("The evidence:")
cat("The two-sided p-value of", p_value_two_sided,"suggests marginal evidence")
cat("against the fair coin hypothesis at conventional significance levels.")
cat("The 95% credible interval includes 0.5, suggesting uncertainty.")
cat("Personal opinion:", prob_biased_toward_heads * 100, "% probability on")
cat("the coin being biased for heads, based on:")
cat("1. The observed proportion (0.56) exceeds 0.5")
cat("2. The Bayesian posterior strongly supports this")
cat("3. However, the evidence is not strong enough to be definitive")

cat("P.S. With visualizations generative AI helped me a lot, so code that makes them for 80% belongs to them.")
cat("The only reason that I found and only justification for me, is that I want to complete the task that requires math skills and not wasting time on pretty grphs.")
cat("Next prompt was used - make a great visualization outof this data, comants and description")
# Problem 2: Polygraph Analogy

# Given data from table
truthful_correct <- 131  # Tester thinks truthful, tester is truthful
truthful_incorrect <- 15  # Tester thinks truthful, tester is lying
lying_correct <- 125  # Tester thinks lying, tester is lying
lying_incorrect <- 9  # Tester thinks lying, tester is truthful

total_truthful <- truthful_correct + lying_incorrect  # 140 actually truthful
total_lying <- truthful_incorrect + lying_correct  # 140 actually lying
total <- total_truthful + total_lying

# Part a: Type I and Type II errors

# H0: Person is telling the truth
# HA: Person is lying
# Type I error: Reject H0 when H0 is true
# = Think person is lying when they're actually truthful
type_I_error_count <- lying_incorrect  # 9
type_I_error_prob <- type_I_error_count / total_truthful
# Type II error: Fail to reject H0 when H0 is false (HA is true)
# = Think person is truthful when they're actually lying
type_II_error_count <- truthful_incorrect  # 15
type_II_error_prob <- type_II_error_count / total_lying
cat("Type I Error (False Positive):")
cat("Definition: Conclude person is lying when they're actually truthful")
cat("Count:", type_I_error_count, "out of", total_truthful, "truthful people")
cat("Problem 2a01. P(Type I error):", type_I_error_prob)

cat("Type II Error (False Negative):")
cat("Definition: Conclude person is truthful when they're actually lying")
cat("Count:", type_II_error_count, "out of", total_lying, "lying people")
cat("Problem 2a02. P(Type II error):", type_II_error_prob)

# Part b: Relationships in NHST

cat("Relationships:")
cat("1. Significance level (alpha) = P(Type I error)")
cat("Set before the test")
cat("alpha =", type_I_error_prob, "in this example")

cat("2. Power = 1 - P(Type II error)")
cat("Probability of correctly rejecting H0 when HA is true")
cat("Power =", 1 - type_II_error_prob, "in this example")
cat("3. Trade-off: Decreasing alpha generally increases P(Type II error) which decreases power")
cat("4. Both error probabilities can be reduced by increasing sample size")


# Problem 3 (z-test)

speed_limit <- 60
sigma <- 0.5 
n_cameras <- 3
alpha_speed <- 0.04

se_xbar <- sigma / sqrt (n_cameras)

cat("Setup:")
cat("Speed limit: 60 km/h")
cat("Masurement error: N(0, 0.5^2)")
cat("Number of cameras:", n_cameras)
cat("SE of x:", se_xbar)

# Part a: in a NHST context

cat("H0: Car is not speeding (mu = 60 km/h)")
cat("HA: Car is speeding (mu > 60 km/h)")
cat("This is a ONE-SIDED test (upper tail)")
cat("Test statistic: xmean (average of 3 radar readings)")

# Part b: Threshold and error rates

# i Find threshold for x
z_crit <- qnorm(1 - alpha_speed)
threshold_xbar <- speed_limit + z_crit * se_xbar

cat("Problem 3b01. Threshold for issuing ticket:x >=", threshold_xbar, "km/h")

# ii Graph
par(mfrow = c(1, 2))

# Under H0
x_h0 <- seq(58, 62, length.out = 1000)
y_h0 <- dnorm(x_h0, mean = speed_limit, sd = se_xbar)

plot(x_h0, y_h0, type = "l", lwd = 2, col = "blue",
     xlab = "Average Speed Reading (km/h)", ylab = "Density",
     main = "Distribution under H0 (mu = 60)")
abline(v = speed_limit, col = "black", lty = 2, lwd = 1)
abline(v = threshold_xbar, col = "red", lty = 2, lwd = 2)

x_reject <- x_h0[x_h0 >= threshold_xbar]
y_reject <- y_h0[x_h0 >= threshold_xbar]
polygon(c(x_reject, rev(x_reject)), c(y_reject, rep(0, length(y_reject))),
        col = rgb(1, 0, 0, 0.3), border = NA)

text(threshold_xbar, max(y_h0) * 0.5, 
     paste0("alpha = ", alpha_speed), pos = 4, col = "red", cex = 1.1)

legend("topleft", c("H0: mu = 60", "Threshold", "Type I error"),
       col = c("blue", "red", rgb(1, 0, 0, 0.3)), 
       lwd = c(2, 2, 10), lty = c(1, 2, 1), cex = 0.8)

# iii P(person getting ticket was not speeding) - This requires Bayes theorem
# We need prior probabilities, but the question asks about conditional probability
# P(not speeding | gets ticket) = P(ticket | not speeding) * P(not speeding) / P(ticket)
# Without priors, we can only say the direct probability

cat("Problem 3b03. The probability that a person getting a ticket was not speeding:")
cat("This depends on the proportion of speeders in the population (unknown).")
cat("Answer: c. Unknown (requires prior probabilities)")

# iv Percentage of tickets given in error (assuming no one speeds)
# This is just the Type I error rate
pct_error <- alpha_speed * 100

cat("Problem 3b04. Assuming no one speeds, percentage of tickets given in error:")
cat("  ", pct_error, "%")

# Part c is missing, it was kinda hard to do s I won`t include my previous work here.

# Problem 4: Uniform Distribution Test

# Test setup
theta_h0 <- 2
theta_ha <- 2  # Testing if tilta != 2
alpha_unif <- 0.05  # implied from context

cat("Setup:")
cat("X ~ Uniform[0, tilta]")
cat("H0: tilta = 2")
cat("HA: tilta != 2")
cat("Decision rule: Reject H0 if x <= 0.1 or x >= 1.9")


# Part a: Probability of Type I error


# Under H0: X ~ Uniform[0, 2]
# P(Type I) = P(x <= 0.1 or x >= 1.9 | tilta = 2)
p_lower <- 0.1 / theta_h0
p_upper <- (theta_h0 - 1.9) / theta_h0
type_I_prob <- p_lower + p_upper

cat("Under H0 (tilta = 2), X ~ Uniform[0, 2]")
cat("P(X <= 0.1) =", p_lower)

# i Find threshold for x
z_crit <- qnorm(1 - alpha_speed)
threshold_xbar <- speed_limit + z_crit * se_xbar

cat("Problem 3b01. Threshold for issuing ticket:")
cat("x >=", threshold_xbar, "km/h")

# ii Graph
par(mfrow = c(1, 2))

# Under H0 (not speeding)
x_h0 <- seq(58, 62, length.out = 1000)
y_h0 <- dnorm(x_h0, mean = speed_limit, sd = se_xbar)

plot(x_h0, y_h0, type = "l", lwd = 2, col = "blue",
     xlab = "Average Speed Reading (km/h)", ylab = "Density",
     main = "Distribution under H0 (mu = 60)")
abline(v = speed_limit, col = "black", lty = 2, lwd = 1)
abline(v = threshold_xbar, col = "red", lty = 2, lwd = 2)

# Shade rejection region
x_reject <- x_h0[x_h0 >= threshold_xbar]
y_reject <- y_h0[x_h0 >= threshold_xbar]
polygon(c(x_reject, rev(x_reject)), c(y_reject, rep(0, length(y_reject))),
        col = rgb(1, 0, 0, 0.3), border = NA)

text(threshold_xbar, max(y_h0) * 0.5, 
     paste0("α = ", alpha_speed), pos = 4, col = "red", cex = 1.1)

legend("topleft", c("H0: mu = 60", "Threshold", "Type I error"),
       col = c("blue", "red", rgb(1, 0, 0, 0.3)), 
       lwd = c(2, 2, 10), lty = c(1, 2, 1), cex = 0.8)

# iii P(person getting ticket was not speeding) - This requires Bayes theorem
# We need prior probabilities, but the question asks about conditional probability
# P(not speeding | gets ticket) = P(ticket | not speeding) * P(not speeding) / P(ticket)
# Without priors, we can only say the direct probability

cat("Problem 3b03. The probability that a person getting a ticket was not speeding:")
cat("This depends on the proportion of speeders in the population (unknown).")
cat("Answer: c. Unknown (requires prior probabilities)")

# iv Percentage of tickets given in error (assuming no one speeds)
# This is just the Type I error rate
pct_error <- alpha_speed * 100

cat("Problem 3b04. Assuming no one speeds, percentage of tickets given in error:")
cat("  ", pct_error, "%")
