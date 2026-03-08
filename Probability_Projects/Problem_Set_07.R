library(knitr)

# Helper function to print nice tables
print_bayes_table <- function(hypotheses, priors, likelihoods, title = "") {
  products <- priors * likelihoods
  posteriors <- products / sum(products)
  
  df <- data.frame(
    Hypothesis = hypotheses,
    Prior = priors,
    Likelihood = likelihoods,
    "Prior × Likelihood" = products,
    Posterior = posteriors,
    check.names = FALSE
  )
  
  cat(title, "\n")
  cat(rep("=", nchar(title)), "\n", sep = "")
  print(df, row.names = FALSE)
  cat("Sum of products:", sum(products))
  return(posteriors)
}

# PROBLEM 1
cat("MONTY HALL (Sober and Drunk)")

# Part a
cat("Sober Monty opens door B with goat")
hypotheses <- c("Car behind A", "Car behind B", "Car behind C")
priors <- c(1/3, 1/3, 1/3)
likelihoods_sober <- c(1/2, 0, 1)  # If A: random choice, If B: can't open, If C: must open B

post_sober <- print_bayes_table(hypotheses, priors, likelihoods_sober, 
                                "Sober Monty Bayes Table")
cat("STRATEGY: Switch to door C (probability", round(post_sober[3], 4))

# Part b
cat("Drunk Monty opens door B with goat")
likelihoods_drunk <- c(1/2, 0, 1/2)  # Opens B or C randomly, happened to be B with goat

post_drunk <- print_bayes_table(hypotheses, priors, likelihoods_drunk,
"Drunk Monty Bayes Table")
cat("STRATEGY: Indifferent (both A and C have probability", round(post_drunk[1], 4), ")\n\n")

# Part c
cat("Mixed scenario (P(Sober)=0.7, P(Drunk)=0.3)")
p_sober <- 0.7
p_drunk <- 0.3

hypotheses_mixed <- c("A & Sober", "A & Drunk", "B & Sober", "B & Drunk", 
                      "C & Sober", "C & Drunk")
priors_mixed <- c(1/3 * p_sober, 1/3 * p_drunk, 
                  1/3 * p_sober, 1/3 * p_drunk,
                  1/3 * p_sober, 1/3 * p_drunk)
likelihoods_mixed <- c(1/2, 1/2, 0, 0, 1, 1/2)

post_mixed <- print_bayes_table(hypotheses_mixed, priors_mixed, likelihoods_mixed,"Mixed Monty Bayes Table")

# Marginal posteriors
post_A <- post_mixed[1] + post_mixed[2]
post_B <- post_mixed[3] + post_mixed[4]
post_C <- post_mixed[5] + post_mixed[6]

cat("Marginal Posteriors:")
cat("P(Car behind A | data) =", round(post_A, 4))
cat("P(Car behind B | data) =", round(post_B, 4))
cat("P(Car behind C | data) =", round(post_C, 4))
cat("STRATEGY: Switch to door C (probability) =", round(post_C, 4))

# PROBLEM 2
cat("DICE PREDICTION")

# Part a
cat("First roll analysis")
dice_sides <- c(4, 6, 8, 12, 20)
priors_dice <- rep(1/5, 5)
likelihoods_7 <- ifelse(dice_sides >= 7, 1/dice_sides, 0)

cat("Prior Predictive Probability:")
prior_pred_7 <- sum(priors_dice * likelihoods_7)
cat("P(first roll = 7) =", round(prior_pred_7, 4))

post_dice_1 <- print_bayes_table(
  paste0(dice_sides, "-sided die"),
  priors_dice,
  likelihoods_7,"Posterior after first roll = 7"
)

posterior_pred_7 <- sum(post_dice_1 * likelihoods_7)
cat("Posterior Predictive Probability:")
cat("P(second roll = 7 | first = 7) =", round(posterior_pred_7, 4))

# Part b
cat("Posterior after n rolls of 7")
n_values <- c(1, 5, 10, 20, 50)

for (n in n_values) {
  likelihoods_n <- likelihoods_7^n
  post_n <- (priors_dice * likelihoods_n) / sum(priors_dice * likelihoods_n)
  
  cat("After n =", n, "rolls:")
  df <- data.frame(
    die = paste0(dice_sides, "-sided"),
    posterior = round(post_n, 6)
  )
  print(df, row.names = FALSE)
}

cat("As n -> inf: P(8-sided die | data) -> 1")
cat("Reason: 1/8 > 1/12 > 1/20, so (1/8)^n dominates")

# Part c
cat("Ranking next roll values after n=10")
n <- 10
likelihoods_n <- likelihoods_7^n
post_dice_10 <- (priors_dice * likelihoods_n) / sum(priors_dice * likelihoods_n)

cat("Posterior probabilities after 10 rolls of 7:")
df_10 <- data.frame(
  Die = paste0(dice_sides, "-sided"),
  posterior = round(post_dice_10, 6)
)
print(df_10, row.names = FALSE)

cat("Ranking for next roll:")
cat("1.Values 1-8: Most likely (can come from 8-sided die)")
cat("2.Values 9-12: Less likely (can only come from 12 or 20-sided)")
cat("3.Values 13-20: Least likely (can only come from 20-sided)")

# Part d
cat("Posterior predictive PMF for (n+1)st roll")
n <- 10
likelihoods_n <- likelihoods_7^n
post_dice_n <- (priors_dice * likelihoods_n) / sum(priors_dice * likelihoods_n)

# For each possible value, compute predictive probability
pred_pmf <- numeric(20)
for (value in 1:20) {
  for (i in 1:5) {
    if (dice_sides[i] >= value) {
      pred_pmf[value] <- pred_pmf[value] + post_dice_n[i] / dice_sides[i]
    }
  }
}

cat("Posterior Predictive PMF (n=10):")
df_pmf <- data.frame(
  Value = 1:20,
  probability = round(pred_pmf, 6)
)
print(df_pmf)

cat("Grouped probabilities:")
cat("P(x є {1,2,...,8}) =", round(pred_pmf[1], 6))
cat("P(x є {9,10,11,12}) =", round(pred_pmf[9], 6))
cat("P(x є {13,...,20}) =", round(pred_pmf[13], 6))

# Part e
cat("Limit as n -> inf ")
cat("As n -> inf:")
cat("P(x є {1,...,8} | data) -> 1/8 = 0.125")
cat("P(x є {9,...,20} | data) -> 0")
cat("Reason: We become certain it's the 8-sided die")

# PROBLEM 3

cat("ODDS WITH COINS")


# Setup
coin_types <- c(0.3, 0.5, 0.7)
coin_counts <- c(10, 30, 10)
total_coins <- sum(coin_counts)
priors_coins <- coin_counts / total_coins

# Part a
cat("Prior odds")
cat("Prior probabilities:")
cat("P(0.3 coin) =", priors_coins[1])
cat("P(0.5 coin) =", priors_coins[2])
cat("P(0.7 coin) =", priors_coins[3])

odds_03 <- priors_coins[1] / (1 - priors_coins[1])
odds_07 <- priors_coins[3] / (1 - priors_coins[3])
cat("Prior odds:")
cat("Odds(0.3 coin) = 1:4 or", round(odds_03, 4), ":1")
cat("Odds(0.7 coin) = 1:4 or", round(odds_07, 4), ":1")

# Part b
cat("Prior predictive odds of heads")
p_heads <- sum(priors_coins * coin_types)
p_tails <- 1 - p_heads
cat("P(Heads) =", p_heads)
cat("P(Tails) =", p_tails)
cat("Prior predictive odds(Heads) = 1:1")

# Part c
cat("Posterior odds after observing Heads")
likelihoods_heads <- coin_types

post_coins <- print_bayes_table(
  paste0("p=", coin_types, " coin"),
  priors_coins,
  likelihoods_heads,"Bayes Table after observing Heads"
)

post_odds_03 <- post_coins[1] / (1 - post_coins[1])
post_odds_07 <- post_coins[3] / (1 - post_coins[3])

cat("Posterior odds:")
cat("(i) Odds(0.3 coin) = 3:22 or", round(post_odds_03, 4), ":1")
cat("(ii) Odds(0.7 coin) = 7:18 or", round(post_odds_07, 4), ":1")

# Part d
cat("Posterior predictive odds for 2nd flip")
p_heads_2 <- sum(post_coins * coin_types)
p_tails_2 <- 1 - p_heads_2
cat("P(2nd flip = Heads | 1st = Heads) =", round(p_heads_2, 4))
cat("P(2nd flip = Tails | 1st = Heads) =", round(p_tails_2, 4))
cat("Posterior predictive odds(Heads) =", round(p_heads_2/p_tails_2, 4), ":1")

# PROBLEM 5
cat("BAYES AT THE MOVIES")

# Setup
lambda_oscar <- 10
lambda_emmy <- 15
prior_odds_ratio <- 1/10  # Oscar:Emmy = 1:10
tickets <- c(12, 10, 11, 4, 11)

# Poisson likelihood
poisson_prob <- function(k, lambda) {
  lambda^k * exp(-lambda) / factorial(k)
}

# Compute likelihoods
lik_oscar <- prod(sapply(tickets, function(k) poisson_prob(k, lambda_oscar)))
lik_emmy <- prod(sapply(tickets, function(k) poisson_prob(k, lambda_emmy)))

cat("Data: 5 hours with tickets =", tickets)

cat("Likelihoods:")
cat("L(Oscar | data) =", format(lik_oscar, scientific = TRUE))
cat("L(Emmy | data) =", format(lik_emmy, scientific = TRUE))

bayes_factor <- lik_oscar / lik_emmy
cat("Bayes Factor (Oscar/Emmy) =", round(bayes_factor, 4))

# Prior odds
p_oscar_prior <- 1/11
p_emmy_prior <- 10/11
prior_odds <- p_oscar_prior / p_emmy_prior

cat("Prior odds (Oscar:Emmy) =", round(prior_odds, 4), ":1")

# Posterior odds
posterior_odds <- prior_odds * bayes_factor
cat("Posterior odds (Oscar:Emmy) =", round(posterior_odds, 4), ":1")
cat("In ratio form: Oscar:Emmy +-= 1:", round(1/posterior_odds, 1))

p_oscar_post <- posterior_odds / (1 + posterior_odds)
p_emmy_post <- 1 / (1 + posterior_odds)
cat("Posterior probabilities:")
cat("P(Oscar | data) =", round(p_oscar_post, 4))
cat("P(Emmy | data) =", round(p_emmy_post, 4))

cat("Conclusion: Manager is MORE confident it's Emmy!")

# PROBLEM 6
cat("CENSORED DATA")

# Part a
cat("Sequential updating")

# Likelihoods
p_1_given_4 <- 1/4
p_1_given_6 <- 1/6
p_0_given_4 <- 3/4
p_0_given_6 <- 5/6

# Data
censored_data <- c(1, 0, 1, 1, 1)

# Initial odds
odds <- 1  # 1:1

cat("Initial odds (4-sided:6-sided) = 1:1")

for (i in 1:length(censored_data)) {
  if (censored_data[i] == 1) {
    odds_ratio <- p_1_given_4 / p_1_given_6
  } else {
    odds_ratio <- p_0_given_4 / p_0_given_6
  }
  
  odds <- odds * odds_ratio
  cat("After roll", i, "(value =", censored_data[i])
  cat("Odds ratio =", round(odds_ratio, 4))
  cat("New odds =", round(odds, 4), ":1")
}

cat("Final odds (4-sided:6-sided) +-=", round(odds, 2), ":1")

# Part b
cat("Evidence direction")
cat("Value '1' favors 4-sided die:")
cat("P(1|4-sided) =", p_1_given_4)
cat("P(1|6-sided) =", round(p_1_given_6, 4))
cat("Ratio =", round(p_1_given_4/p_1_given_6, 4), "> 1")

cat("Value '0' favors 6-sided die:")
cat("P(0|4-sided) =", p_0_given_4)
cat("P(0|6-sided) =", round(p_0_given_6, 4))
cat("Ratio =", round(p_0_given_4/p_0_given_6, 4), "< 1")

cat("Reflection in odds:")
cat("Roll 2 ('0'): odds decreased (moved toward 6-sided)")
cat("Rolls 1,3,4,5 ('1'): odds increased by 3/2 each (toward 4-sided)")
cat("Overall: Four '1's dominate one '0', strongly favoring 4-sided die!")



