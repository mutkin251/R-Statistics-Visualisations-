######################################### --- Problem 1

# для: f(x|a) = (a^5 / 4!) * x^4 * e^(-ax)
set.seed(52)
m <- 100
sample_data <- rgamma(m, shape=5, rate=2)
S <- sum(sample_data)
a_mle <- 5*m / S

cat("Example calculation with", m, "data points:")
cat("Sum of data (S) =", round(S, 4))
cat("MLE estimate: a_hat =", round(a_mle, 4))

######################################### --- Problem 2

############################################################# Part a

# Я так розумію тут треба простописати як воно буде без чиселок?
cat("Model: yi ~ N(axi + b, sigma^2)")
cat("PDF: f(yi | xi, a, b, sigma) = (1/(sigma*sqrt(2*pi))) * exp(-(yi - axi - b)^2 / (2*sigma^2))")
cat("Likelihood: L(a,b,sigma | x1,y1) = (1/(sigma*sqrt(2*pi))) * exp(-(y1 - ax1 - b)^2 / (2*sigma^2))")
cat("Log-likelihood: l(a,b,sigma) = -log(sigma) - 0.5*log(2*pi) - (y1 - ax1 - b)^2 / (2*sigma^2)")
#я не дуже впевнений, це те до чого я прийшов +- за 1.5 години спроб, скоріш за все це правильно

############################################################# Part b

cat("Part (b): MLE for data (1,8), (3,2), (5,1) with sigma=3")
x <- c(1, 3, 5)
y <- c(8, 2, 1)
sigma <- 3
n <- length(x)

for(i in 1:n) {
  cat(sprintf("  (x%d, y%d) = (%d, %d)", i, i, x[i], y[i]))
}
cat("Log-likelihood:")
cat("l(a,b) = -n*log(sigma) - n*log(sqrt(2*pi)) - (1/(2*sigma^2)) * sum((yi - axi - b)^2)")
cat("To maximize/minimize sum of squared errors: sum((yi - axi - b)^2)")
cat("Partial derivatives:")
cat("dl/da = (1/sigma^2) * sum((yi - axi - b) * xi) = 0")
cat("dl/db = (1/sigma^2) * sum(yi - axi - b) = 0")

cat("Final equations:")
cat("sum(xi * yi) = a*sum(xi^2) + b*sum(xi)")
cat("sum(yi) = a*sum(xi) + n*b")

sum_x <- sum(x)
sum_y <- sum(y)
sum_x2 <- sum(x^2)
sum_xy <- sum(x*y)

cat("Sums:")
cat(sprintf("  sum(xi) = %d", sum_x))
cat(sprintf("  sum(yi) = %d", sum_y))
cat(sprintf("  sum(xi^2) = %d", sum_x2))
cat(sprintf("  sum(xi*yi) = %d", sum_xy))
cat(sprintf("  n = %d", n))

# Тут я затупив, почав щось не те вирішувати, і потім зрозумів що це треба матрицями робить
A <- matrix(c(sum_x2, sum_x, sum_x, n), nrow=2, byrow=TRUE)
b_vec <- c(sum_xy, sum_y)
solution <- solve(A, b_vec)
a_mle <- solution[1]
b_mle <- solution[2]

cat("Solving the system:")
cat(sprintf("  %d = %d*a + %d*b (equation 1)", sum_xy, sum_x2, sum_x))
cat(sprintf("  %d = %d*a + %d*b (equation 2)", sum_y, sum_x, n))

cat("Solution:")
cat(sprintf("  a_MLE = %.4f", a_mle))
cat(sprintf("  b_MLE = %.4f", b_mle))

############################################################# Part c

par(mar=c(5,5,4,2))
plot(x, y, pch=19, cex=1.5, col="blue", 
     xlab="x", ylab="y", 
     main="Linear Regression: y = ax + b",
     xlim=c(0, 6), ylim=c(0, 10),
     cex.lab=1.2, cex.main=1.3)

# regression line
abline(a=b_mle, b=a_mle, col="red", lwd=2)

equation_text <- sprintf("y = %.4f*x + %.4f", a_mle, b_mle)
text(4, 8, equation_text, col="red", cex=1.1)
grid(col="gray", lty="dotted")

######################################### --- Problem 3

############################################################# Part a

data_uniform <- c(1.2, 2.1, 1.3, 10.5, 5)

cat("For uniform(a,b) distribution:")
cat(" f(x|a,b) = 1/(b-a) if a <= x <= b, or 0")

cat("Likelihood: L(a,b) = (1/(b-a))^n if a <= min(xi) and max(xi) <= b or 0")

cat("To maximize we need to minimize (b-a)")
cat("a <= min(xi) and max(xi) <= b")

cat("So: a_MLE = min(xi) and b_MLE = max(xi)")

a_uniform_mle <- min(data_uniform)
b_uniform_mle <- max(data_uniform)

cat("Solution:")
cat(sprintf("  a_MLE = min(data) = %.1f", a_uniform_mle))
cat(sprintf("  b_MLE = max(data) = %.1f", b_uniform_mle))

############################################################# Part b

cat("By the same logic as in part a:")
cat("Likelihood is maximized when (b-a) is minimized")
cat("Subject to constraints: a <= all xi and all xi <= b")

cat("So: a_MLE = min(x1, x2, ..., xn) and b_MLE = max(x1, x2, ..., xn)")

set.seed(52)
n_uniform <- 50
true_a <- 2
true_b <- 8
sample_uniform <- runif(n_uniform, true_a, true_b)

a_est <- min(sample_uniform)
b_est <- max(sample_uniform)

cat(sprintf("True parameters: a=%.1f, b=%.1f", true_a, true_b))
cat(sprintf("Estimated: a_MLE=%.4f, b_MLE=%.4f", a_est, b_est))



