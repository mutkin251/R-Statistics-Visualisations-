# Problem 1
n <- 16
xbar <- 11
s2 <- 4
s <- sqrt(s2)
t <- (xbar - 10) / (s/sqrt(n))

# (a)
p2 <- 2*(1 - pt(t, df=15))
p2

# (b)
p1 <- 1 - pt(t, df=15)
p1

# Problem 2

# (a)
p_ex1 <- pbinom(3, 12, 0.5)
p_ex1

pbinom(2, 12, 0.5)

# (b)
p_ex2 <- 1 - pnbinom(9-1, size=3, prob=0.5)
p_ex2

#  (c)
# Статистично p-value не визначене — stopping rule невідомий, aбо я щось не зрозумів

# (d)
# Likelihood L(theta | x=3, n=12)
L1 <- function(theta) dbinom(3,12,theta)

# Likelihood L(theta | tails=9, 3 heads)
L2 <- function(theta) dnbinom(9, size=3, prob=theta)

m <- 1
n <- 1
# Prior Beta(m,n) => posterior:
posterior1 <- c(m+3, n+9)# Exp1: Beta(m+3, n+(12-3))
posterior2 <- c(m+3, n+9)# Exp2: Beta(m+3, n+9)
posterior1
posterior2

# Problem 3
# Дані
x <- c(1.76, -2.28, -0.56, 1.46, 0.59, 1.26, -1.94, -0.79, -0.86, -1.41, 2.07, 1.30)
n <- length(x)
s2 <- var(x)
chi2 <- (n-1)*s2/1      # sigma0^2 = 1
pval <- 1 - pchisq(chi2, df = n-1)
list(n=n, sample_var=s2, chi2_stat=chi2, df=n-1, p_value=pval)
# рішення при alpha=0.05
if(pval < 0.05) "Reject H0" else "Do not reject H0"

# Problem 4
obs <- c(7,13,12,9,9,13,11,10,16)
p_benford <- c(0.301,0.176,0.125,0.097,0.079,0.067,0.058,0.051,0.046)
chisq_res <- chisq.test(x = obs, p = p_benford, rescale.p = TRUE, simulate.p.value = FALSE)
chisq_res$statistic
chisq_res$parameter
chisq_res$p.value
# рішення при alpha=0.001
if(chisq_res$p.value < 0.001) "Reject H0 (does not follow Benford)" else "Do not reject H0 (consistent with Benford)"

# Problem 5
n <- 351
means <- c(clean=1.32, d5=1.26, d10=1.53, full=1.39)
vars  <- c(clean=0.56, d5=0.80, d10=0.93, full=0.82)
k <- length(means)
N <- n*k
grand_mean <- sum(means * n) / N

# SSB and SSW (by-group summary using means and sample variances)
SSB <- sum(n * (means - grand_mean)^2)
SSW <- sum((n-1) * vars)
dfB <- k-1
dfW <- N-k
MSB <- SSB/dfB
MSW <- SSW/dfW
Fstat <- MSB/MSW
p_anova <- 1 - pf(Fstat, dfB, dfW)
list(N=N, k=k, SSB=SSB, SSW=SSW, MSB=MSB, MSW=MSW, F=Fstat, p_value=p_anova)

# Рішення при alpha=0.01
if(p_anova < 0.01) "Reject H0 (not all means equal)" else "Do not reject H0"

# (b) 
test_pair <- function(mean1, var1, n1, mean2, var2, n2) {
  sp2 <- ((n1-1)*var1 + (n2-1)*var2) / (n1+n2-2)
  tstat <- (mean1 - mean2) / sqrt(sp2*(1/n1 + 1/n2))
  df <- n1 + n2 - 2
  pval <- 1 - pt(tstat, df)
  c(t=tstat, df=df, p_one_sided=pval)
}

t_clean <- test_pair(means["d10"], vars["d10"], n, means["clean"], vars["clean"], n)
t_5day  <- test_pair(means["d10"], vars["d10"], n, means["d5"],    vars["d5"],    n)
t_full  <- test_pair(means["d10"], vars["d10"], n, means["full"],  vars["full"],  n)

list(d10_vs_clean=t_clean, d10_vs_5day=t_5day, d10_vs_full=t_full)

# Рішення кожного теста при alpha=0.01
sapply(list(t_clean,t_5day,t_full), function(ti) if(ti["p_one_sided"] < 0.01) "Reject" else "Do not reject")