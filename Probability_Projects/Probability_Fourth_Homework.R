library(tidyverse)
library(ggplot2)

##########################################---   1
# пдф і цдф для експоненти
dexp_r <- function(x, lambda) {
  ifelse(x >= 0 lambda * exp(-lambda * x), 0)
}
pexp_r <- function(x, lambda) {
  ifelse(x >= 0, 1 - exp(-lambda * x), 0)
}

########   a
lambda <- 1.2
x1 <- 2.0
# base work 
p_formula <- exp(-lambda * x1)
p_formula

#######   b
# середнє і стандартне відхилення
mean_X <- 1 / lambda
var_X  <- 1 / (lambda^2)
sd_X   <- sqrt(var_X)
mean_X
sd_X

######    c

lambda_new <- 1.2
lambda_for_T <- 2 * lambda_new

# пдф і цдф для T
F_T <- function(t) { 
  ifelse(t >= 0, 1 - exp(-lambda_for_T * t), 0) }
f_T <- function(t) { 
  ifelse(t >= 0, lambda_for_T * exp(-lambda_for_T * t), 0) }

tvalues <- seq(0, 3, by = 0.5)
data.frame(t = tvalues,
           P_T_t = exp(-lambda_for_T * tvalues),
           CDF_T = sapply(tvalues, F_T),
           PDF_T = sapply(tvalues, f_T)
           )

# симулюємо як і просили
set.seed(52)
n <- 5e5
X1 <- rexp(n, rate = lambda)
X2 <- rexp(n, rate = lambda)
T_sim <- pmin(X1, X2)
mean(T_sim)
sd(T_sim)

#####   d
lambda1 <- 2
lambda2 <- 3
lambda3 <- 5

# це наче геометричний розподіл по принципу - до поки не випаде
lambda_total <- lambda1 + lambda2 + lambda3
when_first_fail <- 1 / lambda_total
lambda_total
when_first_fail  # тут не звертайте увагу, тут в роках виводить

# checking
set.seed(52)
n <- 200000
B1 <- rexp(n, rate = lambda1)
B2 <- rexp(n, rate = lambda2)
B3 <- rexp(n, rate = lambda3)  # тутпросто рахуємо покожному окремо
Tmin_simul <- pmin(B1, B2, B3)
mean(Tmin_simul)
sd(Tmin_simul)



emp_cdf <- ecdf(Tmin_simul)
zmin <- seq(0, 1, length.out = 100)
plot(zmin, emp_cdf(zmin), type = "l", col = "blue", lwd = 2,
     main = "Empirical CDF of Tmin vs Theoretical CDF",
     xaxis = "t (years)", yaxis = "CDF")
lines(zmin, 1 - exp(-lambda_total * zmin), col = "red", lwd = 2, lty = 2)
legend("bottomright", legend = c("calculated", "theoretical"),
       col = c("blue", "red"), lwd = 2, lty = c(1,2))

##########################################---   2
#####  a

n <- 400
P_volod <- 0.50
P_petr <- 0.20
P_rest <- 1 - (P_volod + P_petr)

clt_prob <- function(P_true, n, P_cut, direction = "greater") {
  mean_p <- P_true
  sd_p <- sqrt(P_true * (1 - P_true) / n)
  z <- (P_cut - mean_p) / sd_p
  if (direction == "greater") {
    return(1 - pnorm(z))
  } else if (direction == "less") {
    return(pnorm(z))
  } else {
    stop("direction should be grater or less")
  }
} # короче, це сі ті і для ЦЛТ

#####   a

P_cut_vol <- 0.525
P_vol_over_52_5 <- clt_prob(P_volod, n, P_cut_vol, "greater")
P_vol_over_52_5

#####   b

P_cut_rest <- 0.31
P_rest_less_31 <- clt_prob(P_rest, n, P_cut_rest, "less")
P_rest_less_31

cat("Results (Elections problem)")
cat("P(Volodymyr >= 52.5 %)", P_vol_over_52_5)
cat("P(Yulia Viktor Leonid < 31 %)", P_rest_less_31)

##########################################---   3

n <- 1000 
a <- -5 # min 
b <- 5  # max

mu_x <- (a + b) / 2         # тут 0 бо середнє між -5 і 5 = 0
# mu_x
var_x <- (b - a)^2 / 12 # тута признаюсь скористався фотомас, не 
                        # хотів помилитися на такому єтапі де треба просто чиселки підставити
# var_x
sd_x <- sqrt(var_x)
# sd_x 2.886751

mu_S <- n * mu_x
var_S <- n * var_x
sd_S <- sqrt(var_S)

n <- 100
# тут ми наближуємо нашу ЦЛТ по формулі з рідінгів
z_pos <- (n - mu_S) / sd_S
p_exceed <- 2 * (1 - pnorm(z_pos))

cat("CLT estimation for kopiykas")
cat("Single-order mean =,",mu_x," sd = ", sd_x)
cat("Sum mean = , sum sd = ", mu_S, sd_S)
cat("z = ", z_pos)
cat("Estimated p = P(|S_n| >",n," ) +-=", p_exceed)
# я не знаю що конкретноу мене не правильнопроблем сет чекер каже що десь 
# помилка, хоча я наче по рідінгам робив, має бути правильно

##########################################---   4

x <- seq(0, 10, length.out = 1000)
# щільність для ф_х ф_у
fX <- exp(-x)     # f_X(x)
y <- seq(0, 30, length.out = 1000)
fY <- (1/3) * exp(-y/3) # f_Y(y)

# Щільність для V = X^3
v <- seq(0, 2000, length.out = 1000)
fV <- (1/3) * v^(-2/3) * exp(-v^(1/3))
fV[1] <- (1/3) * (0 + 0)  # через ліміт до нескінченності, тут буде +-0

##########################################---   5

lambda <- 1
a <- 3 # тут підставляйте чиселки які хочете
b <- 17
#####  a

# Нууу, яне знаю, тут має бути 1, може я чогось не зрозумів, якось просто
E_X  <- 1 / lambda
E_X# в середньому з одногоччисла?
Var_X <- 1 / lambda^2
Var_X# варіенс тут буде рахуватися як хвилинки з мю в квадраті
SD_X  <- sqrt(Var_X)
SD_X

# Y = 3X
E_Y  <- 3 * E_X
E_Y
Var_Y <- 3^2 * Var_X
Var_Y
SD_Y  <- sqrt(Var_Y)
SD_Y
# Z = aX + b
E_Z  <- a * E_X + b
Var_Z <- a^2 * Var_X
SD_Z  <- sqrt(Var_Z)

units_E <- "minutes"
units_Var <- "minutes^2"

df_sum <- tibble(
  Variable = c("X", "Y", "Z"),
  Mean = c(E_X, E_Y, E_Z),
  Variance = c(Var_X, Var_Y, Var_Z),
  SD = c(SD_X, SD_Y, SD_Z),
  Units_Mean = units_E,
  Units_Var = units_Var
)  
# просто в одному місці збираємо все і виводимо, бо консоль чуть забивалась при 
# виводі поелементно
print(df_sum)

cat("Units: {a} is lacking of dimension {b} has units of minutes")

#####   b
# E[V] = ∫^inf x^3 e^{-x} dx = 3! = 6  ледь знайшов інфу про те як це 
# розкладати якби не проблем чекер попередній варіант був би прийнятий мноюза правду
E_V <- 6
Var_units_V <- "minutes^6"
E_units_V <- "minutes^3"

cat("Expected value of V = ", E_V, E_units_V)

#####   c
# черезте шо медіана по х менше 1, треба брати логарифм
m_X <- log(2)
m_X
# тут букваль та ж формула з попередньої частини
m_V <- (log(2))^3
m_V
df_medians <- tibble(
  Variable = c("X", "V"),
  Median = c(m_X, m_V),
  Units = c("minutes", "minutes^3")
)
print(df_medians)


##########################################---   6

# Я не встиг ;(

# апдейт, я до вечора 8 числа так і не встиг зробити 6 частину,трохи був в тільті 
# і вирішив, ну ок почекаю поки викладуть рішення подивлюся потім, зараз вже нічого не поробиш
# А тут ви оновлюєте файл о здачі 9 числа весора, я б знав, я б доробив, а так невезуха, знову не доробив виходить :(