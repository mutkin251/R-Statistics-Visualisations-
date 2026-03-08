library(tidyverse)
library(dplyr)

########################################## --- 1

#Тут я вирішував в зошиті, повірте,числавзяті не з повітря
c <- 12/7

# f(x,y)
f <- function(x, y) c * (x^2 + x*y)

fX <- function(x) c * (x^2 + x/2)
fY <- function(y) c * (1/3 + y/2)
#Наші Ешки , тут ми просто переписуємо формули через формули
EX <- integrate(function(x) x * fX(x), 0, 1)$value
EY <- integrate(function(y) y * fY(y), 0, 1)$value
EX2 <- integrate(function(x) x^2 * fX(x), 0, 1)$value
EY2 <- integrate(function(y) y^2 * fY(y), 0, 1)$value

VarX <- EX2 - EX^2
VarY <- EY2 - EY^2

EXY <- integrate(
  function(x) {
    sapply(x, function(xi) {
      integrate(function(y) xi * y * f(xi, y), 0, 1)$value
    })
  }, 0, 1
)$value 
# ОТ тут я помучався, справа в тому що вону в штики приймало що я йому список
# зкормлював, уоли треба значення, вимушено переробив от так

CovXY <- EXY - EX*EY
CorrXY <- CovXY / sqrt(VarX * VarY)

cat("c =", c,"\n")
cat("E[X] =", EX,"\n")
cat("E[Y] =", EY,"\n")
cat("Var[X] =", VarX,"\n")
cat("Var[Y] =", VarY,"\n")
cat("Cov[XY] =", CovXY,"\n")
cat("Corr[XY] =", CorrXY,"\n")
########################################## --- 2

p <- matrix(c(1/18, 1/9, 1/6,
              1/9, 1/6, 1/18,
              1/6, 1/18, 1/9), nrow = 3, byrow = TRUE)
sum(p)
# Ну, якщо 1 в сумі, то поки все ок
pX <- rowSums(p)
pY <- colSums(p)
pX
pY
# Well, X and Y are NOT independdent
########################################## --- 3

c_values <- c(0, 0.25, 0.5)
for (c in c_values) {
  cov_xy <- 4*c - 1
  cor_xy <- cov_xy # Var(X)=Var(Y)=1
  cat("c =",c , " ->  Cov(X,Y)=",cov_xy, ", Corr(X,Y)=",cor_xy)
}

########################################## --- 4
######################### a
L <- 60
f_joint <- 1/(L*L)  # 1/3600
F_joint <- function(a,b) { ifelse(a<0|b<0,0, ifelse(a>60|b>60,1, (a*b)/(L*L) )) }

######################### b
p_A_before_1230 <- 30/60

######################### c
p_c_indep <- (15/60)*(15/60) 
p_c_area <- p_c_indep
######################### d
w <- 5
area_d <- L*w - (w^2)/2

######################### e
w2 <- 15
area_meet <- 1575
p_meet <- area_meet/(L*L)

cat("Joint pdf f(a,b) = 1/3600","\n")
cat("F(a,b) = a*b/3600 for 0<=a,b<=60","\n")
cat("(b) P(A <= 30) =", p_A_before_1230, "\n")
cat("(c) P(A<15, B in [30,45]) =", p_c_indep, "\n")
cat("(d) P(0 < A-B < 5) =", area_d, "\n")
cat("(e) P(|A-B| <= 15) =", p_meet, "=", 7/16, "\n")

########################################## --- 5
#Я цю задачу сидів обдумував досить довго, але тут наче просто, просто я тупив довго
# Я вже не став писати в зошиті її, наче і складно, але і наче просто, 
# треба просто додумаися вчасно, і не почати розписувати незрозуміло що
lambda <- 2
n <- 10
var_Xi <- 1 / lambda^2
overlap <- 9
cov_XY <- overlap * var_Xi
var_X <- n * var_Xi
var_Y <- n * var_Xi
# Тут мені треба було додуматися взяти не від того що у мене є, а що мені треба
cor_XY <- cov_XY / sqrt(var_X * var_Y)
cat("Cov(X, Y) =", cov_XY, "\n")
cat("Cor(X, Y) =", cor_XY, "\n")

# Наче все, непогані задачі, перша прям на давго можна засидітися якщо формул 
# перед очима нема, дякую за цікаві задачки, сподіваюся вонимені допоможуть!