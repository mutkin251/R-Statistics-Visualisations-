###########################################################--- 1 
# 1.1 Перша задачка про дві дівчинки, коли ми знаємо про те що 1 дівчинка
# Всі можливі комбінаії: (F,F), (F,M), (M,F), (M,M)
# puppies <- c("FF", "FM", "MF", "MM")
# Знаючи що перша дівчинка будемо рахувати заметодом дерева
bothF_oneF <- (1/4) / (1/4 + 1/4)
cat("Problem 1(a): P(both female | older is female) =", bothF_oneF)

# 1.2 шанс що хочаб один мальчик
# відповідно забираємо варіант ДД
prob_atleastM <- (1/4) / (3/4)
cat("Problem 1(b): P(both male | at least one male) =", prob_atleastM)


###########################################################--- 2
#Вхідні дані
p_blue <- 0.01
p_green <- 0.99
# Вхідні шанси на розпізнання
p_blue_given_blue <- 0.99
p_blue_given_green <- 0.02
# Базова формула Баяса
p_blue_given_full <- (p_blue_given_blue * p_blue) /
  (p_blue_given_blue * p_blue + p_blue_given_green * p_green)

cat("Problem 2: P(taxi actually blue | witness says blue) =", p_blue_given_full)

# Тут будемо малювати  табличку для наглядності з 10000 випадками
# З w я помітив що це будуть ті які "розповіла" нам спостерігачка
total_cases <- 10000
blue_cases <- total_cases * p_blue
green_cases <- total_cases * p_green

w_blue_true <- blue_cases * p_blue_given_blue
w_green_true <- blue_cases * (1 - p_blue_given_blue)
w_blue_false <- green_cases * p_blue_given_green
w_green_false <- green_cases * (1 - p_blue_given_green)

table <- data.frame(
  Actual_Color = c("Blue", "Green", "Total"),
  Taxis = c(blue_cases, green_cases, total_cases),
  Witness_Says_Blue = c(w_blue_true, w_blue_false,
                        w_blue_true + w_blue_false),
  Witness_Says_Green = c(w_green_true, w_green_false,
                         w_green_true + w_green_false)
)

text <- "Your Honor,\n
Today I'm here to represent Mr.Mayers who is here because as complainant - Ms.Richardson proclaims\n
that the night of March 15 at 23:42 PM she witnessed a crime that she classified as\n
hit-and-run by the driver of the blue car. Coincidentally that night the driver of the car was Mr.Mayers\n
but as he states - he is innocent. I'm here to present you a science based approach that will\n
defend my client. As we can see from the table that I made, lets just imagine that instead of a 1\n
blue car for the whole city we would have for example 100, but with the same percentile amount of\n
green ones to make it fair. The data suggests that the witness sees blue cars as blue\n
99% of the time and green cars as blue 2% of the time. In that way, Your Honor , we used that formula\n
to calculate probability by the given formula - p_blue_given_report <- (p_report_blue_given_blue * p_blue)\n
/(p_report_blue_given_blue * p_blue + p_report_blue_given_green * p_green). The result will shock you.\n
Your Honor, the estimated probability for taxi being blue while witness claims that it was blue\n
is 0.333333. Your Honor, with such a low probability it is not enough evidence to claim that Mr.Mayers\n
is guilty. Therefore I require Mr.Mayers to be immediately released, moral compensation from\n
Ms.Richardson - 5 grands and court spends another 5 grands.\n
That's all for my turn, Your Honor."
cat(text)
print(table)

################################################################--- 3

cards <- c(1,1,1,1,2,2,2,2)
# Симуляційна перевірка 
set.seed(52)
simulate_once <- function() {
  first <- sample(cards, 1)
  remaining <- cards[-match(first, cards)] 
  # Забираємо 1 карту після того як витягуємо її
  if (first == 1) {
    extra <- sample(remaining, 1)
    sum(c(first, extra))
  # Тут ми робимо наступне: дивимося на наступну карту яку витягли і сумуємо номінали 
  } else {
    extra <- sample(remaining, 2)
    sum(c(first, extra))
  # Те ж саме,але тепер перша не 1, а 2
  }
}
sims <- replicate(100, simulate_once())
# Беремо вектор зі 100 чисел і будемо знаходити середнє
E_sim <- mean(sims)
# В теорії тут приблизно таке виходить
E_analytic <- ((1/2)*(18/7) + (1/2)*(34/7))
cat("Problem 3: Expected value E[X]")
cat("Analytic =", E_analytic)
cat("Simulation ~", E_sim)

########################################################--- 4

# Можливі кубики
dice <- c(4, 6, 8, 8)   # 1 D4, 1 D6, 2 D8
p_imov <- table(dice) / length(dice)  # для зручності виводи і  перегляду
# Функція для обчислення повних імовірностей
tot_imov <- function(R) {
  # Можливі кубики
  sides <- c(4, 6, 8)
  
  # Ймовірність отримати результат R для кожного кубика
  likelihood <- numeric(length(sides))
  for (i in seq_along(sides)) {
    if (R <= sides[i]) {
      likelihood[i] <- 1 / sides[i]
    } else {
      likelihood[i] <- 0
    }
  }

  # Апріорні ймовірності
  priors <- as.numeric(p_imov[as.character(sides)])
  # Чисельники формули Байєса
  numerators <- likelihood * priors
  # щоб сума дорівнювала 1
  posteriors <- numerators / sum(numerators)
  # Результат зручніше зберегти у вигляді іменованого вектора
  names(posteriors) <- paste0("D", sides)
  return(posteriors)
}

# a pmf з S
cat("Problem 4(a): pmf of S")
print(p_imov)

# b Для R = 3
R3 <- tot_imov(3)
cat("Problem 4(b): given R=3")
print(R3)

# Для R = 6
R6 <- tot_imov(6)
cat("Problem 4(c): given R=6")
print(R6)

# Тут очевидно що тільки кубик з 8 гранями, оскільки у інших кубиків не може такого бути,але все ж
R7 <- tot_imov(7)
cat("Problem 4(d): given R=7")
print(R7)


#####################################################--- 5

set.seed(52)
f <- function(n) {
  sample(160:200, n)  # випадкові значення росту
}

count_local_minima <- function(n) {
  heights <- f(n)
  count <- 0
  
  for (i in 1:n) {
    left <- ifelse(i == 1, n, i - 1)     # сусід зліва
    right <- ifelse(i == n, 1, i + 1)    # сусід справа
    
    if (heights[i] < heights[left] & heights[i] < heights[right]) {
      count <- count + 1
    }
  }
  return(count)
}
cat("Problem 5: Seating arrangement and relative height")
mean(replicate(10000, count_local_minima(10)))  # ~ n/3

####################################################--- 6
#a
s <- 10010101100101010100101111010001000101110101010011
s <- rbinom(50,1,0.5) # Я не дуже зрозумів чому саморобний ряд не міняє результат ні від чого, а з 
# рбіном щось таки відбувається, наче б рбіном робить теж, що я прописав ручками, але все ж
#b
max(rle(s)$lengths)
#c
set.seed(52)
simulate_longest_run <- function(nflips = 50, ntrials = 10000) {
  longest_runs <- numeric(ntrials)
  for (j in 1:ntrials) {
    trial <- rbinom(nflips, 1, 0.5)
    longest_runs[j] <- max(rle(trial)$lengths)
  }
  return(mean(longest_runs))
}
# Дивимось тричі як і замовляли
avg1 <- simulate_longest_run()
avg2 <- simulate_longest_run()
avg3 <- simulate_longest_run()
cat("Problem 6(c): simulate 50 tosses of a fair coin and estimate the average length of the longest run")
cat("Average longest run lengths:\n",
    avg1, "\n", avg2, "\n", avg3)

# d Вірогідність для >= 8
simulate_prob_run8 <- function(nflips = 50, ntrials = 10000) {
  count <- 0
  for (j in 1:ntrials) {
    trial <- rbinom(nflips, 1, 0.5)
    if (max(rle(trial)$lengths) >= 8) {
      count <- count + 1
    }
  }
  return(count / ntrials)
}
# Знову ранимо тричі
prob1 <- simulate_prob_run8()
prob2 <- simulate_prob_run8()
prob3 <- simulate_prob_run8()
cat("Problem 6(d): estimate the probability of a run of 8 or more")
cat("Probabilities of run >= 8:\n",
    prob1, "\n", prob2, "\n", prob3, "\n")