library(tidyverse)
# seed(52)

#################################-----Problem №1-----##################################################
total_hands <- choose(52, 6)# загальна кіл-ть можливих варіантів взяти 6 рандомнихкарт з 52 
# А тут рахуємо шанс витягнути 2 пари(4 карти) і ще 2 так щобвони парою не були
two_pair <- choose(13, 2) * choose(4, 2)^2 * choose(11, 2) * 4^2 #Беремо 2 номіналу, і для 
# кожного по2 різні масті. і залишилися 11 мастей, з них беремо 2 без можливого повторення без врахування порядку
prob_two_pair <- two_pair / total_hands # ну і рахуємотакий шанс

# А тепер беремо одну комбу із трійок
one_triplet <- choose(13, 1) * choose(4,3) * choose(12,3) * 4^3 # Беремо лише один номінал 
# для трійок і даємо будь-які 3 масті ,а потім для інших трьох беремо різні номінали для уникнення 
# можливих пар і даємо їм будь-якузмастей(наче не важливо,але все ж ні), но і без урахування порядку
prob_one_triplet <- one_triplet / total_hands

#Results 
cat("Total hands:",total_hands, "\n")
cat("Two-pair hands:", two_pair, "Probability:", prob_two_pair, "\n")
cat("Tripplet hands:", one_triplet, "Probability:", prob_one_triplet, "\n")

if (prob_two_pair > prob_one_triplet) {
  cat("Two-pair is more probable.\n")
} else {
  cat("Three-of-a-kind is more probable.\n")
}

#################################-----Problem №2.1-----################################################
# Синій:   п'ять трійок,1 - 6
# Orange: 1 - 1, п'ять четвірок
# White:  три 2, три 5

blue   <- c(3,3,3,3,3,6)
orange <- c(1,4,4,4,4,4)
white  <- c(2,2,2,5,5,5)

# Тут ми будемо дивитися чи кубик А буде "бити" кубик В
prob_beats <- function(diceA, diceB) {
  outcomes <- expand.grid(a = diceA, b = diceB) #дата фрейм з всіиа комбінаціями для А і В
  # щоб потім взяти середнє і таким чином більш об'єктивно підійти до розрахунку
  mean(outcomes$a > outcomes$b)
}

p_orange_white <- prob_beats(white, orange)
p_orange_blue  <- prob_beats(orange, blue)# Власне наші комбінації
p_blue_white   <- prob_beats(blue, white)

cat("P(Orange < White) =", p_orange_white, "\n")
cat("P(Orange > Blue)  =", p_orange_blue, "\n")
cat("P(Blue > White)   =", p_blue_white, "\n")
cat("Results:")
cat("Orange beats Blue, Blue beats White, White beats Orange.\n")
#################################-----Problem №2.2-----##############################################
# Два синіх проти двох(чи двух, я не пам'ятаю, не бийте:( ) білих, беремо суму і дивимось на них
# White:  три 2, три 5
# Синій:   п'ять трійок,1 - 6
# Ідею взяв з https://stackoverflow.com/questions/66857586/creating-a-basic-r-dice-rolling-function-to-sum-dice-values

white <- c(2,2,2,5,5,5)
str(white)# 3 двійки, 3 п’ятірки
blue  <- c(3,3,3,3,3,6)    # 5 трійок, 1-6
# Функція: розподіл сум для 2 кидків одного кубика
two_dice_sums <- function(dice) { # that`s where I discovered expand.grid https://stackoverflow.com/questions/61067133/how-to-use-expand-grid-on-a-list-of-vectors
  outcomes <- expand.grid(dice, dice)   # всі комбінації
  rowSums(outcomes)                     # сума для кожної
}

# Отримуємо всі можливі суми
white_sums <- two_dice_sums(white)
print(white_sums)
blue_sums  <- two_dice_sums(blue)
print(blue_sums)
# Рахуємо ймовірність P(sum(White2) > sum(Blue2))
count <- 0
for (wsum in white_sums) {
  for (bsum in blue_sums) {
    if (wsum > bsum) {            #начасі це найпростіший варіант до якого я додумався,
                                 # не відкидаю можливочті що існують варіанти краще
      count <- count + 1
    }
  }
}

# Загальна кількість пар
total <- length(white_sums) * length(blue_sums)
p_white2_beats_blue2 <- count / total
cat("Two white dice vs two blue dice\n")
cat("Chances of winning with 2 white dice`s > Winning with 2 blue dice`s =", p_white2_beats_blue2, "\n")



#################################-----Problem №3.(1-7)-----############################################
# 1
# Для загального випадку 1 / 365^n
# Як я зрозумів питання: яка імовірність послідовності при умові рівномірного розподілу і незалежності днів народження
prob_function <- function(n) {
  return(1 / (365^n))
}

# 2 Загальне рішення для всіх випадків:
# A: someone shares your birthday
# B: some two people share a birthday
# C: some three people share a birthday
#P(A) = 1 - (364/365)^n
P_All <- function(n) {
  1 - (364/365)^n
}
# 3
# Тобто, при такій кількості людей я зможу знайти когось з таким самим днем народження як і у мене з шансом трохи більше ніж 50ї\50
find_n_A <- function() {
  n <- 1
  while (P_All(n) <= 0.5) {
    n <- n + 1
  }
  return(n)
}
cat("Найменший n для P(A) > 0.5:", find_n_A(), "\n")

# 4. Питання: чому в 3 ми отримали 253, а не 365\2 = +-182. Дайте логічне пояснення без вдавання багатов числа
# Відповідь: Логічно що якщо ми будемо в кімнаті з 36 людей, то шансна подію А дуже малий, 
# інша справа з тим, коли нас буде 183, однак це все ще половина року, 
# та і дні можуть повторюватися, але вже не з тобою,а між ними, тому все ще <50%.
# А коли в кімнаті +- 0.75 від 365, то шанси вже набагато більші, і як ми бачимо з результату 3 завдання, це так.

# 5 рішення для пункту В
sim_B <- function(npeople, ntrials=10000) {
  count <- 0
  for (i in 1:ntrials) {
    bdays <- sample(1:365, npeople, replace=TRUE) #Власне код з студіо 1 
    if (any(duplicated(bdays))) {
      count <- count + 1
    }
  }
  return(count/ntrials)
}

find_n_B <- function(point=0.9, ntrials=10000) {
  n <- 2                                           #повторюємо 10000 разів для чистоти експерименту,
  while (sim_B(n, ntrials) < point) {       #встановлюємо простенький каунтер і знаходимо 
    n <- n + 1                                    #чисоло яке нас задовольнить
  }
  return(n)
}
cat("Smallest n for P(B) > 0.9:", find_n_B(), "\n")

# 6 Find an exact formula for P(B)
# Вірогідність того, що всі дні народження різні:
# P(немає збігу) = 365/365 * 364/365 * ... * (365-n+1)/365
P_B_exact <- function(n) {
  if (n > 365) return(1)
  prob_no_match <- 1 #вираження функції через код, більше інформації про цей випадок я дізнався тут: https://betterexplained.com/articles/understanding-the-birthday-paradox/
  for (k in 0:(n-1)) {
    prob_no_match <- prob_no_match * (365 - k)/365
  }
  return(1 - prob_no_match)
}

cat("P(B) конкретно при n=23:", P_B_exact(23), "\n")

# 7. 
# Рівно тажсама задача, але вже для 3 людей


sim_C <- function(npeople, ntrials=10000) {
  count <- 0
  for (i in 1:ntrials) {
    bdays <- sample(1:365, npeople, replace=TRUE)
    if (max(table(bdays)) >= 3) {  #Тут краще використати таблицю для обробки саме на кількість днів більше 3, а не просто пара
      count <- count + 1
    }
  }
  return(count/ntrials)
}
find_n_C <- function(point=0.5, ntrials=100) {#для більш точного результату збільште тут ntrials
  n <- 2
  while (sim_C(n, ntrials) < point) {
    n <- n + 1
  }
  return(n)
}
cat("Найменший n для P(C) > 0.5:", find_n_C(), "\n")
















































