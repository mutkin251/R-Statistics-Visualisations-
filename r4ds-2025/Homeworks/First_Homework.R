library(tidyverse)
# install.packages(lubricate)
# library(lybricate)
########################### ---------1--------- ##############################

set.seed(52) #для можливості відтворити результат
n <- 100
x <- runif(n, 0, 10) # числа від 0 до 10 не цілі, і дуже точні
# print(x) дивимося результат
y<-2 * x + runif(n, -1, 1) # рахуємовідхилання 

# print(sum(1:100)) # додаткові тести можливих ідей
# print(x[1:10])

mean_x<-mean(x) # це нам заміняє довгу формула суми Хі-сум(j(1:100)*Xj) чи щось тека 
# print(mean_x) для тесту і проглядування проміжних результатів
mean_y<-mean(y) #аналогічно
resultch<-sum((x-mean_x)*(y-mean_y)) #нехитрорахуємо чисельник
resultzn<-sqrt(sum((x-mean_x)^2))*sqrt(sum((y-mean_y)^2)) # знаменник від -1 до 1
result<-resultch/resultzn 
result #загальний результат
########################### -------- 2 -------- ##############################

set.seed(52)  # для можливості відтворити результат
n <- 7
BTC <- round(rnorm(n, mean = 100, sd = 200))
ETH <- round(rnorm(n, mean = 100, sd = 200))
# print(ETH)
days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
names(BTC) <- days# 2. Додаємо назви дням тижня
names(ETH) <- days
# print(BTC)
total_BTC <- sum(BTC)# 3. Загальний прибуток/збиток за тиждень
total_ETH <- sum(ETH)
# print(total_BTC)
avg_BTC <- mean(BTC)# 4. Середній денний прибуток/збиток
avg_ETH <- mean(ETH)

# print(total_BTC/n) #різні шляхи отримання одного і того ж
# print(avg_BTC)
# typeof(BTC)
#str(BTC)
best_day_BTC <- names(BTC)[which.max(BTC)] #ідею взяв з https://www.r-bloggers.com/2024/12/how-to-find-the-column-with-the-max-value-for-each-row-in-r/
best_day_ETH <- names(ETH)[which.max(ETH)]  # 5. День з найбільшим прибутком для кожної криптовалюти
# 

if(total_BTC > total_ETH){ # 6. Яка криптовалюта була більш прибутковою
  print("Bitcoin")
} else{
    print("Ethereum")
}

########################### -------- 3 -------- ##############################

planets_tbl<-read_csv("https://raw.githubusercontent.com/Aranaur/aranaur.rbind.io/refs/heads/main/lectures/kse/STAT150-r4ds/25-26/assignment/data/exoplanets_unique.csv")
# str(planets_tbl)
summary(planets_tbl)
glimpse(planets_tbl)
str(planets_tbl)
# library(readxl)
# excel_sheets("wb_jp.xls")

sum(planets_tbl$pl_rade<1,na.rm=TRUE)
mean(planets_tbl$pl_rade, na.rm = TRUE) #Average radius for exoplanet(compare to earth)
mean(planets_tbl$pl_masse, na.rm = TRUE) # AAverage mass

hottest <- planets_tbl |> 
  filter(pl_eqt == max(pl_eqt, na.rm = TRUE)) |> 
  select(pl_name, pl_eqt)

hottest$pl_eqt - 273.15  # у градусах Цельсія

sum(planets_tbl$disc_year==2025,na.rm=TRUE)

our_sys <- data.frame(
  planet = c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"),
  diameter_km = c(4879,12104,12756,6792,142984,120536,51118,49528),
  rings	= c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE),
  moons	= c(0,0,1,2,95,146,28,16),
  mean_Temperature_C	= c(167,464,15,-65,-110,-140,-195,-200),
  orbit_Period_days = c(88,225,365,687,4333,10759,30687,60190)
)
exoplanets <-(
  (mean(planets_tbl$pl_rade, na.rm = TRUE))*(our_sys[["diameter_km"]][3]))#Average radius for exoplanets
if(mean(our_sys$diameter_km,na.pr=TRUE) < exoplanets){ 
  print("Exoplanets out there ARE statisticly bigger than our planets")
} else{
  print("Exoplanets out there ARE NOT statisticly bigger than our planets")
}


set.seed(52)
exo_sample <- planets_tbl |> # Беремо не пусті значення 5 штук по 4 категоріям
  filter(!is.na(pl_rade), !is.na(pl_masse), !is.na(pl_orbper), !is.na(pl_eqt)) |> 
  sample_n(5) |>
  select(pl_name, pl_rade, pl_masse, pl_orbper, pl_eqt)

exo_sample <- exo_sample |> 
  mutate(pl_tempC = pl_eqt - 273.15) #з кельвінів в цельсій

solar_clean <- our_sys |> #зводимо до спільного знаменника
  transmute(
    pl_name = planet,
    pl_rade = diameter_km / 12756,   # у "радіусах Землі"
    pl_masse = NA,                   # маси немає в моїй таблиці, а в різних джерелах інформація розходиться в залежності від часу її отримання
    pl_orbper = orbit_Period_days,
    pl_tempC = mean_Temperature_C
  )

# Об'єднуємо
comparison <- bind_rows(exo_sample, solar_clean)
print(comparison) #Фінал





