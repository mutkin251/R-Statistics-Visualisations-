library(tidyverse)
library(lybricate)
install.packages(c('nycflights13'),
                 repos = 'https://cran.rstudio.com',
                 dependencies  = TRUE)
sort(round(sqrt(abs(sin(1:10))), 2),decreasing=TRUE)
#Вбудований ctrl+shift+m
1:10 |>
  sin() |>
  abs() |>
  sqrt() |>
  round(2) |>
  sort(decreasing=TRUE)
#тайдиверсовський
1:10 %|%  
  sin() %|%  
  abs() %|% 
  sqrt() %|%
  round(2) %|% 
  sort(decreasing=TRUE)
#helper for everything and documentation Fn+F1
mtcars |> 
  lm(mpg ~ disp, data = _)
mtcars %|%
  lm(mpg ~ disp, data = .)

iris_tbl <-as_tibble(iris)
iris_tbl
typeof(iris_tbl)
class(iris_tbl)
str(iris_tbl)
glimpse(iris_tbl)

cars_tbl<-read_csv('https://raw.githubusercontent.com/Aranaur/aranaur.rbind.io/main/datasets/cars/cars.csv')

starwars

penguins=as_tibble(penguins)
penguins |> 
  filter(species %in% c("Adelie","Chinstrap"),
    sex=='female') |> 
  select(!c(island,year)) #columns that we want to see

penguins |> 
  select(ends_with('s'))
penguins |> 
  select(contains("_"))

penguins |> 
  select(matches('[ec]i'))

penguins |> 
  select(where(is.numeric),everything()) #спочатку чиселки, а потім все інше
library(palmerpenguins)
library(ggthemes)


# day 2
install.packages("janitor")
library(readxl)
install.packages("readxl")
excel_sheets("wb_jp.xls")

wb_jp_tbl <- read_excel("wb_jp.xls",
                        sheet="Data",
                        skip=3) |> 
  select(-c("Country Name","Country Code", "Indicator Name"))
  pivot_longer(
    cols=-`Indicator Code`,
    names_to="year",
    values_to="values") |> 
  janitor::clean_names()
  #use ctr+space to quick find the file
penguins
penguins |> 
  arrange(desc(body_mass_g), flipper_length_mm) |> 
  select(species,body_mass_g)

penguins |> 
  slice(1:5)#from , to
  slice(c(1,20,135))#index
  slice(seq(1,n(),10))
  slice_max(body_mass_g,n=5)
set.seed(73)
penguins |> 
  slice_sample(n=5,replace=TRUE)

penguins |> 
  distinct(species)

penguins |> 
  transmute(
    species,
    bmi=body_mass_g/(flipper_length_mm/10)^2,
    bmi_group = case_when(
      bmi<=8 ~ "underweight",
      bmi>=10 & bmi<12 ~ "normal",
      bmi >=12 ~ "overweight",
      .default = NA
    )
  )


penguins |> 
  group_by(species) |> 
  summarise(
    n=n(),
    mean_body_mass=mean(body_mass_g,na.rm=TRUE),
    min_body_mass=min(body_mass_g,na.rm=TRUE),
    sd_body_mass=sd(body_mass_g,na.rm=TRUE),
    max_body_mass=max(body_mass_g,na.rm=TRUE)
  )

c(1,NA,5) |> mean(na.rm=TRUE)

glimpse(penguins)

summary(penguins)

penguins |> 
  select(-year) |> 
  summarise(
    across(where(is.numeric),
           list(avg=mean,
                stdev=sd,
                md=median),
           .names="{.fn}_{.col}"),
        # \(x) mean(x,na.rm=TRUE),
        # \(x) sd(x,na.rm=TRUE),
        # \(x) median(x,na.rm=TRUE)),
    .by = species
  )
library(nycflights13)
flights
planes0

flights |> 
  left_join(
    planes |> rename(year_built = year)) #не співпадають назви, тому міняємо їх

penguins |> 
  summarise(count=n(),
            .by = species)













