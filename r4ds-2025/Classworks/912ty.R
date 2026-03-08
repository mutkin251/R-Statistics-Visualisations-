library(tidyverse)
library(rvest)
library(janitor)

url <-"https://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression"
ml100 <-read_html(url)
ml100

f <-
  ml100 |> 
  html_element(".wikitable:nth-child(11) :nth-child(1)") 
f

