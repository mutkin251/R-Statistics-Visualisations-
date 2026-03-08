.libPaths(c(file.path("D:", "R-4.5.1", "library"), .libPaths()))
library(tidyverse)
library(pacman)
library(scales)
library(ggplot2)
pacman::p_load(
  tidyverse,
  rvest,
  janitor,
  ggrepel,
  jsonlite,
  httr2,
  dplyr,
  duckdb
)

# url <- "https://uk.wikipedia.org/wiki/%D0%A1%D0%BF%D0%B8%D1%81%D0%BE%D0%BA_%D0%BA%D1%80%D0%B0%D1%97%D0%BD_%D0%B7%D0%B0_%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%BD%D1%8F%D0%BC"
# m100 <- read_html(url)
# tables <- m100 |> html_elements("table")
# length(tables)  # to see how many tables exist
# pre_iaaf <- tables[[3]]
# pre_iaaf_df <- pre_iaaf |> html_table(fill = TRUE)
# head(pre_iaaf_df)
# # тут вономені виводить непотрібні 2 рядки спочатку, і я їх хочу позбавитися через непотрібність і 
# # через те що вони псують заголовки моїх стовбців
# 
# pre_iaaf_df_clean <- pre_iaaf_df[-c(1,2), ]
# names(pre_iaaf_df_clean) <- c(
#   "Місце", "Країна", "2006", "2009", "2012", "2015", "2021", "Щільність"
# )
# pre_iaaf_df_clean <- pre_iaaf_df_clean |>
#   mutate(across(
#     c("2006", "2009", "2012", "2015", "2021", "Щільність"),
#     ~str_replace_all(., "[^0-9]", "") |> as.numeric()
#   ))
# pre_iaaf_df$`2006`[1:10]
# pre_iaaf_df_clean <- pre_iaaf_df_clean |>
#   mutate(across(
#     c("2006", "2009", "2012", "2015", "2021", "Щільність"),
#     ~ . |>
#       str_replace_all("[[:space:]]", "") |>
#       str_replace_all("[^0-9]", "") |> 
#       na_if("") |> 
#       as.numeric()
#   ))

# 
# length(pre_iaf)
# pre_iaaf_df_clean
# 
# clean_numeric_columns <- function(df) {
#   df |> 
#     mutate(across(
#       -c(Країна),
#       ~ .x |>
#         str_replace_all("[^0-9]", "") |>  # все, крім цифр
#         as.ineger()                      # в число
#     ))
# }
# pre_iaaf_df_clean <- clean_numeric_columns(pre_iaaf_df_clean)
# pre_iaaf_df_clean

# Одна з робочих версій, однак мав відкинути її черз приколизтим що в результаті воно виводило ліст()

# url <- "https://en.wikipedia.org/wiki/List_of_countries_by_suicide_rate"
# m100 <- read_html(url)
# pre_iaf <- m100 |> 
#   html_elements(".wikitable sortable static-row-numbers sticky-header-multi sort-under-center col1left jquery-tablesorter") |> 
#   html_elements("tbody") |> 
#   html_table()
# pre_iaf

# url <- "https://en.wikipedia.org/wiki/List_of_countries_by_suicide_rate"
# page1 <- read_html(url)
# 
# suicide_df <- page1 |>
#   html_elements("table.wikitable") |>
#   html_children() |>
#   html_table(fill = TRUE)
# suicide_df
# suicide_df <- suicide_df[2]
# length(suicide_df)
# suicide_df <- suicide_df[0:1]
# suicide_df
# # suicide_df <- suicide_df[1:length(suicide_df)]
# # suicide_df
# colnames(suicide_df[[1]]) <- c("Location", "Region", "All", "Male", "Female", "M/F")
# suicide_df
# suicide_clean <- suicide_df |>
#   clean_names() |>
#   mutate(across(
#     c(all, male, female, mf),
#     ~ str_replace_all(., "[^0-9.]", "") |> na_if("") |> as.numeric()
#   )) |>
#   filter(!is.na(country) & country != "" & !country %in% c("World", "—"))
# suicide_clean

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_suicide_rate"
page1 <- read_html(url)

suicide_list <- page1 |>
  html_elements("table.wikitable") |>
  html_table(fill = TRUE)

suicide_df <- suicide_list[[1]]

suicide_df <- suicide_df[-1, ]

colnames(suicide_df) <- c("Location", "Region", "All", "Male", "Female", "M/F")

suicide_clean <- suicide_df |>
  mutate(across(
    c(All, Male, Female, `M/F`),
    ~ str_replace_all(., "[^0-9.]", "") |> na_if("") |> as.numeric()
  )) |>
  filter(Location != "World", !is.na(All))

head(suicide_clean)


suicide_clean |>
  slice_max(All, n = 15) |>
  ggplot(aes(x = reorder(Location, All), y = All, fill = Region)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 Countries by Suicide Mortality Rate (WHO, 2021)",
    y = "Suicides per 100K population",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10)
  )


url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

page2 <- read_html(url)
page2

dell <- page2 |> 
  html_elements("table.wikitable") |> 
  html_children() |> 
  html_table(fill = TRUE)
dell <- dell[[2]]

dell <- dell |> 
  select(-"% ofworld", -"Date", -"Source (official or fromthe United Nations)", -"Notes")

dell |> head()

data_inner = merge(dell, suicide_clean,by="Location",all=TRUE)
data_inner <- data_inner |> janitor::clean_names()
# data_inner <- data_inner |>
#   mutate(location = reorder(location, all))
data_inner <- data_inner |>
  mutate(
    across(c(all, male, female, m_f), ~ as.numeric(gsub(",", "", .x))),
    location = reorder(location, -all, FUN = function(x) ifelse(is.na(x), 0, x))
  )
levels(data_inner$location)[1:10]
data_inner <- data_inner |>
  mutate(
    across(c(all, male, female, m_f), ~ as.numeric(gsub(",", "", .x)))
  ) |>
  arrange(desc(all)) |>
  mutate(location = factor(location, levels = unique(location)))

data_inner


data_inner <- data_inner |>
  mutate(
    population = as.numeric(gsub(",", "", population)),
    all = as.numeric(all),
    est_deaths = (all / 100000) * population
  )

top15_deaths <- data_inner |>
  filter(!is.na(est_deaths)) |>
  arrange(desc(est_deaths)) |>
  slice_head(n = 15)

ggplot(top15_deaths, aes(x = reorder(location, est_deaths), y = est_deaths, fill = region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Estimated Number of Suicide Deaths (Top 15 Countries, WHO 2021)",
    subtitle = "Based on population × suicide rate per 100K",
    x = NULL,
    y = "Estimated deaths",
    fill = "Region"
  ) +
  theme_minimal(base_size = 13)

## 
# 
# 
# https://fred.stlouisfed.org/series/FPCPITOTLZGWLD
# 
# api_FRED_key <- bd34daf5b426771c8367111ecb36e8e7
# 
# 
# library(httr2)
# 
# req <- request("https://fred.stlouisfed.org/")
# 
# resp <- req %>%
#   req_url_path_append("series","FPCPITOTLZGWLD") |> 
#   req_url_query(
#     series_id = "FPCPITOTLZGWLD",
#     api_key = "bd34daf5b426771c8367111ecb36e8e7",
#     # Sys.getenv("FRED_API_KEY")# usethis::edit_r_environ()
#     file_type = "json"
#   ) |>  
#   req_perform()
# 
# fred <- resp |>  
#   resp_body_json(resp, check_type = TRUE)
# 
# jsonedit(fred, mode = "view")


if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(httr2, jsonlite, dplyr, ggplot2)

games_id <- 8502020240
# Your player ID (Steam/OpenDota account ID)
player_id <- 1515008780
req1 <- request(paste0("https://api.opendota.com/api/matches/", games_id))
req <- request(paste0("https://api.opendota.com/api/players/", player_id))

# Send the request and get the response
resp1 <- req_perform(req1)
resp <- req_perform(req)

# Parse JSON content into R list
game_data <- resp_body_json(resp1, simplifyVector = TRUE)
player_data <- resp_body_json(resp, simplifyVector = TRUE)
game_data <- game_data$players
game_data
game_data[sapply(game_data, is.null)] <- NA
# str(player_data, max.level = 2)
# profile <- player_data$profile
# profile[sapply(profile, is.null)] <- NA

# player_df <- as.data.frame(profile) |> 
#   select(account_id, personaname, loccountrycode, avatarmedium)
# player_df

game_data_df <- as.data.frame(game_data) |> 
  select(player_slot, hero_id, kills, deaths,assists)
game_data_df


ggplot(game_data_df, aes(x = factor(hero_id), y = kills, fill = kills)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#00BFC4", high = "#F8766D") +
  labs(
    title = paste("Kills per Player — Match", games_id),
    x = "Hero ID",
    y = "Kills"
  ) +
  theme_minimal(base_size = 13)



ggplot(game_data_df, aes(x = factor(hero_id), y = ((kills + 0.5 * assists) / pmax(deaths, 1)), fill = kills)) +
  geom_col() +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = paste("KDA Ratio for Each Player — Match", games_id),
    x = "Hero ID",
    y = "KDA Ratio"
  ) +
  theme_minimal(base_size = 13)
