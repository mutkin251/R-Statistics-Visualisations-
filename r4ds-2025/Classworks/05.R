.libPaths(c(file.path("D:", "R-4.5.1", "library"), .libPaths()))


library(tidyverse)
library(pacman)
pacman::p_load(
  tidyverse,
  rvest,
  janitor,
  ggrepel,
  jsonlite,
  httr2
  )

url <- "http://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression"
m100 <- read_html(url)
m100

pre_iaaf <- 
  m100 |>
  html_element(".wikitable:nth-child(11) :nth-child(1)") |> 
  html_table() |> 
  janitor::clean_names() |>         
  mutate(date = mdy(date))

pre_iaaf

pre_iaaf <- 
  m100 |>
  html_element(".wikitable:nth-child(11) :nth-child(1)") |> 
  html_table() |> 
  janitor::clean_names() |>         
  mutate(date = mdy(date))

iaaf_12_76 <- 
  m100 |>
  html_element("#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(17) > tbody
") |> 
  html_table() |> 
  clean_names() |>         
  mutate(date = mdy(date))

iaaf_12_76

iaaf_77 <- 
  m100 |>
  html_element("#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(23) > tbody") |> 
  html_table() |> 
  clean_names() |>         
  mutate(date = mdy(date))

iaaf_77

iaaf_68_87 <- 
  m100 |>
  html_element(".wikitable:nth-child(27) :nth-child(1)") |> 
  html_table() |> 
  clean_names() |>         
  mutate(date = mdy(date),
         athlete = str_remove(athlete, "\\[.*\\]"))
iaaf_68_87

wr100 <- 
  bind_rows(
    pre_iaaf |> select(time, athlete, nationality, date) |> mutate(era = "Pre-IAAF"),
    iaaf_12_76 |> select(time, athlete, nationality, date) |> mutate(era = "Pre-automatic"),
    iaaf_77 |> select(time, athlete, nationality, date) |> mutate(era = "Modern"),
    iaaf_68_87 |> select(time, athlete, nationality, date) |> mutate(era = "Low-altitude")
  )
wr100 |> 
  count(nationality) |>
  ggplot(aes(x=fct_reorder(nationality,n),y=n)) +
  geom_col()+
  coord_flip()


wr100plot <- wr100 |> 
  ggplot(aes(date, time,color = fct_reorder(era,date))) +
  geom_point() +
  geom_text_repel(aes(label = athlete), size = 3, max.overlaps = 5) + # label points
  geom_smooth(method = "lm", se = FALSE) + # add trend line
  labs(
    title = "Men's 100m World Record Progression",
    x = "Date", y = "Time (seconds)",
    caption = "STAT150 | Source: Wikipedia",
    color = "Era"
  ) +
  facet_wrap(~era, ncol=2, scales="free")+
  theme_minimal() +
  scale_y_continuous(breaks = seq(9, 11, by = 0.2)) + # y axis ticks
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

ggsave("wr100plot.jpg", wr100plot, width = 10, height = 6)

# ----







base_url <- "https://www.ctrs.com.ua/smartfony/brand-apple/"
ctrs <- read_html(base_url)

ctrs

ctrs_iphn <- 
  ctrs |> 
  html_elements(".MainProductCard-module__title___3fVuF , .MainProductCard-module__price___34KIa")

html_table(ctrs_iphn[1:5])

ctrs_iphn <- html_text2(ctrs_iphn)
head(ctrs_iphn, 20)

ctrs_iphn |> 
  matrix(nrow = 2) |>
  t() |>
  as_tibble() |> 
  set_names(c("model", "price")) |> 
  mutate(
    price = str_remove_all(price, "₴|\\s") |> as.numeric(),
    id = str_extract(model, "\\(([^)]+)\\)") |> str_remove_all("\\(|\\)"),
    model = str_remove(model, "\\(([^)]+)\\)") |> str_trim()
  )

pages <- paste0("page_", 1:9)
  
iphone_scrape <- function(page) {
  url <- paste0(base_url, page)
  cat("url:", url, "\n")
  
  ctrs_iphones <- url |> 
    read_html() |> 
    html_elements(".MainProductCard-module__title___3fVuF , .MainProductCard-module__price___34KIa") |> 
    html_text2() |> 
    matrix(nrow = 2) |>
    t() |>
    as_tibble() |>
    set_names(c("model", "price")) |>
    mutate(
      price = str_remove_all(price, "₴|\\s") |> as.numeric(),
      id = str_extract(model, "\\(([^)]+)\\)") |> str_remove_all("\\(|\\)"),
      model = str_remove(model, "\\(([^)]+)\\)") |> str_trim()
    )
  Sys.sleep(1)
  return(ctrs_iphones)
}

iphones <- map_dfr(pages, iphone_scrape)

# %% NYC Trees

nyc_trees <- 
  fromJSON("https://data.cityofnewyork.us/resource/uvpi-gqnh.json?$limit=2000") |> 
  as_tibble()
  
nyc_trees
nyc_trees |> glimpse()

  
nyc_trees_plot <- nyc_trees |> 
  select(longitude, latitude, stump_diam, spc_common) |> 
  mutate(
    across(longitude:stump_diam, as.numeric)
  ) |> 
  ggplot(aes(longitude, latitude, size = stump_diam, color = spc_common)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "NYC Street Trees",
    x = "Longitude", y = "Latitude",
    caption = "STAT150 | Source: NYC Open Data",
    color = "Species",
    size = "Stump Diameter (inches)"
  )

plotly::ggplotly(nyc_trees_plot)
  
# %% FRED

req <- request("https://api.stlouisfed.org/fred/")

resp <- req |>
  req_url_path_append("series", "observations") |>
  req_url_query(
    series_id = "IHLIDXUSTPITOPHE",
    api_key = Sys.getenv("FRED_API_KEY"), # usethis::edit_r_environ()
    file_type = "json"
  ) |>
  req_perform()

fred <- resp |>
  resp_body_json()

jsonedit(fred, mode = "view")

fred_tbl <-
  fred$observations |>
  bind_rows()

fred_tbl

fred_plot <- fred_tbl |>
  mutate(across(realtime_start:date, ymd)) |>
  mutate(value = as.numeric(value))  |>
  ggplot(aes(date, value)) +
  geom_line(color = "#181485") +
  labs(
    x="Date", y="Index Feb, 1 2020=100",
    title="Job Postings on Indeed in the US",
    caption="STAT150: R for Data Science | Source: FRED"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

#R selenium























  