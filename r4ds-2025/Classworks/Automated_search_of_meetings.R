library(rvest)
library(xml2)
library(purrr)
library(stringr)
library(tibble)
library(tidyverse)
url <- "https://en.wikipedia.org/wiki/List_of_serving_heads_of_state_and_government_that_have_visited_Ukraine_during_the_Russian_invasion_of_Ukraine"
page <- read_html(url)
tbl_node <- page |> html_element("table.wikitable")
rows <- tbl_node |> html_elements("tr")
rows <- rows[-1]

# Безпечна обгортка для url_absolute з обробкою помилок
safe_url_absolute <- function(path, base) {
  tryCatch(
    url_absolute(path, base),
    error = function(e) {
      cat("Помилка у url_absolute:", conditionMessage(e), "\n")
      return(NA_character_)
    }
  )
}
parse_row <- function(row_node) {
  tds <- row_node |> html_elements("td")
  if (length(tds) == 0) return(NULL)
  
  state <- tds[1] |> html_text2() |> str_squish()
  head  <- if(length(tds) >= 2) tds[2] |> html_text2() |> str_squish() else NA
  dates <- if(length(tds) >= 3) tds[3] |> html_text2() |> str_squish() else NA
  details <- if(length(tds) >= 4) tds[4] |> html_text2() |> str_squish() else NA
  
  refs <- tds |> html_elements("sup.reference a")
  hrefs_raw <- refs |> html_attr("href") |> na.omit()
  
  get_reference_url <- function(h) {
    if (is.na(h) || h == "") return(NA_character_)
    
    if (startsWith(h, "#")) {
      id <- substring(h, 2)
      xpath_sel <- paste0('//*[@id="', id, '"]')
      li_node <- page %>% html_element(xpath = xpath_sel)
      
      # Заміна length() на більш надійну перевірку
      if (inherits(li_node, "xml_missing") || is.null(li_node)) return(NA_character_)
      
      ext <- li_node %>% html_elements("a.external") %>% html_attr("href") |> na.omit()
      if (length(ext) > 0) return(ext[1])
      
      a_any <- li_node %>% html_elements("a") %>% html_attr("href") |> na.omit()
      if (length(a_any) > 0) {
        h2 <- a_any[1]
        if (startsWith(h2, "/")) {
          cat("Викликаємо safe_url_absolute для h2:", h2, "\n")
          return(safe_url_absolute(h2, "https://en.wikipedia.org"))
        }
        if (startsWith(h2, "http")) return(h2)
        return(NA_character_)
      }
      
      return(safe_url_absolute(h, url))
    } else {
      if (startsWith(h, "/")) return(safe_url_absolute(h, "https://en.wikipedia.org"))
      if (startsWith(h, "http")) return(h)
      return(h)
    }
  }
  
  cat("Обробляємо рядок для держави:", state, "\n")
  print(hrefs_raw)
  
  reference_links <- tryCatch({
    hrefs_raw |> map_chr(get_reference_url) |> unique() |> discard(is.na)
  }, error = function(e) {
    cat("Помилка при обробці посилань у рядку:", state, "\n")
    cat("hrefs_raw:", paste(hrefs_raw, collapse = ", "), "\n")
    stop(e)
  })
  
  tibble::tibble(
    state = state,
    head = head,
    dates = dates,
    details = details,
    reference_links = list(reference_links)
  )
}

# Виконання розбору усіх рядків
rows_parsed <- rows |> map(parse_row) |> compact()

# Переглянути результат
rows_parsed_ex <- rows_parsed[[155]]
rows_parsed_ex$reference_links

library(dplyr)
library(tidyr)
library(lubridate)
library(writexl)
# 1. Об'єднуємо всі tibble в один датафрейм
df_all <- bind_rows(rows_parsed)

# 2. Перейменовуємо колонки
colnames(df_all) <- c("Country", "Who", "Date", "Details", "References")

# 3. Приводимо Date до формату Date
# Спочатку спробуємо розпізнати дати у різних форматах:
df_all <- df_all %>%
  mutate(
    Date_parsed = parse_date_time(Date, orders = c("mdy", "ymd", "dmy"), exact = FALSE),
    # Якщо дата не розпізналася, лишаємо NA
    Date_parsed = as.Date(Date_parsed)
  )

# 4. Фільтруємо по діапазону дат
df_filtered <- df_all %>%
  filter(!is.na(Date_parsed) & Date_parsed >= as.Date("2022-02-24") & Date_parsed <= as.Date("2023-12-31"))

# 5. Розгортаємо список References в окремі рядки
df_expanded <- df_filtered %>%
  unnest(References)

# 6. Зберігаємо у Excel
write_xlsx(df_expanded, "visitors_to_Ukraine.xlsx")

# Повідомлення
cat("Дані успішно збережені у файл visitors_to_Ukraine.xlsx у робочій директорії.\n")


