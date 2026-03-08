# Завантажуємо необхідні бібліотеки
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

# URL сторінки з декомунізацією
url <- "https://en.wikipedia.org/wiki/List_of_Ukrainian_placenames_affected_by_decommunization"

# Читаємо HTML сторінки
page <- read_html(url)

# Функція для очищення тексту
clean_text <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)
  text %>%
    str_remove_all("\\[\\d+\\]") %>%
    str_squish()
}

# МАПІНГ РЕЗОЛЮЦІЙ НА ДАТИ (розширений)
resolution_to_date <- c(
  # RVRU варіанти
  "(RVRU) 984-VIII" = "4 February 2016",
  "(RVRU) 1037-VIII" = "17 March 2016",
  "(RVRU) 1351-VIII" = "12 May 2016",
  "(RVRU) 1352-VIII" = "12 May 2016",
  "(RVRU) 1353-VIII" = "12 May 2016",
  "(RVRU) 1374-VIII" = "19 May 2016",
  "(RVRU) 1375-VIII" = "19 May 2016",
  "(RVRU) 1377-VIII" = "19 May 2016",
  "(RVRU) 1465-VIII" = "14 July 2016",
  "(RVRU) 1466-VIII" = "14 July 2016",
  "(RVRU) 1467-VIII" = "14 July 2016",
  "(RVRU) 1468-VIII" = "14 July 2016",
  "(RVRU) 2615-VIII" = "20 November 2018",
  "(RVRU) 825-VIII" = "25 November 2015",
  "(RVRU) 68-IX" = "Unknown",
  "(RVRU) 69-IX" = "Unknown",
  
  # ПВРУ варіанти (кирилиця)
  "ПВРУ 825-VIII" = "25 November 2015",
  "ПВРУ 984-VIII" = "4 February 2016",
  "ПВРУ 1037-VIII" = "17 March 2016",
  "ПВРУ 1351-VIII" = "12 May 2016",
  "ПВРУ 1352-VIII" = "12 May 2016",
  "ПВРУ 1353-VIII" = "12 May 2016",
  "ПВРУ 1374-VIII" = "19 May 2016",
  "ПВРУ 1375-VIII" = "19 May 2016",
  "ПВРУ 1377-VIII" = "19 May 2016",
  "ПВРУ 1465-VIII" = "14 July 2016",
  "ПВРУ 1466-VIII" = "14 July 2016",
  "ПВРУ 1467-VIII" = "14 July 2016",
  "ПВРУ 1468-VIII" = "14 July 2016",
  "ПВРУ 2615-VIII" = "20 November 2018",
  "ПВРУ 68-IX" = "Unknown",
  "ПВРУ 69-IX" = "Unknown"
)

# Функція для конвертації резолюції в дату (ВИПРАВЛЕНА)
convert_reference_to_date <- function(ref_text) {
  if (is.na(ref_text) || ref_text == "") return(NA_character_)
  
  # Очищуємо текст від [citation needed]
  ref_clean <- str_remove(ref_text, "\\[citation needed\\]")
  ref_clean <- str_squish(ref_clean)
  
  # Шукаємо патерни:
  # 1. (RVRU) XXX-VIII
  pattern1 <- "\\(RVRU\\)\\s*\\d+-[IVX]+"
  match1 <- str_extract(ref_clean, pattern1)
  
  if (!is.na(match1) && match1 %in% names(resolution_to_date)) {
    return(resolution_to_date[[match1]])
  }
  
  # 2. ПВРУ XXX-VIII (кирилиця)
  pattern2 <- "ПВРУ\\s*\\d+-[IVX]+"
  match2 <- str_extract(ref_clean, pattern2)
  
  if (!is.na(match2) && match2 %in% names(resolution_to_date)) {
    return(resolution_to_date[[match2]])
  }
  
  # Якщо не знайшли, повертаємо оригінал
  return(ref_text)
}

# ============================================
# ДЕКОМУНІЗАЦІЯ - всі таблиці
# ============================================

all_tables <- page %>% html_nodes("table.wikitable")

cat("Знайдено таблиць:", length(all_tables), "\n\n")

all_data <- list()

for (table_num in seq_along(all_tables)) {
  
  cat(sprintf("=== Обробка таблиці %d ===\n", table_num))
  
  table_node <- all_tables[[table_num]]
  
  header_row <- table_node %>% html_nodes("tr") %>% .[[1]]
  headers <- header_row %>% html_nodes("th") %>% html_text(trim = TRUE)
  
  cat("Заголовки:", paste(headers, collapse = " | "), "\n")
  
  rows <- table_node %>%
    html_nodes("tr") %>%
    tail(-1)
  
  table_data <- list()
  
  # Змінні для rowspan
  rowspan_type <- list(value = NA, counter = 0)
  rowspan_oblast <- list(value = NA, counter = 0)
  rowspan_status <- list(value = NA, counter = 0)
  
  for (i in seq_along(rows)) {
    row <- rows[[i]]
    
    th_cells <- row %>% html_nodes("th")
    td_cells <- row %>% html_nodes("td")
    
    total_cells <- length(th_cells) + length(td_cells)
    
    if (total_cells == 0) next
    
    # ЗАПОВНЮЄМО З КІНЦЯ
    td_idx <- length(td_cells)
    
    # Остання колонка - References/Date
    if (td_idx >= 1) {
      date_ref <- td_cells[[td_idx]] %>% html_text(trim = TRUE)
      td_idx <- td_idx - 1
    } else {
      date_ref <- NA_character_
    }
    
    # Передостання - New Name
    if (td_idx >= 1) {
      new_name <- td_cells[[td_idx]] %>% html_text(trim = TRUE)
      td_idx <- td_idx - 1
    } else {
      new_name <- NA_character_
    }
    
    # Середня колонка - Oblast/Raion/Status
    if (rowspan_oblast$counter > 0) {
      oblast_raion <- rowspan_oblast$value
      rowspan_oblast$counter <- rowspan_oblast$counter - 1
    } else {
      if (td_idx >= 1) {
        cell <- td_cells[[td_idx]]
        oblast_raion <- cell %>% html_text(trim = TRUE)
        
        rowspan_attr <- cell %>% html_attr("rowspan")
        if (!is.na(rowspan_attr)) {
          rowspan_oblast$value <- oblast_raion
          rowspan_oblast$counter <- as.integer(rowspan_attr) - 1
        }
        
        td_idx <- td_idx - 1
      } else {
        oblast_raion <- rowspan_oblast$value
      }
    }
    
    # Old Name
    if (length(th_cells) >= 1) {
      old_name <- th_cells[[1]] %>% html_text(trim = TRUE)
    } else if (td_idx >= 1) {
      old_name <- td_cells[[td_idx]] %>% html_text(trim = TRUE)
      td_idx <- td_idx - 1
    } else {
      old_name <- NA_character_
    }
    
    # Type
    if (rowspan_type$counter > 0) {
      type <- rowspan_type$value
      rowspan_type$counter <- rowspan_type$counter - 1
    } else {
      if (td_idx >= 1) {
        cell <- td_cells[[td_idx]]
        type <- cell %>% html_text(trim = TRUE)
        
        rowspan_attr <- cell %>% html_attr("rowspan")
        if (!is.na(rowspan_attr)) {
          rowspan_type$value <- type
          rowspan_type$counter <- as.integer(rowspan_attr) - 1
        }
      } else {
        type <- NA_character_
      }
    }
    
    # Конвертуємо резолюцію в дату
    date_converted <- convert_reference_to_date(date_ref)
    
    # Зберігаємо дані
    table_data[[length(table_data) + 1]] <- tibble(
      table_number = table_num,
      type = clean_text(type),
      old_name = clean_text(old_name),
      oblast_raion_status = clean_text(oblast_raion),
      new_name = clean_text(new_name),
      date = clean_text(date_converted),
      original_reference = clean_text(date_ref)
    )
  }
  
  if (length(table_data) > 0) {
    table_df <- bind_rows(table_data)
    
    # Сепаратори
    separator1 <- tibble(
      table_number = table_num,
      type = NA_character_,
      old_name = NA_character_,
      oblast_raion_status = NA_character_,
      new_name = NA_character_,
      date = NA_character_,
      original_reference = NA_character_
    )
    
    separator2 <- tibble(
      table_number = table_num,
      type = "--- TABLE SEPARATOR ---",
      old_name = sprintf("TABLE %d END - ADD CITY/REGION HERE", table_num),
      oblast_raion_status = NA_character_,
      new_name = NA_character_,
      date = NA_character_,
      original_reference = NA_character_
    )
    
    separator3 <- tibble(
      table_number = table_num + 0.5,
      type = NA_character_,
      old_name = NA_character_,
      oblast_raion_status = NA_character_,
      new_name = NA_character_,
      date = NA_character_,
      original_reference = NA_character_
    )
    
    all_data[[length(all_data) + 1]] <- table_df
    all_data[[length(all_data) + 1]] <- separator1
    all_data[[length(all_data) + 1]] <- separator2
    all_data[[length(all_data) + 1]] <- separator3
    
    cat(sprintf("Таблиця %d: зібрано %d записів\n", table_num, nrow(table_df)))
    
    # Показуємо конвертовані дати
    unique_dates <- table_df %>% 
      filter(!is.na(date)) %>% 
      distinct(date, original_reference) %>%
      arrange(date)
    
    if (nrow(unique_dates) > 0) {
      cat("Дати в цій таблиці:\n")
      print(unique_dates)
    }
    cat("\n")
  }
}

# Об'єднуємо
decommunization_data <- bind_rows(all_data)

# Статистика
real_data <- decommunization_data %>%
  filter(!is.na(old_name) & type != "--- TABLE SEPARATOR ---")

cat("\n=== СТАТИСТИКА ===\n")
cat("Записів (без сепараторів):", nrow(real_data), "\n\n")

cat("Розподіл за датами:\n")
real_data %>%
  count(date, sort = TRUE) %>%
  print(n = 30)

cat("\nПриклади записів:\n")
print(head(real_data, 30))

# Збереження
decommunization_final <- decommunization_data %>%
  select(-original_reference)

write.csv(decommunization_final, "ukraine_decommunization_all_tables.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

save(decommunization_final, file = "ukraine_decommunization_all_tables.RData")

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(decommunization_final, 
                      path = "ukraine_decommunization_all_tables.xlsx")
}

cat("\n=== ГОТОВО! ===\n")

