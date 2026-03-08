# %% Google sheets + gmailr

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  gmailr,
  googlesheets4,
  tidyverse,
  janitor
)


gm_auth_configure(path = "Client_API_Key.json")

gm_auth(
  "mutkin@kse.org.ua", # your email here
  cache = "auth" # folder for token here
)

url <- "https://docs.google.com/spreadsheets/d/1yV6Vg1H0buf93PmlTJApvZ2-aaR6mfFedQmIKrpgANU/edit?usp=sharing"
spreadsheet_id  <- str_extract(url, "(?<=/d/)[a-zA-Z0-9-_]+")

submissions <- read_sheet(spreadsheet_id, sheet = "r4ds") |> 
  janitor::clean_names()

submissions

parse_last_access <- function(x) {
  # функція для одного рядка або вектора
  # тут регулярні вирази для вилучення чисел
  # (?<=\\b) - позитивний погляд назад для межі слова
  # \\d+ - одна або більше цифр
  # (?=\\s*day) - позитивний погляд вперед для "день" з можливими пробілами перед ним
  # аналогічно для годин, хвилин, секунд
  days  <- as.numeric(str_extract(x, "(?<=\\b)\\d+(?=\\s*day)"))
  hours <- as.numeric(str_extract(x, "(?<=\\b)\\d+(?=\\s*hour)"))
  mins  <- as.numeric(str_extract(x, "(?<=\\b)\\d+(?=\\s*min)"))
  secs  <- as.numeric(str_extract(x, "(?<=\\b)\\d+(?=\\s*sec)"))
  
  # заміна NA на 0
  days[is.na(days)]   <- 0
  hours[is.na(hours)] <- 0
  mins[is.na(mins)]   <- 0
  secs[is.na(secs)]   <- 0
  
  # створюємо період (можна замінити на duration() якщо потрібно)
  time_period <- days(days) + hours(hours) + minutes(mins) + seconds(secs)
  
  return(time_period)
}

Sys.time() - parse_last_access("1 hour 24 mins")

submissions_tbl <- submissions |> 
  mutate(
    last_datetime = Sys.time() - parse_last_access(last_access)
  )

send_filter_tbl <- submissions_tbl |>
  filter(roles == "Teacher")

email_body <- "
Dear {{name}},\n\nThis is a reminder to complete your R for Data Science course submission.
Please ensure that you have filled out all the required fields in the form.\n\n
Best regards,\nCourse Team
"

plot <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  labs(
    title = "Scatter plot of MPG vs Weight",
    x = "Miles Per Gallon (MPG)",
    y = "Weight (1000 lbs)"
  ) +
  theme_minimal()

ggsave("mtcars.png", plot, width = 8, height = 6)

for (i in 1:nrow(send_filter_tbl)) {
  email_adress <- send_filter_tbl$email[i]
  email_name   <- send_filter_tbl$name[i]
  email_content <- str_replace_all(email_body, "\\{\\{name\\}\\}", email_name)
  
  email <- gm_mime() |>
    gm_to(email_adress) |>
    gm_from("imiroshnychenko@kse.org.ua") |>
    gm_subject("Reminder: Complete Your R for Data Science Course Submission") |>
    gm_text_body(email_content) |> 
    gm_attach_file("mtcars.png")
  
  gm_send_message(email)
  cat("Email sent to:", email_adress, "\n")
}