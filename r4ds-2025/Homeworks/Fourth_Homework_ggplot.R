library(tidyverse)
library(ggrepel)
library(patchwork)
library(scales)

monsters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-27/monsters.csv')

monsters_sel <- monsters |> 
  select(name, size, type, hp, ac, cr)

monsters_hp_cr <- monsters_sel |> 
  filter(!is.na(cr)) |>
  mutate(hp_num = as.integer(sub("^(\\d+).*", "\\1", hp))) |>
  filter(!is.na(hp_num))

p1 <- ggplot(monsters_hp_cr, aes(x = cr, y = hp_num, color = size)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Більший рейтинг — вищі шанси вижити",
    subtitle = "Взаємозв’язок HP і Challenge Rating серед монстрів D&D",
    x = "Challenge Rating (CR)",
    y = "Hit Points (HP)",
    color = "Розмір монстра"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
p1


hp_by_size <- monsters |> 
  filter(!is.na(hp_number)) |>
  group_by(size) |>
  summarise(
    mean_hp = mean(hp_number, na.rm = TRUE),
    median_hp = median(hp_number, na.rm = TRUE),
    count = n()
  ) |>
  arrange(desc(mean_hp))

p2 <- ggplot(hp_by_size, aes(x = reorder(size, mean_hp), y = mean_hp, fill = size)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(mean_hp)), vjust = -0.5, size = 4) +
  labs(
    title = "Великі монстри мають вищий середній HP",
    subtitle = "Середні значення HP за розміром істоти",
    x = "Розмір",
    y = "Середній HP"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold")) +
  coord_flip()
p2


monster_langs <- monsters |>
  filter(!is.na(languages)) |>
  separate_rows(languages, sep = ";|,") |>
  mutate(languages = str_trim(languages)) |>
  filter(languages != "" & languages != "None")

lang_counts <- monster_langs |>
  count(languages, sort = TRUE) |>
  slice_head(n = 10)

p3 <- ggplot(lang_counts, aes(x = reorder(languages, n), y = n)) +
  geom_col(fill = "#4B8BBE") +
  geom_text(aes(label = n), hjust = -0.1, size = 4) +
  coord_flip() +
  labs(
    title = "Мови, якими говорять найбільше монстрів",
    subtitle = "Топ-10 найпоширеніших мов у D&D",
    x = NULL, y = "Кількість монстрів"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))
p3



library(patchwork)
final_plot <- (p1 / p2) | p3 +
  plot_annotation(
    title = "Монстри Dungeons & Dragons: анатомія сили та спілкування",
    subtitle = "Візуальний аналіз даних із TidyTuesday (травень 2025)",
    caption = "Джерело: github.com/rfordatascience/tidytuesday"
  )
final_plot
