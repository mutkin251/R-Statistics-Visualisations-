library(ggrepel)
library(patchwork)
library(scales)
library(tidyverse)
library(plotly)
library(tidytuesdayR)
library(stringr)

# glimpse(taylor_albums)
# metacritic_score_mean <- mean(taylor_albums$metacritic_score)

# ggplotly(taylor_albums, aes(x=album_release,y=metacritic_score))+
#   geom_smooth()+
#   geom_point()
# gg <- plot_ly(taylor_albums,x=taylor_albums$album_release,
#               y=taylor_albums$metacritic_score,type="scatter",mode="lines+markers")
# gg
# fig <- plot_ly(data, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines')
#
# fig
#          # geom_point(mapping=aes(y=metacritic_score_mean),data=,
#          #            color='red',size=3)
# 
# 
# ggplot(mpg, aes(x = displ, y = hwy, shape = class)) +
#   geom_point()

tuesdata <- tidytuesdayR::tt_load(2023, week = 42)
taylor_album_songs <- tuesdata$taylor_album_songs
taylor_albums <- tuesdata$taylor_albums

taylor_album_songs <- taylor_album_songs |>
  mutate(album_name = str_trim(str_to_lower(album_name)))

taylor_albums <- taylor_albums |>
  mutate(album_name = str_trim(str_to_lower(album_name)))

# Середнє valence + список пісень для кожного альбому
album_songs <- taylor_album_songs |> 
  group_by(album_name) |> 
  summarise(
    songs = paste(track_name, collapse = ", "),
    avg_valence = mean(valence, na.rm = TRUE),
    .groups = "drop"
  )
# album_songs

# Об'єднання з даними про альбоми
taylor_albums <- taylor_albums |> 
  left_join(album_songs, by = "album_name") |> 
  # Витягуємо рік релізу
  mutate(
    album_year = as.numeric(substr(album_release, 1, 4)),
    label_text = paste0(
      "<b>", str_to_title(album_name), "</b><br>",
      "📅 Release: ", album_release, "<br>",
      "⭐ Metacritic: ", metacritic_score, "<br>",
      "🎵 Avg Valence: ", round(avg_valence, 2), "<br><br>",
      "<b>Tracklist:</b><br>", songs
    )
  )


# 🎨 Побудова інтерактивного графіка ----
fig <- plot_ly(
  data = taylor_albums,
  x = ~album_year,
  y = ~metacritic_score,
  type = 'scatter',
  mode = 'lines+markers',
  text = ~label_text,
  hoverinfo = 'text',
  line = list(shape = "spline", width = 4, color = "rgba(80,80,120,0.4)"),
  marker = list(
    size = 18,
    color = ~avg_valence,
    colorscale = "RdYlBu",
    line = list(color = "white", width = 2)
  )
) |>
  layout(
    title = list(
      text = "<b>Taylor Swift: Metacritic Scores Over Time</b>",
      x = 0.1,
      font = list(size = 24, color = "#2b2b2b")
    ),
    xaxis = list(
      title = "Album Release Year",
      tickformat = "d",
      showgrid = FALSE
    ),
    yaxis = list(
      title = "Metacritic Score",
      showgrid = TRUE,
      gridcolor = "rgba(220,220,220,0.4)",
      zeroline = FALSE
    ),
    hoverlabel = list(
      bgcolor = "rgba(255,255,255,0.97)",
      font = list(color = "black", size = 13)
    ),
    plot_bgcolor = "rgba(248,248,248,1)",
    paper_bgcolor = "rgba(255,255,255,1)",
    margin = list(l = 80, r = 40, t = 80, b = 60)
  )

# 🔎 Відобразити графік
fig

# ggplot(taylor_albums, aes(x=taylor_albums$album_name,y=desc(taylor_albums$metacritic_score)))+
#   geom_point()


fig_albums <- taylor_albums |>
  arrange(desc(metacritic_score)) |>
  mutate(album_name = str_to_title(album_name)) |>
  plot_ly(
    x = ~reorder(album_name, metacritic_score),
    y = ~metacritic_score,
    type = "bar",
    text = ~paste0(
      "<b>", album_name, "</b><br>",
      "📅 ", album_year, "<br>",
      "⭐ Metacritic: ", metacritic_score, "<br>",
      "🎵 Avg Valence: ", round(avg_valence, 2)
    ),
    hoverinfo = "text",
    marker = list(
      color = ~avg_valence,
      colorscale = "RdYlBu",
      line = list(color = "white", width = 1.5)
    )
  ) |>
  layout(
    title = list(
      text = "<b>🏆 Taylor Swift Albums Ranked by Metacritic Score</b>",
      x = 0.1,
      font = list(size = 22)
    ),
    xaxis = list(title = "", categoryorder = "total descending"),
    yaxis = list(title = "Metacritic Score", range = c(0, 100)),
    plot_bgcolor = "rgba(248,248,248,1)",
    paper_bgcolor = "rgba(255,255,255,1)",
    margin = list(l = 80, r = 40, t = 60, b = 100)
  )


# ==============================================================
# 🎶 Візуалізація 3: Топ-5 пісень за позитивністю (valence)
# ==============================================================

# Обчислюємо середній valence по кожній пісні
top_songs <- taylor_album_songs |>
  filter(!is.na(valence)) |>
  arrange(desc(valence)) |>
  slice_head(n = 5) |>
  mutate(
    track_name = str_to_title(track_name),
    album_name = str_to_title(album_name),
    label_text = paste0(
      "<b>", track_name, "</b><br>",
      "💿 Album: ", album_name, "<br>",
      "🎵 Valence: ", round(valence, 2)
    )
  )

taylor_albums |> 
  filter(is.na(avg_valence)) |> 
  pull(album_name)

fig_songs <- plot_ly(
  data = top_songs,
  x = ~reorder(track_name, valence),
  y = ~valence,
  type = "bar",
  text = ~label_text,
  hoverinfo = "text",
  marker = list(
    color = ~valence,
    colorscale = "RdYlBu",
    line = list(color = "white", width = 1.5)
  )
) |>
  layout(
    title = list(
      text = "<b>🎶 Top 5 Happiest Taylor Swift Songs (Valence)</b>",
      x = 0.1,
      font = list(size = 22)
    ),
    xaxis = list(title = "", categoryorder = "total descending"),
    yaxis = list(title = "Valence (Mood Score)", range = c(0, 1)),
    plot_bgcolor = "rgba(248,248,248,1)",
    paper_bgcolor = "rgba(255,255,255,1)",
    margin = list(l = 80, r = 40, t = 60, b = 100)
  )


fig          # основний скатерплот (твій)
fig_albums   # рейтинг альбомів
fig_songs    # топ-5 пісень

