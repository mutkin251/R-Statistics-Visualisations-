.libPaths(c(file.path("D:", "R-4.5.1", "library"), .libPaths()))

library(duckdb)
library(DBI)

json_url <- 'https://storage.googleapis.com/kagglesdsdata/datasets/1108393/1862570/One%20Piece%20json.json?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20251121%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20251121T185830Z&X-Goog-Expires=259200&X-Goog-SignedHeaders=host&X-Goog-Signature=5fd39af07b43e46caa94d01460572f978e83a7e40cbac97aeee4cec72ca4fb9ac04416c385ce3df2417503c7d8056dbb6b54a1d576f0991053f81ee2b514ad3bb51be471163d2d42f9c5f4aa29a875d050f93560d050795c8db5a1fe603a78ef0295a039583e89f02eb29473447b689ef32f6ce3ce632dada09e62e7aa1da7ad0bd9babeb05e89c5b76b781c3436da6e39315b58d63a4ddd7522690805cfe779c896686586b010d3c0b78f379a009ea6d93d1785f79203a483b98b5cfededb1989c2775787c573a2964d5bc3dfddb67098c7519944840374b5724fcc086fbac4783d774ef9ee982851e00559c3e93b296709731eb63facab8dfa0a698a974b59'

con <- dbConnect(duckdb())

dbExecute(con, "INSTALL json;")
dbExecute(con, "LOAD json;")
dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")

json_data_df <- dbGetQuery(con, sprintf("SELECT * FROM read_json_auto('%s')", json_url))

# Make the dataset structured -> Write into a DuckDB relational table(definition from Internet)

dbWriteTable(con, "episodes", json_data_df, overwrite = TRUE)

dbGetQuery(con, "PRAGMA table_info(episodes);")

episode_957 <- dbGetQuery(con, "
    SELECT *
    FROM episodes
    WHERE episode = 957;
")

print(episode_957)

# Disconnect properly
dbDisconnect(con, shutdown=TRUE)
