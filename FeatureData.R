df <- read.csv("NintendoGames.csv", stringsAsFactors = FALSE)
tf <- read.csv("data.csv", stringsAsFactors = FALSE)
df <- rbind(df, tf)
df <- df[!duplicated(df[c("title", "platform")]), ]
df <- df[, c("meta_score", "platform", "user_score")]
df[is.na(df)] <- 0
df <- df[df$user_score > 0.0&df$meta_score > 0.0, ]
write.csv(df, "FeatureData.csv", row.names = FALSE)
