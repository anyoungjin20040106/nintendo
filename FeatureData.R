# 데이터 불러오기
df <- read.csv("NintendoGames.csv", stringsAsFactors = FALSE)
tf <- read.csv("data.csv", stringsAsFactors = FALSE)
# 데이터 결합
df <- rbind(df, tf)
df <- df[!duplicated(df[c("title", "platform")]), ]

df <- df[, c( "meta_score","platform", "user_score")]

# NA를 0으로 대체
df[is.na(df)] <- 0


# user_score가 0보다 큰 행만 선택
df <- df[df$user_score > 0.0&df$meta_score > 0.0, ]
# 결과 확인
df

# 데이터 저장
write.csv(df, "FeatureData.csv", row.names = FALSE)
