# 데이터 불러오기
df <- read.csv("NintendoGames.csv", stringsAsFactors = FALSE)
tf <- read.csv("data.csv", stringsAsFactors = FALSE)

# 필요한 열만 선택
df <- df[, c("meta_score", "title", "platform", "user_score")]
tf <- tf[, c("meta_score", "title", "platform", "user_score")]

# 데이터 결합
df <- rbind(df, tf)

# 'platform'과 'title'에 NA가 있는 행 제거
df <- df[!is.na(df$platform), ]
df <- df[!is.na(df$title), ]

# NA를 0으로 대체
df[is.na(df)] <- 0

# user_score와 meta_score가 모두 0보다 큰 행만 선택
df <- df[df$user_score > 0.0 & df$meta_score > 0.0, ]

# 'title'과 'platform' 기준으로 중복 제거
df <- df[!duplicated(df[c("title", "platform")]), ]

# 결과 확인
df

# 데이터 저장
write.csv(df, "FeatureData.csv", row.names = FALSE)
