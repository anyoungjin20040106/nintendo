# 필요한 패키지 설치 및 로드
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("rstatix")) install.packages("rstatix")
if (!require("ggpubr")) install.packages("ggpubr")

library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)

# 데이터 로드
data <- read.csv("FeatureData.csv", stringsAsFactors = FALSE)

# 필요한 컬럼만 선택 및 전처리
data_clean <- data %>%
  filter(!is.na(user_score) & user_score != "") %>%
  mutate(user_score = as.numeric(user_score))

# 플랫폼별로 그룹화하여 요약 통계 계산
platform_stats <- data_clean %>%
  group_by(platform) %>%
  summarise(
    count = n(),
    mean_score = mean(user_score, na.rm = TRUE),
    sd_score = sd(user_score, na.rm = TRUE)
  ) %>%
  filter(count >= 10) %>%  # 10개 이상의 게임이 있는 플랫폼만 선택
  arrange(desc(mean_score))

# 분석에 사용할 플랫폼 필터링 (10개 이상의 게임이 있는 플랫폼만 선택)
platform_counts <- data_clean %>%
  count(platform) %>%
  filter(n >= 10) %>%
  pull(platform)

data_filtered <- data_clean %>%
  filter(platform %in% platform_counts)

# 플랫폼별 평균 점수 시각화 (박스플롯)
plot1 <- ggplot(data_filtered, aes(x = reorder(platform, user_score, FUN = median), 
                                 y = user_score, 
                                 fill = platform)) +
  geom_boxplot() +
  labs(title = "닌텐도 플랫폼별 게임 평점 분포",
       x = "플랫폼",
       y = "사용자 평점 (10점 만점)",
       caption = "10개 이상의 게임이 있는 플랫폼만 포함") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text = element_text(size = 10)) +
  coord_flip()

# 플랫폼별 게임 수와 평균 평점 시각화
plot2 <- ggplot(platform_stats, aes(x = reorder(platform, mean_score), 
                                 y = mean_score, 
                                 size = count,
                                 color = platform)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = paste0(round(mean_score, 1), "\n(n=", count, ")")), 
            vjust = -0.5, size = 3, lineheight = 0.8) +
  labs(title = "플랫폼별 평균 평점과 게임 수",
       x = "플랫폼",
       y = "평균 평점 (10점 만점)",
       size = "게임 수",
       caption = "괄호 안의 숫자는 각 플랫폼의 게임 수를 나타냅니다.") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.box = "vertical",
        legend.margin = margin()) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  guides(color = "none")

# ANOVA 분석 수행
anova_result <- aov(user_score ~ platform, data = data_filtered)
anova_summary <- summary(anova_result)

# 사후 분석 (Tukey's HSD)
posthoc <- TukeyHSD(anova_result)
posthoc_df <- as.data.frame(posthoc$platform)
posthoc_df$comparison <- rownames(posthoc_df)

# 유의미한 차이를 보이는 그룹만 필터링
significant_pairs <- posthoc_df[posthoc_df$p.adj < 0.05, ]

# 유의미한 차이를 보이는 그룹 시각화
if (nrow(significant_pairs) > 0) {
  plot3 <- ggplot(significant_pairs, 
                 aes(x = reorder(comparison, -diff), 
                     y = diff, 
                     ymin = lwr, 
                     ymax = upr)) +
    geom_pointrange(color = "#0072B2") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "플랫폼 간 평균 평점 차이 (Tukey HSD)",
         subtitle = "유의수준 0.05에서 유의미한 차이를 보이는 플랫폼 쌍",
         x = "플랫폼 비교",
         y = "평균 평점 차이",
         caption = "점은 평균 차이, 선은 95% 신뢰구간을 나타냅니다.") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          panel.grid.major.x = element_blank()) +
    coord_flip()
} else {
  cat("유의미한 차이를 보이는 플랫폼 쌍이 없습니다.\n")
}

# 결과 저장
dir.create("results", showWarnings = FALSE)

ggsave("results/platform_scores_boxplot.png", plot1, width = 10, height = 6, dpi = 300)
ggsave("results/platform_scores_mean.png", plot2, width = 10, height = 6, dpi = 300)

if (exists("plot3")) {
  ggsave("results/tukey_hsd_results.png", plot3, width = 12, height = 8, dpi = 300)
}

# 결과 요약 파일 생성
sink("results/analysis_results.txt")

cat("=== 닌텐도 플랫폼별 게임 평점 분석 결과 ===\n\n")

# 플랫폼별 요약 통계 출력
cat("=== 플랫폼별 요약 통계 (10개 이상 게임 보유) ===\n")
print(platform_stats)

# ANOVA 결과 요약
cat("\n=== 일원분산분석(ANOVA) 결과 ===\n")
print(anova_summary)

# Tukey HSD 사후 분석 결과
cat("\n=== Tukey HSD 사후 분석 결과 (p < 0.05) ===\n")
if (exists("significant_pairs") && nrow(significant_pairs) > 0) {
  print(significant_pairs)
} else {
  cat("유의미한 차이를 보이는 플랫폼 쌍이 없습니다.\n")
}

sink()

# 분석 완료 메시지
cat("\n분석이 완료되었습니다. 결과는 다음 파일에서 확인하세요:\n")
cat("- 플랫폼별 평점 분포: results/platform_scores_boxplot.png\n")
cat("- 플랫폼별 평균 평점: results/platform_scores_mean.png\n")
if (exists("plot3")) {
  cat("- 플랫폼 간 유의미한 차이: results/tukey_hsd_results.png\n")
}
cat("- 상세 분석 결과: results/analysis_results.txt\n")
