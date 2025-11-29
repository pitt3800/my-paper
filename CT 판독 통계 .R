# ============================================================================
# CT 판독 결과 Fever Focus 분석 - R 코드
# 
# 작성일: 2024년 11월
# 목적: 응급실 발열 환자의 CT 판독결과에서 감염성 병변(fever focus) 분석
# ============================================================================

# CT_Fever_Focus_Analysis_Results 분석


install.packages("flextable")

# 1. 필수 패키지 로드 ------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr)
library(DT)
library(gtsummary)
library(flextable)

setwd("/Users/youjinlee/Library/Mobile Documents/com~apple~CloudDocs/My R/Fever c claude")
# 2. 데이터 로드 ------------------------------------------------
# 분석 결과 파일 읽기
ct_data <- read_excel("CT_Fever_Focus_Analysis_Results.xlsx", sheet = 1)

# 데이터 구조 확인
cat("=" , rep("=", 70), "\n", sep = "")
cat("데이터 개요\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("전체 환자 수:", nrow(ct_data), "명\n")
cat("변수 개수:", ncol(ct_data), "개\n")
str(ct_data)

# 3. 기술 통계 분석 ------------------------------------------------

# 3.1 Fever Focus 분포
fever_summary <- ct_data %>%
  summarise(
    총_환자수 = n(),
    발열_focus_양성 = sum(fever_focus == 1, na.rm = TRUE),
    발열_focus_음성 = sum(fever_focus == 0, na.rm = TRUE),
    양성률 = 발열_focus_양성 / 총_환자수 * 100
  )

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("Fever Focus 분석 결과\n")
cat("=" , rep("=", 70), "\n", sep = "")
print(fever_summary)

# 3.2 성별, 연령별 분포
demographic_table <- ct_data %>%
  mutate(
    age_group = case_when(
      나이 < 65 ~ "< 65세",
      나이 >= 65 & 나이 < 75 ~ "65-74세",
      나이 >= 75 & 나이 < 85 ~ "75-84세",
      나이 >= 85 ~ "≥ 85세",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(fever_focus) %>%
  summarise(
    n = n(),
    male_n = sum(성별 == "M", na.rm = TRUE),
    female_n = sum(성별 == "F", na.rm = TRUE),
    age_mean = mean(나이, na.rm = TRUE),
    age_sd = sd(나이, na.rm = TRUE),
    age_median = median(나이, na.rm = TRUE),
    age_q1 = quantile(나이, 0.25, na.rm = TRUE),
    age_q3 = quantile(나이, 0.75, na.rm = TRUE)
  )

cat("\n성별 및 연령 분포:\n")
print(demographic_table)

# 4. 질환 분포 분석 ------------------------------------------------

# 4.1 주요 진단명 집계
disease_list <- ct_data %>%
  select(disease_1, disease_2, disease_3) %>%
  pivot_longer(everything(), names_to = "order", values_to = "disease") %>%
  filter(!is.na(disease)) %>%
  count(disease, sort = TRUE)

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("주요 진단명 Top 20\n")
cat("=" , rep("=", 70), "\n", sep = "")
print(disease_list %>% head(20))

# 4.2 질환 카테고리 분류
categorize_disease <- function(disease) {
  if(is.na(disease)) return(NA)
  
  disease_lower <- tolower(disease)
  
  case_when(
    str_detect(disease_lower, "pneumonia|consolidation|infiltration") ~ "폐렴",
    str_detect(disease_lower, "abscess|empyema") ~ "농양",
    str_detect(disease_lower, "cystitis|pyelonephritis|uti") ~ "요로감염",
    str_detect(disease_lower, "cholecystitis|cholangitis") ~ "담도계감염",
    str_detect(disease_lower, "colitis|appendicitis|diverticulitis|enteritis") ~ "장관감염",
    str_detect(disease_lower, "pancreatitis") ~ "췌장염",
    str_detect(disease_lower, "osteomyelitis|spondylitis|discitis") ~ "근골격계감염",
    str_detect(disease_lower, "tuberculosis|tb") ~ "결핵",
    str_detect(disease_lower, "cellulitis|phlegmon") ~ "연조직감염",
    TRUE ~ "기타"
  )
}

# 질환 카테고리 적용
ct_data <- ct_data %>%
  mutate(
    disease_category = sapply(disease_1, categorize_disease)
  )

# 카테고리별 집계
category_summary <- ct_data %>%
  filter(fever_focus == 1) %>%
  count(disease_category, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100)

cat("\n질환 카테고리별 분포 (Fever Focus 양성 환자):\n")
print(category_summary)

# 5. 논문용 Table 1 생성 ------------------------------------------------

# 5.1 기본 특성 테이블
table1_data <- ct_data %>%
  mutate(
    fever_focus = factor(fever_focus, levels = c(0, 1), 
                         labels = c("Negative", "Positive")),
    성별 = factor(성별),
    age_group = case_when(
      나이 < 65 ~ "< 65",
      나이 >= 65 & 나이 < 75 ~ "65-74",
      나이 >= 75 & 나이 < 85 ~ "75-84",
      나이 >= 85 ~ "≥ 85",
      TRUE ~ NA_character_
    )
  ) %>%
  select(fever_focus, 나이, 성별, age_group)

# gtsummary를 이용한 Table 1 생성
table1 <- table1_data %>%
  tbl_summary(
    by = fever_focus,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      나이 ~ "Age (years)",
      성별 ~ "Sex",
      age_group ~ "Age group"
    ),
    missing_text = "Missing"
  ) %>%
  add_overall() %>%
  add_p() %>%
  bold_p() %>%
  modify_header(label ~ "**Characteristics**")

# Table 1 출력
cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("Table 1. Patient Characteristics by Fever Focus Status\n")
cat("=" , rep("=", 70), "\n", sep = "")
print(table1)

# 6. 시계열 분석 (월별 추이) ------------------------------------------------

ct_data <- ct_data %>%
  mutate(
    년월 = format(as.Date(as.character(내원일자), "%Y%m%d"), "%Y-%m")
  )

monthly_trend <- ct_data %>%
  group_by(년월) %>%
  summarise(
    총_환자수 = n(),
    양성_환자수 = sum(fever_focus == 1, na.rm = TRUE),
    양성률 = 양성_환자수 / 총_환자수 * 100
  )

cat("\n월별 Fever Focus 양성률 추이:\n")
print(monthly_trend)

# 7. 시각화 ------------------------------------------------

# 7.1 Fever Focus 분포 막대그래프
library(ggplot2)

p1 <- ct_data %>%
  count(fever_focus) %>%
  mutate(fever_focus = factor(fever_focus, 
                              levels = c(0, 1),
                              labels = c("Negative", "Positive"))) %>%
  ggplot(aes(x = fever_focus, y = n, fill = fever_focus)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_manual(values = c("Negative" = "#4CAF50", "Positive" = "#F44336")) +
  labs(title = "Distribution of Fever Focus",
       x = "Fever Focus Status",
       y = "Number of Patients") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("fever_focus_distribution.png", p1, width = 8, height = 6, dpi = 300)
p1

# 7.2 연령별 분포
p2 <- ct_data %>%
  mutate(fever_focus = factor(fever_focus)) %>%
  ggplot(aes(x = 나이, fill = fever_focus)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "dodge") +
  scale_fill_manual(values = c("0" = "#4CAF50", "1" = "#F44336"),
                    labels = c("Negative", "Positive")) +
  labs(title = "Age Distribution by Fever Focus Status",
       x = "Age (years)",
       y = "Number of Patients",
       fill = "Fever Focus") +
  theme_minimal()

ggsave("age_distribution_by_fever_focus.png", p2, width = 10, height = 6, dpi = 300)

# 8. 통계 검정 ------------------------------------------------

# 8.1 연령 차이 검정 (t-test)
age_test <- t.test(나이 ~ fever_focus, data = ct_data)
cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("연령 차이 검정 (Independent t-test)\n")
cat("=" , rep("=", 70), "\n", sep = "")
print(age_test)

# 8.2 성별 차이 검정 (Chi-square test)
gender_table <- table(ct_data$성별, ct_data$fever_focus)
gender_test <- chisq.test(gender_table)
cat("\n성별 차이 검정 (Chi-square test):\n")
print(gender_test)

# 9. 민감도 분석: 불확실한 케이스 ------------------------------------------------

# R/O (rule out) 케이스 분석
uncertain_cases <- ct_data %>%
  filter(str_detect(판독결과, "(?i)r/o|rule out|suspected|probable|possible"))

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("불확실한 진단 케이스 분석\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("불확실한 케이스 수:", nrow(uncertain_cases), "명\n")
cat("전체 중 비율:", round(nrow(uncertain_cases) / nrow(ct_data) * 100, 2), "%\n")

# 10. 결과 저장 ------------------------------------------------

# 분석 결과를 RData로 저장
save(ct_data, fever_summary, demographic_table, disease_list, 
     category_summary, monthly_trend, 
     file = "ct_fever_focus_analysis.RData")

# CSV 파일로도 저장 (논문 제출용)
write.csv(fever_summary, "fever_summary_statistics.csv", row.names = FALSE)
write.csv(disease_list, "disease_distribution.csv", row.names = FALSE)
write.csv(category_summary, "disease_categories.csv", row.names = FALSE)

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("분석 완료!\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("생성된 파일:\n")
cat("1. ct_fever_focus_analysis.RData - 전체 분석 결과\n")
cat("2. fever_summary_statistics.csv - 기본 통계\n")
cat("3. disease_distribution.csv - 질환 분포\n")
cat("4. disease_categories.csv - 질환 카테고리별 분포\n")
cat("5. fever_focus_distribution.png - 분포 그래프\n")
cat("6. age_distribution_by_fever_focus.png - 연령별 분포 그래프\n")

# 세션 정보 저장 (재현 가능성을 위해)
sink("session_info.txt")
sessionInfo()
sink()

cat("\n세션 정보가 session_info.txt에 저장되었습니다.\n")