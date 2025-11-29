#------------------------------------------------------------------------------
# 0. 환경 설정
#------------------------------------------------------------------------------
library(tidyverse)      # 데이터 조작
library(readr)          # CSV 파일 읽기 (한글 인코딩 지원)
library(writexl)        # Excel 파일 쓰기
library(janitor)        # 변수명 클리닝
library(lubridate)      # 날짜 처리
library(readxl)


setwd("/Users/youjinlee/Documents/My R/fever paper/2017_2025")

#. 데이터 불러오기
input_file <- "ER_LAB_RSLT.csv"  

fever_lab <- read_csv(
  input_file,
  locale = locale(encoding = "CP949"),
  show_col_types = FALSE
)

combine1 <- read_excel(
  "base_result_nurse_fever.xlsx"
)

combine2 <- read_excel(
  "base_result_nurse_fever(3.0).xlsx"
)

ct1 <- read_excel(
  "CT_result(1.0).xlsx"
)

ct2 <- read_excel(
  "CT_result(3.0).xlsx"
)



#------------------------------------------------------------------------------
# 1. 기본 구조 및 결측치 요약
#------------------------------------------------------------------------------
summarize_df <- function(df, name) {
  cat("\n=====", name, "=====\n")
  cat("행:", nrow(df), " / 열:", ncol(df), "\n")
  print(df %>% glimpse())
  
  cat("\n결측치 비율 (상위 10개):\n")
  miss_rate <- df %>% 
    summarise(across(everything(), ~mean(is.na(.))*100)) %>%
    pivot_longer(everything(), names_to="variable", values_to="missing_pct") %>%
    arrange(desc(missing_pct)) %>%
    slice(1:10)
  print(miss_rate)
}

summarize_df(fever_lab, "fever_lab")
summarize_df(combine1, "combine1")
summarize_df(combine2, "combine2")
summarize_df(ct1, "ct1")
summarize_df(ct2, "ct2")

#------------------------------------------------------------------------------
# 2. 샘플 데이터 생성 (1~2% 또는 최대 1000행)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 2. 샘플 데이터 생성 (1~2% 또는 최대 1000행)
#------------------------------------------------------------------------------
sample_df <- function(df, name) {
  set.seed(2025)
  n <- min(round(nrow(df) * 0.02), 1000)
  df_sample <- df %>% sample_n(n)
  write_excel_csv(df_sample, paste0(name, "_sample.csv"))
  cat(sprintf("✓ %s 샘플 저장 완료 (%d행)\n", name, n))
}

sample_df(fever_lab, "fever_lab")
sample_df(combine1, "combine1")
sample_df(combine2, "combine2")
sample_df(ct1, "ct1")
sample_df(ct2, "ct2")


#==============================================================================
# 재방문 환자 분석
# 목표: combine1, combine2 병합 후 2회 이상 내원 환자 분석
#==============================================================================

# 2. 컬럼명 통일 -------------------------------------------------------------
cat("=== STEP 2: 컬럼명 통일 ===\n")

# combine1의 "이름" → "환자명"으로 변경
if ("이름" %in% names(combine1)) {
  combine1 <- combine1 %>% rename(환자명 = 이름)
  cat("✓ combine1: '이름' → '환자명'\n")
}

# "전원온병원" → "전원온 병원" 통일
if ("전원온병원" %in% names(combine1)) {
  combine1 <- combine1 %>% rename(`전원온 병원` = 전원온병원)
  cat("✓ combine1: '전원온병원' → '전원온 병원'\n")
}

# "퇴원일" → "퇴원일자" 통일
if ("퇴원일" %in% names(combine1)) {
  combine1 <- combine1 %>% rename(퇴원일자 = 퇴원일)
  cat("✓ combine1: '퇴원일' → '퇴원일자'\n")
}

# combine2에 "노트" 컬럼 추가 (없을 경우)
if (!"노트" %in% names(combine2)) {
  combine2$노트 <- NA_character_
  cat("✓ combine2: '노트' 컬럼 추가\n")
}

# 3. 데이터 타입 통일 (병합 전 필수) -----------------------------------------


# 모든 컬럼을 문자형으로 변환 (타입 불일치 방지)
combine1 <- combine1 %>% mutate(across(everything(), as.character))
combine2 <- combine2 %>% mutate(across(everything(), as.character))


# 4. 두 파일 병합 ------------------------------------------------------------
cat("=== STEP 4: 데이터 병합 ===\n")

# 컬럼 순서 맞추기 (combine1 기준)
col_names <- names(combine1)
combine2 <- combine2[, col_names]

# 병합
combined <- bind_rows(combine1, combine2)


# 5. 재방문 환자 분석 --------------------------------------------------------


# 등록번호 + 내원일자 기준으로 방문 횟수 계산
visit_counts <- combined %>%
  # 등록번호, 내원일자가 모두 있는 행만
  filter(!is.na(등록번호), !is.na(내원일자), 
         등록번호 != "", 내원일자 != "") %>%
  # 환자별 내원일자별 그룹화
  group_by(등록번호, 내원일자) %>%
  summarise(n_visits_same_day = n(), .groups = "drop") %>%
  # 환자별 총 방문일 수 계산
  group_by(등록번호) %>%
  summarise(
    총_방문일수 = n(),
    방문일자_목록 = paste(내원일자, collapse = ", "),
    .groups = "drop"
  )

# 2회 이상 내원한 환자만 필터링
repeat_visitors <- visit_counts %>%
  filter(총_방문일수 >= 2) %>%
  arrange(desc(총_방문일수), 등록번호);repeat_visitors

cat(sprintf("✓ 전체 환자 수: %d명\n", n_distinct(combined$등록번호)))
cat(sprintf("✓ 2회 이상 내원 환자: %d명 (%.1f%%)\n", 
            nrow(repeat_visitors),
            nrow(repeat_visitors) / n_distinct(combined$등록번호) * 100))

# 6. 재방문 환자 상세 정보 테이블 --------------------------------------------


# 재방문 환자의 전체 방문 기록
repeat_visit_details <- combined %>%
  filter(등록번호 %in% repeat_visitors$등록번호) %>%
  select(등록번호, 환자명, 내원일자, 내원시간, 성별, 나이, 
         주증상1, 진단명, 진료결과) %>%
  arrange(등록번호, 내원일자, 내원시간)


# 7. 결과 테이블 요약 --------------------------------------------------------

# 방문 횟수별 환자 수
visit_freq_summary <- repeat_visitors %>%
  count(총_방문일수, name = "환자수") %>%
  mutate(비율 = round(환자수 / sum(환자수) * 100, 1)) %>%
  arrange(desc(총_방문일수))

print(visit_freq_summary, n = 20)

# 8. 결과 저장 ---------------------------------------------------------------

# CSV 저장 (한글 깨짐 방지)
write_excel_csv(repeat_visitors, "재방문환자_요약.csv")
write_excel_csv(repeat_visit_details, "재방문환자_상세기록.csv")
write_excel_csv(visit_freq_summary, "방문횟수_분포.csv")

# RDS 저장 (R 전용)
saveRDS(repeat_visitors, "재방문환자_요약.rds")
saveRDS(repeat_visit_details, "재방문환자_상세기록.rds")

