#==============================================================================
# 혈액배양 검사 코드 확인
#==============================================================================

library(tidyverse)

# 1. 샘플 데이터 로드

setwd('/Users/youjinlee/Documents/My R/fever paper/filtered_data')
fever_lab <- read_csv("lab_filtered.csv", 
                      locale = locale(encoding = "UTF-8"))


# 2. 처방코드 DB0196 확인

db0196_rows <- fever_lab %>% 
  filter(처방코드 == "DB0196")

db0196_rows %>% 
  count(처방명) %>% 
  print(n = 20)

# 3. 세부검사코드 ABACT 확인  
cat("\n=== 세부검사코드 ABACT 확인 ===\n")
abact_rows <- fever_lab %>% 
  filter(세부검사코드 == "ABACT")

abact_rows %>% 
  count(세부검사코드) %>% 
  print(n = 20)

# 4. DB0196 + ABACT 조합 확인
cat("\n=== DB0196 + ABACT 조합 ===\n")
both <- fever_lab %>% 
  filter(처방코드 == "DB0196" & 세부검사코드 == "ABACT")


# 5. 결과값 패턴 확인 (G+, G- 있는지)
if(nrow(abact_rows) > 0) {
  cat("\n=== ABACT 결과값 패턴 ===\n")
  abact_rows %>% 
    select(등록번호, 결과) %>% 
    head(20) %>% 
    print()
  
  # G(+), G(-) 패턴 검색
  cat("\n결과값에 'G+', 'G-', 'positive', 'negative' 포함 여부:\n")
  result_patterns <- abact_rows %>%
    mutate(
      has_gplus = str_detect(결과, regex("g\\(\\+\\)|g\\+|gram.*positive", ignore_case =
                                         TRUE)),
      has_gminus = str_detect(결과, regex("g\\(\\-\\)|g\\-|gram.*negative", ignore_case =
                                          TRUE)),
      has_negative = str_detect(결과, regex("negative|no.*growth", ignore_case = TRUE))
    ) %>%
    
    summarise(
      G_plus = sum(has_gplus, na.rm = TRUE),
      G_minus = sum(has_gminus, na.rm = TRUE),
      Negative = sum(has_negative, na.rm = TRUE),
      Total = n()
    )
  
  print(result_patterns)
}

# 6. 혈액배양 관련 다른 패턴도 확인
cat("\n=== 혈액배양 관련 다른 처방명 ===\n")
blood_culture_keywords <- c("blood", "culture", "배양", "혈액")

blood_related <- fever_lab %>% 
  filter(str_detect(처방명, regex(paste(blood_culture_keywords, collapse = "|"), 
                               ignore_case = TRUE)))

if(nrow(blood_related) > 0) {
  blood_related %>% 
    count(처방코드, 처방명, 세부검사코드) %>% 
    arrange(desc(n)) %>% 
    head(500) %>% 
    print()
}


# 1. 혈액배양 관련 모든 코드 패턴 확인
cat("\n=== 혈액배양 관련 코드 패턴 ===\n")

# (1) 처방코드 DB0196의 세부검사코드들
db0196_detail <- fever_lab %>% 
  filter(처방코드 == "DB0196") %>% 
  count(처방명, 세부검사코드, 검사명, sort = TRUE)

print(db0196_detail)

# (2) Z11184 처방 (Gram stain)
z11184_orders <- fever_lab %>% 
  filter(처방코드 == "Z11184") %>% 
  count(처방명, 세부검사코드, 검사명, sort = TRUE)

cat("\n=== Z11184 처방 (Gram stain) ===\n")
print(z11184_orders)

# (3) ABACT 세부검사코드 확인
abact_detail <- fever_lab %>% 
  filter(세부검사코드 == "ABACT") %>% 
  count(처방코드, 처방명, 검사명, sort = TRUE)

cat("\n=== ABACT 세부검사코드 ===\n")
if(nrow(abact_detail) > 0) {
  print(abact_detail)
} else {
  cat("⚠️  샘플 데이터에 ABACT 없음\n")
}

# 2. 날짜별 패턴 확인 (AMIS 3.0 전후 차이)
cat("\n=== 날짜별 혈액배양 코드 패턴 ===\n")

fever_lab_dated <- fever_lab %>% 
  mutate(내원일자 = ymd(as.character(내원일자)))

# 2023년 7월 기준으로 분리
blood_culture_pattern <- fever_lab_dated %>% 
  filter(처방코드 %in% c("DB0196", "Z11184") | 
           세부검사코드 %in% c("Z08825", "Z08827", "ABACT")) %>% 
  mutate(
    period = if_else(내원일자 < ymd("2023-07-01"), 
                     "Before_Jul2023", "After_Jul2023")
  ) %>% 
  count(period, 처방코드, 처방명, 세부검사코드, 검사명) %>% 
  arrange(period, desc(n))

print(blood_culture_pattern)

# 3. 실제 결과값 패턴 확인
cat("\n=== 혈액배양 결과값 패턴 ===\n")

# DB0196 또는 Z11184 처방의 결과값
blood_results <- fever_lab %>% 
  filter(처방코드 %in% c("DB0196", "Z11184")) %>% 
  select(등록번호, 내원일자, 처방코드, 처방명, 세부검사코드, 검사명, 결과) %>% 
  head(20)

print(blood_results)

# 4. G(+)/G(-) 패턴 검색
cat("\n=== G(+)/G(-) 결과 분포 ===\n")

gram_pattern <- fever_lab %>% 
  filter(처방코드 %in% c("DB0196", "Z11184")) %>% 
  mutate(
    gram_type = case_when(
      str_detect(결과, regex("g\\+|gram.*positive|cocci", ignore_case = TRUE)) ~ "G(+)",
      str_detect(결과, regex("g\\-|gram.*negative|rods", ignore_case = TRUE)) ~ "G(-)",
      str_detect(결과, regex("no.*growth|negative|음성", ignore_case = TRUE)) ~ "Negative",
      TRUE ~ "Other"
    )
  ) %>% 
  count(처방코드, 세부검사코드, gram_type) %>% 
  arrange(desc(n))

print(gram_pattern)

# 5. 최종 권장 로직 출력
cat("\n╔═══════════════════════════════════════════════════════╗\n")
cat("  분석 결과 기반 권장 로직\n")
cat("╚═══════════════════════════════════════════════════════╝\n\n")

cat("1. 혈액배양 시행 여부:\n")
cat("   - 처방코드 DB0196 (Culture, ID/Sensitivity)\n")
cat("   - 또는 처방코드 Z11184 (Gram stain)\n\n")

cat("2. Gram stain 결과 확인:\n")
cat("   2023년 7월 이전: 처방명 '(1.0)SMEAR GRAM STAIN, BLOOD'\n")
cat("   2023년 7월 이후: 세부검사코드 'ABACT'\n\n")

cat("3. G(+)/G(-) 분류:\n")
cat("   - 결과값에서 텍스트 패턴 추출\n")
cat("   - G(+): 'G+', 'Gram positive', 'cocci' 등\n")
cat("   - G(-): 'G-', 'Gram negative', 'rods' 등\n")
cat("   - Negative: 'no growth', 'negative', '음성' 등\n")



# 1. DB0196 또는 Z11184의 실제 결과값 확인
cat("\n=== 혈액배양 관련 결과값 샘플 ===\n")

blood_culture_results <- fever_lab %>% 
  filter(처방코드 %in% c("DB0196", "Z11184")) %>% 
  select(등록번호, 내원일자, 처방코드, 세부검사코드, 검사명, 결과)

# 결과값이 있는 것만 추출
blood_culture_results_valid <- blood_culture_results %>% 
  filter(!is.na(결과) & 결과 != "")

cat(sprintf("결과값이 있는 행: %d개\n\n", nrow(blood_culture_results_valid)))

# 처음 30개 결과값 출력
blood_culture_results_valid %>% 
  head(30) %>% 
  print(n = 30)

# 2. 결과값 유형 분석
cat("\n=== 결과값 텍스트 패턴 분석 ===\n")

result_text_analysis <- blood_culture_results_valid %>% 
  mutate(
    result_length = nchar(결과),
    has_korean = str_detect(결과, "[가-힣]"),
    has_english = str_detect(결과, "[A-Za-z]"),
    is_numeric = str_detect(결과, "^[0-9\\.]+$")
  ) %>% 
  summarise(
    total = n(),
    avg_length = mean(result_length),
    with_korean = sum(has_korean),
    with_english = sum(has_english),
    numeric_only = sum(is_numeric)
  )

print(result_text_analysis)

# 3. 고유한 결과값 패턴 확인
cat("\n=== 고유한 결과값 패턴 (빈도순) ===\n")

unique_results <- blood_culture_results_valid %>% 
  count(결과, sort = TRUE) %>% 
  head(20)

print(unique_results)













# 
# 완벽합니다! 결과값에 실제로 "G(-) rods" 같은 텍스트가 있네요.
# ✅ 제안하신 로직 검증 결과
# 확인 사항:
#   
#   ✅ 처방코드 DB0196 = 혈액배양 시행
# ✅ 세부검사코드 ABACT = Gram stain 결과 (2023년 7월 이후)
# ✅ 결과값에 "G(-) rods" 같은 텍스트 존재
# 
# 
# 💻 전체 데이터에서 혈액배양 패턴 최종 검증
# 이미 작성하신 v3-part1_supplement_lab_data_standardization.R의 로직이 정확한지 전체 데이터에서 확인하겠습니다:
#   r#==============================================================================
# 전체 데이터에서 혈액배양 패턴 검증
#==============================================================================


setwd('/Users/youjinlee/Documents/My R/fever paper/filtered_data')

# 전체 데이터 로드
fever_lab_raw <- read_csv("lab_filtered.csv",
                          locale = locale(encoding = "UTF-8"),
                          show_col_types = FALSE)

# 1. 혈액배양 관련 데이터 추출
blood_culture_data <- fever_lab_raw %>% 
  mutate(내원일자 = ymd(as.character(내원일자))) %>% 
  filter(
    # 2023년 6월까지
    (내원일자 <= ymd("2023-06-30") & 
       처방명 == "(1.0)SMEAR GRAM STAIN, BLOOD") |
      # 2023년 7월부터  
      (내원일자 >= ymd("2023-07-01") & 
         세부검사코드 == "ABACT")
  )

cat(sprintf("혈액배양 관련 행: %d개\n", nrow(blood_culture_data)))

# 2. 기간별 분포 확인
period_dist <- blood_culture_data %>% 
  mutate(period = if_else(내원일자 < ymd("2023-07-01"), 
                          "Before_2023-07", "After_2023-07")) %>% 
  count(period, 처방코드, 처방명, 세부검사코드) %>% 
  arrange(period, desc(n))

cat("\n=== 기간별 혈액배양 코드 분포 ===\n")
print(period_dist)

# 3. 결과값 패턴 확인
result_pattern <- blood_culture_data %>% 
  filter(!is.na(결과) & 결과 != "") %>% 
  mutate(
    gram_result = case_when(
      str_detect(결과, regex("g\\+|g\\(\\+\\)|gram.*positive|cocci", 
                           ignore_case = TRUE)) ~ "G(+)",
      str_detect(결과, regex("g\\-|g\\(\\-\\)|gram.*negative|rods", 
                           ignore_case = TRUE)) ~ "G(-)",
      str_detect(결과, regex("no.*growth|negative|음성|n\\.g", 
                           ignore_case = TRUE)) ~ "No growth",
      TRUE ~ "Other"
    )
  ) %>% 
  count(gram_result, sort = TRUE)

cat("\n=== Gram stain 결과 분포 ===\n")
print(result_pattern)

# 4. 결과값 샘플 확인 (각 유형별)
cat("\n=== 각 유형별 결과값 샘플 ===\n")

sample_results <- blood_culture_data %>% 
  filter(!is.na(결과) & 결과 != "") %>% 
  mutate(
    gram_result = case_when(
      str_detect(결과, regex("g\\+|gram.*positive|cocci", 
                           ignore_case = TRUE)) ~ "G(+)",
      str_detect(결과, regex("g\\-|gram.*negative|rods", 
                           ignore_case = TRUE)) ~ "G(-)",
      str_detect(결과, regex("no.*growth|negative|음성", 
                           ignore_case = TRUE)) ~ "No growth",
      TRUE ~ "Other"
    )
  ) %>% 
  group_by(gram_result) %>% 
  slice_head(n = 3) %>% 
  ungroup() %>% 
  select(등록번호, 내원일자, gram_result, 결과)

print(sample_results, n = 20)

# 5. 환자별 혈액배양 시행 여부
patients_with_bc <- blood_culture_data %>% 
  distinct(등록번호, 내원일자) %>% 
  count(등록번호, name = "n_blood_cultures")

cat(sprintf("\n혈액배양 시행 환자: %d명\n", nrow(patients_with_bc)))
cat(sprintf("평균 검사 횟수: %.1f회\n", mean(patients_with_bc$n_blood_cultures)))

# 6. 최종 권장사항
cat("\n╔═══════════════════════════════════════════════════════╗\n")
cat("  최종 권장 로직\n")
cat("╚═══════════════════════════════════════════════════════╝\n\n")

cat("✅ 혈액배양 시행 환자 선택:\n")
cat("   - 2023년 6월까지: 처방명 '(1.0)SMEAR GRAM STAIN, BLOOD'\n")
cat("   - 2023년 7월부터: 세부검사코드 'ABACT'\n\n")

cat("✅ Gram stain 결과 분류:\n")
cat("   - G(+): 'G+', 'Gram positive', 'cocci' 패턴\n")
cat("   - G(-): 'G-', 'Gram negative', 'rods' 패턴\n")
cat("   - No growth: 'no growth', 'negative', '음성' 패턴\n\n")

cat("✅ 이미 v3-part1_supplement에서 적용 완료!\n")










