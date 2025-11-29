#==============================================================================
# v3-Step2d 수정: Fever Lab 중복 검사 처리 개선 (수정본)
#==============================================================================
# 문제: Long 형식 원본에서 중복 확인 필요
# 해결: part1_fever_lab.rds(Long)에서 중복 분석 → Wide 변환
#==============================================================================

library(tidyverse)
library(janitor)
library(lubridate)

setwd("/Users/youjinlee/Library/Mobile Documents/com~apple~CloudDocs/My R/Fever c claude/2017_2025_s")

cat("\n=== 중복 검사 처리 개선 (수정) ===\n\n")

#==============================================================================
# STEP 1: Long 형식 원본 데이터 로드
#==============================================================================
cat("=== STEP 1: Long 형식 원본 데이터 로드 ===\n\n")

# ⭐ Long 형식 원본 (Part 1 결과)
fever_lab_long <- readRDS("cleaned_data/part1_fever_lab.rds")

cat(sprintf("✓ Long 형식 원본: %d rows, %d columns\n", 
            nrow(fever_lab_long), ncol(fever_lab_long)))
cat(sprintf("✓ 컬럼: %s\n\n", paste(names(fever_lab_long), collapse = ", ")))

# 데이터 구조 확인
cat("데이터 샘플 (첫 3행):\n")
fever_lab_long %>%
  select(patient_id, visit_date, detail_name, result) %>%
  head(3) %>%
  print()
cat("\n")


#==============================================================================
# STEP 2: 중복 패턴 탐색
#==============================================================================
cat("=== STEP 2: 중복 검사 패턴 분석 ===\n\n")

# 2.1 환자당 같은 날 같은 검사 중복 확인
duplicate_pattern <- fever_lab_long %>%
  group_by(patient_id, visit_date, detail_name) %>%
  summarize(
    n_tests = n(),
    values = paste(result, collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(n_tests > 1)  # 중복만 추출

cat(sprintf("중복 검사 발견: %d건\n", nrow(duplicate_pattern)))
cat(sprintf("전체 검사 중 비율: %.1f%%\n\n", 
            nrow(duplicate_pattern) / nrow(fever_lab_long) * 100))

# 중복이 많은 검사 Top 10
if(nrow(duplicate_pattern) > 0) {
  cat("중복이 많은 검사 Top 10:\n")
  duplicate_summary <- duplicate_pattern %>%
    count(detail_name, sort = TRUE) %>%
    head(10)
  print(duplicate_summary)
  
  # 중복 검사 값들 예시
  cat("\n중복 검사 값 예시 (처음 5건):\n")
  duplicate_pattern %>%
    select(patient_id, visit_date, detail_name, n_tests, values) %>%
    head(5) %>%
    print()
  
  # CSV 저장
  write_excel_csv(duplicate_pattern, 
                  "reports/Lab_duplicate_cases_FULL.csv")
  write_excel_csv(duplicate_summary, 
                  "reports/Lab_duplicate_summary.csv")
  
  cat("\n✓ 중복 케이스 저장:\n")
  cat("  - reports/Lab_duplicate_cases_FULL.csv\n")
  cat("  - reports/Lab_duplicate_summary.csv\n\n")
  
} else {
  cat("✓ 중복 검사 없음!\n\n")
}


#==============================================================================
# STEP 3: Wide 변환 - 두 가지 방법
#==============================================================================
cat("=== STEP 3: Wide 변환 - 중복 처리 방법 비교 ===\n\n")

# 공통 전처리
fever_lab_prep <- fever_lab_long %>%
  mutate(
    patient_id = as.character(patient_id),
    visit_date = suppressWarnings(ymd(as.character(visit_date))),
    result_numeric = suppressWarnings(as.numeric(result))
  ) %>%
  filter(
    !is.na(detail_name),
    detail_name != "",
    str_trim(detail_name) != ""
  )

cat(sprintf("전처리 완료: %d rows → Wide 변환 시작\n\n", nrow(fever_lab_prep)))


# 방법 1: 첫 번째 값 사용 (first) ✅ 권장
cat("[방법 1] 첫 번째 값 사용 (first)\n")

fever_lab_wide_first <- fever_lab_prep %>%
  group_by(patient_id, visit_date, detail_name) %>%
  summarize(
    result_numeric = first(result_numeric),  # 첫 번째 값
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = detail_name, 
    values_from = result_numeric
  ) %>%
  janitor::clean_names()

cat(sprintf("✓ Wide 변환 완료 (first): %d rows, %d columns\n", 
            nrow(fever_lab_wide_first), ncol(fever_lab_wide_first)))

# 결측치 확인
n_missing_first <- sum(is.na(fever_lab_wide_first))
cat(sprintf("  결측치: %d개\n\n", n_missing_first))


# 방법 2: 평균값 사용 (mean)
cat("[방법 2] 평균값 사용 (mean)\n")

fever_lab_wide_mean <- fever_lab_prep %>%
  group_by(patient_id, visit_date, detail_name) %>%
  summarize(
    result_numeric = mean(result_numeric, na.rm = TRUE),  # 평균값
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = detail_name, 
    values_from = result_numeric
  ) %>%
  janitor::clean_names()

cat(sprintf("✓ Wide 변환 완료 (mean): %d rows, %d columns\n", 
            nrow(fever_lab_wide_mean), ncol(fever_lab_wide_mean)))

# 결측치 확인
n_missing_mean <- sum(is.na(fever_lab_wide_mean))
cat(sprintf("  결측치: %d개\n\n", n_missing_mean))


#==============================================================================
# STEP 4: 두 방법 비교
#==============================================================================
cat("=== STEP 4: 두 방법 비교 (주요 검사) ===\n\n")

# 비교할 주요 검사 (원본 이름 그대로)
key_labs_original <- c("WBC", "CRP", "Hb", "Platelet", "Glucose", 
                       "Creatinine", "BUN", "Sodium", "Potassium",
                       "AST", "ALT", "Total_bilirubin")

# clean_names() 후 변환된 이름
key_labs_clean <- key_labs_original %>%
  tolower() %>%
  str_replace_all("_", "_")

# 실제 존재하는 변수만 선택
key_labs_available <- intersect(key_labs_clean, names(fever_lab_wide_first))

cat(sprintf("비교 가능한 주요 검사: %d개\n", length(key_labs_available)))
if(length(key_labs_available) > 0) {
  cat(sprintf("  %s\n\n", paste(key_labs_available, collapse = ", ")))
}

if(length(key_labs_available) > 0) {
  
  comparison <- tibble(
    Variable = key_labs_available,
    First_Mean = map_dbl(key_labs_available, 
                         ~mean(fever_lab_wide_first[[.x]], na.rm = TRUE)),
    First_SD = map_dbl(key_labs_available,
                       ~sd(fever_lab_wide_first[[.x]], na.rm = TRUE)),
    Mean_Mean = map_dbl(key_labs_available,
                        ~mean(fever_lab_wide_mean[[.x]], na.rm = TRUE)),
    Mean_SD = map_dbl(key_labs_available,
                      ~sd(fever_lab_wide_mean[[.x]], na.rm = TRUE))
  ) %>%
    mutate(
      Difference = abs(First_Mean - Mean_Mean),
      Difference_Pct = round(Difference / First_Mean * 100, 2)
    ) %>%
    arrange(desc(Difference_Pct))
  
  cat("주요 검사 변수 비교 (차이가 큰 순):\n")
  print(comparison, n = nrow(comparison))
  cat("\n")
  
  write_excel_csv(comparison, "reports/Lab_comparison_first_vs_mean.csv")
  cat("✓ 비교 결과 저장: reports/Lab_comparison_first_vs_mean.csv\n\n")
  
  # 차이가 큰 변수 확인
  large_diff <- comparison %>%
    filter(Difference_Pct > 5)
  
  if(nrow(large_diff) > 0) {
    cat("⚠️  차이가 5% 이상인 변수:\n")
    print(large_diff)
    cat("\n해석: 중복 검사 값들이 상당히 다름 → 원인 확인 필요\n\n")
  } else {
    cat("✓ 모든 주요 변수에서 두 방법의 차이 <5%\n")
    cat("  → 어느 방법을 사용해도 결과에 큰 영향 없음\n\n")
  }
}


#==============================================================================
# STEP 5: 최종 선택 및 저장
#==============================================================================
cat("=== STEP 5: 최종 방법 선택 및 저장 ===\n\n")

# ✅ 권장: 첫 번째 값 사용 (first)
fever_lab_final <- fever_lab_wide_first

cat("📌 선택된 방법: 첫 번째 값 (first)\n\n")

cat("선택 이유:\n")
cat("  1. 임상적 의미: 초기 상태를 반영 (가장 먼저 입력된 값)\n")
cat("  2. 보수적 접근: 시간순 가정 (먼저 입력 = 먼저 시행)\n")
cat("  3. 의료 연구 관행: 대부분의 응급의학 논문에서 사용\n")
if(exists("large_diff") && nrow(large_diff) == 0) {
  cat("  4. 실증적 근거: 두 방법 간 차이 미미\n")
}
cat("\n")

cat("💡 논문 Methods 기술 예시:\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("\"For laboratory tests performed multiple times on\n")
cat(" the same day, the first recorded value was used\n")
cat(" for analysis, assuming it represented the initial\n")
cat(" clinical state at presentation.\"\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

# 중복 제거 및 최종 저장
fever_lab_final <- fever_lab_final %>% 
  distinct()

saveRDS(fever_lab_final, "cleaned_data/part2_fever_lab_wide_FIXED.rds")
write_excel_csv(fever_lab_final, "cleaned_data/part2_fever_lab_wide_FIXED.csv")

cat(sprintf("✓ 최종 데이터 저장 완료\n"))
cat(sprintf("  행 수: %d, 열 수: %d\n", 
            nrow(fever_lab_final), ncol(fever_lab_final)))
cat("  파일:\n")
cat("  - cleaned_data/part2_fever_lab_wide_FIXED.rds\n")
cat("  - cleaned_data/part2_fever_lab_wide_FIXED.csv\n\n")


#==============================================================================
# STEP 6: 처리 결과 요약 리포트
#==============================================================================
cat("=== STEP 6: 처리 결과 요약 ===\n\n")

summary_report <- tibble(
  Metric = c(
    "원본 데이터 (Long)",
    "중복 검사 건수",
    "중복 비율 (%)",
    "전처리 후",
    "최종 데이터 (Wide)",
    "환자 수",
    "검사 항목 수"
  ),
  Value = c(
    format(nrow(fever_lab_long), big.mark = ","),
    format(nrow(duplicate_pattern), big.mark = ","),
    sprintf("%.2f%%", nrow(duplicate_pattern) / nrow(fever_lab_long) * 100),
    format(nrow(fever_lab_prep), big.mark = ","),
    format(nrow(fever_lab_final), big.mark = ","),
    format(n_distinct(fever_lab_final$patient_id), big.mark = ","),
    format(ncol(fever_lab_final) - 2, big.mark = ",")
  )
)

print(summary_report)
write_excel_csv(summary_report, "reports/Lab_duplicate_handling_summary.csv")

cat("\n✓ 요약 리포트 저장: reports/Lab_duplicate_handling_summary.csv\n\n")


#==============================================================================
# 완료 메시지
#==============================================================================
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║        중복 검사 처리 완료 (수정본)                        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("✅ 완료 항목:\n")
cat("  1. ✓ Long 형식 원본에서 중복 패턴 분석\n")
cat("  2. ✓ 두 가지 방법 비교 (first vs mean)\n")
cat("  3. ✓ 최종 방법 선택 및 근거 제시\n")
cat("  4. ✓ Wide 형식 변환 및 저장\n")
cat("  5. ✓ 상세 리포트 생성\n\n")

cat("📁 생성 파일:\n")
cat("  주요 데이터:\n")
cat("  • part2_fever_lab_wide_FIXED.rds - ⭐ 최종 Lab 데이터\n")
cat("  • part2_fever_lab_wide_FIXED.csv - CSV 버전\n\n")
cat("  분석 리포트:\n")
cat("  • Lab_duplicate_cases_FULL.csv - 중복 케이스 전체\n")
cat("  • Lab_duplicate_summary.csv - 중복 요약\n")
cat("  • Lab_comparison_first_vs_mean.csv - 방법 비교\n")
cat("  • Lab_duplicate_handling_summary.csv - 처리 요약\n\n")

cat("➡️  다음 단계:\n")
cat("  1. reports/ 폴더의 CSV 파일들을 열어 결과 확인\n")
cat("  2. 특히 Lab_comparison_first_vs_mean.csv에서\n")
cat("     Difference_Pct 컬럼 확인 (대부분 <5%면 정상)\n")
cat("  3. 이상 없으면 '중복 처리 완료' 답변\n")
cat("  4. v3-Step2e (Nurse/CT) 코드 계속 진행\n\n")

# 세션 정보
writeLines(capture.output(sessionInfo()), 
           "reports/Lab_duplicate_fix_session_info.txt")

cat("💡 Tip: 중복 검사가 많다면 (>10%):\n")
cat("   - 중복 케이스를 직접 확인 (Lab_duplicate_cases_FULL.csv)\n")
cat("   - 데이터 품질 문제 가능성 검토\n")
cat("   - 필요시 데이터 추출 과정 재확인\n\n")