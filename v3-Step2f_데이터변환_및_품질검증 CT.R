# =============================================================================
# Part 2f/3: CT 데이터 변환 및 품질 검증
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: CT 데이터 로드 → 외부 분석용 내보내기 → 분석 결과 통합 → 품질 검증
# 참고: v3-Step2d (fever_lab.R) 스타일 적용
# 예상 소요: 5-7분
# =============================================================================

#------------------------------------------------------------------------------
# 0. 환경 설정
#------------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(lubridate)

# 작업 디렉토리 설정 (사용자 환경에 맞게 수정)
setwd("Users/youjinlee/Documents/My R/fever paper/2017_2025_s")



cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 2f/3: CT 데이터 변환 및 품질 검증                     \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

#------------------------------------------------------------------------------
# 공통 함수 정의: 변환 문제 식별
#------------------------------------------------------------------------------
identify_problems <- function(original_col, converted_col, col_name) {
  # 원본에 값이 있었는데 변환 후 NA가 된 경우 식별
  problems <- which(!is.na(original_col) & 
                      original_col != "" & 
                      is.na(converted_col))
  
  if(length(problems) > 0) {
    return(tibble(
      row_num = problems,
      variable = col_name,
      original_value = as.character(original_col[problems]),
      converted_value = as.character(converted_col[problems])
    ))
  } else {
    return(NULL)
  }
}

#==============================================================================
# PART A: 데이터 로드 및 외부 분석용 내보내기
#==============================================================================

cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  PART A: CT 요약 생성 및 외부 분석용 내보내기               \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

#------------------------------------------------------------------------------
# A1. Part 1 결과물 로드
#------------------------------------------------------------------------------
cat("=== STEP A1: Part 1 CT 데이터 로드 ===\n")

# 파일 존재 확인
if (!file.exists("cleaned_data/part1_ct.rds")) {
  stop("❌ v3-Step1을 먼저 실행해주세요! (part1_ct.rds 파일 없음)")
}

ct_raw <- readRDS("cleaned_data/part1_ct.rds")

cat(sprintf("✓ CT 원본 데이터 로드: %d행, %d열\n", nrow(ct_raw), ncol(ct_raw)))
cat(sprintf("✓ 환자 수: %d명\n\n", n_distinct(ct_raw$patient_id)))

# 데이터 구조 확인
cat("현재 CT 데이터 열 구조:\n")
for(col in names(ct_raw)) {
  cat(sprintf("  - %s (%s)\n", col, class(ct_raw[[col]])[1]))
}
cat("\n")

#------------------------------------------------------------------------------
# A2. 기본 타입 변환 (날짜, 시간)
#------------------------------------------------------------------------------
cat("=== STEP A2: 기본 타입 변환 ===\n")

ct_typed <- ct_raw %>%
  dplyr::mutate(
    patient_id = as.character(patient_id),
    exam_date = suppressWarnings(ymd(as.character(exam_date))),
    receive_time = suppressWarnings(as.integer(receive_time))
  )

# 변환 결과 확인
cat(sprintf("✓ exam_date 변환 성공: %d / %d (%.1f%%)\n",
            sum(!is.na(ct_typed$exam_date)),
            nrow(ct_typed),
            sum(!is.na(ct_typed$exam_date)) / nrow(ct_typed) * 100))
cat(sprintf("✓ receive_time 변환 성공: %d / %d (%.1f%%)\n\n",
            sum(!is.na(ct_typed$receive_time)),
            nrow(ct_typed),
            sum(!is.na(ct_typed$receive_time)) / nrow(ct_typed) * 100))

#------------------------------------------------------------------------------
# A3. CT 요약 생성 (환자별, 검사일별)
#------------------------------------------------------------------------------
cat("=== STEP A3: CT 요약 생성 ===\n")

ct_summary <- ct_typed %>%
  dplyr::group_by(patient_id, exam_date) %>%
  dplyr::summarize(
    n_ct_scans = dplyr::n(),
    ct_findings_combined = paste(ct_finding, collapse = " | "),
    .groups = "drop"
  ) 

cat(sprintf("✓ CT 요약 생성 완료: %d행 (환자-검사일 조합)\n", nrow(ct_summary)))
cat(sprintf("✓ 환자 수: %d명\n", n_distinct(ct_summary$patient_id)))



#------------------------------------------------------------------------------
# A4. 외부 분석용 템플릿 생성 및 내보내기
#------------------------------------------------------------------------------
cat("=== STEP A4: 외부 분석용 템플릿 생성 ===\n")

ct_for_analysis <- ct_summary %>%
  dplyr::mutate(
    # 분석할 열들 (빈 값으로 초기화)
    fever_focus = NA_integer_,     # 1 = positive, 0 = negative
    disease_1 = NA_character_,     # 주요 질환 1
    disease_2 = NA_character_,     # 주요 질환 2
    disease_3 = NA_character_      # 주요 질환 3
  ) %>%
  dplyr::select(
    patient_id, exam_date, n_ct_scans,
    ct_findings_combined, 
    fever_focus, disease_1, disease_2, disease_3
  )

# CSV 내보내기 (UTF-8 BOM for Excel 호환)
write_excel_csv(ct_for_analysis, "CT_for_external_analysis.csv")

cat("✅ 외부 분석용 파일 생성 완료!\n")
cat("   파일명: CT_for_external_analysis.csv\n")
cat(sprintf("   총 %d건의 CT 기록\n", nrow(ct_for_analysis)))
cat(sprintf("   위치: %s\n\n", getwd()))



# 중간 결과 저장 (PART B에서 사용)
saveRDS(ct_typed, "cleaned_data/part2f_ct_typed_temp.rds")
saveRDS(ct_summary, "cleaned_data/part2f_ct_summary_temp.rds")

cat("✓ 중간 결과 저장 완료 (part2f_ct_*_temp.rds)\n\n")

cat("═══════════════════════════════════════════════════════════════\n")
cat("  PART A 완료! 외부 분석 후 PART B를 실행하세요.              \n")
cat("═══════════════════════════════════════════════════════════════\n\n")

#==============================================================================
# PART B: 외부 분석 결과 통합 및 품질 검증
#==============================================================================
# ⚠️ 외부 분석 완료 후 이 섹션부터 실행하세요!
# 실행 방법: 이 줄부터 선택하여 실행 (Cmd+Enter)

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("  PART B: 외부 분석 결과 통합 및 품질 검증                   \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

#------------------------------------------------------------------------------
# B1. 외부 분석 결과 불러오기
#------------------------------------------------------------------------------
cat("=== STEP B1: 외부 분석 결과 로드 ===\n")

# 분석 결과 파일 확인
analysis_file <- "ct_fever_focus_analyzed.csv"

if (!file.exists(analysis_file)) {
  stop(sprintf("❌ '%s' 파일을 찾을 수 없습니다!\n", analysis_file),
       "   PART A의 가이드를 참고하여 외부 분석을 완료해주세요.")
}

# 중간 저장 데이터 로드
ct_typed <- readRDS("cleaned_data/part2f_ct_typed_temp.rds")
ct_summary_original <- readRDS("cleaned_data/part2f_ct_summary_temp.rds")

cat(sprintf("✓ 원본 CT 요약: %d행\n", nrow(ct_summary_original)))

# 외부 분석 결과 로드
ct_analysis_result <- read_csv(
  analysis_file,
  locale = locale(encoding = "UTF-8"),
  col_types = cols(
    patient_id = col_character(),
    exam_date = col_date(format = ""),
    n_ct_scans = col_integer(),
    ct_findings_combined = col_character(),
    fever_focus = col_character(),
    disease_1 = col_character(),
    disease_2 = col_character(),
    disease_3 = col_character()
  ),
  show_col_types = FALSE
)

cat(sprintf("✓ 외부 분석 결과 로드: %d행\n", nrow(ct_analysis_result)))

#------------------------------------------------------------------------------
# B2. 데이터 검증 및 병합
#------------------------------------------------------------------------------
cat("\n=== STEP B2: 데이터 검증 및 병합 ===\n")

# 행 개수 검증
if (nrow(ct_summary_original) != nrow(ct_analysis_result)) {
  warning(sprintf("⚠️  행 개수 불일치! 원본: %d, 분석결과: %d\n", 
                  nrow(ct_summary_original), nrow(ct_analysis_result)))
  cat("   → 행 번호 기반 병합을 시도합니다.\n\n")
}

# fever_focus 값 표준화 (문자 → 정수)
ct_analysis_clean <- ct_analysis_result %>%
  dplyr::mutate(
    row_num = row_number(),
    
    # fever_focus 표준화
    fever_focus = dplyr::case_when(
      fever_focus == "1" | fever_focus == 1 ~ 1L,
      fever_focus == "0" | fever_focus == 0 ~ 0L,
      is.na(fever_focus) ~ 0L,
      TRUE ~ 0L
    ),
    
    # disease 열 정리
    dplyr::across(
      starts_with("disease"),
      ~if_else(is.na(.) | str_trim(.) == "", "None", str_trim(.))
    )
  )

# 원본과 병합 (행 번호 기준)

ct_summary_merged <- ct_summary_original %>%
  dplyr::mutate(row_num = row_number()) %>%
  dplyr::left_join(
    ct_analysis_clean %>%
      dplyr::select(row_num, fever_focus, disease_1, disease_2, disease_3),
    by = "row_num"
  ) %>%
  dplyr::select(-row_num) %>%
  dplyr::mutate(
    fever_focus = if_else(is.na(fever_focus), 0L, fever_focus),
    dplyr::across(starts_with("disease"), ~if_else(is.na(.), "None", .))
  )


cat("✓ 데이터 병합 완료\n")







# fever_focus 검증
invalid_fever <- ct_summary_merged %>%
  dplyr::filter(fever_focus < 0 | fever_focus > 1)

if (nrow(invalid_fever) > 0) {
  stop(sprintf("❌ fever_focus 잘못된 값: %d건\n", nrow(invalid_fever)),
       "   fever_focus는 0 또는 1만 가능합니다.")
}

cat("✓ fever_focus 값 검증 완료 (0 또는 1만 존재)\n\n")

#------------------------------------------------------------------------------
# B3. 전체 변수 품질 검증
#------------------------------------------------------------------------------
cat("=== STEP B3: 전체 변수 품질 검증 ===\n")

# 공통 변수 추출
common_vars <- names(ct_summary_merged)

cat(sprintf("✓ 검사 대상 변수: %d개\n", length(common_vars)))
cat("검사 진행 중...\n")

# 품질 리포트 생성
quality_ct <- map_dfr(common_vars, function(var_name) {
  
  col_data <- as.character(ct_summary_merged[[var_name]])
  
  n_total <- length(col_data)
  n_missing <- sum(is.na(col_data))
  n_empty <- sum(col_data == "" | col_data == "None", na.rm = TRUE)
  n_valid <- n_total - n_missing
  
  tibble(
    Variable = var_name,
    Total = n_total,
    Valid = n_valid,
    Missing = n_missing,
    Empty_or_None = n_empty,
    Valid_Pct = round(n_valid / n_total * 100, 1),
    Status = dplyr::case_when(
      n_valid / n_total >= 0.95 ~ "✓ Excellent",
      n_valid / n_total >= 0.80 ~ "○ Good",
      n_valid / n_total >= 0.50 ~ "△ Fair",
      TRUE ~ "✗ Poor"
    )
  )
}) %>%
  dplyr::arrange(desc(Missing))

cat("\n=== CT 데이터 품질 리포트 ===\n")
print(quality_ct, n = nrow(quality_ct))



# 품질 요약
cat("\n=== 품질 요약 ===\n")
cat(sprintf("• Excellent (≥95%%): %d개 변수\n", 
            sum(quality_ct$Status == "✓ Excellent")))
cat(sprintf("• Good (80-94%%): %d개 변수\n", 
            sum(quality_ct$Status == "○ Good")))
cat(sprintf("• Fair (50-79%%): %d개 변수\n", 
            sum(quality_ct$Status == "△ Fair")))
cat(sprintf("• Poor (<50%%): %d개 변수\n", 
            sum(quality_ct$Status == "✗ Poor")))

# 품질 리포트 저장
write_excel_csv(quality_ct, "reports/02f_quality_ct.csv")
cat("\n✓ 품질 리포트 저장: reports/02f_quality_ct.csv\n")

#------------------------------------------------------------------------------
# B4. CT 분석 결과 요약
#------------------------------------------------------------------------------
cat("\n=== STEP B4: CT 분석 결과 요약 ===\n")

# fever_focus 요약
fever_summary <- ct_summary_merged %>%
  dplyr::count(fever_focus) %>%
  dplyr::mutate(
    Label = if_else(fever_focus == 1, "Positive", "Negative"),
    Percentage = round(n / sum(n) * 100, 1)
  )

cat("\n--- Fever Focus 분포 ---\n")
print(fever_summary)

# 질환별 빈도 (표준화)
disease_freq_raw <- ct_summary_merged %>%
  dplyr::mutate(row_id = row_number()) %>%
  dplyr::select(row_id, disease_1, disease_2, disease_3) %>%
  tidyr::pivot_longer(
    cols = starts_with("disease"),
    names_to = "order",
    values_to = "disease"
  ) %>%
  dplyr::filter(disease != "None", disease != "")

# 질환명 표준화
disease_standardized <- disease_freq_raw %>%
  dplyr::mutate(
    disease_std = dplyr::case_when(
      str_detect(tolower(disease), "pneumonia|폐렴") ~ "Pneumonia",
      str_detect(tolower(disease), "uti|urinary|pyelonephritis|신우신염") ~ "UTI/Pyelonephritis",
      str_detect(tolower(disease), "abscess|농양") ~ "Abscess",
      str_detect(tolower(disease), "cholecystitis|cholangitis|담낭염|담관염") ~ "Cholecystitis/Cholangitis",
      str_detect(tolower(disease), "colitis|enterocolitis|장염|대장염") ~ "Colitis/Enterocolitis",
      str_detect(tolower(disease), "ileus|obstruction|폐색") ~ "Ileus/Obstruction",
      str_detect(tolower(disease), "diverticulitis|게실염") ~ "Diverticulitis",
      str_detect(tolower(disease), "peritonitis|복막염") ~ "Peritonitis",
      str_detect(tolower(disease), "pancreatitis|췌장염") ~ "Pancreatitis",
      str_detect(tolower(disease), "cancer|malignancy|carcinoma|tumor|암") ~ "Cancer/Malignancy",
      str_detect(tolower(disease), "stroke|ich|hemorrhage|infarction|뇌경색|뇌출혈") ~ "Stroke/ICH",
      str_detect(tolower(disease), "fracture|골절") ~ "Fracture",
      str_detect(tolower(disease), "effusion|ascites|흉수|복수") ~ "Effusion/Ascites",
      str_detect(tolower(disease), "infection|감염|cellulitis") ~ "Other infection",
      TRUE ~ "Other"
    )
  )

# 표준화된 질환별 빈도
disease_freq <- disease_standardized %>%
  dplyr::distinct(row_id, disease_std) %>%
  dplyr::count(disease_std, sort = TRUE, name = "n_patients") %>%
  dplyr::mutate(
    percentage = round(n_patients / nrow(ct_summary_merged) * 100, 1)
  )

cat("\n--- 표준화된 질환별 빈도 (환자 기준) ---\n")
print(disease_freq, n = 15)

# 원본-표준 매핑
disease_mapping <- disease_standardized %>%
  dplyr::count(disease_std, disease, sort = TRUE) %>%
  dplyr::arrange(disease_std, desc(n))

# 분석 결과 저장
ct_analysis_summary <- tibble(
  Metric = c("Total CT scans", "Fever focus (+)", "Fever focus (+) %",
             "Disease identified", "Unique diseases"),
  Value = c(
    nrow(ct_summary_merged),
    sum(ct_summary_merged$fever_focus == 1),
    round(sum(ct_summary_merged$fever_focus == 1) / nrow(ct_summary_merged) * 100, 1),
    sum(ct_summary_merged$disease_1 != "None"),
    n_distinct(disease_standardized$disease_std)
  )
)

write_excel_csv(ct_analysis_summary, "reports/02f_ct_analysis_summary.csv")
write_excel_csv(disease_freq, "reports/02f_ct_disease_frequency.csv")
write_excel_csv(disease_mapping, "reports/02f_ct_disease_mapping.csv")

cat("\n✓ 분석 결과 저장 완료\n")

#------------------------------------------------------------------------------
# B5. 중복 제거 및 최종 저장
#------------------------------------------------------------------------------
cat("\n=== STEP B5: 중복 제거 및 최종 저장 ===\n")

# 중복 제거
ct_final <- ct_summary_merged %>%
  dplyr::distinct()

cat(sprintf("중복 제거: %d → %d (제거: %d)\n",
            nrow(ct_summary_merged),
            nrow(ct_final),
            nrow(ct_summary_merged) - nrow(ct_final)))

# 최종 저장
saveRDS(ct_final, "cleaned_data/part2_ct_summary_typed.rds")
write_excel_csv(ct_final, "cleaned_data/part2_ct_summary_typed.csv")



# 원본 typed 데이터도 저장 (향후 필요시 사용)
saveRDS(ct_typed, "cleaned_data/part2_ct_typed.rds")

cat("\n✓ 최종 데이터 저장:\n")
cat("   • cleaned_data/part2_ct_summary.rds (CT 요약 + 분석결과)\n")
cat("   • cleaned_data/part2_ct_summary.csv\n")
cat("   • cleaned_data/part2_ct_typed.rds (원본 typed)\n")

# 임시 파일 삭제
if (file.exists("cleaned_data/part2f_ct_typed_temp.rds")) {
  file.remove("cleaned_data/part2f_ct_typed_temp.rds")
}
if (file.exists("cleaned_data/part2f_ct_summary_temp.rds")) {
  file.remove("cleaned_data/part2f_ct_summary_temp.rds")
}
cat("✓ 임시 파일 삭제 완료\n\n")

#------------------------------------------------------------------------------
# B6. 완료 메시지
#------------------------------------------------------------------------------
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 2f 완료: CT 데이터 변환 및 품질 검증                  \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("✅ 완료된 작업:\n")
cat("   [PART A]\n")
cat("   1. ✓ Part 1 CT 데이터 로드\n")
cat("   2. ✓ 기본 타입 변환 (날짜, 시간)\n")
cat("   3. ✓ CT 요약 생성 (환자별, 검사일별)\n")
cat("   4. ✓ 외부 분석용 템플릿 내보내기\n\n")

cat("   [PART B]\n")
cat("   5. ✓ 외부 분석 결과 통합\n")
cat("   6. ✓ 데이터 검증 (fever_focus 0/1)\n")
cat("   7. ✓ 전체 변수 품질 검증\n")
cat("   8. ✓ CT 분석 결과 요약 (질환별 빈도)\n")
cat("   9. ✓ 중복 제거 및 최종 저장\n\n")

cat("📊 최종 데이터 요약:\n")
cat(sprintf("   • CT 검사 건수: %d건\n", nrow(ct_final)))
cat(sprintf("   • 환자 수: %d명\n", n_distinct(ct_final$patient_id)))
cat(sprintf("   • Fever focus (+): %d건 (%.1f%%)\n",
            sum(ct_final$fever_focus == 1),
            sum(ct_final$fever_focus == 1) / nrow(ct_final) * 100))
cat(sprintf("   • Disease identified: %d건\n",
            sum(ct_final$disease_1 != "None")))

cat("\n📁 생성된 파일:\n")
cat("   데이터:\n")
cat("   • cleaned_data/part2_ct_summary.rds (최종)\n")
cat("   • cleaned_data/part2_ct_summary.csv\n")
cat("   • cleaned_data/part2_ct_typed.rds\n\n")
cat("   리포트:\n")
cat("   • reports/02f_quality_ct.csv\n")
cat("   • reports/02f_ct_analysis_summary.csv\n")
cat("   • reports/02f_ct_disease_frequency.csv\n")
cat("   • reports/02f_ct_disease_mapping.csv\n\n")

cat("➡️  다음 단계: v3-Step3b (코호트 선정 및 최종 데이터셋)\n")
cat("   ※ part2_ct_summary.rds를 사용합니다.\n\n")

# 세션 정보 저장
writeLines(capture.output(sessionInfo()), "reports/02f_session_info.txt")

#==============================================================================
# 사용 가이드
#==============================================================================
# 
# [첫 번째 실행] - PART A만 실행
#   1. 전체 코드 실행 또는 PART A 섹션만 실행
#   2. CT_for_external_analysis.csv 파일이 생성됨
#   3. Excel/Numbers에서 열어 fever_focus, disease_1/2/3 입력
#   4. ct_summary_analysis_result.csv로 저장
#
# [두 번째 실행] - PART B만 실행
#   1. PART B 섹션부터 선택하여 실행 (Cmd+Shift+Enter)
#   2. 또는 전체 코드 다시 실행 (PART A는 빠르게 통과)
#   3. 최종 데이터가 cleaned_data/part2_ct_summary.rds에 저장됨
#
# [주의사항]
#   • 외부 분석 시 patient_id, exam_date 절대 수정 금지
#   • fever_focus는 반드시 0 또는 1만 입력
#   • 파일명을 정확히 ct_summary_analysis_result.csv로 저장
#
#==============================================================================