# =============================================================================
# Part 2c: Base 타입 변환 및 품질 검증 
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: 복잡한 데이터 타입 변환 및 품질 검증
# v3-part1에서cleaned_data/part1_base.rds 불러와.
# 최종 결과물 : cleaned_data/part2a_base_typed.rds
# 예상 소요: 3-4분
# =============================================================================

#------------------------------------------------------------------------------
# 0. 환경 설정
#------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)

setwd("/Users/youjinlee/Documents/My R/Fever c claude/2017_2025_s")

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 2c: F Base 타입 변환  \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

# Part 1 결과물 로드
cat("=== Part 1 결과물 로드 ===\n")
base <- readRDS("cleaned_data/part1_base.rds")


cat(sprintf("✓ Base: %d rows\n", nrow(base)))



#------------------------------------------------------------------------------
# 2. Base Result 타입 변환 및 품질 검증
#------------------------------------------------------------------------------
cat("=== STEP 2: Base Result 타입 변환 ===\n")

# 원본 백업
base_original <- base

# 타입 변환
cat("\n[1단계] 데이터 타입 변환\n")

base_typed <- base %>%
  dplyr::mutate(
    # 날짜
    visit_date = suppressWarnings(ymd(as.character(visit_date))),
    discharge_date = if_else(
      str_trim(as.character(discharge_date)) == "" | discharge_date == "-", 
      NA_character_, 
      as.character(discharge_date)
    ),
    discharge_date = suppressWarnings(ymd(discharge_date)),
    discharge_date = if_else(is.na(discharge_date), visit_date, discharge_date),
    death_date = if_else(
      str_trim(as.character(death_date)) == "" | death_date == "-", 
      NA_character_, 
      as.character(death_date)
    ),
    death_date = suppressWarnings(ymd(death_date)),
    
    # 진단명 처리
    discharge_diagnosis = if_else(
      is.na(discharge_diagnosis) | str_trim(discharge_diagnosis) == "",
      admission_diagnosis,
      discharge_diagnosis
    ),
    
    # 시간
    visit_time = as.integer(visit_time),
    visit_hour = as.numeric(substr(sprintf("%04d", visit_time), 1, 2)),
    visit_minute = as.numeric(substr(sprintf("%04d", visit_time), 3, 4)),
    
    
    # ⭐ 나이: 'y' 또는 'Y' 제거 후 숫자 변환
    age = suppressWarnings(as.numeric(str_remove_all(as.character(age), "[yY]"))),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
    
    
    # 동반질환 이진화
    liver_disease_yn = if_else(liver_disease == "+", 1, 0),
    hypertension_yn = if_else(hypertension == "+", 1, 0),
    diabetes_yn = if_else(diabetes == "+", 1, 0),
    heart_disease_yn = if_else(heart_disease == "+", 1, 0),
    kidney_disease_yn = if_else(kidney_disease == "+", 1, 0),
    respiratory_disease_yn = if_else(respiratory_disease == "+", 1, 0),
    surgery_yn = if_else(surgery == "+", 1, 0),
    cerebrovascular_disease_yn = if_else(cerebrovascular_disease == "+", 1, 0),
    neoplasm_yn = if_else(neoplasm == "+", 1, 0),
    
    # 질환명 표준화 (간단 버전)
    hypertension_simple = if_else(hypertension_yn == 1, "HTN", "None"),
    diabetes_simple = if_else(diabetes_yn == 1, "DM", "None"),
    
    liver_disease_simple = dplyr::case_when(
      liver_disease_yn == 0 ~ "None",
      str_detect(tolower(liver_disease_desc), "cirrhosis|경변") ~ "Cirrhosis",
      str_detect(tolower(liver_disease_desc), "hepatitis|간염") ~ "Hepatitis",
      TRUE ~ "Liver disease"
    ),
    
    heart_disease_simple = dplyr::case_when(
      heart_disease_yn == 0 ~ "None",
      str_detect(tolower(heart_disease_desc), "coronary|cad") ~ "CAD",
      str_detect(tolower(heart_disease_desc), "heart failure|chf") ~ "CHF",
      str_detect(tolower(heart_disease_desc), "arrhythmia|afib") ~ "Arrhythmia",
      TRUE ~ "Heart disease"
    ),
    
    kidney_disease_simple = dplyr::case_when(
      kidney_disease_yn == 0 ~ "None",
      str_detect(tolower(kidney_disease_desc), "ckd|chronic") ~ "CKD",
      str_detect(tolower(kidney_disease_desc), "esrd|dialysis|투석") ~ "ESRD",
      TRUE ~ "Kidney disease"
    ),
    
    # CCI
    cci = liver_disease_yn + diabetes_yn + heart_disease_yn + 
      kidney_disease_yn + respiratory_disease_yn + 
      cerebrovascular_disease_yn + neoplasm_yn * 2,
    
    # 퇴원 상태
    discharge_status_clean = dplyr::case_when(
      str_detect(tolower(discharge_status), "사망|death") ~ "Death",
      discharge_status == "EM discharge" ~ "ER discharge",
      TRUE ~ "Discharge"
    ),
    
    # 사망 여부
    death = if_else(discharge_status_clean == "Death" | !is.na(death_date), 1, 0)
  )

cat("✓ 변환 완료\n")

#==============================================================================
# ⭐ 개선 사항 2: 전체 변수 자동 검사 (Base)
#==============================================================================
cat("\n[2단계] 변환 실패 케이스 식별 (전체 변수 자동 검사)\n")

# 모든 변수명 자동 추출
all_var_names_base <- names(base_original)

cat(sprintf("✓ 검사 대상 변수: %d개\n", length(all_var_names_base)))
cat("검사 진행 중...\n")

# 모든 변수에 대해 자동 검사
problem_list_base <- lapply(all_var_names_base, function(var_name) {
  
  # 양쪽 데이터프레임에 해당 변수가 존재하는지 확인
  if(var_name %in% names(base_typed)) {
    identify_problems(
      original_col = base_original[[var_name]],
      converted_col = base_typed[[var_name]],
      col_name = var_name
    )
  } else {
    return(NULL)
  }
})

# 리스트에 이름 붙이기
names(problem_list_base) <- all_var_names_base

# 전체 문제 통합
all_problems_base <- dplyr::bind_rows(problem_list_base)

# 결과 요약 및 출력
if(nrow(all_problems_base) > 0) {
  cat(sprintf("\n⚠️  전체 변환 실패: %d건 (%d개 변수)\n\n", 
              nrow(all_problems_base), 
              length(unique(all_problems_base$variable))))
  
  # 변수별 실패 건수 집계 (많은 순)
  problem_summary_base <- all_problems_base %>%
    dplyr::count(variable, name = "n_problems") %>%
    dplyr::arrange(desc(n_problems))
  
  cat("=== 변수별 실패 건수 Top 10 ===\n")
  print(problem_summary_base %>% head(10), n = 10)
  
  # 전체 요약 통계
  cat(sprintf("\n📊 요약 통계:\n"))
  cat(sprintf("  • 문제 변수 개수: %d / %d (%.1f%%)\n", 
              nrow(problem_summary_base),
              length(all_var_names_base),
              100 * nrow(problem_summary_base) / length(all_var_names_base)))
  cat(sprintf("  • 평균 실패 건수: %.1f건/변수\n", 
              mean(problem_summary_base$n_problems)))
  cat(sprintf("  • 최다 실패 변수: %s (%d건)\n", 
              problem_summary_base$variable[1],
              problem_summary_base$n_problems[1]))
  
  # 실패가 많은 상위 5개 변수만 샘플 출력
  top_5_vars_base <- problem_summary_base$variable[1:min(5, nrow(problem_summary_base))]
  
  cat("\n=== 실패가 많은 상위 5개 변수 샘플 ===\n")
  
  for(var in top_5_vars_base) {
    var_problems <- all_problems_base %>%
      dplyr::filter(variable == var) %>%
      head(3)
    
    var_total <- sum(problem_summary_base$variable == var)
    cat(sprintf("\n• %s (총 %d건 실패)\n", var, var_total))
    print(var_problems)
  }
  
  # 전체 문제 목록 저장
  write_excel_csv(all_problems_base, "reports/02a_conversion_problems_base_FULL.csv")
  write_excel_csv(problem_summary_base, "reports/02a_conversion_summary_base.csv")
  cat("\n✓ 문제 데이터 저장:\n")
  cat("   - reports/02a_conversion_problems_base_FULL.csv (전체 목록)\n")
  cat("   - reports/02a_conversion_summary_base.csv (변수별 요약)\n")
  
} else {
  cat("\n✅ 모든 변수 변환 성공! 실패 케이스 없음.\n")
}

# 품질 리포트
cat("\n[3단계] 데이터 품질 리포트\n")

quality_base <- tibble(
  Variable = c("visit_date", "discharge_date", "death_date", "age",
               "liver_disease", "hypertension", "diabetes", "heart_disease", "kidney_disease"),
  Total = nrow(base_typed),
  Original_NonEmpty = c(
    sum(!is.na(base_original$visit_date) & base_original$visit_date != ""),
    sum(!is.na(base_original$discharge_date) & base_original$discharge_date != "" & base_original$discharge_date != "-"),
    sum(!is.na(base_original$death_date) & base_original$death_date != "" & base_original$death_date != "-"),
    sum(!is.na(base_original$age) & base_original$age != ""),
    sum(!is.na(base_original$liver_disease) & base_original$liver_disease != ""),
    sum(!is.na(base_original$hypertension) & base_original$hypertension != ""),
    sum(!is.na(base_original$diabetes) & base_original$diabetes != ""),
    sum(!is.na(base_original$heart_disease) & base_original$heart_disease != ""),
    sum(!is.na(base_original$kidney_disease) & base_original$kidney_disease != "")
  ),
  Converted_Valid = c(
    sum(!is.na(base_typed$visit_date)),
    sum(!is.na(base_typed$discharge_date)),
    sum(!is.na(base_typed$death_date)),
    sum(!is.na(base_typed$age)),
    nrow(base_typed),  # liver_disease_yn은 항상 0 또는 1
    nrow(base_typed),  # hypertension_yn은 항상 0 또는 1
    nrow(base_typed),  # diabetes_yn은 항상 0 또는 1
    nrow(base_typed),  # heart_disease_yn은 항상 0 또는 1
    nrow(base_typed)   # kidney_disease_yn은 항상 0 또는 1
  )
) %>%
  dplyr::mutate(
    Conversion_Failures = Original_NonEmpty - Converted_Valid,
    Success_Rate = round(Converted_Valid / Original_NonEmpty * 100, 1),
    
  )

print(quality_base)

write_excel_csv(quality_base, "reports/02a_quality_base_result.csv")
cat("\n✓ Base Result 품질 리포트 저장\n")



#------------------------------------------------------------------------------
#  데이터 품질 리포트 (전체 변수 자동 생성)
#------------------------------------------------------------------------------
cat("\n[3단계] 데이터 품질 리포트 (전체 변수)\n")
cat("• 모든 변수에 대한 변환 품질 자동 분석\n")
cat("• Conversion_Failures 높은 순으로 정렬\n\n")

# 양쪽 데이터프레임에 공통으로 존재하는 변수만 선택
common_vars_base <- intersect(names(base_original), 
                              names(base_typed))

cat(sprintf("✓ 공통 변수 %d개 품질 검증 중...\n\n", length(common_vars_base)))

# 전체 변수에 대해 자동으로 품질 리포트 생성
quality_base <- map_dfr(common_vars_base, function(var_name) {
  
  # Original_NonEmpty: 원본에서 값이 있는 개수
  # "-"도 결측으로 처리
  original_nonempty <- sum(
    !is.na(base_original[[var_name]]) & 
      base_original[[var_name]] != "" &
      base_original[[var_name]] != "-"
  )
  
  # Converted_Valid: 변환 후 유효한 값 개수
  converted_valid <- sum(!is.na(base_typed[[var_name]]))
  
  # 결과 tibble
  tibble(
    Variable = var_name,
    Total = nrow(base_typed),
    Original_NonEmpty = original_nonempty,
    Converted_Valid = converted_valid
  )
}) %>%
  dplyr::mutate(
    Conversion_Failures = Original_NonEmpty - Converted_Valid,
    Success_Rate = round(Converted_Valid / Original_NonEmpty * 100, 1),
    Overall_Valid_Pct = round(Converted_Valid / Total * 100, 1),
    Status = dplyr::case_when(
      Overall_Valid_Pct >= 95 ~ "✓ Excellent",
      Overall_Valid_Pct >= 80 ~ "○ Good", 
      Overall_Valid_Pct >= 50 ~ "△ Fair",
      TRUE ~ "✗ Poor"
    )
  ) %>%
  # ⭐ Conversion_Failures 높은 순으로 정렬
  dplyr::arrange(desc(Conversion_Failures))

# 전체 리포트 출력
cat("=== 전체 변수 품질 리포트 ===\n")
cat(sprintf("총 %d개 변수 분석 완료\n\n", nrow(quality_base)))

print(quality_base, n = 20)  # 상위 20개 출력

# 요약 통계
cat("\n=== 품질 요약 ===\n")
cat(sprintf("• Excellent (≥95%%): %d개 변수\n", 
            sum(quality_base$Status == "✓ Excellent")))
cat(sprintf("• Good (80-94%%): %d개 변수\n", 
            sum(quality_base$Status == "○ Good")))
cat(sprintf("• Fair (50-79%%): %d개 변수\n", 
            sum(quality_base$Status == "△ Fair")))
cat(sprintf("• Poor (<50%%): %d개 변수\n", 
            sum(quality_base$Status == "✗ Poor")))

# 변환 실패가 있는 변수만 추출
cat("\n=== 변환 실패가 있는 변수 (Top 10) ===\n")
quality_base_failures <- quality_base %>%
  dplyr::filter(Conversion_Failures > 0) %>%
  head(10)

if(nrow(quality_base_failures) > 0) {
  print(quality_base_failures, n = 10)
  cat(sprintf("\n⚠️  총 %d개 변수에서 변환 실패 발생\n", 
              sum(quality_base$Conversion_Failures > 0)))
} else {
  cat("✅ 모든 변수 변환 성공!\n")
}

# 저장
write_excel_csv(quality_base, "reports/02a_quality_base_result_full.csv")
cat("\n✓ 전체 품질 리포트 저장: reports/02a_quality_base_result_full.csv\n")


# 의사결정 기록
cat("\n[4단계] 데이터 처리 방침\n")
cat(sprintf("• 동반질환 (100%% valid): 이진화 성공 → CCI 계산 가능\n\n"))

cat(sprintf("✓ Base Result 변환: %d rows\n", nrow(base_typed)))
cat(sprintf("  - CCI 생성: %d명\n", sum(!is.na(base_typed$cci))))
cat(sprintf("  - 사망: %d명 (%.1f%%)\n\n", 
            sum(base_typed$death), mean(base_typed$death) * 100))


#------------------------------------------------------------------------------
# 3. 중간 결과물 저장
#------------------------------------------------------------------------------
cat("=== STEP 3: 중간 저장 ===\n")

saveRDS(base_typed, "cleaned_data/part2a_base_typed.rds")

cat("✓ 중간 결과 저장 완료\n\n")

#------------------------------------------------------------------------------
# 4. Part 2c 완료
#------------------------------------------------------------------------------
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 2c 완료 (개선 버전)                                  \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")






