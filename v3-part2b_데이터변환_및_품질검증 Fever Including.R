# =============================================================================
# Part 2b/: Fever Including 타입 변환 및 품질 검증 
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: 복잡한 데이터 타입 변환 및 품질 검증
# 수정사항: 전체 변수 자동 검사 기능 추가
# v3-part1에서cleaned_data/part1_fever_including.rds 불러와.
# cleaned_data/part2a_fever_including_typed.rds
# 최종 결과물 : cleaned_data/part2a_fever_including_typed.rds
# =============================================================================

#------------------------------------------------------------------------------
# 0. 환경 설정
#------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)

setwd("/Users/youjinlee/Documents/My R/fever paper/2017_2025_s")

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 2a: Fever Including  타입 변환   \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

# Part 1 결과물 로드
cat("=== Part 1 결과물 로드 ===\n")
fever_including <- readRDS("cleaned_data/part1_fever_including.rds")


cat(sprintf("✓ Fever Including: %d rows\n\n", nrow(fever_including)))

#------------------------------------------------------------------------------
# 1. Fever Including 타입 변환
#------------------------------------------------------------------------------
cat("=== STEP 1: Fever Including 타입 변환 ===\n")

# 원본 백업
fever_including_original <- fever_including


# 타입 변환 (경고 억제)
cat("\n[1단계] 데이터 타입 변환\n")


fever_including_typed <- fever_including %>%
  dplyr::mutate(
    # 날짜
    visit_date = suppressWarnings(ymd(as.character(visit_date))),
    onset_date = suppressWarnings(ymd(as.character(onset_date))),
    actual_discharge_date = suppressWarnings(ymd(as.character(actual_discharge_date))),
    
    # 시간
    visit_time = suppressWarnings(as.integer(visit_time)),
    visit_hour = suppressWarnings(as.numeric(substr(sprintf("%04d", visit_time), 1, 2))),
    visit_minute = suppressWarnings(as.numeric(substr(sprintf("%04d", visit_time), 3, 4))),
    
    # 기본 정보
    age = suppressWarnings(as.numeric(age)),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
    
    # 활력징후
    systolic_bp = suppressWarnings(as.numeric(systolic_bp)),
    diastolic_bp = suppressWarnings(as.numeric(diastolic_bp)),
    pulse_rate = suppressWarnings(as.numeric(pulse_rate)),
    respiratory_rate = suppressWarnings(as.numeric(respiratory_rate)),
    temperature = suppressWarnings(as.numeric(temperature)),
    spo2 = suppressWarnings(as.numeric(spo2)),
    bst = suppressWarnings(as.numeric(bst)),
    weight = suppressWarnings(as.numeric(weight)),
    consciousness_level = suppressWarnings(as.integer(consciousness_level)),
    
    # 기타
    stay_duration = suppressWarnings(as.numeric(stay_duration)),
    stay_hours = round(stay_duration / 60, 1)
  )

cat("✓ 변환 완료\n")



#==============================================================================
# ⭐ 1: 전체 변수 자동 검사 (Fever Including)
#==============================================================================
cat("\n[2단계] 변환 실패 케이스 식별 (전체 변수 자동 검사)\n")

# 문제 식별 함수 정의 evernote 확인
identify_problems <- function(original_col, converted_col, col_name) {
  problems <- which(!is.na(original_col) & original_col != "" & is.na(converted_col))
  # problems은 벡터인데 변환실패한 항목 위치 알려줌
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

# 모든 변수명 자동 추출
all_var_names_fever <- names(fever_including_original)

cat(sprintf("✓ 검사 대상 변수: %d개\n", length(all_var_names_fever)))
cat("검사 진행 중...\n")

# 모든 변수에 대해 자동 검사 evernote:품질검증 시스템
   ## 앞에서 만든 identify_problems 함수 이용.
problem_list_fever <- lapply(all_var_names_fever, function(var_name) {
  
  # 양쪽 데이터프레임에 해당 변수가 존재하는지 확인
  if(var_name %in% names(fever_including_typed)) {
    identify_problems(
      original_col = fever_including_original[[var_name]],
      converted_col = fever_including_typed[[var_name]],
      col_name = var_name
    )
  } else {
    return(NULL)
  }
})

# problem_list_fever가 **“데이터프레임들의 리스트”**이고, bind_rows()가 
# 그 리스트 안의 모든 데이터프레임을 한 장짜리 데이터로 합쳐주는 역할

# 리스트에 이름 붙이기
names(problem_list_fever) <- all_var_names_fever

# 전체 문제 통합
all_problems_fever <- dplyr::bind_rows(problem_list_fever)

# 결과 요약 및 출력
if(nrow(all_problems_fever) > 0) {
  cat(sprintf("\n⚠️  전체 변환 실패: %d건 (%d개 변수)\n\n", 
              nrow(all_problems_fever), 
              length(unique(all_problems_fever$variable))))
  
  # 변수별 실패 건수 집계 (많은 순)
  problem_summary_fever <- all_problems_fever %>%
    dplyr::count(variable, name = "n_problems") %>%
    dplyr::arrange(desc(n_problems))
  
  cat("=== 변수별 실패 건수 Top 10 ===\n")
  print(problem_summary_fever %>% head(10), n = 10)
  
  
  # 실패가 많은 상위 5개 변수만 샘플 출력
  top_5_vars_fever <- problem_summary_fever$variable[1:min(5, nrow(problem_summary_fever))]
  
  cat("\n=== 실패가 많은 상위 5개 변수 샘플 ===\n")
  
  for(var in top_5_vars_fever) {
    var_problems <- all_problems_fever %>%
      dplyr::filter(variable == var) %>%
      head(3)
    
    var_total <- sum(problem_summary_fever$variable == var)
    cat(sprintf("\n• %s (총 %d건 실패)\n", var, var_total))
    print(var_problems)
  }
  
} else {
  cat("\n✅ 모든 변수 변환 성공! 실패 케이스 없음.\n")
}

#여기까지 결과를 보고 데이터 수정해서 가능한 경우 최대한 많은 변수를 전활 할 수 있게 하자.



# 파생 변수 생성
cat("\n[3단계] 파생 변수 생성\n")

fever_including_typed <- fever_including_typed %>%
  dplyr::mutate(
    consciousness_label = dplyr::case_when(
      consciousness_level == 1 | consciousness_level == "A" ~ "Alert",
      consciousness_level == 2 | consciousness_level == "V" ~ "Verbal",
      consciousness_level == 3 | consciousness_level == "P" ~ "Pain",
      consciousness_level == 4 | consciousness_level == "U" ~ "Unresponsive",
      TRUE ~ "Unknown"
    ),
    
    # 체온 범주
    fever_category = dplyr::case_when(
      is.na(temperature) ~ "Unknown",
      temperature < 36.0 ~ "Hypothermia",
      temperature < 37.5 ~ "Normal",
      temperature < 38.0 ~ "Low fever",
      temperature < 39.0 ~ "Moderate fever",
      TRUE ~ "High fever"
    ),
    has_fever = if_else(!is.na(temperature) & temperature >= 38.0, 1, 0),
    
    # 혈압 범주
    bp_category = dplyr::case_when(
      is.na(systolic_bp) | is.na(diastolic_bp) ~ "Unknown",
      systolic_bp < 90 | diastolic_bp < 60 ~ "Hypotension",
      systolic_bp < 120 & diastolic_bp < 80 ~ "Normal",
      systolic_bp < 140 & diastolic_bp < 90 ~ "Prehypertension",
      TRUE ~ "Hypertension"
    ),
    
    # 이상 징후
    tachycardia = if_else(!is.na(pulse_rate) & pulse_rate > 100, 1, 0),
    tachypnea = if_else(!is.na(respiratory_rate) & respiratory_rate > 20, 1, 0),
    hypoxemia = if_else(!is.na(spo2) & spo2 < 94, 1, 0)
  )

cat("✓ 파생 변수 생성 완료\n")

# 활력징후 요약
fever_vitals_summary <- fever_including_typed %>%
  dplyr::group_by(patient_id, visit_date) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(patient_id, visit_date, 
                systolic_bp, diastolic_bp, pulse_rate, respiratory_rate,
                temperature, spo2, bst, 
                fever_category, has_fever, bp_category, 
                tachycardia, tachypnea, hypoxemia,
                consciousness_level, consciousness_label,
                chief_complaint_1, diagnosis, stay_hours)

# 품질 리포트
cat("\n[4단계] 데이터 품질 리포트\n")
cat("• 전체 변수 변환 실패는 위에서 이미 검사 완료\n")
cat("• 이 리포트는 논문 보고용 핵심 변수만 상세 분석\n\n")


quality_report <- tibble(
  Variable = c("visit_date", "onset_date", "age", "temperature", 
               "systolic_bp", "pulse_rate", "spo2"),
  Total = nrow(fever_including_typed),
  Original_NonEmpty = c(
    sum(!is.na(fever_including_original$visit_date) & fever_including_original$visit_date != ""),
    sum(!is.na(fever_including_original$onset_date) & fever_including_original$onset_date != ""),
    sum(!is.na(fever_including_original$age) & fever_including_original$age != ""),
    sum(!is.na(fever_including_original$temperature) & fever_including_original$temperature != ""),
    sum(!is.na(fever_including_original$systolic_bp) & fever_including_original$systolic_bp != ""),
    sum(!is.na(fever_including_original$pulse_rate) & fever_including_original$pulse_rate != ""),
    sum(!is.na(fever_including_original$spo2) & fever_including_original$spo2 != "")
  ),
  Converted_Valid = c(
    sum(!is.na(fever_including_typed$visit_date)),
    sum(!is.na(fever_including_typed$onset_date)),
    sum(!is.na(fever_including_typed$age)),
    sum(!is.na(fever_including_typed$temperature)),
    sum(!is.na(fever_including_typed$systolic_bp)),
    sum(!is.na(fever_including_typed$pulse_rate)),
    sum(!is.na(fever_including_typed$spo2))
  )
) %>%
  dplyr::mutate(
    Conversion_Failures = Original_NonEmpty - Converted_Valid,
    Success_Rate = round(Converted_Valid / Original_NonEmpty * 100, 1),
    Overall_Valid_Pct = round(Converted_Valid / Total * 100, 1),
    Status = dplyr::case_when(
      Overall_Valid_Pct >= 95 ~ "✓ Excellent",
      Overall_Valid_Pct >= 80 ~ "○ Good", 
      TRUE ~ "△ Fair"
    )
  )

print(quality_report)

write_excel_csv(quality_report, "reports/02a_quality_fever_including.csv")
cat("\n✓ 품질 리포트 저장\n")



# 4.1 품질 리포트 (전체 변수 자동 생성)
#------------------------------------------------------------------------------
cat("\n[4단계] 데이터 품질 리포트 (전체 변수)\n")
cat("• 모든 변수에 대한 변환 품질 자동 분석\n")
cat("• Conversion_Failures 높은 순으로 정렬\n\n")

# 양쪽 데이터프레임에 공통으로 존재하는 변수만 선택
common_vars <- intersect(names(fever_including_original), 
                         names(fever_including_typed))

cat(sprintf("✓ 공통 변수 %d개 품질 검증 중...\n\n", length(common_vars)))

# 전체 변수에 대해 자동으로 품질 리포트 생성
quality_report <- map_dfr(common_vars, function(var_name) {
  
  # Original_NonEmpty: 원본에서 값이 있는 개수
  original_nonempty <- sum(
    !is.na(fever_including_original[[var_name]]) & 
      fever_including_original[[var_name]] != ""
  )
  
  # Converted_Valid: 변환 후 유효한 값 개수
  converted_valid <- sum(!is.na(fever_including_typed[[var_name]]))
  
  # 결과 tibble
  tibble(
    Variable = var_name,
    Total = nrow(fever_including_typed),
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
cat(sprintf("총 %d개 변수 분석 완료\n\n", nrow(quality_report)))

print(quality_report, n = 20)  # 상위 20개 출력

# 요약 통계
cat("\n=== 품질 요약 ===\n")
cat(sprintf("• Excellent (≥95%%): %d개 변수\n", 
            sum(quality_report$Status == "✓ Excellent")))
cat(sprintf("• Good (80-94%%): %d개 변수\n", 
            sum(quality_report$Status == "○ Good")))
cat(sprintf("• Fair (50-79%%): %d개 변수\n", 
            sum(quality_report$Status == "△ Fair")))
cat(sprintf("• Poor (<50%%): %d개 변수\n", 
            sum(quality_report$Status == "✗ Poor")))

# 변환 실패가 있는 변수만 추출
cat("\n=== 변환 실패가 있는 변수 (Top 10) ===\n")
quality_report_failures <- quality_report %>%
  dplyr::filter(Conversion_Failures > 0) %>%
  head(10)

if(nrow(quality_report_failures) > 0) {
  print(quality_report_failures, n = 10)
  cat(sprintf("\n⚠️  총 %d개 변수에서 변환 실패 발생\n", 
              sum(quality_report$Conversion_Failures > 0)))
} else {
  cat("✅ 모든 변수 변환 성공!\n")
}

# 저장
write_excel_csv(quality_report, "reports/02a_quality_fever_including_full.csv")
cat("\n✓ 전체 품질 리포트 저장: reports/02a_quality_fever_including_full.csv\n")




# 3. 중간 결과물 저장
#------------------------------------------------------------------------------
cat("=== STEP 3: 중간 저장 ===\n")

# 3.1 핵심 데이터셋
cat("\n[핵심 데이터셋]\n")
saveRDS(fever_including_typed, "cleaned_data/part2a_fever_including_typed.rds")
cat("✓ fever_including_typed (타입 변환 완료)\n")

saveRDS(fever_vitals_summary, "cleaned_data/part2a_fever_vitals_summary.rds")
cat("✓ fever_vitals_summary (활력징후 요약)\n")

#==============================================================================
# 다음 단계 v3-Step2b
#==============================================================================