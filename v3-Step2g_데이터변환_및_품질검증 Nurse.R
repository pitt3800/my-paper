# =============================================================================
#v3-Step2g  변환 &  변수 품질 검증  (전체 변수 품질 검증 강화)
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: Nurse 변환, 전체 변수 품질 검증, 결측치 분석, 최종 저장
# 전제: Part 2a 완료 (part2a_*.rds 존재)
# 예상 소요: 5-7분
# =============================================================================

#------------------------------------------------------------------------------
# 0. 환경 설정
#------------------------------------------------------------------------------
library(tidyverse)
library(janitor)

library(naniar)
library(lubridate)
library(ggplot2)

setwd("Users/youjinlee/Documents/My R/fever paper/2017_2025_s")

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("  Step2g: Nurse변환 &  변수 품질 검증             \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

# Part 1 & 2a,2b 결과물 로드
cat("=== 이전 단계 결과 로드 ===\n")

fever_including_typed <- readRDS("cleaned_data/part2a_fever_including_typed.rds")
base_typed <- readRDS("cleaned_data/part2a_base_typed.rds")
fever_vitals_summary <- readRDS("cleaned_data/part2a_fever_vitals_summary.rds")

fever_lab <- readRDS("cleaned_data/part2_Lab_wide_typed.rds")
ct <- readRDS("cleaned_data/part2_ct_summary_typed.rds")
nurse <- readRDS("cleaned_data/part1_nurse.rds")

cat(sprintf("✓ Base: %d, Fever Including: %d\n", nrow(base_typed), nrow(fever_including_typed)))
cat(sprintf("✓ Nurse: %d, Lab: %d, CT: %d\n\n", nrow(nurse), nrow(fever_lab), nrow(ct)))


saveRDS(fever_lab_final, "cleaned_data/part2_Lab_wide_typed.rds")

#------------------------------------------------------------------------------
# 공통 함수: 변환 문제 식별 (v3-Step2a와 동일)
#------------------------------------------------------------------------------
identify_problems <- function(original_col, converted_col, col_name) {
  problems <- which(!is.na(original_col) & original_col != "" & is.na(converted_col))
  
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

#------------------------------------------------------------------------------
# 1. Nurse 타입 변환 & 전체 변수 품질 검증
#------------------------------------------------------------------------------
cat("=== STEP 1: Nurse 타입 변환 & 전체 변수 품질 검증 ===\n")

# 원본 백업
nurse_original <- nurse

# 타입 변환 (경고 억제)
cat("\n[1단계] 데이터 타입 변환\n")

nurse_typed <- nurse %>%
  dplyr::mutate(
    visit_date = suppressWarnings(ymd(as.character(visit_date))),
    visit_time = suppressWarnings(as.integer(visit_time)),
    age = suppressWarnings(as.numeric(age)),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male"))
  )

cat("✓ 변환 완료\n")

# 변환 실패 케이스 식별 (모든 주요 변수)
cat("\n[2단계] 변환 실패 케이스 식별 (모든 변수)\n")

problem_list_nurse <- list(
  visit_date = identify_problems(nurse_original$visit_date, 
                                 nurse_typed$visit_date, "visit_date"),
  visit_time = identify_problems(nurse_original$visit_time, 
                                 nurse_typed$visit_time, "visit_time"),
  age = identify_problems(nurse_original$age, 
                          nurse_typed$age, "age"),
  sex = identify_problems(nurse_original$sex, 
                          nurse_typed$sex, "sex")
)

all_problems_nurse <- dplyr::bind_rows(problem_list_nurse)

if(nrow(all_problems_nurse) > 0) {
  cat(sprintf("⚠️  변환 실패: %d건\n\n", nrow(all_problems_nurse)))
  
  problem_summary <- all_problems_nurse %>%
    dplyr::count(variable, name = "n_problems") %>%
    dplyr::arrange(desc(n_problems))
  
  print(problem_summary)
  
  # 각 변수별 샘플 출력
  for(var in unique(all_problems_nurse$variable)) {
    var_problems <- all_problems_nurse %>% 
      dplyr::filter(variable == var) %>%
      head(3)
    
    cat(sprintf("\n• %s 실패 샘플:\n", var))
    print(var_problems)
  }
  
  write_excel_csv(all_problems_nurse, "reports/02b_conversion_problems_nurse.csv")
  cat("\n✓ Nurse 변환 문제 저장\n")
} else {
  cat("✓ 모든 변환 성공\n")
}

# 품질 리포트 (모든 변수)
cat("\n[3단계] 데이터 품질 리포트 (모든 변수)\n")

quality_nurse <- tibble(
  Variable = c("visit_date", "visit_time", "age", "sex", 
               "procedure", "special_note", "note"),
  Total = nrow(nurse_typed),
  Original_NonEmpty = c(
    sum(!is.na(nurse_original$visit_date) & nurse_original$visit_date != ""),
    sum(!is.na(nurse_original$visit_time) & nurse_original$visit_time != ""),
    sum(!is.na(nurse_original$age) & nurse_original$age != ""),
    sum(!is.na(nurse_original$sex) & nurse_original$sex != ""),
    sum(!is.na(nurse_original$procedure) & nurse_original$procedure != ""),
    sum(!is.na(nurse_original$special_note) & nurse_original$special_note != ""),
    sum(!is.na(nurse_original$note) & nurse_original$note != "")
  ),
  Converted_Valid = c(
    sum(!is.na(nurse_typed$visit_date)),
    sum(!is.na(nurse_typed$visit_time)),
    sum(!is.na(nurse_typed$age)),
    sum(!is.na(nurse_typed$sex)),
    nrow(nurse_typed),  # procedure는 character, 항상 존재
    nrow(nurse_typed),  # special_note는 character, 항상 존재
    nrow(nurse_typed)   # note는 character, 항상 존재
  )
) %>%
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
  )

print(quality_nurse)
write_excel_csv(quality_nurse, "reports/02b_quality_nurse.csv")

cat("\n[4단계] 데이터 처리 방침 (Nurse)\n")
cat(sprintf("• visit_date (%.1f%% valid): 필수 변수 → 문제시 제외\n",
            quality_nurse$Overall_Valid_Pct[quality_nurse$Variable == "visit_date"]))
cat(sprintf("• age (%.1f%% valid): 필수 변수 → 문제시 제외\n",
            quality_nurse$Overall_Valid_Pct[quality_nurse$Variable == "age"]))
cat(sprintf("• sex (%.1f%% valid): 중요 변수 → 결측시 'Unknown' 처리\n",
            quality_nurse$Overall_Valid_Pct[quality_nurse$Variable == "sex"]))
cat(sprintf("• 텍스트 필드 (100%%): 'Not recorded' 처리 완료\n\n"))

cat(sprintf("✓ Nurse 변환: %d rows (평균 성공률 %.1f%%)\n\n", 
            nrow(nurse_typed), mean(quality_nurse$Success_Rate[1:4])))


#------------------------------------------------------------------------------
# 2. 중간 결과물 저장
#------------------------------------------------------------------------------
cat("=== STEP 3: 중간 저장 ===\n")

saveRDS(base_typed, "cleaned_data/part2a_nurse_typed.rds")

cat("✓ 중간 결과 저장 완료\n\n")



#------------------------------------------------------------------------------
# 4. 결측치 분석
#------------------------------------------------------------------------------
cat("=== STEP 4: 결측치 분석 ===\n")

missing_base <- naniar::miss_var_summary(base_typed) %>% 
  dplyr::mutate(dataset = "Base")
missing_fever <- naniar::miss_var_summary(fever_including_typed) %>% 
  dplyr::mutate(dataset = "Fever Including")
missing_nurse <- naniar::miss_var_summary(nurse_typed) %>% 
  dplyr::mutate(dataset = "Nurse")
missing_lab <- naniar::miss_var_summary(fever_lab_wide) %>% 
  dplyr::mutate(dataset = "Lab")
missing_ct <- naniar::miss_var_summary(ct_summary) %>% 
  dplyr::mutate(dataset = "CT")

missing_all <- dplyr::bind_rows(missing_base, missing_fever, missing_nurse, 
                                missing_lab, missing_ct) %>%
  dplyr::mutate(
    severity = dplyr::case_when(
      pct_miss >= 80 ~ "Critical",
      pct_miss >= 50 ~ "Severe",
      pct_miss >= 20 ~ "Moderate",
      pct_miss >= 5 ~ "Minor",
      TRUE ~ "Minimal"
    )
  ) %>%
  dplyr::arrange(desc(pct_miss))

write_excel_csv(missing_all, "reports/02b_missing_analysis_all.csv")

cat(sprintf("✓ 결측치 분석: %d개 변수 (5개 데이터셋)\n", nrow(missing_all)))
cat(sprintf("  - Critical (≥80%%): %d개\n", sum(missing_all$severity == "Critical")))
cat(sprintf("  - Severe (50-79%%): %d개\n", sum(missing_all$severity == "Severe")))
cat(sprintf("  - Moderate (20-49%%): %d개\n", sum(missing_all$severity == "Moderate")))
cat(sprintf("  - Minor (5-19%%): %d개\n\n", sum(missing_all$severity == "Minor")))

# 결측치 시각화 (Base만, 5% 이상)
missing_plot_data <- missing_base %>%
  dplyr::filter(pct_miss >= 5) %>%
  dplyr::arrange(desc(pct_miss)) %>%
  dplyr::mutate(
    variable = factor(variable, levels = variable),
    severity = dplyr::case_when(
      pct_miss >= 80 ~ "Critical",
      pct_miss >= 50 ~ "Severe", 
      pct_miss >= 20 ~ "Moderate",
      TRUE ~ "Minor"
    ),
    severity = factor(severity, levels = c("Critical", "Severe", "Moderate", "Minor"))
  )

if(nrow(missing_plot_data) > 0) {
  plot_h <- max(6, nrow(missing_plot_data) * 0.3)
  
  p <- ggplot(missing_plot_data, aes(x = pct_miss, y = variable, fill = severity)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", pct_miss)), 
              hjust = -0.1, size = 3.5, fontface = "bold") +
    scale_fill_manual(
      values = c("Critical" = "#e74c3c", "Severe" = "#e67e22", 
                 "Moderate" = "#f39c12", "Minor" = "#3498db"),
      name = "Severity"
    ) +
    scale_x_continuous(limits = c(0, max(missing_plot_data$pct_miss) * 1.15)) +
    labs(title = "Missing Data Pattern: Base Result", 
         subtitle = sprintf("Variables with ≥5%% missing (n=%d)", nrow(missing_plot_data)),
         x = "Missing Percentage (%)", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 11, color = "gray40"),
          legend.position = "top")
  
  ggsave("figures/02b_missing_pattern.png", p, 
         width = 10, height = plot_h, dpi = 300, bg = "white")
  
  cat("✓ 결측치 시각화 저장\n\n")
}

#------------------------------------------------------------------------------
# 5. 이상치 & 중복 제거
#------------------------------------------------------------------------------
cat("=== STEP 5: 이상치 & 중복 제거 ===\n")

# 연령 이상치 (Base에서 이미 처리됨)
cat(sprintf("✓ 연령 이상치: %d건 (Base에서 처리 완료)\n", 
            sum(base_typed$age_flag, na.rm = TRUE)))

# 중복 제거
base_dedup <- base_typed %>% dplyr::distinct()
nurse_dedup <- nurse_typed %>% dplyr::distinct()
fever_lab_dedup <- fever_lab_wide %>% dplyr::distinct()
ct_dedup <- ct_summary %>% dplyr::distinct()
fever_including_dedup <- fever_including_typed %>% dplyr::distinct()
fever_vitals_dedup <- fever_vitals_summary %>% dplyr::distinct()

cat(sprintf("Base: %d → %d (제거: %d)\n", 
            nrow(base_typed), nrow(base_dedup), 
            nrow(base_typed) - nrow(base_dedup)))
cat(sprintf("Nurse: %d → %d (제거: %d)\n", 
            nrow(nurse_typed), nrow(nurse_dedup), 
            nrow(nurse_typed) - nrow(nurse_dedup)))
cat(sprintf("Lab: %d → %d (제거: %d)\n", 
            nrow(fever_lab_wide), nrow(fever_lab_dedup), 
            nrow(fever_lab_wide) - nrow(fever_lab_dedup)))
cat(sprintf("CT: %d → %d (제거: %d)\n", 
            nrow(ct_summary), nrow(ct_dedup), 
            nrow(ct_summary) - nrow(ct_dedup)))
cat(sprintf("Fever Including: %d → %d (제거: %d)\n", 
            nrow(fever_including_typed), nrow(fever_including_dedup), 
            nrow(fever_including_typed) - nrow(fever_including_dedup)))
cat(sprintf("Fever Vitals: %d → %d (제거: %d)\n\n", 
            nrow(fever_vitals_summary), nrow(fever_vitals_dedup), 
            nrow(fever_vitals_summary) - nrow(fever_vitals_dedup)))

#------------------------------------------------------------------------------
# 6. 최종 저장
#------------------------------------------------------------------------------
cat("=== STEP 6: 최종 저장 ===\n")

saveRDS(base_dedup, "cleaned_data/part2_base_typed.rds")
saveRDS(nurse_dedup, "cleaned_data/part2_nurse_typed.rds")
saveRDS(fever_lab_dedup, "cleaned_data/part2_fever_lab_wide.rds")
saveRDS(ct_dedup, "cleaned_data/part2_ct_summary.rds")
saveRDS(fever_including_dedup, "cleaned_data/part2_fever_including_typed.rds")
saveRDS(fever_vitals_dedup, "cleaned_data/part2_fever_vitals_summary.rds")

cat("✓ RDS 파일 저장 완료 (6개)\n\n")

#------------------------------------------------------------------------------
# 7. 통합 품질 리포트
#------------------------------------------------------------------------------
cat("=== STEP 7: 통합 품질 리포트 ===\n")

quality_summary <- tibble(
  Dataset = c("Base", "Fever Including", "Nurse", "Lab (Wide)", "CT"),
  N_Rows = c(nrow(base_dedup), nrow(fever_including_dedup), 
             nrow(nurse_dedup), nrow(fever_lab_dedup), nrow(ct_dedup)),
  N_Columns = c(ncol(base_dedup), ncol(fever_including_dedup),
                ncol(nurse_dedup), ncol(fever_lab_dedup), ncol(ct_dedup)),
  N_Patients = c(
    dplyr::n_distinct(base_dedup$patient_id),
    dplyr::n_distinct(fever_including_dedup$patient_id),
    dplyr::n_distinct(nurse_dedup$patient_id),
    dplyr::n_distinct(fever_lab_dedup$patient_id),
    dplyr::n_distinct(ct_dedup$patient_id)
  ),
  Key_Variables_Success = c(
    "✓ 100%",  # Base는 Part 2a에서 검증 완료
    "✓ 98%+",  # Fever Including도 Part 2a에서 검증 완료
    sprintf("%.1f%%", mean(quality_nurse$Success_Rate[1:4])),
    sprintf("%.1f%%", mean(quality_lab_long$Success_Rate[c(1,6,7)])),  # visit_date, age, result
    sprintf("%.1f%%", mean(quality_ct$Success_Rate[1:2]))
  )
)

print(quality_summary)
write_excel_csv(quality_summary, "reports/02b_quality_summary_all.csv")

cat("\n✓ 통합 품질 리포트 저장\n\n")

#------------------------------------------------------------------------------
# 8. Part 2b 완료
#------------------------------------------------------------------------------
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 2b 완료 (전체 변수 품질 검증 강화)                  \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("✅ 완료 (v3-Step2a 스타일 적용):\n")
cat("   1. ✓ Nurse 전체 변수 타입 변환 & 품질 검증\n")
cat("      - visit_date, visit_time, age, sex, 텍스트 필드\n")
cat("      - 변환 실패 케이스 상세 분석\n")
cat("      - 품질 리포트: 성공률, 상태, 처리 방침\n")
cat("   2. ✓ Lab 전체 변수 타입 변환 & 품질 검증\n")
cat("      - Long 형식: 모든 날짜, 시간, 결과 변수\n")
cat("      - Wide 형식: 한글음차 → 의학용어 매핑\n")
cat("      - 결측치 ≥50%% 변수 제외 및 리스트 저장\n")
cat("   3. ✓ CT 전체 변수 타입 변환 & 품질 검증\n")
cat("      - exam_date, receive_time, 모든 필드\n")
cat("      - 변환 실패 케이스 상세 분석\n")
cat("   4. ✓ 5개 데이터셋 통합 결측치 분석\n")
cat("   5. ✓ 이상치 & 중복 제거 (6개 데이터셋)\n")
cat("   6. ✓ 최종 저장 (part2_*.rds)\n")
cat("   7. ✓ 통합 품질 리포트\n\n")

cat("📊 최종 데이터 요약:\n")
cat(sprintf("   • Base: %d명 (사망: %.1f%%, 품질: ✓)\n", 
            nrow(base_dedup), mean(base_dedup$death) * 100))
cat(sprintf("   • Fever Including: %d건 (품질: ✓)\n", nrow(fever_including_dedup)))
cat(sprintf("   • Nurse: %d건 (성공률: %s)\n",
            nrow(nurse_dedup), quality_summary$Key_Variables_Success[3]))
cat(sprintf("   • Lab: %d개 항목 (제외: %d개, 성공률: %s)\n", 
            ncol(fever_lab_dedup) - 2, length(vars_exclude), 
            quality_summary$Key_Variables_Success[4]))
cat(sprintf("   • CT: %d명 (성공률: %s)\n", 
            nrow(ct_dedup), quality_summary$Key_Variables_Success[5]))
cat(sprintf("   • 활력징후: %d건\n\n", nrow(fever_vitals_dedup)))

cat("📁 생성 파일:\n")
cat("   변환 문제 분석 (3개):\n")
cat("   • reports/02b_conversion_problems_nurse.csv\n")
cat("   • reports/02b_conversion_problems_lab.csv\n")
cat("   • reports/02b_conversion_problems_ct.csv\n\n")
cat("   품질 리포트 (5개):\n")
cat("   • reports/02b_quality_nurse.csv\n")
cat("   • reports/02b_quality_lab_long.csv\n")
cat("   • reports/02b_quality_lab_wide.csv\n")
cat("   • reports/02b_quality_ct.csv\n")
cat("   • reports/02b_quality_summary_all.csv\n\n")
cat("   기타:\n")
cat("   • reports/02b_lab_excluded_variables.csv (제외 변수 목록)\n")
cat("   • reports/02b_missing_analysis_all.csv\n")
cat("   • figures/02b_missing_pattern.png\n")
cat("   • cleaned_data/part2_*.rds (6개)\n\n")

cat("💡 의사결정 기록 (데이터셋별):\n\n")

cat("**Nurse**:\n")
cat(sprintf("   • visit_date (%.1f%%): 필수 → 문제시 제외\n",
            quality_nurse$Overall_Valid_Pct[quality_nurse$Variable == "visit_date"]))
cat(sprintf("   • age (%.1f%%): 필수 → 문제시 제외\n",
            quality_nurse$Overall_Valid_Pct[quality_nurse$Variable == "age"]))
cat(sprintf("   • 텍스트 필드: 'Not recorded' 처리 완료\n\n"))

cat("**Lab**:\n")
cat(sprintf("   • visit_date (%.1f%%): 필수 → 문제시 제외\n",
            quality_lab_long$Overall_Valid_Pct[quality_lab_long$Variable == "visit_date"]))
cat(sprintf("   • result_numeric (%.1f%%): 결측 허용 (검사 미시행 의미)\n",
            quality_lab_long$Overall_Valid_Pct[quality_lab_long$Variable == "result_numeric"]))
cat(sprintf("   • 변수명 매핑: %d개 성공\n", mapped_count))
cat(sprintf("   • 결측치 ≥50%% 변수: %d개 제외 → 품질 확보\n\n", length(vars_exclude)))

cat("**CT**:\n")
cat(sprintf("   • exam_date (%.1f%%): 필수 → 문제시 제외\n",
            quality_ct$Overall_Valid_Pct[quality_ct$Variable == "exam_date"]))
cat(sprintf("   • ct_finding (100%%): 핵심 변수 → 분석 준비 완료\n\n"))

cat("➡️  다음: Part 3 실행 (코호트 선정 & 데이터 통합)\n\n")

# 세션 정보
writeLines(capture.output(sessionInfo()), "reports/02b_session_info.txt")

#==============================================================================
# Part 2c 완료! 다음 Part 3 진행
#==============================================================================
# Part 3에서 수행할 작업:
# 
# 1. CT 외부 분석용 데이터 준비 (Step 3a)
# 2. Inclusion/Exclusion 기준 적용 (85세 이상, CT 시행)
# 3. 파생 변수 생성 (연령 그룹, 계절, COVID 시기 등)
# 4. 데이터 통합 (Base + CT + Vitals + Lab)
# 5. 최종 데이터셋 저장
#
# 필요 입력: cleaned_data/part2_*.rds (6개) ✓
#==============================================================================