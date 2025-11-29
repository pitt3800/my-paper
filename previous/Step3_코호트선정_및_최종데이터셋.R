# =============================================================================
# Part 3/3: 코호트 선정 및 최종 데이터셋
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: 코호트 필터링, 파생 변수, 데이터 통합, 최종 저장, 통계
# 예상 소요: 3-5분
# =============================================================================

#------------------------------------------------------------------------------
# 0. 환경 설정
#------------------------------------------------------------------------------
library(tidyverse)      # 데이터 조작
library(janitor)        # 변수명 클리닝
library(lubridate)      # 날짜 처리

# 작업 디렉토리 설정
setwd("/mnt/project")

cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 3/3: 코호트 선정 및 최종 데이터셋                     \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

# Part 2 결과물 로드
cat("=== Part 2 결과물 로드 ===\n")
base_dedup <- readRDS("cleaned_data/part2_base_typed.rds")
nurse_dedup <- readRDS("cleaned_data/part2_nurse_typed.rds")
fever_lab_wide_dedup <- readRDS("cleaned_data/part2_fever_lab_wide.rds")
ct_summary_dedup <- readRDS("cleaned_data/part2_ct_summary.rds")

cat("✓ 데이터 로드 완료\n")
cat(sprintf("  - Base: %d rows\n", nrow(base_dedup)))
cat(sprintf("  - Nurse: %d rows\n", nrow(nurse_dedup)))
cat(sprintf("  - Fever Lab: %d rows\n", nrow(fever_lab_wide_dedup)))
cat(sprintf("  - CT: %d rows\n\n", nrow(ct_summary_dedup)))

#------------------------------------------------------------------------------
# 1. Inclusion/Exclusion 기준 적용
#------------------------------------------------------------------------------
cat("=== STEP 1: Inclusion/Exclusion 기준 ===\n")

flowchart <- tibble(
  Step = character(),
  N = numeric(),
  Excluded = numeric(),
  Reason = character()
)

# Step 1: 전체
flowchart <- flowchart %>%
  add_row(Step = "1. Total patients", 
          N = nrow(base_dedup), 
          Excluded = 0,
          Reason = "")

# Step 2: 85세 이상
base_age_filter <- base_dedup %>%
  filter(age >= 85)

flowchart <- flowchart %>%
  add_row(Step = "2. Age ≥ 85 years", 
          N = nrow(base_age_filter),
          Excluded = nrow(base_dedup) - nrow(base_age_filter),
          Reason = "Age < 85 years")

cat(sprintf("✓ 85세 이상: %d명 (%.1f%%)\n", 
            nrow(base_age_filter), 
            nrow(base_age_filter)/nrow(base_dedup)*100))

# Step 3: CT 시행
patients_with_ct <- unique(ct_summary_dedup$patient_id)

base_with_ct <- base_age_filter %>%
  filter(patient_id %in% patients_with_ct)

flowchart <- flowchart %>%
  add_row(Step = "3. CT scan performed", 
          N = nrow(base_with_ct),
          Excluded = nrow(base_age_filter) - nrow(base_with_ct),
          Reason = "No CT scan")

cat(sprintf("✓ CT 시행: %d명 (%.1f%%)\n", 
            nrow(base_with_ct),
            nrow(base_with_ct)/nrow(base_age_filter)*100))

# Step 4: 최종 분석 대상
base_final <- base_with_ct

flowchart <- flowchart %>%
  add_row(Step = "4. Final analysis cohort", 
          N = nrow(base_final),
          Excluded = 0,
          Reason = "")

# Exclusion 비율 계산
flowchart <- flowchart %>%
  mutate(
    Exclusion_Pct = round(Excluded / lag(N) * 100, 1),
    Cumulative_Exclusion = nrow(base_dedup) - N,
    Cumulative_Pct = round(Cumulative_Exclusion / nrow(base_dedup) * 100, 1)
  )

write_csv(flowchart, "reports/03_flowchart.csv")

cat(sprintf("\n✓ 최종 분석 코호트: %d명\n", nrow(base_final)))
cat(sprintf("  - Flowchart 저장: reports/03_flowchart.csv\n\n"))

#------------------------------------------------------------------------------
# 2. 파생 변수 생성
#------------------------------------------------------------------------------
cat("=== STEP 2: 파생 변수 생성 ===\n")

base_final <- base_final %>%
  mutate(
    # 연령 그룹
    age_group = cut(
      age, 
      breaks = c(85, 90, 95, 100, 120),
      labels = c("85-89", "90-94", "95-99", "100+"),
      include.lowest = TRUE,
      right = FALSE
    ),
    
    # 계절
    season = case_when(
      month(visit_date) %in% c(12, 1, 2) ~ "Winter",
      month(visit_date) %in% c(3, 4, 5) ~ "Spring",
      month(visit_date) %in% c(6, 7, 8) ~ "Summer",
      month(visit_date) %in% c(9, 10, 11) ~ "Fall"
    ),
    
    # 연도
    year = year(visit_date),
    covid_period = case_when(
      year == 2020 ~ "2020 (Early COVID)",
      year == 2021 ~ "2021 (Mid COVID)",
      year == 2022 ~ "2022 (Late COVID)",
      TRUE ~ as.character(year)
    ),
    
    # 방문 시간대
    visit_shift = case_when(
      visit_hour >= 7 & visit_hour < 15 ~ "Day (07-15)",
      visit_hour >= 15 & visit_hour < 23 ~ "Evening (15-23)",
      TRUE ~ "Night (23-07)"
    ),
    
    # CCI 그룹
    cci_group = case_when(
      cci == 0 ~ "0 (None)",
      cci >= 1 & cci <= 2 ~ "1-2 (Mild)",
      cci >= 3 & cci <= 4 ~ "3-4 (Moderate)",
      cci >= 5 ~ "5+ (Severe)"
    ),
    
    # 입원 기간 (음수 방지)
    los_days = as.numeric(discharge_date - visit_date),
    los_days = if_else(los_days < 0, 0, los_days),
    los_group = case_when(
      is.na(los_days) ~ "<1 week",
      los_days < 7 ~ "<1 week",
      los_days >= 7 & los_days < 14 ~ "1-2 weeks",
      los_days >= 14 & los_days < 30 ~ "2-4 weeks",
      los_days >= 30 ~ "≥4 weeks",
      TRUE ~ "<1 week"
    )
  )

cat("✓ 파생 변수 생성 완료\n")
cat("  - 연령 그룹 (85-89, 90-94, 95-99, 100+)\n")
cat("  - 계절, 연도, COVID 시기\n")
cat("  - 방문 시간대 (Day/Evening/Night)\n")
cat("  - CCI 그룹\n")
cat("  - 입원 기간 그룹\n\n")

#------------------------------------------------------------------------------
# 3. 최종 결측값 처리
#------------------------------------------------------------------------------
cat("=== STEP 3: 최종 결측값 처리 ===\n")

missing_before <- sum(is.na(base_final))
cat(sprintf("처리 전 결측값: %d개\n", missing_before))

base_final <- base_final %>%
  mutate(
    # Character/Factor 변수의 NA → "Unknown" 또는 적절한 값
    across(where(is.character), ~if_else(is.na(.) | . == "", "Unknown", .)),
    
    # Numeric 변수 중 파생 변수의 NA 처리
    across(c(visit_hour, visit_minute), ~if_else(is.na(.), 0, .)),
    
    # Factor 변수의 NA 처리
    sex = as.character(sex),
    sex = if_else(is.na(sex), "Unknown", sex),
    sex = factor(sex),
    
    age_group = as.character(age_group),
    age_group = if_else(is.na(age_group), "85-89", age_group),
    age_group = factor(age_group, levels = c("85-89", "90-94", "95-99", "100+", "Unknown")),
    
    season = if_else(is.na(season), "Unknown", season),
    covid_period = if_else(is.na(covid_period), "Unknown", covid_period),
    visit_shift = if_else(is.na(visit_shift), "Day (07-15)", visit_shift),
    cci_group = if_else(is.na(cci_group), "0 (None)", cci_group),
    los_group = if_else(is.na(los_group), "<1 week", los_group),
    
    # 동반질환 simple 변수들 - 혹시 NA가 있다면 "None"으로
    across(ends_with("_simple"), ~if_else(is.na(.), "None", .)),
    
    # 이진 변수들의 NA → 0
    across(ends_with("_yn"), ~if_else(is.na(.), 0, .)),
    death = if_else(is.na(death), 0, death),
    
    # CCI의 NA → 0
    cci = if_else(is.na(cci), 0, cci),
    
    # 날짜 변수 - 이미 처리되었지만 재확인
    discharge_date = if_else(is.na(discharge_date), visit_date, discharge_date)
  )

missing_after <- sum(is.na(base_final))
cat(sprintf("처리 후 결측값: %d개\n\n", missing_after))

if(missing_after > 0) {
  cat("⚠️ 남은 결측값이 있는 변수:\n")
  missing_vars <- base_final %>%
    summarize(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
    filter(n_missing > 0) %>%
    arrange(desc(n_missing))
  print(missing_vars)
  cat("\n")
}

#------------------------------------------------------------------------------
# 4. 데이터 통합
#------------------------------------------------------------------------------
cat("=== STEP 4: 데이터 통합 ===\n")

# 4.1 Base + CT
base_ct <- base_final %>%
  left_join(ct_summary_dedup, by = c("patient_id", "visit_date"), suffix = c("", "_ct"))

# CT 변수의 결측값 처리
base_ct <- base_ct %>%
  mutate(
    n_ct_scans = if_else(is.na(n_ct_scans), 0, n_ct_scans),
    ct_any_positive = if_else(is.na(ct_any_positive), -1, ct_any_positive),
    ct_findings_combined = if_else(is.na(ct_findings_combined), "No CT performed", ct_findings_combined)
  )

cat(sprintf("✓ Base + CT: %d rows, %d cols\n", nrow(base_ct), ncol(base_ct)))

# 4.2 Base + CT + Lab
base_ct_lab <- base_ct %>%
  left_join(fever_lab_wide_dedup, by = c("patient_id", "visit_date"))

# Lab 변수 확인
lab_vars <- names(base_ct_lab)[names(base_ct_lab) %in% c(
  "wbc", "rbc", "hb", "hct", "platelet", "rdw", "pdw",
  "crp", "glucose", "creatinine", "bun", "na", "k", "cl", "ca", "mg", "phosphorus",
  "ast", "alt", "ggt", "alp", "total_bilirubin", "direct_bilirubin", "albumin", "total_protein",
  "cpk", "ldh", "amylase", "lipase",
  "cholesterol", "triglyceride", "hdl", "ldl",
  "uric_acid", "d_dimer", "lactate", "osmolality", "total_co2", "ketone", "rf",
  "covid19_pcr_np", "covid19_pcr_sputum"
)]

cat(sprintf("  - Lab 변수: %d개\n", length(lab_vars)))

# Lab 변수의 NA → -999 (검사 안함)
if(length(lab_vars) > 0) {
  base_ct_lab <- base_ct_lab %>%
    mutate(across(all_of(lab_vars), ~if_else(is.na(.), -999, .)))
  cat("  - Lab 변수 결측값 → -999 (검사 안함)\n")
}

cat(sprintf("✓ Base + CT + Lab: %d rows, %d cols\n", nrow(base_ct_lab), ncol(base_ct_lab)))

# 4.3 Base + CT + Lab + Nurse
base_full <- base_ct_lab %>%
  left_join(
    nurse_dedup %>% select(patient_id, visit_date, procedure, special_note, note),
    by = c("patient_id", "visit_date")
  )

# Nurse 텍스트 변수의 NA 처리
base_full <- base_full %>%
  mutate(
    procedure = if_else(is.na(procedure), "Not recorded", procedure),
    special_note = if_else(is.na(special_note), "None", special_note),
    note = if_else(is.na(note), "None", note)
  )

cat(sprintf("✓ Base Full: %d rows, %d cols\n\n", nrow(base_full), ncol(base_full)))

#------------------------------------------------------------------------------
# 5. 최종 데이터셋 저장
#------------------------------------------------------------------------------
cat("=== STEP 5: 최종 데이터셋 저장 ===\n")

# 5.1 RDS 저장
saveRDS(base_final, "cleaned_data/base_clean.rds")
saveRDS(base_ct, "cleaned_data/base_ct_clean.rds")
saveRDS(base_ct_lab, "cleaned_data/base_ct_lab_clean.rds")
saveRDS(base_full, "cleaned_data/base_full_clean.rds")

cat("✓ RDS 저장 완료\n")

# 5.2 분석용 데이터셋 (텍스트/참고용 필드 제외)
base_analysis <- base_ct %>%
  select(-ends_with("_desc"), 
         -admission_diagnosis, -discharge_diagnosis,
         -ct_findings_combined)

saveRDS(base_analysis, "cleaned_data/base_analysis.rds")
cat("✓ base_analysis.rds 저장 (⭐ 주요 분석용)\n")

# 5.3 Lab이 있는 환자만 (wbc, crp 있는 환자)
if("wbc" %in% names(base_ct_lab) && "crp" %in% names(base_ct_lab)) {
  base_with_lab <- base_ct_lab %>%
    filter(wbc != -999 & crp != -999) %>%
    select(-ends_with("_desc"),
           -admission_diagnosis, -discharge_diagnosis,
           -ct_findings_combined)
  
  saveRDS(base_with_lab, "cleaned_data/base_with_lab.rds")
  cat(sprintf("✓ base_with_lab.rds 저장 (Lab 있는 환자: %d명)\n", nrow(base_with_lab)))
}

# 5.4 CSV 저장
write_csv(base_final, "cleaned_data/base_clean.csv")
write_csv(base_ct, "cleaned_data/base_ct_clean.csv")
write_csv(base_ct_lab, "cleaned_data/base_ct_lab_clean.csv")
write_csv(base_full, "cleaned_data/base_full_clean.csv")
write_csv(base_analysis, "cleaned_data/base_analysis.csv")
if(exists("base_with_lab")) {
  write_csv(base_with_lab, "cleaned_data/base_with_lab.csv")
}

cat("✓ CSV 저장 완료\n")

# 5.5 개별 데이터셋 저장
saveRDS(fever_lab_wide_dedup, "cleaned_data/fever_lab_wide.rds")
saveRDS(ct_summary_dedup, "cleaned_data/ct_summary.rds")
saveRDS(nurse_dedup, "cleaned_data/nurse.rds")

write_csv(fever_lab_wide_dedup, "cleaned_data/fever_lab_wide.csv")
write_csv(ct_summary_dedup, "cleaned_data/ct_summary.csv")
write_csv(nurse_dedup, "cleaned_data/nurse.csv")

cat("✓ 개별 데이터셋 저장 완료\n\n")

#------------------------------------------------------------------------------
# 6. 데이터 딕셔너리
#------------------------------------------------------------------------------
cat("=== STEP 6: 데이터 딕셔너리 생성 ===\n")

create_dictionary <- function(df, dataset_name) {
  tibble(
    Dataset = dataset_name,
    Variable = names(df),
    Type = map_chr(df, ~class(.)[1]),
    N_Missing = map_dbl(df, ~sum(is.na(.))),
    Pct_Missing = round(map_dbl(df, ~mean(is.na(.)) * 100), 1),
    N_Unique = map_dbl(df, ~n_distinct(., na.rm = TRUE)),
    Example = map_chr(df, ~{
      vals <- head(na.omit(.), 2)
      if(length(vals) == 0) return("")
      paste(vals, collapse = ", ")
    })
  )
}

dictionary <- bind_rows(
  create_dictionary(base_final, "Base Final"),
  create_dictionary(base_ct, "Base + CT"),
  create_dictionary(base_ct_lab, "Base + CT + Lab")
)

write_csv(dictionary, "reports/04_data_dictionary.csv")
cat(sprintf("✓ 데이터 딕셔너리: %d개 변수\n\n", nrow(dictionary)))

#------------------------------------------------------------------------------
# 7. 기술통계
#------------------------------------------------------------------------------
cat("=== STEP 7: 기술통계 ===\n")

# 7.1 연령
age_summary <- base_final %>%
  summarize(
    N = n(),
    Mean = round(mean(age, na.rm = TRUE), 1),
    SD = round(sd(age, na.rm = TRUE), 1),
    Median = median(age, na.rm = TRUE),
    Q1 = quantile(age, 0.25, na.rm = TRUE),
    Q3 = quantile(age, 0.75, na.rm = TRUE),
    Min = min(age, na.rm = TRUE),
    Max = max(age, na.rm = TRUE)
  )

write_csv(age_summary, "reports/07_age_summary.csv")

# 7.2 성별
sex_summary <- base_final %>%
  count(sex) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))

write_csv(sex_summary, "reports/07_sex_summary.csv")

# 7.3 동반질환
comorbidity_summary <- base_final %>%
  summarize(
    Liver = sum(liver_disease_yn, na.rm = TRUE),
    Hypertension = sum(hypertension_yn, na.rm = TRUE),
    Diabetes = sum(diabetes_yn, na.rm = TRUE),
    Heart = sum(heart_disease_yn, na.rm = TRUE),
    Kidney = sum(kidney_disease_yn, na.rm = TRUE),
    Respiratory = sum(respiratory_disease_yn, na.rm = TRUE),
    Cerebrovascular = sum(cerebrovascular_disease_yn, na.rm = TRUE),
    Neoplasm = sum(neoplasm_yn, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Comorbidity", values_to = "Count") %>%
  mutate(Percentage = round(Count / nrow(base_final) * 100, 1)) %>%
  arrange(desc(Count))

write_csv(comorbidity_summary, "reports/07_comorbidity_summary.csv")

# 7.4 사망
death_summary <- base_final %>%
  count(death) %>%
  mutate(
    Status = if_else(death == 1, "Death", "Survived"),
    Percentage = round(n / sum(n) * 100, 1)
  )

write_csv(death_summary, "reports/07_death_summary.csv")

# 7.5 CT positive
ct_positive_summary <- base_ct %>%
  filter(!is.na(ct_any_positive) & ct_any_positive != -1) %>%
  count(ct_any_positive) %>%
  mutate(
    Result = if_else(ct_any_positive == 1, "Positive", "Negative"),
    Percentage = round(n / sum(n) * 100, 1)
  )

write_csv(ct_positive_summary, "reports/07_ct_positive_summary.csv")

cat("✓ 기술통계 생성 완료\n")
cat("  - 연령, 성별, 동반질환, 사망, CT 양성률\n\n")

#------------------------------------------------------------------------------
# 8. 세션 정보
#------------------------------------------------------------------------------
writeLines(capture.output(sessionInfo()), "reports/08_session_info.txt")

#------------------------------------------------------------------------------
# 9. 최종 완료 메시지
#------------------------------------------------------------------------------
cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("                  데이터 클리닝 완료                        \n")
cat("╚═══════════════════════════════════════════════════════════╝\n")
cat(sprintf("\n📊 최종 분석 코호트: %d명 (85세 이상)\n", nrow(base_final)))
cat(sprintf("   • 평균 연령: %.1f ± %.1f세\n", 
            mean(base_final$age, na.rm=TRUE), 
            sd(base_final$age, na.rm=TRUE)))
cat(sprintf("   • 여성: %d명 (%.1f%%)\n", 
            sum(base_final$sex == "Female", na.rm=TRUE),
            sum(base_final$sex == "Female", na.rm=TRUE)/nrow(base_final)*100))
cat(sprintf("   • CT 양성: %d명 (%.1f%%)\n",
            sum(base_ct$ct_any_positive == 1, na.rm=TRUE),
            sum(base_ct$ct_any_positive == 1, na.rm=TRUE)/sum(base_ct$ct_any_positive != -1)*100))
cat(sprintf("   • 사망: %d명 (%.1f%%)\n",
            sum(base_final$death == 1, na.rm=TRUE),
            sum(base_final$death == 1, na.rm=TRUE)/nrow(base_final)*100))

cat("\n📁 생성된 파일:\n")
cat("   cleaned_data/\n")
cat("   ├── original_cleaned/           ⭐ 원본 cleaned 버전\n")
cat("   │   ├── base_result_cleaned.{csv,xlsx}\n")
cat("   │   ├── nurse_cleaned.{csv,xlsx}\n")
cat("   │   ├── fever_lab_cleaned.{csv,xlsx}\n")
cat("   │   └── ct_cleaned.{csv,xlsx}\n")
cat("   │\n")
cat("   ├── base_clean.{rds,csv}         # 기본 환자 정보\n")
cat("   ├── base_ct_clean.{rds,csv}      # 환자 + CT\n")
cat("   ├── base_analysis.{rds,csv}      ⭐ 주요 분석용 (Lab 제외)\n")
cat("   ├── base_with_lab.{rds,csv}      ⭐ Lab 검사 있는 환자만\n")
cat("   ├── base_ct_lab_clean.{rds,csv}  # 전체 (Lab -999 포함)\n")
cat("   └── base_full_clean.{rds,csv}    # 완전판 (Nurse 포함)\n\n")
cat("   reports/\n")
cat("   ├── 01_missing_analysis.csv\n")
cat("   ├── 03_flowchart.csv             ⭐ 논문 Figure 1\n")
cat("   ├── 04_data_dictionary.csv\n")
cat("   └── 07_*_summary.csv\n\n")

cat("🔍 결측값 처리 전략:\n")
cat("   1. 질환 관련 변수 빈칸 → '-' (질환 없음)\n")
cat("   2. discharge_status 빈칸 → 'EM discharge'\n")
cat("   3. death_date 빈칸 → 생존 (0)\n")
cat("   4. Lab 변수 결측치 ≥50%% → 분석에서 제외\n")
cat("   5. Lab 검사 안함 → -999 표시 (base_ct_lab)\n")
cat("   6. discharge_diagnosis 빈칸 → admission_diagnosis로 채움\n")
cat("   7. discharge_date 빈칸 → visit_date와 동일\n")
cat("   8. 질환 관련 빈칸 → 'None'\n")
cat("   9. 수술 기왕력 빈칸 → 'None'\n")
cat("   10. CT 안함 → ct_any_positive = -1\n\n")

cat("⚠️  Lab 변수 사용 시 주의사항:\n")
cat("   • base_analysis: Lab 변수 없음 (깔끔)\n")
cat("   • base_with_lab: Lab 검사 있는 환자만 (권장)\n")
cat("   • base_ct_lab: 모든 환자 포함 (Lab -999 = 검사 안함)\n\n")

cat("✨ 표준화된 질환명:\n")
cat("   • HTN, DM (고혈압, 당뇨)\n")
cat("   • CAD, CHF, Arrhythmia (심질환)\n")
cat("   • CKD, ESRD (신질환)\n")
cat("   • COPD, Asthma, TB (호흡기)\n")
cat("   • Cirrhosis, Hepatitis (간질환)\n")
cat("   • Cerebral infarction, ICH, Stroke (뇌혈관)\n")
cat("   • Cancer subtypes (암종별)\n")
cat("   • None = 질환 없음\n\n")

cat("✅ 다음 단계:\n")
cat("   1. reports/03_flowchart.csv 확인\n")
cat("   2. reports/07_*_summary.csv 검토\n")
cat("   3. 분석 데이터 로드:\n\n")
cat("   # 기본 분석 (Lab 없음, 깔끔)\n")
cat("   df <- readRDS('cleaned_data/base_analysis.rds')\n\n")
cat("   # Lab 포함 분석 (Lab 있는 환자만)\n")
cat("   df_lab <- readRDS('cleaned_data/base_with_lab.rds')\n\n")
cat("   # 전체 데이터 (Lab -999 포함)\n")
cat("   # df_full <- readRDS('cleaned_data/base_ct_lab_clean.rds')\n\n")

cat("💡 결측값 확인:\n")
cat("   sum(is.na(df))  # 0이어야 함!\n\n")

cat("╚═══════════════════════════════════════════════════════════╝\n\n")

#==============================================================================
# Part 3/3 완료
#==============================================================================
