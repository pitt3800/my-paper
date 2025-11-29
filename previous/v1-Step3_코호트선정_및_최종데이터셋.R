# =============================================================================
# Part 3/3: 코호트 선정 및 최종 데이터셋 (수정버전)
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: 코호트 필터링, 파생 변수, 데이터 통합, 최종 저장, 통계
# 수정: fever_including 활력징후 통합
# 예상 소요: 4-6분
# =============================================================================

#------------------------------------------------------------------------------
# 0. 환경 설정
#------------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(lubridate)

setwd("/Users/youjinlee/Library/Mobile Documents/com~apple~CloudDocs/My R/Fever c claude")

cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 3/3: 코호트 선정 및 최종 데이터셋 (수정버전)          \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

# Part 2 결과물 로드
cat("=== Part 2 결과물 로드 ===\n")
base_dedup <- readRDS("cleaned_data/part2_base_typed.rds")
nurse_dedup <- readRDS("cleaned_data/part2_nurse_typed.rds")
fever_lab_wide_dedup <- readRDS("cleaned_data/part2_fever_lab_wide.rds")
ct_summary_dedup <- readRDS("cleaned_data/part2_ct_summary.rds")
fever_including_dedup <- readRDS("cleaned_data/part2_fever_including_typed.rds")
fever_vitals_summary <- readRDS("cleaned_data/part2_fever_vitals_summary.rds")

cat("✓ 데이터 로드 완료\n\n")

#------------------------------------------------------------------------------
# 1. Inclusion/Exclusion 기준
#------------------------------------------------------------------------------
cat("=== STEP 1: Inclusion/Exclusion 기준 ===\n")

flowchart <- tibble(
  Step = character(), N = numeric(), Excluded = numeric(), Reason = character()
)

# Step 1: 전체
flowchart <- flowchart %>%
  add_row(Step = "1. Total patients", N = nrow(base_dedup), Excluded = 0, Reason = "")

# Step 2: 85세 이상
base_age_filter <- base_dedup %>% filter(age >= 85)
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
base_with_ct <- base_age_filter %>% filter(patient_id %in% patients_with_ct)

flowchart <- flowchart %>%
  add_row(Step = "3. CT scan performed", 
          N = nrow(base_with_ct),
          Excluded = nrow(base_age_filter) - nrow(base_with_ct),
          Reason = "No CT scan")

cat(sprintf("✓ CT 시행: %d명 (%.1f%%)\n", nrow(base_with_ct),
            nrow(base_with_ct)/nrow(base_age_filter)*100))

# Step 4: 최종
base_final <- base_with_ct
flowchart <- flowchart %>%
  add_row(Step = "4. Final analysis cohort", N = nrow(base_final), Excluded = 0, Reason = "")

flowchart <- flowchart %>%
  mutate(
    Exclusion_Pct = round(Excluded / lag(N) * 100, 1),
    Cumulative_Exclusion = nrow(base_dedup) - N,
    Cumulative_Pct = round(Cumulative_Exclusion / nrow(base_dedup) * 100, 1)
  )

write_excel_csv(flowchart, "reports/03_flowchart.csv")
cat(sprintf("✓ 최종 코호트: %d명\n\n", nrow(base_final)))

#------------------------------------------------------------------------------
# 2. 파생 변수 생성
#------------------------------------------------------------------------------
cat("=== STEP 2: 파생 변수 생성 ===\n")

base_final <- base_final %>%
  mutate(
    # 연령 그룹
    age_group = cut(age, breaks = c(85, 90, 95, 100, 120),
                    labels = c("85-89", "90-94", "95-99", "100+"),
                    include.lowest = TRUE, right = FALSE),
    
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
    
    # 입원 기간
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

cat("✓ 파생 변수 생성 완료\n\n")

#------------------------------------------------------------------------------
# 3. 최종 결측값 처리
#------------------------------------------------------------------------------
cat("=== STEP 3: 최종 결측값 처리 ===\n")

missing_before <- sum(is.na(base_final))
cat(sprintf("처리 전: %d개\n", missing_before))

base_final <- base_final %>%
  mutate(
    across(where(is.character), ~if_else(is.na(.) | . == "", "Unknown", .)),
    across(c(visit_hour, visit_minute), ~if_else(is.na(.), 0, .)),
    
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
    
    across(ends_with("_simple"), ~if_else(is.na(.), "None", .)),
    across(ends_with("_yn"), ~if_else(is.na(.), 0, .)),
    death = if_else(is.na(death), 0, death),
    cci = if_else(is.na(cci), 0, cci),
    discharge_date = if_else(is.na(discharge_date), visit_date, discharge_date)
  )

missing_after <- sum(is.na(base_final))
cat(sprintf("처리 후: %d개\n\n", missing_after))

#------------------------------------------------------------------------------
# 4. ⭐ Fever Including 활력징후 통합
#------------------------------------------------------------------------------
cat("=== STEP 4: Fever Including 활력징후 통합 ===\n")

# Base + CT
base_ct <- base_final %>%
  left_join(ct_summary_dedup, by = c("patient_id", "visit_date"), suffix = c("", "_ct"))

base_ct <- base_ct %>%
  mutate(
    n_ct_scans = if_else(is.na(n_ct_scans), 0, n_ct_scans),
    ct_any_positive = if_else(is.na(ct_any_positive), -1, ct_any_positive),
    ct_findings_combined = if_else(is.na(ct_findings_combined), "No CT performed", ct_findings_combined)
  )

cat(sprintf("✓ Base + CT: %d rows\n", nrow(base_ct)))

# ⭐ Base + CT + Vitals (활력징후)
base_ct_vitals <- base_ct %>%
  left_join(fever_vitals_summary, by = c("patient_id", "visit_date"), suffix = c("", "_vitals"))

# 활력징후 결측값 처리
base_ct_vitals <- base_ct_vitals %>%
  mutate(
    # 활력징후 결측 → -999 (측정 안함)
    across(c(systolic_bp, diastolic_bp, pulse_rate, respiratory_rate, 
             temperature, spo2, bst), 
           ~if_else(is.na(.), -999, .)),
    
    # 범주형 활력징후 변수
    fever_category = if_else(is.na(fever_category), "Unknown", fever_category),
    bp_category = if_else(is.na(bp_category), "Unknown", bp_category),
    consciousness_label = if_else(is.na(consciousness_label), "Unknown", consciousness_label),
    chief_complaint_1 = if_else(is.na(chief_complaint_1), "Unknown", chief_complaint_1),
    diagnosis = if_else(is.na(diagnosis), "Unknown", diagnosis),
    
    # 이진 변수
    across(c(has_fever, tachycardia, tachypnea, hypoxemia), 
           ~if_else(is.na(.), 0, .)),
    
    # 체류시간
    stay_hours = if_else(is.na(stay_hours), 0, stay_hours)
  )

cat(sprintf("✓ Base + CT + Vitals: %d rows (활력징후 추가!)\n", nrow(base_ct_vitals)))

# Base + CT + Lab
base_ct_lab <- base_ct %>%
  left_join(fever_lab_wide_dedup, by = c("patient_id", "visit_date"))

lab_vars <- names(base_ct_lab)[names(base_ct_lab) %in% c(

  "WBC", "RBC", "Hb", "HCT", "Platelet", "RDW", "PDW",
  "CRP", "Glucose", "Creatinine", "BUN", "Na", "K", "Cl", "Ca", "Mg", "Phosphorus",
  "AST", "ALT", "GGT", "ALP", "Total_Bilirubin", "Direct_Bilirubin", "Albumin", "Total_Protein",
  "CPK", "LDH", "Amylase", "Lipase", "Cholesterol", "Triglyceride", "HDL", "LDL",
  "Uric_Acid", "D_Dimer", "Lactate", "Osmolality", "Total_CO2", "Ketone", "RF",
  "COVID19_PCR_NP", "COVID19_PCR_Sputum"
  
)]

if(length(lab_vars) > 0) {
  base_ct_lab <- base_ct_lab %>%
    mutate(across(all_of(lab_vars), ~if_else(is.na(.), -999, .)))
}

cat(sprintf("✓ Base + CT + Lab: %d rows\n", nrow(base_ct_lab)))

# ⭐ Base + CT + Lab + Vitals (완전판)
base_ct_lab_vitals <- base_ct_lab %>%
  left_join(fever_vitals_summary, by = c("patient_id", "visit_date"), suffix = c("", "_vitals"))

# 활력징후 결측값 처리
base_ct_lab_vitals <- base_ct_lab_vitals %>%
  mutate(
    across(c(systolic_bp, diastolic_bp, pulse_rate, respiratory_rate, 
             temperature, spo2, bst), 
           ~if_else(is.na(.), -999, .)),
    fever_category = if_else(is.na(fever_category), "Unknown", fever_category),
    bp_category = if_else(is.na(bp_category), "Unknown", bp_category),
    consciousness_label = if_else(is.na(consciousness_label), "Unknown", consciousness_label),
    chief_complaint_1 = if_else(is.na(chief_complaint_1), "Unknown", chief_complaint_1),
    diagnosis = if_else(is.na(diagnosis), "Unknown", diagnosis),
    across(c(has_fever, tachycardia, tachypnea, hypoxemia), ~if_else(is.na(.), 0, .)),
    stay_hours = if_else(is.na(stay_hours), 0, stay_hours)
  )

cat(sprintf("✓ Base + CT + Lab + Vitals: %d rows (완전판!)\n", nrow(base_ct_lab_vitals)))

# Base Full (Nurse 포함)
base_full <- base_ct_lab_vitals %>%
  left_join(
    nurse_dedup %>% select(patient_id, visit_date, procedure, special_note, note),
    by = c("patient_id", "visit_date")
  )

base_full <- base_full %>%
  mutate(
    procedure = if_else(is.na(procedure), "Not recorded", procedure),
    special_note = if_else(is.na(special_note), "None", special_note),
    note = if_else(is.na(note), "None", note)
  )

cat(sprintf("✓ Base Full: %d rows\n\n", nrow(base_full)))

#------------------------------------------------------------------------------
# 5. 최종 데이터셋 저장
#------------------------------------------------------------------------------
cat("=== STEP 5: 최종 데이터셋 저장 ===\n")

# RDS
saveRDS(base_final, "cleaned_data/base_clean.rds")
saveRDS(base_ct, "cleaned_data/base_ct_clean.rds")
saveRDS(base_ct_vitals, "cleaned_data/base_ct_vitals_clean.rds")
saveRDS(base_ct_lab, "cleaned_data/base_ct_lab_clean.rds")
saveRDS(base_ct_lab_vitals, "cleaned_data/base_ct_lab_vitals_clean.rds")
saveRDS(base_full, "cleaned_data/base_full_clean.rds")

cat("✓ RDS 저장 완료\n")

# 분석용 데이터셋
base_analysis <- base_ct_vitals %>%
  select(-ends_with("_desc"), -admission_diagnosis, -discharge_diagnosis,
         -ct_findings_combined)

saveRDS(base_analysis, "cleaned_data/base_analysis.rds")
cat("✓ base_analysis.rds (⭐ 주요 분석용 - 활력징후 포함!)\n")

# Lab 있는 환자만
if("wbc" %in% names(base_ct_lab_vitals) && "crp" %in% names(base_ct_lab_vitals)) {
  base_with_lab_vitals <- base_ct_lab_vitals %>%
    filter(wbc != -999 & crp != -999) %>%
    select(-ends_with("_desc"), -admission_diagnosis, -discharge_diagnosis,
           -ct_findings_combined)
  
  saveRDS(base_with_lab_vitals, "cleaned_data/base_with_lab_vitals.rds")
  cat(sprintf("✓ base_with_lab_vitals.rds (Lab+Vitals: %d명)\n", nrow(base_with_lab_vitals)))
}

# CSV
write_excel_csv(base_final, "cleaned_data/base_clean.csv")
write_excel_csv(base_ct, "cleaned_data/base_ct_clean.csv")
write_excel_csv(base_ct_vitals, "cleaned_data/base_ct_vitals_clean.csv")
write_excel_csv(base_ct_lab, "cleaned_data/base_ct_lab_clean.csv")
write_excel_csv(base_ct_lab_vitals, "cleaned_data/base_ct_lab_vitals_clean.csv")
write_excel_csv(base_full, "cleaned_data/base_full_clean.csv")
write_excel_csv(base_analysis, "cleaned_data/base_analysis.csv")
if(exists("base_with_lab_vitals")) {
  write_excel_csv(base_with_lab_vitals, "cleaned_data/base_with_lab_vitals.csv")
}

cat("✓ CSV 저장 완료\n")

# 개별 데이터셋
saveRDS(fever_lab_wide_dedup, "cleaned_data/fever_lab_wide.rds")
saveRDS(ct_summary_dedup, "cleaned_data/ct_summary.rds")
saveRDS(nurse_dedup, "cleaned_data/nurse.rds")
saveRDS(fever_vitals_summary, "cleaned_data/fever_vitals_summary.rds")

write_excel_csv(fever_lab_wide_dedup, "cleaned_data/fever_lab_wide.csv")
write_excel_csv(ct_summary_dedup, "cleaned_data/ct_summary.csv")
write_excel_csv(nurse_dedup, "cleaned_data/nurse.csv")
write_excel_csv(fever_vitals_summary, "cleaned_data/fever_vitals_summary.csv")

cat("✓ 개별 데이터셋 저장 완료\n\n")

#------------------------------------------------------------------------------
# 6. 데이터 딕셔너리
#------------------------------------------------------------------------------
cat("=== STEP 6: 데이터 딕셔너리 ===\n")

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
  create_dictionary(base_ct_vitals, "Base + CT + Vitals"),
  create_dictionary(base_ct_lab_vitals, "Base + CT + Lab + Vitals")
)

write_excel_csv(dictionary, "reports/04_data_dictionary.csv")
cat(sprintf("✓ 데이터 딕셔너리: %d개 변수\n\n", nrow(dictionary)))

#------------------------------------------------------------------------------
# 7. 기술통계
#------------------------------------------------------------------------------
cat("=== STEP 7: 기술통계 ===\n")

# 연령
age_summary <- base_final %>%
  summarize(
    N = n(), Mean = round(mean(age, na.rm = TRUE), 1),
    SD = round(sd(age, na.rm = TRUE), 1), Median = median(age, na.rm = TRUE),
    Q1 = quantile(age, 0.25, na.rm = TRUE), Q3 = quantile(age, 0.75, na.rm = TRUE),
    Min = min(age, na.rm = TRUE), Max = max(age, na.rm = TRUE)
  )
write_excel_csv(age_summary, "reports/07_age_summary.csv")

# 성별
sex_summary <- base_final %>%
  count(sex) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
write_excel_csv(sex_summary, "reports/07_sex_summary.csv")

# 동반질환
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
write_excel_csv(comorbidity_summary, "reports/07_comorbidity_summary.csv")

# 사망
death_summary <- base_final %>%
  count(death) %>%
  mutate(Status = if_else(death == 1, "Death", "Survived"),
         Percentage = round(n / sum(n) * 100, 1))
write_excel_csv(death_summary, "reports/07_death_summary.csv")

# CT positive
ct_positive_summary <- base_ct %>%
  filter(!is.na(ct_any_positive) & ct_any_positive != -1) %>%
  count(ct_any_positive) %>%
  mutate(Result = if_else(ct_any_positive == 1, "Positive", "Negative"),
         Percentage = round(n / sum(n) * 100, 1))
write_excel_csv(ct_positive_summary, "reports/07_ct_positive_summary.csv")

# ⭐ 활력징후 요약
vitals_summary <- base_ct_vitals %>%
  filter(temperature != -999) %>%
  summarize(
    N_with_vitals = n(),
    Mean_Temp = round(mean(temperature, na.rm = TRUE), 1),
    SD_Temp = round(sd(temperature, na.rm = TRUE), 1),
    Mean_SBP = round(mean(systolic_bp[systolic_bp != -999], na.rm = TRUE), 1),
    Mean_HR = round(mean(pulse_rate[pulse_rate != -999], na.rm = TRUE), 1),
    Fever_38C_n = sum(has_fever == 1, na.rm = TRUE),
    Fever_38C_pct = round(sum(has_fever == 1, na.rm = TRUE) / n() * 100, 1)
  )
write_excel_csv(vitals_summary, "reports/07_vitals_summary.csv")

cat("✓ 기술통계 생성 완료\n\n")

#------------------------------------------------------------------------------
# 8. 세션 정보
#------------------------------------------------------------------------------
writeLines(capture.output(sessionInfo()), "reports/08_session_info.txt")

#------------------------------------------------------------------------------
# 9. 최종 완료 메시지
#------------------------------------------------------------------------------
cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("          데이터 클리닝 완료 (수정버전)                     \n")
cat("╚═══════════════════════════════════════════════════════════╝\n")
cat(sprintf("\n📊 최종 코호트: %d명 (85세 이상)\n", nrow(base_final)))
cat(sprintf("   • 평균 연령: %.1f ± %.1f세\n", 
            mean(base_final$age, na.rm=TRUE), sd(base_final$age, na.rm=TRUE)))
cat(sprintf("   • 여성: %d명 (%.1f%%)\n", 
            sum(base_final$sex == "Female", na.rm=TRUE),
            sum(base_final$sex == "Female", na.rm=TRUE)/nrow(base_final)*100))
cat(sprintf("   • CT 양성: %d명 (%.1f%%)\n",
            sum(base_ct$ct_any_positive == 1, na.rm=TRUE),
            sum(base_ct$ct_any_positive == 1, na.rm=TRUE)/sum(base_ct$ct_any_positive != -1)*100))
cat(sprintf("   • 사망: %d명 (%.1f%%)\n",
            sum(base_final$death == 1, na.rm=TRUE),
            sum(base_final$death == 1, na.rm=TRUE)/nrow(base_final)*100))

cat("\n⭐ 활력징후 추가 정보:\n")
n_with_vitals <- sum(base_ct_vitals$temperature != -999)
cat(sprintf("   • 활력징후 있음: %d명 (%.1f%%)\n", 
            n_with_vitals, n_with_vitals/nrow(base_ct_vitals)*100))
if(n_with_vitals > 0) {
  cat(sprintf("   • 평균 체온: %.1f°C\n", 
              mean(base_ct_vitals$temperature[base_ct_vitals$temperature != -999], na.rm=TRUE)))
  cat(sprintf("   • 발열(≥38°C): %d명\n", 
              sum(base_ct_vitals$has_fever == 1, na.rm=TRUE)))
}

cat("\n📁 생성된 파일:\n")
cat("   cleaned_data/\n")
cat("   ├── original_cleaned/\n")
cat("   │   ├── base_result_cleaned.{csv,xlsx}\n")
cat("   │   ├── nurse_cleaned.{csv,xlsx}\n")
cat("   │   ├── fever_lab_cleaned.{csv,xlsx}\n")
cat("   │   ├── ct_cleaned.{csv,xlsx}\n")
cat("   │   └── fever_including_cleaned.{csv,xlsx}    ⭐\n")
cat("   │\n")
cat("   ├── base_clean.{rds,csv}\n")
cat("   ├── base_ct_clean.{rds,csv}\n")
cat("   ├── base_ct_vitals_clean.{rds,csv}           ⭐ CT + 활력징후\n")
cat("   ├── base_analysis.{rds,csv}                  ⭐ 주요 분석용\n")
cat("   ├── base_with_lab_vitals.{rds,csv}           ⭐ Lab + 활력징후\n")
cat("   ├── base_ct_lab_vitals_clean.{rds,csv}       ⭐ 완전판\n")
cat("   └── base_full_clean.{rds,csv}\n\n")
cat("   reports/\n")
cat("   ├── 03_flowchart.csv\n")
cat("   ├── 04_data_dictionary.csv\n")
cat("   ├── 07_vitals_summary.csv                    ⭐ 활력징후 통계\n")
cat("   └── 07_*_summary.csv\n\n")

cat("🔍 결측값 처리:\n")
cat("   1. 질환 빈칸 → '-'\n")
cat("   2. death_date 빈칸 → 생존 (0)\n")
cat("   3. Lab 검사 안함 → -999\n")
cat("   4. ⭐ 활력징후 측정 안함 → -999\n\n")

cat("⚠️  데이터셋 선택 가이드:\n")
cat("   • base_analysis: Lab/활력징후 제외 (기본 분석)\n")
cat("   • base_ct_vitals_clean: 활력징후 포함 (⭐ 권장)\n")
cat("   • base_with_lab_vitals: Lab + 활력징후 (완전 분석)\n\n")

cat("✅ 다음 단계:\n")
cat("   # 활력징후 포함 분석 (권장)\n")
cat("   df <- readRDS('cleaned_data/base_analysis.rds')\n\n")
cat("   # Lab + 활력징후 분석\n")
cat("   df_lab <- readRDS('cleaned_data/base_with_lab_vitals.rds')\n\n")

cat("💡 결측값 확인:\n")
cat("   sum(is.na(df))  # 0이어야 함!\n\n")

cat("╚═══════════════════════════════════════════════════════════╝\n\n")

#==============================================================================
# Part 3/3 완료
#==============================================================================
