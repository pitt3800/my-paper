# =============================================================================
# Step 3: 최종 코호트 선정 및 데이터셋 생성 (통합 최적화 버전)
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: CT 분석 통합, 코호트 선정, 데이터 통합, 최종 저장
# 전제: ct_for_external_analysis.csv 파일 준비 완료
# 예상 소요: 5-7분
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
cat("  Step 3: 최종 코호트 선정 및 데이터셋 생성                  \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

#------------------------------------------------------------------------------
# 1. CT 외부 분석 결과 불러오기 및 검증
#------------------------------------------------------------------------------
cat("=== STEP 1: CT 외부 분석 결과 불러오기 ===\n")

# Part 2의 원본 CT 데이터
ct_original <- readRDS("cleaned_data/part2_ct_summary.rds")
cat(sprintf("✓ 원본 CT: %d건\n", nrow(ct_original)))

# CSV 파일 확인
if (!file.exists("ct_for_external_analysis.csv")) {
  stop("❌ 'ct_for_external_analysis.csv' 파일을 찾을 수 없습니다!")
}

# CSV 분석 결과 로드
ct_analysis_only <- read_csv("ct_for_external_analysis.csv", 
                             locale = locale(encoding = "UTF-8"),
                             col_types = cols(
                               patient_id = col_character(),
                               visit_date = col_skip(),
                               n_ct_scans = col_skip(),
                               ct_findings_combined = col_skip(),
                               fever_focus = col_character(),
                               disease_1 = col_character(),
                               disease_2 = col_character(),
                               disease_3 = col_character()
                             )) %>%
  mutate(
    row_num = row_number(),
    fever_focus = case_when(
      fever_focus == "1" | as.character(fever_focus) == "1" ~ 1L,
      fever_focus == "0" | as.character(fever_focus) == "0" ~ 0L,
      TRUE ~ NA_integer_
    ),
    across(starts_with("disease"), ~if_else(is.na(.) | str_trim(.) == "", "", str_trim(.)))
  )

cat(sprintf("✓ CSV 분석 결과: %d건\n", nrow(ct_analysis_only)))

# 행 개수 검증
if (nrow(ct_original) != nrow(ct_analysis_only)) {
  stop(sprintf("❌ 행 개수 불일치! 원본: %d, CSV: %d", 
               nrow(ct_original), nrow(ct_analysis_only)))
}

# 행 번호로 병합
ct_summary_dedup <- ct_original %>%
  mutate(row_num = row_number()) %>%
  left_join(ct_analysis_only %>% select(row_num, fever_focus, disease_1, disease_2, disease_3),
            by = "row_num") %>%
  select(-row_num) %>%
  mutate(
    patient_id = as.character(patient_id),
    visit_date = as.Date(visit_date),
    fever_focus = as.integer(if_else(is.na(fever_focus), 0L, fever_focus)),
    across(starts_with("disease"), ~if_else(. == "", "None", .)),
    n_ct_scans = if_else(is.na(n_ct_scans), 0, as.numeric(n_ct_scans)),
    ct_findings_combined = if_else(is.na(ct_findings_combined) | str_trim(ct_findings_combined) == "",
                                   "No CT performed", ct_findings_combined)
  )

# fever_focus 검증
invalid_fever <- ct_summary_dedup %>% filter(fever_focus < 0 | fever_focus > 1)
if (nrow(invalid_fever) > 0) {
  stop(sprintf("❌ fever_focus 잘못된 값: %d건", nrow(invalid_fever)))
}

cat("✓ CT 분석 통합 완료\n\n")

# CT 분석 결과 요약
ct_analysis_summary <- tibble(
  Metric = c("Total CT", "Fever focus (+)", "Fever focus (%)", 
             "Disease_1", "Disease_2", "Disease_3"),
  Value = c(
    nrow(ct_summary_dedup),
    sum(ct_summary_dedup$fever_focus == 1),
    round(sum(ct_summary_dedup$fever_focus == 1) / nrow(ct_summary_dedup) * 100, 1),
    sum(ct_summary_dedup$disease_1 != "None"),
    sum(ct_summary_dedup$disease_2 != "None"),
    sum(ct_summary_dedup$disease_3 != "None")
  )
)

print(ct_analysis_summary)
write_excel_csv(ct_analysis_summary, "reports/03_ct_analysis_summary.csv")

# Disease 빈도
disease_freq <- ct_summary_dedup %>%
  select(disease_1, disease_2, disease_3) %>%
  pivot_longer(everything(), names_to = "order", values_to = "disease") %>%
  filter(disease != "None") %>%
  count(disease, sort = TRUE) %>%
  mutate(percentage = round(n / nrow(ct_summary_dedup) * 100, 1))

write_excel_csv(disease_freq, "reports/03_ct_disease_frequency.csv")
cat("✓ CT 분석 요약 저장\n\n")

#------------------------------------------------------------------------------
# 2. Part 2 결과물 로드
#------------------------------------------------------------------------------
cat("=== STEP 2: Part 2 데이터 로드 ===\n")

base_dedup <- readRDS("cleaned_data/part2_base_typed.rds")
nurse_dedup <- readRDS("cleaned_data/part2_nurse_typed.rds")
fever_lab_wide_dedup <- readRDS("cleaned_data/part2_fever_lab_wide.rds")
fever_vitals_summary <- readRDS("cleaned_data/part2_fever_vitals_summary.rds")

cat(sprintf("✓ Base: %d행, Nurse: %d행, Lab: %d행, Vitals: %d행\n\n",
            nrow(base_dedup), nrow(nurse_dedup), 
            nrow(fever_lab_wide_dedup), nrow(fever_vitals_summary)))

#------------------------------------------------------------------------------
# 3. Inclusion/Exclusion 기준
#------------------------------------------------------------------------------
cat("=== STEP 3: 코호트 선정 ===\n")

flowchart <- tibble(
  Step = c("1. Total patients", "2. Age ≥ 85 years", 
           "3. CT performed", "4. Final cohort"),
  N = c(nrow(base_dedup), NA, NA, NA),
  Excluded = c(0, NA, NA, 0),
  Reason = c("", "Age < 85", "No CT", "")
)

# 85세 이상
base_age_filter <- base_dedup %>% filter(age >= 85)
flowchart$N[2] <- nrow(base_age_filter)
flowchart$Excluded[2] <- nrow(base_dedup) - nrow(base_age_filter)

# CT 시행
patients_with_ct <- unique(ct_summary_dedup$patient_id)
base_final <- base_age_filter %>% filter(patient_id %in% patients_with_ct)
flowchart$N[3] <- nrow(base_final)
flowchart$Excluded[3] <- nrow(base_age_filter) - nrow(base_final)
flowchart$N[4] <- nrow(base_final)

flowchart <- flowchart %>%
  mutate(
    Exclusion_Pct = round(Excluded / lag(N) * 100, 1),
    Cumulative_Exclusion = nrow(base_dedup) - N,
    Cumulative_Pct = round(Cumulative_Exclusion / nrow(base_dedup) * 100, 1)
  )

write_excel_csv(flowchart, "reports/03_flowchart.csv")
cat(sprintf("✓ 최종 코호트: %d명 (85세 이상 + CT 시행)\n\n", nrow(base_final)))

#------------------------------------------------------------------------------
# 4. 파생 변수 생성
#------------------------------------------------------------------------------
cat("=== STEP 4: 파생 변수 생성 ===\n")

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
    
    # COVID 시기
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
      los_days < 7 ~ "<1 week",
      los_days < 14 ~ "1-2 weeks",
      los_days < 30 ~ "2-4 weeks",
      TRUE ~ "≥4 weeks"
    )
  )

cat("✓ 파생 변수 생성 완료\n\n")

#------------------------------------------------------------------------------
# 5. 최종 결측값 처리
#------------------------------------------------------------------------------
cat("=== STEP 5: 결측값 처리 ===\n")

base_final <- base_final %>%
  mutate(
    across(where(is.character), ~if_else(is.na(.) | . == "", "Unknown", .)),
    across(c(visit_hour, visit_minute), ~if_else(is.na(.), 0, .)),
    sex = factor(if_else(is.na(as.character(sex)), "Unknown", as.character(sex))),
    age_group = factor(if_else(is.na(as.character(age_group)), "85-89", as.character(age_group)),
                       levels = c("85-89", "90-94", "95-99", "100+", "Unknown")),
    across(c(season, covid_period, visit_shift, cci_group, los_group), 
           ~if_else(is.na(.), "Unknown", .)),
    across(ends_with("_simple"), ~if_else(is.na(.), "None", .)),
    across(ends_with("_yn"), ~if_else(is.na(.), 0, .)),
    death = if_else(is.na(death), 0, death),
    cci = if_else(is.na(cci), 0, cci)
  )

cat(sprintf("✓ 결측값 처리 완료 (총 NA: %d개)\n\n", sum(is.na(base_final))))

#------------------------------------------------------------------------------
# 6. 데이터 통합
#------------------------------------------------------------------------------
cat("=== STEP 6: 데이터 통합 ===\n")

# 6.1 Base + CT
base_ct <- base_final %>%
  left_join(ct_summary_dedup, by = c("patient_id", "visit_date"), suffix = c("", "_ct")) %>%
  mutate(
    n_ct_scans = if_else(is.na(n_ct_scans), 0, n_ct_scans),
    fever_focus = if_else(is.na(fever_focus), 0L, fever_focus),
    across(starts_with("disease"), ~if_else(is.na(.), "None", .)),
    ct_findings_combined = if_else(is.na(ct_findings_combined), "No CT performed", ct_findings_combined)
  )

cat(sprintf("✓ Base + CT: %d행\n", nrow(base_ct)))

# 6.2 Base + CT + Vitals
base_ct_vitals <- base_ct %>%
  left_join(fever_vitals_summary, by = c("patient_id", "visit_date"), suffix = c("", "_vitals")) %>%
  mutate(
    across(c(systolic_bp, diastolic_bp, pulse_rate, respiratory_rate, temperature, spo2, bst),
           ~if_else(is.na(.), -999, .)),
    across(c(fever_category, bp_category, consciousness_label, chief_complaint_1, diagnosis),
           ~if_else(is.na(.), "Unknown", .)),
    across(c(has_fever, tachycardia, tachypnea, hypoxemia), ~if_else(is.na(.), 0, .)),
    stay_hours = if_else(is.na(stay_hours), 0, stay_hours)
  )

cat(sprintf("✓ Base + CT + Vitals: %d행\n", nrow(base_ct_vitals)))

# 6.3 Base + CT + Lab
base_ct_lab <- base_ct %>%
  left_join(fever_lab_wide_dedup, by = c("patient_id", "visit_date"))

lab_vars <- names(base_ct_lab)[names(base_ct_lab) %in% c(
  "WBC", "RBC", "Hb", "HCT", "Platelet", "RDW", "PDW", "CRP", "Glucose", 
  "Creatinine", "BUN", "Na", "K", "Cl", "Ca", "Mg", "Phosphorus",
  "AST", "ALT", "GGT", "ALP", "Total_Bilirubin", "Direct_Bilirubin", 
  "Albumin", "Total_Protein", "CPK", "LDH", "Amylase", "Lipase", 
  "Cholesterol", "Triglyceride", "HDL", "LDL", "Uric_Acid", "D_Dimer", 
  "Lactate", "Osmolality", "Total_CO2", "Ketone", "RF",
  "COVID19_PCR_NP", "COVID19_PCR_Sputum"
)]

if(length(lab_vars) > 0) {
  base_ct_lab <- base_ct_lab %>%
    mutate(across(all_of(lab_vars), ~if_else(is.na(.), -999, .)))
}

cat(sprintf("✓ Base + CT + Lab: %d행\n", nrow(base_ct_lab)))

# 6.4 Base + CT + Lab + Vitals
base_ct_lab_vitals <- base_ct_lab %>%
  left_join(fever_vitals_summary, by = c("patient_id", "visit_date"), suffix = c("", "_vitals")) %>%
  mutate(
    across(c(systolic_bp, diastolic_bp, pulse_rate, respiratory_rate, temperature, spo2, bst),
           ~if_else(is.na(.), -999, .)),
    across(c(fever_category, bp_category, consciousness_label, chief_complaint_1, diagnosis),
           ~if_else(is.na(.), "Unknown", .)),
    across(c(has_fever, tachycardia, tachypnea, hypoxemia), ~if_else(is.na(.), 0, .)),
    stay_hours = if_else(is.na(stay_hours), 0, stay_hours)
  )

cat(sprintf("✓ Base + CT + Lab + Vitals: %d행\n", nrow(base_ct_lab_vitals)))

# 6.5 Nurse 통합 (Many-to-Many 해결)
nurse_summarized <- nurse_dedup %>%
  group_by(patient_id, visit_date) %>%
  summarize(
    n_procedures = n(),
    procedure = paste(unique(procedure[procedure != "Not recorded"]), collapse = " | "),
    special_note = paste(unique(special_note[special_note != "None"]), collapse = " | "),
    note = paste(unique(note[note != "None"]), collapse = " | "),
    .groups = "drop"
  ) %>%
  mutate(
    procedure = if_else(procedure == "", "Not recorded", procedure),
    special_note = if_else(special_note == "", "None", special_note),
    note = if_else(note == "", "None", note)
  )

cat(sprintf("   Nurse 통합: %d행 → %d행\n", nrow(nurse_dedup), nrow(nurse_summarized)))

base_full <- base_ct_lab_vitals %>%
  left_join(nurse_summarized, by = c("patient_id", "visit_date")) %>%
  mutate(
    n_procedures = if_else(is.na(n_procedures), 0, n_procedures),
    procedure = if_else(is.na(procedure), "Not recorded", procedure),
    special_note = if_else(is.na(special_note), "None", special_note),
    note = if_else(is.na(note), "None", note)
  )

cat(sprintf("✓ Base Full: %d행 (행 수 유지 확인)\n\n", nrow(base_full)))

# 행 수 검증
if(nrow(base_full) != nrow(base_ct_lab_vitals)) {
  cat(sprintf("⚠️  경고: 행 수 불일치! 예상: %d, 실제: %d\n\n",
              nrow(base_ct_lab_vitals), nrow(base_full)))
}

#------------------------------------------------------------------------------
# 7. 분석용 데이터셋 생성
#------------------------------------------------------------------------------
cat("=== STEP 7: 분석용 데이터셋 ===\n")

base_analysis <- base_ct_vitals %>%
  select(-ends_with("_desc"), -admission_diagnosis, -discharge_diagnosis)

cat(sprintf("✓ base_analysis: %d행 (주요 분석용)\n", nrow(base_analysis)))

if("WBC" %in% names(base_ct_lab_vitals) && "CRP" %in% names(base_ct_lab_vitals)) {
  base_with_lab_vitals <- base_ct_lab_vitals %>%
    filter(WBC != -999 & CRP != -999) %>%
    select(-ends_with("_desc"), -admission_diagnosis, -discharge_diagnosis)
  
  cat(sprintf("✓ base_with_lab_vitals: %d행 (Lab 포함)\n\n", nrow(base_with_lab_vitals)))
}

#------------------------------------------------------------------------------
# 8. 최종 저장
#------------------------------------------------------------------------------
cat("=== STEP 8: 최종 저장 ===\n")

# RDS
saveRDS(base_final, "cleaned_data/base_clean.rds")
saveRDS(base_ct, "cleaned_data/base_ct_clean.rds")
saveRDS(base_ct_vitals, "cleaned_data/base_ct_vitals_clean.rds")
saveRDS(base_ct_lab, "cleaned_data/base_ct_lab_clean.rds")
saveRDS(base_ct_lab_vitals, "cleaned_data/base_ct_lab_vitals_clean.rds")
saveRDS(base_full, "cleaned_data/base_full_clean.rds")
saveRDS(ct_summary_dedup, "cleaned_data/ct_summary_final.rds")
saveRDS(base_analysis, "cleaned_data/base_analysis.rds")
if(exists("base_with_lab_vitals")) {
  saveRDS(base_with_lab_vitals, "cleaned_data/base_with_lab_vitals.rds")
}

cat("✓ RDS 저장 완료\n")

# CSV
write_excel_csv(base_final, "cleaned_data/base_clean.csv")
write_excel_csv(base_ct, "cleaned_data/base_ct_clean.csv")
write_excel_csv(base_ct_vitals, "cleaned_data/base_ct_vitals_clean.csv")
write_excel_csv(base_ct_lab, "cleaned_data/base_ct_lab_clean.csv")
write_excel_csv(base_ct_lab_vitals, "cleaned_data/base_ct_lab_vitals_clean.csv")
write_excel_csv(base_full, "cleaned_data/base_full_clean.csv")
write_excel_csv(ct_summary_dedup, "cleaned_data/ct_summary_final.csv")
write_excel_csv(base_analysis, "cleaned_data/base_analysis.csv")
if(exists("base_with_lab_vitals")) {
  write_excel_csv(base_with_lab_vitals, "cleaned_data/base_with_lab_vitals.csv")
}

cat("✓ CSV 저장 완료\n\n")

#------------------------------------------------------------------------------
# 9. 데이터 딕셔너리
#------------------------------------------------------------------------------
cat("=== STEP 9: 데이터 딕셔너리 ===\n")

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
  create_dictionary(ct_summary_dedup, "CT Summary")
)

write_excel_csv(dictionary, "reports/04_data_dictionary.csv")
cat(sprintf("✓ 데이터 딕셔너리: %d개 변수\n\n", nrow(dictionary)))

#------------------------------------------------------------------------------
# 10. 기술통계
#------------------------------------------------------------------------------
cat("=== STEP 10: 기술통계 ===\n")

# 연령
age_summary <- base_final %>%
  summarize(N = n(), Mean = round(mean(age, na.rm = TRUE), 1),
            SD = round(sd(age, na.rm = TRUE), 1), Median = median(age, na.rm = TRUE),
            Q1 = quantile(age, 0.25, na.rm = TRUE), Q3 = quantile(age, 0.75, na.rm = TRUE),
            Min = min(age, na.rm = TRUE), Max = max(age, na.rm = TRUE))
write_excel_csv(age_summary, "reports/07_age_summary.csv")

# 성별
sex_summary <- base_final %>%
  count(sex) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
write_excel_csv(sex_summary, "reports/07_sex_summary.csv")

# 동반질환
comorbidity_summary <- base_final %>%
  summarize(Liver = sum(liver_disease_yn, na.rm = TRUE),
            Hypertension = sum(hypertension_yn, na.rm = TRUE),
            Diabetes = sum(diabetes_yn, na.rm = TRUE),
            Heart = sum(heart_disease_yn, na.rm = TRUE),
            Kidney = sum(kidney_disease_yn, na.rm = TRUE),
            Respiratory = sum(respiratory_disease_yn, na.rm = TRUE),
            Cerebrovascular = sum(cerebrovascular_disease_yn, na.rm = TRUE),
            Neoplasm = sum(neoplasm_yn, na.rm = TRUE)) %>%
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
  summarize(Total = n(),
            Fever_Focus_Positive = sum(fever_focus == 1, na.rm = TRUE),
            Fever_Focus_Pct = round(sum(fever_focus == 1, na.rm = TRUE) / n() * 100, 1))
write_excel_csv(ct_positive_summary, "reports/07_ct_positive_summary.csv")

# 활력징후
vitals_summary <- base_ct_vitals %>%
  filter(temperature != -999) %>%
  summarize(N_with_vitals = n(),
            Mean_Temp = round(mean(temperature, na.rm = TRUE), 1),
            SD_Temp = round(sd(temperature, na.rm = TRUE), 1),
            Mean_SBP = round(mean(systolic_bp[systolic_bp != -999], na.rm = TRUE), 1),
            Mean_HR = round(mean(pulse_rate[pulse_rate != -999], na.rm = TRUE), 1),
            Fever_38C_n = sum(has_fever == 1, na.rm = TRUE),
            Fever_38C_pct = round(sum(has_fever == 1, na.rm = TRUE) / n() * 100, 1))
write_excel_csv(vitals_summary, "reports/07_vitals_summary.csv")

cat("✓ 기술통계 생성 완료\n\n")

#------------------------------------------------------------------------------
# 11. 최종 완료 메시지
#------------------------------------------------------------------------------
cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("          Step 3 완료: 최종 데이터셋 준비 완료               \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat(sprintf("📊 최종 코호트: %d명 (85세 이상 + CT 시행)\n", nrow(base_final)))
cat(sprintf("   • 평균 연령: %.1f ± %.1f세\n", 
            mean(base_final$age, na.rm=TRUE), sd(base_final$age, na.rm=TRUE)))
cat(sprintf("   • 여성: %d명 (%.1f%%)\n", 
            sum(base_final$sex == "Female", na.rm=TRUE),
            sum(base_final$sex == "Female", na.rm=TRUE)/nrow(base_final)*100))

cat("\n⭐ CT 분석 결과:\n")
cat(sprintf("   • Fever focus (+): %d명 (%.1f%%)\n",
            sum(base_ct$fever_focus == 1, na.rm=TRUE),
            sum(base_ct$fever_focus == 1, na.rm=TRUE)/nrow(base_ct)*100))
cat(sprintf("   • Disease identified: %d명\n",
            sum(base_ct$disease_1 != "None", na.rm=TRUE)))
cat(sprintf("   • 사망: %d명 (%.1f%%)\n",
            sum(base_final$death == 1, na.rm=TRUE),
            sum(base_final$death == 1, na.rm=TRUE)/nrow(base_final)*100))

cat("\n📁 생성된 데이터셋:\n")
cat("   • base_analysis.rds           ⭐ 주요 분석용 (CT+Vitals)\n")
cat("   • base_ct_vitals_clean.rds    CT + 활력징후\n")
cat("   • base_with_lab_vitals.rds    Lab + CT + 활력징후 (완전)\n")
cat("   • base_full_clean.rds         모든 데이터 포함\n")
cat("   • ct_summary_final.rds        CT 분석 결과\n\n")

cat("✅ 다음 단계: 통계 분석\n")
cat("   df <- readRDS('cleaned_data/base_analysis.rds')\n")
cat("   table(df$fever_focus)  # CT 양성률\n\n")

cat("╚═══════════════════════════════════════════════════════════╝\n\n")

writeLines(capture.output(sessionInfo()), "reports/08_session_info.txt")

#==============================================================================
# Step 3 완료
#==============================================================================