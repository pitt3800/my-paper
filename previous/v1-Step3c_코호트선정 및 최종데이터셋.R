# =============================================================================
# Step 3c: 최종 데이터셋 통합 및 통계
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: 최종 데이터셋 통합, RDS/CSV 저장, 데이터 딕셔너리, 기술통계
# 전제: Step 3b 완료 (part3b_base_final.rds, part3b_ct_summary.rds)
# 예상 소요: 3-4분
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
cat("  Step 3c: 최종 데이터셋 통합 및 통계                        \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

#------------------------------------------------------------------------------
# 1. Step 3b 결과물 로드
#------------------------------------------------------------------------------
cat("=== STEP 1: Step 3b 결과물 로드 ===\n")

base_final <- readRDS("cleaned_data/part3b_base_final.rds")
ct_summary_dedup <- readRDS("cleaned_data/part3b_ct_summary.rds")



#------------------------------------------------------------------------------
# 2. Part 2 데이터 로드 (Lab, Vitals, Nurse)
#------------------------------------------------------------------------------

nurse_dedup <- readRDS("cleaned_data/part2_nurse_typed.rds")
fever_lab_wide_dedup <- readRDS("cleaned_data/part2_fever_lab_wide.rds")
fever_vitals_summary <- readRDS("cleaned_data/part2_fever_vitals_summary.rds")



#------------------------------------------------------------------------------
# 3. 데이터 통합 - Base + CT
#------------------------------------------------------------------------------
cat("=== STEP 3: 데이터 통합 ===\n")

# 3.1 Base + CT
base_ct <- base_final %>%
  left_join(ct_summary_dedup, by = c("patient_id", "visit_date"), suffix = c("", "_ct"))

base_ct <- base_ct %>%
  mutate(
    fever_focus = if_else(is.na(fever_focus), 0L, fever_focus),
    n_ct_scans = if_else(is.na(n_ct_scans), 0, n_ct_scans),
    disease_1 = if_else(is.na(disease_1), "no fever focus", disease_1),
    disease_2 = if_else(is.na(disease_2), "no fever focus", disease_2),
    disease_3 = if_else(is.na(disease_3), "no fever focus", disease_3),
    ct_findings_combined = if_else(is.na(ct_findings_combined), 
                                   "No CT performed", 
                                   ct_findings_combined)
  )

cat(sprintf("✓ Base + CT: %d rows\n", nrow(base_ct)))

# 3.2 Base + CT + Vitals
base_ct_vitals <- base_ct %>%
  left_join(fever_vitals_summary, by = c("patient_id", "visit_date"), suffix = c("", "_vitals"))

base_ct_vitals <- base_ct_vitals %>%
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

cat(sprintf("✓ Base + CT + Vitals: %d rows\n", nrow(base_ct_vitals)))

# 3.3 Base + CT + Lab
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

# 3.4 Base + CT + Lab + Vitals (완전판)
base_ct_lab_vitals <- base_ct_lab %>%
  left_join(fever_vitals_summary, by = c("patient_id", "visit_date"), suffix = c("", "_vitals"))

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

cat(sprintf("✓ Base + CT + Lab + Vitals: %d rows\n", nrow(base_ct_lab_vitals)))

# 3.5 Base Full (Nurse 포함) - 수정
cat("   Nurse 데이터 요약 중...\n")

nurse_summary <- nurse_dedup %>%
  group_by(patient_id, visit_date) %>%
  summarize(
    n_records = n(),  # 기록 개수
    procedure = paste(unique(procedure[procedure != "Not recorded"]), collapse = "; "),
    special_note = paste(unique(special_note[special_note != "None"]), collapse = "; "),
    note = paste(unique(note[note != "None"]), collapse = "; "),
    .groups = "drop"
  ) %>%
  mutate(
    procedure = if_else(procedure == "", "Not recorded", procedure),
    special_note = if_else(special_note == "", "None", special_note),
    note = if_else(note == "", "None", note)
  )

base_full <- base_ct_lab_vitals %>%
  left_join(nurse_summary, by = c("patient_id", "visit_date"))

base_full <- base_full %>%
  mutate(
    n_records = if_else(is.na(n_records), 0L, n_records),
    procedure = if_else(is.na(procedure), "Not recorded", procedure),
    special_note = if_else(is.na(special_note), "None", special_note),
    note = if_else(is.na(note), "None", note)
  )

cat(sprintf("✓ Base Full: %d rows\n", nrow(base_full)))
cat(sprintf("   (Nurse: 평균 %.1f개 기록/환자)\n\n", mean(base_full$n_records[base_full$n_records > 0])))

#------------------------------------------------------------------------------
# 4. 분석용 데이터셋 생성
#------------------------------------------------------------------------------
cat("=== STEP 4: 분석용 데이터셋 생성 ===\n")

# 주요 분석용 (CT + Vitals)
base_analysis <- base_ct_vitals %>%
  select(-ends_with("_desc"), -admission_diagnosis, -discharge_diagnosis)

cat(sprintf("✓ base_analysis: %d rows\n", nrow(base_analysis)))

# Lab 있는 환자만 (완전 분석용)
if("WBC" %in% names(base_ct_lab_vitals) && "CRP" %in% names(base_ct_lab_vitals)) {
  base_with_lab_vitals <- base_ct_lab_vitals %>%
    filter(WBC != -999 & CRP != -999) %>%
    select(-ends_with("_desc"), -admission_diagnosis, -discharge_diagnosis)
  
  cat(sprintf("✓ base_with_lab_vitals: %d rows\n\n", nrow(base_with_lab_vitals)))
} else {
  cat("⚠️  Lab 변수 없음 - base_with_lab_vitals 생성 건너뜀\n\n")
}

#------------------------------------------------------------------------------
# 5. RDS 저장
#------------------------------------------------------------------------------
cat("=== STEP 5: RDS 저장 ===\n")

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

cat("✓ RDS 저장 완료\n\n")

#------------------------------------------------------------------------------
# 6. CSV 저장
#------------------------------------------------------------------------------
cat("=== STEP 6: CSV 저장 ===\n")

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

# 개별 데이터셋
write_excel_csv(fever_lab_wide_dedup, "cleaned_data/fever_lab_wide.csv")
write_excel_csv(fever_vitals_summary, "cleaned_data/fever_vitals_summary.csv")
write_excel_csv(nurse_dedup, "cleaned_data/nurse.csv")

cat("✓ CSV 저장 완료\n\n")

#------------------------------------------------------------------------------
# 7. 데이터 딕셔너리
#------------------------------------------------------------------------------
cat("=== STEP 7: 데이터 딕셔너리 ===\n")

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
  create_dictionary(ct_summary_dedup, "CT Summary with Analysis")
)

write_excel_csv(dictionary, "reports/04_data_dictionary.csv")
cat(sprintf("✓ 데이터 딕셔너리: %d개 변수\n\n", nrow(dictionary)))

#------------------------------------------------------------------------------
# 8. 기술통계
#------------------------------------------------------------------------------
cat("=== STEP 8: 기술통계 ===\n")

# 8.1 연령
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
write_excel_csv(age_summary, "reports/07_age_summary.csv")

# 8.2 성별
sex_summary <- base_final %>%
  count(sex) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
write_excel_csv(sex_summary, "reports/07_sex_summary.csv")

# 8.3 동반질환
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

# 8.4 사망
death_summary <- base_final %>%
  count(death) %>%
  mutate(Status = if_else(death == 1, "Death", "Survived"),
         Percentage = round(n / sum(n) * 100, 1))
write_excel_csv(death_summary, "reports/07_death_summary.csv")

# 8.5 CT positive (fever_focus 기준)
ct_positive_summary <- base_ct %>%
  summarize(
    Total = n(),
    Fever_Focus_Positive = sum(fever_focus == 1, na.rm = TRUE),
    Fever_Focus_Pct = round(sum(fever_focus == 1, na.rm = TRUE) / n() * 100, 1)
  )
write_excel_csv(ct_positive_summary, "reports/07_ct_positive_summary.csv")

# 8.6 활력징후
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
# 9. 최종 완료 메시지
#------------------------------------------------------------------------------
cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("          데이터 클리닝 완료 (CT 분석 통합)                 \n")
cat("╚═══════════════════════════════════════════════════════════╝\n")
cat(sprintf("\n📊 최종 코호트: %d명 (85세 이상)\n", nrow(base_final)))
cat(sprintf("   • 평균 연령: %.1f ± %.1f세\n", 
            mean(base_final$age, na.rm=TRUE), 
            sd(base_final$age, na.rm=TRUE)))
cat(sprintf("   • 여성: %d명 (%.1f%%)\n", 
            sum(base_final$sex == "Female", na.rm=TRUE),
            sum(base_final$sex == "Female", na.rm=TRUE)/nrow(base_final)*100))

cat("\n⭐ CT 분석 결과 (외부 분석 완료!):\n")
cat(sprintf("   • Fever focus positive: %d명 (%.1f%%)\n",
            sum(base_ct$fever_focus == 1, na.rm=TRUE),
            sum(base_ct$fever_focus == 1, na.rm=TRUE)/nrow(base_ct)*100))
cat(sprintf("   • Disease identified: %d명\n",
            sum(base_ct$disease_1 != "no fever focus", na.rm=TRUE)))
cat(sprintf("   • 사망: %d명 (%.1f%%)\n",
            sum(base_final$death == 1, na.rm=TRUE),
            sum(base_final$death == 1, na.rm=TRUE)/nrow(base_final)*100))

cat("\n📁 생성된 주요 파일:\n")
cat("   cleaned_data/\n")
cat("   ├── ct_summary_final.{rds,csv}          ⭐ CT 분석 결과\n")
cat("   ├── base_analysis.{rds,csv}             ⭐ 주요 분석용 (CT+Vitals)\n")
cat("   ├── base_ct_vitals_clean.{rds,csv}      CT + 활력징후\n")
cat("   ├── base_with_lab_vitals.{rds,csv}      Lab + CT + 활력징후\n")
cat("   └── base_full_clean.{rds,csv}           완전판\n\n")
cat("   reports/\n")
cat("   ├── 03_flowchart.csv\n")
cat("   ├── 03_ct_analysis_summary.csv          ⭐ CT 분석 요약\n")
cat("   ├── 03_ct_disease_frequency.csv         ⭐ 질환 빈도\n")
cat("   ├── 04_data_dictionary.csv\n")
cat("   └── 07_*_summary.csv\n\n")

cat("🔑 새로운 CT 분석 변수:\n")
cat("   • fever_focus: 발열 원인 CT 소견 (0/1)\n")
cat("   • disease_1/2/3: 발견된 질환명\n")
cat("   • ct_findings_combined: 원본 CT 소견\n\n")

cat("✅ 다음 단계:\n")
cat("   # CT 분석 포함 데이터 로드\n")
cat("   df <- readRDS('cleaned_data/base_analysis.rds')\n\n")
cat("   # fever_focus로 분석\n")
cat("   table(df$fever_focus)  # 발열 원인 CT 소견 분포\n")
cat("   table(df$disease_1)    # 주요 질환 분포\n\n")

cat("💡 분석 팁:\n")
cat("   • fever_focus = 1: CT에서 발열 원인 발견\n")
cat("   • fever_focus = 0: CT 정상 또는 발열 무관\n")
cat("   • disease_1: 주요 진단명 (발열 원인)\n\n")

cat("📊 데이터셋 선택 가이드:\n")
cat("   • base_analysis: 기본 분석 (CT + Vitals)\n")
cat("   • base_ct_vitals_clean: CT + 활력징후 전체\n")
cat("   • base_with_lab_vitals: Lab + CT + Vitals (완전 분석)\n")
cat("   • base_full_clean: 모든 데이터 포함\n\n")

cat("🎉 데이터 클리닝 완료!\n")
cat("   이제 통계 분석을 시작할 수 있습니다.\n\n")

cat("╚═══════════════════════════════════════════════════════════╝\n\n")

# 세션 정보
writeLines(capture.output(sessionInfo()), "reports/08_session_info_step3c.txt")

#==============================================================================
# Step 3c 완료 - 전체 데이터 클리닝 프로세스 종료
#==============================================================================