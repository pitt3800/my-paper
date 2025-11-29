# =============================================================================
# Part 2/3: 데이터 변환 및 품질 검증
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: 데이터 타입 변환, Lab Wide 변환, 결측치 분석, 품질 검증
# 예상 소요: 3-5분
# =============================================================================

#------------------------------------------------------------------------------
# 0. 환경 설정
#------------------------------------------------------------------------------
library(tidyverse)      # 데이터 조작
library(janitor)        # 변수명 클리닝
library(skimr)          # 데이터 요약
library(naniar)         # 결측치 분석
library(lubridate)      # 날짜 처리

# 작업 디렉토리 설정
setwd("/Users/youjinlee/Library/Mobile Documents/com~apple~CloudDocs/My R/Fever c claude")

cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 2/3: 데이터 변환 및 품질 검증                         \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

# Part 1 결과물 로드
cat("=== Part 1 결과물 로드 ===\n")
base <- readRDS("cleaned_data/part1_base.rds")
nurse <- readRDS("cleaned_data/part1_nurse.rds")
fever_lab <- readRDS("cleaned_data/part1_fever_lab.rds")
ct <- readRDS("cleaned_data/part1_ct.rds")

cat("✓ 데이터 로드 완료\n")
cat(sprintf("  - Base: %d rows\n", nrow(base)))
cat(sprintf("  - Nurse: %d rows\n", nrow(nurse)))
cat(sprintf("  - Fever Lab: %d rows\n", nrow(fever_lab)))
cat(sprintf("  - CT: %d rows\n\n", nrow(ct)))

#------------------------------------------------------------------------------
# 1. Base Result 데이터 타입 변환
#------------------------------------------------------------------------------
cat("=== STEP 1: Base Result 데이터 타입 변환 ===\n")

base_typed <- base %>%
  mutate(
    # 날짜 변환 (YYYYMMDD 형식)
    visit_date = ymd(as.character(visit_date)),
    
    # 퇴원일, 사망일 (빈 문자열 → NA 처리)
    discharge_date = if_else(
      str_trim(as.character(discharge_date)) == "" | discharge_date == "-", 
      NA_character_, 
      as.character(discharge_date)
    ),
    discharge_date = ymd(discharge_date),
    # 퇴원일 빈칸 → 내원일과 동일하게 (응급실 당일 퇴실)
    discharge_date = if_else(is.na(discharge_date), visit_date, discharge_date),
    
    death_date = if_else(
      str_trim(as.character(death_date)) == "" | death_date == "-", 
      NA_character_, 
      as.character(death_date)
    ),
    death_date = ymd(death_date),
    
    # 퇴원 진단명 빈칸 → 입원 진단명으로 채우기
    discharge_diagnosis = if_else(
      is.na(discharge_diagnosis) | str_trim(discharge_diagnosis) == "",
      admission_diagnosis,
      discharge_diagnosis
    ),
    
    # 시간 변환 (HHMM → hour, minute)
    visit_time = as.integer(visit_time),
    visit_time_fmt = sprintf("%04d", visit_time),
    visit_hour = as.numeric(substr(visit_time_fmt, 1, 2)),
    visit_minute = as.numeric(substr(visit_time_fmt, 3, 4)),
    
    # 나이를 숫자로 변환
    age = as.numeric(age),
    
    # 범주형 변환
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
    
    # 동반질환 이진화 (+ = Yes, - or 없음 = No)
    liver_disease_yn = if_else(liver_disease == "+", 1, 0),
    hypertension_yn = if_else(hypertension == "+", 1, 0),
    diabetes_yn = if_else(diabetes == "+", 1, 0),
    heart_disease_yn = if_else(heart_disease == "+", 1, 0),
    kidney_disease_yn = if_else(kidney_disease == "+", 1, 0),
    respiratory_disease_yn = if_else(respiratory_disease == "+", 1, 0),
    surgery_yn = if_else(surgery == "+", 1, 0),
    cerebrovascular_disease_yn = if_else(cerebrovascular_disease == "+", 1, 0),
    neoplasm_yn = if_else(neoplasm == "+", 1, 0),
    
    # 동반질환 DESC 표준화 (빈칸 = "None", 결측값 없음)
    hypertension_simple = if_else(
      hypertension_yn == 1,
      "HTN",
      "None"
    ),
    
    diabetes_simple = if_else(
      diabetes_yn == 1,
      "DM",
      "None"
    ),
    
    liver_disease_simple = case_when(
      liver_disease_yn == 0 ~ "None",
      str_detect(tolower(liver_disease_desc), "cirrhosis|경변|lc") ~ "Cirrhosis",
      str_detect(tolower(liver_disease_desc), "hepatitis|간염") ~ "Hepatitis",
      str_detect(tolower(liver_disease_desc), "fatty|지방간") ~ "Fatty liver",
      TRUE ~ "Liver disease"
    ),
    
    heart_disease_simple = case_when(
      heart_disease_yn == 0 ~ "None",
      str_detect(tolower(heart_disease_desc), "coronary|관상동맥|cad|ischemic") ~ "CAD",
      str_detect(tolower(heart_disease_desc), "chf|heart failure|심부전") ~ "CHF",
      str_detect(tolower(heart_disease_desc), "arrhythmia|부정맥|afib|atrial") ~ "Arrhythmia",
      str_detect(tolower(heart_disease_desc), "valve|판막") ~ "Valvular",
      str_detect(tolower(heart_disease_desc), "cardiomyopathy|심근병증") ~ "Cardiomyopathy",
      TRUE ~ "Heart disease"
    ),
    
    kidney_disease_simple = case_when(
      kidney_disease_yn == 0 ~ "None",
      str_detect(tolower(kidney_disease_desc), "ckd|chronic kidney|만성신") ~ "CKD",
      str_detect(tolower(kidney_disease_desc), "esrd|end stage|말기신|dialysis|투석") ~ "ESRD",
      str_detect(tolower(kidney_disease_desc), "aki|acute kidney|급성신") ~ "AKI",
      TRUE ~ "Kidney disease"
    ),
    
    respiratory_disease_simple = case_when(
      respiratory_disease_yn == 0 ~ "None",
      str_detect(tolower(respiratory_disease_desc), "copd|만성폐쇄") ~ "COPD",
      str_detect(tolower(respiratory_disease_desc), "asthma|천식") ~ "Asthma",
      str_detect(tolower(respiratory_disease_desc), "tuberculosis|결핵|tb") ~ "TB",
      str_detect(tolower(respiratory_disease_desc), "pneumonia|폐렴") ~ "Pneumonia",
      str_detect(tolower(respiratory_disease_desc), "interstitial|간질성") ~ "ILD",
      TRUE ~ "Respiratory disease"
    ),
    
    cerebrovascular_disease_simple = case_when(
      cerebrovascular_disease_yn == 0 ~ "None",
      str_detect(tolower(cerebrovascular_disease_desc), "infarction|경색|ischemic") ~ "Cerebral infarction",
      str_detect(tolower(cerebrovascular_disease_desc), "hemorrhage|출혈|ich|bleeding") ~ "ICH",
      str_detect(tolower(cerebrovascular_disease_desc), "stroke|뇌졸중|cva") ~ "Stroke",
      TRUE ~ "CVD"
    ),
    
    neoplasm_simple = case_when(
      neoplasm_yn == 0 ~ "None",
      str_detect(tolower(neoplasm_desc), "lung|폐암") ~ "Lung cancer",
      str_detect(tolower(neoplasm_desc), "stomach|gastric|위암") ~ "Gastric cancer",
      str_detect(tolower(neoplasm_desc), "colon|colorectal|대장암") ~ "Colorectal cancer",
      str_detect(tolower(neoplasm_desc), "liver|hepat|간암|hcc") ~ "Liver cancer",
      str_detect(tolower(neoplasm_desc), "breast|유방암") ~ "Breast cancer",
      str_detect(tolower(neoplasm_desc), "pancrea|췌장암") ~ "Pancreatic cancer",
      str_detect(tolower(neoplasm_desc), "prostate|전립선암") ~ "Prostate cancer",
      str_detect(tolower(neoplasm_desc), "leukemia|백혈병") ~ "Leukemia",
      str_detect(tolower(neoplasm_desc), "lymphoma|림프종") ~ "Lymphoma",
      TRUE ~ "Cancer"
    ),
    
    surgery_simple = case_when(
      surgery_yn == 0 ~ "None",
      str_detect(tolower(surgery_desc), "cardiac|심장|cabg|valve") ~ "Cardiac surgery",
      str_detect(tolower(surgery_desc), "abdom|복부|gastric|colon|appendix") ~ "Abdominal surgery",
      str_detect(tolower(surgery_desc), "orthopedic|정형|fracture|bone") ~ "Orthopedic surgery",
      str_detect(tolower(surgery_desc), "neuro|brain|spine|뇌|척추") ~ "Neurosurgery",
      TRUE ~ "Post-op"
    ),
    
    # Charlson Comorbidity Index (간단 버전)
    cci = liver_disease_yn + 
          diabetes_yn + 
          heart_disease_yn + 
          kidney_disease_yn + 
          respiratory_disease_yn + 
          cerebrovascular_disease_yn + 
          neoplasm_yn * 2,  # Cancer: weight 2
    
    # 퇴원 상태 표준화
    discharge_status_clean = case_when(
      str_detect(tolower(discharge_status), "사망|death") ~ "Death",
      discharge_status == "EM discharge" ~ "ER discharge",
      str_detect(tolower(discharge_status), "퇴원|discharge") ~ "Discharge",
      TRUE ~ "Discharge"
    ),
    
    # 사망 여부 (결측값 없이 0 또는 1)
    death = case_when(
      discharge_status_clean == "Death" | !is.na(death_date) ~ 1,
      TRUE ~ 0
    )
  )

cat("✓ Base Result 타입 변환 완료\n")
cat(sprintf("  - 날짜 변환: visit_date, discharge_date, death_date\n"))
cat(sprintf("  - 질환 이진화: 9개 질환\n"))
cat(sprintf("  - 질환명 표준화: HTN, DM, CAD, CKD 등\n"))
cat(sprintf("  - CCI 계산 완료\n\n"))

#------------------------------------------------------------------------------
# 2. Nurse 데이터 타입 변환
#------------------------------------------------------------------------------
cat("=== STEP 2: Nurse 데이터 타입 변환 ===\n")

nurse_typed <- nurse %>%
  mutate(
    visit_date = ymd(as.character(visit_date)),
    visit_time = as.integer(visit_time),
    age = as.numeric(age),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male"))
  )

cat("✓ Nurse 타입 변환 완료\n\n")

#------------------------------------------------------------------------------
# 3. Fever Lab Wide 형식 변환
#------------------------------------------------------------------------------
cat("=== STEP 3: Fever Lab Wide 형식 변환 ===\n")

fever_lab_typed <- fever_lab %>%
  mutate(
    visit_date = ymd(as.character(visit_date)),
    visit_time = as.integer(visit_time),
    age = as.numeric(age),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
    result_numeric = suppressWarnings(as.numeric(result))
  )

# Wide format 변환 (상세처방명 기준)
cat("  - Long → Wide 형식 변환 중...\n")
fever_lab_wide <- fever_lab_typed %>%
  group_by(patient_id, visit_date, order_detail) %>%
  summarize(result_numeric = mean(result_numeric, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = order_detail,
    values_from = result_numeric
  ) %>%
  clean_names()

# 한글 음차를 표준 의학 약어로 변환
lab_name_mapping <- c(
  # 혈액학 검사
  "baeghyeolgusu" = "WBC",
  "jeoghyeolgusu" = "RBC", 
  "ilbanhyeol_aeggeomsa_hyeolsaegso" = "Hb",
  "hematokeuliteu" = "Hct",
  "hyeolsopansu" = "Platelet",
  "jeoghyeolgubunpogyesu_rdw" = "Rdw",
  "hyeolsopanbunpogyesu_pdw" = "Pdw",
  
  # 생화학 검사
  "cr_pjeonglyang_eung_geub" = "CRP",
  "dangjeonglyang_geomsa_eung_geub" = "Glucose",
  "keuleatiningeomsa_eung_geub" = "Creatinine",
  "yosojilsogeomsa_eung_geub" = "BUN",
  "sodiumgeomsa_eung_geub" = "Na",
  "potasyum_eung_geub" = "K",
  "yeomsogeomsa_eung_geub" = "Cl",
  "chongkalsyum_eung_geub" = "Ca",
  "mageunesyum_eung_geub" = "Mg",
  "in" = "Phosphorus",
  
  # 간기능 검사
  "got_eung_geub" = "AST",
  "gpt_eung_geub" = "ALT",
  "gammajitipi" = "Ggt",
  "alkallinposeupataje" = "ALP",
  "chongbillilubinjeonglyang_geomsa_eung_geub" = "Total_bilirubin",
  "billilubin_jigjeob" = "Direct_bilirubin",
  "albumin" = "Albumin",
  "chongdanbaegjeonglyang_geomsa_eung_geub" = "Total_protein",
  
  # 심장/근육 효소
  "cp_kgeomsa_eung_geub" = "CPK",
  "ld_hgeomsa_eung_geub" = "LDH",
  
  # 췌장 효소
  "amillajegeomsa_eung_geub" = "Amylase",
  "lipaajegeomsa_eung_geub" = "Lipase",
  
  # 지질 검사
  "kolleseutelol" = "Cholesterol",
  "teuligeuliselaideu" = "Triglyceride",
  "hd_lkolleseutelol" = "hdl",
  "ldl_kolleseutelol" = "ldl",
  
  # 기타 검사
  "yosan_eung_geub" = "Uric_acid",
  "d_dime_rgeomsa" = "D_dimer",
  "yusan_abga" = "Lactate",
  "samtuabnongdo_hyeolcheong_eung_geub" = "Osmolality",
  "hyeolcheongchongtansangaseunongdo_eung_geub" = "Total_co2",
  "ketongeomsa_eung_geub" = "Ketone",
  "ra_jeonglyang" = "RF",
  
  # 감염 검사
  "covid_19_naso_oropharyngeal_swab" = "covid19_pcr_np",
  "covid_19_sputum" = "covid19_pcr_sputum"
)

# 변수명 변경 적용
for (old_name in names(lab_name_mapping)) {
  new_name <- lab_name_mapping[old_name]
  if (old_name %in% names(fever_lab_wide)) {
    names(fever_lab_wide)[names(fever_lab_wide) == old_name] <- new_name
  }
}

cat(sprintf("✓ Wide 형식 변환 완료 (%d개 검사 항목)\n", 
            ncol(fever_lab_wide) - 2))

# Severe/Critical 결측치 변수 제외
lab_vars <- setdiff(names(fever_lab_wide), c("patient_id", "visit_date"))
lab_missing <- fever_lab_wide %>%
  select(all_of(lab_vars)) %>%
  summarize(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct")

# 50% 이상 결측치 변수 제외
vars_to_exclude <- lab_missing %>%
  filter(missing_pct >= 50) %>%
  pull(variable)

if(length(vars_to_exclude) > 0) {
  cat(sprintf("  - 결측치 ≥50%% 변수 제외: %d개\n", length(vars_to_exclude)))
  fever_lab_wide <- fever_lab_wide %>%
    select(-all_of(vars_to_exclude))
}

cat(sprintf("✓ 최종 Lab 변수: %d개\n\n", ncol(fever_lab_wide) - 2))

#------------------------------------------------------------------------------
# 4. CT 데이터 처리
#------------------------------------------------------------------------------
cat("=== STEP 4: CT 데이터 처리 ===\n")

ct_typed <- ct %>%
  mutate(
    visit_date = ymd(as.character(visit_date)),
    visit_time = as.integer(visit_time),
    age = as.numeric(age),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
    
    # CT positive finding 판정
    ct_positive = if_else(
      str_detect(tolower(ct_finding), "정상|normal|no acute|unremarkable|negative|없음"),
      0, 1
    )
  )

# CT 환자당 요약
ct_summary <- ct_typed %>%
  group_by(patient_id, visit_date) %>%
  summarize(
    n_ct_scans = n(),
    ct_any_positive = max(ct_positive, na.rm = TRUE),
    ct_findings_combined = paste(ct_finding, collapse = " | "),
    .groups = "drop"
  )

cat("✓ CT 처리 완료\n")
cat(sprintf("  - CT positive/negative 판정\n"))
cat(sprintf("  - 환자별 CT 요약 완료\n\n"))

#------------------------------------------------------------------------------
# 5. 결측치 분석
#------------------------------------------------------------------------------
cat("=== STEP 5: 결측치 분석 ===\n")

missing_base <- miss_var_summary(base_typed) %>%
  mutate(dataset = "Base Result")

missing_nurse <- miss_var_summary(nurse_typed) %>%
  mutate(dataset = "Nurse")

missing_fever_lab <- miss_var_summary(fever_lab_wide) %>%
  mutate(dataset = "Fever Lab")

missing_ct <- miss_var_summary(ct_summary) %>%
  mutate(dataset = "CT")

missing_all <- bind_rows(missing_base, missing_nurse, missing_fever_lab, missing_ct) %>%
  mutate(
    severity = case_when(
      pct_miss >= 80 ~ "Critical (≥80%)",
      pct_miss >= 50 ~ "Severe (50-79%)",
      pct_miss >= 20 ~ "Moderate (20-49%)",
      pct_miss >= 5 ~ "Minor (5-19%)",
      TRUE ~ "Minimal (<5%)"
    )
  ) %>%
  arrange(desc(pct_miss))

write_csv(missing_all, "reports/02_missing_analysis.csv")

cat(sprintf("✓ 결측치 분석 완료 (총 %d개 변수)\n", nrow(missing_all)))
cat(sprintf("  - Critical (≥80%%): %d개\n", sum(missing_all$severity == "Critical (≥80%)")))
cat(sprintf("  - Severe (50-79%%): %d개\n", sum(missing_all$severity == "Severe (50-79%)")))
cat(sprintf("  - Moderate (20-49%%): %d개\n", sum(missing_all$severity == "Moderate (20-49%)")))
cat(sprintf("  - Minor (5-19%%): %d개\n", sum(missing_all$severity == "Minor (5-19%)")))
cat(sprintf("  - Minimal (<5%%): %d개\n\n", sum(missing_all$severity == "Minimal (<5%)")))

# 결측치 시각화
missing_plot <- gg_miss_var(base_typed, show_pct = TRUE) +
  labs(
    title = "Missing Data Pattern: Base Result Dataset",
    subtitle = "85+ Years Old Patients with Fever",
    x = "Variables",
    y = "Number of Missing Values"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

ggsave("figures/02_missing_pattern.png", missing_plot, 
       width = 12, height = 6, dpi = 300)

cat("✓ 결측치 패턴 시각화 저장: figures/02_missing_pattern.png\n\n")

#------------------------------------------------------------------------------
# 6. 이상치 처리
#------------------------------------------------------------------------------
cat("=== STEP 6: 이상치 처리 ===\n")

# 연령 이상치
base_typed <- base_typed %>%
  mutate(
    age_flag = if_else(age < 0 | age > 120, 1, 0),
    age_clean = if_else(age < 0 | age > 120, NA_real_, as.numeric(age))
  )

cat(sprintf("✓ 연령 이상치 확인: %d건\n\n", sum(base_typed$age_flag, na.rm = TRUE)))

#------------------------------------------------------------------------------
# 7. 중복 제거
#------------------------------------------------------------------------------
cat("=== STEP 7: 중복 제거 ===\n")

base_dedup <- base_typed %>% distinct()
nurse_dedup <- nurse_typed %>% distinct()
fever_lab_wide_dedup <- fever_lab_wide %>% distinct()
ct_summary_dedup <- ct_summary %>% distinct()

cat(sprintf("Base: %d → %d (제거: %d)\n", 
            nrow(base_typed), nrow(base_dedup), 
            nrow(base_typed) - nrow(base_dedup)))
cat(sprintf("Nurse: %d → %d (제거: %d)\n", 
            nrow(nurse_typed), nrow(nurse_dedup), 
            nrow(nurse_typed) - nrow(nurse_dedup)))
cat(sprintf("Fever Lab: %d → %d (제거: %d)\n", 
            nrow(fever_lab_wide), nrow(fever_lab_wide_dedup), 
            nrow(fever_lab_wide) - nrow(fever_lab_wide_dedup)))
cat(sprintf("CT: %d → %d (제거: %d)\n\n", 
            nrow(ct_summary), nrow(ct_summary_dedup), 
            nrow(ct_summary) - nrow(ct_summary_dedup)))

#------------------------------------------------------------------------------
# 8. 중간 결과물 저장 (Part 3에서 사용)
#------------------------------------------------------------------------------
cat("=== STEP 8: 중간 결과물 저장 ===\n")

saveRDS(base_dedup, "cleaned_data/part2_base_typed.rds")
saveRDS(nurse_dedup, "cleaned_data/part2_nurse_typed.rds")
saveRDS(fever_lab_wide_dedup, "cleaned_data/part2_fever_lab_wide.rds")
saveRDS(ct_summary_dedup, "cleaned_data/part2_ct_summary.rds")

cat("✓ 중간 결과물 저장 완료 (RDS 형식)\n")
cat("  - cleaned_data/part2_base_typed.rds\n")
cat("  - cleaned_data/part2_nurse_typed.rds\n")
cat("  - cleaned_data/part2_fever_lab_wide.rds\n")
cat("  - cleaned_data/part2_ct_summary.rds\n\n")

#------------------------------------------------------------------------------
# 9. Part 2 완료 확인
#------------------------------------------------------------------------------
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 2/3 완료                                              \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("✅ 완료된 작업:\n")
cat("   1. ✓ Base Result 데이터 타입 변환\n")
cat("      - 날짜, 시간, 연령 변환\n")
cat("      - 질환 이진화 (9개 질환)\n")
cat("      - 질환명 표준화 (HTN, DM, CAD 등)\n")
cat("      - CCI 계산\n")
cat("   2. ✓ Nurse 데이터 타입 변환\n")
cat("   3. ✓ Fever Lab Wide 형식 변환\n")
cat("      - Long → Wide 형식\n")
cat("      - 변수명 표준화 (한글음차 → 의학용어)\n")
cat("      - 결측치 ≥50%% 변수 제외\n")
cat("   4. ✓ CT 결과 처리\n")
cat("      - CT positive/negative 판정\n")
cat("      - 환자별 CT 요약\n")
cat("   5. ✓ 결측치 분석 및 시각화\n")
cat("   6. ✓ 이상치 처리\n")
cat("   7. ✓ 중복 제거\n\n")

cat("📁 생성된 파일:\n")
cat("   cleaned_data/\n")
cat("   ├── part2_base_typed.rds           # Part 3 입력 파일\n")
cat("   ├── part2_nurse_typed.rds\n")
cat("   ├── part2_fever_lab_wide.rds       # Wide 형식\n")
cat("   └── part2_ct_summary.rds           # 환자별 요약\n\n")
cat("   reports/\n")
cat("   └── 02_missing_analysis.csv        # 결측치 분석\n\n")
cat("   figures/\n")
cat("   └── 02_missing_pattern.png         # 결측치 패턴\n\n")

cat("📊 데이터 요약:\n")
cat(sprintf("   • Base Result: %d명 (중복 제거)\n", nrow(base_dedup)))
cat(sprintf("   • Fever Lab: %d개 검사 항목\n", ncol(fever_lab_wide_dedup) - 2))
cat(sprintf("   • CT 시행: %d명\n", nrow(ct_summary_dedup)))
cat("\n")

# 세션 정보 저장
writeLines(capture.output(sessionInfo()), "reports/02_session_info_part2.txt")

#==============================================================================
# 다음 단계 (Part 3/3)
#==============================================================================
# Part 3에서 수행할 작업:
# 
# 1. Inclusion/Exclusion 기준 적용
#    - 85세 이상 필터링
#    - CT 시행 환자만 선택
#    - Flowchart 생성
#
# 2. 파생 변수 생성
#    - 연령 그룹 (85-89, 90-94, 95-99, 100+)
#    - 계절, 연도, COVID 시기
#    - 방문 시간대 (주간/야간)
#    - CCI 그룹
#    - 입원 기간 그룹
#
# 3. 데이터 통합
#    - Base + CT
#    - Base + CT + Lab
#    - Base + CT + Lab + Nurse (Full)
#
# 4. 최종 결측값 처리
#    - 모든 NA 제거 (분석 가능한 형태로)
#    - Lab -999 처리 (검사 안함)
#
# 5. 최종 데이터셋 저장
#    - base_clean.rds
#    - base_ct_clean.rds
#    - base_analysis.rds (주요 분석용)
#    - base_with_lab.rds (Lab 있는 환자만)
#    - base_ct_lab_clean.rds (전체)
#    - base_full_clean.rds (Nurse 포함)
#
# 6. 데이터 딕셔너리 생성
# 7. 기술통계 생성
#    - 연령, 성별
#    - 동반질환
#    - 사망률
#    - CT 양성률
#
# 필요 입력 파일:
#   - cleaned_data/part2_base_typed.rds
#   - cleaned_data/part2_nurse_typed.rds
#   - cleaned_data/part2_fever_lab_wide.rds
#   - cleaned_data/part2_ct_summary.rds
#
# 예상 산출물:
#   - cleaned_data/base_analysis.rds (⭐ 주요 분석용)
#   - cleaned_data/base_with_lab.rds (Lab 포함)
#   - reports/03_flowchart.csv
#   - reports/04_data_dictionary.csv
#   - reports/07_*_summary.csv (기술통계)
#==============================================================================
