# =============================================================================
# 85세 이상 발열 환자 CT 진단 가치 연구: 데이터 클리닝
# =============================================================================
# 연구 배경:
#   - 대상: 85세 이상 발열 환자 (Fever without localizing signs)
#   - 기간: 2020-2022 (COVID-19 pandemic)
#   - 목적: CT 스캔의 진단적 유용성 평가
# =============================================================================

# 패키지 로드 -------------------------------------------------------------------
library(tidyverse)      # 데이터 조작
library(readxl)         # Excel 파일
library(janitor)        # 클리닝
library(skimr)          # 요약
library(naniar)         # 결측치 분석
library(lubridate)      # 날짜 처리

# 작업 디렉토리 설정 ------------------------------------------------------------
setwd("/Users/youjinlee/Library/Mobile Documents/com~apple~CloudDocs/My R/Fever c claude")

# 출력 디렉토리 생성
dir.create("cleaned_data", showWarnings = FALSE)
dir.create("reports", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# =============================================================================
# STEP 1: 데이터 로드 & 백업
# =============================================================================

cat("\n=== STEP 1: 데이터 로드 ===\n")

base_raw <- read_excel("base_result_s.xlsx")
base_original <- base_raw

nurse_raw <- read_excel("nurse_s.xlsx")
nurse_original <- nurse_raw

fever_lab_raw <- read_excel("fever_lab_s.xlsx")
fever_lab_original <- fever_lab_raw

ct_raw <- read_excel("ct_s.xlsx")
ct_original <- ct_raw

cat(sprintf("Base Result: %d rows, %d columns\n", nrow(base_raw), ncol(base_raw)))
cat(sprintf("Nurse: %d rows, %d columns\n", nrow(nurse_raw), ncol(nurse_raw)))
cat(sprintf("Fever Lab: %d rows, %d columns\n", nrow(fever_lab_raw), ncol(fever_lab_raw)))
cat(sprintf("CT: %d rows, %d columns\n", nrow(ct_raw), ncol(ct_raw)))

# 초기 데이터 크기 확인
data_size <- tibble(
  Dataset = c("Base Result", "Nurse", "Fever Lab", "CT"),
  N_Rows = c(nrow(base_raw), nrow(nurse_raw), nrow(fever_lab_raw), nrow(ct_raw)),
  N_Cols = c(ncol(base_raw), ncol(nurse_raw), ncol(fever_lab_raw), ncol(ct_raw)),
  N_Patients = c(
    n_distinct(base_raw$등록번호),
    n_distinct(nurse_raw$등록번호),
    n_distinct(fever_lab_raw$등록번호),
    n_distinct(ct_raw$등록번호)
  )
)

write_csv(data_size, "reports/00_initial_data_size.csv")

# =============================================================================
# STEP 2: 변수명 표준화 (한글 → 영문)
# =============================================================================

cat("\n=== STEP 2: 변수명 표준화 ===\n")

base <- base_raw %>%
  rename(
    patient_id = 등록번호,
    visit_date = 내원일자,
    visit_time = 내원시간,
    sex = 성별,
    age = 나이,
    liver_disease = 간질환,
    liver_disease_desc = `간질환 DESC`,
    hypertension = 고혈압,
    hypertension_desc = `고혈압 DESC`,
    diabetes = 당뇨,
    diabetes_desc = `당뇨 DESC`,
    heart_disease = 심질환,
    heart_disease_desc = `심질환 DESC`,
    kidney_disease = 신질환,
    kidney_disease_desc = `신질환 DESC`,
    respiratory_disease = 호흡기질환,
    respiratory_disease_desc = `호흡기질환 DESC`,
    surgery = 수술,
    surgery_desc = `수술 DESC`,
    cerebrovascular_disease = 뇌혈관질환,
    cerebrovascular_disease_desc = `뇌혈관질환 DESC`,
    neoplasm = Neoplasm,
    neoplasm_desc = `Neoplasm DESC`,
    admission_diagnosis = `입원/퇴실시 진단명`,
    discharge_diagnosis = `퇴원시 진단명`,
    discharge_status = 퇴원상태,
    discharge_date = 퇴원일,
    death_date = 사망일
  )

nurse <- nurse_raw %>%
  rename(
    patient_id = 등록번호,
    patient_name = 환자명,
    visit_date = 내원일,
    visit_time = 내원시간,
    sex = 성별,
    age = 나이,
    procedure = Procedure,
    special_note = 특기사항,
    note = Note
  )

fever_lab <- fever_lab_raw %>%
  rename(
    patient_id = 등록번호,
    visit_date = 내원일자,
    visit_time = 내원시간,
    patient_name = 환자명,
    sex = 성별,
    age = 나이,
    order_code = 처방코드,
    order_name = 처방명,
    order_detail = 상세처방명,
    result = 처방결과
  )

ct <- ct_raw %>%
  rename(
    patient_id = 등록번호,
    visit_date = 내원일자,
    visit_time = 내원시간,
    patient_name = 환자명,
    sex = 성별,
    age = 나이,
    order_code = 처방코드,
    order_name = 처방명,
    ct_finding = 판독결과
  )

cat("변수명 표준화 완료\n")

# =============================================================================
# STEP 3: 데이터 타입 변환
# =============================================================================

cat("\n=== STEP 3: 데이터 타입 변환 ===\n")

# 3.1 Base Result
base <- base %>%
  mutate(
    # 날짜 변환 (YYYYMMDD 형식)
    visit_date = ymd(as.character(visit_date)),
    
    # 퇴원일, 사망일 (빈 문자열 → NA 처리)
    discharge_date = if_else(
      str_trim(as.character(discharge_date)) == "", 
      NA_character_, 
      as.character(discharge_date)
    ),
    discharge_date = ymd(discharge_date),
    # 퇴원일 빈칸 → 내원일과 동일하게 (응급실 당일 퇴실)
    discharge_date = if_else(is.na(discharge_date), visit_date, discharge_date),
    
    death_date = if_else(
      str_trim(as.character(death_date)) == "", 
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
    # 먼저 숫자로 확실히 변환
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
      "None"  # 결측값 대신 명확한 표시
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
    
    # 7. 수술 기왕력 (빈칸 = "None")
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
      is.na(discharge_status) | str_trim(discharge_status) == "" ~ "ER discharge",  # 응급실 퇴실
      str_detect(tolower(discharge_status), "퇴원|discharge") ~ "Discharge",
      TRUE ~ "Discharge"
    ),
    
    # 사망 여부 (결측값 없이 0 또는 1)
    # death_date 빈칸 = 퇴원 이후까지 생존
    death = case_when(
      discharge_status_clean == "Death" | !is.na(death_date) ~ 1,
      TRUE ~ 0  # 모든 빈칸 = 생존
    )
  )

cat("Base Result 변환 완료\n")

# 3.2 Nurse
nurse <- nurse %>%
  mutate(
    visit_date = ymd(as.character(visit_date)),
    visit_time = as.integer(visit_time),
    age = as.numeric(age),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male"))
  )

cat("Nurse 변환 완료\n")

# 3.3 Fever Lab - Wide format 변환
fever_lab <- fever_lab %>%
  mutate(
    visit_date = ymd(as.character(visit_date)),
    visit_time = as.integer(visit_time),
    age = as.numeric(age),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
    result_numeric = suppressWarnings(as.numeric(result))
  )

# Lab 검사명 확인
cat("\nFever Lab 검사 종류 (처방명):\n")
print(table(fever_lab$order_name))

cat("\nFever Lab 상세처방명 (실제 검사 항목):\n")
print(table(fever_lab$order_detail))

# Wide format 변환 (상세처방명 기준)
# 이유: 처방명(예: ROUTINE CBC)은 여러 검사를 포함하므로
#       상세처방명(예: WBC, Hemoglobin, Platelet)으로 분리해야 
#       각 검사의 임상적 의미를 정확히 반영 가능
fever_lab_wide <- fever_lab %>%
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
  "baeghyeolgusu" = "wbc",
  "jeoghyeolgusu" = "rbc", 
  "ilbanhyeol_aeggeomsa_hyeolsaegso" = "hb",
  "hematokeuliteu" = "hct",
  "hyeolsopansu" = "platelet",
  "jeoghyeolgubunpogyesu_rdw" = "rdw",
  "hyeolsopanbunpogyesu_pdw" = "pdw",
  
  # 생화학 검사
  "cr_pjeonglyang_eung_geub" = "crp",
  "dangjeonglyang_geomsa_eung_geub" = "glucose",
  "keuleatiningeomsa_eung_geub" = "creatinine",
  "yosojilsogeomsa_eung_geub" = "bun",
  "sodiumgeomsa_eung_geub" = "na",
  "potasyum_eung_geub" = "k",
  "yeomsogeomsa_eung_geub" = "cl",
  "chongkalsyum_eung_geub" = "ca",
  "mageunesyum_eung_geub" = "mg",
  "in" = "phosphorus",
  
  # 간기능 검사
  "got_eung_geub" = "ast",
  "gpt_eung_geub" = "alt",
  "gammajitipi" = "ggt",
  "alkallinposeupataje" = "alp",
  "chongbillilubinjeonglyang_geomsa_eung_geub" = "total_bilirubin",
  "billilubin_jigjeob" = "direct_bilirubin",
  "albumin" = "albumin",
  "chongdanbaegjeonglyang_geomsa_eung_geub" = "total_protein",
  
  # 심장/근육 효소
  "cp_kgeomsa_eung_geub" = "cpk",
  "ld_hgeomsa_eung_geub" = "ldh",
  
  # 췌장 효소
  "amillajegeomsa_eung_geub" = "amylase",
  "lipaajegeomsa_eung_geub" = "lipase",
  
  # 지질 검사
  "kolleseutelol" = "cholesterol",
  "teuligeuliselaideu" = "triglyceride",
  "hd_lkolleseutelol" = "hdl",
  "ldl_kolleseutelol" = "ldl",
  
  # 기타 검사
  "yosan_eung_geub" = "uric_acid",
  "d_dime_rgeomsa" = "d_dimer",
  "yusan_abga" = "lactate",
  "samtuabnongdo_hyeolcheong_eung_geub" = "osmolality",
  "hyeolcheongchongtansangaseunongdo_eung_geub" = "total_co2",
  "ketongeomsa_eung_geub" = "ketone",
  "ra_jeonglyang" = "rf",
  
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

cat(sprintf("Fever Lab wide format 변환 완료 (%d개 검사 항목)\n", 
            ncol(fever_lab_wide) - 2))  # patient_id, visit_date 제외

# 변환된 변수명 확인
cat("\n변환된 Lab 변수명:\n")
lab_vars <- setdiff(names(fever_lab_wide), c("patient_id", "visit_date"))
print(lab_vars)

# Severe/Critical 결측치 변수 제외
# 각 Lab 변수의 결측치 비율 계산
lab_missing <- fever_lab_wide %>%
  select(-patient_id, -visit_date) %>%
  summarize(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct")

# 50% 이상 결측치 변수 제외 (Severe >= 50%, Critical >= 80%)
vars_to_exclude <- lab_missing %>%
  filter(missing_pct >= 50) %>%
  pull(variable)

if(length(vars_to_exclude) > 0) {
  cat("\n제외되는 Lab 변수 (결측치 ≥50%):\n")
  print(vars_to_exclude)
  
  # 제외
  fever_lab_wide <- fever_lab_wide %>%
    select(-all_of(vars_to_exclude))
  
  cat(sprintf("\n최종 Lab 변수: %d개 (제외: %d개)\n", 
              ncol(fever_lab_wide) - 2, 
              length(vars_to_exclude)))
}

# 3.4 CT
ct <- ct %>%
  mutate(
    visit_date = ymd(as.character(visit_date)),
    visit_time = as.integer(visit_time),
    age = as.numeric(age),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
    
    # CT positive finding 판정
    # 정상/음성 키워드가 없으면 양성으로 간주
    ct_positive = if_else(
      str_detect(tolower(ct_finding), "정상|normal|no acute|unremarkable|negative|없음"),
      0, 1
    )
  )

# CT 환자당 요약
ct_summary <- ct %>%
  group_by(patient_id, visit_date) %>%
  summarize(
    n_ct_scans = n(),
    ct_any_positive = max(ct_positive, na.rm = TRUE),
    ct_findings_combined = paste(ct_finding, collapse = " | "),
    .groups = "drop"
  )

cat("CT 변환 완료\n")

# =============================================================================
# STEP 4: 결측치 분석
# =============================================================================

cat("\n=== STEP 4: 결측치 분석 ===\n")

missing_base <- miss_var_summary(base) %>%
  mutate(dataset = "Base Result")

missing_nurse <- miss_var_summary(nurse) %>%
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

write_csv(missing_all, "reports/01_missing_analysis.csv")

cat(sprintf("결측치 분석 완료. 총 %d개 변수 분석\n", nrow(missing_all)))
cat(sprintf("  - Critical (≥80%%): %d개\n", sum(missing_all$severity == "Critical (≥80%)")))
cat(sprintf("  - Severe (50-79%%): %d개\n", sum(missing_all$severity == "Severe (50-79%)")))

# 결측치 시각화
missing_plot <- gg_miss_var(base, show_pct = TRUE) +
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

ggsave("figures/01_missing_pattern.png", missing_plot, 
       width = 12, height = 6, dpi = 300)

# =============================================================================
# STEP 5: 이상치 처리
# =============================================================================

cat("\n=== STEP 5: 이상치 처리 ===\n")

# 연령 이상치
base <- base %>%
  mutate(
    age_flag = if_else(age < 0 | age > 120, 1, 0),
    age_clean = if_else(age < 0 | age > 120, NA_real_, as.numeric(age))
  )

cat(sprintf("연령 이상치: %d건\n", sum(base$age_flag, na.rm = TRUE)))

# =============================================================================
# STEP 6: 중복 제거
# =============================================================================

cat("\n=== STEP 6: 중복 제거 ===\n")

base_dedup <- base %>% distinct()
nurse_dedup <- nurse %>% distinct()
fever_lab_wide_dedup <- fever_lab_wide %>% distinct()
ct_summary_dedup <- ct_summary %>% distinct()

cat(sprintf("Base: %d → %d (제거: %d)\n", 
            nrow(base), nrow(base_dedup), nrow(base) - nrow(base_dedup)))
cat(sprintf("Fever Lab: %d → %d (제거: %d)\n", 
            nrow(fever_lab_wide), nrow(fever_lab_wide_dedup), 
            nrow(fever_lab_wide) - nrow(fever_lab_wide_dedup)))

# =============================================================================
# STEP 7: Inclusion/Exclusion 기준
# =============================================================================

cat("\n=== STEP 7: Inclusion/Exclusion 기준 적용 ===\n")

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

cat(sprintf("85세 이상: %d명 (%.1f%%)\n", 
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

cat(sprintf("CT 시행: %d명 (%.1f%%)\n", 
            nrow(base_with_ct),
            nrow(base_with_ct)/nrow(base_age_filter)*100))

# Step 4: 최종 분석 대상 (outcome 정보 관계없이 포함)
base_final <- base_with_ct

flowchart <- flowchart %>%
  add_row(Step = "4. Final analysis cohort", 
          N = nrow(base_final),
          Excluded = 0,
          Reason = "")

# Exclusion 비율
flowchart <- flowchart %>%
  mutate(
    Exclusion_Pct = round(Excluded / lag(N) * 100, 1),
    Cumulative_Exclusion = nrow(base_dedup) - N,
    Cumulative_Pct = round(Cumulative_Exclusion / nrow(base_dedup) * 100, 1)
  )

write_csv(flowchart, "reports/03_flowchart.csv")

cat(sprintf("\n최종 분석 코호트: %d명\n", nrow(base_final)))

# =============================================================================
# STEP 8: 파생 변수 생성
# =============================================================================

cat("\n=== STEP 8: 파생 변수 생성 ===\n")

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
    
    # 입원 기간
    los_days = as.numeric(discharge_date - visit_date),
    los_group = case_when(
      is.na(los_days) ~ "Unknown",
      los_days < 7 ~ "<1 week",
      los_days >= 7 & los_days < 14 ~ "1-2 weeks",
      los_days >= 14 & los_days < 30 ~ "2-4 weeks",
      los_days >= 30 ~ "≥4 weeks"
    )
  )

cat("파생 변수 생성 완료\n")

# =============================================================================
# STEP 9: 데이터 통합
# =============================================================================

cat("\n=== STEP 9: 데이터 통합 ===\n")

# Base + CT
base_ct <- base_final %>%
  left_join(ct_summary_dedup, by = c("patient_id", "visit_date"), suffix = c("", "_ct"))

# Base + CT + Lab
base_ct_lab <- base_ct %>%
  left_join(fever_lab_wide_dedup, by = c("patient_id", "visit_date"))

# Base + CT + Lab + Nurse
base_full <- base_ct_lab %>%
  left_join(
    nurse_dedup %>% select(patient_id, visit_date, procedure, special_note, note),
    by = c("patient_id", "visit_date")
  )

# Note: special_note와 note는 참고용 텍스트 정보
# 통계 분석에는 사용하지 않음 (필요시 텍스트 마이닝 가능)

cat(sprintf("Base + CT: %d rows, %d cols\n", nrow(base_ct), ncol(base_ct)))
cat(sprintf("Base + CT + Lab: %d rows, %d cols\n", nrow(base_ct_lab), ncol(base_ct_lab)))
cat(sprintf("Base Full: %d rows, %d cols\n", nrow(base_full), ncol(base_full)))

# =============================================================================
# STEP 10: 최종 데이터 저장
# =============================================================================

cat("\n=== STEP 10: 데이터 저장 ===\n")

# RDS
saveRDS(base_final, "cleaned_data/base_clean.rds")
saveRDS(base_ct, "cleaned_data/base_ct_clean.rds")
saveRDS(base_ct_lab, "cleaned_data/base_ct_lab_clean.rds")
saveRDS(base_full, "cleaned_data/base_full_clean.rds")

# 분석용 데이터셋 (텍스트/참고용 필드 제외)
base_analysis <- base_ct_lab %>%
  select(-ends_with("_desc"), 
         -admission_diagnosis, -discharge_diagnosis,
         -ct_findings_combined)

saveRDS(base_analysis, "cleaned_data/base_analysis.rds")

# CSV
write_csv(base_final, "cleaned_data/base_clean.csv")
write_csv(base_ct, "cleaned_data/base_ct_clean.csv")
write_csv(base_ct_lab, "cleaned_data/base_ct_lab_clean.csv")
write_csv(base_full, "cleaned_data/base_full_clean.csv")
write_csv(base_analysis, "cleaned_data/base_analysis.csv")

# 개별 데이터셋
saveRDS(fever_lab_wide_dedup, "cleaned_data/fever_lab_wide.rds")
saveRDS(ct_summary_dedup, "cleaned_data/ct_summary.rds")
saveRDS(nurse_dedup, "cleaned_data/nurse.rds")

write_csv(fever_lab_wide_dedup, "cleaned_data/fever_lab_wide.csv")
write_csv(ct_summary_dedup, "cleaned_data/ct_summary.csv")
write_csv(nurse_dedup, "cleaned_data/nurse.csv")

cat("데이터 저장 완료\n")

# =============================================================================
# STEP 11: 데이터 딕셔너리
# =============================================================================

cat("\n=== STEP 11: 데이터 딕셔너리 생성 ===\n")

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
cat(sprintf("데이터 딕셔너리: %d개 변수\n", nrow(dictionary)))

# =============================================================================
# STEP 12: 기술통계
# =============================================================================

cat("\n=== STEP 12: 기술통계 ===\n")

# 연령
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

# 성별
sex_summary <- base_final %>%
  count(sex) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))

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

# 사망
death_summary <- base_final %>%
  count(death) %>%
  mutate(
    Status = if_else(death == 1, "Death", "Survived"),
    Percentage = round(n / sum(n) * 100, 1)
  )

# CT positive
ct_positive_summary <- base_ct %>%
  filter(!is.na(ct_any_positive)) %>%
  count(ct_any_positive) %>%
  mutate(
    Result = if_else(ct_any_positive == 1, "Positive", "Negative"),
    Percentage = round(n / sum(n) * 100, 1)
  )

# 저장
write_csv(age_summary, "reports/07_age_summary.csv")
write_csv(sex_summary, "reports/07_sex_summary.csv")
write_csv(comorbidity_summary, "reports/07_comorbidity_summary.csv")
write_csv(death_summary, "reports/07_death_summary.csv")
write_csv(ct_positive_summary, "reports/07_ct_positive_summary.csv")

cat("기술통계 생성 완료\n")

# =============================================================================
# STEP 13: 세션 정보
# =============================================================================

writeLines(capture.output(sessionInfo()), "reports/08_session_info.txt")

# =============================================================================
# 완료 메시지
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("                  데이터 클리닝 완료                             \n")
cat("═══════════════════════════════════════════════════════════════\n")
cat(sprintf("\n📊 최종 분석 코호트: %d명 (85세 이상)\n", nrow(base_final)))
cat(sprintf("   • 평균 연령: %.1f ± %.1f세\n", 
            mean(base_final$age, na.rm=TRUE), 
            sd(base_final$age, na.rm=TRUE)))
cat(sprintf("   • 여성: %d명 (%.1f%%)\n", 
            sum(base_final$sex == "Female", na.rm=TRUE),
            sum(base_final$sex == "Female", na.rm=TRUE)/nrow(base_final)*100))
cat(sprintf("   • CT 양성: %d명 (%.1f%%)\n",
            sum(base_ct$ct_any_positive == 1, na.rm=TRUE),
            sum(base_ct$ct_any_positive == 1, na.rm=TRUE)/sum(!is.na(base_ct$ct_any_positive))*100))
cat(sprintf("   • 사망: %d명 (%.1f%%)\n",
            sum(base_final$death == 1, na.rm=TRUE),
            sum(base_final$death == 1, na.rm=TRUE)/nrow(base_final)*100))

cat("\n📁 생성된 파일:\n")
cat("   cleaned_data/\n")
cat("   ├── base_clean.{rds,csv}\n")
cat("   ├── base_ct_clean.{rds,csv}\n")
cat("   ├── base_ct_lab_clean.{rds,csv}\n")
cat("   ├── base_analysis.{rds,csv}      ⭐ 주요 분석용 (간결)\n")
cat("   └── base_full_clean.{rds,csv}\n\n")
cat("   reports/\n")
cat("   ├── 01_missing_analysis.csv\n")
cat("   ├── 03_flowchart.csv             ⭐ 논문 Figure 1\n")
cat("   ├── 04_data_dictionary.csv\n")
cat("   └── 07_*_summary.csv\n\n")

cat("📝 결측값 처리 전략 (NA 없음):\n")
cat("   1. death_date 빈칸 → 생존 (0), 결측값 없음\n")
cat("   2. Lab 변수 결측치 ≥50% → 분석에서 제외\n")
cat("   3. discharge_diagnosis 빈칸 → admission_diagnosis로 채움\n")
cat("   4. discharge_status 빈칸 → 'ER discharge'\n")
cat("   5. discharge_date 빈칸 → visit_date와 동일 (응급실 당일 퇴실)\n")
cat("   6. 질환 관련 빈칸 → 'None' (결측값 없음)\n")
cat("   7. 수술 기왕력 빈칸 → 'None' (결측값 없음)\n\n")

cat("✨ 표준화된 질환명:\n")
cat("   • HTN, DM (고혈압, 당뇨)\n")
cat("   • CAD, CHF, Arrhythmia (심질환)\n")
cat("   • CKD, ESRD (신질환)\n")
cat("   • COPD, Asthma, TB (호흡기)\n")
cat("   • Cirrhosis, Hepatitis (간질환)\n")
cat("   • Cerebral infarction, ICH, Stroke (뇌혈관)\n")
cat("   • Cancer subtypes (암종별)\n\n")

cat("✅ 다음 단계:\n")
cat("   1. reports/03_flowchart.csv 확인\n")
cat("   2. reports/07_*_summary.csv 검토\n")
cat("   3. 분석 데이터 로드 (권장):\n")
cat("      df <- readRDS('cleaned_data/base_analysis.rds')\n")
cat("      # 또는 전체 데이터:\n")
cat("      # df <- readRDS('cleaned_data/base_ct_lab_clean.rds')\n\n")

cat("═══════════════════════════════════════════════════════════════\n\n")
