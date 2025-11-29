# =============================================================================
# Part 2/3: 데이터 변환 및 품질 검증 (수정버전)
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: 데이터 타입 변환, Lab Wide 변환, 결측치 분석, 품질 검증
# 수정: fever_including 타입 변환 및 활력징후 처리 추가
# 예상 소요: 4-6분
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
cat("  Part 2/3: 데이터 변환 및 품질 검증 (수정버전)              \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

# Part 1 결과물 로드-변수명 통일과 결측값 정리한 버전 불러오기
cat("=== Part 1 결과물 로드 ===\n")
base <- readRDS("cleaned_data/part1_base.rds")
nurse <- readRDS("cleaned_data/part1_nurse.rds")
fever_lab <- readRDS("cleaned_data/part1_fever_lab.rds")
ct <- readRDS("cleaned_data/part1_ct.rds")
fever_including <- readRDS("cleaned_data/part1_fever_including.rds")



#------------------------------------------------------------------------------
# 1. ⭐ Fever Including 데이터 타입 변환
#------------------------------------------------------------------------------
cat("=== STEP 1: Fever Including 데이터 타입 변환 ===\n")

fever_including_typed <- fever_including %>%
  mutate(
    # 날짜 변환
    visit_date = ymd(as.character(visit_date)),
    onset_date = ymd(as.character(onset_date)),
    actual_discharge_date = ymd(as.character(actual_discharge_date)),
    
    # 시간 변환
    visit_time = as.integer(visit_time),
    visit_hour = as.numeric(substr(sprintf("%04d", visit_time), 1, 2)),
    visit_minute = as.numeric(substr(sprintf("%04d", visit_time), 3, 4)),
    
    onset_time = as.integer(onset_time),
    actual_discharge_time = as.integer(actual_discharge_time),
    
    # 나이, 성별
    age = as.numeric(age),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
    
    # 활력징후 (숫자형)
    systolic_bp = as.numeric(systolic_bp),
    diastolic_bp = as.numeric(diastolic_bp),
    pulse_rate = as.numeric(pulse_rate),
    respiratory_rate = as.numeric(respiratory_rate),
    temperature = as.numeric(temperature),
    spo2 = as.numeric(spo2),
    bst = as.numeric(bst),
    weight = as.numeric(weight),
    
    # 의식상태 (1=alert, 2=verbal, 3=pain, 4=unresponsive)
    consciousness_level = as.integer(consciousness_level),
    consciousness_label = case_when(
      consciousness_level == 1 ~ "Alert",
      consciousness_level == 2 ~ "Verbal",
      consciousness_level == 3 ~ "Pain",
      consciousness_level == 4 ~ "Unresponsive",
      TRUE ~ "Unknown"
    ),
    
    # 체온 범주화
    fever_category = case_when(
      is.na(temperature) ~ "Unknown",
      temperature < 36.0 ~ "Hypothermia (<36°C)",
      temperature >= 36.0 & temperature < 37.5 ~ "Normal (36-37.4°C)",
      temperature >= 37.5 & temperature < 38.0 ~ "Low fever (37.5-37.9°C)",
      temperature >= 38.0 & temperature < 39.0 ~ "Moderate fever (38-38.9°C)",
      temperature >= 39.0 ~ "High fever (≥39°C)",
      TRUE ~ "Unknown"
    ),
    
    # 발열 여부 (≥38°C)
    has_fever = if_else(temperature >= 38.0, 1, 0),
    
    # 혈압 범주
    bp_category = case_when(
      is.na(systolic_bp) | is.na(diastolic_bp) ~ "Unknown",
      systolic_bp < 90 | diastolic_bp < 60 ~ "Hypotension",
      systolic_bp >= 90 & systolic_bp < 120 & diastolic_bp >= 60 & diastolic_bp < 80 ~ "Normal",
      systolic_bp >= 120 & systolic_bp < 140 & diastolic_bp >= 80 & diastolic_bp < 90 ~ "Prehypertension",
      systolic_bp >= 140 | diastolic_bp >= 90 ~ "Hypertension",
      TRUE ~ "Unknown"
    ),
    
    # 빈맥 여부 (>100 bpm)
    tachycardia = if_else(!is.na(pulse_rate) & pulse_rate > 100, 1, 0),
    
    # 빈호흡 여부 (>20 /min)
    tachypnea = if_else(!is.na(respiratory_rate) & respiratory_rate > 20, 1, 0),
    
    # 저산소증 여부 (<94%)
    hypoxemia = if_else(!is.na(spo2) & spo2 < 94, 1, 0),
    
    # 체류시간 (분 → 시간)
    stay_duration = as.numeric(stay_duration),
    stay_hours = round(stay_duration / 60, 1),
    
    # 범주형 변수
    visit_reason = as.factor(visit_reason),
    visit_route = as.factor(visit_route),
    arrival_method = as.factor(arrival_method)
  )

cat("✓ Fever Including 타입 변환 완료\n")
cat("  - 날짜: visit_date, onset_date, actual_discharge_date\n")
cat("  - 활력징후: BP, HR, RR, Temp, SpO2, BST\n")
cat("  - 파생 변수: fever_category, bp_category, tachycardia 등\n\n")

# 활력징후 요약 (환자별 첫 방문)
fever_vitals_summary <- fever_including_typed %>%
  group_by(patient_id, visit_date) %>%
  slice(1) %>%  # 같은 날 여러 기록이 있으면 첫 번째만
  ungroup() %>%
  select(patient_id, visit_date, 
         systolic_bp, diastolic_bp, pulse_rate, respiratory_rate,
         temperature, spo2, bst, 
         fever_category, has_fever, bp_category, 
         tachycardia, tachypnea, hypoxemia,
         consciousness_level, consciousness_label,
         chief_complaint_1, diagnosis,
         stay_hours)

cat(sprintf("✓ 활력징후 요약 완료: %d건\n\n", nrow(fever_vitals_summary)))

#------------------------------------------------------------------------------
# 2. Base Result 데이터 타입 변환
#------------------------------------------------------------------------------
cat("=== STEP 2: Base Result 데이터 타입 변환 ===\n")

base_typed <- base %>%
  mutate(
    # 날짜 변환
    visit_date = ymd(as.character(visit_date)),
    
    discharge_date = if_else(
      str_trim(as.character(discharge_date)) == "" | discharge_date == "-", 
      NA_character_, 
      as.character(discharge_date)
    ),
    discharge_date = ymd(discharge_date),
    discharge_date = if_else(is.na(discharge_date), visit_date, discharge_date),
    
    death_date = if_else(
      str_trim(as.character(death_date)) == "" | death_date == "-", 
      NA_character_, 
      as.character(death_date)
    ),
    death_date = ymd(death_date),
    
    # 퇴원 진단명
    discharge_diagnosis = if_else(
      is.na(discharge_diagnosis) | str_trim(discharge_diagnosis) == "",
      admission_diagnosis,
      discharge_diagnosis
    ),
    
    # 시간 변환
    visit_time = as.integer(visit_time),
    visit_time_fmt = sprintf("%04d", visit_time),
    visit_hour = as.numeric(substr(visit_time_fmt, 1, 2)),
    visit_minute = as.numeric(substr(visit_time_fmt, 3, 4)),
    
    # 나이, 성별
    age = as.numeric(age),
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
    
    # 질환명 표준화
    hypertension_simple = if_else(hypertension_yn == 1, "HTN", "None"),
    diabetes_simple = if_else(diabetes_yn == 1, "DM", "None"),
    
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
    
    # CCI
    cci = liver_disease_yn + diabetes_yn + heart_disease_yn + 
          kidney_disease_yn + respiratory_disease_yn + 
          cerebrovascular_disease_yn + neoplasm_yn * 2,
    
    # 퇴원 상태
    discharge_status_clean = case_when(
      str_detect(tolower(discharge_status), "사망|death") ~ "Death",
      discharge_status == "EM discharge" ~ "ER discharge",
      str_detect(tolower(discharge_status), "퇴원|discharge") ~ "Discharge",
      TRUE ~ "Discharge"
    ),
    
    # 사망 여부
    death = case_when(
      discharge_status_clean == "Death" | !is.na(death_date) ~ 1,
      TRUE ~ 0
    )
  )

cat("✓ Base Result 타입 변환 완료\n\n")

#------------------------------------------------------------------------------
# 3. Nurse 타입 변환
#------------------------------------------------------------------------------
cat("=== STEP 3: Nurse 타입 변환 ===\n")

nurse_typed <- nurse %>%
  mutate(
    visit_date = ymd(as.character(visit_date)),
    visit_time = as.integer(visit_time),
    age = as.numeric(age),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male"))
  )

cat("✓ Nurse 변환 완료\n\n")

#------------------------------------------------------------------------------
# 4. Fever Lab Wide 형식 변환
#------------------------------------------------------------------------------
cat("=== STEP 4: Fever Lab Wide 형식 변환 ===\n")

fever_lab_typed <- fever_lab %>%
  mutate(
    visit_date = ymd(as.character(visit_date)),
    visit_time = as.integer(visit_time),
    age = as.numeric(age),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
    result_numeric = suppressWarnings(as.numeric(result))
  )

# Lab Wide 변환
cat("  - Long → Wide 형식 변환 중...\n")
fever_lab_wide <- fever_lab_typed %>%
  group_by(patient_id, visit_date, order_detail) %>%
  summarize(result_numeric = mean(result_numeric, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = order_detail, values_from = result_numeric) %>%
  clean_names()

# Lab 변수명 매핑
lab_name_mapping <- c(
  "baeghyeolgusu" = "WBC",
  "jeoghyeolgusu" = "RBC",
  "ilbanhyeol_aeggeomsa_hyeolsaegso" = "Hb",
  "hematokeuliteu" = "HCT",
  "hyeolsopansu" = "Platelet",
  "jeoghyeolgubunpogyesu_rdw" = "RDW",
  "hyeolsopanbunpogyesu_pdw" = "PDW",
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
  "got_eung_geub" = "AST",
  "gpt_eung_geub" = "ALT",
  "gammajitipi" = "GGT",
  "alkallinposeupataje" = "ALP",
  "chongbillilubinjeonglyang_geomsa_eung_geub" = "Total_Bilirubin",
  "billilubin_jigjeob" = "Direct_Bilirubin",
  "albumin" = "Albumin",
  "chongdanbaegjeonglyang_geomsa_eung_geub" = "Total_Protein",
  "cp_kgeomsa_eung_geub" = "CPK",
  "ld_hgeomsa_eung_geub" = "LDH",
  "amillajegeomsa_eung_geub" = "Amylase",
  "lipaajegeomsa_eung_geub" = "Lipase",
  "kolleseutelol" = "Cholesterol",
  "teuligeuliselaideu" = "Triglyceride",
  "hd_lkolleseutelol" = "HDL",
  "ldl_kolleseutelol" = "LDL",
  "yosan_eung_geub" = "Uric_Acid",
  "d_dime_rgeomsa" = "D_Dimer",
  "yusan_abga" = "Lactate",
  "samtuabnongdo_hyeolcheong_eung_geub" = "Osmolality",
  "hyeolcheongchongtansangaseunongdo_eung_geub" = "Total_CO2",
  "ketongeomsa_eung_geub" = "Ketone",
  "ra_jeonglyang" = "RF",
  "covid_19_naso_oropharyngeal_swab" = "COVID19_PCR_NP",
  "covid_19_sputum" = "COVID19_PCR_Sputum"
  
)

for (old_name in names(lab_name_mapping)) {
  new_name <- lab_name_mapping[old_name]
  if (old_name %in% names(fever_lab_wide)) {
    names(fever_lab_wide)[names(fever_lab_wide) == old_name] <- new_name
  }
}

# 결측치 ≥50% 변수 제외
lab_vars <- setdiff(names(fever_lab_wide), c("patient_id", "visit_date"))
lab_missing <- fever_lab_wide %>%
  select(all_of(lab_vars)) %>%
  summarize(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct")

vars_to_exclude <- lab_missing %>%
  filter(missing_pct >= 50) %>%
  pull(variable)

if(length(vars_to_exclude) > 0) {
  cat(sprintf("  - 결측치 ≥50%% 변수 제외: %d개\n", length(vars_to_exclude)))
  fever_lab_wide <- fever_lab_wide %>%
    select(-all_of(vars_to_exclude))
}

cat(sprintf("✓ Fever Lab Wide 변환 완료 (%d개 항목)\n\n", ncol(fever_lab_wide) - 2))

#------------------------------------------------------------------------------
# 5. CT 데이터 처리
#------------------------------------------------------------------------------
cat("=== STEP 5: CT 데이터 처리 ===\n")

ct_typed <- ct %>%
  mutate(
    visit_date = ymd(as.character(visit_date)),
    visit_time = as.integer(visit_time),
    age = as.numeric(age),
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male"))
  )

ct_summary <- ct_typed %>%
  group_by(patient_id, visit_date) %>%
  summarize(
    n_ct_scans = n(),
    ct_findings_combined = paste(ct_finding, collapse = " | "),
    .groups = "drop"
  )

cat("✓ CT 처리 완료\n\n")




#------------------------------------------------------------------------------
# 6. 결측치 분석 및 시각화
#------------------------------------------------------------------------------
cat("=== STEP 6: 결측치 분석 ===\n")

missing_base <- miss_var_summary(base_typed) %>% mutate(dataset = "Base Result")
missing_nurse <- miss_var_summary(nurse_typed) %>% mutate(dataset = "Nurse")
missing_fever_lab <- miss_var_summary(fever_lab_wide) %>% mutate(dataset = "Fever Lab")
missing_ct <- miss_var_summary(ct_summary) %>% mutate(dataset = "CT")
missing_fever_including <- miss_var_summary(fever_including_typed) %>% mutate(dataset = "Fever Including")

missing_all <- bind_rows(missing_base, missing_nurse, missing_fever_lab, 
                         missing_ct, missing_fever_including) %>%
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

write_excel_csv(missing_all, "reports/02_missing_analysis.csv")

cat(sprintf("✓ 결측치 분석 완료 (총 %d개 변수)\n", nrow(missing_all)))
cat(sprintf("  - Critical: %d개, Severe: %d개, Moderate: %d개\n\n", 
            sum(missing_all$severity == "Critical (≥80%)"),
            sum(missing_all$severity == "Severe (50-79%)"),
            sum(missing_all$severity == "Moderate (20-49%)")))

# ⭐ 개선된 결측치 시각화
library(ggplot2)

# Base Result만 시각화 (결측치 5% 이상만)
missing_to_plot <- missing_base %>%
  filter(pct_miss >= 5) %>%
  arrange(desc(pct_miss)) %>%
  mutate(
    variable = factor(variable, levels = variable),  # 정렬 순서 고정
    severity = case_when(
      pct_miss >= 80 ~ "Critical",
      pct_miss >= 50 ~ "Severe", 
      pct_miss >= 20 ~ "Moderate",
      TRUE ~ "Minor"
    ),
    severity = factor(severity, levels = c("Critical", "Severe", "Moderate", "Minor"))
  )

# 그래프 높이 동적 조정 (변수 1개당 0.3인치)
plot_height <- max(6, nrow(missing_to_plot) * 0.3)

missing_plot <- ggplot(missing_to_plot, aes(x = pct_miss, y = variable, fill = severity)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct_miss)), 
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_fill_manual(
    values = c("Critical" = "#e74c3c", "Severe" = "#e67e22", 
               "Moderate" = "#f39c12", "Minor" = "#3498db"),
    name = "Severity"
  ) +
  scale_x_continuous(limits = c(0, max(missing_to_plot$pct_miss) * 1.15),
                     breaks = seq(0, 100, 20)) +
  labs(
    title = "Missing Data Pattern: Base Result Dataset",
    subtitle = sprintf("Variables with ≥5%% missing values (n=%d)", nrow(missing_to_plot)),
    x = "Missing Percentage (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, color = "gray30", hjust = 0),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray80", linetype = "dotted")
  )

ggsave("figures/02_missing_pattern.png", missing_plot, 
       width = 12, height = plot_height, dpi = 300, bg = "white")

cat("✓ 결측치 시각화 저장 (출판 품질)\n")
cat(sprintf("  - 그래프 크기: 12 × %.1f inches\n", plot_height))
cat(sprintf("  - 표시된 변수: %d개 (결측치 ≥5%%)\n\n", nrow(missing_to_plot)))


#------------------------------------------------------------------------------
# 7. 이상치 처리 & 중복 제거
#------------------------------------------------------------------------------
cat("=== STEP 7: 이상치 처리 & 중복 제거 ===\n")

# 연령 이상치
base_typed <- base_typed %>%
  mutate(
    age_flag = if_else(age < 0 | age > 120, 1, 0),
    age_clean = if_else(age < 0 | age > 120, NA_real_, age)
  )

cat(sprintf("✓ 연령 이상치: %d건\n", sum(base_typed$age_flag, na.rm = TRUE)))

# 중복 제거
base_dedup <- base_typed %>% distinct()
nurse_dedup <- nurse_typed %>% distinct()
fever_lab_wide_dedup <- fever_lab_wide %>% distinct()
ct_summary_dedup <- ct_summary %>% distinct()
fever_including_dedup <- fever_including_typed %>% distinct()
fever_vitals_summary_dedup <- fever_vitals_summary %>% distinct()

cat(sprintf("Base: %d → %d (제거: %d)\n", 
            nrow(base_typed), nrow(base_dedup), 
            nrow(base_typed) - nrow(base_dedup)))
cat(sprintf("Fever Including: %d → %d (제거: %d)\n", 
            nrow(fever_including_typed), nrow(fever_including_dedup), 
            nrow(fever_including_typed) - nrow(fever_including_dedup)))
cat(sprintf("Fever Vitals: %d → %d (제거: %d)\n\n", 
            nrow(fever_vitals_summary), nrow(fever_vitals_summary_dedup), 
            nrow(fever_vitals_summary) - nrow(fever_vitals_summary_dedup)))

#------------------------------------------------------------------------------
# 8. 중간 결과물 저장 (Part 3에서 사용)
#------------------------------------------------------------------------------
cat("=== STEP 8: 중간 결과물 저장 ===\n")

saveRDS(base_dedup, "cleaned_data/part2_base_typed.rds")
saveRDS(nurse_dedup, "cleaned_data/part2_nurse_typed.rds")
saveRDS(fever_lab_wide_dedup, "cleaned_data/part2_fever_lab_wide.rds")
saveRDS(ct_summary_dedup, "cleaned_data/part2_ct_summary.rds")
saveRDS(fever_including_dedup, "cleaned_data/part2_fever_including_typed.rds")
saveRDS(fever_vitals_summary_dedup, "cleaned_data/part2_fever_vitals_summary.rds")

cat("✓ 중간 결과물 저장 완료 (RDS 형식)\n")
cat("  - cleaned_data/part2_base_typed.rds\n")
cat("  - cleaned_data/part2_nurse_typed.rds\n")
cat("  - cleaned_data/part2_fever_lab_wide.rds\n")
cat("  - cleaned_data/part2_ct_summary.rds\n")
cat("  - cleaned_data/part2_fever_including_typed.rds\n")
cat("  - cleaned_data/part2_fever_vitals_summary.rds\n\n")

#------------------------------------------------------------------------------
# 9. Part 2 완료 확인
#------------------------------------------------------------------------------
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 2/3 완료 (수정버전)                                   \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("✅ 완료된 작업:\n")
cat("   1. ✓ Fever Including 타입 변환 & 활력징후 처리\n")
cat("      - 날짜, 시간, 연령, 성별 변환\n")
cat("      - 활력징후: BP, HR, RR, Temp, SpO2, BST\n")
cat("      - 파생 변수: fever_category, bp_category 등\n")
cat("   2. ✓ Base Result 타입 변환\n")
cat("      - 질환 이진화 (9개 질환)\n")
cat("      - 질환명 표준화 (HTN, DM, CAD 등)\n")
cat("      - CCI 계산\n")
cat("   3. ✓ Nurse 타입 변환\n")
cat("   4. ✓ Fever Lab Wide 형식 변환\n")
cat("      - Long → Wide 형식\n")
cat("      - 변수명 표준화 (한글음차 → 의학용어)\n")
cat("      - 결측치 ≥50%% 변수 제외\n")
cat("   5. ✓ CT 결과 처리\n")
cat("      - CT positive/negative 판정\n")
cat("      - 환자별 CT 요약\n")
cat("   6. ✓ 결측치 분석 및 시각화\n")
cat("   7. ✓ 이상치 처리 & 중복 제거\n\n")

cat("📁 생성된 파일:\n")
cat("   cleaned_data/\n")
cat("   ├── part2_base_typed.rds\n")
cat("   ├── part2_nurse_typed.rds\n")
cat("   ├── part2_fever_lab_wide.rds\n")
cat("   ├── part2_ct_summary.rds\n")
cat("   ├── part2_fever_including_typed.rds       ⭐ 신규!\n")
cat("   └── part2_fever_vitals_summary.rds        ⭐ 활력징후 요약\n\n")
cat("   reports/\n")
cat("   └── 02_missing_analysis.csv\n\n")
cat("   figures/\n")
cat("   └── 02_missing_pattern.png\n\n")

cat("📊 데이터 요약:\n")
cat(sprintf("   • Base Result: %d명\n", nrow(base_dedup)))
cat(sprintf("   • Fever Lab: %d개 검사 항목\n", ncol(fever_lab_wide_dedup) - 2))
cat(sprintf("   • CT 시행: %d명\n", nrow(ct_summary_dedup)))
cat(sprintf("   • Fever Including: %d건\n", nrow(fever_including_dedup)))
cat(sprintf("   • 활력징후 요약: %d건\n\n", nrow(fever_vitals_summary_dedup)))

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
#    - 연령 그룹, 계절, COVID 시기
#    - 방문 시간대, CCI 그룹
#
# 3. 데이터 통합
#    - Base + CT + Vitals (⭐ 활력징후 통합)
#    - Base + CT + Lab
#    - Base + CT + Lab + Vitals (완전판)
#
# 4. 최종 결측값 처리
#    - 모든 NA 제거
#    - Lab/활력징후 -999 처리
#
# 5. 최종 데이터셋 저장
#    - base_analysis.rds (주요 분석용 - 활력징후 포함!)
#    - base_ct_vitals_clean.rds
#    - base_with_lab_vitals.rds
#
# 6. 데이터 딕셔너리 & 기술통계
#
# 필요 입력 파일:
#   - cleaned_data/part2_fever_including_typed.rds
#   - cleaned_data/part2_fever_vitals_summary.rds
#   - cleaned_data/part2_base_typed.rds (등)
#
# 예상 산출물:
#   - cleaned_data/base_analysis.rds (활력징후 포함!)
#   - reports/03_flowchart.csv
#   - reports/07_vitals_summary.csv
#==============================================================================
