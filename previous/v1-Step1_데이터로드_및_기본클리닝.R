# =============================================================================
# Part 1/3: 데이터 로드 및 기본 클리닝 (수정버전)
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: 원본 데이터 로드, 변수명 표준화, 결측치 사전 처리
# 수정: fever_including.xlsx 추가
# 예상 소요: 2-3분
# =============================================================================

#------------------------------------------------------------------------------
# 0. 환경 설정
#------------------------------------------------------------------------------
library(tidyverse)      # 데이터 조작
library(readxl)         # Excel 파일 읽기
library(writexl)        # Excel 파일 쓰기
library(janitor)        # 변수명 클리닝
library(lubridate)      # 날짜 처리

# 작업 디렉토리 설정
setwd("/Users/youjinlee/Library/Mobile Documents/com~apple~CloudDocs/My R/Fever c claude")

# 출력 디렉토리 생성
dir.create("cleaned_data", showWarnings = FALSE)
dir.create("cleaned_data/original_cleaned", showWarnings = FALSE, recursive = TRUE)
dir.create("reports", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 1/3: 데이터 로드 및 기본 클리닝 (수정버전)            \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

#------------------------------------------------------------------------------
# 1. 데이터 로드 & 백업
#------------------------------------------------------------------------------
cat("=== STEP 1: 데이터 로드 ===\n")

# 원본 파일 로드
base_raw <- read_excel("base_result.xlsx")
base_original <- base_raw

nurse_raw <- read_excel("nurse.xlsx")
nurse_original <- nurse_raw

fever_lab_raw <- read_excel("fever_lab.xlsx")
fever_lab_original <- fever_lab_raw

ct_raw <- read_excel("ct.xlsx")
ct_original <- ct_raw

fever_including_raw <- read_excel("fever_including.xlsx")
fever_including_original <- fever_including_raw

# 데이터 크기 확인
nrow(base_raw)
ncol(base_raw)
nrow(nurse_raw)
ncol(nurse_raw)
nrow(fever_lab_raw)
ncol(fever_lab_raw)
nrow(ct_raw)
ncol(ct_raw)
nrow(fever_including_raw)
ncol(fever_including_raw)

dataset_sizes <- tibble(
  Dataset = c("base_raw", "nurse_raw", "fever_lab_raw", "ct_raw", "fever_including_raw"),
  N_Row = c(
    nrow(base_raw),
    nrow(nurse_raw),
    nrow(fever_lab_raw),
    nrow(ct_raw),
    nrow(fever_including_raw)
  ),
  N_Col = c(
    ncol(base_raw),
    ncol(nurse_raw),
    ncol(fever_lab_raw),
    ncol(ct_raw),
    ncol(fever_including_raw)
  )
)

dataset_sizes

# 초기 데이터 크기 리포트
data_size <- tibble(
  Dataset = c("Base Result", "Nurse", "Fever Lab", "CT", "Fever Including"),
  N_Rows = c(nrow(base_raw), nrow(nurse_raw), nrow(fever_lab_raw), 
             nrow(ct_raw), nrow(fever_including_raw)),
  N_Cols = c(ncol(base_raw), ncol(nurse_raw), ncol(fever_lab_raw), 
             ncol(ct_raw), ncol(fever_including_raw)),
  N_Patients = c(
    n_distinct(base_raw$등록번호),
    n_distinct(nurse_raw$등록번호),
    n_distinct(fever_lab_raw$등록번호),
    n_distinct(ct_raw$등록번호),
    n_distinct(fever_including_raw$등록번호)
  )
)

data_size

write_excel_csv(data_size, "reports/00_initial_data_size.csv")
cat(sprintf("✓ 초기 데이터 크기 저장: reports/00_initial_data_size.csv\n\n"))

#------------------------------------------------------------------------------
# 2. 변수명 표준화 (한글 → 영문)
#------------------------------------------------------------------------------
cat("=== STEP 2: 변수명 표준화 ===\n")

# 2.1 Base Result
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
  ) %>%
  mutate(patient_id = as.character(patient_id)) 

cat("✓ Base Result 변수명 변환 완료\n")

# 2.2 Nurse
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
  ) %>%
  mutate(patient_id = as.character(patient_id))  

cat("✓ Nurse 변수명 변환 완료\n")

# 2.3 Fever Lab
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
  )  %>%
  mutate(patient_id = as.character(patient_id))

cat("✓ Fever Lab 변수명 변환 완료\n")

# 2.4 CT
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
  )  %>%
  mutate(patient_id = as.character(patient_id))

cat("✓ CT 변수명 변환 완료\n")

# 2.5 ⭐ Fever Including 
fever_including <- fever_including_raw %>%
  rename(
    patient_id = 등록번호,
    patient_name = 이름,
    sex = 성별,
    age = 나이,
    patient_location = 환자장소,
    insurance_type = 보험종류,
    department = 진료과1,
    visit_date = 내원일자,
    visit_time = 내원시간,
    onset_date = 발병일자1,
    onset_time = 발병시간1,
    discharge_date_raw = 퇴실일자,
    discharge_time_raw = 퇴실시간,
    chief_complaint_1 = 주증상1,
    chief_complaint_code_1 = 주증상코드1,
    chief_complaint_2 = 주증상2,
    chief_complaint_code_2 = 주증상코드2,
    diagnosis = 진단명,
    disease_code_1 = 상병코드1,
    disease_code_2 = 상병코드2,
    visit_reason = 내원사유,
    visit_route = 내원경로,
    referred_hospital_name = 전원온병원명,
    referred_hospital_type = 전원온병원종류,
    arrival_method = 내원수단,
    consciousness_level = 의식상태,
    systolic_bp = 수축기혈압,
    diastolic_bp = 이완기혈압,
    pulse_rate = 맥박수,
    respiratory_rate = 호흡수,
    temperature_type = 체온구분,
    temperature = 체온,
    spo2 = SPO2,
    bst = BST,
    weight = 체중,
    treatment_result = 진료결과,
    admission_dept_doctor = 입원과_진료지정의사,
    ward_room = 병동_병실,
    admission_decision_date = 입원결정일자,
    admission_decision_time = 입원결정시간,
    actual_discharge_date = 실제퇴실일자,
    actual_discharge_time = 실제퇴실시간,
    discharge_dept = 퇴실과,
    discharge_decision_doctor = 퇴실결정의사,
    discharge_decision_doctor_name = 퇴실결정의사이름,
    referred_hospital_code = 전원온병원코드,
    admin_special_note = 원무특기여부,
    stay_duration = 체류시간,
    severe_emergency = 중증응급,
    final_treatment = 최종치료,
    initial_triage = 최초분류,
    final_triage = 최종분류
  ) %>%
  mutate(patient_id = as.character(patient_id))

cat("✓ Fever Including 변수명 변환 완료\n\n")

#------------------------------------------------------------------------------
# 3. 결측치 사전 처리
#------------------------------------------------------------------------------
cat("=== STEP 3: 결측치 사전 처리 ===\n")

# 3.1 Base Result - 질환 관련 빈칸 처리
base_cleaned <- base %>%
  mutate(
    # 질환 관련 변수: 빈칸 → "-" (질환 없음)
    across(c(liver_disease, hypertension, diabetes, heart_disease, 
             kidney_disease, respiratory_disease, surgery, 
             cerebrovascular_disease, neoplasm),
           ~if_else(is.na(.) | str_trim(as.character(.)) == "", "-", as.character(.))),
    
    # DESC 변수도 동일 처리
    across(ends_with("_desc"),
           ~if_else(is.na(.) | str_trim(as.character(.)) == "", "-", as.character(.))),
    
    # discharge_status 빈칸 → 'EM discharge' (응급실 퇴실)
    discharge_status = if_else(
      is.na(discharge_status) | str_trim(as.character(discharge_status)) == "",
      "EM discharge",
      as.character(discharge_status)
    ),
    
    # 진단명 처리: discharge_diagnosis 빈칸 → admission_diagnosis로 채우기
    admission_diagnosis = as.character(admission_diagnosis),
    discharge_diagnosis = if_else(
      is.na(discharge_diagnosis) | str_trim(as.character(discharge_diagnosis)) == "",
      admission_diagnosis,  # 빈칸이면 입원/퇴실시 진단명으로 채움
      as.character(discharge_diagnosis)
    ),
    
    # 날짜 처리: discharge_date 빈칸 → visit_date로 채우기
    discharge_date = if_else(
      is.na(discharge_date) | str_trim(as.character(discharge_date)) == "",
      as.character(visit_date),  # 빈칸이면 내원일자로 채움
      as.character(discharge_date)
    ),
    death_date = as.character(death_date)
  )

cat("✓ Base Result 결측치 처리 완료\n")
cat("  - 질환 관련 변수 빈칸 → '-' (질환 없음)\n")
cat("  - discharge_status 빈칸 → 'EM discharge' (응급실 퇴실)\n")
cat("  - discharge_diagnosis 빈칸 → admission_diagnosis로 채움\n")
cat("  - discharge_date 빈칸 → visit_date로 채움\n")

# 3.2 Nurse - 텍스트 필드 결측치 처리
nurse_cleaned <- nurse %>%
  mutate(
    across(c(procedure, special_note, note),
           ~if_else(is.na(.) | str_trim(as.character(.)) == "", 
                    "Not recorded", 
                    as.character(.)))
  )

cat("✓ Nurse 결측치 처리 완료\n")
cat("  - 텍스트 필드 빈칸 → 'Not recorded'\n")

# 3.3 Fever Lab - 결측치 그대로 유지 (검사 안 함을 의미)
fever_lab_cleaned <- fever_lab %>%
  mutate(
    # 문자형 변수만 처리
    across(where(is.character), ~as.character(.))
  )

cat("✓ Fever Lab 결측치 유지 (검사 미시행 의미)\n")

# 3.4 CT - 결측치 그대로 유지 -판독 소견 추출에 대해서는 'claude 프로텍트' 과정ㅓ쳐야.
ct_cleaned <- ct %>%
  mutate(
    across(where(is.character), ~as.character(.))
  )

cat("✓ CT 결측치 유지\n\n")

#------------------------------------------------------------------------------
# 4. Original Cleaned 버전 저장
#------------------------------------------------------------------------------
cat("=== STEP 4: Original Cleaned 버전 저장 ===\n")

# 4.1 Base Result Cleaned
write_excel_csv(base_cleaned, "cleaned_data/original_cleaned/base_result_cleaned.csv")
write_xlsx(base_cleaned, "cleaned_data/original_cleaned/base_result_cleaned.xlsx")
cat("✓ Base Result Cleaned 저장 완료\n")

# 4.2 Nurse Cleaned
write_excel_csv(nurse_cleaned, "cleaned_data/original_cleaned/nurse_cleaned.csv")
write_xlsx(nurse_cleaned, "cleaned_data/original_cleaned/nurse_cleaned.xlsx")
cat("✓ Nurse Cleaned 저장 완료\n")

# 4.3 Fever Lab Cleaned
write_excel_csv(fever_lab_cleaned, "cleaned_data/original_cleaned/fever_lab_cleaned.csv")
write_xlsx(fever_lab_cleaned, "cleaned_data/original_cleaned/fever_lab_cleaned.xlsx")
cat("✓ Fever Lab Cleaned 저장 완료\n")

# 4.4 CT Cleaned
write_excel_csv(ct_cleaned, "cleaned_data/original_cleaned/ct_cleaned.csv")
write_xlsx(ct_cleaned, "cleaned_data/original_cleaned/ct_cleaned.xlsx")
cat("✓ CT Cleaned 저장 완료\n")

# 4.5 ⭐ Fever Including Cleaned
write_excel_csv(fever_including_cleaned, "cleaned_data/original_cleaned/fever_including_cleaned.csv")
write_xlsx(fever_including_cleaned, "cleaned_data/original_cleaned/fever_including_cleaned.xlsx")
cat("✓ Fever Including Cleaned 저장 완료\n\n")

#------------------------------------------------------------------------------
# 5. 중간 결과물 저장 (Part 2에서 사용)
#------------------------------------------------------------------------------
cat("=== STEP 5: 중간 결과물 저장 ===\n")

saveRDS(base_cleaned, "cleaned_data/part1_base.rds")
saveRDS(nurse_cleaned, "cleaned_data/part1_nurse.rds")
saveRDS(fever_lab_cleaned, "cleaned_data/part1_fever_lab.rds")
saveRDS(ct_cleaned, "cleaned_data/part1_ct.rds")
saveRDS(fever_including_cleaned, "cleaned_data/part1_fever_including.rds")

cat("✓ 중간 결과물 저장 완료 (RDS 형식)\n")
cat("  - cleaned_data/part1_base.rds\n")
cat("  - cleaned_data/part1_nurse.rds\n")
cat("  - cleaned_data/part1_fever_lab.rds\n")
cat("  - cleaned_data/part1_ct.rds\n")
cat("  - cleaned_data/part1_fever_including.rds\n\n")

#------------------------------------------------------------------------------
# 6. Part 1 완료 확인
#------------------------------------------------------------------------------
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 1/3 완료 (수정버전)                                   \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("✅ 완료된 작업:\n")
cat("   1. ✓ 5개 원본 Excel 파일 로드 (fever_including 추가!)\n")
cat("   2. ✓ 변수명 한글 → 영문 표준화\n")
cat("   3. ✓ 결측치 사전 처리\n")
cat("      - 질환 빈칸 → '-' (질환 없음)\n")
cat("      - discharge_status 빈칸 → 'EM discharge'\n")
cat("      - fever_including: 진단명/증상 빈칸 → 'Unknown'\n")
cat("   4. ✓ Original Cleaned 버전 저장 (CSV + Excel)\n")
cat("   5. ✓ 중간 결과물 저장 (RDS)\n\n")

cat("📁 생성된 파일:\n")
cat("   cleaned_data/original_cleaned/\n")
cat("   ├── base_result_cleaned.{csv,xlsx}\n")
cat("   ├── nurse_cleaned.{csv,xlsx}\n")
cat("   ├── fever_lab_cleaned.{csv,xlsx}\n")
cat("   ├── ct_cleaned.{csv,xlsx}\n")
cat("   └── fever_including_cleaned.{csv,xlsx}    ⭐ 신규 추가!\n\n")

cat("📊 Fever Including 데이터 요약:\n")
cat(sprintf("   • 총 방문 기록: %d건\n", nrow(fever_including_cleaned)))
cat(sprintf("   • 환자 수: %d명\n", n_distinct(fever_including_cleaned$patient_id)))
cat(sprintf("   • 포함 변수: 활력징후, 진단명, 증상, 퇴실 정보\n\n"))

# 세션 정보 저장
writeLines(capture.output(sessionInfo()), "reports/01_session_info_part1.txt")

#==============================================================================
# 다음 단계 (Part 2/3)
#==============================================================================
# Part 2에서 수행할 작업:
# 
# 1. Fever Including 데이터 타입 변환
#    - 날짜 변환 (visit_date, onset_date, discharge_date)
#    - 활력징후 숫자형 변환
#    - 범주형 변환 (sex, consciousness_level)
#    - 체온 범주화 (fever 정의)
#
# 2. Fever Including 활력징후 요약
#    - 환자별 첫 방문 활력징후
#    - 혈압, 맥박, 호흡, 체온, SpO2 등
#
# 3. 기존 데이터 타입 변환 (Base, Nurse, Lab, CT)
#
# 필요 입력 파일:
#   - cleaned_data/part1_fever_including.rds
#   - cleaned_data/part1_base.rds (등)
#
# 예상 산출물:
#   - cleaned_data/part2_fever_including_typed.rds
#==============================================================================
