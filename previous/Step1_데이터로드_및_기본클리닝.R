# =============================================================================
# Part 1/3: 데이터 로드 및 기본 클리닝
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: 원본 데이터 로드, 변수명 표준화, 결측치 사전 처리
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
cat("  Part 1/3: 데이터 로드 및 기본 클리닝                        \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

#------------------------------------------------------------------------------
# 1. 데이터 로드 & 백업
#------------------------------------------------------------------------------
cat("=== STEP 1: 데이터 로드 ===\n")

# 원본 파일 로드
base_raw <- read_excel("base_result_s.xlsx")
base_original <- base_raw

nurse_raw <- read_excel("nurse_s.xlsx")
nurse_original <- nurse_raw

fever_lab_raw <- read_excel("fever_lab_s.xlsx")
fever_lab_original <- fever_lab_raw

ct_raw <- read_excel("ct_s.xlsx")
ct_original <- ct_raw

# 데이터 크기 확인
cat(sprintf("✓ Base Result: %d rows, %d columns\n", nrow(base_raw), ncol(base_raw)))
cat(sprintf("✓ Nurse: %d rows, %d columns\n", nrow(nurse_raw), ncol(nurse_raw)))
cat(sprintf("✓ Fever Lab: %d rows, %d columns\n", nrow(fever_lab_raw), ncol(fever_lab_raw)))
cat(sprintf("✓ CT: %d rows, %d columns\n", nrow(ct_raw), ncol(ct_raw)))

# 초기 데이터 크기 리포트
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
  )

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
  )

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
  )

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
  )

cat("✓ CT 변수명 변환 완료\n\n")

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
    
    # discharge_status 빈칸 → 'EM discharge'
    discharge_status = if_else(
      is.na(discharge_status) | str_trim(as.character(discharge_status)) == "",
      "EM discharge",
      as.character(discharge_status)
    ),
    
    # 진단명 처리
    admission_diagnosis = as.character(admission_diagnosis),
    discharge_diagnosis = as.character(discharge_diagnosis),
    
    # 날짜 처리
    discharge_date = as.character(discharge_date),
    death_date = as.character(death_date)
  )

cat("✓ Base Result 결측치 처리 완료\n")
cat("  - 질환 관련 변수 빈칸 → '-' (질환 없음)\n")
cat("  - discharge_status 빈칸 → 'EM discharge'\n")

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

# 3.4 CT - 결측치 그대로 유지
ct_cleaned <- ct %>%
  mutate(
    across(where(is.character), ~as.character(.))
  )

cat("✓ CT 결측치 유지\n\n")

#------------------------------------------------------------------------------
# 4. Original Cleaned 버전 저장
#------------------------------------------------------------------------------
cat("=== STEP 4: Original Cleaned 버전 저장 ===\n")

# 4.1 Base Result Cleaned 저장
write_csv(base_cleaned, "cleaned_data/original_cleaned/base_result_cleaned.csv")
write_xlsx(base_cleaned, "cleaned_data/original_cleaned/base_result_cleaned.xlsx")
cat("✓ Base Result Cleaned 저장 완료\n")

# 4.2 Nurse Cleaned 저장
write_csv(nurse_cleaned, "cleaned_data/original_cleaned/nurse_cleaned.csv")
write_xlsx(nurse_cleaned, "cleaned_data/original_cleaned/nurse_cleaned.xlsx")
cat("✓ Nurse Cleaned 저장 완료\n")

# 4.3 Fever Lab Cleaned 저장
write_csv(fever_lab_cleaned, "cleaned_data/original_cleaned/fever_lab_cleaned.csv")
write_xlsx(fever_lab_cleaned, "cleaned_data/original_cleaned/fever_lab_cleaned.xlsx")
cat("✓ Fever Lab Cleaned 저장 완료\n")

# 4.4 CT Cleaned 저장
write_csv(ct_cleaned, "cleaned_data/original_cleaned/ct_cleaned.csv")
write_xlsx(ct_cleaned, "cleaned_data/original_cleaned/ct_cleaned.xlsx")
cat("✓ CT Cleaned 저장 완료\n\n")

#------------------------------------------------------------------------------
# 5. 중간 결과물 저장 (Part 2에서 사용)
#------------------------------------------------------------------------------
cat("=== STEP 5: 중간 결과물 저장 ===\n")

# RDS 형식으로 저장 (데이터 타입 보존)
saveRDS(base_cleaned, "cleaned_data/part1_base.rds")
saveRDS(nurse_cleaned, "cleaned_data/part1_nurse.rds")
saveRDS(fever_lab_cleaned, "cleaned_data/part1_fever_lab.rds")
saveRDS(ct_cleaned, "cleaned_data/part1_ct.rds")

cat("✓ 중간 결과물 저장 완료 (RDS 형식)\n")
cat("  - cleaned_data/part1_base.rds\n")
cat("  - cleaned_data/part1_nurse.rds\n")
cat("  - cleaned_data/part1_fever_lab.rds\n")
cat("  - cleaned_data/part1_ct.rds\n\n")

#------------------------------------------------------------------------------
# 6. Part 1 완료 확인
#------------------------------------------------------------------------------
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 1/3 완료                                              \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("✅ 완료된 작업:\n")
cat("   1. ✓ 4개 원본 Excel 파일 로드\n")
cat("   2. ✓ 변수명 한글 → 영문 표준화\n")
cat("   3. ✓ 결측치 사전 처리\n")
cat("      - 질환 빈칸 → '-' (질환 없음)\n")
cat("      - discharge_status 빈칸 → 'EM discharge'\n")
cat("   4. ✓ Original Cleaned 버전 저장 (CSV + Excel)\n")
cat("   5. ✓ 중간 결과물 저장 (RDS)\n\n")

cat("📁 생성된 파일:\n")
cat("   cleaned_data/original_cleaned/\n")
cat("   ├── base_result_cleaned.{csv,xlsx}    # 원본 구조 유지 (질환 빈칸 처리)\n")
cat("   ├── nurse_cleaned.{csv,xlsx}          # 원본 구조 유지\n")
cat("   ├── fever_lab_cleaned.{csv,xlsx}      # 원본 구조 유지\n")
cat("   └── ct_cleaned.{csv,xlsx}             # 원본 구조 유지\n\n")
cat("   cleaned_data/\n")
cat("   ├── part1_base.rds                    # Part 2 입력 파일\n")
cat("   ├── part1_nurse.rds\n")
cat("   ├── part1_fever_lab.rds\n")
cat("   └── part1_ct.rds\n\n")

cat("📊 데이터 요약:\n")
cat(sprintf("   • Base Result: %d명 환자\n", n_distinct(base_cleaned$patient_id)))
cat(sprintf("   • Nurse: %d건 기록\n", nrow(nurse_cleaned)))
cat(sprintf("   • Fever Lab: %d건 검사\n", nrow(fever_lab_cleaned)))
cat(sprintf("   • CT: %d건 스캔\n", nrow(ct_cleaned)))
cat("\n")

# 세션 정보 저장
writeLines(capture.output(sessionInfo()), "reports/01_session_info_part1.txt")

#==============================================================================
# 다음 단계 (Part 2/3)
#==============================================================================
# Part 2에서 수행할 작업:
# 
# 1. 데이터 타입 변환
#    - 날짜 변환 (visit_date, discharge_date, death_date)
#    - 숫자형 변환 (age, visit_time)
#    - 범주형 변환 (sex, discharge_status)
#    - 질환 이진화 (liver_disease_yn, hypertension_yn 등)
#    - 질환명 표준화 (HTN, DM, CAD, CKD 등)
#
# 2. Fever Lab Wide 형식 변환
#    - Long → Wide 형식 (검사항목별 컬럼)
#    - 변수명 표준화 (한글음차 → 의학용어)
#    - 결측치 ≥50% 변수 제외
#
# 3. CT 결과 처리
#    - CT positive/negative 판정
#    - 환자별 CT 요약 (n_ct_scans, ct_any_positive)
#
# 4. 결측치 분석
#    - 변수별 결측치 비율 계산
#    - Severity 분류 (Critical/Severe/Moderate/Minor)
#    - 결측치 패턴 시각화
#
# 5. 이상치 처리 & 중복 제거
#    - 연령 이상치 확인
#    - 중복 행 제거
#
# 필요 입력 파일:
#   - cleaned_data/part1_base.rds
#   - cleaned_data/part1_nurse.rds
#   - cleaned_data/part1_fever_lab.rds
#   - cleaned_data/part1_ct.rds
#
# 예상 산출물:
#   - cleaned_data/part2_base_typed.rds
#   - cleaned_data/part2_fever_lab_wide.rds
#   - cleaned_data/part2_ct_summary.rds
#   - reports/02_missing_analysis.csv
#   - figures/02_missing_pattern.png
#==============================================================================
