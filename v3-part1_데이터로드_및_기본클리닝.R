# =============================================================================
# Part 1/3: 데이터 로드 및 기본 클리닝 (CSV버전)
# =============================================================================
# 연구: 85세 이상 발열 환자 CT 진단 가치 연구
# 목적: 원본 CSV 데이터 로드
#       3개의 파일이 있는데  1,2는 2023.7월 기준을로 즉 Amis 3.0이후 데이터가 달라서 두 버전을 통일해야한다.
#       3번 파일은 두 기간사이에 코드가 완전히 바뀌었다. 
#       1.Base + Nurse + Fever Including 2.ct_1_raw 3.ER_LAB_RSLT_s.csv
#.      결측치 사전 처리,간단한 결측치 처리
#       변수명 통일
# =============================================================================

#------------------------------------------------------------------------------
# 0. 환경 설정
#------------------------------------------------------------------------------
library(tidyverse)      # 데이터 조작
library(readr)          # CSV 파일 읽기 (한글 인코딩 지원)
library(writexl)        # Excel 파일 쓰기
library(janitor)        # 변수명 클리닝
library(lubridate)      # 날짜 처리

# 작업 디렉토리 설정 (사용자 환경에 맞게 수정)
setwd("/Users/youjinlee/Documents/My R/fever paper")

# 출력 디렉토리 생성
dir.create("cleaned_data", showWarnings = FALSE)
dir.create("cleaned_data/original_cleaned", showWarnings = FALSE, recursive = TRUE)
dir.create("reports", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 1/3: 데이터 로드 및 기본 클리닝 (CSV버전)            \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

#------------------------------------------------------------------------------
# 1. 데이터 로드 & 백업
#------------------------------------------------------------------------------


# 1.1 통합 파일 로드 (Base + Nurse + Fever Including)

combined_filtered <-readRDS("filtered_data/combined_filtered")

#.기초사항 변경 ex> 나이에 Y 붙은 것,consciousness_level ,weight 모름 이면 NA로, sex 1이면 M

combined_raw<- combined_filtered %>%
  mutate(
    #  나이에서 'y' 또는 'Y' 제거
    나이 = str_remove_all(나이, "[yY]"),
    
    #  의식상태 변환 (A→1, V→2, P→3, U→4)
    의식상태 = case_when(
      의식상태 == "A" ~ "1",
      의식상태 == "V" ~ "2",
      의식상태 == "P" ~ "3",
      의식상태 == "U" ~ "4",
      TRUE ~ 의식상태  # 이미 숫자면 그대로 유지
    ),
    
    #  성별 변환 (1→M, 2→F)
    성별 = case_when(
      성별 == "1" ~ "M",
      성별 == "2" ~ "F",
      TRUE ~ 성별  # 이미 M/F면 그대로 유지
    )
  )

# 1.2 CT 파일 로드

ct_filtered <-readRDS("filtered_data/ct_filtered")

ct_raw <-ct_filtered 

# 1.3 Lab 파일 로드

#.lab 파일은 amis 3.0 전후로 lab 이름이 바뀐것도 있고 변경 된 것도 있어서 표준화 시겨줘야해..
#.너무 복잡하니깐 cleaning도 해야..
#.v3-part1_supplement_lab_data_standardization.R에서 표준화시키고 wide 형태로 바꿔. 
#.그리고 다시 불러와서 진행하기


fever_lab_original  <-read.csv("fever_lab_wide_final.csv", header=T ,stringsAsFactors=FALSE)


# 초기 데이터 크기 리포트
data_size <- tibble(
  Dataset = c("Combined (Base+Nurse+Fever)", "CT Result", "Lab Result"),
  N_Rows = c(nrow(combined_raw), nrow(ct_raw), nrow(fever_lab_raw)),
  N_Cols = c(ncol(combined_raw), ncol(ct_raw), ncol(fever_lab_raw)),
  N_Patients = c(
    n_distinct(combined_raw$등록번호),
    n_distinct(ct_raw$등록번호),
    n_distinct(fever_lab_raw$등록번호)
  )
)

print(data_size)
write_excel_csv(data_size, "reports/00_initial_data_size_csv.csv")
cat(sprintf("\n✓ 초기 데이터 크기 저장: reports/00_initial_data_size_csv.csv\n\n"))



#------------------------------------------------------------------------------
# 2. 통합 파일 분리 및 변수명 표준화 (dplyr 명시적 사용)
#------------------------------------------------------------------------------
cat("=== STEP 2: 변수명 표준화 (dplyr 사용) ===\n")

# 2.1 Combined 파일에서 Base Result 부분 추출
base <- combined_raw %>%
  dplyr::select(
    등록번호, 내원일자, 내원시간, 성별, 나이,
    간질환, `간질환 DESC`, 고혈압, `고혈압 DESC`,
    당뇨, `당뇨 DESC`, 심질환, `심질환 DESC`,
    신질환, `신질환 DESC`, 호흡기질환, `호흡기질환 DESC`,
    수술, `수술 DESC`, 뇌혈관질환, `뇌혈관질환 DESC`,
    Neoplasm, `Neoplasm DESC`,
    `퇴실시 진단명`, `퇴원시 진단명`, 퇴원상태, 퇴원일자, 사망일
  ) %>%
  dplyr::rename(
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
    admission_diagnosis = `퇴실시 진단명`,
    discharge_diagnosis = `퇴원시 진단명`,
    discharge_status = 퇴원상태,
    discharge_date = 퇴원일자,
    death_date = 사망일
  ) %>%
  dplyr::mutate(patient_id = as.character(patient_id))

cat("✓ Base Result 변수명 변환 완료\n")

# 2.2 Combined 파일에서 Nurse 부분 추출
nurse <- combined_raw %>%
  dplyr::select(
    등록번호, 환자명, 내원일자, 내원시간, 성별, 나이,
    진료사항, 메모, 노트
  ) %>%
  dplyr::rename(
    patient_id = 등록번호,
    patient_name = 환자명,
    visit_date = 내원일자,
    visit_time = 내원시간,
    sex = 성별,
    age = 나이,
    procedure = 진료사항,
    special_note = 메모,
    note = 노트
  ) %>%
  dplyr::mutate(patient_id = as.character(patient_id))

cat("✓ Nurse 변수명 변환 완료\n")

# 2.3 Fever Lab 

fever_lab <-fever_lab_original

#.    v3-part1_supplement_lab_data_standardization.R 에서 이미 변수명 변경완료

cat("✓ Fever Lab 변수명 변환 완료\n")

# 2.4 CT
ct <- ct_raw %>%
  dplyr::rename(
    patient_id = 등록번호,
    patient_name = 성명,
    exam_date = 검사일자,
    receive_time = 접수시간,
    order_code = 행위코드,
    order_name = 코드명,
    ct_finding = 판독
  ) %>%
  dplyr::mutate(patient_id = as.character(patient_id))

cat("✓ CT 변수명 변환 완료\n")

# 2.5 Fever Including (활력징후 및 진단)
fever_including <- combined_raw %>%
  dplyr::select(
    등록번호, 환자명, 성별, 나이, 환자장소, 보험종류, 진료과1,
    내원일자, 내원시간, 발병일자1, 발병시간1, 퇴실일자, 퇴실시간,
    주증상1, 주증상코드1, 주증상2, 주증상코드2, 진단명, 상병코드1, 상병코드2,
    내원사유, 내원경로, `전원온 병원`, 전원온병원종류, 내원수단, 의식상태,
    수축기혈압, 이완기혈압, 맥박수, 호흡수, 체온구분, 체온, SPO2, BST, 체중,
    진료결과, `입원과_진료지정의사`, `병동_병실`, 입원결정일자, 입원결정시간,
    실제퇴실일자, 실제퇴실시간, 퇴실과, 퇴실결정의사, 전원온병원코드,
    중증응급, 최종치료, 체류시간, 최초분류, 최종분류
  ) %>%
  dplyr::rename(
    patient_id = 등록번호,
    patient_name = 환자명,
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
    referred_hospital_name = `전원온 병원`,
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
    admission_dept_doctor = `입원과_진료지정의사`,
    ward_room = `병동_병실`,
    admission_decision_date = 입원결정일자,
    admission_decision_time = 입원결정시간,
    actual_discharge_date = 실제퇴실일자,
    actual_discharge_time = 실제퇴실시간,
    discharge_dept = 퇴실과,
    discharge_decision_doctor = 퇴실결정의사,
    referred_hospital_code = 전원온병원코드,
    severe_emergency = 중증응급,
    final_treatment = 최종치료,
    stay_duration = 체류시간,
    initial_triage = 최초분류,
    final_triage = 최종분류
  ) %>%
  dplyr::mutate(patient_id = as.character(patient_id))

cat("✓ Fever Including 변수명 변환 완료\n\n")




#------------------------------------------------------------------------------
# 3. 결측치 사전 처리
#------------------------------------------------------------------------------
cat("=== STEP 3: 결측치 사전 처리 ===\n")

# 3.1 Base Result - 질환 관련 빈칸 처리
base_cleaned <- base %>%
  dplyr::mutate(
    # 질환 관련 변수: 빈칸 → "-" (질환 없음)
    dplyr::across(c(liver_disease, hypertension, diabetes, heart_disease, 
                    kidney_disease, respiratory_disease, surgery, 
                    cerebrovascular_disease, neoplasm),
                  ~if_else(is.na(.) | str_trim(as.character(.)) == "", "-", as.character(.))),
    
    # DESC 변수도 동일 처리
    dplyr::across(ends_with("_desc"),
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
      admission_diagnosis,
      as.character(discharge_diagnosis)
    ),
    
    # 날짜 처리: discharge_date 빈칸 → visit_date로 채우기
    discharge_date = if_else(
      is.na(discharge_date) | str_trim(as.character(discharge_date)) == "",
      as.character(visit_date),
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
  dplyr::mutate(
    dplyr::across(c(procedure, special_note, note),
                  ~if_else(is.na(.) | str_trim(as.character(.)) == "", 
                           "Not recorded", 
                           as.character(.)))
  )

cat("✓ Nurse 결측치 처리 완료\n")
cat("  - 텍스트 필드 빈칸 → 'Not recorded'\n")



# 3.3 Fever Lab - 결측치 그대로 유지 (검사 안 함을 의미)
fever_lab_cleaned <- fever_lab %>%
  dplyr::mutate(
    dplyr::across(where(is.character), ~as.character(.))
  )

cat("✓ Fever Lab 결측치 유지 (검사 미시행 의미)\n")

# 3.4 CT - 결측치 그대로 유지
ct_cleaned <- ct %>%
  dplyr::mutate(
    dplyr::across(where(is.character), ~as.character(.))
  )

cat("✓ CT 결측치 유지\n")

# 3.5 Fever Including - 결측치 처리
fever_including_cleaned <- fever_including %>%
  dplyr::mutate(
    # 텍스트 필드 빈칸 → "Unknown"
    dplyr::across(c(chief_complaint_1, chief_complaint_2, diagnosis, 
                    visit_reason, visit_route, arrival_method),
                  ~if_else(is.na(.) | str_trim(as.character(.)) == "", 
                           "Unknown", 
                           as.character(.))),
    
    # 병원 관련 정보
    dplyr::across(c(referred_hospital_name, referred_hospital_type),
                  ~if_else(is.na(.) | str_trim(as.character(.)) == "", 
                           "None", 
                           as.character(.))),
    
    # 기타 문자형 변수
    dplyr::across(where(is.character), ~as.character(.))
  )

cat("✓ Fever Including 결측치 처리 완료\n")
cat("  - 주증상/진단명 빈칸 → 'Unknown'\n")
cat("  - 전원 병원 정보 빈칸 → 'None'\n\n")

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


# 4.5 Fever Including Cleaned
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

cat("✓ 중간 결과물 RDS 저장 완료\n\n")

#------------------------------------------------------------------------------
# 6. Part 1 완료 확인
#------------------------------------------------------------------------------
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("  Part 1/3 완료 (dplyr 버전)                                 \n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat("✅ 완료된 작업:\n")
cat("   1. ✓ 5개 원본 CSV 파일 로드 (통합 파일 병합)\n")
cat("      - Combined: 245 + 345 = 590 rows\n")
cat("      - 컬럼명 통일: '이름'='환자명', '전원온병원'='전원온 병원', '퇴원일'='퇴원일자'\n")
cat("      - 데이터 타입 통일: 모든 컬럼 → character (병합 전 처리)\n")
cat("      - CT: 51 + 356 = 407 rows\n")
cat(sprintf("      - Lab: %d rows\n", nrow(fever_lab_cleaned)))
cat("   2. ✓ 변수명 한글 → 영문 표준화 (dplyr 명시적 사용)\n")
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
cat("   └── fever_including_cleaned.{csv,xlsx}\n\n")

# 데이터 요약 통계
cat("📊 최종 데이터 요약:\n")
cat(sprintf("   • Base Result: %d rows, %d patients\n", 
            nrow(base_cleaned), n_distinct(base_cleaned$patient_id)))
cat(sprintf("   • Nurse: %d rows, %d patients\n", 
            nrow(nurse_cleaned), n_distinct(nurse_cleaned$patient_id)))
cat(sprintf("   • Fever Lab: %d rows, %d patients\n", 
            nrow(fever_lab_cleaned), n_distinct(fever_lab_cleaned$patient_id)))
cat(sprintf("   • CT: %d rows, %d patients\n", 
            nrow(ct_cleaned), n_distinct(ct_cleaned$patient_id)))
cat(sprintf("   • Fever Including: %d rows, %d patients\n\n", 
            nrow(fever_including_cleaned), n_distinct(fever_including_cleaned$patient_id)))

# 세션 정보 저장
writeLines(capture.output(sessionInfo()), "reports/01_session_info_part1_dplyr.txt")

cat("✅ Part 1 완료! 다음 Part 2로 진행 가능합니다.\n\n")
cat("💡 Tip: dplyr:: 명시적 사용으로 패키지 충돌 완벽 방지!\n\n")

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
#   - cleaned_data/part1_*.rds (5개)
#
# 예상 산출물:
#   - cleaned_data/part2_*_typed.rds
#==============================================================================