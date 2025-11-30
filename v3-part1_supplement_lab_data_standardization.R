#!/usr/bin/env Rscript
#==============================================================================
# ER Lab 데이터 전처리 - 
#       2023.7월 기준을로 즉 Amis 3.0이후 데이터가 달라서 두 버전을 통일해야한다.
#       3번 파일은 두 기간사이에 코드가 완전히 바뀌었다.
# 목적: ER_LAB_RSLT_s.csv를 Wide 형식으로 변환 및 정리
# 작성일: 2025
#==============================================================================

# 0. 패키지 로드 ===============================================================
# 필요한 패키지 설치 (처음 실행 시)
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readr")) install.packages("readr")
if (!require("stringr")) install.packages("stringr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("openxlsx")) install.packages("openxlsx")

library(tidyverse)
library(readr)
library(stringr)
library(lubridate)
library(openxlsx)

cat("\n========================================\n")
cat("ER Lab 데이터 전처리 시작\n")
cat("========================================\n\n")

# ==============================================================================
# PART 1: 데이터 로드 및 표준화
# ==============================================================================

# 1.1 매핑 테이블 생성 =========================================================
create_manual_mapping <- function() {
  # 검사명 표준화를 위한 매핑 테이블
  manual_mapping <- tribble(
    ~original_name, ~standardized_name,
    
    # (1.0) 접두사 제거
    "(1.0)APTT(Normal Control)", "APTT_Normal_Control",
    "(1.0)PT(INR)", "PT_INR",
    "(1.0)PT(%)", "PT_percent",
    "(1.0)PCT", "PCT",
    "(1.0)Final Report", "Final_Report",
    "(1.0)Final report", "Final_Report",
    
    # CBC 관련
    "WBC (Qn)[ChemR-I],Blood", "WBC",
    "RBC (Qn)[ChemR-I],Blood", "RBC",
    "Hb (Qn)[ChemR-I],Blood", "Hb",
    "Hct (Qn)[ChemR-I],Blood", "Hct",
    "Platelet (Qn)[ChemR-I],Blood", "Platelet",
    "Neutrophil (Qn)[ChemR-I],Blood", "Neutrophil",
    "Lymphocyte (Qn)[ChemR-I],Blood", "Lymphocyte",
    "Monocyte (Qn)[ChemR-I],Blood", "Monocyte",
    "Eosinophil (Qn)[ChemR-I],Blood", "Eosinophil",
    "Basophil (Qn)[ChemR-I],Blood", "Basophil",
    
    # 전해질
    "Sodium (Qn)[EM],Blood", "Sodium",
    "Potassium (Qn)[EM],Blood", "Potassium",
    "Chloride (Qn)[EM],Blood", "Chloride",
    "Total calcium (Qn)[ChemR-I],Blood", "Total_calcium",
    "Phosphorus (Qn)[ChemR-I],Blood", "Phosphorus",
    "Magnesium (Qn)[ChemR-I],Blood", "Magnesium",
    
    # 간기능
    "AST(SGOT) (Qn)[ChemR-I],Blood", "AST",
    "ALT(SGPT) (Qn)[ChemR-I],Blood", "ALT",
    "Total bilirubin (Qn)[ChemR-I],Blood", "Total_bilirubin",
    "Direct bilirubin (Qn)[ChemR-I],Blood", "Direct_bilirubin",
    "Albumin (Qn)[ChemR-I],Blood", "Albumin",
    "Total protein (Qn)[ChemR-I],Blood", "Total_protein",
    "γ-GT (Qn)[ChemR-I],Blood", "GGT",
    "Alkaline phosphatase (Qn)[ChemR-I],Blood", "ALP",
    
    # 신기능
    "BUN (Qn)[ChemR-I],Blood", "BUN",
    "Creatinine (Qn)[ChemR-I],Blood", "Creatinine",
    "eGFR(CKD-EPI) (Qn),Blood", "eGFR_CKD_EPI",
    "eGFR(MDRD) (Qn),Blood", "eGFR_MDRD",
    
    # 염증 마커
    "CRP (Qn),Blood", "CRP",
    "Procalcitonin (Qn),Blood", "PCT",
    "ESR (Qn)[ESR],Blood", "ESR",
    
    # 응고 검사
    "PT(%) (Qn)[ChemR-I],Blood", "PT_percent",
    "PT(INR) (Qn)[ChemR-I],Blood", "PT_INR",
    "aPTT (Qn)[ChemR-I],Blood", "aPTT",
    "Fibrinogen (Qn)[ChemR-I],Blood", "Fibrinogen",
    "D-dimer (Qn)[ChemR-I],Blood", "D_dimer",
    
    # 기타 주요 검사
    "Glucose (Qn)[ChemR-I],Blood", "Glucose",
    "Amylase (Qn)[ChemR-I],Blood", "Amylase",
    "Lipase (Qn)[ChemR-I],Blood", "Lipase",
    "LDH (Qn)[ChemR-I],Blood", "LDH",
    "CPK (Qn)[ChemR-I],Blood", "CPK",
    "Troponin I (Qn)[ChemR-I],Blood", "Troponin_I",
    "BNP (Qn)[ChemR-I],Blood", "BNP",
    "NT-proBNP (Qn)[ChemR-I],Blood", "NT_proBNP",
    "HbA1c (Qn)[ChemR-I],Blood", "HbA1c",
    "Lactate (Qn)[ChemR-I],Blood", "Lactate",
    
    # 요분석
    "Urine WBC (Qn),Urine", "Urine_WBC",
    "Urine RBC (Qn),Urine", "Urine_RBC",
    "Urine protein (Qn),Urine", "Urine_protein",
    "Urine glucose (Qn),Urine", "Urine_glucose"
  )
  
  return(manual_mapping)
}

# 1.2 자동 표준화 함수 =========================================================
standardize_detail_name <- function(detail_name) {
  # NA 처리
  if (is.na(detail_name)) {
    return(NA_character_)
  }
  
  # 문자열로 변환
  detail_name <- as.character(detail_name)
  
  # 패턴 기반 정리
  # (1.0) 및 (숫자.숫자) prefix 제거
  detail_name <- str_replace(detail_name, "^\\(\\d+\\.\\d+\\)", "")
  
  # [ChemR-I], [ChemR-II] 등 제거
  detail_name <- str_replace_all(detail_name, "\\[ChemR-[IV]+\\]", "")
  
  # [EM], [ESR] 등 방법명 제거
  detail_name <- str_replace_all(detail_name, "\\[\\w+\\]", "")
  
  # (Qn), (Semi-Qn) 정량 표시 제거
  detail_name <- str_replace_all(detail_name, "\\s*\\(Qn\\)", "")
  detail_name <- str_replace_all(detail_name, "\\s*\\(Semi-Qn\\)", "")
  
  # 끝의 specimen 정보 제거
  detail_name <- str_replace(detail_name, ",\\s*(Blood|Serum|Plasma|Urine|CSF|Body fluid).*$", "")
  
  # 일반 약어 표준화
  detail_name <- str_replace(detail_name, "AST\\(SGOT\\)", "AST")
  detail_name <- str_replace(detail_name, "ALT\\(SGPT\\)", "ALT")
  
  # 중복 공백 제거 및 trim
  detail_name <- str_squish(detail_name)
  
  # 특수문자를 언더스코어로 변환 (변수명으로 사용하기 위해)
  detail_name <- str_replace_all(detail_name, "[\\s\\-\\(\\)\\[\\]\\{\\}\\+\\%\\/]", "_")
  detail_name <- str_replace_all(detail_name, "__+", "_")  # 연속된 언더스코어 제거
  detail_name <- str_replace(detail_name, "^_|_$", "")  # 시작/끝 언더스코어 제거
  
  return(detail_name)
}
# ==============================================================================
# 1.3 데이터 로드 및 전처리 ====================================================
# ==============================================================================
cat("1. 데이터 로딩 중...\n")

# 파일 경로 설정 (필요에 따라 수정)
setwd("Users/youjinlee/Documents/My R/fever paper/2017_2025_s")

# 파일 경로 설정
input_file <- "ER_LAB_RSLT_s.csv"  

# 데이터 읽기
fever_lab_original <- read_csv(
  input_file,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# BOM 제거 (첫 번째 컬럼명)
if (str_detect(names(fever_lab_original)[1], "^[\\ufeff]")) {
  names(fever_lab_original)[1] <- str_replace(names(fever_lab_original)[1], "^[\\ufeff]", "")
}

# 컬럼명 확인
cat("컬럼 구조 확인:\n")
cat(sprintf("  %s\n\n", paste(names(fever_lab_original), collapse = ", ")))

fever_lab <- fever_lab_original %>%
  rename(
    patient_id = 등록번호,
    visit_date = 내원일자,
    visit_time = 내원시간,
    patient_name = 환자명,
    sex = 성별,
    age = 내원당시나이,
    order_code = 처방코드,
    order_name = 처방명,
    detail_code= 세부검사코드, 
    detail_name = 검사명,
    result = 결과
  )

# ==============================================================================
# PART 2: Blood Culture 처리 및 15% 필터링
# ==============================================================================

cat("2. Blood Culture 처리 중...\n")

# Blood culture 행 식별 및 처리
# 2023년 6월까지
blood_culture_rows_pre2023 <- which(
  !is.na(fever_lab$visit_date) &
    fever_lab$visit_date <= ymd("2023-06-30") & 
    !is.na(fever_lab$order_name) &
    fever_lab$order_name == "(1.0)SMEAR GRAM STAIN, BLOOD"
)

# 2023년 7월부터
blood_culture_rows_post2023 <- which(
  !is.na(fever_lab$visit_date) &
    fever_lab$visit_date >= ymd("2023-07-01") & 
    !is.na(fever_lab$detail_code) &
    fever_lab$detail_code == "ABACT"
)

all_blood_culture_rows <- c(blood_culture_rows_pre2023, blood_culture_rows_post2023)

# Blood culture 처리
if (length(all_blood_culture_rows) > 0) {
  # detail_name을 Blood_culture로 변경
  fever_lab$detail_name[all_blood_culture_rows] <- "Blood_culture"
  
  # result를 G(+), G(-), no growth로 변경
  for (i in all_blood_culture_rows) {
    result_val <- fever_lab$result[i]
    
    if (is.na(result_val)) {
      fever_lab$result[i] <- NA_character_
    } else {
      result_lower <- tolower(as.character(result_val))
      
      if (grepl("g\\+|g\\(\\+\\)|gram.*positive|그람.*양성|cocci", result_lower)) {
        fever_lab$result[i] <- "G(+)"
      } else if (grepl("g\\-|g\\(\\-\\)|gram.*negative|그람.*음성|rods", result_lower)) {
        fever_lab$result[i] <- "G(-)"
      } else if (grepl("no.*growth|negative|음성|n\\.g|없음|not.*detected", result_lower)) {
        fever_lab$result[i] <- "no growth"
      } else if (grepl("positive|양성|detected|검출", result_lower)) {
        fever_lab$result[i] <- "G(+)"
      } else {
        # 기본값은 no growth
        fever_lab$result[i] <- "no growth"
      }
    }
  }
}

cat(sprintf("  - Blood culture로 변환된 레코드: %d개\n", length(all_blood_culture_rows)))
cat(sprintf("    • 2023년 6월까지: %d개\n", length(blood_culture_rows_pre2023)))
cat(sprintf("    • 2023년 7월부터: %d개\n", length(blood_culture_rows_post2023)))

# Blood culture 결과 분포 확인
if (length(all_blood_culture_rows) > 0) {
  bc_summary <- fever_lab %>%
    filter(detail_name == "Blood_culture") %>%
    count(result) %>%
    arrange(desc(n))
  cat("\n  Blood culture 결과 분포:\n")
  print(bc_summary)
}



# 기존 검사명 표준화 (Blood_culture는 제외)
cat("\n3. 검사명 표준화 중...\n")

manual_mapping <- create_manual_mapping()

# Blood_culture를 제외한 검사명만 표준화
unique_test_names <- fever_lab %>%
  filter(detail_name != "Blood_culture") %>%
  distinct(detail_name) %>%
  filter(!is.na(detail_name))

mapping_table <- unique_test_names %>%
  mutate(
    standardized_manual = manual_mapping$standardized_name[
      match(detail_name, manual_mapping$original_name)
    ],
    standardized_auto = map_chr(detail_name, standardize_detail_name),
    standardized_name = coalesce(standardized_manual, standardized_auto)
  ) %>%
  select(original_name = detail_name, standardized_name)

# Blood_culture를 제외하고 표준화 적용
fever_lab <- fever_lab %>%
  left_join(mapping_table, by = c("detail_name" = "original_name")) %>%
  mutate(
    original_detail_name = detail_name,
    detail_name = case_when(
      detail_name == "Blood_culture" ~ "Blood_culture",  # Blood_culture는 그대로 유지
      TRUE ~ coalesce(standardized_name, detail_name)
    )
  ) %>%
  select(-standardized_name)

cat(sprintf("  - 표준화 후 unique 검사명: %d개\n\n", n_distinct(fever_lab$detail_name)))

# 환자 수 및 검사 빈도 계산
n_patients <- n_distinct(fever_lab$patient_id)
cat(sprintf("4. 데이터 품질 확인...\n  - 총 환자 수: %d명\n", n_patients))

test_frequency <- fever_lab %>%
  group_by(detail_name) %>%
  summarise(
    n_tests = n(),
    n_patients_tested = n_distinct(patient_id),
    patient_percentage = round(n_patients_tested / n_patients * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(n_patients_tested))

# 15% 필터링 - 제거할 변수 명확히 정의
cat("\n5. 15% 필터링 및 불필요 변수 제거...\n")

# 반드시 제거할 culture 관련 변수들 - 직접 지정
must_exclude_exact <- c(
  "Bacteria_Culture_ID_Sensitivity",
  "Bacteria_Final_Des_Culture_ID_Sensitivity", 
  "Bacteria_Preliminary_Des_Culture_ID_Sensitivity",
  "Aerobic_detection_time_hrs_Des_Culture_ID_Sensitivity",
  "Anaerobic_detection_time_hrs_Des_Culture_ID_Sensitivity",
  "배양개시시간",
  "양성배양시간"
)

# 추가 패턴
must_exclude_patterns <- c(
  "Aerobic_detection",
  "Anaerobic_detection",
  "Bacteria_Final",
  "Bacteria_Preliminary",
  "Bacteria_Culture",
  "detection_time",
  "_Culture_ID",
  "Culture_ID_Sensitivity"
)

# 패턴 매칭으로 제거할 변수 찾기 - NA 처리 추가
pattern_matches <- test_frequency$detail_name[
  sapply(test_frequency$detail_name, function(x) {
    # NA 체크 먼저
    if (is.na(x)) return(FALSE)
    # Blood_culture는 제외하지 않음
    if (x == "Blood_culture") return(FALSE)
    # 직접 지정한 변수인지 확인
    if (x %in% must_exclude_exact) return(TRUE)
    # 패턴 중 하나라도 매칭되면 제외
    any(sapply(must_exclude_patterns, function(pattern) {
      grepl(pattern, x, ignore.case = TRUE)
    }))
  })
]

# 직접 지정한 변수와 패턴 매칭 결합
vars_to_exclude <- unique(c(must_exclude_exact, pattern_matches))

cat(sprintf("  - 제거할 culture 관련 변수: %d개\n", length(vars_to_exclude)))
if (length(vars_to_exclude) > 0) {
  cat("    제거 목록:\n")
  # 특히 문제가 되는 변수들이 포함되었는지 확인
  problem_vars <- c("Bacteria_Culture_ID_Sensitivity",
                    "Bacteria_Final_Des_Culture_ID_Sensitivity", 
                    "Bacteria_Preliminary_Des_Culture_ID_Sensitivity")
  
  for (var in problem_vars) {
    if (var %in% vars_to_exclude) {
      cat(sprintf("      ✓ %s (제거됨)\n", var))
    } else if (var %in% test_frequency$detail_name) {
      cat(sprintf("      ✗ %s (주의: 제거 안됨!)\n", var))
    }
  }
  
  # 기타 제거 변수
  other_vars <- setdiff(vars_to_exclude, problem_vars)
  if (length(other_vars) > 0 && length(other_vars) <= 10) {
    for (var in other_vars) {
      cat(sprintf("      • %s\n", var))
    }
  } else if (length(other_vars) > 10) {
    for (var in head(other_vars, 5)) {
      cat(sprintf("      • %s\n", var))
    }
    cat(sprintf("      ... 외 %d개\n", length(other_vars) - 5))
  }
}

# 15% 이상 시행된 검사 선택 (Blood_culture는 무조건 포함, 특정 변수는 무조건 제외)
tests_to_keep <- test_frequency %>%
  filter(!is.na(detail_name)) %>%  # NA 제외 추가
  filter(
    (patient_percentage >= 15 | detail_name == "Blood_culture") &
      !detail_name %in% vars_to_exclude
  ) %>%
  pull(detail_name)

# 혹시 남아있는 문제 변수 강제 제거
problem_vars_final <- c(
  "Bacteria_Culture_ID_Sensitivity",
  "Bacteria_Final_Des_Culture_ID_Sensitivity", 
  "Bacteria_Preliminary_Des_Culture_ID_Sensitivity"
)
tests_to_keep <- setdiff(tests_to_keep, problem_vars_final)

cat(sprintf("\n  - 15%% 이상 시행 검사: %d개\n", 
            sum(test_frequency$patient_percentage >= 15)))
cat(sprintf("  - Blood_culture 포함 여부: %s\n", 
            ifelse("Blood_culture" %in% tests_to_keep, "포함", "미포함")))
cat(sprintf("  - 최종 보존할 검사: %d개\n", length(tests_to_keep)))

# 필터링 적용
fever_lab_filtered <- fever_lab %>%
  filter(detail_name %in% tests_to_keep)

cat(sprintf("  - 필터링 후 레코드: %s개\n\n", format(nrow(fever_lab_filtered), big.mark = ",")))

# 환자별 첫 번째 검사 결과 선택 (초기 상태 반영)
cat("6. 환자별 첫 번째 검사 결과 선택 (중복 처리)...\n")
cat("   → 방법: 같은 날 중복 검사는 첫 번째 값 사용\n")
cat("   → 가정: 먼저 입력된 값 = 먼저 시행된 검사\n")
cat("   → 임상적 의미: 초기 상태 반영\n\n")

# 중복 검사 패턴 확인
duplicate_check <- fever_lab_filtered %>%
  group_by(patient_id, detail_name) %>%
  summarize(n = n(), .groups = "drop") %>%
  filter(n > 1)

cat(sprintf("   중복 검사 발견: %d건 (전체의 %.1f%%)\n\n", 
            nrow(duplicate_check),
            nrow(duplicate_check) / n_distinct(fever_lab_filtered$patient_id, 
                                               fever_lab_filtered$detail_name) * 100))

# ⭐ 첫 번째 값 사용 (원본 순서 유지)
fever_lab_latest <- fever_lab_filtered %>%
  group_by(patient_id, detail_name) %>%
  slice(1) %>%  # 첫 번째 행만 선택
  ungroup()

cat(sprintf("   중복 제거 후: %s개 레코드 → %s개 레코드\n\n", 
            format(nrow(fever_lab_filtered), big.mark = ","),
            format(nrow(fever_lab_latest), big.mark = ",")))
# 결과값 정리 (Blood_culture는 문자열 유지, 나머지는 숫자로 변환)
cat("7. 검사 결과값 정리...\n")

# Blood_culture는 문자열로 유지하기 위해 별도 처리
blood_culture_data <- fever_lab_latest %>%
  filter(detail_name == "Blood_culture") %>%
  select(patient_id, visit_date, visit_time, sex, age, detail_name, result)

# 나머지 데이터는 숫자로 변환
other_data <- fever_lab_latest %>%
  filter(detail_name != "Blood_culture")

# 숫자 변환 함수
clean_result_value <- function(value) {
  if (is.na(value)) return(NA_real_)
  
  value <- as.character(value) %>% str_trim()
  
  if (value == "") return(NA_real_)
  
  # 일반 음성/양성 변환
  if (tolower(value) %in% c("negative", "neg", "음성", "-")) return(0)
  if (tolower(value) %in% c("positive", "pos", "양성", "+")) return(1)
  
  # 부등호 처리
  value <- str_replace(value, "^[<>≤≥]", "")
  
  # 범위 처리
  if (str_detect(value, "^\\d+\\.?\\d*-\\d+\\.?\\d*$")) {
    parts <- str_split(value, "-")[[1]]
    return(mean(as.numeric(parts), na.rm = TRUE))
  }
  
  # 숫자 추출
  numeric_match <- str_extract(value, "^[-+]?\\d*\\.?\\d+")
  if (!is.na(numeric_match)) {
    return(as.numeric(numeric_match))
  }
  
  return(NA_real_)
}

# 나머지 데이터 숫자 변환
other_data <- other_data %>%
  mutate(result_clean = map_dbl(result, clean_result_value))

# Wide 형식으로 변환
cat("\n8. Wide 형식으로 변환...\n")

id_vars <- c("patient_id", "visit_date", "visit_time", "sex", "age")

# Blood_culture wide 변환 (문자열 유지)
if (nrow(blood_culture_data) > 0) {
  blood_wide <- blood_culture_data %>%
    select(all_of(id_vars), detail_name, result) %>%
    pivot_wider(
      names_from = detail_name,
      values_from = result,
      values_fn = first
    )
} else {
  blood_wide <- fever_lab_latest %>%
    select(all_of(id_vars)) %>%
    distinct() %>%
    mutate(Blood_culture = NA_character_)
}

# 나머지 데이터 wide 변환
other_wide <- other_data %>%
  select(all_of(id_vars), detail_name, result_clean) %>%
  pivot_wider(
    names_from = detail_name,
    values_from = result_clean,
    values_fn = first
  )

# 두 데이터 결합
lab_wide <- blood_wide %>%
  left_join(other_wide, by = id_vars)

# 중복 행 제거 (같은 환자의 여러 방문 중 가장 최근 것만)
lab_wide <- lab_wide %>%
  group_by(patient_id) %>%
  arrange(desc(visit_date), desc(visit_time)) %>%
  slice(1) %>%
  ungroup()

cat(sprintf("  - Wide 형식 데이터: %d 행 × %d 열\n", nrow(lab_wide), ncol(lab_wide)))

# 변수명 정리 및 재배열
cat("\n9. 변수명 정리 및 재배열...\n")

# 변수명 정리 함수 (QI → urine 변환 포함)
clean_column_name <- function(col) {
  if (col %in% c(id_vars, "Blood_culture")) return(col)
  
  # QI/Ql를 urine으로 변환
  col <- str_replace_all(col, "_QI_|_Ql_|_QI|_Ql|QI_|Ql_", "_urine_")
  
  # 특수문자 처리
  col <- str_replace_all(col, "[^\\w]", "_")
  col <- str_replace_all(col, "_+", "_")
  col <- str_replace(col, "^_|_$", "")
  
  # 숫자로 시작하면 Lab_ 접두사 추가
  if (str_detect(col, "^\\d")) {
    col <- paste0("Lab_", col)
  }
  
  return(make.names(col))
}

names(lab_wide) <- map_chr(names(lab_wide), clean_column_name)

# 변수 그룹별로 재배열
base_vars <- id_vars
blood_vars <- "Blood_culture"
artery_vars <- names(lab_wide)[str_detect(names(lab_wide), "Artery_blood|artery")]
urine_vars <- names(lab_wide)[str_detect(names(lab_wide), "urine|Urine")]
other_vars <- setdiff(names(lab_wide), c(base_vars, blood_vars, artery_vars, urine_vars))

# 재배열
lab_wide <- lab_wide %>%
  select(
    all_of(base_vars),
    all_of(blood_vars),
    all_of(sort(artery_vars)),
    all_of(sort(urine_vars)),
    all_of(sort(other_vars))
  )

cat(sprintf("  - 변수 재배열 완료:\n"))
cat(sprintf("    • 기본 정보: %d개\n", length(base_vars)))
cat(sprintf("    • Blood culture: %d개\n", length(blood_vars)))
cat(sprintf("    • Artery blood: %d개\n", length(artery_vars)))
cat(sprintf("    • Urine 검사: %d개\n", length(urine_vars)))
cat(sprintf("    • 기타 검사: %d개\n", length(other_vars)))

# 최종 확인: 제거되어야 할 변수가 있는지 체크
problem_vars_to_check <- c(
  "Bacteria_Culture_ID_Sensitivity",
  "Bacteria_Final_Des_Culture_ID_Sensitivity", 
  "Bacteria_Preliminary_Des_Culture_ID_Sensitivity",
  "Aerobic_detection_time_hrs_Des_Culture_ID_Sensitivity",
  "Anaerobic_detection_time_hrs_Des_Culture_ID_Sensitivity"
)

# 변수명 정리 후 형태도 고려
problem_vars_all_forms <- unique(c(
  problem_vars_to_check,
  make.names(problem_vars_to_check),
  str_replace_all(problem_vars_to_check, "[^A-Za-z0-9_]", "_"),
  str_replace_all(problem_vars_to_check, "[^A-Za-z0-9_]", ".")
))

remaining_exclude <- names(lab_wide)[names(lab_wide) %in% problem_vars_all_forms]

if (length(remaining_exclude) > 0) {
  cat("\n⚠️ 경고: 제거되어야 했지만 남은 변수들을 강제 제거합니다:\n")
  for (var in remaining_exclude) {
    cat(sprintf("    • %s → 제거\n", var))
  }
  # 강제 제거
  lab_wide <- lab_wide %>%
    select(-all_of(remaining_exclude))
  cat(sprintf("    → 강제 제거 완료. 최종 변수 수: %d개\n", ncol(lab_wide)))
} else {
  cat("\n✅ 모든 불필요한 culture 변수가 성공적으로 제거되었습니다.\n")
}

# ==============================================================================
# PART 3: 결과 저장
# ==============================================================================

cat("\n10. 최종 데이터 저장...\n")

# CSV 파일로 저장
write_csv(lab_wide, "fever_lab_wide_final.csv", na = "")
cat("  - fever_lab_wide_final.csv 저장 완료\n")

# Excel 파일로 저장
write.xlsx(lab_wide, "fever_lab_wide_final.xlsx")
cat("  - fever_lab_wide_final.xlsx 저장 완료\n")

# RDS 파일로 저장
saveRDS(lab_wide, "cleaned_data/Part2_fever_lab_wide_final.rds")
cat("  - fever_lab_wide_final.rds 저장 완료\n")

"cleaned_data/fever_lab_wide_final.rds"
# ==============================================================================
# PART 4: 요약 리포트
# ==============================================================================

cat("\n========================================\n")
cat("처리 완료 요약\n")
cat("========================================\n")

cat("\n최종 데이터셋 정보:\n")
cat(sprintf("- 환자 수: %d명\n", n_distinct(lab_wide$patient_id)))
cat(sprintf("- 전체 변수: %d개\n", ncol(lab_wide)))
cat(sprintf("- 검사 변수: %d개\n", ncol(lab_wide) - length(id_vars)))
cat(sprintf("- 데이터 크기: %d × %d\n", nrow(lab_wide), ncol(lab_wide)))

# Blood culture 결과 분포 확인
if ("Blood_culture" %in% names(lab_wide)) {
  cat("\n최종 Blood culture 결과 분포:\n")
  bc_final <- lab_wide %>%
    count(Blood_culture) %>%
    filter(!is.na(Blood_culture))
  print(bc_final)
  cat(sprintf("  - Blood culture 검사 시행 환자: %d명\n", sum(bc_final$n)))
}

# 변수 목록 확인
cat("\n변수 목록 샘플 (처음 20개):\n")
for (i in 1:min(20, length(names(lab_wide)))) {
  cat(sprintf("  %2d. %s\n", i, names(lab_wide)[i]))
}

cat("\n🎉 전체 작업 완료!\n")
cat("\n✅ 수정 사항:\n")
cat("  • Blood_culture 변수에 G(+), G(-), no growth 문자열 유지\n")
cat("  • 불필요한 culture 변수 완전 제거\n")
cat("  • QI → urine 변환\n")
cat("  • Artery_blood 변수 그룹화\n")

cat("\n생성된 파일:\n")
cat("  1. fever_lab_wide_final.csv - 최종 데이터\n")
cat("  2. fever_lab_wide_final.xlsx - Excel 형식\n")
cat("  3. fever_lab_wide_final.rds - R 전용 형식\n")