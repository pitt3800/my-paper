#===============================================================================================
  # sample data 만들기: base와 fever lab이 다 갖추어진 경우: 완결된 가상의 데이터로  분석 연습용
#=======================================================================================================

#==============================================================================
# sample_r/에 있는 파일 불러와서 내 모든 CSV 파일을 자동으로 읽어, 파일명으로 객체 저장- 
# 여기서 만들어진 파일을 AI에 첨부

#==============================================================================
library(tidyverse)

# 1️⃣ CSV 파일 경로 목록 불러오기
csv_files <- list.files(path = "/Users/youjinlee/Documents/My R/fever paper/sample_r", pattern = "\\.csv$", full.names = TRUE)

# 2️⃣ 각 파일을 읽어서 파일명으로 객체 생성
for (file in csv_files) {
  # 파일명에서 확장자(.csv) 제거 → 변수명으로 사용
  varname <- tools::file_path_sans_ext(basename(file))
  
  # 파일 읽기
  df <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  
  # 전역 환경(Global Environment)에 동적으로 변수 생성
  assign(varname, df)
  
  cat("✓", varname, "불러오기 완료 (", nrow(df), "행)\n")
}

cat("\n=== 모든 CSV 파일 로드 완료 ===\n")


#.여기서 만든 AI로 sample_r을 올려서 작업시작함.




#==============================================================================
# CT와 Lab 모두 시행한 환자 데이터 병합
# 목표: CT + Lab 모두 시행 환자만 선택하여 통합 데이터셋 생성 raw 데이터 이용하기
#==============================================================================

# 작업 디렉토리 설정 (
setwd('/Users/youjinlee/Documents/My R/fever paper')

#------------------------------------------------------------------------------
# 1. 데이터 로드 (완전 raw data)
#------------------------------------------------------------------------------
ct <- read_csv("2017_2025_raw_data/ct12.csv", 
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE)

lab <- read_csv("2017_2025_raw_data/ER_LAB_RSLT.csv",
                locale = locale(encoding = "CP949"),
                show_col_types = FALSE)

combined <- read_csv("2017_2025_raw_data/combined.csv",
                     locale = locale(encoding = "UTF-8"),
                     show_col_types = FALSE)

# 데이터 크기 확인
cat(sprintf("\n원본 데이터: CT %d행, Lab %d행, Combined %d행\n\n", 
            nrow(ct), nrow(lab), nrow(combined)))
#.이 세파일은 원본 그대로 다만  combined는 합치기 위해 변수명만 아주 조금 바꿈.




#------------------------------------------------------------------------------
# 2. CT와 Lab 모두 시행한 환자 식별( 두번 이상 내원환 환자는 한번만 내원한 것으로 취급합)
#------------------------------------------------------------------------------
ct_patients <- unique(ct$등록번호)
lab_patients <- unique(lab$등록번호)
both_patients <- intersect(ct_patients, lab_patients)

cat(sprintf("CT 시행: %d명, Lab 시행: %d명, 둘 다: %d명\n\n", 
            length(ct_patients), length(lab_patients), length(both_patients)))

#.CT 시행: 3785명, Lab 시행: 6259명, 둘 다: 3765명

#------------------------------------------------------------------------------
# 3. 필터링
#------------------------------------------------------------------------------
combined_filtered <- combined %>% filter(등록번호 %in% both_patients)
ct_filtered <- ct %>% filter(등록번호 %in% both_patients)
lab_filtered <- lab %>% filter(등록번호 %in% both_patients)

cat(sprintf("필터링 결과: Combined %d행, CT %d행, Lab %d행\n\n",
            nrow(combined_filtered), nrow(ct_filtered), nrow(lab_filtered)))

# 필터링 결과: Combined 5780행, CT 7017행, Lab 1245379행


#------------------------------------------------------------------------------
# 4. 저장
#------------------------------------------------------------------------------



write_excel_csv(combined_filtered , "filtered_data/combined_filtered.csv")
write_excel_csv(ct_filtered, "filtered_data/ct_filtered.csv")
write_excel_csv(lab_filtered, "filtered_data/lab_filtered.csv")

saveRDS(combined_filtered, "filtered_data/combined_filtered")
saveRDS(ct_filtered , "filtered_data/ct_filtered")
saveRDS(lab_filtered, "filtered_data/lab_filtered.rds")







cat("저장 완료: merged_ct_lab_patients.csv, .rds\n\n")


#------------------------------------------------------------------------------
# 8. 확인
#------------------------------------------------------------------------------
# 컬럼명 출력
cat("컬럼 구조 (처음 20개):\n")
print(head(names(final_merged), 20))

# 데이터 미리보기
cat("\n데이터 샘플:\n")
final_merged %>% select(1:10) %>% head(3)

cat("\n✅ 완료!\n")

