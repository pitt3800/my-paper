#.데이터 로딩& 파일 합치기  & smple data 만들기
#.raw_data 저장하기

#------------------------------------------------------------------------------
# 0. 환경 설정
#------------------------------------------------------------------------------
library(tidyverse)      # 데이터 조작
library(readr)          # CSV 파일 읽기 (한글 인코딩 지원)
library(writexl)        # Excel 파일 쓰기
library(janitor)        # 변수명 클리닝
library(lubridate)      # 날짜 처리
library(readxl)


setwd("/Users/youjinlee/Documents/My R/fever paper")

#. 데이터 불러오기 ,완전 원본 데이터
input_file <- "2017_2025_raw_data/ER_LAB_RSLT.csv"  

fever_lab <- read_csv(
  input_file,
  locale = locale(encoding = "CP949"),
  show_col_types = FALSE
)

combined_1 <- read_excel(
  "2017_2025_raw_data/base_result_nurse_fever.xlsx"
)

combined_2 <- read_excel(
  "2017_2025_raw_data/base_result_nurse_fever(3.0).xlsx"
)


ct1<-read.csv("2017_2025_raw_data/CT_result(1.0).csv", header=T ,stringsAsFactors=FALSE)

ct2<-read.csv("2017_2025_raw_data/CT_result(3.0).csv", header=T ,stringsAsFactors=FALSE)

#------------------------------------------------------------------------------
# . 기본 구조 및 결측치 요약
#------------------------------------------------------------------------------
summarize_df <- function(df, name) {
  cat("\n=====", name, "=====\n")
  cat("행:", nrow(df), " / 열:", ncol(df), "\n")
  print(df %>% glimpse())
  
  cat("\n결측치 비율 (상위 10개):\n")
  miss_rate <- df %>% 
    summarise(across(everything(), ~mean(is.na(.))*100)) %>%
    pivot_longer(everything(), names_to="variable", values_to="missing_pct") %>%
    arrange(desc(missing_pct)) %>%
    slice(1:10)
  print(miss_rate)
}

summarize_df(fever_lab, "fever_lab")
summarize_df(combined_1, "combined_1")
summarize_df(combined_2, "combined_2")
summarize_df(ct1, "ct1")
summarize_df(ct2, "ct2")



#------------------------------------------------------------------------------
# 결측치 상위 5개 변수 포함 행만 필터링-결측치가 포함된 case가 뭔지
#------------------------------------------------------------------------------

fever_lab_missing_top5 <- fever_lab %>%
  filter(
    is.na(보고일) |
      is.na(결과) |
      is.na(검사명)
    # 필요시 추가 변수 포함
    # 예: is.na(다른_변수) |
  )
fever_lab_missing_top5


#==============================================================================
# 파일 통합
# 목표: combined_1, combined_2 병합 , ct1, ct2 병합
#==============================================================================

# . 컬럼명 통일 -------------------------------------------------------------
cat("=== STEP 2: 컬럼명 통일 ===\n")

# combined_1의 "이름" → "환자명"으로 변경
if ("이름" %in% names(combined_1)) {
  combined_1 <- combined_1 %>% rename(환자명 = 이름)
  cat("✓ combined_1: '이름' → '환자명'\n")
}
# "전원온병원" → "전원온 병원" 통일
if ("전원온병원" %in% names(combined_1)) {
  combined_1 <- combined_1 %>% rename(`전원온 병원` = 전원온병원)
  cat("✓ combined_1: '전원온병원' → '전원온 병원'\n")
}
# "퇴원일" → "퇴원일자" 통일
if ("퇴원일" %in% names(combined_1)) {
  combined_1 <- combined_1 %>% rename(퇴원일자 = 퇴원일)
  cat("✓ combined_1: '퇴원일' → '퇴원일자'\n")
}
# combined_2에 "노트" 컬럼 추가 (없을 경우)
if (!"노트" %in% names(combined_2)) {
  combined_2$노트 <- NA_character_
  cat("✓ combined_2: '노트' 컬럼 추가\n")
}
# 3. 데이터 타입 통일 (병합 전 필수) -----------------------------------------
# 모든 컬럼을 문자형으로 변환 (타입 불일치 방지)
combined_1 <- combined_1 %>% mutate(across(everything(), as.character))
combined_2 <- combined_2 %>% mutate(across(everything(), as.character))

# 4. 두 파일 병합 ------------------------------------------------------------
cat("=== STEP 4: 데이터 병합 ===\n")

# 컬럼 순서 맞추기 (combined_1 기준)
col_names <- names(combined_1)
combined_2 <- combined_2[, col_names]

# 병합
combined <- bind_rows(combined_1, combined_2)
write_excel_csv(combined, "2017_2025_raw_data/combined.csv")



##.ct1, ct 2파일 병합 ================================================================

# 각 파일 크기 확인
dim(ct1)  # 행, 열
dim(ct2)

# 컬럼명 확인 (두 파일이 동일한지)
names(ct1)
names(ct2)

# 행 결합 (row bind)
ct12 <- bind_rows(ct1, ct2)

# 결합 결과 확인
dim(ct12)
nrow(ct1) + nrow(ct2)

# 첫/마지막 몇 행 확인
head(ct12, 3)
tail(ct12, 3)

write_excel_csv(ct12, "2017_2025_raw_data/ct12.csv")




#------------------------------------------------------------------------------------------------
# 2. 샘플 데이터 생성 (1~2% 또는 최대 1000행) 이것은 raw 데이터로 처음부터 review 등
#.  AI에넣고 코딩 요구할때 쓰자.
#------------------------------------------------------------------------------------------------
sample_df <- function(df, name) {
  set.seed(2025)
  n <- min(round(nrow(df) * 0.02), 1000)
  df_sample <- df %>% sample_n(n)
  write_excel_csv(df_sample, paste0(name, "_sample.csv"))
  cat(sprintf("✓ %s 샘플 저장 완료 (%d행)\n", name, n))
}

sample_df(fever_lab, "sample_r/fever_lab")
sample_df(combined_1, "sample_r/combined_1")
sample_df(combined_2, "sample_r/combined_2")
sample_df(combined, "sample_r/combined")
sample_df(ct1, "sample_r/ct1")
sample_df(ct2, "sample_r/ct2")
sample_df(ct12, "sample_r/ct12")

#.csv 파일까지 다 저장시켰네.sample_r/에 저장함





# 5. 재방문 환자 분석 --------------------------------------------------------

#.연구 주제에 따라 이분을 선택해야..

#1.모든 재방문을 하나의 방문으로 할 수도 있고.. 현재 fever & CT 연구
#2 ⭐ 같은 날 여러 번 내원해도 1회로 카운트, 한 환자가 다른날 여러번 내원하면 다른케이스로 할  수도 있고
#3.같은 날 재내원한 환자만 선택할 수도 있고.
#4.여러날 재내원한 환자만 선택할 수도 있고
#5.모든 재내원을 각자의 경우로 선택할 수도 있고.







# 8. 결과 저장 ---------------------------------------------------------------

# CSV 저장 (한글 깨짐 방지)

#.write_excel_csv(combined, "combined_sample.csv") 앞에서 했음
#.5개law file csv 


## RDS 저장 (R 전용)


#.original


saveRDS(combined_1, "2017_2025_raw_data/combined_1.rds")
saveRDS(combined_2, "2017_2025_raw_data/combined_2.rds")
saveRDS(ct1, "2017_2025_raw_data/ct1.rds")
saveRDS(ct2, "2017_2025_raw_data/ct2.rds")

saveRDS(fever_lab, "2017_2025_raw_data/fever_lab.rds")
saveRDS(ct12, "2017_2025_raw_data/ct12.rds")
saveRDS(combined, "2017_2025_raw_data/combined.rds")





























#===============================================================================================
# 아래 내용은 재방문 환자에 대한 정보로 나중에 필요할 때 정리
#===============================================================================================

# 
# 
# # 등록번호 + 내원일자 기준으로 방문 횟수 계산
# # ⭐ 같은 날 여러 번 내원해도 1회로 카운트, 한 환자가 다른날 여러번 내원하면 다른케이스로
# visit_counts <- combined %>%
#   filter(!is.na(등록번호), !is.na(내원일자), 
#          등록번호 != "", 내원일자 != "") %>%
#   # 등록번호 + 내원일자 조합이 같으면 1개 행만 남김
#   distinct(등록번호, 내원일자) %>%
#   # 환자별 총 방문일 수 계산
#   group_by(등록번호) %>%
#   summarise(
#     총_방문일수 = n(),
#     방문일자_목록 = paste(내원일자, collapse = ", "),
#     .groups = "drop"
#   )
# visit_counts
# 
# 
# # 2회 이상 내원한 환자만 필터링
# repeat_visitors <- visit_counts %>%
#   filter(총_방문일수 >= 2) %>%
#   arrange(desc(총_방문일수), 등록번호)
# repeat_visitors
# 
# 
# # 6. 재방문 환자 상세 정보 테이블 --------------------------------------------
# 
# # 재방문 환자의 전체 기록
# repeat_visit_details <- combined %>%
#   filter(등록번호 %in% repeat_visitors$등록번호) %>%
#   select(등록번호, 환자명, 내원일자, 내원시간, 성별, 나이, 
#          주증상1, 진단명, 진료결과) %>%
#   arrange(등록번호, 내원일자, 내원시간)
# repeat_visit_details
# 
# 
# # 같은 날 2번 이상 내원한 환자 정보
# revisit_patients_info <- combined %>%
#   # 데이터 정제
#   filter(!is.na(등록번호), !is.na(내원일자), 
#          등록번호 != "", 내원일자 != "") %>%
#   
#   # 일일 방문 횟수 계산 (mutate = 원본 유지!)
#   group_by(등록번호, 내원일자) %>%
#   mutate(일일방문횟수 = n()) %>%  #  n() 그굽으로 묶었을때 그 그룹마다 몇개씩 있냐?
#   ungroup() %>%
#   
#   # 하루 2번 이상 방문한 적 있는 환자만
#   group_by(등록번호) %>%
#   filter(any(일일방문횟수 >= 2)) %>%
#   ungroup() %>%
#   
#   # 각 환자의 첫 방문만 (환자당 1행)
#   group_by(등록번호) %>%
#   arrange(내원일자) %>%
#   slice(1) %>%
#   ungroup() %>%
#   
#   # 필요한 컬럼만
#   select(등록번호, 나이, 성별,일일방문횟수)
# 
# # 결과 확인
# revisit_patients_info
# 
# # 환자 수 확인
# cat("같은 날 2번 이상 내원한 환자:", nrow(revisit_patients_info), "명\n")
# 


# 
# 
# 
# # 7. 결과 테이블 요약 --------------------------------------------------------
# 
# # 방문 횟수별 환자 수
# visit_freq_summary <- repeat_visitors %>%
#   count(총_방문일수, name = "환자수") %>%
#   mutate(비율 = round(환자수 / sum(환자수) * 100, 1)) %>%
#   arrange(desc(총_방문일수))
# 
# visit_freq_summary
# print(visit_freq_summary, n = 20)

