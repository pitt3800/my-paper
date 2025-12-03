#.하고 싶은 것 result 열의 값이G(+) or G(-) 인 데이터를 식별
#.이를 위해선  result 열의 값이G(+) or G(-) 인  order_code, order_name ,detail_code, detail_name을 구해야.
#===============================================================================================
# result 값이 G(+) or G(-) 일때 order_code, order_name ,detail_code, detail_name
#===================================================================================
# 결론: order_code Z11184	order_name (1.0)SMEAR GRAM STAIN, BLOOD
# detail_code	Z11184  detail_name	(1.0)SMEAR GRAM STAIN, BLOOD result Gram+cocci(clusters) ??


setwd("/Users/youjinlee/Documents/My R/fever paper")

fever_lab_original <-readRDS("filtered_data/lab_filtered.rds")

# BOM 제거
if (str_detect(names(fever_lab_original)[1], "^[\\ufeff]")) {
  names(fever_lab_original)[1] <- str_replace(names(fever_lab_original)[1], "^[\\ufeff]", "")
}

# 컬럼명 변경

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
    detail_code = 세부검사코드, 
    detail_name = 검사명,
    result = 결과
  ) %>%
  mutate(visit_date = ymd(as.character(visit_date)))

# 전체 데이터 확인
cat(sprintf("전체 데이터: %s건\n\n", format(nrow(fever_lab), big.mark = ",")))

#===============================================================================================
# 일단 2023-06-30 전 후로 데이터 프레임 나누고 result가 G(+) or G(-) 인 행의 order_name 
#.order_code, detail_code 등 구하기
#===============================================================================================


# 2023년 6월 30일까지 데이터
pre2023 <- fever_lab %>%
  filter(visit_date <= ymd("2023-06-30"))

# 2023년 7월 1일부터 데이터
post2023 <- fever_lab %>%
  filter(visit_date >= ymd("2023-07-01"))


# ==============================================================================
# STEP 2: G(+) 또는 G(-) 결과가 있는 행 찾기
# ==============================================================================

# 2023-06-30까지 데이터에서 result 에서 G(+)/G(-) 찾기

gram_pre2023 <- pre2023 %>%
  filter(str_detect(
    result,
    regex("g\\+|g\\-|g\\(\\+\\)|g\\(\\-\\)|gram.*positive|gram.*negative|그람.*양성|그람.*음성",
          ignore_case = TRUE)
  )) %>% 
  select(patient_id, patient_name,visit_date, order_name,order_code, detail_code,detail_name, result) %>%
  distinct()

#.pre2023과 gram-pre2023 비교함. 복잡하다. 
#.결론은 detatil_code: z11184,(같은결과지만 detail_name : (1.0)SMEAR GRAM STAIN, BLOOD)
# 인것만 filtering하고 추후 bactremia  연구시 detatil_code: z11184에서 G(+-)인군과 아닌군 비교하면 될듯.




# 2023-07-01부터 데이터에서 G(+)/G(-) 찾기
gram_post2023 <- post2023 %>%
  filter(str_detect(
    result,
    regex("g\\+|g\\-|g\\(\\+\\)|g\\(\\-\\)|gram.*positive|gram.*negative|그람.*양성|그람.*음성",
          ignore_case = TRUE)
  )) %>% 
  select(patient_id, patient_name,visit_date, order_name,order_code, detail_code,detail_name, result) %>%
  distinct()



cat("\n========================================\n")
cat("2023-07-01부터 G(+)/G(-) 결과\n")
cat("========================================\n")
cat(sprintf("발견: %d건\n\n", nrow(gram_post2023)))

# order_name 종류
cat("--- order_name 종류 ---\n")
gram_post2023 %>%
  count(order_name, sort = TRUE) %>%
  print(n = Inf)

cat("\n--- detail_name 종류 ---\n")
gram_post2023 %>%
  count(detail_name, sort = TRUE) %>%
  print(n = Inf)

cat("\n--- detail_code 종류 ---\n")
gram_post2023 %>%
  count(detail_code, sort = TRUE) %>%
  print(n = Inf)

cat("\n--- 샘플 데이터 (상위 5건) ---\n")
gram_post2023 %>%
  head(5) %>%
  print()




# ==============================================================================
# STEP 1: 2023-06-30까지 Blood Culture 행 추출
# ==============================================================================

# 2023-06-30까지 데이터에서 해당 order_name으로 추출
# (앞선 분석에서 찾은 order_name 사용)
blood_culture_pre2023 <- fever_lab %>%
  filter(
    visit_date <= ymd("2023-06-30"),
    order_name == "(1.0)SMEAR GRAM STAIN, BLOOD"
  )

# detail_name 확인
cat("--- detail_name 분포 ---\n")
blood_culture_pre2023 %>%
  count(detail_name, sort = TRUE) %>%
  print(n = 20)

# result 분포 확인
cat("\n--- result 분포 (상위 10개) ---\n")
blood_culture_pre2023 %>%
  count(result, sort = TRUE) %>%
  print(n = 10)

# 샘플 데이터
cat("\n--- 샘플 데이터 (처음 5건) ---\n")
blood_culture_pre2023 %>%
  select(patient_id, visit_date, detail_name, result) %>%
  head(5) %>%
  print()

# ==============================================================================
# STEP 2: 2023-07-01부터 Blood Culture 행 추출
# ==============================================================================

# 2023-07-01부터는 detail_code로 추출
# (앞선 분석에서 detail_code = "ABACT" 확인됨)
blood_culture_post2023 <- fever_lab %>%
  filter(
    visit_date >= ymd("2023-07-01"),
    detail_code == "ABACT"
  )

cat("\n========================================\n")
cat("2023-07-01부터 Blood Culture 추출\n")
cat("========================================\n")
cat(sprintf("추출: %s건\n", format(nrow(blood_culture_post2023), big.mark = ",")))
cat(sprintf("환자 수: %d명\n\n", n_distinct(blood_culture_post2023$patient_id)))

# order_name 확인
cat("--- order_name 분포 ---\n")
blood_culture_post2023 %>%
  count(order_name, sort = TRUE) %>%
  print(n = 10)

# detail_name 확인
cat("\n--- detail_name 분포 ---\n")
blood_culture_post2023 %>%
  count(detail_name, sort = TRUE) %>%
  print(n = 10)

# result 분포 확인
cat("\n--- result 분포 (상위 10개) ---\n")
blood_culture_post2023 %>%
  count(result, sort = TRUE) %>%
  print(n = 10)

# 샘플 데이터
cat("\n--- 샘플 데이터 (처음 5건) ---\n")
blood_culture_post2023 %>%
  select(patient_id, visit_date, detail_name, result) %>%
  head(5) %>%
  print()

# ==============================================================================
# STEP 3: 두 기간 데이터 통합
# ==============================================================================

# 두 기간 데이터 병합
blood_culture_all <- bind_rows(
  blood_culture_pre2023 %>% mutate(period = "2023-06 이전"),
  blood_culture_post2023 %>% mutate(period = "2023-07 이후")
)

cat("\n========================================\n")
cat("전체 Blood Culture 통합\n")
cat("========================================\n")
cat(sprintf("전체: %s건\n", format(nrow(blood_culture_all), big.mark = ",")))
cat(sprintf("환자 수: %d명\n", n_distinct(blood_culture_all$patient_id)))
cat(sprintf("기간별:\n"))
blood_culture_all %>%
  count(period) %>%
  print()

# ==============================================================================
# STEP 4: 결과 저장
# ==============================================================================

# 각 기간별 저장
write_excel_csv(blood_culture_pre2023, "blood_culture_pre2023_full.csv")
write_excel_csv(blood_culture_post2023, "blood_culture_post2023_full.csv")

# 통합 저장
write_excel_csv(blood_culture_all, "blood_culture_all_periods.csv")

# RDS 저장 (R 전용)
saveRDS(blood_culture_pre2023, "blood_culture_pre2023.rds")
saveRDS(blood_culture_post2023, "blood_culture_post2023.rds")
saveRDS(blood_culture_all, "blood_culture_all.rds")

blood_culture <- read_csv(
  "blood_culture_pre2023_full.csv",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)
gram_specific <- blood_culture %>%
  filter(
    str_detect(result, "Gram\\+cocci\\(clusters\\)") | 
      str_detect(result, "Gram negative rod")
  )




# ==============================================================================
# STEP 2: order_code, order_name, detail_code, detail_name 확인
# ==============================================================================

# 검사 정보 추출
test_info <- gram_specific %>%
  select(order_code, order_name, detail_code, detail_name, result) %>%
  distinct()

cat("--- 검사 정보 ---\n")
print(test_info, n = Inf)

# ==============================================================================
# STEP 3: 각 컬럼별 고유값 확인
# ==============================================================================

cat("\n--- order_code 종류 ---\n")
gram_specific %>%
  count(order_code, sort = TRUE) %>%
  print(n = Inf)

cat("\n--- order_name 종류 ---\n")
gram_specific %>%
  count(order_name, sort = TRUE) %>%
  print(n = Inf)

cat("\n--- detail_code 종류 ---\n")
gram_specific %>%
  count(detail_code, sort = TRUE) %>%
  print(n = Inf)

cat("\n--- detail_name 종류 ---\n")
gram_specific %>%
  count(detail_name, sort = TRUE) %>%
  print(n = Inf)

# ==============================================================================
# STEP 4: result 패턴 확인
# ==============================================================================

cat("\n--- result 패턴 ---\n")
gram_specific %>%
  mutate(
    gram_type = case_when(
      str_detect(result, "Gram\\+cocci\\(clusters\\)") ~ "Gram+cocci(clusters)",
      str_detect(result, "Gram negative rod") ~ "Gram negative rod",
      TRUE ~ "기타"
    )
  ) %>%
  count(gram_type, sort = TRUE) %>%
  print()

# ==============================================================================
# STEP 5: 전체 데이터 샘플
# ==============================================================================

cat("\n--- 샘플 데이터 (처음 10건) ---\n")
gram_specific %>%
  select(patient_id, visit_date, order_code, order_name, 
         detail_code, detail_name, result) %>%
  head(10) %>%
  print()

# ==============================================================================
# STEP 6: 결과 저장
# ==============================================================================

# 전체 데이터 저장
write_excel_csv(gram_specific, "gram_specific_results.csv")

# 검사 정보만 저장
write_excel_csv(test_info, "gram_specific_test_info.csv")







# 기간별 통계
summary_stats <- blood_culture_all %>%
  group_by(period) %>%
  summarise(
    총_건수 = n(),
    환자_수 = n_distinct(patient_id),
    평균_건수_per_환자 = round(n() / n_distinct(patient_id), 1),
    .groups = "drop"
  )

print(summary_stats)

# G(+)/G(-) 분포 확인
cat("\n--- Gram 염색 결과 분포 ---\n")
blood_culture_all %>%
  mutate(
    gram_result = case_when(
      str_detect(result, "G\\(\\+\\)|G\\+|gram.*positive|그람.*양성") ~ "G(+)",
      str_detect(result, "G\\(\\-\\)|G\\-|gram.*negative|그람.*음성") ~ "G(-)",
      str_detect(result, "no.*growth|negative|음성") ~ "No growth",
      TRUE ~ "기타"
    )
  ) %>%
  count(period, gram_result) %>%
  print(n = Inf)

cat("\n✅ Blood Culture 추출 완료\n\n")




























# ==============================================================================
# STEP 1: 2023-06-30 기준으로 데이터 분리
# ==============================================================================

# ==============================================================================
# STEP 3: 결과 저장
# ==============================================================================

# Excel 저장
write_excel_csv(gram_pre2023, "gram_positive_negative_pre2023.csv")
write_excel_csv(gram_post2023, "gram_positive_negative_post2023.csv")

cat("\n========================================\n")
cat("결과 파일 저장 완료\n")
cat("========================================\n")
cat("- gram_positive_negative_pre2023.csv\n")
cat("- gram_positive_negative_post2023.csv\n\n")

# ==============================================================================
# STEP 4: 요약 정보
# ==============================================================================

cat("\n========================================\n")
cat("요약 정보\n")
cat("========================================\n\n")

cat("**2023-06-30까지**\n")
if(nrow(gram_pre2023) > 0) {
  cat(sprintf("• 가장 많은 order_name: %s (%d건)\n",
              gram_pre2023 %>% count(order_name, sort = TRUE) %>% slice(1) %>% pull(order_name),
              gram_pre2023 %>% count(order_name, sort = TRUE) %>% slice(1) %>% pull(n)))
  cat(sprintf("• 가장 많은 detail_name: %s (%d건)\n\n",
              gram_pre2023 %>% count(detail_name, sort = TRUE) %>% slice(1) %>% pull(detail_name),
              gram_pre2023 %>% count(detail_name, sort = TRUE) %>% slice(1) %>% pull(n)))
} else {
  cat("• G(+)/G(-) 결과 없음\n\n")
}

cat("**2023-07-01부터**\n")
if(nrow(gram_post2023) > 0) {
  cat(sprintf("• 가장 많은 order_name: %s (%d건)\n",
              gram_post2023 %>% count(order_name, sort = TRUE) %>% slice(1) %>% pull(order_name),
              gram_post2023 %>% count(order_name, sort = TRUE) %>% slice(1) %>% pull(n)))
  cat(sprintf("• 가장 많은 detail_name: %s (%d건)\n",
              gram_post2023 %>% count(detail_name, sort = TRUE) %>% slice(1) %>% pull(detail_name),
              gram_post2023 %>% count(detail_name, sort = TRUE) %>% slice(1) %>% pull(n)))
  if("detail_code" %in% names(gram_post2023)) {
    cat(sprintf("• 가장 많은 detail_code: %s (%d건)\n\n",
                gram_post2023 %>% count(detail_code, sort = TRUE) %>% slice(1) %>% pull(detail_code),
                gram_post2023 %>% count(detail_code, sort = TRUE) %>% slice(1) %>% pull(n)))
  }
} else {
  cat("• G(+)/G(-) 결과 없음\n\n")
}

cat("✅ 분석 완료\n\n")