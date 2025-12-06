#===============================================================================================
  # v3-part0_data_review2.R에서 만든 CT_ChestAbdomen.csv, CT_ChestAbdomen.rsd 불러와
  
  #.이 파일을 기준으로 fever_lab , combined filtering 하기 즉 CT 찍은 사람 중에서 lab한 사람들만 골라내기
  #.matching_data  폴더에 2개 ㅍ일 만들기
  #.sample_r2 다시 만들기- AI로 코드 만들기 위해
#=======================================================================================================

#. 데이터 불러오기

combined <-readRDS("2017_2025_raw_data/combined.rds")
fever_lab <-readRDS("2017_2025_raw_data/fever_lab.rds")

CT_ChestAbdomen <-readRDS("2017_2025_raw_data/CT_ChestAbdomen.rds")

CT_ChestAbdomen <- CT_ChestAbdomen %>%
  rename(환자명 = 성명)

#. 비교하기 위해 모든 변수 class 바꾸기


combined <- combined %>%
  mutate(
    등록번호 = as.character(등록번호),
    환자명 = as.character(환자명),
    내원일자 = ymd(내원일자)
  )

fever_lab <- fever_lab %>%
  mutate(
    등록번호 = as.character(등록번호),
    환자명 = as.character(환자명),
    내원일자 = ymd(내원일자)
  )


CT_ChestAbdomen <- CT_ChestAbdomen %>%
  mutate(
    등록번호 = as.character(등록번호),
    환자명 = as.character(환자명),
    검사일자 = ymd(검사일자)
  )



# 1. CT_ChestAbdomen에서 환자 목록 추출
# CT 검사를 받은 환자의 등록번호, 성명, 검사일자 추출
CT_patients <- CT_ChestAbdomen %>%
  select(등록번호, 환자명, 검사일자) %>%
  distinct()

cat("CT 검사 환자 수:", nrow(CT_patients), "\n")
CT_patients %>% head()


# 2. combined 데이터에서 CT 환자만 필터링
# 등록번호와 환자명이 모두 일치하는 행만 선택
combined_ct <- combined %>%
  inner_join(CT_patients, 
             by = c("등록번호" = "등록번호", "환자명" = "환자명"))

cat("\n=== combined 필터링 결과 ===\n")
cat("필터링 전:", nrow(combined), "행\n")
cat("필터링 후:", nrow(combined_ct), "행\n\n")


# 3. combined에서 CT 검사일자와 가장 가까운 내원일자만 선택
# 날짜 차이 절대값을 계산하여 가장 작은 것만 선택
combined_closest <- combined_ct %>%
  mutate(날짜차이 = abs(내원일자 - 검사일자)) %>%
  group_by(등록번호, 검사일자) %>%
  slice_min(날짜차이, n = 1, with_ties = FALSE) %>%  # 동일 차이 시 첫 번째만
  ungroup() %>%
  select(-날짜차이)  # 임시 변수 제거

cat("=== 가장 가까운 내원일자만 선택 ===\n")
cat("선택 전:", nrow(combined_ct), "행\n")
cat("선택 후:", nrow(combined_closest), "행\n\n")

# 환자별 내원일자 확인
combined_closest %>%
  count(등록번호, name = "방문횟수") %>%
  count(방문횟수, name = "환자수")

#.CT_ChestAbdomen 가 combined_closest 와 비교하여 행 하나가 더 많네..그래서 확인해보자.
CT_ChestAbdomen_only <- CT_ChestAbdomen %>%
  anti_join(combined_closest, by = "등록번호")
CT_ChestAbdomen_only






# 4. fever_lab 데이터에서 CT 환자만 필터링
fever_lab_ct <- fever_lab %>%
  inner_join(CT_patients, 
             by = c("등록번호" = "등록번호", "환자명" = "환자명"))

cat("\n=== fever_lab 필터링 결과 ===\n")
cat("필터링 전:", nrow(fever_lab), "행\n")
cat("필터링 후:", nrow(fever_lab_ct), "행\n\n")


# 5. fever_lab에서 CT 검사일자와 가장 가까운 내원일자만 선택
fever_lab_closest <- fever_lab_ct %>%
  mutate(날짜차이 = abs(내원일자 - 검사일자)) %>%
  group_by(등록번호, 검사일자, 세부검사코드) %>%  # 검사항목별로도 구분
  slice_min(날짜차이, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-날짜차이)

cat("=== 가장 가까운 내원일자만 선택 ===\n")
cat("선택 전:", nrow(fever_lab_ct), "행\n")
cat("선택 후:", nrow(fever_lab_closest), "행\n\n")

# 환자별 검사 수 확인
fever_lab_closest %>%
  count(등록번호, name = "검사항목수") %>%
  summary()


# 6. 결과 검증
cat("\n=== 결과 검증 ===\n")

# 6-1. CT 환자 수와 일치 확인
cat("CT 환자 수:", n_distinct(CT_patients$등록번호), "\n")
cat("combined_closest 환자 수:", n_distinct(combined_closest$등록번호), "\n")
cat("fever_lab_closest 환자 수:", n_distinct(fever_lab_closest$등록번호), "\n\n")

# 6-2. 샘플 환자의 날짜 매칭 확인 (첫 5명)
sample_check <- combined_closest %>%
  select(등록번호, 환자명, 내원일자, 검사일자) %>%
  head(5)

cat("샘플 환자의 내원일자-검사일자 매칭:\n")
sample_check %>%
  mutate(날짜차이 = abs(내원일자 - 검사일자)) %>%
  print()

# 6-3. 내원일자와 검사일자 차이 분포
cat("\n내원일자-검사일자 차이 (일) 분포:\n")
combined_closest %>%
  mutate(날짜차이_일 = abs(내원일자 - 검사일자)) %>%
  pull(날짜차이_일) %>%
  summary()


#.AI 를 위한 sample 뽑기

set.seed(123) 


# 0>CT_ChestAbdomen 에서 100명 무작위 추출
CT_ChestAbdomen_sample <-CT_ChestAbdomen %>%
  sample_n(100)

# 1) 추출된 등록번호만 벡터로 저장
selected_ids <-CT_ChestAbdomen_sample$등록번호

# 3) fever_lab_CT_matching에서 동일 환자만 필터링
fever_lab_matching_sample <- fever_lab_closest %>%
  filter(등록번호 %in% selected_ids)

# 4) combined_CT_matching에서 100명 무작위 추출
combined_CT_matching_sample <- combined_closest %>%
  filter(등록번호 %in% selected_ids)




# 7. 결과 저장
saveRDS(combined_closest, "matching_data/combined_CT_matching.rds")
saveRDS(fever_lab_closest, "matching_data/fever_lab_CT_matching.rds")
saveRDS(CT_ChestAbdomen, "matching_data/CT_ChestAbdomen_matching.rds") #.이 파일은 바뀐게 없으나 단계별로 같은 폴더에 관리하기 쉽게 만음

write_csv(combined_closest, "matching_data/combined_CT_matching.csv")
write_csv(fever_lab_closest, "matching_data/fever_lab_CT_matching.csv")
write_csv(CT_ChestAbdomen, "matching_data/CT_ChestAbdomen_matching.csv")

saveRDS(combined_CT_matching_sample, "matching_sample/combined_CT_matching_sample.rds")
saveRDS(fever_lab_matching_sample, "matching_sample/fever_lab_matching_sample.rds")
saveRDS(CT_ChestAbdomen_sample, "matching_sample/CT_ChestAbdomen_sample.rds")

write_csv(CT_ChestAbdomen_sample, "matching_sample/CT_ChestAbdomen_sample.csv")
write_csv(fever_lab_matching_sample, "matching_sample/fever_lab_matching_sample.csv")
write_csv(combined_CT_matching_sample, "matching_sample/combined_CT_matching_sample.csv")
