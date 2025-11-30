#.fever_lab 파일과 combined 파일에 등록번호가 같은가? combined 파일의 환자명단에서 lab있는지
#.lab여부.csv 파일 만들기


fever_lab <-readRDS("fever_lab.rds")
ct12 <-readRDS("ct12.rds")
combined <-readRDS("combined.rds")


combined_ids <- unique(combined$등록번호)
lab_ids <- unique(fever_lab$등록번호)


# Combined 등록번호 (중복 제거)
combined_ids <- combined %>% 
  distinct(등록번호) %>% 
  pull(등록번호) %>% 
  as.character()

# Fever Lab 등록번호 (중복 제거)
# 변수명이 다를 수 있으므로 확인

lab_ids <- fever_lab %>% 
  distinct(등록번호) %>% 
  pull(등록번호) %>% 
  as.character()

# Combined에는 있지만 Lab에는 없는 등록번호
missing_in_lab <- setdiff(combined_ids, lab_ids)

missing_in_combined <- setdiff(lab_ids, combined_ids)


#===============================================================================================
# 그럼 실질적으로 어떤 환자가 없는지 전체 데이트 프레임에서 보자..
#===============================================================================================
fever_lab_info <- fever_lab %>%
  mutate(
    등록번호 = as.character(등록번호),
    내원일자 = as.character(내원일자),
    내원시간 = as.character(내원시간)
  ) %>%
  distinct(등록번호, 내원일자, 내원시간) # 중복 제거

# 모든 key 컬럼을 character로 변환
combined_info<- combined %>%
  mutate(
    등록번호 = as.character(등록번호),
    내원일자 = as.character(내원일자),
    내원시간 = as.character(내원시간)
  )

# 3. Lab 검사 플래그 추가 ----
fever_lab_flag <- fever_lab_info %>%
  mutate(lab_done = 1) %>%
  select(등록번호, 내원일자, 내원시간, lab_done) #.fever_lab 등록번호가 있다. 그럼 검사가 있다라고생각

# 4. 병합 (left_join) ----
combined_final <- combined_info %>%
  left_join(
    fever_lab_flag,
    by = c("등록번호", "내원일자", "내원시간")
  ) %>%
  mutate(
    lab_done = if_else(is.na(lab_done), 0, lab_done)
  )

# 결과 확인
combined_final

combined_final_simple <-combined_final %>%
  select(등록번호, 환자명,내원일자, 내원시간,주증상1, lab_done)
combined_final_simple

write_excel_csv(combined_final_simple, "lab여부.csv")
