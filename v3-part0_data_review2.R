#.목표
#.CT12 정리 : 이 자료가 가장 중요하고 적은 자료니깐. 이걸 중심으로 
#.데이터 소실 최소화 하기 위해 CT 찍은 환자 먼저 정리 후 이 환자 중 lab한 사람으로 
#.CT12 에서 APCT, chest CT 관련된 CT만 남기기
#.APCT, chest CT 둘다 찍은 환자만 filtering 이후 둘다 찍은 날만 남기기
#.CT_ChestAbdomen.csv, CT_ChestAbdomen.rds 남기기
# 이 파일은 1874명 CT 찍은 환자..만 추렸어..


#.이전 데이터 불러오기

setwd('/Users/youjinlee/Documents/My R/fever paper')

fever_lab <-readRDS("2017_2025_raw_data/fever_lab.rds")
ct12 <-readRDS("2017_2025_raw_data/ct12.rds")
combined <-readRDS("2017_2025_raw_data/combined.rds")



#==============================================================================
# CT 필터링: Chest + Abdomen CT 동시 촬영 환자 선택 (첫 방문 우선)
# 목표: 
#   1. Brain CT  & Other 제거
#   2. Chest CT + Abdomen CT 둘 다 촬영한 환자만 선택
#   3. 여러번 내원 시 → Chest+Abdomen 둘 다 찍은 가장 이른 날짜만 포함
#==============================================================================


# 2. CT 유형 분류 및 날짜 변환 =================================================
ct_classified <- ct12 %>%
  mutate(
    # 검사일자 날짜 형식으로 변환
    exam_date = ymd(as.character(검사일자)),
    
    # 코드명을 소문자로 변환
    코드명_lower = tolower(코드명),
    
    # CT 유형 분류
    CT_type = case_when(
      # Brain CT or other 패턴
      str_detect(코드명_lower, "brain|head|두부|neck|facial|spine|extremity|face|temporal|OMU|orbit") ~ "Other",
      
      # Chest CT 패턴 (enhanced 여부 무관)
      str_detect(코드명_lower, "chest|thorax|흉부|pulnonary|aortic") ~ "Chest",
      
      # Abdomen CT 패턴 (enhanced 여부 무관)
      str_detect(코드명_lower, "abdomen|abdominal|복부|liver|간|pelvis|urinary") ~ "Abdomen",
      
      # 기타
      TRUE ~ "Other"
    )
  )

# CT 유형별 건수
ct_classified %>% 
  count(CT_type, sort = TRUE)

# 3. Brain CT & Other 제거 ============================================================
ct_no_brain <- ct_classified %>%
  filter(CT_type != "Other")

cat(sprintf("Brain& Other CT 제거 후: %d건, %d명\n\n", 
            nrow(ct_no_brain), n_distinct(ct_no_brain$등록번호)))

# 4. 환자별-날짜별 CT 유형 집계 ===============================================
# 각 환자의 각 날짜에 어떤 CT를 촬영했는지 확인
patient_date_summary <- ct_no_brain %>%
  group_by(등록번호,성명,exam_date) %>%
  summarize(
    # 해당 날짜에 촬영한 CT 유형
    has_Chest = any(CT_type == "Chest"),
    has_Abdomen = any(CT_type == "Abdomen"),
    both_CT = has_Chest & has_Abdomen,  # 둘 다 촬영 여부
    
    # CT 건수
    n_CT = n(),
    
    .groups = "drop"
  )

# 결과 확인
patient_date_summary

# 5. Chest + Abdomen 둘 다 촬영한 날짜만 선택 ==================================
dates_with_both <- patient_date_summary %>%
  filter(both_CT == TRUE)

cat(sprintf("Chest+Abdomen 둘 다 촬영한 날짜: %d건 (%d명)\n\n", 
            nrow(dates_with_both), n_distinct(dates_with_both$등록번호)))

dates_with_both

# 6. 각 환자별로 가장 이른 날짜 선택 ==========================================
first_visit_per_patient <- dates_with_both %>%
  group_by(등록번호) %>%
  arrange(exam_date) %>%  # 날짜 오름차순 정렬
  slice(1) %>%  # 첫 번째 행(가장 이른 날짜)만 선택
  ungroup()

cat(sprintf("최종 선정: %d명 (각 환자의 첫 Chest+Abdomen 촬영일)\n\n", 
            nrow(first_visit_per_patient)))

first_visit_per_patient

# 7. 해당 날짜의 CT 기록만 추출 ===============================================
ct_final <- ct_no_brain %>%
  # 환자번호-날짜 조합이 first_visit_per_patient에 있는 것만 선택
  semi_join(
    first_visit_per_patient %>% select(등록번호, exam_date),
    by = c("등록번호", "exam_date")
  ) %>%
  # CT_type == Brain, Other 제거 (Chest, Abdomen만)
  filter(CT_type %in% c("Chest", "Abdomen")) %>%
  # 정렬 (환자번호, 날짜, CT유형 순)
  arrange(등록번호, exam_date, CT_type)

cat(sprintf("최종 CT 기록: %d건 (%d명)\n\n", 
            nrow(ct_final), n_distinct(ct_final$등록번호)))

ct_final

#.CT 판독 소견을 합쳐서 하나의 행으로 만들어
ct_final <- ct_final %>%
  group_by(등록번호, 성명, 검사일자) %>%
  summarize(
    # 접수시간: 첫 번째 값 사용
    접수시간 = first(접수시간),
    
    # 판독: 모든 판독 내용을 "\n\n[구분선]\n\n"으로 구분하여 결합
    판독 = paste(
      paste0("[", CT_type, " CT]\n", 판독),
      collapse = "\n\n=====================================\n\n"
    ),
    
    .groups = "drop"
  ) %>%
  # 열 순서 정리
  select(등록번호, 성명, 검사일자, 접수시간, 판독)

cat(sprintf("통합 후: %d건 (%d명)\n\n", 
            nrow(ct_final), n_distinct(ct_final$등록번호)))

# 결과 확인
ct_final



# 10. 결과 저장 ===============================================================

write_excel_csv(ct_final, "2017_2025_raw_data/CT_ChestAbdomen.csv")
saveRDS(ct_final, "2017_2025_raw_data/CT_ChestAbdomen.rds")  # 분석용 컬럼 포함



# 11. 최종 검증 ===============================================================
# 각 환자가 정확히 1개의 날짜만 가지고 있는지 확인
visit_count <- ct_final %>%
  distinct(등록번호, exam_date) %>%
  count(등록번호, name = "n_dates")

if (all(visit_count$n_dates == 1)) {
  cat("✅ 검증 통과: 모든 환자가 정확히 1개 날짜만 보유\n")
} else {
  cat("⚠️ 경고: 일부 환자가 여러 날짜 보유\n")
  visit_count %>% filter(n_dates > 1) %>% print()
}

# 각 환자-날짜 조합이 Chest와 Abdomen 둘 다 가지는지 확인
final_check <- ct_final %>%
  group_by(등록번호, exam_date) %>%
  summarize(
    has_Chest = any(CT_type == "Chest"),
    has_Abdomen = any(CT_type == "Abdomen"),
    both_CT = has_Chest & has_Abdomen,
    .groups = "drop"
  )

if (all(final_check$both_CT)) {
  cat("✅ 검증 통과: 모든 날짜가 Chest + Abdomen 포함\n\n")
} else {
  cat("⚠️ 경고: 일부 날짜가 둘 다 미포함\n")
  final_check %>% filter(!both_CT) %>% print()
}

#==============================================================================
# ✅ 완료
# 
# 처리 규칙:
# 1. Brain CT 완전 제거
# 2. Chest CT + Abdomen CT 둘 다 촬영한 환자만 선택
# 3. 여러번 내원 → 둘 다 찍은 가장 이른 날짜만 포함
# 4. 그 날짜의 Chest, Abdomen CT만 포함 (Other 제외)
#==============================================================================







