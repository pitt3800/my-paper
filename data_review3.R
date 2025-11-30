# combined 파일에 있는 목록이 fever_lab에 들어가 있나?

------------------------------------------------------------------------------------------------------------------------
  # Fever Lab 데이터
  ---  ---------------------------------------------------------------------------------------------------------------------------
  input_file <- "ER_LAB_RSLT.csv"  
  
  fever_lab <- read_csv(
    input_file,
    locale = locale(encoding = "CP949"),
    show_col_types = FALSE
  )
  
  combined_info <- combined %>%
    select(등록번호, 내원일자, 내원시간,환자명) %>%
    distinct() %>%
    arrange(내원일자,내원시간,환자명 )
  
  write_excel_csv(combined_info , "combined_info.csv")
  
  
  
  lab_info <- fever_lab %>%
    select(등록번호, 내원일자, 내원시간,환자명) %>%
    distinct() %>%
    arrange(내원일자,내원시간,환자명 )
  
  
  
  
  
  
  
  # 모든 key 컬럼을 character로 변환
  combined <- combined_info %>%
    mutate(
      등록번호 = as.character(등록번호),
      내원일자 = as.character(내원일자),
      내원시간 = as.character(내원시간)
    )
  
  lab <- lab_info %>%
    mutate(
      등록번호 = as.character(등록번호),
      내원일자 = as.character(내원일자),
      내원시간 = as.character(내원시간)
    )
  
  # 타입 확인
  str(combined)
  str(lab)
  
  # 3. Lab 검사 플래그 추가 ----
  lab_flag <- lab %>%
    mutate(lab_done = 1) %>%
    select(등록번호, 내원일자, 내원시간, lab_done)
  
  # 4. 병합 (left_join) ----
  combined_final <- combined %>%
    left_join(
      lab_flag,
      by = c("등록번호", "내원일자", "내원시간")
    ) %>%
    mutate(
      lab_done = if_else(is.na(lab_done), 0, lab_done)
    )
  
  # 결과 확인
  combined_final
  
  write_excel_csv(combined_final,"compare.csv")
  
  