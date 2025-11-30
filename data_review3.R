# 두 combined 파일에 있는 목록이 fever_lab에 들어가 있나?

------------------------------------------------------------------------------------------------------------------------
  # Fever Lab 데이터
  ---  ---------------------------------------------------------------------------------------------------------------------------
  input_file <- "ER_LAB_RSLT.csv"  
  
  fever_lab <- read_csv(
    input_file,
    locale = locale(encoding = "CP949"),
    show_col_types = FALSE
  )
  

patient_info <- fever_lab %>%
  select(등록번호, 내원일자, 내원시간,환자명) %>%
  distinct() %>%
  arrange(내원일자,내원시간,환자명 )

combined <- combined %>%
select(등록번호, 내원일자, 내원시간,환자명) %>%
  distinct() %>%
  arrange(내원일자,내원시간,환자명 )