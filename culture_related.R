#==============================================================================
# 혈액배양 검사 코드 확인 # 2023-07-01부터 데이터에서 G(+)/G(-) 찾기
#.v3-part1_supple_lab_standard2.R 참고 코딩
#==============================================================================

library(tidyverse)


setwd('/Users/youjinlee/Documents/My R/fever paper/filtered_data')
fever_lab <- read_csv("lab_filtered.csv", 
                      locale = locale(encoding = "UTF-8"))


fever_lab <- fever_lab %>%
  mutate(내원일자 = ymd(내원일자)) %>%     # 기존 열을 덮어쓰기
  filter(내원일자 >= ymd("2023-07-01"))     # 2023.7.1 이후만 포함

# 2. 처방코드 DB0196 확인

db0196_rows <- fever_lab %>% 
  filter(처방코드 == "DB0196")

db0196_rows %>% 
  count(처방명) %>% 
  print(n = 20)

# 3. 세부검사코드 ABACT 확인  
cat("\n=== 세부검사코드 ABACT 확인 ===\n")
abact_rows <- fever_lab %>% 
  filter(세부검사코드 == "ABACT")

abact_rows %>% 
  count(세부검사코드) %>% 
  print(n = 20)

# 4. DB0196 + ABACT 조합 확인
cat("\n=== DB0196 + ABACT 조합 ===\n")
both <- fever_lab %>% 
  filter(처방코드 == "DB0196" & 세부검사코드 == "ABACT")


# 5. 결과값 패턴 확인 (G+, G- 있는지)
if(nrow(abact_rows) > 0) {
  cat("\n=== ABACT 결과값 패턴 ===\n")
  abact_rows %>% 
    select(등록번호, 결과) %>% 
    head(20) %>% 
    print()
  
  # G(+), G(-), Yeast, Negative 패턴 검색 및 분류
  cat("\n결과값 상세 분류:\n")
  result_patterns <- abact_rows %>%
    mutate(
      has_gplus = str_detect(결과, regex("g\\(\\+\\)|g\\+|gram.*positive", 
                                       ignore_case = TRUE)),
      has_gminus = str_detect(결과, regex("g\\(\\-\\)|g\\-|gram.*negative", 
                                        ignore_case = TRUE)),
      has_yeast = str_detect(결과, regex("yeast", 
                                       ignore_case = TRUE)),
      has_negative = str_detect(결과, regex("negative|no.*growth", 
                                          ignore_case = TRUE)),
      
      # 최종 분류
      culture_result = case_when(
        has_negative ~ "Negative",
        has_yeast ~ "Yeast",
        has_gplus & has_gminus ~ "Mixed (G+ & G-)",
        has_gplus ~ "Gram-positive",
        has_gminus ~ "Gram-negative",
        TRUE ~ "Other"
      )
    )
  
  # 집계 결과
  result_summary <- result_patterns %>%
    summarise(
      G_plus = sum(has_gplus, na.rm = TRUE),
      G_minus = sum(has_gminus, na.rm = TRUE),
      Yeast = sum(has_yeast, na.rm = TRUE),
      Negative = sum(has_negative, na.rm = TRUE),
      Mixed = sum(has_gplus & has_gminus, na.rm = TRUE),
      Total = n()
    )
  
  print(result_summary)
  
  # 최종 분류별 빈도표
  cat("\n최종 분류별 분포:\n")
  result_patterns %>%
    count(culture_result) %>%
    mutate(percentage = round(n / sum(n) * 100, 1)) %>%
    print()
}










