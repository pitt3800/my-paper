# =============================================================================
# Part 2.5/3: CT 판독 소견 추출  
# =============================================================================




# Step2에서저장된 part2_ct_summary.rds  파일 불러와서 
# claude 프로젝트 'CT 판독소견 추출'로 fever focus를 찾은 다음 Step3에서 불러와야해.
#.여기서 정리된 fever focus는 정확하지 않으니 최종 inclusion lever에서 수동으로 확인해야해


ct_summary_dedup <- readRDS("cleaned_data/part2_ct_summary.rds") # Step2 에서 불러오기

write_excel_csv(ct_summary_dedup, "CT_summary.csv") #. 이 csv 파일을 claude에서 'CT 판독소견추출"분석

ct_summary_raw<-read.csv("ct_summary_analysis_result.csv", header=T ,stringsAsFactors=FALSE) 
#.분석된 csv 파일을 불러와..

.write_excel_csv( , " .csv")


ct_summary_typed <- ct_summary_raw %>%
  mutate(
    # patient_id - 문자형으로 (base_result와 동일)
    patient_id = as.character(patient_id),
    
    # visit_date - 날짜형으로 변환 (YYYY-MM-DD → Date)
    visit_date = ymd(visit_date),
    
    # 나머지 컬럼들 타입 변환
    n_ct_scans = as.integer(n_ct_scans),
    ct_findings_combined = as.character(ct_findings_combined),
    fever_focus = as.integer(fever_focus),
    disease_1 = as.character(disease_1),
    disease_2 = as.character(disease_2), 
    disease_3 = as.character(disease_3),
    year = as.integer(year),
    month = as.integer(month)
  )



saveRDS(ct_summary_typed, "cleaned_data/part2_ct_summary.rds")


