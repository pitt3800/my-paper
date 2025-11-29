# 2024_01~12 (1).xlsx
# 202301~202306.xlsx
# 202307~202312.xlsx
# 202501~.xlsx
# covid 19 이후 EMR 데이터를 정리하기

library(readxl)
library(stringr)

#1.Sample 만 뽑아서 기존파일_s 파일 만들기

# 파일 리스트
files <- c("2024_01~12 (1).xlsx", "202301~202306.xlsx", "202307~202312.xlsx", "202501~.xlsx")

for (file in files) {
  # 엑셀 파일 불러오기
  data <- read_excel(file)
  
  # 1행부터 200행까지 선택 (데이터 행 수 확인 포함)
  subset_data <- data[1:min(200, nrow(data)), ]
  
  # 새로운 파일 이름 생성 (확장자 제거 후 "_s" 붙이고 .csv 확장자 추가)
  base_name <- str_remove(file, "\\.xlsx$")
  new_file <- paste0(base_name, "_s.csv")
  
  # CSV로 저장 (행 이름 제외)
  write_excel_csv(subset_data, new_file)
}




#2. 원본파일에서 주증상과, 진단명 1만 중복없이 뽑아내기 


# 파일 리스트
files <- c("2024_01~12 (1).xlsx", "202301~202306.xlsx", "202307~202312.xlsx", "202501~.xlsx")

for (file in files) {
  # 엑셀 파일 불러오기
  data <- read_excel(file)
  
  # '주증상'과 '진단명1' 열만 선택
  selected_data <- data[, c('주증상', '진단명1')]
  
  # 중복 행 제거
  unique_data <- unique(selected_data)
  
  # 새로운 파일 이름 생성 (확장자 제거 후 "_unique" 붙이고 .csv 확장자 추가)
  base_name <- str_remove(file, "\\.xlsx$")
  new_file <- paste0(base_name, "_unique.csv")
  
  # CSV로 저장 (행 이름 제외)
  write_excel_csv(unique_data, new_file)
}


files <- c("fever_including.xlsx", "base_result.xlsx", "fever_lab.xlsx", "ct.xlsx","nurse.xlsx")

for (file in files) {
  # 엑셀 파일 불러오기
  data <- read_excel(file)
  
  # 1행부터 200행까지 선택 (데이터 행 수 확인 포함)
  subset_data <- data[1:min(200, nrow(data)), ]
  
  # 새로운 파일 이름 생성 (확장자 제거 후 "_s" 붙이고 .csv 확장자 추가)
  base_name <- str_remove(file, "\\.xlsx$")
  new_file <- paste0(base_name, "_request.csv")
  
  # CSV로 저장 (행 이름 제외)
  write_excel_csv(subset_data, new_file)
}

