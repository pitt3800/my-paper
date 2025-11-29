# ===============================================================================
# R 패키지 충돌 방지 설정
# 모든 분석 스크립트 실행 전에 이 파일을 먼저 source 하세요
# ===============================================================================

cat("패키지 로드 및 충돌 방지 설정 중...\n\n")

# 1. 기존에 로드된 문제 패키지 제거
if("MASS" %in% (.packages())) {
  detach("package:MASS", unload = TRUE)
  cat("✓ MASS 패키지 언로드 완료\n")
}

# 2. 필수 패키지 로드
required_packages <- c("tidyverse", "readr", "writexl", "stringr", "lubridate")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE, warn.conflicts = FALSE)
}

cat("✓ 기본 패키지 로드 완료\n")

# 3. conflicted 패키지로 충돌 관리 (권장)
if(!require("conflicted", quietly = TRUE)) {
  install.packages("conflicted")
}
library(conflicted)

# 4. 충돌 함수에 대한 우선순위 설정
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("summarise", "dplyr")



# 5. 현재 충돌 상황 확인
cat("현재 select 함수 위치:\n")
print(find("select"))



# 필요한 패키지 설치 확인 및 로드
packages <- c("tidyverse", "readr", "writexl", "stringr", "lubridate")

# 패키지 설치 함수
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# 패키지 로드
invisible(lapply(packages, install_if_missing))