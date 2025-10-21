# 💻 Shiny App 실행 방법
## 1️⃣ 로컬 환경에서 실행하기

1. 깃허브 저장소에서 "Code → Download ZIP" 버튼을 눌러 프로젝트를 다운로드합니다.
2. 압축을 해제한 뒤, 폴더를 엽니다. (예: C:/Users/username/Downloads/2025-bigcontest-3gram/app/)
3. RStudio를 실행하고, 폴더 안의 app.R 파일을 엽니다.
4. 아래 패키지를 설치합니다 (최초 1회만 필요):
```r
install.packages(c("shinyWidgets", "shiny", "tidyverse", "tidymodels", "bonsai", "zoo", "echarts4r", "sf", "VIM"))
```
5. RStudio 상단의 [Run App] 버튼을 클릭하거나, 콘솔에서 아래 명령어를 입력합니다:
```r
shiny::runApp()
```
6. 브라우저가 자동으로 열리며, 로컬 주소에서 "CORE-AI: 성동구 소상공인 위기 진단 대시보드"를 사용할 수 있습니다.

---

## 2️⃣ 온라인 배포 버전 사용하기

- 아래 링크를 통해 웹 브라우저에서 직접 CORE-AI 대시보드를 실행할 수 있습니다.
- 🔗 [CORE-AI 성동구 소상공인 위기 진단 대시보드](https://3-gram.shinyapps.io/risk-score-dashboard/)

## ⚠️ 주의사항
- shinyapps.io의 메모리 제한으로 인해, 일부 server 기능 (매출구간 예측 결과 게이지 출력)이 생략된 부분 동작 버전입니다.
  완전한 기능을 사용하려면 로컬 환경에서 실행하는 것을 권장합니다.
- 현재 버전에서 직접 업로드 가능한 데이터 형태는 persona_a.csv와 같아야 합니다.
  즉, 기초 전처리를 거친 후 분기별 집계가 진행된 데이터여야 하며, 외부데이터와 병합된 형태여야 합니다. 
