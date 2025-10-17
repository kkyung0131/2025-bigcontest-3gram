## 외부데이터 설명
### 1. 202301_202412_주민등록인구및세대현황_월간.csv
- 출처: [https://jumin.mois.go.kr/](https://jumin.mois.go.kr/)
- 수집 방법
  -  행정구역: 서울시 성동구
  -  등록구분: 전체
  -  조회기간: 월간 2023년 1월 ~ 2024년 12월
- 진행한 전처리
  - 행정동별 총 인구수 데이터만 select한 후 분기별 집계
  - 변수명: pop_all_mean, pop_all_std
  
### 2. 서울시 상권분석서비스(길단위인구-행정동).csv
- 출처: [https://data.seoul.go.kr/dataList/OA-15568/S/1/datasetView.do](https://data.seoul.go.kr/dataList/OA-15568/S/1/datasetView.do)
- 진행한 전처리
  - 데이터셋 행정동 명칭 통일하여 아래 8개의 변수 데이터만 select 
  - 변수명: move_num, m_move_num, f_move_num, age_20_under_move_num, age_30_move_num, age_40_move_num, age_50_move_num, age_60_over_move_num

### 3. 서울시 상권분석서비스(상주인구-행정동).csv
- 출처: [https://data.seoul.go.kr/dataList/OA-15584/S/1/datasetView.do](https://data.seoul.go.kr/dataList/OA-15584/S/1/datasetView.do)
- 진행한 전처리
  - 데이터셋 행정동 명칭 통일하여 아래 8개의 변수 데이터만 select 
  - 변수명: resid_num, m_resid_num, f_resid_num, age_20_under_resid_num, age_30_resid_num, age_40_resid_num, age_50_resid_num, age_60_over_resid_num

### 4. 서울시 상권분석서비스(직장인구-행정동).csv
- 출처: [https://data.seoul.go.kr/dataList/OA-15569/S/1/datasetView.do](https://data.seoul.go.kr/dataList/OA-15569/S/1/datasetView.do)
- 진행한 전처리
  - 데이터셋 행정동 명칭 통일하여 아래 8개의 변수 데이터만 select 
  - 변수명: office_num, m_office_num, f_office_num, age_20_under_office_num, age_30_office_num, age_40_office_num, age_50_office_num, age_60_over_office_num

### 5.






