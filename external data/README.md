## 외부데이터 설명
1. 202301_202412_주민등록인구및세대현황_월간.csv
- 출처: [https://jumin.mois.go.kr/](https://jumin.mois.go.kr/)
- 수집 방법
  -  행정구역: 서울시 성동구
  -  등록구분: 전체
  -  조회기간: 월간 2023년 1월 ~ 2024년 12월
- 진행한 전처리
  - 행정동별 총 인구수 데이터만 select한 후 분기별 집계
  - 변수명: pop_all_mean, pop_all_std

2. 서울시 상권분석서비스(길단위인구-행정동).csv
- 출처: [https://data.seoul.go.kr/dataList/OA-15568/S/1/datasetView.do](https://data.seoul.go.kr/dataList/OA-15568/S/1/datasetView.do)
- 진행한 전처리
  - 데이터셋 행정동 명칭 통일
  - 성별별 연령대별 인구수에서 성별별 인구수(m/f)와 연령대별 인구수(age_20~60) 변수 생성
  - 변수명: resid_num, m_resid_num, f_resid_num, age_20_under_resid_num, age_30_resid_num, age_40_resid_num, age_50_resid_num, age_60_over_resid_num

3. 서울시 상권분석서비스(상주인구-행정동).csv
  

5. 서울시 상권분석서비스(직장인구-행정동).csv
