# CORE-AI: Composite Of Risk Ensemble-AI
**앙상블 아키텍처를 활용한 가맹점 위험 요인 분해 모델**을 개발하고,
이를 활용하여 **성동구 소상공인 위기 진단 대시보드**를 구현합니다.

제13회 빅콘테스트 2025 AI·데이터 경진대회 AI데이터분석 분야 <Team. 쓰리그램(3-gram)>

---

## 🔹 주요 기능
- 경영 위기를 단일 현상이 아닌, **매출/고객/상권**의 세 가지 요인으로 분해
- **상호보완적 앙상블**을 통해 가맹점의 종합적 위기 수준을 보다 정밀하게 진단
- **요인별 위험 스코어**와 **종합 위험 스코어**로 가맹점의 위기를 직관적으로 확인
- 4개의 탭으로 구성된 **성동구 소상공인 위기 진단 대시보드**를 구현하여, 위기진단과 맞춤형 솔루션 제공

---

## 🔹 모델 개요


---

## 🔹 프로젝트 구조

🗂️2025-bigcontest-3gram/  
├── app/  
│   ├── models/                         
│   │   ├── my_sales_models_2.rds            # 매출 위험 스코어 예측 모델  
│   │   ├── my_cust_models_4class.rds        # 고객 패턴 분류 모델  
│   │   ├── my_mkt_models_2.rds              # 상권 유형 분류 모델  
│   │   └── sales_model_xgb_regression.rds   # 매출구간 예측 모델    
│   └── app.R                                # R shinyapp 실행                       
├── data/  
│   ├── raw/          # 원본 데이터  
│   ├── processed/    # 전처리 후 저장  
│   └── external/     # 외부 데이터   
├──scripts/  
   ├── 01_load_packages.R           # 패키지 로드  
   ├── 02_basic_preprocessing.R     # 데이터 전처리 함수  
   ├── 03_data_pipeline_models.R    # 모델별 데이터 파이프라인  
   ├── 04_train_models.R            # 위험 요인별 모델 학습  
   ├── 05_predict.R                 # 예측 및 위험 스코어 계산  
   └── 06_final_model.R             # 최종 모델 앙상블  

---

## 🔹 Shiny App 화면 예시
<img width="1500" height="917" alt="image" src="https://github.com/user-attachments/assets/7b4afe82-3b84-483d-b51f-c8993d9cdd1e" />


- `/app` 폴더에서 Shiny App에 대한 자세한 설명을 확인할 수 있습니다.

- 아래 링크를 통해 웹 브라우저에서 직접 CORE-AI 대시보드를 실행할 수 있습니다.  
  🔗 [CORE-AI 성동구 소상공인 위기 진단 대시보드](https://3-gram.shinyapps.io/risk-score-dashboard/)

---

## 🔹 보고서

---

