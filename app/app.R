
# ======================================================================
# 📊 CORE-AI를 활용한 성동구 소상공인 위기 진단 대시보드 
# ======================================================================

# ==============================================================================
# 0. 실행 준비
# ==============================================================================
library(shinyWidgets)
library(shiny)
library(tidyverse)
library(tidymodels)
library(bonsai)
library(zoo)
library(echarts4r)
library(sf)

source("final_newdata_pipeline.R")

seoul_dong <- st_read("./hangjeongdong_서울특별시.geojson")
area_risk_weights <- c(4, 3, 1, 2)
top_variables <- c(
  "sim_store_cnt", "store_cnt", "franchise_cnt", "pop_all_std",
  "move_num", "age_30_move_num", "rent", "age_20_under_move_num",
  "f_move_num", "m_move_num"
)
dong_replacements <- c("왕십리2동"="왕십리제2동", "성수1가1동"="성수1가제1동", "성수1가2동"="성수1가제2동",
                       "성수2가1동"="성수2가제1동", "성수2가3동"="성수2가제3동", "행당1동"="행당제1동","행당2동"="행당제2동", "금호2·3가동"="금호2.3가동")

time_slider_labels <- c("1분기 전", "현재 분기", "1분기 후", "2분기 후", "3분기 후", "4분기 후")
cluster_info <- list(
  "혼란-경쟁 상권" = "새로운 가게의 진입과 이탈이 매우 활발해 경쟁이 치열하고 변동성이 높은 시장입니다. 불안정한 시장 진입과 이탈이 반복되어 위험도가 매우 높습니다.",
  "성숙-쇠퇴 상권" = "규모가 가장 크지만, 성장이 멈추고 쇠퇴기에 진입한 상권입니다. 시장이 포화되어 경쟁력이 약한 가게들이 문을 닫고 있는 중입니다.",
  "활성화-성장 상권" = "새로운 가게가 활발하게 문을 열고 있는, 성장 잠재력이 큰 상권입니다. 상권 전체에 활력이 넘치고 있습니다.",
  "일시적 변동 상권" = "일시적인 개폐업 변동성을 보이는 상권입니다. 갑작스러운 변화가 예측 불가능한 위험 요인이 될 수 있습니다."
)

seongdong_dong_simple <- seoul_dong %>%
  filter(startsWith(adm_nm, "서울특별시 성동구")) %>%
  mutate(
    adm_nm_short = str_replace(adm_nm, "서울특별시 성동구 ", ""),
    adm_nm_short = str_replace_all(adm_nm_short, dong_replacements)
  ) %>%
  select(adm_nm_short, geometry) %>%          # 불필요 컬럼 제거
  st_simplify(dTolerance = 50)               # 메모리/렌더링 최적화



# ==============================================================================
# 1. 모델 로드
# ==============================================================================

sales_risk_model_loaded <- readRDS("./models/my_sales_models_2.rds")
sales_model_loaded <- readRDS("./models/sales_model_xgb_regression.rds")
cust_model_loaded <- readRDS("./models/my_cust_models_4class.rds")
mkt_model_loaded <- readRDS("./models/my_mkt_models_2.rds")

rec <- extract_recipe(sales_model_loaded[[1]]$model)
used_vars <- rec$var_info %>%
  filter(role == "predictor") %>%
  pull(variable)


# ==============================================================================
# 2. 최종 위험 점수 계산 함수
# ==============================================================================

final_risk_model <- function(new_data, w1 = 0.3333, w2 = 0.3333, w3 = 0.3333) {

  if (is.null(new_data) || nrow(new_data) == 0) {
    return(list(
      final_score = NA_real_,
      component_scores = data.frame(category = c("고객", "매출", "상권"), score = NA_real_)
    ))
  }

  processed_cust_data <- process_pipeline_cust_newdata(new_data)
  processed_mkt_data <- process_pipeline_mkt_newdata(new_data)

  final_input_sales <- new_data %>% filter(row_number() == n())
  final_input_cust <- processed_cust_data %>% filter(row_number() == n())
  final_input_row <- processed_mkt_data %>% filter(row_number() == n())

  prediction_sales <- predict_score_sales(sales_risk_model_loaded, final_input_sales)
  prediction_cust <- predict_score_cust(cust_model_loaded, final_input_cust)
  prediction_mkt <- predict_score_mkt(mkt_model_loaded, final_input_row)

  sales_score_raw <- prediction_sales$sales_risk_pred
  cust_score_raw <- prediction_cust$risk_score_scaled
  mkt_score_raw <- prediction_mkt$risk_score

  final_risk_score <- (w1 * sales_score_raw + w2 * cust_score_raw + w3 * mkt_score_raw) * 100

  component_scores_df <- data.frame(
    category = c("매출", "고객", "상권"),
    score = c(sales_score_raw, cust_score_raw, mkt_score_raw) * 100
  )

  list(
    final_score = round(final_risk_score),
    component_scores = component_scores_df
  )
}


# ==============================================================================
# 3. UI 정의
# ==============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Noto+Sans+KR:wght@300;400;700&display=swap');
    body { font-family: 'Noto Sans KR', sans-serif; background-color: #f8f9fa; }

    .navbar { background-color: #00462A !important; border-color: #00462A !important; padding: 10px 0; }
    .navbar-header .navbar-brand { color: white !important; font-weight: 700; font-size: 24px; margin-left: 20px; }
    .sidebar { background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.05); border-right: 2px solid #e0e0e0; }

    .selectize-input.focus {border-color: #007043 !important; box-shadow: 0 0 0 0.2rem rgba(0, 112, 67, 0.25) !important; }
    .selectize-dropdown-content .option.active {background-color: #00462A; color: white;}
    .selectize-dropdown .option.selected {background-color: #c8e6c9 !important; color: #1b5e20 !important; font-weight: 700;}
    .selectize-input.items.not-empty.has-options.full {border-color: #00462A; box-shadow: 0 0 0 0.2rem rgba(76, 175, 80, 0.25);}
    .selectize-input.items.not-empty.has-options.full .item {color: #ffffff;}


    .score-card {border: 2px solid #e0e0e0; border-radius: 10px;padding: 20px; background-color: #ffffff; box-shadow: 0 3px 8px rgba(0,0,0,0.08);}
    .score-title { font-weight:700; font-size:24px; color:#00462A; margin-bottom:15px; text-align:center; }

    .nav-pills > li > a { color: #00462A !important; font-weight:600; }
    .nav-pills > li > a:hover { color: #007043 !important; }
    .nav-pills > li.active > a { color: #fff !important; background-color: #00462A !important; }

    .analysis-button { background-color: #00462A; color: white; font-weight: 700; border-radius: 6px; padding: 10px 15px; width: 100%; margin-top: 15px; }
    .analysis-button:hover { background-color: #007043; color: #f1f1f1; transform: translateY(-2px); box-shadow: 0 4px 8px rgba(0,0,0,0.2); }

    .irs--shiny .irs-bar,
    .irs--shiny .irs-bar-edge,
    .irs--shiny .irs-single { background-color: #00462A !important; border-color: #00462A !important; }
    .irs--shiny .irs-handle { border: 2px solid #00462A !important; }
  "))
  ),

  # --- 네비게이션 바 ---
  tags$nav(class = "navbar navbar-default", role = "navigation",
           div(class = "container-fluid",
               div(class = "navbar-header",
                   tags$a(class = "navbar-brand", href = "#",
                          "성동구 소상공인 위기 진단 대시보드")
               )
           )
  ),

  # --- 탭 구성 ---
  tabsetPanel(
    id = "main_tabs", type = "pills",

    # --- 종합 위험 진단 탭 ---
    tabPanel(
      "종합 위험 진단",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar",
          width = 3,
          h3("데이터 업로드", style = "font-weight: 700; color: #00462A;"),
          selectInput(
            "data_source",
            "데이터 선택:",
            choices = c(
              "직접 업로드" = "upload",
              "페르소나 A (persona_a.csv)" = "persona_a",
              "페르소나 B (persona_b.csv)" = "persona_b",
              "페르소나 C (persona_c.csv)" = "persona_c"
            ),
            selected = "upload"
          ),
          tags$hr(),
          uiOutput("data_load_ui"),
          actionButton("analyze_button", "위험 진단 시작하기", class = "analysis-button")
        ),
        
        mainPanel(width = 9,
                  fluidRow(
                    # --- 종합 위험 스코어 ---
                    column(
                      width = 6,
                      div(
                        class = "score-card",
                        h3("종합 위험 스코어", class = "score-title"),
                        tags$hr(),
                        uiOutput("final_score_ui"),
                        echarts4rOutput("risk_gauge", height = "260px")
                      )
                    ),
                    
                    # --- 요소별 위험 스코어 ---
                    column(
                      width = 6,
                      div(
                        class = "score-card",
                        h3("요소별 위험 스코어", class = "score-title"),
                        tags$hr(),
                        echarts4rOutput("radar_chart")
                      )
                    )
                  ))
      ),
      tags$hr(),
      div(
        style = "padding: 10px;",
        # 좌우 여백
        h4("대시보드 활용 안내", style = "font-size: 20px; font-weight: 700; color: #00462A;"),
        p(
          "이 대시보드는 서울 성동구의 가맹점 데이터로 훈련된 CORE-AI를 활용하여, 새로운 가맹점 데이터의 잠재적 위기를 다각도로 진단합니다."
        ),
        tags$ul(tags$li(
          strong("종합 위험 진단 탭"),
          tags$ul(
            tags$li(
              strong("데이터 업로드:"),
              " 좌측 사이드바에서 샘플 데이터를 선택하거나 직접 데이터를 업로드한 후 '위험 진단 시작하기' 버튼을 클릭하여 분석을 시작할 수 있습니다."
            ),
            tags$li(
              strong("종합 위험 스코어:"),
              " 매출, 고객, 상권 데이터를 종합하여 현재 가게의 전반적인 위험 수준을 점수로 보여줍니다. 점수가 높을수록 위험도가 높다는 것을 의미합니다."
            ),
            tags$li(
              strong("요소별 위험 스코어:"),
              " 전체 위험을 구성하는 세 가지 핵심 요소(매출, 고객, 상권)의 개별 위험도를 레이더 차트로 시각화하여, 어떤 부분이 취약한지 파악할 수 있습니다."
            )
          )
        )),
        p("위험 스코어가 높은 요소의 탭으로 이동하면 개별 요소의 위험도를 세부적으로 확인할 수 있습니다."),
        tags$ul(
          tags$li(
            strong("매출 분석 탭:"),
            " 가맹점의 매출 위험 스코어와 매출 구간을 6개의 주요 요인 변수 변화에 따라 시뮬레이션하며 예측할 수 있습니다."
          ),
          tags$li(
            strong("고객 분석 탭:"),
            " 가맹점의 고객 위험 스코어와 고객 클래스를 11개의 주요 요인 변수 변화에 따라 시뮬레이션하며 예측할 수 있습니다."
          ),
          tags$li(
            strong("상권 분석 탭:"),
            " 현재 가맹점의 업종과 위치에 따른 상권 위험 스코어와 클래스를 분기별 변화에 따라 확인할 수 있습니다."
          )
        )
      )
    ), 

    # --- 매출 분석 탭 ---
    tabPanel("매출 분석",
             fluidRow(
               # --- 매출 위험 스코어 ---
               column(
                 width = 4, div(
                   class = "score-card",
                   h3("매출 위험 스코어", class = "score-title"),
                   tags$hr(),
                   div(
                     style = "display:flex; justify-content:center; align-items:center; width:100%;",
                     echarts4rOutput("sales_gauge", height = "250px", width = "100%")
                   ),
                   div(
                     style = "text-align:center; margin: 15px 0 15px 0;",
                     actionButton(
                       inputId = "reset_sales_sliders",
                       label = "초기화",
                       style = "
                      background-color:#00462A;
                      color:white;
                      font-weight:600;
                      border:none;
                      border-radius:8px;
                      padding:8px 25px;
                      font-size:16px;
                      box-shadow:0 2px 6px rgba(0,0,0,0.2);
                    "
                     )
                   )
                 )
               ),
               
               # --- 예측 매출 구간: 좌측 게이지 + 우측 설명 ---
               column(
                 width = 8, div(
                   class = "score-card",
                   h3("예상 매출 구간", class = "score-title", style = "margin-bottom: 10px;"),
                   tags$hr(),
                   div(
                     style = "display:flex; width:100%; height:250px; align-items:center;",
                     
                     div(
                       style = "flex:6; display:flex; justify-content:center; align-items:center; height:100%;",
                       echarts4rOutput("sales_prob_chart", width = "100%", height = "100%")
                     ),
                     
                     div(
                       style = "flex:4; padding-left:30px; border-left:1px solid #ddd; display:flex; align-items:center; height:100%;",
                       tags$ul(
                         style = "padding:0; margin:0; font-size:15px; line-height:1.6;",
                         tags$li(tags$strong("1구간:", style = "color:#00A65A;"), " 매우 안정적인 매출 구간"),
                         tags$li(tags$strong("2구간:", style = "color:#7ED957;"), " 안정적인 매출 구간"),
                         tags$li(tags$strong("3구간:", style = "color:#F6C600;"), " 평균 매출 구간"),
                         tags$li(tags$strong("4구간:", style = "color:#FFA500;"), " 다소 불안정한 매출 구간"),
                         tags$li(tags$strong("5구간:", style = "color:#E74C3C;"), " 불안정한 매출 구간"),
                         tags$li(tags$strong("6구간:", style = "color:#8B0000;"), " 매우 불안정한 매출 구간")
                       )
                     )
                   )
                 )
               )),
             
             # --- 매출 주요 요인 슬라이더 ---
             hr(),
             fluidRow(column(
               width = 12, div(
                 class = "slider-box",
                 h4("매출 주요 요인 시뮬레이션", style = "font-weight:700; color:#00462A; margin-bottom:10px;"),
                 uiOutput("sales_sliders_ui")
               )
             ))), 
    
    # --- 고객 분석 탭 ---
    tabPanel("고객 분석",
             fluidRow(column(
               4,
               div(
                 class = "score-card",
                 h3("고객 위험 스코어", class = "score-title"),
                 tags$hr(),
                 
                 uiOutput("cust_score_ui"),
                 echarts4rOutput("cust_risk_gauge", height = "250px"),
                 uiOutput("cust_class_box_ui"),
                 
                 div(
                   style = "text-align:center; margin: 15px 0 15px 0;",
                   actionButton(
                     inputId = "reset_cust_sliders",
                     label = "초기화",
                     style = "
                                      background-color:#00462A;
                                      color:white;
                                      font-weight:600;
                                      border:none;
                                      border-radius:8px;
                                      padding:8px 25px;
                                      font-size:16px;
                                      box-shadow:0 2px 6px rgba(0,0,0,0.2);
                                      "
                   )
                 )
               )
             ),
             column(
               8,
               div(
                 class = "score-card",
                 h3("고객 유형 확률 변화", class = "score-title"),
                 tags$hr(),
                 echarts4rOutput("cust_prob_chart", height = "250px"),
                 div(
                   style = "margin-top: 20px; padding: 15px; border-top: 1px solid #ccc; font-size: 14px;",
                   tags$h4("고객 유형 설명", style = "font-size: 16px; color: #00462A; margin-bottom: 10px; font-weight: 700"),
                   tags$ul(
                     tags$li(
                       tags$strong("성장형:", style = "color:#00A65A;"),
                       " 유니크 고객 수가 많고 재방문도 활발하여, 앞으로 성장 가능성이 큰 고객군입니다."
                     ),
                     tags$li(
                       tags$strong("단골형:", style = "color:#007BFF;"),
                       " 유니크 고객 수는 적지만 재방문이 잦아, 충성도가 높은 핵심 고객군입니다."
                     ),
                     tags$li(
                       tags$strong("체험형:", style = "color:#F6C600;"),
                       " 유니크 고객 수는 많지만 재방문은 적어, 체험 중심의 고객군입니다."
                     ),
                     tags$li(
                       tags$strong("위기형:", style = "color:#E74C3C;"),
                       " 유니크 고객 수가 적고 재방문도 적어, 관리가 필요한 위험 고객군입니다."
                     )
                   )
                 )
               )
             ),),
             tags$hr(),
             fluidRow(column(
               width = 12, div(
                 class = "slider-box",
                 h4("고객 주요 요인 시뮬레이션", style = "font-weight:700; color:#00462A; margin-bottom:10px;"),
                 uiOutput("cust_sliders_ui")
               )
             ))),
    
    
    tabPanel("상권 분석",
             fluidRow(
               # --- 좌측 패널: 슬라이더, 위험 스코어 및 확률 ---
               column(
                 width = 4,
                 div(
                   class = "score-card",
                   h3("상권 위험 스코어", class = "score-title"),
                   tags$hr(),
                   div(
                     style = "text-align:center; margin-bottom: -15px;",
                     # 게이지와 간격 조정
                     h3(uiOutput("risk_score_text"),
                        style = "font-weight:700; color:#00462A; margin-bottom:0; font-size:24px;")
                   ),
                   echarts4rOutput("risk_gauge_mkt", height = "250px"),
                   div(
                     style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px; padding: 10px; text-align: center; margin-top: 10px;",
                     h4("상권 유형 예측 결과", style = "margin: 0 0 5px 0; color: #333; font-weight:600;"),
                     div(style = "font-size: 24px; font-weight: 700; color: #00462A;",
                         textOutput("predicted_cluster"))
                   ),
                   tags$hr(),
                   h4("예측 기간 선택", style = "font-weight:700; color:#00462A; margin-bottom:10px;"),
                   sliderTextInput(
                     "time_slider",
                     label = NULL,
                     choices = time_slider_labels,
                     selected = "현재 분기",
                     grid = TRUE,
                     animate = FALSE,
                     width = "100%"
                   )
                 )
               ),
               
               # --- 우측 패널: 시계열 예측 지도 및 클러스터 설명 가이드 ---
               column(
                 width = 8,
                 div(
                   class = "score-card",
                   h3("상권 유형 시계열 지도", class = "score-title"),
                   tags$hr(),
                   plotOutput("static_map", height = "400px"),
                   tags$hr(),
                   h4("상권 유형 설명", style = "font-weight:700; color:#00462A; margin-bottom:10px;"),
                   div(style = "line-height:1.6;",
                       tags$ul(lapply(names(cluster_info), function(name) {
                         tags$li(HTML(paste0("<b>", name, ":</b> ", cluster_info[[name]])))
                       })))
                 )
               )))
  )
)


# ==============================================================================
# 4. 서버 로직
# ==============================================================================

server <- function(input, output, session) {


  # ============================================================================
  # 4.1. 데이터 업로드
  # ============================================================================
  uploaded_data <- reactiveVal(NULL)

  output$data_load_ui <- renderUI({
    if (input$data_source == "upload") {
      tagList(
        h3("데이터 업로드", style = "font-weight: 700; color: #555;"),
        fileInput("file_upload", "CSV 파일 선택", accept = c("text/csv", ".csv"))
      )
    } else {
      sample_name <- switch(input$data_source,
                            "persona_a" = "persona_a.csv",
                            "persona_b" = "persona_b.csv",
                            "persona_c" = "persona_c.csv")
      tagList(p(strong("선택된 샘플 데이터:"), sample_name))
    }
  })

  observeEvent(input$file_upload, {
    req(input$file_upload)
    if (input$data_source != "upload") return()
    tryCatch({
      data <- read.csv(input$file_upload$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
      uploaded_data(data)
      showNotification("파일 업로드 완료.", type = "message")
    }, error = function(e) {
      showNotification(paste("파일 읽기 오류:", e$message), type = "error")
      uploaded_data(NULL)
    })
  })

  analysis_results <- eventReactive(input$analyze_button, {
    if (input$data_source == "upload") {
      data <- uploaded_data()
      source_name <- "업로드된 파일"
    } else {
      file_path <- switch(input$data_source,
                          "persona_a" = "./persona_a.csv",
                          "persona_b" = "./persona_b.csv",
                          "persona_c" = "./persona_c.csv")
      source_name <- paste("샘플 데이터 (", basename(file_path), ")")
      data <- tryCatch(
        readr::read_csv(file_path, show_col_types = FALSE) %>%
          dplyr::mutate(across(c(id, dong, big_ind), as.character)),
        error = function(e) {
          showNotification(paste("샘플 파일 로드 실패:", e$message), type = "error")
          NULL
        }
      )
    }

    if (is.null(data) || nrow(data) == 0) {
      showNotification(paste(source_name, "가 유효하지 않습니다."), type = "warning")
      return(final_risk_model(NULL))
    }

    results <- final_risk_model(data)
    showNotification(paste0(source_name, " 분석 완료!"), type = "message")
    results
  })


  # ============================================================================
  # 4.2. 종합 탭 로직
  # ============================================================================

  # --- 종합 위험 스코어 출력 ---
  output$final_score_ui <- renderUI({
    results <- analysis_results()
    score <- results$final_score
    
    if (is.na(score)) {
      color <- "#7f8c8d"
      text <- "위험 점수 계산 중..."
    } else {
      if (score < 33.3) {
        color <- "#00A65A"
      } else if (score < 66.6) {
        color <- "#F6C600"
      } else {
        color <- "#E74C3C"
      }
      text <- paste0("위험 스코어: ", score, "점")
    }
    
    div(style = "text-align:center; margin-bottom: 6px;",
        h2(text, style = sprintf("color:%s; font-weight:800; font-size:34px; margin:0;", color))
    )
  })

  # --- 종합 위험 스코어 게이지 출력 ---
  output$risk_gauge <- renderEcharts4r({
    results <- analysis_results()
    score <- results$final_score
    
    if (is.na(score)) {
      score <- 0
      risk_label <- "분석 시작"
    } else {
      risk_label <- if (score < 33.3) "안정" else if (score < 66.6) "주의" else "위험"
    }

    color_breaks <- list(
      list(0.333, "#00A65A"),  # 초록
      list(0.666, "#F6C600"),   # 노랑
      list(1, "#E74C3C")      # 빨강
    )

    gauge_data <- data.frame(value = score)

    gauge_data %>%
      e_charts() %>%
      e_gauge(
        name = "Risk Score",
        value = score,
        max = 100,
        startAngle = 180,
        endAngle = 0,
        splitNumber = 10,
        axisLine = list(
          lineStyle = list(width = 30, color = color_breaks)
        ),
        pointer = list(
          length = "60%",
          width = 5,
          itemStyle = list(color = "black")
        ),
        
        axisTick = list(
          show = TRUE,
          splitNumber = 5,
          length = 6,
          lineStyle = list(color = "#333", width = 1)
        ),
        splitLine = list(
          show = TRUE,
          length = 10,
          lineStyle = list(color = "#333", width = 1.5)
        ),

        axisLabel = list(show = FALSE),
        title = list(show = FALSE),

        detail = list(
          show = TRUE,
          formatter = htmlwidgets::JS(
            sprintf("function(){return '%s';}", risk_label)
          ),
          fontSize = 22,
          color = if (risk_label == "안정") "#00A65A" else if (risk_label == "주의") "#F6C600" else "#E74C3C",
          offsetCenter = c(0, "40%")  
        )
      ) %>%
      e_tooltip(
        formatter = htmlwidgets::JS("function(params){return '종합 위험도: ' + params.value + '점';}")
      ) %>%
      e_legend(show = FALSE)
  })


  # --- 요소별 위험 스코어 레이더 출력 ---
  output$radar_chart <- renderEcharts4r({
    results <- analysis_results()
    score_df <- results$component_scores

    if (is.null(score_df) || any(is.na(score_df$score))) {
      empty_df <- data.frame(indicator = "데이터 없음", score = 0)
      return(
        empty_df %>%
          e_charts(indicator) %>%
          e_radar(score, max = 1, name = "위험 스코어") %>%
          e_title("점수 데이터가 없습니다.")
      )
    }

    df <- score_df %>%
      dplyr::mutate(
        indicator = factor(category, levels = c("매출", "고객", "상권")),
        score = score / 100
      ) %>%
      dplyr::select(indicator, score)

    df %>%
      e_charts(indicator) %>%
      e_radar(
        score,
        max = 1,
        areaStyle = list(opacity = 0.3, color = "#00462A"), 
        lineStyle = list(width = 3, color = "#00462A"),
        itemStyle = list(color = "#00462A", borderWidth = 5), 
        label = list(                                       
          show = TRUE,
          formatter = htmlwidgets::JS("function(params){ return (params.value * 100).toFixed(0) + '점'; }"),
          offset = c(0, -15), 
          fontSize = 15,    
          fontWeight = 'bold',
          color = "#00462A" 
        ),
        z = 2 
      ) %>%
      e_radar_opts(
        splitLine = list(lineStyle = list(color = "#ccc")), 
        axisLine = list(lineStyle = list(color = "#aaa")),  
        name = list(textStyle = list(color = "#333", fontSize = 16, fontWeight = 'bold')) 
      ) %>%
      e_tooltip(trigger = "item") %>%
      e_legend(show = FALSE)
  })

  # --- 종합 탭에서 선택한 공용 데이터 ---
  current_data <- reactive({
    if (input$data_source == "upload") {
      req(uploaded_data())
      uploaded_data()
    } else {
      file_path <- switch(input$data_source,
                          "persona_a" = "./persona_a.csv",
                          "persona_b" = "./persona_b.csv",
                          "persona_c" = "./persona_c.csv")
      readr::read_csv(file_path, show_col_types = FALSE) #%>%
        #dplyr::mutate(across(c(id, dong, big_ind), as.character))
    }
  })


  # ============================================================================
  # 4.3. 매출 분석 탭 로직
  # ============================================================================
  sales_sim_base <- reactive({
    req(analysis_results())
    base_df <- current_data()
    if ("row.number" %in% names(base_df)) {
      sim_row <- base_df %>% dplyr::filter(row.number == max(row.number))
    } else {
      sim_row <- base_df[nrow(base_df), ]
    }
    sim_row
  })
  
  
  # --- 슬라이더 UI ---
  output$sales_sliders_ui <- renderUI({
    base <- sales_sim_base()
    req(base)
    tagList(
      fluidRow(
        column(4, 
               sliderInput("ind_cancel_rat_mean", "동일 업종 내 해지 가맹점 비율(%)", 0, 100,
                           value = round(base$ind_cancel_rat_mean,1), step = 1, width = "80%"),
               sliderInput("sales_new_cust_rat_mean", "신규 고객 비율(%)", 0, 100,
                           value = round(base$new_cust_rat_mean,1), step = 1 , width = "80%"),
        ),
        column(4, 
               sliderInput("sales_m_rat_mean", "남성 고객 비율(%)", 0, 100, value = round(base$m_rat_mean,1), step = 1, width = "80%"),
               sliderInput("unique_cust_cat_mean", "유니크 고객 수 구간 (1: 상위, 6: 하위)", 1, 6,
                           value = round(base$unique_cust_cat_mean,1), width = "80%")
        ),
        column(4, 
               sliderInput("sales_f_rat_mean", "여성 고객 비율(%)", 0, 100, value = round(base$f_rat_mean,1), step = 1, width = "80%"),
               sliderInput("duration_cat_mean", "운영개월 수 구간 (1: 상위, 6: 하위)", 1, 6, 
                           value = round(base$duration_cat_mean,1), width = "80%")
        )
      )
    )
  })
  
  # --- 성별 비율 (남성 + 여성 = 100) 연결 ---
  observeEvent(input$sales_m_rat_mean, {
    req(input$sales_m_rat_mean, input$sales_f_rat_mean)
    new_f_rat <- 100 - input$sales_m_rat_mean
    if (abs(new_f_rat - input$sales_f_rat_mean) > 0.01) {
      updateSliderInput(session, "sales_f_rat_mean", value = round(new_f_rat, 1))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$sales_f_rat_mean, {
    req(input$sales_m_rat_mean, input$sales_f_rat_mean)
    new_m_rat <- 100 - input$sales_f_rat_mean
    if (abs(new_m_rat - input$sales_m_rat_mean) > 0.01) {
      updateSliderInput(session, "sales_m_rat_mean", value = round(new_m_rat, 1))
    }
  }, ignoreInit = TRUE)
  
  
  # --- 초기화 버튼 ---
  observeEvent(input$reset_sales_sliders, {
    base <- sales_sim_base()
    vars <- c("ind_cancel_rat_mean", "sales_m_rat_mean", "sales_f_rat_mean", "sales_new_cust_rat_mean", "unique_cust_cat_mean")
    
    updateSliderInput(session, "ind_cancel_rat_mean", value = round(base[["ind_cancel_rat_mean"]], 1))
    updateSliderInput(session, "sales_m_rat_mean", value = round(base[["m_rat_mean"]], 1))
    updateSliderInput(session, "sales_f_rat_mean", value = round(base[["f_rat_mean"]], 1))
    updateSliderInput(session, "sales_new_cust_rat_mean", value = round(base[["new_cust_rat_mean"]], 1))
    updateSliderInput(session, "unique_cust_cat_mean", value = round(base[["unique_cust_cat_mean"]], 1))
    updateSliderInput(session, "duration_cat_mean", value = round(base[["duration_cat_mean"]], 1))
  })
  
  # --- 시뮬레이션 데이터 ---
  sales_sim_data <- reactive({
    sim_row <- sales_sim_base()
    
    sim_row$ind_cancel_rat_mean <- input$ind_cancel_rat_mean
    sim_row$m_rat_mean <- input$sales_m_rat_mean
    sim_row$f_rat_mean <- input$sales_f_rat_mean
    sim_row$new_cust_rat_mean <- input$sales_new_cust_rat_mean
    sim_row$unique_cust_cat_mean <- input$unique_cust_cat_mean
    sim_row$duration_cat_mean <- input$duration_cat_mean
    
    sim_row
  })
  
  # --- 매출 위험 점수 시뮬레이션 예측  결과---
  sales_risk_score <- reactive({
    req(sales_sim_data())
    predict_score_sales(sales_risk_model_loaded, sales_sim_data())
  })
    
  # --- 매출 구간 예측 ---
  predict_sales_simple <- function(model_list, new_data) {
    if (is.null(model_list) || is.null(new_data)) {
      return(NULL)
    }
    
    preds <- purrr::map(seq_along(model_list), function(i) {
      m_entry <- model_list[[i]]
      if ("model" %in% names(m_entry)) {
        m <- m_entry$model
      } else {
        m <- m_entry
      }
      
      out <- tryCatch({
        p <- predict(m, new_data, type = "numeric")
        p
      }, error = function(e) {
        NULL
      })
      out
    })
    
    preds <- preds[!sapply(preds, is.null)]
    if (length(preds) == 0) {
      return(NULL)
    }
    
    preds_df <- bind_cols(preds)
    ensemble_val <- rowMeans(preds_df, na.rm = TRUE)
    list(pred_value = ensemble_val)
  }
  
  # --- 매출 구간 시뮬레이션 예측 결과 --- 
  sales_prediction <- reactive({
    req(sales_sim_data())
    res <- predict_sales_simple(sales_model_loaded, sales_sim_data())
    res
  })
  
  # --- 게이지 출력 ---
  output$sales_gauge <- renderEcharts4r({
    res <- sales_risk_score()
    req(res)
    
    validate(need(length(res$sales_risk_pred) > 0, "예측값 없음"))
    
    score <- as.numeric(res$sales_risk_pred) * 100
    validate(need(!is.na(score), "예측값 없음"))
    
    risk_label <- if (score < 33.3) "안정" else if (score < 66.6) "주의" else "위험"
    
    color_breaks <- list(
      list(0.333, "#00A65A"),
      list(0.666, "#F6C600"),
      list(1, "#E74C3C")
    )
    
    detail_color <- if (risk_label == "안정") "#00A65A" else if (risk_label == "주의") "#F6C600" else "#E74C3C"
    
    data.frame(value = score) %>%
      e_charts() %>%
      
      # 첫 번째 게이지: 점수 (위쪽 표시)
      e_gauge(
        name = "",
        value = score,
        max = 100,
        startAngle = 180,
        endAngle = 0,
        axisLine = list(lineStyle = list(width = 25, color = color_breaks)),
        pointer = list(length = "60%", width = 5, itemStyle = list(color = "black")),
        splitLine = list(
          show = TRUE,
          length = 10,
          lineStyle = list(color = "#333", width = 1.5)
        ),
        axisLabel = list(show = FALSE),
        detail = list(
          show = TRUE,
          formatter = htmlwidgets::JS("function(value){ return '위험 스코어: ' + value.toFixed(0) + '점'; }"),
          fontSize = 24,
          fontWeight = 'bold',
          color = detail_color,
          offsetCenter = c(0, '-130%') 
        ),
        center = c("50%", "60%")
      ) %>%
      
      # 두 번째 게이지: 위험도 라벨 (아래쪽 표시)
      e_gauge(
        name = "",
        value = score,
        max = 100,
        startAngle = 180,
        endAngle = 0,
        axisLine = list(show = FALSE),  
        pointer = list(show = FALSE),    
        axisLabel = list(show = FALSE),
        splitLine = list(show = FALSE),
        axisTick = list(show = FALSE),
        detail = list(
          show = TRUE,
          formatter = htmlwidgets::JS("
          function(value){
            if(value < 33.3) return '안정';
            else if(value < 66.6) return '주의';
            else return '위험';
          }
        "),
          fontSize = 20,
          fontWeight = 'bold',
          color = detail_color,
          offsetCenter = c(0, '30%')  
        ),
        center = c("50%", "60%")
      )
  })
  
  # --- 예측 매출 구간 표시 (게이지 + 설명) ---
  output$sales_prob_chart <- renderEcharts4r({
    
    res <- sales_prediction()
    req(res$pred_value)
    
    pred_val <- round(as.numeric(res$pred_value[1]))
    pred_val <- max(1, min(6, pred_val)) 
    
    colors_hex <- c("#00A65A", "#7ED957", "#F6C600", "#FFA500", "#E74C3C", "#8B0000")
    breaks <- seq(0, 1, length.out = 7)
    color_segments <- lapply(seq_len(6), function(i) list(breaks[i + 1], colors_hex[i]))
    
    data.frame(value = pred_val) %>%
      e_charts() %>%
      e_gauge(
        name = "",
        value = pred_val,
        min = 1, max = 6,
        startAngle = 180, endAngle = 0,
        radius = "100%",                
        axisLine = list(
          lineStyle = list(
            width = 30,
            color = color_segments
          )
        ),
        pointer = list(
          show = TRUE,
          length = "70%",
          width = 6,
          itemStyle = list(color = "black")
        ),
        axisTick = list(show = FALSE),
        splitLine = list(show = FALSE),
        axisLabel = list(show = FALSE),
        detail = list(
          show = TRUE,
          formatter = htmlwidgets::JS("function(value){ return value.toFixed(0) + '구간'; }"),
          fontSize = 24,
          fontWeight = "bold",
          color = colors_hex[pred_val],
          offsetCenter = c(0, "30%")  
        ),
        center = c("50%", "70%")
      ) %>%
      e_tooltip(show = FALSE)
  })


  # ============================================================================
  # 4.4. 고객 분석 탭 로직
  # ============================================================================
  
  # --- 슬라이더와 숫자 필드 모두 사용 ---
  combined_slider_numeric <- function(inputId, label, value, min = 0, max = 100, step = 1) {
    numericId <- paste0(inputId, "_num")
    
    tagList(
      div(label, style = "font-weight: bold; margin-bottom: 5px;"),
      fluidRow(
        column(8,
               sliderInput(inputId, NULL,
                           min = min, max = max, value = value, step = step, width = "100%")
        ),
        column(4,
               numericInput(numericId, NULL,
                            value = value, min = min, max = max, step = step, width = "100%")
        )
      )
    )
  }
  
  # --- 고객 모델 예측을 위한 데이터 처리 ---
  cust_sim_base <- reactive({
    req(analysis_results())
    
    base_df <- current_data()
    processed_df <- process_pipeline_cust_newdata(base_df)

    if ("row.number" %in% names(processed_df)) {
      sim_row <- processed_df %>% dplyr::filter(row.number == max(row.number))
    } else {
      sim_row <- processed_df[nrow(processed_df), ]
    }
    
    sim_row
  })
  
  # --- 고객 모델 시뮬레이션을 위한 UI 정의 (M/F와 신규 고객은 슬라이더만 사용) ---
  output$cust_sliders_ui <- renderUI({
    base <- cust_sim_base()
    
    tagList(
      fluidRow(
        column(4,
               sliderInput("cust_new_cust_rat_mean", "신규 고객 비율(%)", width = "65%",
                           min = 0, max = 100, value = round(base$new_cust_rat_mean, 1), step = 1),
               combined_slider_numeric("move_cust_rat_mean", "유동인구 고객 비율(%)",
                                       value = round(base$move_cust_rat_mean, 1)),
               combined_slider_numeric("age_20_under_rat_mean", "20대 이하 고객 비율(%)",
                                       value = round(base$age_20_under_rat_mean, 1)),
               combined_slider_numeric("age_50_rat_mean", "50대 고객 비율(%)",
                                       value = round(base$age_50_rat_mean, 1))
        ),
        column(4,
               sliderInput("cust_m_rat_mean", "남성 고객 비율(%)", width = "65%",
                           min = 0, max = 100, value = round(base$m_rat_mean, 1), step = 1),
               combined_slider_numeric("resid_cust_rat_mean", "거주인구 고객 비율(%)",
                                       value = round(base$resid_cust_rat_mean, 1)),
               combined_slider_numeric("age_30_rat_mean", "30대 고객 비율(%)",
                                       value = round(base$age_30_rat_mean, 1)),
               combined_slider_numeric("age_60_over_rat_mean", "60대 이상 고객 비율(%)",
                                       value = round(base$age_60_over_rat_mean, 1))
        ),
        column(4,
               sliderInput("cust_f_rat_mean", "여성 고객 비율(%)", width = "65%",
                           min = 0, max = 100, value = round(base$f_rat_mean, 1), step = 1),
               combined_slider_numeric("office_cust_rat_mean", "직장인구 고객 비율(%)",
                                       value = round(base$office_cust_rat_mean, 1)),
               combined_slider_numeric("age_40_rat_mean", "40대 고객 비율(%)",
                                       value = round(base$age_40_rat_mean, 1)),
               tags$div(
                 style = "font-size: 90%; color: #666;", 
                 strong("※ 설정 안내"),
                 br(),
                 "• '유동', '거주', '직장' 고객 비율의 합을 100%로 맞춰주세요.",
                 br(),
                 "• 모든 연령대(20대 ~ 60대 이상) 고객 비율의 합을 100%로 맞춰주세요."
               )
        )
      ),
      
      # --- 합계 불일치 경고 메시지 영역 ---
      fluidRow(
        column(6, uiOutput("cust_pop_ratio_warning_ui")),
        column(6, uiOutput("cust_age_ratio_warning_ui"))
      )
    )
  })
  
  observeEvent(input$cust_m_rat_mean, {
    req(input$cust_m_rat_mean, input$cust_f_rat_mean)
    new_f_rat <- 100 - input$cust_m_rat_mean
    if (abs(new_f_rat - input$cust_f_rat_mean) > 0.01) {
      updateSliderInput(session, "cust_f_rat_mean", value = round(new_f_rat, 1))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$cust_f_rat_mean, {
    req(input$cust_m_rat_mean, input$cust_f_rat_mean)
    new_m_rat <- 100 - input$cust_f_rat_mean
    if (abs(new_m_rat - input$cust_m_rat_mean) > 0.01) {
      updateSliderInput(session, "cust_m_rat_mean", value = round(new_m_rat, 1))
    }
  }, ignoreInit = TRUE)
  
  # --- 거주/직장/유동인구 합 = 100 경고 로직 ---
  cust_pop_ratio_total <- reactive({
    if (is.null(input$resid_cust_rat_mean) || is.null(input$office_cust_rat_mean) || is.null(input$move_cust_rat_mean)) {
      return(NULL)
    }
    input$resid_cust_rat_mean + input$office_cust_rat_mean + input$move_cust_rat_mean
  })
  
  output$cust_pop_ratio_warning_ui <- renderUI({
    total <- cust_pop_ratio_total()
    if (is.null(total)) return(NULL)
    
    if (abs(total - 100) > 0.1) {
      showNotification(
        paste0("⚠ 유동/거주/직장인구 비율 합계가 100%가 아닙니다. 현재 합계: ", round(total, 1), "%"),
        type = "error",
        duration = 5
      )
    } else {
      NULL
    }
  })
  
  observeEvent(input$resid_cust_rat_mean_num, {
    req(input$resid_cust_rat_mean_num)
    if (input$resid_cust_rat_mean != input$resid_cust_rat_mean_num) {  
      updateSliderInput(session, "resid_cust_rat_mean", value = input$resid_cust_rat_mean_num)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$office_cust_rat_mean_num, {
    req(input$office_cust_rat_mean_num)
    if (input$office_cust_rat_mean != input$office_cust_rat_mean_num) { 
      updateSliderInput(session, "office_cust_rat_mean", value = input$office_cust_rat_mean_num)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$move_cust_rat_mean_num, {
    req(input$move_cust_rat_mean_num)
    if (input$move_cust_rat_mean != input$move_cust_rat_mean_num) {  
      updateSliderInput(session, "move_cust_rat_mean", value = input$move_cust_rat_mean_num)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$resid_cust_rat_mean, {
    req(input$resid_cust_rat_mean)
    if (input$resid_cust_rat_mean != input$resid_cust_rat_mean_num) {  
      updateNumericInput(session, "resid_cust_rat_mean_num", value = input$resid_cust_rat_mean)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$office_cust_rat_mean, {
    req(input$office_cust_rat_mean)
    if (input$office_cust_rat_mean != input$office_cust_rat_mean_num) { 
      updateNumericInput(session, "office_cust_rat_mean_num", value = input$office_cust_rat_mean)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$move_cust_rat_mean, {
    req(input$move_cust_rat_mean)
    if (input$move_cust_rat_mean != input$move_cust_rat_mean_num) {  
      updateNumericInput(session, "move_cust_rat_mean_num", value = input$move_cust_rat_mean)
    }
  }, ignoreInit = TRUE)
  
  
  # --- 연령대별 고객 비중 합 = 100 경고 로직 ---
  cust_age_ratio_total <- reactive({
    age_vars <- c("age_20_under_rat_mean", "age_30_rat_mean", "age_40_rat_mean", "age_50_rat_mean", "age_60_over_rat_mean")
    
    if (any(sapply(age_vars, function(v) is.null(input[[v]])))) {
      return(NULL)
    }
    
    sum(
      input$age_20_under_rat_mean, input$age_30_rat_mean, input$age_40_rat_mean,
      input$age_50_rat_mean, input$age_60_over_rat_mean
    )
  })
  
  output$cust_age_ratio_warning_ui <- renderUI({
    total <- cust_age_ratio_total()
    if (is.null(total)) return(NULL)
    
    if (abs(total - 100) > 0.1) {
      showNotification(
        paste0("⚠ 연령대별 고객 비율의 합계가 100%가 아닙니다. 현재 합계: ", round(total, 1), "%"),
        type = "error",
        duration = 5
      )
    } else {
      NULL
    }
  })
  
  age_vars <- c(
    "age_20_under_rat_mean", "age_30_rat_mean", "age_40_rat_mean",
    "age_50_rat_mean", "age_60_over_rat_mean"
  )
  
  for (id in age_vars) {
    local({
      slider_id <- id
      num_id <- paste0(id, "_num")
      observeEvent(input[[num_id]], {
        req(input[[num_id]])
        if (input[[slider_id]] != input[[num_id]]) { 
          updateSliderInput(session, slider_id, value = input[[num_id]])
        }
      }, ignoreInit = TRUE)
    })
  }
  
  for (id in age_vars) {
    local({
      slider_id <- id
      num_id <- paste0(id, "_num")
      observeEvent(input[[slider_id]], {
        req(input[[slider_id]])
        if (input[[slider_id]] != input[[num_id]]) {  
          updateNumericInput(session, num_id, value = input[[slider_id]])
        }
      }, ignoreInit = TRUE)
    })
  }
  
  
  # --- 시뮬레이션 초기화 버튼 ---
  observeEvent(input$reset_cust_sliders, {
    base <- cust_sim_base()
    
    updateSliderInput(session, "cust_new_cust_rat_mean", value = round(base[["new_cust_rat_mean"]], 1))
    updateSliderInput(session, "cust_m_rat_mean", value = round(base[["m_rat_mean"]], 1))
    updateSliderInput(session, "cust_f_rat_mean", value = round(base[["f_rat_mean"]], 1))
    
    other_vars <- c("move_cust_rat_mean", "resid_cust_rat_mean", "office_cust_rat_mean",
                    "age_20_under_rat_mean", "age_30_rat_mean", "age_40_rat_mean",
                    "age_50_rat_mean", "age_60_over_rat_mean")
    
    for (v in other_vars) {
      if (!is.null(input[[v]])) {
        updateSliderInput(session, v, value = round(base[[v]], 1))
        updateNumericInput(session, paste0(v, "_num"), value = round(base[[v]], 1))
      }
    }
  })
  
  # --- 고객 시뮬레이션 데이터 ---
  cust_sim_data <- reactive({
    sim_row <- cust_sim_base()
    
    sim_row$new_cust_rat_mean <- input$cust_new_cust_rat_mean
    sim_row$m_rat_mean <- input$cust_m_rat_mean
    sim_row$f_rat_mean <- input$cust_f_rat_mean
    
    sim_vars <- c("move_cust_rat_mean", "resid_cust_rat_mean", "office_cust_rat_mean",
                  "age_20_under_rat_mean", "age_30_rat_mean", "age_40_rat_mean",
                  "age_50_rat_mean", "age_60_over_rat_mean")
    
    for (var in sim_vars) {
      if (!is.null(input[[var]])) {
        sim_row[[var]] <- input[[var]]
      }
    }
    
    sim_row
  })
  

  # --- 고객 시뮬레이션 예측 결과 ---
  cust_prediction <- reactive({
    req(cust_sim_data())
    predict_score_cust(cust_model_loaded, cust_sim_data())
  })

  colors <- c("성장형" = "#00A65A", "단골형" = "#007BFF",
              "체험형" = "#F6C600", "위기형" = "#E74C3C")

  # --- 고객 예측 클래스 및 위험 스코어 출력 ---
  output$cust_score_ui <- renderUI({
    res <- cust_prediction()
    score <- round(res$risk_score_scaled * 100)
    score_color <- if (score < 33.3) "#00A65A" else if (score < 66.6) "#F6C600" else "#E74C3C"
    
    div(
      style = "text-align:center; margin-bottom: -20px;",
      h3(paste0("위험 스코어: ", score, "점"), style = sprintf("color:%s; font-weight:700;", score_color))
    )
  })
  
  
  # ---  고객 예측 클래스 박스 출력 (게이지 아래) ---
  output$cust_class_box_ui <- renderUI({
    res <- cust_prediction()
    cls <- res$ensemble_class
    
    cls_kor <- switch(cls,
                      "1_Growth" = "성장형",
                      "2_Loyal"  = "단골형",
                      "3_Trial"  = "체험형",
                      "4_AtRisk" = "위기형",
                      cls)
    
    cls_color <- colors[cls_kor]
    
    div(
      style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px; padding: 10px; text-align: center; margin-top: -10px;",
      h4("고객 유형 예측 결과", style="margin: 0 0 5px 0; color: #333; font-weight:600;"),
      div(
        style = sprintf("font-size: 24px; font-weight: 700; color: %s;", cls_color),
        cls_kor
      )
    )
  })

  # --- 고객 위험 스코어 게이지 출력 ---
  output$cust_risk_gauge <- renderEcharts4r({
    res <- cust_prediction()
    score <- round(res$risk_score_scaled * 100)

    risk_label <- if (score < 33.3) {
      "안정"
    } else if (score < 66.6) {
      "주의"
    } else {
      "위험"
    }

    color_breaks <- list(
      list(0.333, "#00A65A"),
      list(0.666, "#F6C600"),
      list(1, "#E74C3C")
    )

    data.frame(value = score) %>%
      e_charts() %>%
      e_gauge(
        name = "cust score",
        value = score,
        max = 100,
        startAngle = 180,
        endAngle = 0,
        axisLine = list(lineStyle = list(width = 25, color = color_breaks)),
        pointer = list(length = "60%", width = 5, itemStyle = list(color = "black")),
        axisLabel = list(show = FALSE),
        splitLine = list(
          show = TRUE,
          length = 10,
          lineStyle = list(color = "#333", width = 1.5)
        ),
        title = list(show = FALSE),
        detail = list(
          show = TRUE,
          formatter = htmlwidgets::JS(sprintf("function(){return '%s';}", risk_label)),
          fontSize = 22,
          color = if (risk_label == "안정") "#00A65A" else if (risk_label == "주의") "#F6C600" else "#E74C3C",
          offsetCenter = c(0, "40%")
        )
      ) %>%
      e_legend(show = FALSE)
  })

  # --- 고객 클래스별 확률 막대 출력 ---
  output$cust_prob_chart <- renderEcharts4r({
    res <- cust_prediction()

    probs <- res %>%
      dplyr::select(dplyr::starts_with(".pred_")) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "class", values_to = "prob") %>%
      dplyr::mutate(class = gsub("^\\.pred_", "", class),
                    class_kor = dplyr::recode(class,
                                              "1_Growth" = "성장형",
                                              "2_Loyal" = "단골형",
                                              "3_Trial" = "체험형",
                                              "4_AtRisk" = "위기형"))

    colors <- c("성장형" = "#00A65A", "단골형" = "#007BFF",
                "체험형" = "#F6C600", "위기형" = "#E74C3C")

    probs %>%
      e_charts(class_kor) %>%
      e_bar(prob, name = "확률", itemStyle = list(color = htmlwidgets::JS(
        "function(params) {
        var c = {'성장형':'#00A65A','단골형':'#007BFF','체험형':'#F6C600','위기형':'#E74C3C'};
        return c[params.name];
      }"
      ))) %>%
      e_y_axis(min = 0, max = 1) %>%
      e_labels(
        show = TRUE,
        position = "top",
        formatter = htmlwidgets::JS("function(params) {
          var v = Array.isArray(params.value) ? params.value[1] : params.value;
          if (v === null || v === undefined || isNaN(Number(v))) return '';
          return (Number(v) * 100).toFixed(1) + '%';
        }")
      ) %>%
      e_tooltip(trigger = "item") %>%
      e_legend(show = FALSE)
  })


  # ============================================================================
  # 4.5. 상권 분석 탭 로직
  # ============================================================================
  
  # --- 상권 모델 예측을 위한 데이터 처리 ---
  all_pred_data_with_score <- reactive({
    req(analysis_results())
    
    base_df <- current_data()
    test_processed <- process_pipeline_mkt_newdata(base_df)
    
    target_dong_val <- test_processed %>% 
      arrange(desc(year), desc(quarter)) %>% 
      pull(dong) %>% .[1]
    
    df_t <- test_processed %>% 
      filter(dong == target_dong_val) %>%
      arrange(desc(year), desc(quarter)) %>% 
      head(1)
    
    future_data <- list()
    for (i in 1:4) {
      new_row <- df_t
      
      current_year <- as.integer(df_t$year)
      current_quarter <- as.integer(df_t$quarter)
      
      new_quarter <- current_quarter + i
      new_year <- current_year
      
      if (new_quarter > 4) {
        new_year <- new_year + floor((new_quarter - 1) / 4)
        new_quarter <- (new_quarter - 1) %% 4 + 1
      }
      
      new_row$year <- new_year
      new_row$quarter <- new_quarter
      future_data[[i]] <- new_row
    }
    future_data_df <- bind_rows(future_data)
    
    past_data <- test_processed %>% 
      filter(dong == target_dong_val) %>%
      arrange(desc(year), desc(quarter)) %>% 
      head(2) %>%
      arrange(year, quarter)
    
    all_pred_data <- bind_rows(past_data, future_data_df)
    
    pred_prob_df <- predict(mkt_model_loaded, new_data = all_pred_data, type = "prob")
    
    pred_risk_df <- pred_prob_df %>%
      mutate(
        risk_score = .pred_1 * area_risk_weights[1] +
          .pred_2 * area_risk_weights[2] +
          .pred_3 * area_risk_weights[3] +
          .pred_4 * area_risk_weights[4]
      )
    
    bind_cols(all_pred_data, pred_risk_df) %>%
      mutate(time_step_label = time_slider_labels)
  })
  
  # --- 위험 스코어 게이지 차트 출력 ---
  output$risk_gauge_mkt <- renderEcharts4r({
    req(all_pred_data_with_score())
    
    df_filtered <- all_pred_data_with_score() %>%
      filter(time_step_label == input$time_slider)
    
    scaled_score <- round((df_filtered$risk_score - 1) / (4 - 1) * 100)
    
    risk_label <- if (scaled_score < 33.3) {
      "안정"
    } else if (scaled_score < 66.6) {
      "주의"
    } else {
      "위험"
    }
    
    color_breaks <- list(
      list(0.333, "#00A65A"),
      list(0.666, "#F6C600"),
      list(1, "#E74C3C")
    )
    
    data.frame(value = scaled_score) %>%
      e_charts() %>%
      e_gauge(
        name = "Risk Score",
        value = scaled_score,
        max = 100,
        startAngle = 180,
        endAngle = 0,
        axisLine = list(lineStyle = list(width = 25, color = color_breaks)),
        pointer = list(length = "60%", width = 5, itemStyle = list(color = "black")),
        splitLine = list(
          show = TRUE,
          length = 10,
          lineStyle = list(color = "#333", width = 1.5)
        ),
        axisLabel = list(show = FALSE),
        title = list(show = FALSE),
        detail = list(
          show = TRUE,
          formatter = htmlwidgets::JS(sprintf("function(){return '%s';}", risk_label)),
          fontSize = 22,
          color = if (risk_label == "안정") "#00A65A" else if (risk_label == "주의") "#F6C600" else "#E74C3C",
          offsetCenter = c(0, "40%")  
        )
      ) %>%
      e_legend(show = FALSE)
  })
  
  output$risk_score_text <- renderUI({
    req(all_pred_data_with_score())
    
    df_filtered <- all_pred_data_with_score() %>%
      filter(time_step_label == input$time_slider)
    
    scaled_score <- round((df_filtered$risk_score - 1) / (4 - 1) * 100)
    
    score_color <- if (scaled_score < 33.3) {
      "#00A65A"
    } else if (scaled_score < 66.6) {
      "#F6C600"
    } else {
      "#E74C3C"
    }
    
    HTML(sprintf('<span style="font-weight:bold; font-size:24px; color:%s;">위험 스코어: %d점</span>',
                 score_color, scaled_score))
  })
  
  # --- 클러스터 분류 확률이 가장 높은 클러스터 출력 ---
  output$predicted_cluster <- renderText({
    req(all_pred_data_with_score())
    
    df_filtered <- all_pred_data_with_score() %>%
      filter(time_step_label == input$time_slider)
    
    cluster_probs <- df_filtered %>% 
      select(.pred_1, .pred_2, .pred_3, .pred_4)
    
    predicted_class <- which.max(cluster_probs)
    
    cluster_names <- names(cluster_info)
    
    paste0(cluster_names[predicted_class])
  })
  
  # --- 시계열 지도 출력 ---
  output$static_map <- renderPlot({
    req(all_pred_data_with_score())
    
    df_filtered <- all_pred_data_with_score() %>%
      filter(time_step_label == input$time_slider)
    
    target_dong_val <- df_filtered$dong[1]
    
    map_data <- seongdong_dong_simple %>%
      left_join(df_filtered, by = c("adm_nm_short" = "dong")) %>%
      mutate(
        risk_score_scaled = (risk_score - 1) / (4 - 1) * 100,
        risk_score_display = if_else(adm_nm_short == target_dong_val, risk_score_scaled, NA_real_)
      )
    
    ggplot(data = map_data) +
      geom_sf(aes(fill = risk_score_display), color = "black") +
      suppressWarnings(               
        geom_sf_text(aes(label = adm_nm_short), size = 2.5, color = "black")
      ) +
      scale_fill_gradientn(
        colors = c("#00A65A", "#F6C600", "#E74C3C"), 
        name = "위험 스코어 (0-100)", 
        na.value = "gray80", 
        limits = c(0, 100)
      ) +
      labs(
        title = paste0(df_filtered$year, "년 ", df_filtered$quarter, "분기"),
        subtitle = paste0("가게 상권: ", target_dong_val, "\n", "가게 업종: ", df_filtered$big_ind[1])
      ) +
      theme_void() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        legend.position = "right"
      )
  })
}


# ==============================================================================
# 5. 앱 실행
# ==============================================================================

shinyApp(ui, server)

