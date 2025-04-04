library(shiny)
library(gbm)
library(survival)

# 加载模型、基线生存函数和 mean_risk_score
load("gbm_model.RData")
load("baseline_surv.RData")

# 定义UI
ui <- fluidPage(
  titlePanel("Clinical Risk Score and Survival Probability Calculator"),
  sidebarLayout(
    sidebarPanel(
      h4("Please fill in the clinical parameters:"),
      radioButtons("Age", "Age",
                   choices = list("<75y" = 0, "≥75y" = 1),
                   selected = 0),
      radioButtons("NLR", "NLR",
                   choices = list("<3.3" = 0, "≥3.3" = 1),
                   selected = 0),
      radioButtons("PLR", "PLR",
                   choices = list("<545.7" = 0, "≥545.7" = 1),
                   selected = 0),
      radioButtons("TyG", "TyG",
                   choices = list("<5.8" = 0, "≥5.8" = 1),
                   selected = 0),
      radioButtons("CEA", "CEA (ng/mL)",
                   choices = list("≤5" = 0, ">5" = 1),
                   selected = 0),
      radioButtons("CA199", "CA199 (U/mL)",
                   choices = list("<30" = 0, "≥30" = 1),
                   selected = 0),
      selectInput("Tumor_differentiation", "Tumor differentiation",
                  choices = list("high differentiated" = 1, 
                                 "moderately differentiated" = 2, 
                                 "poorly differentiated/Undifferentiated" = 3),
                  selected = 1),
      radioButtons("UICC", "UICC stage",
                   choices = list("I-II stage" = 0, "III-IV stage" = 1),
                   selected = 0),
      radioButtons("Perineural_invasion", "Perineural invasion",
                   choices = list("Pos" = 0, "Neg" = 1),
                   selected = 0),
      actionButton("calc", "calculate", class = "btn-primary")
    ),
    mainPanel(
      h3("Calculation result:"),
      verbatimTextOutput("riskScore"),
      h4("Survival probability:"),
      verbatimTextOutput("survProb"),
      hr(),
      h5("Parameter Description:"),
      h6("1. TyG = ln[fasting triglycerides(mg/dL) × fasting blood glucose(mg/dL)/2]"),
      h6("2. For other parameter units, please refer to the clinical test report.")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$calc, {
    # 构建新数据框
    new_data <- data.frame(
      Age = as.numeric(input$Age),
      NLR = as.numeric(input$NLR),
      PLR = as.numeric(input$PLR),
      TyG = as.numeric(input$TyG),
      CEA = as.numeric(input$CEA),
      CA199 = as.numeric(input$CA199),
      Tumor_differentiation = as.numeric(input$Tumor_differentiation),
      UICC = as.numeric(input$UICC),
      Perineural_invasion = as.numeric(input$Perineural_invasion)
    )
    
    # 检查列名
    if(!all(colnames(new_data) %in% fit$subFeature)) {
      showNotification("Error: Model variable mismatch! Please check the data column names",
                       type = "error")
      return()
    }
    
    # 计算风险评分
    risk_score <- predict(fit, newdata = new_data, type = "link")
    
    # 计算生存概率
    surv_prob <- summary(baseline_surv, times = c(1, 3, 5))$surv
    patient_surv_prob <- surv_prob^exp(risk_score - mean_risk_score)
    
    # 显示结果
    output$riskScore <- renderText({
      sprintf("Risk score = %.2f", risk_score)
    })
    
    output$survProb <- renderText({
      sprintf("1-year survival probability = %.2f%%\n3-year survival probability = %.2f%%\n5-year survival probability = %.2f%%",
              patient_surv_prob[1] * 100,
              patient_surv_prob[2] * 100,
              patient_surv_prob[3] * 100)
    })
  })
}

shinyApp(ui, server)
