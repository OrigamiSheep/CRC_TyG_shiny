library(shiny)
library(gbm)
library(survival)

# 加载模型、基线生存函数和 mean_risk_score
load("gbm_model.RData")
load("baseline_surv.RData")

# 定义UI
ui <- fluidPage(
  titlePanel("临床风险评分与生存概率计算器"),
  sidebarLayout(
    sidebarPanel(
      h4("请填写临床参数:"),
      radioButtons("Age", "年龄",
                   choices = list("<75岁" = 0, "≥75岁" = 1),
                   selected = 0),
      radioButtons("NLR", "中性粒细胞淋巴细胞比值(NLR)",
                   choices = list("<3.3" = 0, "≥3.3" = 1),
                   selected = 0),
      radioButtons("PLR", "血小板淋巴细胞比值(PLR)",
                   choices = list("<545.7" = 0, "≥545.7" = 1),
                   selected = 0),
      radioButtons("TyG", "甘油三酯葡萄糖指数(TyG)",
                   choices = list("<5.8" = 0, "≥5.8" = 1),
                   selected = 0),
      radioButtons("CEA", "癌胚抗原(CEA ng/mL)",
                   choices = list("≤5" = 0, ">5" = 1),
                   selected = 0),
      radioButtons("CA199", "CA199 (U/mL)",
                   choices = list("<30" = 0, "≥30" = 1),
                   selected = 0),
      selectInput("Tumor_differentiation", "肿瘤分化程度",
                  choices = list("高分化" = 1, 
                                 "中分化" = 2, 
                                 "低分化/未分化" = 3),
                  selected = 1),
      radioButtons("UICC", "UICC分期",
                   choices = list("I-II期" = 0, "III-IV期" = 1),
                   selected = 0),
      radioButtons("Perineural_invasion", "神经侵犯",
                   choices = list("阴性" = 0, "阳性" = 1),
                   selected = 0),
      actionButton("calc", "计算", class = "btn-primary")
    ),
    mainPanel(
      h3("计算结果:"),
      verbatimTextOutput("riskScore"),
      h4("生存概率:"),
      verbatimTextOutput("survProb"),
      hr(),
      h5("参数说明:"),
      h6("1. TyG = ln[空腹甘油三酯(mg/dL) × 空腹血糖(mg/dL)/2]"),
      h6("2. 其他参数单位参见临床检测报告")
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
      showNotification("错误：模型变量不匹配！请检查数据列名",
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
      sprintf("风险评分 = %.2f", risk_score)
    })
    
    output$survProb <- renderText({
      sprintf("1年生存概率 = %.2f%%\n3年生存概率 = %.2f%%\n5年生存概率 = %.2f%%",
              patient_surv_prob[1] * 100,
              patient_surv_prob[2] * 100,
              patient_surv_prob[3] * 100)
    })
  })
}

shinyApp(ui, server)
