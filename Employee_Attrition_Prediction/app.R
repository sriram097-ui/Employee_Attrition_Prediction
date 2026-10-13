# Employee Attrition Prediction — Professional Shiny Dashboard
# Run from this project folder with: shiny::runApp()
# Required packages: shiny, shinydashboard, tidyverse, ggplot2, DT, randomForest, scales

required_packages <- c("shiny", "shinydashboard", "tidyverse", "ggplot2", "DT", "randomForest", "scales")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "),
    "\nInstall them in the R console with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))"
  )
}

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(DT)
library(randomForest)
library(scales)

# ---------- Data loading and preparation ----------
find_data_file <- function() {
  candidates <- c(
    "data/employee_attrition.csv",
    "data/WA_Fn-UseC_-HR-Employee-Attrition.csv",
    "employee_attrition.csv",
    "WA_Fn-UseC_-HR-Employee-Attrition.csv"
  )
  found <- candidates[file.exists(candidates)]
  if (length(found)) return(found[[1]])
  NULL
}

data_path <- find_data_file()
if (is.null(data_path)) {
  # Small clearly-labelled demo dataset so the app can launch before the user's CSV is added.
  set.seed(2026)
  n <- 320
  demo <- tibble(
    EmployeeNumber = seq_len(n),
    Age = sample(20:60, n, TRUE),
    Department = sample(c("Research & Development", "Sales", "Human Resources"), n, TRUE, c(.62, .32, .06)),
    JobRole = sample(c("Research Scientist", "Sales Executive", "Laboratory Technician", "Manufacturing Director", "Healthcare Representative"), n, TRUE),
    MonthlyIncome = sample(seq(1800, 19000, by = 100), n, TRUE),
    YearsAtCompany = sample(0:35, n, TRUE),
    JobSatisfaction = sample(1:4, n, TRUE),
    WorkLifeBalance = sample(1:4, n, TRUE),
    OverTime = sample(c("Yes", "No"), n, TRUE, c(.28, .72)),
    Attrition = ifelse(runif(n) < plogis(-2.25 + .055 * (35 - Age) + .8 * (OverTime == "Yes") +
                                      .045 * (5 - YearsAtCompany) + .00003 * (7000 - MonthlyIncome)),
                       "Yes", "No")
  )
  employee_data <- demo
  demo_mode <- TRUE
} else {
  employee_data <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)
  demo_mode <- FALSE
}

# Normalize common column-name variations.
names(employee_data) <- make.names(names(employee_data), unique = TRUE)
if (!"Attrition" %in% names(employee_data)) {
  attr_candidate <- names(employee_data)[tolower(names(employee_data)) %in% c("attrition", "left", "churn", "target")]
  if (length(attr_candidate)) names(employee_data)[match(attr_candidate[[1]], names(employee_data))] <- "Attrition"
}
if (!"Attrition" %in% names(employee_data)) employee_data$Attrition <- "Unknown"

employee_data$Attrition <- as.character(employee_data$Attrition)
employee_data$Attrition[is.na(employee_data$Attrition) | employee_data$Attrition == ""] <- "Unknown"
employee_data$Attrition <- factor(employee_data$Attrition)

numeric_cols <- names(employee_data)[vapply(employee_data, is.numeric, logical(1))]
categorical_cols <- names(employee_data)[vapply(employee_data, function(x) is.character(x) || is.factor(x), logical(1))]
default_num <- intersect(c("Age", "MonthlyIncome", "YearsAtCompany"), numeric_cols)
if (!length(default_num)) default_num <- head(numeric_cols, 1)
default_cat <- intersect(c("Department", "JobRole", "OverTime"), categorical_cols)
if (!length(default_cat)) default_cat <- head(categorical_cols, 1)

theme_set(theme_minimal(base_size = 13))
navy <- "#14213D"
teal <- "#12B8A6"
coral <- "#F06C5B"
blue <- "#4D7CFE"
muted <- "#718096"

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "People Analytics", titleWidth = 280),
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Executive Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Explore Workforce", tabName = "explore", icon = icon("users")),
      menuItem("Attrition Insights", tabName = "insights", icon = icon("magnifying-glass-chart")),
      menuItem("Prediction Lab", tabName = "prediction", icon = icon("brain")),
      menuItem("Employee Records", tabName = "records", icon = icon("table"))
    ),
    hr(),
    div(style = "padding: 0 15px;",
      h4("Dashboard filters", style = "color:white;"),
      selectInput("department", "Department", choices = c("All departments", sort(unique(as.character(employee_data[[default_cat[1]]])))), selected = "All departments"),
      selectInput("attrition", "Attrition status", choices = c("All", levels(employee_data$Attrition)), selected = "All"),
      helpText("Filters apply to the overview, charts, and records.")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');
      body, h1, h2, h3, h4, .main-sidebar, .main-header { font-family: 'Inter', sans-serif; }
      .content-wrapper, .right-side { background: #F4F7FB; }
      .main-header .logo { font-weight: 800; letter-spacing: .2px; }
      .main-header .navbar { background: #14213D !important; }
      .main-header .logo { background: #101A30 !important; }
      .main-sidebar { background: #14213D !important; }
      .sidebar-menu>li.active>a { border-left-color: #12B8A6 !important; background: #203354 !important; }
      .box { border-radius: 12px; border-top: 0; box-shadow: 0 5px 22px rgba(25,45,75,.06); }
      .box-header { border-radius: 12px 12px 0 0; }
      .small-box { border-radius: 12px; box-shadow: 0 5px 22px rgba(25,45,75,.08); }
      .small-box h3 { font-weight: 800; }
      .content { padding: 22px; }
      .section-title { font-weight: 800; color: #14213D; margin: 2px 0 4px; }
      .section-subtitle { color: #718096; margin-bottom: 18px; }
      .notice { padding: 12px 15px; background: #EAF8F6; border-left: 4px solid #12B8A6; border-radius: 7px; margin-bottom: 15px; color: #244; }
      table.dataTable thead th { background: #EEF2F8; color: #14213D; }
    "))),
    tabItems(
      tabItem(tabName = "overview",
        h2(class = "section-title", "Executive Overview"),
        p(class = "section-subtitle", "A real-time snapshot of workforce composition, retention, and people risks."),
        uiOutput("data_notice"),
        fluidRow(
          valueBoxOutput("kpi_employees", width = 3),
          valueBoxOutput("kpi_attrition", width = 3),
          valueBoxOutput("kpi_retained", width = 3),
          valueBoxOutput("kpi_age", width = 3)
        ),
        fluidRow(
          box(title = "Attrition distribution", status = "primary", solidHeader = TRUE, width = 5, plotOutput("attrition_donut", height = 290)),
          box(title = "Attrition by department", status = "primary", solidHeader = TRUE, width = 7, plotOutput("dept_chart", height = 290))
        ),
        fluidRow(
          box(title = "Workforce profile", status = "primary", solidHeader = TRUE, width = 6, plotOutput("age_chart", height = 270)),
          box(title = "Monthly income distribution", status = "primary", solidHeader = TRUE, width = 6, plotOutput("income_chart", height = 270))
        )
      ),
      tabItem(tabName = "explore",
        h2(class = "section-title", "Explore the Workforce"),
        p(class = "section-subtitle", "Compare workforce attributes and discover patterns across employee groups."),
        fluidRow(
          box(title = "Choose chart dimensions", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("explore_x", "Group by", choices = categorical_cols, selected = if (length(default_cat)) default_cat[1] else NULL),
            selectInput("explore_metric", "Numeric measure", choices = numeric_cols, selected = if (length(default_num)) default_num[1] else NULL),
            radioButtons("explore_stat", "Summary", choices = c("Average" = "mean", "Median" = "median", "Count" = "count"), inline = FALSE)
          ),
          box(title = "Workforce comparison", status = "primary", solidHeader = TRUE, width = 8, plotOutput("explore_chart", height = 390))
        ),
        fluidRow(box(title = "Attrition rate by selected group", status = "primary", solidHeader = TRUE, width = 12, plotOutput("explore_attrition", height = 320)))
      ),
      tabItem(tabName = "insights",
        h2(class = "section-title", "Attrition Insights"),
        p(class = "section-subtitle", "Understand where attrition is concentrated and which workforce indicators vary by outcome."),
        fluidRow(
          box(title = "Attrition rate by category", status = "primary", solidHeader = TRUE, width = 6,
            selectInput("insight_category", "Category", choices = categorical_cols, selected = if (length(default_cat)) default_cat[1] else NULL),
            plotOutput("category_rate", height = 330)
          ),
          box(title = "Numeric indicators by attrition", status = "primary", solidHeader = TRUE, width = 6,
            selectInput("insight_numeric", "Numeric indicator", choices = numeric_cols, selected = if (length(default_num)) default_num[1] else NULL),
            plotOutput("numeric_attrition", height = 330)
          )
        ),
        fluidRow(box(title = "Correlation overview", status = "primary", solidHeader = TRUE, width = 12, plotOutput("correlation_plot", height = 380)))
      ),
      tabItem(tabName = "prediction",
        h2(class = "section-title", "Prediction Lab"),
        p(class = "section-subtitle", "Train a Random Forest model to estimate attrition risk from available employee attributes."),
        div(class = "notice", "Model results are exploratory, not a substitute for human review. Use predictions responsibly and avoid employment decisions based solely on an algorithm."),
        fluidRow(
          box(title = "Model setup", status = "primary", solidHeader = TRUE, width = 4,
            p("The model uses available predictors and excludes identifier-like columns where possible."),
            sliderInput("ntree", "Number of trees", min = 100, max = 600, value = 250, step = 50),
            actionButton("train_model", "Train / refresh model", icon = icon("play"), class = "btn-primary"),
            br(), br(), uiOutput("model_status")
          ),
          box(title = "Model performance", status = "primary", solidHeader = TRUE, width = 8,
            valueBoxOutput("model_accuracy", width = 6),
            valueBoxOutput("model_oob", width = 6),
            plotOutput("importance_plot", height = 300)
          )
        ),
        fluidRow(box(title = "Score an employee profile", status = "primary", solidHeader = TRUE, width = 12,
          uiOutput("prediction_inputs"),
          actionButton("score_employee", "Estimate attrition risk", icon = icon("calculator"), class = "btn-success"),
          br(), br(), uiOutput("prediction_result")
        ))
      ),
      tabItem(tabName = "records",
        h2(class = "section-title", "Employee Records"),
        p(class = "section-subtitle", "Search, sort, and export the currently filtered employee dataset."),
        fluidRow(
          box(title = "Dataset", status = "primary", solidHeader = TRUE, width = 12,
            downloadButton("download_data", "Download filtered CSV", class = "btn-primary"),
            br(), br(), DTOutput("employee_table")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    d <- employee_data
    if (!is.null(input$attrition) && input$attrition != "All") d <- d[d$Attrition == input$attrition, , drop = FALSE]
    if (!is.null(input$department) && input$department != "All departments" && length(default_cat)) {
      col <- default_cat[1]
      d <- d[as.character(d[[col]]) == input$department, , drop = FALSE]
    }
    d
  })

  output$data_notice <- renderUI({
    if (demo_mode) div(class = "notice", strong("Demo data is active. "),
      "Place your real CSV at data/employee_attrition.csv and restart the app to use your dataset.")
    else div(class = "notice", strong("Dataset loaded: "), data_path, " · ", nrow(employee_data), " employee records.")
  })

  output$kpi_employees <- renderValueBox(valueBox(comma(nrow(filtered_data())), "Employees in view", icon = icon("users"), color = "aqua"))
  output$kpi_attrition <- renderValueBox({
    d <- filtered_data()
    rate <- if (nrow(d) && any(tolower(as.character(d$Attrition)) == "yes")) mean(tolower(as.character(d$Attrition)) == "yes", na.rm = TRUE) else if (nrow(d)) mean(tolower(as.character(d$Attrition)) %in% c("left", "true", "1"), na.rm = TRUE) else 0
    valueBox(percent(rate, accuracy = 0.1), "Attrition rate", icon = icon("user-minus"), color = "red")
  })
  output$kpi_retained <- renderValueBox({
    d <- filtered_data()
    yes <- tolower(as.character(d$Attrition)) %in% c("yes", "left", "true", "1")
    valueBox(comma(sum(!yes, na.rm = TRUE)), "Employees not marked attrition", icon = icon("user-check"), color = "green")
  })
  output$kpi_age <- renderValueBox({
    d <- filtered_data()
    if ("Age" %in% names(d) && is.numeric(d$Age) && nrow(d)) valueBox(round(mean(d$Age, na.rm = TRUE), 1), "Average age", icon = icon("cake-candles"), color = "purple")
    else valueBox("N/A", "Average age", icon = icon("chart-line"), color = "purple")
  })

  output$attrition_donut <- renderPlot({
    d <- filtered_data()
    counts <- d %>% count(Attrition)
    ggplot(counts, aes(x = 2, y = n, fill = Attrition)) +
      geom_col(width = .8, color = "white") + coord_polar(theta = "y") +
      xlim(.5, 2.5) + scale_fill_manual(values = c("Yes" = coral, "No" = teal), na.value = muted) +
      geom_text(aes(label = paste0(Attrition, "\n", comma(n))), position = position_stack(vjust = .5), color = "white", fontface = "bold", size = 4) +
      theme_void() + theme(legend.position = "none")
  })

  output$dept_chart <- renderPlot({
    d <- filtered_data()
    group_col <- if ("Department" %in% names(d)) "Department" else if (length(default_cat)) default_cat[1] else "Attrition"
    d %>% mutate(.attr_yes = tolower(as.character(Attrition)) %in% c("yes", "left", "true", "1")) %>%
      group_by(.data[[group_col]]) %>% summarise(rate = mean(.attr_yes, na.rm = TRUE), n = n(), .groups = "drop") %>%
      ggplot(aes(x = reorder(.data[[group_col]], rate), y = rate, fill = rate)) +
      geom_col(width = .65) + coord_flip() + scale_y_continuous(labels = percent) +
      scale_fill_gradient(low = teal, high = coral) +
      labs(x = NULL, y = "Attrition rate", caption = "Rate based on records marked as attrition") +
      theme(legend.position = "none")
  })

  output$age_chart <- renderPlot({
    d <- filtered_data()
    if ("Age" %in% names(d) && is.numeric(d$Age)) {
      ggplot(d, aes(x = Age, fill = Attrition)) + geom_histogram(bins = 18, alpha = .85, position = "identity") +
        scale_fill_manual(values = c("Yes" = coral, "No" = teal), na.value = muted) + labs(x = "Age", y = "Employees", fill = "Attrition")
    } else {
      col <- if (length(numeric_cols)) numeric_cols[1] else NULL
      validate(need(!is.null(col), "No numeric columns available"))
      ggplot(d, aes(x = .data[[col]])) + geom_histogram(bins = 18, fill = blue, alpha = .9) + labs(x = col, y = "Employees")
    }
  })

  output$income_chart <- renderPlot({
    d <- filtered_data()
    col <- if ("MonthlyIncome" %in% names(d)) "MonthlyIncome" else if (length(numeric_cols)) numeric_cols[1] else NULL
    validate(need(!is.null(col), "No numeric columns available"))
    ggplot(d, aes(x = .data[[col]], fill = Attrition)) + geom_density(alpha = .45, na.rm = TRUE) +
      scale_fill_manual(values = c("Yes" = coral, "No" = teal), na.value = muted) + labs(x = col, y = "Density", fill = "Attrition")
  })

  output$explore_chart <- renderPlot({
    d <- filtered_data()
    req(input$explore_x)
    xcol <- input$explore_x
    if (identical(input$explore_stat, "count")) {
      d %>% count(.data[[xcol]]) %>% ggplot(aes(x = reorder(as.character(.data[[xcol]]), n), y = n)) +
        geom_col(fill = blue, width = .7) + coord_flip() + labs(x = xcol, y = "Employees")
    } else {
      req(input$explore_metric)
      fun <- if (input$explore_stat == "median") median else mean
      d %>% group_by(.data[[xcol]]) %>% summarise(value = fun(.data[[input$explore_metric]], na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(x = reorder(as.character(.data[[xcol]]), value), y = value)) +
        geom_col(fill = teal, width = .7) + coord_flip() + labs(x = xcol, y = paste(input$explore_stat, input$explore_metric))
    }
  })

  output$explore_attrition <- renderPlot({
    d <- filtered_data()
    req(input$explore_x)
    d %>% mutate(.attr_yes = tolower(as.character(Attrition)) %in% c("yes", "left", "true", "1")) %>%
      group_by(.data[[input$explore_x]]) %>% summarise(rate = mean(.attr_yes, na.rm = TRUE), n = n(), .groups = "drop") %>%
      ggplot(aes(x = reorder(as.character(.data[[input$explore_x]]), rate), y = rate)) +
      geom_col(fill = coral, width = .7) + coord_flip() + scale_y_continuous(labels = percent) +
      labs(x = input$explore_x, y = "Attrition rate")
  })

  output$category_rate <- renderPlot({
    d <- filtered_data()
    req(input$insight_category)
    d %>% mutate(.attr_yes = tolower(as.character(Attrition)) %in% c("yes", "left", "true", "1")) %>%
      group_by(.data[[input$insight_category]]) %>% summarise(rate = mean(.attr_yes, na.rm = TRUE), n = n(), .groups = "drop") %>%
      ggplot(aes(x = reorder(as.character(.data[[input$insight_category]]), rate), y = rate)) +
      geom_col(fill = coral, width = .7) + coord_flip() + scale_y_continuous(labels = percent) +
      labs(x = input$insight_category, y = "Attrition rate")
  })

  output$numeric_attrition <- renderPlot({
    d <- filtered_data()
    req(input$insight_numeric)
    ggplot(d, aes(x = Attrition, y = .data[[input$insight_numeric]], fill = Attrition)) +
      geom_boxplot(alpha = .8, outlier.alpha = .25) +
      scale_fill_manual(values = c("Yes" = coral, "No" = teal), na.value = muted) +
      labs(x = "Attrition status", y = input$insight_numeric, fill = "Attrition")
  })

  output$correlation_plot <- renderPlot({
    d <- filtered_data()
    cols <- names(d)[vapply(d, is.numeric, logical(1))]
    validate(need(length(cols) >= 2, "At least two numeric columns are required for a correlation plot."))
    cor_df <- cor(d[, cols, drop = FALSE], use = "pairwise.complete.obs")
    cor_long <- as.data.frame(as.table(cor_df))
    names(cor_long) <- c("Variable1", "Variable2", "Correlation")
    ggplot(cor_long, aes(Variable1, Variable2, fill = Correlation)) +
      geom_tile(color = "white") + geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3) +
      scale_fill_gradient2(low = coral, mid = "white", high = teal, midpoint = 0, limits = c(-1, 1)) +
      labs(x = NULL, y = NULL, fill = "Correlation") + theme(axis.text.x = element_text(angle = 40, hjust = 1))
  })

  model_state <- reactiveVal(NULL)
  model_message <- reactiveVal("Click Train / refresh model to fit a Random Forest.")
  observeEvent(input$train_model, {
    d <- employee_data
    target <- tolower(as.character(d$Attrition))
    d$Attrition <- factor(ifelse(target %in% c("yes", "left", "true", "1"), "Yes",
                                 ifelse(target %in% c("no", "stayed", "false", "0"), "No", NA)))
    d <- d[!is.na(d$Attrition), , drop = FALSE]
    predictors <- setdiff(names(d), c("Attrition", "EmployeeNumber", "EmployeeCount", "Over18", "StandardHours"))
    predictors <- predictors[vapply(d[predictors], function(x) length(unique(na.omit(x))) > 1, logical(1))]
    if (nrow(d) < 30 || length(predictors) < 1 || nlevels(d$Attrition) < 2) {
      model_message("Not enough usable data or both attrition classes are not present.")
      model_state(NULL)
      return()
    }
    d <- d[, c("Attrition", predictors), drop = FALSE]
    for (nm in names(d)) {
      if (is.character(d[[nm]])) d[[nm]] <- as.factor(d[[nm]])
      if (is.factor(d[[nm]])) d[[nm]][is.na(d[[nm]])] <- "Unknown"
      if (is.numeric(d[[nm]])) d[[nm]][is.na(d[[nm]])] <- median(d[[nm]], na.rm = TRUE)
    }
    set.seed(42)
    tryCatch({
      fit <- randomForest(Attrition ~ ., data = d, ntree = input$ntree, importance = TRUE, na.action = na.omit)
      model_state(list(fit = fit, predictors = predictors, train_data = d))
      model_message(paste("Model trained on", nrow(d), "records using", length(predictors), "predictors."))
    }, error = function(e) {
      model_state(NULL)
      model_message(paste("Model could not be trained:", conditionMessage(e)))
    })
  })

  output$model_status <- renderUI({
    div(class = "notice", textOutput("model_status_text"))
  })
  output$model_status_text <- renderText(model_message())

  output$model_accuracy <- renderValueBox({
    m <- model_state()
    if (is.null(m)) valueBox("—", "Out-of-bag accuracy", icon = icon("bullseye"), color = "aqua")
    else {
      err <- tail(m$fit$err.rate[, "OOB"], 1)
      valueBox(percent(1 - err, accuracy = .1), "Out-of-bag accuracy", icon = icon("bullseye"), color = "aqua")
    }
  })
  output$model_oob <- renderValueBox({
    m <- model_state()
    if (is.null(m)) valueBox("—", "OOB error", icon = icon("chart-line"), color = "purple")
    else valueBox(percent(tail(m$fit$err.rate[, "OOB"], 1), accuracy = .1), "OOB error", icon = icon("chart-line"), color = "purple")
  })
  output$importance_plot <- renderPlot({
    m <- model_state()
    validate(need(!is.null(m), "Train a model to view predictor importance."))
    imp <- as.data.frame(importance(m$fit))
    imp$Predictor <- rownames(imp)
    metric <- if ("%IncMSE" %in% names(imp)) "%IncMSE" else names(imp)[1]
    imp <- imp %>% arrange(desc(.data[[metric]])) %>% slice_head(n = 12)
    ggplot(imp, aes(x = reorder(Predictor, .data[[metric]]), y = .data[[metric]])) +
      geom_col(fill = blue, width = .7) + coord_flip() + labs(x = NULL, y = metric, title = "Top predictors")
  })

  output$prediction_inputs <- renderUI({
    d <- employee_data
    m <- model_state()
    predictors <- if (!is.null(m)) m$predictors else setdiff(names(d), c("Attrition", "EmployeeNumber", "EmployeeCount", "Over18", "StandardHours"))
    predictors <- head(predictors, 8)
    controls <- lapply(predictors, function(nm) {
      x <- d[[nm]]
      id <- paste0("pred_", make.names(nm))
      if (is.numeric(x)) {
        val <- median(x, na.rm = TRUE)
        numericInput(id, nm, value = val, min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
      } else {
        opts <- sort(unique(as.character(x)))
        selectInput(id, nm, choices = opts, selected = if (length(opts)) opts[1] else NULL)
      }
    })
    do.call(tagList, controls)
  })

  output$prediction_result <- renderUI(NULL)
  observeEvent(input$score_employee, {
    m <- model_state()
    if (is.null(m)) {
      output$prediction_result <- renderUI(div(class = "notice", "Train the model first, then score a profile."))
      return()
    }
    newrow <- m$train_data[1, m$predictors, drop = FALSE]
    for (nm in m$predictors) {
      id <- paste0("pred_", make.names(nm))
      val <- input[[id]]
      if (is.null(val)) next
      if (is.numeric(m$train_data[[nm]])) newrow[[nm]] <- as.numeric(val)
      else newrow[[nm]] <- factor(as.character(val), levels = levels(m$train_data[[nm]]))
    }
    tryCatch({
      probs <- predict(m$fit, newdata = newrow, type = "prob")
      risk <- if ("Yes" %in% colnames(probs)) probs[1, "Yes"] else max(probs[1, ])
      output$prediction_result <- renderUI({
        div(class = "notice",
          h3(paste0("Estimated attrition probability: ", percent(risk, accuracy = .1))),
          p(if (risk >= .5) "The model flags this profile as higher attrition risk." else "The model flags this profile as lower attrition risk."),
          p("This is a statistical estimate, not a judgment about an individual employee.")
        )
      })
    }, error = function(e) {
      output$prediction_result <- renderUI(div(class = "notice", paste("Could not score this profile:", conditionMessage(e))))
    })
  })

  output$employee_table <- renderDT({
    datatable(filtered_data(), filter = "top", extensions = "Buttons",
      options = list(pageLength = 10, scrollX = TRUE, dom = "Bfrtip",
                     buttons = c("copy", "csv", "excel"), autoWidth = TRUE),
      rownames = FALSE, class = "stripe hover compact")
  })
  output$download_data <- downloadHandler(
    filename = function() paste0("employee_attrition_filtered_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
