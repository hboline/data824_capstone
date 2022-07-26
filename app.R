# DATA 824 Summer 2022
# Capstone Project
# Hayden D. Boline
# Created 18 July 2022

# load libraries ----
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(corrplot)
library(modelr)
library(broom)
library(FactoMineR)
library(factoextra)
library(cluster)
library(scales)

# load and clean data ----
data <- read.csv("abalone.csv", header = TRUE)
data <- data %>% 
  mutate(sex = case_when(sex == "M" ~ "male",
                         sex == "F" ~ "female",
                         sex == "I" ~ "infant")) %>% 
  mutate(sex = factor(sex, levels = c("male", "female", "infant")),
         across(.cols = colnames(.)[2:8], ~ .x*200),
         age = rings + 1) %>% 
  filter(height < 60) %>% 
  select(-rings)

var.names <- setNames(colnames(data), c("Sex", "Length", "Diameter", "Height", "Whole Weight", "Shucked Weight", "Viscera Weight", "Shell Weight", "Age"))

# define user-interface 
ui <- dashboardPage(skin = "green",
  
  # header
  dashboardHeader(title = "DATA 824 Capstone"),
  
  # sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Models", tabName = "models", icon = icon("eye-open", lib = "glyphicon")),
      menuItem("Data Statistics", tabName = "datastat", icon = icon("filter", lib = "glyphicon")),
      menuItem("Data Exploration", tabName = "dataexploration", icon = icon("search", lib = "glyphicon")),
      menuItem("Interactive Visualization", tabName = "visualizations", icon = icon("hand-up", lib = "glyphicon")),
      menuItem("Data", tabName = "datatable", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("question-sign", lib = "glyphicon"))
    )
  ),
  
  # body
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      
      # prediction models
      tabItem(tabName = "models",
        h1("Data Models"),
        h4("Linear Regression for Age Prediction & Principal Components Analysis"),
        br(),
        fluidRow(
          column(width = 3,
            box(width = 12, height = 140, title = "Model Selection",
              selectInput("modelchoice", label = "Select model type:",
                          choices = c("Linear Regression" = "linreg", "PCA" = "pca"),
                          selected = "linreg")
            ),
            box(width = 12, height = 540, title = "Options",
              conditionalPanel(
                condition = "input.modelchoice == 'linreg'",
                selectInput("LRvars", label = "Choose independent variables:",
                            choices = var.names[-9], multiple = TRUE,
                            selected = c("sex", "height", "whole.weight")),
                sliderInput("splt", "Test/Train Split (Training Proportion)",
                            min = .5, max = .95, value = .8, step = .05),
                actionButton("LRgo", "Run Regression Model")
              ),
              conditionalPanel( # PCA ----
                condition = "input.modelchoice == 'pca'",
                selectInput("pcaplots", label = "Choose plot:", 
                            choices = c("PCA Scatterplot" = "opt1", "Information Table" = "opt2"),
                            selected = "opt1"),
                conditionalPanel(
                  condition = "input.pcaplots == 'opt1'",
                  h4("Select PCA Components:"),
                  uiOutput("PCAopt1"),
                  uiOutput("PCAopt2"),
                  uiOutput("PCAopt3"),
                  checkboxInput("incinf", "Include infants?", value = TRUE)
                ),
                conditionalPanel(
                  condition = "input.pcaplots == 'opt2'",
                  selectInput("PCAtableopt", label = "Select Table:",
                              choices = c("Eigenvalues" = "opt1", "Eigenspace Data Coordinates" = "opt2",
                                          "Variable Correlation w/ PCA Axes" = "opt3")) 
                )
              ) # ----
            )
          ),
          conditionalPanel(
            condition = "input.modelchoice == 'linreg'",
            
            column(width = 5,
              box(width = 12, height = 440, title = "Regression Prediction Results Plot",
                radioGroupButtons("LRtag", label = NULL, choices = c("Test" = "test", "Train" = "train", "Both" = "both")),
                plotOutput(outputId = "LRabline", height = 330)
              ),
              box(width = 12, height = 240,
                plotOutput(outputId = "LRrh", height = 220)
              )
            ),
            column(width = 4,
              box(width = 12, height = 300
                
              )
            )
          ),
          conditionalPanel( # PCA ----
            condition = "input.modelchoice == 'pca'",
            column(width = 9,
              conditionalPanel(
                condition = "input.pcaplots == 'opt1'",
                box(width = 12, height = 700,
                  plotOutput(outputId = "PCA1", height = 650)
                )
              ),               
              conditionalPanel(
                condition = "input.pcaplots == 'opt2'",
                DT::dataTableOutput(outputId = "PCAtable")
              )
            )
          ) # ----
          
        )
      ),
      
      # data statistics ----
      tabItem(tabName = "datastat",
        h1("Data Statistics"),
        h4("Statistical Summaries and Tests"),
        br(),
        fluidRow(
          # summary options
          column(width = 7,
            box(width = 12, height = 360, title = "General Summary",
                DT::DTOutput(outputId = "dataSummary")
            )
          ),
          column(width = 5,
            box(width = 12, height = 360,
                radioGroupButtons("statbarchoice", label = "Select Statistic to Visualize:",
                             choices = c("Mean" = 'mean', "Variance" = 'var', "Std. Dev." = 'sd',
                                         "Min" = 'min', "Median" = 'median', "Max" = 'max')),
                plotOutput(outputId = "stddevplot", height = 260)
            )
          )
        ),
        
        fluidRow(
          column(width = 3,
            box(width = 12, title = "Statistical Test", height = 360,
              selectInput("stattestchoice", "Choose statistical test:",
                          choices = c("Gender Measures (2-sample t-test)" = "st1",
                                      "Linear Regression" = "st2")),
              conditionalPanel(
                condition = "input.stattestchoice == 'st1'",
                radioButtons("st1c", label = "Choose gender pair:", inline = TRUE,
                             choices = c("Male-Female" = "mf", "Male-Infant" = "mi", "Female-Infant" = "fi")),
                selectInput("stattestvar", label = "Choose variable to test:",
                            choices = var.names[2:9])
              ),
              conditionalPanel(
                condition = "input.stattestchoice == 'st2'",
                selectInput("st2ind", label = "Choose independent variables:",
                            choices = var.names, multiple = TRUE),
                uiOutput("st2depUI")
              ),
              actionButton("stattestgo", "Perform Test")
            )
          ),
          column(width = 4,
            box(width = 12, title = "Test Results", height = 360,
              conditionalPanel(
                condition = "input.stattestchoice == 'st1'",
                DT::DTOutput(outputId = "st1")
              ),
              conditionalPanel(
                condition = "input.stattestchoice == 'st2'",
                DT::DTOutput(outputId = "st2")
              )
            )
          ),
          column(width = 5,
            conditionalPanel(
              condition = "input.stattestchoice == 'st2'",
              box(width = 12, height = 360, title = "Linear Model Residual Distribution",
                condition = "input.stattestchoice == 'st2'",
                plotOutput(outputId = "st2plot", height = 300)
              )
            )
          )
        )
        
      ), # ----
      
      # data exploration ----
      tabItem(tabName = "dataexploration",
        h1("Data Exploration & Visualization"),
        h4("Box, Violin, Frequency, and Correlation Plots"),
        br(),
        fluidRow(
          # selection and options
          column(width = 3,
            # main selection
            box(width = 12, title = "Select Visualization:",
              selectInput(inputId = "viztype", label = NULL, 
                          choices = c("Box Plots" = "box", "Frequency Plots" = "dist", 
                                      "Violin Plots" = "violin", "Correlation Plot" = "corr"))
            ),
            
            # histogram/area plot options
            conditionalPanel(
              condition = "input.viztype == 'dist'",
              box(width = 12, title = "Plot Options:",
                selectInput(inputId = "distvar", label = "Select measurement type:",
                            choices = c("Lengths (mm)" = "m", "Weights (g)" = "w", "Age by Gender" = "a")),
                radioGroupButtons(inputId = "disttype", label = "Selection distribution type:",
                                  choices = c("Histogram" = "hist", "Area Plot" = "area"))
              )
            ),
            
            # violin plot options 
            conditionalPanel(
              condition = "input.viztype == 'violin'",
              box(width = 12, title = "Plot Options:",
                  selectInput(inputId = "viovar", label = "Select measurement type:",
                              choices = c("Lengths (mm)" = "m", "Weights (g)" = "w", "Age by Gender" = "a"))
              )
            ),
            
            # box plot options
            conditionalPanel(
              condition = "input.viztype == 'box'",
              box(width = 12, title = "Plot Options:",
                selectInput(inputId = "boxvar", label = "Select measurement type:",
                            choices = c("Lengths (mm)" = "m", "Weights (g)" = "w", "Age by Gender" = "a")),
                checkboxInput(inputId = "boxdots", label = "Plot data points?", value = FALSE)
              )
            ),
            
            # correlation plot options
            conditionalPanel(
              condition = "input.viztype == 'corr'",
              box(width = 12, title = "Plot Options:",
                  radioGroupButtons(inputId = "corrmethod", label = "Select correlation method:",
                                    choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"),
                                    selected = "pearson")
              )
            )
          ), 
          
          # plots
          column(width = 9,
                 
            # boxplot
            conditionalPanel(
              condition = "input.viztype == 'box'",
              box(width = 12,
                plotOutput(outputId = "boxplot", height = "700px")
              )
            ), #
            
            # violin plot
            conditionalPanel(
              condition = "input.viztype == 'violin'",
              box(width = 12,
                  plotOutput(outputId = "violinplot", height = "700px")
              )
            ), #
            
            # frequency plots
            conditionalPanel(
              condition = "input.viztype == 'dist'",
              box(width = 12,
                plotOutput(outputId = "distplot", height = "600px")
              )
            ),
            
            # correlation plot
            conditionalPanel(
              condition = "input.viztype == 'corr'",
              box(width = 10,
                  plotOutput(outputId = "corrplot", height = "600px")
              )
            )
          )
        )
      ), # ----
      
      # data table ----
      tabItem(tabName = "datatable",
        h1("Abalone Data"),
        br(),
        fluidRow(
          column(width = 12,
            DT::dataTableOutput(outputId = 'mydata')
          )
        )
      ), # ----
      
      # about page ----
      tabItem(tabName = "about",
        h1("HELLO WORLD"),
        p("lorem ipsum")
      ), # ----
      
      # interactive visualization tab ----
      tabItem(tabName = "visualizations",
        h1("Interactive Visualization"),
        br(),
        fluidRow(
          column(width = 3,
                 
            box(width = 12, title = "Variable Options",
              selectInput(inputId = "xvar", label = "Select x-variable:", 
                          choices = append(var.names[9], var.names[2:8]), selected = "height"),
              selectInput(inputId = "yvar", label = "Select y-variable:", 
                          choices = append(var.names[9], var.names[2:8]), selected = "age"),
              checkboxGroupButtons(inputId = "sexes", label = "Select genders:", 
                                   choiceNames = c("Male", "Female", "Infant"),
                                   choiceValues = c("male", "female", "infant"),
                                   selected = c("male", "female", "infant"),
                                   checkIcon = list(yes = icon("ok", lib = "glyphicon")))
            ),
            box(width = 12, title = "Plot Options",
              sliderInput(inputId = "alpha", label = "Point opacity:",
                          min = 0.0, max = 1.0, step = 0.1, value = 1.0),
              sliderInput(inputId = "ptsize", label = "Point size:",
                          min = 0.1, max = 2.0, step = 0.1, value = 2.0),
              checkboxInput(inputId = "jitchk", label = "Jitter points?", value = TRUE),
              checkboxInput(inputId = "matchfill", label = "Match fill color?", value = FALSE)
            )
          ),
          
          column(width = 9,
            box(width = 40,
              plotlyOutput("plot1", height = "700px")
            )
          )
        )
      ) # ----
      
    )
  )
)


# define server function 
server <- function(input, output, session) {
  
  LRdata <- eventReactive(input$LRgo, {
    #w <- row.names(data) %in% sample(1:nrow(data), floor(input$splt*nrow(data)))
    #data_train <- data[which(w),]
    #data_test <- data[which(!w),]
    #m <- lm(paste0("age ~ ", paste(input$LRvars, collapse = " + ")), data = data_train)
    m <- LRmodel()
    preds <- predict(m, newdata = data_test)
    df_test <- data.frame(cbind(data_test[,c(input$LRvars,"age")], preds))
    df_train <- data.frame(cbind(data_train[,c(input$LRvars,"age")], preds = predict(m)))
    df_test <- df_test %>% mutate(split = "test", col = case_when(preds > age ~ 1, preds <= age ~ -1))
    df_train <- df_train %>% mutate(split = "train", col = case_when(preds > age ~ 1, preds <= age ~ -1))
    df <- union(df_test, df_train)
    df
  })
  
  LRmodel <- eventReactive(input$LRgo, {
    w <- row.names(data) %in% sample(1:nrow(data), floor(input$splt*nrow(data)))
    data_train <- data[which(w),]
    data_test <- data[which(!w),]
    lm(paste0("age ~ ", paste(input$LRvars, collapse = " + ")), data = data_train)
  })
  
  # prediction models
  observeEvent(input$LRgo, {
    output$LR1 <- DT::renderDataTable({
      DT::datatable(LRdata())
    })
    
    output$LRabline <- renderPlot({
      if (input$LRtag != "both"){
        df <- LRdata() %>% filter(split == input$LRtag)
      } else {
        df <- LRdata()
      }
      df %>%
        ggplot(aes(x = age, y = preds, color = as.factor(col))) +
        geom_abline(size = 1, alpha = 0.5) + 
        geom_point(position = position_jitter(seed = 1)) +
        scale_x_continuous(breaks = seq(0,50,5)) +
        scale_y_continuous(breaks = seq(0,50,5)) +
        xlab("Age (Actual)") +
        ylab("Age (Predicted)") +
        theme(legend.position = "none",
              axis.text = element_text(size = 11),
              axis.title = element_text(size = 15),
              panel.border = element_rect(fill = NA, color = "black"))
    })
    
    output$LRrh <- renderPlot({
      if (input$LRtag != "both"){
        df <- LRdata() %>% filter(split == input$LRtag)
      } else {
        df <- LRdata()
      }
      LRmodel()%>% 
        augment(newdata = df) %>% 
        ggplot(aes(`.resid`)) +
        geom_freqpoly(binwidth = 0.1, fill = "coral") +
        xlab("Residual") +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        theme(axis.title.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(fill = NA, color = "black"))
    })
    
    output$TEST <- DT::renderDataTable({
      if (input$LRtag != "both"){
        df <- LRdata() %>% filter(split == input$LRtag)
      } else {
        df <- LRdata()
      }
      LRmodel() %>% 
        augment(newdata = df) %>%
        DT::datatable()
    })
  })
  
  # PCA ----
  output$PCAopt1 <- renderUI({
    r <- if (input$incage){1:8} else {1:7}
    selectInput("PCAcomp1", "Component 1:", choices = r, selected = 1)
  })
  output$PCAopt2 <- renderUI({
    r <- if (input$incage){1:8} else {1:7}
    selectInput("PCAcomp2", "Component 2:", choices = r, selected = 2)
  })
  output$PCAopt3 <- renderUI({
    checkboxInput("incage", "Include age?", value = TRUE
  )})
  
  output$PCA1 <- renderPlot({
    axes <- as.integer(c(input$PCAcomp1,input$PCAcomp2))
    if (input$incinf == FALSE){
      df <- data %>% filter(!sex == "infant")
    } else {
      df <- data
    }
    if (input$incage == FALSE){
      m <- PCA(df[,c(-1,-9)], ncp = 8, graph = FALSE)
    } else {
      m <- PCA(df[-1], ncp = 8, graph = FALSE)
    }
    fviz_pca(m, axes = axes, repel = TRUE, col.ind = df$age, col.var = "black", label = "var") +
      scale_color_gradientn(colors = rainbow(length(levels(as.factor(df$age))))) +
      labs(color = "Age") +
      theme(plot.title = element_blank(),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 14))
  })
  
  output$PCAtable <- DT::renderDataTable({
    m <- PCA(data[-1], ncp = 8, graph = FALSE)
    if (input$PCAtableopt == "opt1"){
      df <- m$eig
    } else if (input$PCAtableopt == "opt2"){
      df <- m$ind$coord
    } else if (input$PCAtableopt == "opt3"){
      df <- m$var$cor
    }
    df <- as.data.frame(df) %>% 
      mutate(across(which(sapply(., is.numeric)), signif, 3))
    DT::datatable(df, options = list(scrollX = TRUE))
  }) # ----
  
  # data stats and summary ----
  output$dataSummary <- DT::renderDataTable({
    df <- round(as.data.frame(rbind(
      "Mean" = sapply(data[,-1], mean),
      "Variance" = sapply(data[,-1], var),
      "Std. Dev." = sapply(data[,-1], sd),
      "Min" = sapply(data[,-1], min),
      "Median" = sapply(data[,-1], median),
      "Max" = sapply(data[,-1], max)
    )), 2)
    colnames(df) <- names(var.names)[2:9]
    #data.frame("Statistic" = row.names(df), df)
    DT::datatable(df, options = list(dom = 't', scrollX = TRUE))
  }) 
  
  output$stddevplot <- renderPlot({
    df <- as.data.frame(sapply(data[,-1], input$statbarchoice))
    colnames(df) <- "value"
    row.names(df) <- names(var.names)[2:9]
    df %>%
      ggplot(aes(x = value, y = reorder(row.names(.), value))) +
      geom_col(fill = "lightseagreen") +
      scale_x_continuous(expand = c(0,0)) +
      theme(axis.title = element_blank(),
            axis.text.y = element_text(size = 12, face = "bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = margin(r = 10))
  }) # ----
  
  # statistical tests ----
  output$st2depUI <- renderUI({
    selectInput("st2dep", label = "Choose dependent variable:",
                choices = var.names[which(!var.names %in% c(input$st2ind, "sex"))])
  })
  
  stdata <- eventReactive(input$stattestgo,{
    if (input$stattestchoice == "st1"){
      data %>% select(sex, eval(input$stattestvar))
    } else {
      data
    }
  })
  
  observeEvent(input$stattestgo, {
    output$st1 <- DT::renderDataTable(isolate({
      if (input$st1c == "mf"){
        x <- stdata() %>% filter(sex == "male") %>% select(2) %>% pull()
        y <- stdata() %>% filter(sex == "female") %>% select(2) %>% pull()
        s1 <- "Male"
        s2 <- "Female"
      } else if (input$st1c == "mi"){
        x <- stdata() %>% filter(sex == "male") %>% select(2) %>% pull()
        y <- stdata() %>% filter(sex == "infant") %>% select(2) %>% pull()
        s1 <- "Male"
        s2 <- "Infant"
      } else {
        x <- stdata() %>% filter(sex == "female") %>% select(2) %>% pull()
        y <- stdata() %>% filter(sex == "infant") %>% select(2) %>% pull()
        s1 <- "Female"
        s2 <- "Infant"
      }
      df <- t.test(x,y) %>% tidy %>% as.data.frame
      df <- cbind(c( "t-Value", "p-Value", paste0("Mean 1 (",s1,")"), paste0("Mean 2 (",s2,")"), 
                     "Mean Diff.", "Mean Diff. 5%", "Mean Diff. 95%"),
                  as.vector(df[,c(4,5,2,3,1,7,8)])) %>% as.data.frame()
      colnames(df) <- c("Statistic", "Value")
      df <- df %>% mutate(Value = as.numeric(Value),
                          across(.cols = 2, signif, 4))
      DT::datatable(df, rownames = FALSE, options = list(dom = 't'))
    }))
    
    output$st2 <- DT::renderDataTable(isolate({
      m <- lm(paste0(input$st2dep, " ~ ", paste(input$st2ind, collapse = " + ")), data = stdata())
      df <- as.data.frame(tidy(m)) %>% mutate(across(.cols = 2:5, signif, 4)) %>% select(1,2,4,5)
      colnames(df) <- c("Model Term", "Estimate", "t-Value", "p-Value")
      rn <- c("Intercept")
      for (i in 2:nrow(df)){
        if (df[i,1] == "sexfemale"){
          rn <- c(rn, "Female")
        } else if (df[i,1] == "sexinfant"){
          rn <- c(rn, "Infant")
        } else {
          rn <- c(rn, names(var.names[var.names == df[i,1]]))
        }
      }
      df[,1] <- rn
      DT::datatable(df, rownames = FALSE, options = list(dom = 't'))
    }))
    
    output$st2plot <- renderPlot(isolate({
      m <- lm(paste0(input$st2dep, " ~ ", paste(input$st2ind, collapse = " + ")), data = stdata())
      m %>% augment %>% 
        ggplot(aes(`.std.resid`)) +
        xlab("Standard Residual") +
        geom_histogram(binwidth = 0.1, fill = "coral") +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        theme(axis.title.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(fill = NA, color = "black"))
    }))
  }) # ----
  
  # data exploration 
  # box plots ----
  output$boxplot <- renderPlot({
    if (input$boxvar == "m"){
      p <- data %>% 
        pivot_longer(cols = 2:4, names_to = "measurement.type", values_to = "measurement.value") %>% 
        ggplot(aes(x = measurement.type, y = measurement.value, color = measurement.type)) +
        geom_boxplot(size = 1) +
        scale_x_discrete(labels = names(var.names)[2:4]) +
        labs(y = "Measurement (mm)") + 
        theme(axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 15),
              axis.title.x = element_blank(), 
              axis.title.y = element_text(size = 20),
              panel.border = element_rect(fill = NA, color = "black"),
              legend.position = 'none')
      if (input$boxdots == TRUE){
        p <- p + geom_point(shape = 19, alpha = 0.1, position = position_jitter(width = 0.1))
      }
    } else if (input$boxvar == "w"){
      p <- data %>% 
        pivot_longer(cols = 5:8, names_to = "measurement.type", values_to = "measurement.value") %>% 
        ggplot(aes(x = measurement.type, y = measurement.value, color = measurement.type)) +
        geom_boxplot(size = 1) +
        scale_x_discrete(labels = names(var.names)[5:8]) +
        labs(y = "Weight (g)") + 
        theme(axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 15),
              axis.title.x = element_blank(), 
              axis.title.y = element_text(size = 20),
              panel.border = element_rect(fill = NA, color = "black"),
              legend.position = 'none')
      if (input$boxdots == TRUE){
        p <- p + geom_point(shape = 19, alpha = 0.1, position = position_jitter(width = 0.1))
      }
    } else {
      p <- data %>%
        ggplot(aes(x = sex, y = age, color = sex)) +
        geom_boxplot(size = 1) +
        scale_x_discrete(labels = c("Male", "Female", "Infant")) +
        labs(y = "Age (years)") + 
        theme(axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 15),
              axis.title.x = element_blank(), 
              axis.title.y = element_text(size = 20),
              panel.border = element_rect(fill = NA, color = "black"),
              legend.position = 'none')
      if (input$boxdots == TRUE){
        p <- p + geom_point(shape = 19, alpha = 0.1, position = position_jitter(width = 0.1))
      }
    }
    p
  }) # ----
  
  # histogram/area plots ----
  output$distplot <- renderPlot({
    if (input$distvar == "m"){
      p <- data %>% 
        pivot_longer(cols = 2:4, names_to = "measurement.type", values_to = "measurement.value") %>% 
        mutate(measurement.type = factor(measurement.type, levels = var.names[2:4])) %>% 
        ggplot(aes(x = measurement.value, fill = measurement.type, color = measurement.type)) +
        labs(x = "Measurement (mm)", y = "Frequency", fill = "Measurement\nType") + 
        scale_fill_discrete(labels = names(var.names)[2:4]) +
        guides(color = "none") +
        theme(axis.text = element_text(size = 15),
              axis.title = element_text(size = 20),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 12),
              panel.border = element_rect(fill = NA, color = "black"))
      if (input$disttype == 'hist'){
        p <- p + geom_histogram(alpha = 1/2, binwidth = 2, position = "identity")
      } else {
        p <- p + geom_area(stat = "bin", alpha = 1/2, binwidth = 2, position = "identity")
      }
      
    } else if (input$distvar == "w"){
      p <- data %>% 
        pivot_longer(cols = 5:8, names_to = "measurement.type", values_to = "measurement.value") %>% 
        mutate(measurement.type = factor(measurement.type, levels = var.names[5:8])) %>% 
        ggplot(aes(x = measurement.value, fill = measurement.type, color = measurement.type)) +
        labs(x = "Weight (g)", y = "Frequency", fill = "Weight Type") + 
        scale_fill_discrete(labels = names(var.names)[5:8]) +
        guides(color = "none") +
        theme(axis.text = element_text(size = 15),
              axis.title = element_text(size = 20),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 12),
              panel.border = element_rect(fill = NA, color = "black"))
      if (input$disttype == 'hist'){
        p <- p + geom_histogram(alpha = 1/3, binwidth = 5, position = "identity")
      } else {
        p <- p + geom_area(stat = "bin", alpha = 1/3, binwidth = 5, position = "identity")
      }
      
    } else {
      p <- data %>% 
        ggplot(aes(x = age, fill = sex, color = sex)) +
        labs(x = "Age (years)", y = "Frequency", fill = "Gender") + 
        scale_fill_discrete(labels = names(var.names)[2:4]) +
        guides(color = "none") +
        theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 20),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 12),
            panel.border = element_rect(fill = NA, color = "black"))
      if (input$disttype == 'hist'){
        p <- p + geom_histogram(alpha = 1/2, binwidth = 1, position = "identity")
      } else {
        p <- p + geom_area(stat = "bin", alpha = 1/2, binwidth = 1, position = "identity")
      }
    }
    p
  }) # ----
  
  # violin plots ----
  output$violinplot <- renderPlot({
    if (input$viovar == "m"){
      p <- data %>% 
        pivot_longer(cols = 2:4, names_to = "measurement.type", values_to = "measurement.value") %>% 
        ggplot(aes(x = measurement.type, y = measurement.value, fill = measurement.type)) +
        geom_violin(size = 1, draw_quantiles = TRUE) +
        scale_x_discrete(labels = names(var.names)[2:4]) +
        labs(y = "Measurement (mm)") + 
        theme(axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 15),
              axis.title.x = element_blank(), 
              axis.title.y = element_text(size = 20),
              panel.border = element_rect(fill = NA, color = "black"),
              legend.position = 'none')
    } else if (input$viovar == "w"){
      p <- data %>% 
        pivot_longer(cols = 5:8, names_to = "measurement.type", values_to = "measurement.value") %>% 
        ggplot(aes(x = measurement.type, y = measurement.value, fill = measurement.type)) +
        geom_violin(size = 1, draw_quantiles = TRUE) +
        scale_x_discrete(labels = names(var.names)[5:8]) +
        labs(y = "Weight (g)") + 
        theme(axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 15),
              axis.title.x = element_blank(), 
              axis.title.y = element_text(size = 20),
              panel.border = element_rect(fill = NA, color = "black"),
              legend.position = 'none')
    } else {
      p <- data %>%
        ggplot(aes(x = sex, y = age, fill = sex)) +
        geom_violin(size = 1, draw_quantiles = TRUE) +
        scale_x_discrete(labels = c("Male", "Female", "Infant")) +
        labs(y = "Age (years)") + 
        theme(axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 15),
              axis.title.x = element_blank(), 
              axis.title.y = element_text(size = 20),
              panel.border = element_rect(fill = NA, color = "black"),
              legend.position = 'none')
    }
    p
  }) # ----
  
  # correlation plot ----
  output$corrplot <- renderPlot({
    data %>% 
      select(-sex) %>%
      cor(method = input$corrmethod) %>% 
      corrplot(method = "color", col.lim = c(0,1), number.cex = 1.5,
               tl.cex = 1.5, addCoef.col = "red", tl.col = 1, cl.cex = 1.5)
  }) # ----
  
  # data table ----
  output$mydata <- DT::renderDataTable({ data })
  # ----
  
  # interactive visualization ----
  output$plot1 <- renderPlotly({
    xvar <- input$xvar
    yvar <- input$yvar
    xvar_name <- names(which(var.names == xvar))
    yvar_name <- names(which(var.names == yvar))
    sexes <- input$sexes
    
    ind2 <- 2
    ind3 <- if_else(xvar_name == yvar_name, 2, 3)
    
    data.in <- data %>% select("sex", eval(xvar), eval(yvar))
    if (colnames(data.in)[ind2] %in% c("length", "height", "diameter")) {
      unitx = " (mm)"
    } else if (colnames(data.in)[ind2] %in% c("whole.weight", "shucked.weight", "viscera.weight", "shell.weight")){
      unitx = " (g)"
    } else {
      unitx = " (years)"
    }
    if (colnames(data.in)[ind3] %in% c("length", "height", "diameter")) {
      unity = " (mm)"
    } else if (colnames(data.in)[ind3] %in% c("whole.weight", "shucked.weight", "viscera.weight", "shell.weight")){
      unity = " (g)"
    } else {
      unity = " (years)"
    }
    #colnames(data.in)[2] <- names(which(var.names == xvar))
    
    xlims <- c(min(data.in[,ind2]),max(data.in[,ind2]))
    ylims <- c(min(data.in[,ind3]),max(data.in[,ind3]))
    clims <- hue_pal()(3)
    
    shape <- if_else(input$matchfill, 19, 21)
    
    if (length(sexes) != 0) {
      if (!"male" %in% sexes) {
        data.in <- data.in %>% filter(!sex == "male")
      }
      if (!"female" %in% sexes) {
        data.in <- data.in %>% filter(!sex == "female")
      }
      if (!"infant" %in% sexes) {
        data.in <- data.in %>% filter(!sex == "infant")
      }
      genchk <- " + Gender"
      p1 <- data.in %>% 
        ggplot(aes(x = .[,ind2], y = .[,ind3], color = sex,
                   text = paste0(
                     xvar_name, ": ", .[,ind2], " ", unitx,
                     "\n", yvar_name, ": ", .[,ind3], " ", unity,
                     "\nSex:", sex
                   ))) +
        geom_point(shape = shape, size = input$ptsize, alpha = input$alpha, fill = "white", 
                   position = position_jitter(width = input$jitchk, seed = 1))
    } else {
      p1 <- data.in %>% 
        ggplot(aes(x = .[,ind2], y = .[,ind3],
                   text = paste0(
                     xvar_name, ": ", .[,ind2], " ", unitx,
                     "\n", yvar_name, ": ", .[,ind3], " ", unity
                   ))) +
        geom_point(shape = shape, size = input$ptsize, alpha = input$alpha, fill = "white", color = "gray40", 
                   position = position_jitter(width = input$jitchk, seed = 1))
      genchk <- ""
    }
    
    p1 <- p1 + 
      labs(x = paste0(xvar_name, unitx), y = paste0(yvar_name, unity), color = "Sex", 
           title = paste0("Abalone ", yvar_name, " ~ ", xvar_name, genchk),
           subtitle = paste0("For n = ", nrow(data), " observations")) +
      scale_color_manual(values = list("male"   = clims[3],
                                       "female" = clims[1],
                                       "infant" = clims[2]), drop = FALSE) +
      scale_x_continuous(limits = xlims) +
      scale_y_continuous(limits = ylims) +
      theme(plot.title = element_text(size = 18),
            axis.text = element_text(size = 13), 
            axis.title = element_text(size = 15),
            panel.border = element_rect(fill = NA, color = "black"))
    ggplotly(p1, tooltip = "text")
  }) # ----
}

#### Run the application ####
shinyApp(ui, server)
