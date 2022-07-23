# DATA 824 Summer 2022
# Capstone Project
# Hayden D. Boline
# Created 18 July 2022

# load libraries ----
library(shiny)
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
      menuItem("Data Exploration", tabName = "dataexploration", icon = icon("search", lib = "glyphicon")),
      menuItem("About", tabName = "about", icon = icon("question-sign", lib = "glyphicon")),
      menuItem("Interactive Visualization", tabName = "visualizations", icon = icon("hand-up", lib = "glyphicon")),
      menuItem("Data", tabName = "datatable", icon = icon("table"))
    )
  ),
  
  # body
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      
      # data exploration ----
      tabItem(tabName = "dataexploration",
        fluidPage(
          h1("Data Exploration & Visualization"),
          h4("Box, Violin, Frequency, and Correlation Plots"),
          br(),
          # selection and options ----
          column(width = 3,
            # main selection ----
            box(width = 12, title = "Select Visualization:",
              selectInput(inputId = "viztype", label = NULL, 
                          choices = c("Box Plots" = "box", "Frequency Plots" = "dist", 
                                      "Violin Plots" = "violin", "Correlation Plot" = "corr"),
                          selected = "corr")
            ), # ----
            
            # histogram/area plot options ----
            conditionalPanel(
              condition = "input.viztype == 'dist'",
              box(width = 12, title = "Plot Options:",
                selectInput(inputId = "distvar", label = "Select measurement type:",
                            choices = c("Lengths (mm)" = "m", "Weights (g)" = "w", "Age by Gender" = "a")),
                radioButtons(inputId = "disttype", label = "Selection distribution type:",
                             choices = c("Histogram" = "hist", "Area Plot" = "area"))
              )
            ), # ----
            
            # violin plot options 
            conditionalPanel(
              condition = "input.viztype == 'violin'",
              box(width = 12, title = "Plot Options:",
                  selectInput(inputId = "viovar", label = "Select measurement type:",
                              choices = c("Lengths (mm)" = "m", "Weights (g)" = "w", "Age by Gender" = "a"))
              )
            ),
            
            # box plot options ----
            conditionalPanel(
              condition = "input.viztype == 'box'",
              box(width = 12, title = "Plot Options:",
                selectInput(inputId = "boxvar", label = "Select measurement type:",
                            choices = c("Lengths (mm)" = "m", "Weights (g)" = "w", "Age by Gender" = "a")),
                checkboxInput(inputId = "boxdots", label = "Plot data points?", value = FALSE)
              )
            ),
            
            # correlation plot options ----
            conditionalPanel(
              condition = "input.viztype == 'corr'",
              box(width = 12, title = "Plot Options:",
                  radioButtons(inputId = "corrmethod", label = "Select correlation method:",
                               choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"),
                               selected = "pearson")
              )
            )
          ), # ----
          
          # plots ----
          column(width = 9,
                 
            # boxplot ----
            conditionalPanel(
              condition = "input.viztype == 'box'",
              box(width = 12,
                plotOutput(outputId = "boxplot", height = "700px")
              )
            ), # ----
            
            # violin plot ----
            conditionalPanel(
              condition = "input.viztype == 'violin'",
              box(width = 12,
                  plotOutput(outputId = "violinplot", height = "700px")
              )
            ), # ----
            
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
          ) # ----
        )
      ), # ----
      
      # data table ----
      tabItem(tabName = "datatable",
        DT::dataTableOutput(outputId = 'mydata')
      ), # ----
      
      # about page ----
      tabItem(tabName = "about",
        h1("HELLO WORLD"),
        p("lorem ipsum")
      ), # ----
      
      # interactive visualization tab ----
      tabItem(tabName = "visualizations",
        fluidRow(
          column(width = 3,
                 
            box(width = 10, title = "Variable Options",
              selectInput(inputId = "xvar", label = "Select x-variable:", 
                          choices = append(var.names[9], var.names[2:8]), selected = "height"),
              selectInput(inputId = "yvar", label = "Select y-variable:", 
                          choices = append(var.names[9], var.names[2:8]), selected = "age"),
              checkboxGroupInput(inputId = "sexes", label = "Select genders:", 
                                 choiceNames = c("Male", "Female", "Infant"),
                                 choiceValues = c("male", "female", "infant"),
                                 selected = c("male", "female", "infant"))
            ),
            box(width = 10, title = "Plot Options",
              sliderInput(inputId = "alpha", label = "Point opacity:",
                          min = 0.0, max = 1.0, step = 0.1, value = 1.0),
              checkboxInput(inputId = "jitchk", label = "Jitter points?", value = TRUE),
              checkboxInput(inputId = "matchfill", label = "Match fill color?", value = FALSE)
            )
          ),
          
          column(width = 7,
            box(width = 40,
              plotlyOutput("plot1", height = "600px")
            )
          )
        )
      ) # ----
      
    )
  )
)


# define server function 
server <- function(input, output, session) {
  
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
  
  # correlation plot
  output$corrplot <- renderPlot({
    data %>% 
      select(-sex) %>%
      cor(method = input$corrmethod) %>% 
      corrplot(method = "color", col.lim = c(0,1), number.cex = 1.5,
               tl.cex = 1.5, addCoef.col = "red", tl.col = 1, cl.cex = 1.5)
  })
  
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
        geom_point(shape = shape, size = 2, alpha = input$alpha, fill = "white", 
                   position = position_jitter(width = input$jitchk, seed = 1))
    } else {
      p1 <- data.in %>% 
        ggplot(aes(x = .[,ind2], y = .[,ind3],
                   text = paste0(
                     xvar_name, ": ", .[,ind2], " ", unitx,
                     "\n", yvar_name, ": ", .[,ind3], " ", unity
                   ))) +
        geom_point(shape = shape, size = 2, alpha = input$alpha, fill = "white", color = "gray40", 
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
