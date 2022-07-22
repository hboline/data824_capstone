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

# define user-interface ----
ui <- dashboardPage(skin = "green",
  
  # header
  dashboardHeader(title = "DATA 824 Capstone"),
  
  # sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualizations", tabName = "visualizations", icon = icon("search", lib = "glyphicon")),
      menuItem("Data", tabName = "datatable", icon = icon("table"))
    )
  ),
  
  # body
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      
      # visualizations tab
      tabItem(tabName = "visualizations",
        fluidRow(
          column(width = 3,
                 
            box(width = 10, title = "Variable Options",
              selectInput(inputId = "xvar", label = "Select x-variable:", choices = var.names[2:8]),
              checkboxGroupInput(inputId = "sexes", label = "Select genders:", 
                                 choiceNames = c("Male", "Female", "Infant"),
                                 choiceValues = c("male", "female", "infant"),
                                 selected = c("male", "female", "infant"))
            ),
            box(width = 10, title = "Plot Options",
              sliderInput(inputId = "alpha", label = "Point opacity:",
                          min = 0.0, max = 1.0, step = 0.1, value = 1.0),
              checkboxInput(inputId = "jitchk", label = "Jitter points?", value = TRUE)
            )
          ),
          
          column(width = 7,
            box(width = 40,
              plotlyOutput("plot1", height = "600px")
            )
          )
        )
      ),
      
      tabItem(tabName = "datatable",
        fluidRow(
          dataTableOutput("data")
        )
      )
      
    )
  )
)


# define server function ----
server <- function(input, output, session) {
  
  output$data <- renderTable({ data })
  
  output$plot1 <- renderPlotly({
    xvar <- input$xvar
    sexes <- input$sexes
    data.in <- data %>% select("sex", eval(xvar), "age")
    if (colnames(data.in)[2] %in% c("length", "height", "diameter")) {
      unit = " (mm)"
    } else {
      unit = " (g)"
    }
    #colnames(data.in)[2] <- names(which(var.names == xvar))
    
    xlims <- c(min(data.in[,2]),max(data.in[,2]))
    ylims <- c(min(data.in[,3]),max(data.in[,3]))
    clims <- hue_pal()(3)
    
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
        ggplot(aes(x = .[,2], y = age, color = sex,
                   text = paste0(
                     names(which(var.names == xvar)), ": ", .[,2], " ", unit,
                     "\nAge: ", age, " years",
                     "\nSex:", sex
                   ))) +
        geom_point(shape = 21, size = 2, alpha = input$alpha, fill = "white", 
                   position = position_jitter(width = input$jitchk, seed = 1))
    } else {
      p1 <- data.in %>% 
        ggplot(aes(x = .[,2], y = age,
                   text = paste0(
                     names(which(var.names == xvar)), ": ", .[,2], " ", unit,
                     "\nAge: ", age, " years"
                   ))) +
        geom_point(shape = 21, size = 2, alpha = input$alpha, fill = "white", color = "gray40", 
                   position = position_jitter(width = input$jitchk, seed = 1))
      genchk <- ""
    }
    
    p1 <- p1 + 
      labs(x = paste0(names(which(var.names == xvar)), unit), y = "Age (years)", color = "Sex", 
           title = paste0("Abalone Age ~ ", names(which(var.names == xvar)), genchk),
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
  })
}

#### Run the application ####
shinyApp(ui, server)
