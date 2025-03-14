install.packages(c("tidyverse", "shiny", "shinydashboard", "plotly", "DT"))
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)



data <- read_csv("C:\\Users\\Mpumelelo Mthimunye\\Documents\\Assignment\\graduate_survey.csv")

data <- data %>%
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%  # Convert empty strings to NA only for character columns
  drop_na()  # Remove rows with any missing values


ui <- dashboardPage(
  dashboardHeader(title = "Eduvos IT Graduate Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Programming Languages", tabName = "prog_lang", icon = icon("code")),
      menuItem("Databases", tabName = "databases", icon = icon("database")),
      menuItem("Web Frameworks", tabName = "web_frameworks", icon = icon("globe")),
      menuItem("Platforms", tabName = "platforms", icon = icon("cloud")),
      menuItem("Industries", tabName = "industries", icon = icon("building")),
      menuItem("Job Roles", tabName = "job_roles", icon = icon("briefcase")),
      menuItem("Employment", tabName = "employment", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "About This Dashboard", status = "primary", solidHeader = TRUE, width = 12,
                    "This dashboard provides insights into the most commonly used technologies by Eduvos IT graduates.")
              )),
      tabItem(tabName = "prog_lang",
              fluidRow(
                box(title = "Top Programming Languages", status = "info", solidHeader = TRUE, 
                    plotlyOutput("prog_lang_plot"))
              )),
      tabItem(tabName = "databases",
              fluidRow(
                box(title = "Popular Databases", status = "info", solidHeader = TRUE, 
                    plotlyOutput("database_plot"))
              ))
    )
  )
)
