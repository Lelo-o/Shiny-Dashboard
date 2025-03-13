# Load libraries
library(tidyverse)
library(stringr)
library(janitor)
library(forcats)
library(readr)

# Load the dataset
survey_data <- read_csv("C:\\Users\\Mpumelelo Mthimunye\\Downloads\\graduate_survey.csv") 

# Select relevant columns
relevant_columns <- c("Campus", "StudyField", "Branch", "Role", "EduLevel", "ProgLang", 
                      "Databases", "Platform", "WebFramework", "Industry", "AISearch", "AITool", "Employment")
data <- data %>% select(all_of(relevant_columns))

# Standardize categorical values
data$Campus <- recode(data$Campus,
                      "Durban" = "Durban",
                      "Umhlanga" = "Durban",
                      .default = as.character(data$Campus))

# Handle missing values (e.g., replace NAs with "Unknown" for categorical variables)
data <- data %>% mutate(across(where(is.character), ~ replace_na(.x, "Unknown")))

# Get top 5 campuses with most responses
top_campuses <- data %>% count(Campus, sort = TRUE) %>% top_n(5, n) %>% pull(Campus)
data <- data %>% filter(Campus %in% top_campuses)


# Save the cleaned dataset
write_csv(survey_subset, "C:/Users/Mpumelelo Mthimunye/Downloads/cleaned_survey_data.csv")

# Display summary of cleaned data
summary(data)


##QUESTION 2:

library(tidyverse)
library(readr)
library(janitor)

# Load dataset
data <- read_csv("C:\\Users\\Mpumelelo Mthimunye\\Downloads\\graduate_survey (1).csv") 


# Select relevant columns
relevant_columns <- c("Campus", "StudyField", "Branch", "Role", "EduLevel", "ProgLang", 
                      "Databases", "Platform", "WebFramework", "Industry", "AISearch", "AITool", "Employment")
data <- data %>% select(all_of(relevant_columns))

# Standardize categorical values
data$Campus <- recode(data$Campus,
                      "Durban" = "Durban",
                      "Umhlanga" = "Durban",
                      .default = as.character(data$Campus))

# Handle missing values (e.g., replace NAs with "Unknown" for categorical variables)
data <- data %>% mutate(across(where(is.character), ~ replace_na(.x, "Unknown")))

# Get top 5 campuses with most responses
top_campuses <- data %>% count(Campus, sort = TRUE) %>% top_n(5, n) %>% pull(Campus)
data <- data %>% filter(Campus %in% top_campuses)

# Save cleaned data
write_csv(survey_subset, "C:/Users/Mpumelelo Mthimunye/Downloads/cleaned_survey_data.csv")


# Display summary of cleaned data
summary(data)

# Visualization
library(ggplot2)
library(tidyr)

# Function to count occurrences of semicolon-separated values
count_tools <- function(column) {
  data %>%
    separate_rows({{ column }}, sep = ";") %>%
    count({{ column }}, sort = TRUE) %>%
    filter(!is.na({{ column }}))
}

# Top tools used by graduates
prog_lang_counts <- count_tools(ProgLang)
database_counts <- count_tools(Databases)
platform_counts <- count_tools(Platform)
webframework_counts <- count_tools(WebFramework)
ai_search_counts <- count_tools(AISearch)
ai_tool_counts <- count_tools(AITool)

# Plot top programming languages
ggplot(prog_lang_counts[1:10,], aes(x = reorder(ProgLang, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top Programming Languages Used by Graduates", x = "Programming Language", y = "Count")

# Most popular industries graduates go into
data %>%
  separate_rows(Industry, sep = ";") %>%
  count(StudyField, Industry, sort = TRUE) %>%
  ggplot(aes(x = reorder(Industry, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Most Popular Industries by Study Field", x = "Industry", y = "Count")

# Top job roles by study field
data %>%
  count(StudyField, Role, sort = TRUE) %>%
  ggplot(aes(x = reorder(Role, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Top Job Roles by Study Field", x = "Job Role", y = "Count")

# Employment rate by study field
data %>%
  count(StudyField, Employment) %>%
  mutate(employment_rate = n / sum(n) * 100) %>%
  ggplot(aes(x = StudyField, y = employment_rate, fill = Employment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Employment Rate (%)")


#q3
install.packages("rsconnect")
library(rsconnect)
rsconnect::setAccountInfo(name='mpumelelo',
                          token='AE4110527557E31FFBF1C23E94FA732D',
                          secret='lA/n97GwPLWjqtI/OSfoy9S9EVzDDKcfrDj0Qxhx')
setwd("C:/Users/Mpumelelo Mthimunye/Downloads/ITRDA assignment/Shiny-Dashboard")

rsconnect::deployApp()




#Question 3: 

install.packages(c("tidyverse", "shiny", "shinydashboard", "plotly", "DT"))
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)



data <- read_csv("C:\\Users\\Mpumelelo Mthimunye\\Downloads\\ITRDA assignment\\cleaned_survey_data.csv")

data_long <- data %>%
  select(ProgLang, Databases, Platform, WebFramework, AISearch, AITool) %>%
  mutate(across(everything(), ~ strsplit(as.character(.), ";"))) %>%
  unnest(cols = everything()) %>%
  drop_na()


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


server <- function(input, output) {
  
  # Top Programming Languages
  output$prog_lang_plot <- renderPlotly({
    lang_counts <- data_long %>%
      count(ProgLang, sort = TRUE) %>%
      head(10)  # Top 10 languages
    
    ggplot(lang_counts, aes(x = reorder(ProgLang, n), y = n, fill = ProgLang)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top Programming Languages Used by Graduates", x = "Language", y = "Count") +
      theme_minimal()
  })
  
  # Top Databases
  output$database_plot <- renderPlotly({
    db_counts <- data_long %>%
      count(Databases, sort = TRUE) %>%
      head(10)
    
    ggplot(db_counts, aes(x = reorder(Databases, n), y = n, fill = Databases)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top Databases Used by Graduates", x = "Database", y = "Count") +
      theme_minimal()
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)

install.packages("rsconnect")


rsconnect::setAccountInfo(name='mpumelelo',
                          token='AE4110527557E31FFBF1C23E94FA732D',
                          secret='lA/n97GwPLWjqtI/OSfoy9S9EVzDDKcfrDj0Qxhx')

rsconnect::deployApp()
exists("data_long")
