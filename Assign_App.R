# Load libraries
library(tidyverse)
library(stringr)
library(janitor)
library(forcats)
library(readr)

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Load the dataset
setwd("C:/Users/Mpumelelo Mthimunye/Documents/Assignment")
data <- read_csv("graduate_survey.csv")

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
write_csv(data,"cleaned_survey_data.csv")

# Display summary of cleaned data
summary(data)


##Question 2: 

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


#Question 3: 
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(readr)


# Load and clean the data (replicate cleaning from script)
data <- read_csv("graduate_survey.csv")

relevant_columns <- c("ProgLang", "Databases", "WebFramework", "Platform", "ToolsTechHaveWorkedWith")
data <- data %>% select(all_of(relevant_columns))

data <- data %>% mutate(across(where(is.character), ~ replace_na(.x, "Unknown")))

# Function to count tools
count_tools <- function(data, column) {
  data %>%
    separate_rows(!!sym(column), sep = ";") %>%
    count(!!sym(column), sort = TRUE) %>%
    filter(!!sym(column) != "Unknown" & !is.na(!!sym(column)))
}

ui <- dashboardPage(
  dashboardHeader(title = "Eduvos Graduate Dashboard Tech Trends"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Programming Languages", tabName = "prog_lang", icon = icon("code")),
      menuItem("Databases", tabName = "databases", icon = icon("database")),
      menuItem("Web Frameworks", tabName = "web_frameworks", icon = icon("globe")),
      menuItem("Platforms", tabName = "platforms", icon = icon("cloud")),
      menuItem("Tech Tools", tabName = "tech_tools", icon = icon("wrench"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "prog_lang",
              fluidRow(
                box(title = "Top Programming Languages", status = "info", solidHeader = TRUE,
                    plotlyOutput("prog_lang_plot"))
              )),
      tabItem(tabName = "databases",
              fluidRow(
                box(title = "Popular Databases", status = "info", solidHeader = TRUE,
                    plotlyOutput("database_plot"))
              )),
      tabItem(tabName = "web_frameworks",
              fluidRow(
                box(title = "Web Frameworks", status = "info", solidHeader = TRUE,
                    plotlyOutput("webframework_plot"))
              )),
      tabItem(tabName = "platforms",
              fluidRow(
                box(title = "Platforms", status = "info", solidHeader = TRUE,
                    plotlyOutput("platform_plot"))
              )),
      tabItem(tabName = "tech_tools",
              fluidRow(
                box(title = "Tech Tools", status = "info", solidHeader = TRUE,
                    plotlyOutput("techtools_plot"))
              ))
    )
  )
)


server <- function(input, output) {
  # Programming Languages
  output$prog_lang_plot <- renderPlotly({
    prog_lang_counts <- count_tools(data, "ProgLang") %>% head(10)
    ggplotly(ggplot(prog_lang_counts, aes(x = reorder(ProgLang, n), y = n, fill = ProgLang)) +
               geom_bar(stat = "identity") +
               coord_flip() +
               labs(title = "Top Programming Languages", x = "Language", y = "Count") +
               theme_minimal())
  })
  
  # Databases
  output$database_plot <- renderPlotly({
    database_counts <- count_tools(data, "Databases") %>% head(10)
    ggplotly(ggplot(database_counts, aes(x = reorder(Databases, n), y = n, fill = Databases)) +
               geom_bar(stat = "identity") +
               coord_flip() +
               labs(title = "Top Databases", x = "Database", y = "Count") +
               theme_minimal())
  })
  
  # Web Frameworks
  output$webframework_plot <- renderPlotly({
    webframework_counts <- count_tools(data, "WebFramework") %>% head(10)
    ggplotly(ggplot(webframework_counts, aes(x = reorder(WebFramework, n), y = n, fill = WebFramework)) +
               geom_bar(stat = "identity") +
               coord_flip() +
               labs(title = "Top Web Frameworks", x = "Framework", y = "Count") +
               theme_minimal())
  })
  
  # Platforms
  output$platform_plot <- renderPlotly({
    platform_counts <- count_tools(data, "Platform") %>% head(10)
    ggplotly(ggplot(platform_counts, aes(x = reorder(Platform, n), y = n, fill = Platform)) +
               geom_bar(stat = "identity") +
               coord_flip() +
               labs(title = "Top Platforms", x = "Platform", y = "Count") +
               theme_minimal())
  })
  
  # Tech Tools
  output$techtools_plot <- renderPlotly({
    techtools_counts <- count_tools(data, "ToolsTechHaveWorkedWith") %>% head(10)
    ggplotly(ggplot(techtools_counts, aes(x = reorder(ToolsTechHaveWorkedWith, n), y = n, fill = ToolsTechHaveWorkedWith)) +
               geom_bar(stat = "identity") +
               coord_flip() +
               labs(title = "Top Tech Tools", x = "Tool", y = "Count") +
               theme_minimal())
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)

install.packages("rsconnect")
library(rsconnect)

# Set your working directory to the directory containing your app files
setwd("C:/Users/Mpumelelo Mthimunye/Documents/Assignment")
rsconnect::setAccountInfo(name='mpumelelo',
                          token='AE4110527557E31FFBF1C23E94FA732D',
                          secret='lA/n97GwPLWjqtI/OSfoy9S9EVzDDKcfrDj0Qxhx')
# Deploy the app
rsconnect::deployApp()

