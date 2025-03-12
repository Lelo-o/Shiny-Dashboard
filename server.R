library(shiny)
library(tidyverse)
library(ggplot2)
library(forcats)
library(stringr)


survey_data <- read_csv("C:\\Users\\Mpumelelo Mthimunye\\Downloads\\ITRDA assignment\\Shiny-Dashboard\\cleaned_survey_data.csv")

# Function to split and count tools
count_tools <- function(data, column_name) {
  data %>%
    separate_rows(!!sym(column_name), sep = ",\\s*") %>%
    mutate(!!sym(column_name) := str_trim(!!sym(column_name))) %>%
    count(!!sym(column_name), sort = TRUE)
}

server <- function(input, output) {

  # Reactive data for top tools
  top_tools_data <- reactive({
    tool_column <- switch(input$toolType,
                              "Programming Languages" = "ProgLang",
                              "Databases" = "Databases",
                              "Web Frameworks" = "WebFramework",
                              "AI Search Tools" = "AISearch",
                              "AI Developer Tools" = "AITool")
    count_tools(survey_data, tool_column) %>% head(10)
  })

  # Reactive data for industries by study field
  industries_data <- reactive({
    survey_data %>%
      filter(StudyField == input$studyField) %>%
      separate_rows(Industry, sep = ",\\s*") %>%
      group_by(StudyField, Industry) %>%
      summarise(count = n(), .groups = "drop") %>%
      top_n(5, count) %>%
      ungroup()
  })

  # Reactive data for job roles by study field
  roles_data <- reactive({
    survey_data %>%
      filter(StudyField == input$studyField) %>%
      group_by(StudyField, Role) %>%
      summarise(count = n(), .groups = "drop") %>%
      top_n(5, count) %>%
      ungroup()
  })

  # Reactive data for employment rate
  employment_data <- reactive({
    survey_data %>%
      group_by(StudyField, Employment) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(StudyField) %>%
      mutate(total = sum(count), percentage = count / total * 100) %>%
      filter(str_detect(Employment, "Employed")) %>%
      ungroup()
  })

  # Plotting functions
  output$topToolsPlot <- renderPlot({
    ggplot(top_tools_data(), aes(x = fct_reorder(!!sym(names(top_tools_data())[1]), n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = paste("Top 10", input$toolType), x = "", y = "Count") +
      theme_minimal()
  })

  output$industriesPlot <- renderPlot({
    ggplot(industries_data(), aes(x = fct_reorder(Industry, count), y = count, fill = StudyField)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(title = "Top Industries by Study Field", x = "Industry", y = "Count") +
      theme_minimal()
  })

  output$rolesPlot <- renderPlot({
    ggplot(roles_data(), aes(x = fct_reorder(Role, count), y = count, fill = StudyField)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(title = "Top Job Roles by Study Field", x = "Job Role", y = "Count") +
      theme_minimal()
  })

  output$employmentPlot <- renderPlot({
    ggplot(employment_data(), aes(x = StudyField, y = percentage, fill = StudyField)) +
      geom_bar(stat = "identity") +
      labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Percentage Employed") +
      theme_minimal() +
      scale_y_continuous(labels = function(x) paste0(x, "%"))
  })
}

shinyApp(ui = ui, server = server)
