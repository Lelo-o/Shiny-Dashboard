library(shiny)

ui <- fluidPage(
  titlePanel("Eduvos IT Graduates Survey Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("toolType", "Select Tool Type:",
                  choices = c("Programming Languages", "Databases", "Web Frameworks", "AI Search Tools", "AI Developer Tools")),
      selectInput("studyField", "Select Study Field:",
                  choices = c("Information Technology", "CS", "DS")),
      selectInput("industry", "Select Industry:",
                  choices = unique(survey_data$Industry)),
      selectInput("role", "Select Job Role:",
                  choices = unique(survey_data$Role))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Top Tools", plotOutput("topToolsPlot")),
        tabPanel("Industries by Study Field", plotOutput("industriesPlot")),
        tabPanel("Job Roles by Study Field", plotOutput("rolesPlot")),
        tabPanel("Employment Rate", plotOutput("employmentPlot"))
      )
    )
  )
)