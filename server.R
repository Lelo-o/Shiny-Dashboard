server <- function(input, output) {
  
  data_long <- data %>%
    select(ProgLang, Databases) %>%
    pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>%
    separate_rows(Value, sep = ";") %>%
    drop_na(Value)
  
  # Top Programming Languages
  output$prog_lang_plot <- renderPlotly({
    lang_counts <- data_long %>%
      filter(Category == "ProgLang") %>% # Filter to only programming languages
      count(Value, sort = TRUE) %>%
      head(10)
    
    ggplot(lang_counts, aes(x = reorder(Value, n), y = n, fill = Value)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top Programming Languages Used by Graduates", x = "Language", y = "Count") +
      theme_minimal()
  })
  
  # Top Databases
  output$database_plot <- renderPlotly({
    db_counts <- data_long %>%
      filter(Category == "Databases") %>%
      count(Value, sort = TRUE) %>%
      head(10)
    
    ggplot(db_counts, aes(x = reorder(Value, n), y = n, fill = Value)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top Databases Used by Graduates", x = "Database", y = "Count") +
      theme_minimal()
  })
}
# Run the Shiny app
shinyApp(ui = ui, server = server)
