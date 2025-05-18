arrow_plot_func <- function(input, output, session, data_func, v, output_id) {
# Setup Variables (Change when Needed:)
  NP <- 100
  colors <- c("purple", "blue", "green", "gray", "red", "pink")
  
  filtered_data <- reactive({
    data_stuff <- data_func() %>%
      filter(country %in% input$country_selection)
    data_stuff <- data_stuff %>% 
      filter(year == v$arrow_min_year | year == v$arrow_max_year)
    selected_labels <- input$arrow_checkbox
    mydat <- NULL
    
    if (nrow(data_stuff) >= 2){
      for (i in seq_along(input$country_selection)){
        country <- input$country_selection[i]
        mydat[[paste(country, "ed")]] = if ("Electoral Democracy" %in% selected_labels) {
          seq(data_stuff$electoral_democracy[data_stuff$year == v$arrow_min_year][i],
              data_stuff$electoral_democracy[data_stuff$year == v$arrow_max_year][i],
              len = NP)
        } else { NA }
        mydat[[paste(country, "ld")]] = if ("Liberal Democracy" %in% selected_labels) {
          seq(data_stuff$liberal_democracy[data_stuff$year == v$arrow_min_year][i],
              data_stuff$liberal_democracy[data_stuff$year == v$arrow_max_year][i],
              len = NP)
        } else { NA }
        mydat[[paste(country, "pd")]] = if ("Participatory Democracy" %in% selected_labels) {
          seq(data_stuff$participatory_democracy[data_stuff$year == v$arrow_min_year][i],
              data_stuff$participatory_democracy[data_stuff$year == v$arrow_max_year][i],
              len = NP)
        } else { NA }
        mydat[[paste(country, "dd")]] = if ("Deliberative Democracy" %in% selected_labels) {
          seq(data_stuff$deliberative_democracy[data_stuff$year == v$arrow_min_year][i],
              data_stuff$deliberative_democracy[data_stuff$year == v$arrow_max_year][i],
              len = NP)
        } else { NA }
        mydat[[paste(country, "egd")]] = if ("Egalitarian Democracy" %in% selected_labels) {
          seq(data_stuff$egalitarian_democracy[data_stuff$year == v$arrow_min_year][i],
              data_stuff$egalitarian_democracy[data_stuff$year == v$arrow_max_year][i],
              len = NP)
        } else { NA }
        mydat[[paste(country, "fe")]] = if ("Fair Elections" %in% selected_labels) {
          seq(data_stuff$fair_elections[data_stuff$year == v$arrow_min_year][i],
              data_stuff$fair_elections[data_stuff$year == v$arrow_max_year][i],
              len = NP)
        } else { NA }
        
      }
      mydat <- as.data.frame(mydat)
      mydat <- mydat %>% select(where(~ !any(is.na(.))))
    }
    return(mydat)
  })
  
  output[[output_id]] <- renderPlotly({
    if (length(input$arrow_checkbox) != 0 && length(input$country_selection) != 0){
      req(filtered_data())
      data <- filtered_data()
      country_list <- paste(input$country_selection, collapse = ", ")
      num_countries <- length(input$country_selection)
      # Create the Initial Plot
      fig <- plot_ly(data, type = 'scatter', mode = 'markers')
      # Run Through all the Criteria we're looking for
      for (i in 1:ncol(data)) {
        #Logic for making sure that everything is displayed properly for two countries
          if (num_countries != 1){
            text_color_index = if (i %% length(input$arrow_checkbox) == 0) length(input$arrow_checkbox) else i %% length(input$arrow_checkbox)
        } else {
          text_color_index = i
        }
        # Last Value refers to the value at the maximum year
        last_val = tail(data[[i]], 1)
        # First Value refers to the value at the minimum year
        first_val = head(data[[i]], 1)
        # Diff takes the difference between the min and the maximum year
        diff = round(last_val - first_val, 3)
        fig <- fig %>%
          # Create the Arrow corresponding to the criteria:
          add_annotations(x = last_val,
                          y = 1-i,
                          xref = "x",
                          yref = "y",
                          ax = first_val,
                          ay = 1-i,
                          axref = "x",
                          ayref = "y",
                          text = "",
                          showarrow = TRUE,
                          arrowhead = 1,
                          arrowwidth = if (abs(diff) <= 0.006) 10 else 6,
                          arrowsize = 2/3,
                          arrowcolor = I(colors[text_color_index])
                          ) %>%
          #Creates the Text so that you know what is what
          add_annotations(x = (last_val + first_val)/ 2,
                          y = (1-i) + 0.5,
                          xref = "x",
                          yref = "y",
                          text = input$arrow_checkbox[text_color_index],
                          showarrow = FALSE
                          ) %>%
          # Creates an invisible trace along the arrow to show hover info
          # stupid plotly does not have hover over whole arrow available yet >_>
          add_trace(x = data[[i]], y = 1-i, mode = "lines", hoverinfo = "text",
                    text = if (diff > 0) paste0("+", diff) else diff, name = input$arrow_checkbox[text_color_index],
                    showlegend = FALSE, line = list(color = "rgba(0,0,0,0)"))
      }
      
      
      fig <- fig %>% layout(
        legend = list(
          itemclick = FALSE,
          itemdoubleclick = FALSE
        ),
        title = paste0("Democracy in ", country_list, " from ", 
                       v$arrow_min_year, " to ",
                       v$arrow_max_year),
        xaxis = list(title = "Democracy Index",
                     zeroline = FALSE,
                     range = c(0,1)),
        # I didn't wanna see any y axis or y axis labeling happening, decluttering it
        yaxis = list(
          showticklabels = FALSE,
          zeroline = FALSE
        )
      )
    }

  })
  
}
