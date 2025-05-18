democracy_plot_func <- function(input, output, session, data_func, v, output_id) {
# Setup Variables (Change when Needed:)
  NP <- 100
  colors <- c("purple", "blue", "green", "gray", "red", "pink")
  labels <- c("Electoral Democracy", "Liberal Democracy", "Participatory Democracy", "Deliberative Democracy", "Egalitarian Democracy", "Fair Elections")
  yvals <- c(1,0,-1,-2,-3, -4)
  
  filtered_data <- reactive({
    data_stuff <- data_func() %>%
      filter(country %in% input$country_selection)
    data_stuff <- data_stuff %>% 
      filter(year == v$arrow_min_year | year == v$arrow_max_year)
    mydat <- NULL
    if (nrow(data_stuff) == 2){
     mydat <- data.frame(
       ed = seq(data_stuff$electoral_democracy[data_stuff$year == v$arrow_min_year],
                data_stuff$electoral_democracy[data_stuff$year == v$arrow_max_year],
                len = NP), 
       ld = seq(data_stuff$liberal_democracy[data_stuff$year == v$arrow_min_year],
                data_stuff$liberal_democracy[data_stuff$year == v$arrow_max_year],
                len = NP), 
       pd = seq(data_stuff$participatory_democracy[data_stuff$year == v$arrow_min_year],
                data_stuff$participatory_democracy[data_stuff$year == v$arrow_max_year],
                len = NP), 
       dd = seq(data_stuff$deliberative_democracy[data_stuff$year == v$arrow_min_year],
                data_stuff$deliberative_democracy[data_stuff$year == v$arrow_max_year],
                len = NP), 
       egd = seq(data_stuff$egalitarian_democracy[data_stuff$year == v$arrow_min_year],
                 data_stuff$egalitarian_democracy[data_stuff$year == v$arrow_max_year],
                 len = NP), 
       fe = seq(data_stuff$fair_elections[data_stuff$year == v$arrow_min_year],
                data_stuff$fair_elections[data_stuff$year == v$arrow_max_year],
                len = NP)
     )
    }

    return(mydat)
  })
  

  
  output[[output_id]] <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()
  # Create the Initial Plot
    fig <- plot_ly(data, type = 'scatter', mode = 'markers')
  # Run Through all the Criteria we're looking for
    for (i in 1:6) {
    # Last Value refers to the value at the maximum year
      last_val = tail(data[[i]], 1)
    # First Value refers to the value at the minimum year
      first_val = head(data[[i]], 1)
    # Diff takes the difference between the min and the maximum year
      diff = round(last_val - first_val, 3)
    #Sizes the arrow according to the changed value:
      if (abs(diff) < 0.005){
        
      }
      fig <- fig %>%
      # Create the Arrow corresponding to the criteria:
        add_annotations(x = last_val,
                        y = yvals[i],
                        xref = "x",
                        yref = "y",
                        ax = first_val,
                        ay = yvals[i],
                        axref = "x",
                        ayref = "y",
                        text = "",
                        showarrow = TRUE,
                        arrowhead = 1,
                        arrowcolor = I(colors[i])) %>%
      #Creates the Text so that you know what is what
        add_annotations(x = (last_val + first_val)/ 2,
                        y = yvals[i] + 0.5,
                        xref = "x",
                        yref = "y",
                        text = labels[i],
                        showarrow = FALSE) %>%
      # Creates an invisible trace along the arrow to show hover info
      # stupid plotly does not have hover over whole arrow available yet >_>
        add_trace(x = data[[i]], y = yvals[i], mode = "lines", hoverinfo = "text",
                  text = if (diff > 0) paste0("+", diff) else diff,
                  showlegend = FALSE, line = list(color = "rgba(0,0,0,0)"))
    }
    
    
    fig <- fig %>% layout(
      title = paste0("Democracy in ", input$country_selection, " from ", 
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
  })
  
}
