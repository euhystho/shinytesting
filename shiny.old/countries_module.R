  countries_plot_func <- function(input, output, session, data_func, v, output_id) {

    filtered_data <- reactive({
        df <- data_func() %>%
          filter(country %in% input$country_selection)
        if (nrow(df) != 0){
          min_year_data <- df %>%
            summarize(min_year = min(year, na.rm = TRUE))
          v$countries_min_time <- max(min_year_data$min_year)
          updateSliderInput(session, "inv_mmt",
                                min = v$countries_min_time,
                                max = v$countries_max_time)
        }
        
        return(df)

    })
    
    filtered_relative_data <- reactive({
        relative_countries <- append(input$country_selection, "United States")
        df <- data_func()  %>%
          group_by(country) %>%
          filter(country %in% relative_countries) %>%  
          arrange(country, year) %>%
          mutate(
            year_diff_prev = year - lag(year),
            year_diff_next = lead(year) - year
          ) %>%
          filter(year_diff_prev <= 1  | year_diff_next <= 1) %>%
          select(-year_diff_prev, -year_diff_next)
        
        min_year_data <- df %>%
          summarize(min_year = min(year, na.rm = TRUE))
        
        v$countries_min_time <- max(min_year_data$min_year)
        updateSliderInput(session, "inv_mmt",
                          min = v$countries_min_time,
                          max = v$countries_max_time
        )
        df <- df %>%  
          filter(year >= max(v$countries_min_time, 1820))  %>%
          group_by(year) %>%
          mutate(relative_gdp = gdppc / gdppc[country == "United States"]) %>%
          mutate(diff_from_us = abs(gdppc - gdppc[country == "United States"])) 
        return(df)

    })
    
    
    output[[output_id]] <- renderPlotly({
      compare_statement <- v$lower_bound < v$countries_min_time
      if (!compare_statement) {
        if (input$relative_us_gdp) {
          req(filtered_relative_data())
          data <- filtered_relative_data()
          y_value <- ~relative_gdp
          fill_val <- 'tozeroy'
          sorted_countries <- data %>% 
            filter(country %in% input$country_selection) %>% 
            group_by(country) %>% 
            summarize(avg_diff = mean(diff_from_us, na.rm = TRUE)) %>% 
            arrange(avg_diff) %>% pull(country) 
        } else {
          req(filtered_data())
          req(input$country_selection)
          data <- filtered_data()
          y_value <- ~gdppc
          fill_val <- 'none'
          sorted_countries <- input$country_selection
        }
        fig <- plot_ly(type = 'scatter', mode = 'lines')
        
        # Generate a color palette based on the number of countries selected
        colors <- RColorBrewer::brewer.pal(max(2 * length(sorted_countries), 3), "Set1")
        for (i in seq_along(sorted_countries)) {
          country_data <- data %>% filter(country == sorted_countries[i])
          fig <- fig %>% add_trace(data = country_data, x = ~year, y = y_value,
                                   name = sorted_countries[i],
                                   line = list(color = colors[i]), 
                                   fill = fill_val,
                                   inherit = TRUE)
        }
        
        fig <- fig %>% layout(title = list(
          text = if (output_id == "maddison_plot") "Data from Maddison Project" else "GDP Per Capita from 1950 to 2023", 
                                           xanchor = "center",
                                           yanchor = "top"),
                              autosize = TRUE,
                              plot_bgcolor='#e5ecf6',  
                              xaxis = list(title = 'Time Period'),  
                              yaxis = list(
                                title = if (input$relative_us_gdp) 'Percentage of US GDP per Capita' else 'GDP per Capita',
                                tickformat = if(input$relative_us_gdp) ".0%" else ""),
                              showlegend = TRUE,
                              shapes = v$intervention_line)
      }
    })
  }
