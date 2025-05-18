# These are the necessary packages for R Shiny:
library(shiny)
library(bslib)
library(bsicons)
library(thematic)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(ggstatsplot)
library(patchwork)
library(RColorBrewer)
library(data.table)
library(progress)
library(memoise)

#STAHP COMPLAINING 
options(warn = -1, spinner.type = 7) 

#OO COOL DEBUGGIE :3
# library(reactlog)
# options(shiny.reactlog = TRUE)

source("countries_module.R")
source("arrow_module.R")
source("image_module.R")

#Put the Base Data here:
set.seed(1234)
x1 <- 100 + arima.sim(model = list(ar = 0.9999), n = 100)
y <- 5 * x1 + rnorm(100)
y[70:100] <- y[70:100] + 300
data <- cbind(y, x1)

ui <- navbarPage(
#Similar to thematics, where you just initialize it and it works in the background
  useShinyjs(),
  #Custom HTML Code for the Image:
  tags$head( 
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css") 
  ),
#Puts Title on the Application on the top
  title = "The Shiniest of Testing ✨",
  
  #Sets the theme for bslib using the Bootswatch Library:
  theme = bs_theme(
    bootswatch = "morph"
  ),

  
#Creates the sidebar
  sidebar = sidebar(
    title = "Controls",
    fillable = TRUE,
    accordion_filters <- accordion(
      id = "the_accordion",
      accordion_panel(
        "Intervention Effect", 
        icon = bs_icon("card-checklist"),
        
      #Setup with input funcs in Shiny are always input id, then the name in the app
      #followed by the values
      div(id = "ani_slider",
          sliderInput("inv_mmt", 
                      "Select an Intervention Moment", 
                      min = 4, 
                      max = 96, 
                      value = 70,
                      animate = animationOptions(interval = 600, loop = TRUE)
          )
          ),
      div(id = "arrow_slider",
          noUiSliderInput("demo_range",
                          "Select a Time Range:",
                          min = 1900, 
                          max = 2023,
                          value = c(1900,2023),
                          tooltips = TRUE,
                          margin = 1,
                          step = 1,
                          format = wNumbFormat(
                            decimals = 0
                          )
          ),
  ),
    div (id = "checkboxes",
         prettyCheckboxGroup("arrow_checkbox",
                             "Select Categories:",
                             choices = c("Option 1", "Option 2", "Option 3"),
                             shape = "curve",
                             plain = TRUE,
                             icon = icon("landmark-flag", lib = "font-awesome")
         )
  ),
      #Switches between the interactive plots through plotly :)
        input_switch("plotly_switch", "Interactivity through Plotly",
                     value = TRUE)
      ),
      
      div(id = "filters",
          accordion_panel(
            title = "Filters",
            icon = bs_icon("funnel-fill"),
            selectizeInput("country_selection", 
                        "Select Countries:", 
                        choices = NULL,
                        multiple = TRUE),
            #Switches between the interactive plots through plotly :)
            input_switch("relative_us_gdp", "GDP Relative to the United States",
                         value = FALSE)
          )
        )

    )
  ),



# Creates a Tab
  tabsetPanel(
    id = "the_tabset",
    tabPanel(
      title = "Comparing Periods",
      value = "cmp_periods",
      page_navbar(
        id = "the_tabs",
        nav_panel(
          title = "Comparing Periods (Base)",
          value = "base_plot",
          card(
            full_screen = TRUE,
            
            conditionalPanel(
              full_screen = TRUE,
              condition = "input.plotly_switch == true",
              fillPage(
                  plotlyOutput("plot2_plotly", height = "100%")
              )
            ),
            
            conditionalPanel(
              condition = "input.plotly_switch == false",
                plotOutput("plot2")     
              
            )
            
          )
        ),
        nav_panel(
          title = "Comparing Periods (Matrix)",
          value = "matrix_plot",
          card(
            full_screen = TRUE,
            
            conditionalPanel(
              full_screen = TRUE,
              condition = "input.plotly_switch == true",
                plotlyOutput("mtx_plotly")

            ),
            
            conditionalPanel(
              condition = "input.plotly_switch == false",
                plotOutput("mtxplot")

            )
            
          )
        ),
        nav_panel(
          title = "Confidence Bands (Dark Statistical Magic)",
          value = "dark_magics",
          card(
            div(class = "magic-image-container", 
                imageOutput("magic_image", width = "100%"))
          ),
        )
      ),
    ),
    tabPanel(
      title = "Comparing Countries GDP Per Capita",
      value = "cmp_countries",
      page_navbar(
        id = "the_countries",
        nav_panel(
          title = "Data from 1950 to 2023",
          value = "hugo_data",
          card(
            full_screen = TRUE,
              plotlyOutput("country_plot")
          )
        ),
        nav_panel(
          title = "Data from Year 1 to 2022",
          value = "maddison_proj",
          card(
            full_screen = TRUE,
              plotlyOutput("maddison_plot")
          )
        ),
        nav_panel(
          title = "Venezuela Falsification BSTS Tests",
          value = "venezuela_model",
          card(
            full_screen = TRUE,
            div(class = "venezuela-image-container", 
                imageOutput("venezuela_image", width = "100%"))
          )
        )


      ),
    ),
    tabPanel(
      title = "Cool Arrow Plots",
      value = "arrow_plot",
      page_navbar(
        id = "the_arrow_plots",
        nav_panel(
          title = "Varieties of Democracy over Time Period",
          value = "democracy",
          card(
            full_screen = TRUE,
            plotlyOutput("democracy_plot")
          )
        )
        
      ),
    ),

  ),

  

)

server <- function(input, output, session){
#Sets the Plots to the same theme as a bslib theme :D
  thematic_shiny()
#Hide the filters before loading anything else...
  shinyjs::hide(id = "filters")
  shinyjs::hide(id = "arrow_slider")
  shinyjs::hide(id = "checkboxes")
  
  
#Kind of like a list of values to set that can by dynamically allocated
  v <- reactiveValues(
    lower_bound = 0,
    pre.period = 0,
    post.period = 0,
    df = NULL,
    country_choices = NULL,
    intervention_line = list(),
    cmp_pds_inv_mmt = NULL,
    cmp_cts_inv_mmt = NULL,
    countries_min_time = 1950,
    countries_max_time = 2023,
    arrow_min_year = 1997,
    arrow_max_year = 2023,
    selected_countries = "Venezuela (Bolivarian Republic of)"
  )

  
#LOAD FASTER PLS Functions Below :3
  
  #Read Matrices:
    will_data <- readRDS("datasets/Matrix1.rds")
    country_data <- reactive ({
      #Read the File 
      readRDS("datasets/Data_SC_BSTS.rds") 
    })
    maddison_data <- reactive({
      readRDS("datasets/MPD.rds") 
    })
    
    region_country_dict <- reactive({
      readRDS("datasets/MPD_Dict.rds")
    })
    
    vdem_data <- reactive({
      readRDS("datasets/VDEM.rds") 
    })
# Memoisation corner :3
    memoise_plot <- memoise(function(inv_data){
        ggbetweenstats(
          data = inv_data,
          x = period,
          y = y,
          title = "Comparing the Pre and Post Intervention Periods",
          xlab = "Period",
          ylab = "Intervention Amounts",
          ggplot.component = list(theme(text = element_text(size = 14)))
        )
      })
    
    base_plot <- reactive({
      req(v$df)
      memoise_plot(inv_data = v$df)
    })
    


#Tracks the Intervention Moment and updates the bounds accordingly
    observeEvent({
      input$inv_mmt
      input$the_tabset}, {
      v$lower_bound <- input$inv_mmt[1]
      v$pre.period <- c(1, v$lower_bound)
      v$post.period <- c(v$lower_bound + 1, 100)
      v$df <- data.table(
        time = seq_len(nrow(data)),
        y = data[, 1],
        period = ifelse(seq_len(nrow(data)) < v$lower_bound, "1. Pre-intervention", "2. Post-intervention")
      )
    compare_statement <- v$lower_bound < v$countries_min_time 
    if (input$the_tabset == "cmp_countries" && compare_statement ){
    # If the intervention moment is before the minimum time period for the country
    # save it, so that we can use it later for switching back
      shinyjs::show(id = "ani_slider")
      shinyjs::show(id = "relative_us_gdp")
      updateSelectizeInput(session, "country_selection")
      v$cmp_pds_inv_mmt = input$inv_mmt[1]
      v$intervention_line <- list(
        list(
          type = "line",
          x0 = v$countries_min_time, x1 = v$countries_min_time,
          y0 = 0, y1 = 1,
          # The one line of code that makes it fit the entire graph D:
          yref = "paper",
          line = list(color = "gray", width = 2, dash = 'dash')
        )
      )
    }
    if (input$the_tabset == "cmp_periods" && !compare_statement){
      shinyjs::show(id = "ani_slider")
      v$cmp_cts_inv_mmt = input$inv_mmt[1]
      v$intervention_line <- list(
        list(
          type = "line",
          x0 = v$cmp_pds_inv_mmt, x1 = v$cmp_pds_inv_mmt,
          y0 = 0, y1 = 1,
          # The one line of code that makes it fit the entire graph D:
          yref = "paper",
          line = list(color = "gray", width = 2, dash = 'dash')
        )
      )
    } else if (input$the_tabset == "arrow_plot"){
      shinyjs::show(id = "arrow_slider")
      shinyjs::show(id = "checkboxes")
      shinyjs::hide(id = "ani_slider")
      shinyjs::hide("plotly_switch")
      shinyjs::show(id = "filters")
      shinyjs::hide(id = "relative_us_gdp")
      v$country_choices <- unique(vdem_data()$country)
      updateSelectizeInput(session, "country_selection", 
                        choices = v$country_choices,
                        selected = "Hong Kong")
    } else {
      shinyjs::show(id = "ani_slider")
      shinyjs::hide(id = "arrow_slider")
      shinyjs::hide(id = "checkboxes")
      updateSelectizeInput(session, "country_selection")
      v$intervention_line <- list(
        list(
          type = "line",
          x0 = v$lower_bound, x1 = v$lower_bound,
          y0 = 0, y1 = 1,
          # The one line of code that makes it fit the entire graph D:
          yref = "paper",
          line = list(color = "gray", width = 2, dash = 'dash')
        )
      )
    } 
    
  })
  #Looks complicated, just isn't
  observeEvent({
    input$the_tabs 
    input$the_tabset},{
      if (input$the_tabs == "base_plot"){
        updateSliderInput(session, "inv_mmt",
                          min = 4,
                          max = 96,
                          value = v$cmp_pds_inv_mmt,
        )
        shinyjs::show("plotly_switch")
      } else {
        updateSliderInput(session, "inv_mmt", 
                          min = 51,
                          max = 91,
                          value = v$cmp_pds_inv_mmt,
        )
      }
      #Gotta hide the switch for the "dark magics"
      if (input$the_tabs == "dark_magics"){
        shinyjs::hide("plotly_switch")
      }
      if (input$the_tabs == "matrix_plot"){
        shinyjs::show("plotly_switch")
      }
      #Makes sure that the slider input is consistent with what is available for
      #each intervention moment in each tab
      if (input$the_tabset == "cmp_countries"){
        shinyjs::show("filters")
        if (is.null(v$cmp_cts_inv_mmt)){
          v$cmp_cts_inv_mmt <- 1990
        }
        updateSliderInput(session, "inv_mmt", 
                          min = v$countries_min_time,
                          max = 2023,
                          value = v$cmp_cts_inv_mmt,
        )
        shinyjs::hide("plotly_switch")
      }
      if (input$the_tabset == "cmp_periods"){
        shinyjs::hide("filters")
      }
      if (input$the_tabset == "arrow_plot"){
        shinyjs::hide("plotly_switch")
        updatePrettyCheckboxGroup(
          session = session,
          inputId = "arrow_checkbox",
          choices = c("Electoral Democracy", "Liberal Democracy", "Participatory Democracy", "Deliberative Democracy", "Egalitarian Democracy", "Fair Elections"),
          selected = c("Electoral Democracy", "Liberal Democracy", "Participatory Democracy", "Deliberative Democracy", "Egalitarian Democracy", "Fair Elections"),
          prettyOptions = list(
            icon = icon("landmark-flag"),  # Icon for Tab 2
            plain = TRUE,
            animation = "smooth",
            shape = "curve"
          )
        )
      }
    })
  observeEvent(input$the_countries,{   
    if (input$the_countries == "venezuela_model") {
      shinyjs::hide(id = "filters")
      v$countries_min_time <- 1970
      v$countries_max_time <- 2020
      updateSliderInput(session, "inv_mmt",
                            min = v$countries_min_time,
                            max = v$countries_max_time)
    } else if (input$the_countries == "hugo_data"){
      v$country_choices <- unique(country_data()$country)
      v$countries_max_time <- 2023
    } else if (input$the_countries == "maddison_proj"){
      v$country_choices <- region_country_dict()
      v$countries_max_time <- 2022
    } 
    if (input$the_countries == "maddison_proj" | input$the_countries == "hugo_data"){
      shinyjs::show(id = "filters")
      updateSelectizeInput(session, "country_selection", 
                        choices = v$country_choices,
                        selected = v$selected_countries)
    }
  })
  
  
  observeEvent({input$demo_range
    input$country_selection},{
    #TODO: Fixed Logic for Multiple Countries... However now doesn't work for no countries yey
      data <- vdem_data()
      new_min <- -Inf
      new_max <- Inf
      if (length(input$country_selection) != 0){
        for (country in input$country_selection){
          country_year_column <- data$year[data$country == country]
          current_min = min(country_year_column)
          current_max = max(country_year_column)
          new_min <- max(new_min, current_min)
          new_max <- min(new_max, current_max)
        }
      } 

      updateNoUiSliderInput(session, "demo_range", 
                            range = c(new_min, new_max))
      v$arrow_min_year <- if (input$demo_range[1] > new_min) input$demo_range[1] else new_min
      v$arrow_max_year <- if (input$demo_range[2] < new_max) input$demo_range[2] else new_max
    })
  
  observeEvent(input$country_selection,{
    v$selected_countries <- input$country_selection
  })



  output$plot2_plotly <- renderPlotly({
    compare_statement <- v$lower_bound < v$countries_min_time
    if (compare_statement){
      p <- base_plot()
      ggplotly(p) %>%
        style(
          hovertemplate = "<b>Time:</b> %{customdata[0]}<br><b>Period:</b> %{customdata[1]}<br><b>Intervention Amount:</b> %{y}<extra></extra>",
          customdata = cbind(v$df$time, v$df$period)
        )
    }

  })
  
  output$plot2 <- renderPlot ({
    compare_statement <- v$lower_bound < v$countries_min_time
    if (compare_statement){
      base_plot()
    }
  })

image_func(session = session,
           output = output, 
           v = v,
           output_id = "magic_image",
           file_path = "images/InterventionMoments/Intervention%d.png")

image_func(session = session,
           output = output, 
           v = v,
           output_id = "venezuela_image",
           file_path = "images/Venezuela/Figure-BSTS_Falsification.%d.jpeg")


countries_plot_func(input = input,
                    output = output, 
                    session = session,
                    data_func = country_data, 
                    v = v,
                    output_id =  "country_plot")

countries_plot_func(input = input,
                    output = output,
                    session = session,
                    data_func = maddison_data,
                    v = v,
                    output_id =  "maddison_plot")

arrow_plot_func(input = input,
                    output = output,
                    session = session,
                    data_func = vdem_data,
                    v = v,
                    output_id =  "democracy_plot")

  output$mtxplot <- renderPlot({
    compare_statement <- v$lower_bound < v$countries_min_time
    if (compare_statement){
      inv_adj_amt = v$lower_bound - 50
      yval = paste0("V", inv_adj_amt)
      
      # Make sure the column names are unique, otherwise error happens... yayyy
      colnames(will_data) <- make.unique(names(will_data))
      
      actual_data <- data.frame(time = seq_len(nrow(data)), y = as.numeric(data[,1]))
      p <- ggplot() + 
        geom_line(data = actual_data, aes(x = time, y = y), color = 'black', size = 1, linetype = "solid", name = "Actual Data") +
        geom_line(data = will_data, aes_string(x = "seq_len(nrow(will_data))", y = yval), color = 'blue', size = 1, linetype = "dashed", name = "Predicted Data") +
        geom_vline(xintercept = v$lower_bound, color = "gray", linetype = "dashed", size = 1) +
        labs(title = 'Comparing the Pre and Post Intervention Periods',
             x = 'Time Period',
             y = 'Intervention Moments') + 
        theme_minimal() + 
        theme(plot.background = element_rect(fill = "white"), legend.position = "bottom")
      
      plot(p)
    }

  })
  
    
  output$mtx_plotly <- renderPlotly({
    compare_statement <- v$lower_bound < v$countries_min_time
    if (compare_statement){
      #Adjust the intervention amount based on the columns of the matrix
      inv_adj_amt <- v$lower_bound - 50
      #Correspond it to the column of the matrix
      yval = paste0("V", inv_adj_amt)
      
      actual_data <- data.frame(time = seq_len(nrow(data)), y = data[,1])
      
      
      fig <- plot_ly(x = actual_data$time, y = actual_data$y, type = 'scatter', mode = 'lines', 
                     name = "Actual Data", 
                     line = list(color = 'rgb(0,0,0)'))%>% 
        layout(title = list(text = "Comparing the Pre and Post Intervention Periods", 
                            xanchor = "center",
                            yanchor = "top"),
               autosize = TRUE,
               plot_bgcolor='#e5ecf6',  
               xaxis = list(  
                 title = 'Time Period'),  
               yaxis = list(  
                 title = 'Intervention Moments'),
               
               showlegend = TRUE,
               #Create a Vertical Line to indicate the intervention moment:
               shapes = v$intervention_line
               
        )
      #Create a "Layer" to show the Predicted Data extrapolated from the matrix :)
      fig <- fig %>% add_trace(y = will_data[[yval]], name = 'Predicted Data', 
                               line = list(color = 'rgb(51,171,249)', dash = 'dash') 
      )
    }

  })

# Ends the app when the user gets out of the shiny app in the tab
  # DO NOT PUSH this into GIT and HOST IT (LOCAL DEV ONLY)
  #session$onSessionEnded(stopApp)
  }

shinyApp(ui,server)

