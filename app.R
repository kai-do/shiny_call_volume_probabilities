library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(shinyWidgets)
library(shinyDarkmode)
library(lemon)

incidents_raw_df <- fread("incident_counts_by_first_in_callsign.csv", na.strings = "NULL") %>%
    mutate(incident_date = as.Date(incident_date)) %>%
    arrange(first_unit_battalion_id, first_unit_station_id)

#incidents_df <-incidents_raw_df %>%
#    mutate(first_unit_battalion = ordered(first_unit_battalion),
#           first_unit_station = ordered(first_unit_station))
#
#sort(unique(incidents_raw_df$first_unit_battalion))
#sort(unique(incidents_raw_df$first_unit_station))

#test <- incidents_raw_df %>%
#    complete(first_unit_battalion, incident_date, fill = list(incident_count = 0)) %>%
#    group_by(first_unit_battalion, incident_date) %>%
#    summarise(first_unit_battalion, incident_count = sum(incident_count)) %>%
#    group_by(first_unit_battalion) %>%
#    summarise(first_unit_battalion, daily_mean_incidents = mean(incident_count)) %>%
#    crossing(trial = 0:100) %>%
#    mutate(poisson_prob = dpois(trial, daily_mean_incidents))
#
#input <- list()
#
#input$grain <- "first_unit_battalion_id"
#input$display <- "Facets"
#xlim_dyn <- 80
#
#test_plot <-
#    {if (input$grain == "first_unit_battalion_id") { 
#        ggplot(test, aes(x = trial, y = poisson_prob, fill = first_unit_battalion)) +
#            #{if (input$display == "Facets") facet_wrap(vars(first_unit_battalion), ncol = 1, scales = 'fixed', shrink = FALSE)}
#            {if (input$display == "Facets") facet_grid(first_unit_battalion ~ NA, scales = 'free_y', space = 'fixed', shrink = FALSE)}
#        
#    } else if (input$grain == "first_unit_station_id") {
#        ggplot(test, aes(x = trial, y = poisson_prob, fill = first_unit_station)) +
#            {if (input$display == "Facets") facet_grid(vars(first_unit_station), scales = 'free_y', space = 'free_y', shrink = FALSE)}
#    }} +
#    geom_bar(stat = "identity",
#             position = "identity",
#             alpha = 0.3) +
#    geom_line(alpha = 0.3) +
#    
#    #geom_text(aes(label = round(poisson_prob, 2)), vjust = -0.2) +
#    labs(x = "Daily Incident Count",
#         y = "Probability",
#         fill = "Legend") +
#    coord_cartesian(xlim = c(0, max_xlim)) +
#    scale_x_continuous(labels = xlim_dyn,
#                       breaks = xlim_dyn) + 
#    theme(text = element_text(size = 16)) 
#
#plot(test_plot)



batt_df <- incidents_raw_df %>%
    distinct(first_unit_battalion_id,
             first_unit_battalion) %>%
    arrange(first_unit_battalion_id)

battalion_choices <-
    setNames(as.list(as.character(batt_df$first_unit_battalion_id)), nm = batt_df$first_unit_battalion)


station_df <- incidents_raw_df %>%
    distinct(first_unit_station_id,
             first_unit_station) %>%
    arrange(first_unit_station_id)

station_choices <-
    setNames(as.list(as.character(station_df$first_unit_station_id)), nm = station_df$first_unit_station)


callsign_choices <-
    sort(unique(incidents_raw_df$first_unit_callsign))

shift_choices <- sort(unique(incidents_raw_df$first_unit_shift))

min_date <- min(incidents_raw_df$incident_date)
max_date <- max(incidents_raw_df$incident_date)

grain_choices <- list("Battalion" = "first_unit_battalion_id",
                      "Station" = "first_unit_station_id")

display_choices <- list("Facets", "Overlayed")

ui <- fluidPage(
    fluidRow(
        use_darkmode(),
        column(11,
               titlePanel(
                   "Poisson Probability Distribution of Daily Incident Counts by Battalion"
               )
        ),
        column(1, tags$div(
            #style = "margin-top: -25px; float: right; margin-right: -150px;",
            style = "margin-top: 25px; text-align: left;",
            prettySwitch("togglemode", "Dark Mode", value = TRUE, fill = TRUE, status = "info")
        )
        )),
    tags$head(
        tags$script(
            '
            var dimension = [0, 0];
            var display_mode = "dark";
            $(document).on("shiny:connected", function(e) {
                dimension[0] = window.innerWidth;
                dimension[1] = window.innerHeight;
                if (window.matchMedia("(prefers-color-scheme: dark)").matches) {
                    display_mode = "dark";
                } else {
                    display_mode = "light";
                };
                Shiny.onInputChange("dimension", dimension);
            });
            $(window).resize(function(e) {
                dimension[0] = window.innerWidth;
                dimension[1] = window.innerHeight;
                Shiny.onInputChange("dimension", dimension);
            });
            '
        )
    ),
    
    #tabsetPanel(type = "pills",
    #            tabPanel("Distribution Plot",
    #                     wellPanel(
    #                     fluidRow(column(12, plotOutput("dist_plot")))
    #                     )
    #            )
    #),
    
    
    
    fluidRow(column(12,
                    wellPanel(
                        sliderInput(
                            "dates",
                            "Date Range:",
                            min = min_date,
                            max = max_date,
                            value = c(min_date, max_date)
                        )
                    ))),
    fluidRow(
        column(3,
               wellPanel(
                   pickerInput(
                       "battalion",
                       "Battalion Filter",
                       choices = battalion_choices,
                       multiple = TRUE,
                       selected = battalion_choices[1:5],
                       options = pickerOptions(
                           actionsBox = TRUE,
                           deselectAllText = "Deselect All",
                           selectAllText = "Select All"
                       )
                   )
               )),
        column(3,
               wellPanel(
                   pickerInput(
                       "station",
                       "Station Filter",
                       choices = station_choices,
                       multiple = TRUE,
                       selected = station_choices,
                       options = pickerOptions(
                           actionsBox = TRUE,
                           deselectAllText = "Deselect All",
                           selectAllText = "Select All"
                       )
                   )
               )),
        column(3,
               wellPanel(
                   pickerInput("grain",
                               "Select Granularity",
                               choices = grain_choices)
               )),
        column(3,
               wellPanel(
                   pickerInput("display",
                               "Display Style",
                               choices = display_choices)
               ))
    ),
    fluidRow(#style = "height: 500px;",
        column(12, 
               
               #style='width:400px;overflow-x: scroll;height:400px;overflow-y: scroll;', 
               #plotOutput("dist_plot")#, width = "500px", height = "500px"))
               uiOutput("dist_plot_ui")
        )),
    hr(), 
)

server <- function(input, output, session) {
    
    darkmode_toggle(inputid = 'togglemode',
                    saveInCookies = TRUE,
                    autoMatchOsTheme = TRUE)
    
    dimension_display <- reactive({
        data.frame(
            x = input$dimension[1],
            y = input$dimension[2],
            ratio = input$dimension[2] / input$dimension[1]
        )
    })
    
    station_choices <- reactive({
        req(input$battalion)
        station_df <- incidents_raw_df %>%
            filter(first_unit_battalion_id %in% input$battalion) %>%
            distinct(first_unit_station_id,
                     first_unit_station) %>%
            arrange(first_unit_station_id)
        
        return(setNames(as.list(
            as.character(station_df$first_unit_station_id)
        ), nm = station_df$first_unit_station))
    })
    
    observe({
        req(input$battalion, input$dates[1], input$dates[2],)
        updatePickerInput(session = getDefaultReactiveDomain(),
                          "station",
                          choices = station_choices(),
                          selected = station_choices())
    })
    
    filtered_incident_df <- reactive({
        req(input$battalion, input$station)
        incidents_raw_df %>%
            filter(
                between(incident_date, input$dates[1], input$dates[2]),
                first_unit_battalion_id %in% input$battalion,
                first_unit_station_id %in% input$station
            )
    }) %>% debounce(1000)
    
    
    incident_counts_df <- reactive({
        req(input$battalion, input$station, input$grain, input$dates[1], input$dates[2])
        
        if (input$grain == "first_unit_battalion_id") {
            df <- filtered_incident_df() %>%
                complete(first_unit_battalion,
                         incident_date,
                         fill = list(incident_count = 0)) %>%
                group_by(first_unit_battalion, incident_date) %>%
                summarise(first_unit_battalion,
                          incident_count = sum(incident_count)) %>%
                group_by(first_unit_battalion) %>%
                summarise(first_unit_battalion,
                          daily_mean_incidents = mean(incident_count)) %>%
                crossing(trial = 0:100) %>%
                mutate(poisson_prob = dpois(trial, daily_mean_incidents))
        } else if (input$grain == "first_unit_station_id")  {
            df <- filtered_incident_df() %>%
                complete(first_unit_station,
                         incident_date,
                         fill = list(incident_count = 0)) %>%
                group_by(first_unit_battalion, first_unit_station, incident_date) %>%
                summarise(first_unit_station,
                          incident_count = sum(incident_count)) %>%
                group_by(first_unit_battalion, first_unit_station) %>%
                summarise(first_unit_station,
                          daily_mean_incidents = mean(incident_count)) %>%
                crossing(trial = 0:100) %>%
                mutate(poisson_prob = dpois(trial, daily_mean_incidents)) %>%
                filter(!is.na(first_unit_battalion))
        }
        return(df)
    })
    
    plot_height <- reactive({
        if(input$grain == "first_unit_battalion_id") {
            output <- max(ceiling(length(input$battalion)) * 80, 500)
        } else if (input$grain == "first_unit_station_id" & input$display == "Facets") {
            output <- max(ceiling(length(input$station)) * 80, 500)
        } else if(input$grain == "first_unit_station_id" & input$display == "Overlayed") {
            output <- 500
        }
        return(output)
    })
    
    output$dist_plot <- renderPlot(
        {
            req(input$grain, input$dates[1], input$dates[2], incident_counts_df(), station_choices())
            
            max_xlim <- plyr::round_any(max(incident_counts_df()$daily_mean_incidents) * 1.5,
                                        10,
                                        f = ceiling)
            
            dim_x <- dimension_display()$x
            
            xlim_dyn_by <- (round(max_xlim/(dim_x/35), 0))
            
            if(length(xlim_dyn_by) < 1) {
                xlim_dyn_by <- 1
            } 
            
            xlim_dyn <- seq.int(from = 0, to = max_xlim, by = max(c(xlim_dyn_by,1)))
            
            
            plot <-
                {if (input$grain == "first_unit_battalion_id") { 
                    ggplot(incident_counts_df(), aes(x = trial, y = poisson_prob, fill = first_unit_battalion)) +
                        {if (input$display == "Facets") facet_wrap(vars(first_unit_battalion), ncol = 1, scales = 'fixed', shrink = FALSE)}
                    #{if (input$display == "Facets") facet_grid(first_unit_battalion ~ NA, scales = 'free_y', space = 'fixed', shrink = FALSE)}
                    
                } else if (input$grain == "first_unit_station_id") {
                    ggplot(incident_counts_df(), aes(x = trial, y = poisson_prob, fill = first_unit_station)) +
                        #{if (input$display == "Facets") facet_wrap(vars(first_unit_station), ncol = 2, scales = 'fixed', shrink = FALSE)}
                        {if (input$display == "Facets") facet_rep_wrap(vars(first_unit_station), ncol = 2, scales = 'fixed', shrink = FALSE, repeat.tick.labels = TRUE)}
                    #{if (input$display == "Facets") facet_grid(first_unit_station ~ first_unit_battalion, scales = 'free_y', space = 'fixed', shrink = FALSE)}
                }} +
                geom_bar(stat = "identity",
                         position = "identity",
                         alpha = 0.3) +
                geom_line(alpha = 0.3) +
                
                #geom_text(aes(label = round(poisson_prob, 2)), vjust = -0.2) +
                labs(x = "Daily Incident Count",
                     y = "Probability",
                     fill = "Legend") +
                coord_cartesian(xlim = c(0, max_xlim)) +
                scale_x_continuous(labels = xlim_dyn,
                                   breaks = xlim_dyn) + 
                theme(text = element_text(size = 16),
                      panel.grid = element_blank(),
                      panel.grid.minor.y = element_blank(),
                      legend.position = "bottom")
            
            return(plot)
        })
    
    
    output$dist_plot_ui <- renderUI({
        plotOutput("dist_plot", height = plot_height(),)
    })
    
    
}

shinyApp(ui = ui, server = server)
