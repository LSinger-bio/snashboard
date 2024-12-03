library(shiny)
library(bslib)
library(osmdata)
library(tidyverse)
library(ggplot2); theme_set(theme_bw()); theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
library(rinat)
library(plotly)
library(lubridate)
library(DT)

# Define UI
ui <- page_sidebar(
    title = "SNASHBOARD",
    # Make the sidebar
    sidebar = sidebar(
        helpText(
            "Welcome to Snashboard, the snail dashboard! Technically this is a gastropod dashboard, but Gashboard just didn't quite have the same ring to it."
        ),
        # User enters their location
        textInput(
            "location",
            label = "Enter your location"
        ),
        actionButton(
            "enter",
            label = "Find snails near me"
        ),
        # User can filter inaturalist observation dates
         uiOutput("yearControl")
    ),

    # Inaturalist and paleobio db output
    # Card for iNaturalist output
    navset_card_underline(
        title = "Snails near you",
        nav_panel("Map", plotlyOutput("inat_map")),
        nav_panel("Abundance data", 
            layout_columns(
                plotlyOutput("inat_bar"), 
                dataTableOutput("inat_abd")
            )
        ),
        nav_panel("All observations", dataTableOutput("inat_table")),
        nav_panel("", tags$img(src='willem-dafoe-gq-style3.png', alt = "Willem Dafoe is delighted by his fancy coat", align = "center"))
    ),
    # Card for pbdb output
    card(
        "Snails that were near you",
        layout_columns(
            # Plots
            card(
                "placeholder"
            ),
            # Table
            card(
                "placeholder"
            )
        )
    )
)

server <- function(input, output, session){
    
    ##############
    ## GET DATA ##
    ##############

    # Get longitude/latitude bounds from location once user hits Enter
    bb <- eventReactive(input$enter, {
        req(input$location)
        getbb(input$location)
    })

    # Get map features (sf)
    map_feat <- eventReactive(input$enter,{
        opq(bbox = bb()) %>% 
            add_osm_feature(key = 'boundary', value = "administrative") %>% 
            osmdata_sf()
    })
    
    # Get iNaturalist data
    inat_data <- eventReactive(input$enter,{
        bounds <- bb()[c(2,1,4,3)]
        get_inat_obs(taxon_name = "Gastropoda", bounds = bounds, quality = "research", maxresults = 1000)
    })

    # Get paleobio db data
    ##PLACEHOLDER##

    ###############
    # REACTIVE UI #
    ###############

    output$yearControl <- renderUI({
        min_yr <- year(min(inat_data()$observed_on))
        max_yr <- year(max(inat_data()$observed_on))
      sliderInput(
            "year",
            label = "Filter iNaturalist observations by year",
            min = min_yr,
            max = max_yr,
            value = c(min_yr, max_yr)
        )
    })

    ######################
    # INATURALIST OUTPUT #
    ######################

    # Make iNaturalist map
    output$inat_map <- renderPlotly({
        inat_data() %>%
        # filter by year
        filter(year(observed_on) >= min(input$year), year(observed_on) <= max(input$year)) %>%
        # plot
        ggplot()+
            geom_point(
                aes(x = longitude, y = latitude, color = scientific_name),
                show.legend = F
            )+
            geom_sf(data = map_feat()$osm_lines)+
            xlim(bb()[c(1,3)])+
            ylim(bb()[c(2,4)]) +
            theme(legend.position = "none")
    })

    # Make iNaturalist abundance bar graph
    output$inat_bar <- renderPlotly({
        inat_data() %>%
        # filter by year
        filter(year(observed_on) >= min(input$year), year(observed_on) <= max(input$year)) %>% 
        # Get genus variable
        separate(scientific_name, into = c("genus","species"), sep = " ", remove = F) %>%
        add_count(genus) %>%
        # Order genus by abundance
        mutate(genus = fct_reorder(genus, -n)) %>%
        # Plot
        ggplot(aes(x = genus, fill = scientific_name))+
        geom_bar() +
        theme(
            legend.position = "none",
            axis.text.x = element_text(angle = 60, hjust = 1)
        )
    })

    # Make iNaturalist abundance data table
    output$inat_abd <- renderDataTable({
        inat_data() %>%
        # filter by year
        filter(year(observed_on) >= min(input$year), year(observed_on) <= max(input$year)) %>%
        add_count(scientific_name) %>%
        distinct(scientific_name, common_name, n)
    })

    # Make iNaturalist observation data table
    output$inat_table <- renderDataTable({
        inat_data() %>%
        # filter by year
        filter(year(observed_on) >= min(input$year), year(observed_on) <= max(input$year)) %>%
        # don't display columns that include iNaturalist username or redundant info
        select(scientific_name, place_guess:longitude, common_name, observed_on)
    })
    
    ###############
    # PBDB OUTPUT #
    ###############

    # Make paleobio db map/plots
    ##PLACEHOLDER##

    # Make paleobio db table
    ##PLACEHOLDER##
}

shinyApp(ui, server)