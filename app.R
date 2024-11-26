library(shiny)
library(bslib)
library(osmdata)
library(tidyverse)
library(rinat)

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
        # User can adjust latitude and longitude within the bounds of their location
        uiOutput("longControl"),
        uiOutput("latControl")
    ),

    # Inaturalist and paleobio db output
        # Card for iNaturalist output
    card(
        "Snails near you",
        layout_columns(
            # Plots
            card(
                plotOutput("inat_map")
            ),
            # Table
            card(
                "placeholder"
            )
        )
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

    ## Create sliders for adjusting latitude/longitude
    output$longControl <- renderUI({
      sliderInput(
            "longitude",
            label = "Adjust longitude",
            min = bb()[1,1],
            max = bb()[1,2],
            value = c(bb()[1,])
        )
    })
    output$latControl <- renderUI({
      sliderInput(
            "latitude",
            label = "Adjust latitude",
            min = bb()[2,1],
            max = bb()[2,2],
            value = c(bb()[2,])
        )
    })

    ######################
    # INATURALIST OUTPUT #
    ######################

    # Make iNaturalist map/plots
    output$inat_map <- renderPlot({
        ggplot()+
            geom_sf(data = map_feat()$osm_lines)+
            theme_bw()+
            ### * COME BACK AND FIX LIMITS * ##
            xlim(input$longitude)+
            ylim(input$latitude) +
            geom_point(
                data = inat_data(), 
                aes(x = longitude, y = latitude, color = scientific_name),
                show.legend = F
            )
    })

    # Make iNaturalist table
    ##PLACEHOLDER##
    
    ###############
    # PBDB OUTPUT #
    ###############

    # Make paleobio db map/plots
    ##PLACEHOLDER##

    # Make paleobio db table
    ##PLACEHOLDER##
}

shinyApp(ui, server)