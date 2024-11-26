library(shiny)
library(bslib)
library(osmdata)

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
    layout_columns(
        # Card for iNaturalist output
        card(
            "Snails near you",
            # Plots
            card(
                "placeholder"
            ),
            # Table
            card(
                "placeholder"
            )
        ),
        # Card for pbdb output
        card(
            "Snails that were near you",
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
    
    # Get longitude/latitude bounds from location once user hits Enter
    bb <- eventReactive(input$enter, {
        req(input$location)
        getbb(input$location)
    })
    
    # Create slider for adjusting longitude
    output$longControl <- renderUI({
      sliderInput(
            "longitude",
            label = "Adjust longitude",
            min = bb()[1,1],
            max = bb()[1,2],
            value = c(bb()[1,])
        )
    })

    # Create slider for adjusting latitude
    output$latControl <- renderUI({
      sliderInput(
            "latitude",
            label = "Adjust latitude",
            min = bb()[2,1],
            max = bb()[2,2],
            c(value = bb()[2,])
        )
    })

    # Get map features (sf)
    map_feat <- eventReactive(input$enter,{
        opq(bbox = bb) %>% 
            add_osm_feature(key = 'boundary', value = "administrative") %>% 
            osmdata_sf()
    })

    # Make base map
    gg_map <- eventReactive(input$enter,{
        ggplot()+
            geom_sf(data = map_feat$osm_lines)+
            theme_bw()+
            xlim(input$longitude)+
            ylim(input$latitude)
    })

    # Get iNaturalist data
    # Make iNaturalist map/plots
    # Make iNaturalist table
    # Get paleobio db data
    # Make paleobio db map/plots
    # Make paleobio db table
}

shinyApp(ui, server)