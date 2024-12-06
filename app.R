library(shiny)
library(bslib)
library(osmdata)
library(tidyverse)
library(ggplot2); theme_set(theme_bw()); theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
library(rinat)
library(plotly)
library(lubridate)
library(DT)
library(paleobioDB)

# Define UI
ui <- page_sidebar(
    title = "SNASHBOARD",
    # Make the sidebar
    sidebar = sidebar(
        helpText(
            "Welcome to Snashboard, the snail dashboard! Technically this is a gastropod dashboard, but Gashboard just didn't have the same ring to it."
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
    layout_columns(
        # Inaturalist and paleobio db output
        # Card for iNaturalist output
        navset_card_underline(
            title = "Snails near you",
            nav_panel("Map", 
                    plotlyOutput("inat_map"),
                    plotlyOutput("inat_bar")
            ),
            nav_panel("Abundance", 
                    dataTableOutput("inat_abd")
            ),
            nav_panel("All observations", dataTableOutput("inat_table")),
            nav_panel("", tags$img(src='willem-dafoe-gq-style3.png', alt = "Willem Dafoe is delighted by his fancy coat", align = "center"))
        ),
        # Card for pbdb output
        navset_card_underline(
            title = "Snails that were near you",
            nav_panel("Map", 
                    plotlyOutput("pbdb_map"),
                    plotlyOutput("pbdb_bar")
            ),
            nav_panel("Eras", 
                    plotlyOutput("pbdb_eras")
            ),
            nav_panel("All observations", dataTableOutput("pbdb_table")),
            nav_panel("", "placeholder")
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
    pbdb_data <- eventReactive(input$enter,{
        bounds <- bb()[c(2,1,4,3)]
        pbdb_occurrences(
            base_name = "Gastropoda",
            show = c("coords", "classext"),
            vocab = "pbdb",
            limit = "all",
            lngmax = bounds[4], lngmin = bounds[2], latmax = bounds[3], latmin = bounds[1]
        )
    })

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
        # add genus so they can sort the table with it
        separate(scientific_name, into = c("genus","species"), sep = " ", remove = F) %>%
        mutate(species = replace_na(species, "sp."))%>%
        add_count(scientific_name) %>%
        distinct(scientific_name, genus, species, common_name, n)
    })

    # Make iNaturalist observation data table
    output$inat_table <- renderDataTable({
        inat_data() %>%
        # filter by year
        filter(year(observed_on) >= min(input$year), year(observed_on) <= max(input$year)) %>%
        # don't display columns that include iNaturalist username or redundant info
        select(scientific_name, place_guess:longitude, common_name, observed_on)%>%
        # add genus so they can sort the table with it
        separate(scientific_name, into = c("genus","species"), sep = " ", remove = F) %>%
        mutate(
            species = replace_na(species, "sp."),
            # round coordinates for ease of display
            latitude = round(latitude, 5),
            longitude = round(longitude, 5)
            )
    })
    
    ###############
    # PBDB OUTPUT #
    ###############

    # Make paleobio db map
    output$pbdb_map <- renderPlotly({
        pbdb_data() %>%
        # plot
        ggplot()+
        # geom_jitter instead of geom_point
        # this is because if fossils are discovered together in the same rock formation they will all have the same coordinates
            geom_jitter(
                aes(x = lng, y = lat, color = genus),
                show.legend = F
            )+
            geom_sf(data = map_feat()$osm_lines)+
            xlim(bb()[c(1,3)])+
            ylim(bb()[c(2,4)]) +
            theme(legend.position = "none")
    })

    # Make pbdb abundance bar graph
    output$pbdb_bar <- renderPlotly({
        pbdb_data() %>%
        add_count(genus) %>%
        # Order genus by abundance
        mutate(genus = fct_reorder(genus, -n)) %>%
        # Plot
        ggplot(aes(x = genus, fill = identified_name))+
        geom_bar() +
        theme(
            legend.position = "none",
            axis.text.x = element_text(angle = 60, hjust = 1)
        )
    })

    # Make era-bars plot :)
    output$pbdb_eras <- renderPlotly({
        pbdb_data() %>%
        ggplot()+
        geom_linerange(aes(y = order, xmax = max_ma, xmin = min_ma, color = early_interval))+
        xlim((c(max(pbdb_data()$min_ma), min(pbdb_data()$max_ma)))) +
        xlab("Million years ago")+
        ggtitle("Era Bars")
    })

    # Make paleobio db table
    output$pbdb_table <- renderDataTable({
        pbdb_data()
    })
}

shinyApp(ui, server) 