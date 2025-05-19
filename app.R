library(bslib)
library(DBI)
library(DT)
library(fontawesome)
library(glue)
library(htmltools)
library(leaflet)
library(sf)
library(shinybusy)
library(shinydashboard)
library(shinyjs)
library(shiny)
library(shinythemes)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Geocoding QA"),
  dashboardSidebar(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    sidebarMenu(id = "sidebarid",
                actionButton("show", "Show Instructions Dialog"),
                menuItem("Geocoding Summary", tabName = "page1"),
                conditionalPanel('input.sidebarid == "page1"'
                                 # textInput("u_name", "Username", ""),
                                 # textInput("u_pass", "User Password", ""),
                                 # actionButton("db_connect", "Connect to Database")
                ),
                menuItem("Show Map", tabName = "page2"),
                conditionalPanel('input.sidebarid == "page2"',
                                 uiOutput("select_state_ui"),
                                 uiOutput("select_county_ui"),
                                 uiOutput("select_confidence_ui"),
                                 checkboxInput("add", "Edit Coordinates?", TRUE),
                                 conditionalPanel("input.add==1",
                                                  textInput("coords", "New Coordinates", ""),
                                                  textOutput('error_msg'),
                                                  actionButton("plot_test_points", "Plot Test Points", icon = icon("map-pin"), class = "btn-primary"),
                                                  actionButton("remove_test_points", "Remove Test Points", icon = icon("refresh"), class = "btn-danger"),
                                                  actionButton("save", "Save Changes to DB", icon = icon("download"), class = "btn-success"),
                                                  checkboxInput("hide", "Hide Fixes", FALSE)
                                 )
                )
    )
  ),
  dashboardBody(
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('scrollToSelectedRow', function(selectedRow) {",
      "  const table = $('#table table').DataTable();",
      "  table.row(selectedRow - 1).node().scrollIntoView()",
      "});"
    )),
    tags$head(tags$style(HTML("div.box-header {text-align: center;}"))),
    tags$head(tags$style(HTML('.shiny-notification-default {background-color:#4BB543; color:#000000;}'))),
    tabItems(
      tabItem(tabName = "page1",
              box(width=12,
                  title = "Intersected Data Points and Geocoding Errors", status = "success", solidHeader = TRUE,
                  p(style="text-align: center;", strong("An in-memory SQLite database has been created under the hood of this app and populated with two example geocoded points in the county of Ashe, North Carolina."), br(),"These tables display the number of erroneously intersected points (relative to county boundaries) and the unreliability level of detected geocoding errors.")),
                # keep the box with the table of geocoding errors hidden until user initiates processing
                  hidden(div(id="error_box_wrap", box(id = "error_box", width=6, DTOutput("outside_county")))),
                  hidden(div(id="error_box_wrap", box(id = "error_box", width=6, DTOutput("low_score"))))
      ),
      tabItem(tabName = "page2",
              fluidRow(
                box(width=6,
                    title = textOutput("county_name"), status = "success", solidHeader = TRUE,
                    p(style="text-align: center;", "Geocoded points in relation to county-specific geographic boundaries"),
                    leafletOutput("map01", height = 620)),
                box(width=6,
                    title = "Geocoded Point Errors", status = "warning", solidHeader = TRUE,
                    p(style="text-align: center;", "Points that fall outside county boundary or are low certainty"),
                    DTOutput("table", height = 620)
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # create dummy geocoded df
  ashe <- data.frame(id = c(1,2),
                     state_fips = c("37", "37"),
                     county_fips = c("009", "009"),
                     name = c("West Jefferson Town Hall", "Ashe County Airport"),
                     address = c("1 S Jefferson Ave, West Jefferson, NC 28694", "639 Airport Rd, Jefferson, NC 28640"),
                     matched_address = c("CG25+R6 West Jefferson, North Carolina", "639 Airport Road Jefferson NC"),
                     confidence = c("high", "unreliable"),
                     status = c("green", "red"),
                     lat = c(36.403298807887595, 36.20719800347172),
                     lon = c(-81.49251381807994, -81.68540965615952),
                     edited = c(F,F)) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)
  
  # create dummy spatial df of 1 NC county boundary
  county_carto <- st_read(system.file("shape/nc.shp", package="sf")) %>% 
    dplyr::select(FIPS, geometry) %>% 
    mutate(state_fips = substr(.$FIPS, start = 1, stop = 2),
           county_fips = substr(.$FIPS, start = 3, stop = 5)) %>% 
    dplyr::select(-FIPS) %>% 
    slice(1) %>% 
    st_set_crs(4326)
  
  # show instructions modal on app startup
  showModal( 
    modalDialog( 
      title = "Dynamic Geocoding App Instructions", 
      easy_close = TRUE, 
      span('1) Click the "Connect to Database" button below', br(),
           '2) Observe the new example output "Geocoding Summary" statistics on the main landing page', br(),
           '3) Toggle the "Show Map" button on the sidebar to dynamically edit the geocoding entries.', br(),
           actionButton("db_connect", "Connect to Database")
           )
    ) 
  )
  
  # and observe in case the user needs to access the instructions again
  observe({ 
    showModal( 
      modalDialog( 
        title = "Dynamic Geocoding App Instructions", 
        easy_close = TRUE, 
        span('1) Click the "Connect to Database" button below (this will wipe any data changes)', br(),
             '2) Observe the new example output "Geocoding Summary" statistics on the main landing page', br(),
             '3) Toggle the "Show Map" button on the sidebar to dynamically edit the geocoding entries.', br(),
             actionButton("db_connect", "Connect to Database")
        )
      ) 
    ) 
  }) |> 
    bindEvent(input$show) 
  
  # set empty reactive values object for db connection string
  connec <- reactiveValues()
  
  # connect to db
  observeEvent(input$db_connect, {
    # spinner to let user know db connection is processing
    show_modal_spinner(spin = "atom", color = "lightblue", text = "Connecting to Database")
    
    tryCatch({
      print("Connecting to Database...")
      connec$connec <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      print("Database Connected!")
    }, error=function(cond) {
      print("Unable to connect to Database")
    })
    
    # terminate spinner
    remove_modal_spinner()
  })
  
  # set empty reactive values object for data
  harmonized_geodata <- reactiveValues()
  
  # populate with example df by default (to change in future)
  observeEvent(input$db_connect, {
    req(connec$connec)
    dbWriteTable(connec$connec, "ashe", ashe)
    dbWriteTable(connec$connec, "county_carto", county_carto)
    tryCatch(
      expr = {
        harmonized_geodata$data <- st_read(connec$connec, query = glue("SELECT * FROM ashe")) %>%  
          st_drop_geometry()
        message("Successfully downloaded dataset from ashe")
      },
      error = function(e){
        message('Download error!')
        print(e)
      },
      warning = function(w){
        message('Download warning!')
        print(w)
      }
    ) 
  })
  
  # set empty reactive values object for boundary data
  bounds <- reactiveValues()
  
  # dynamically update bounds df depending on state selected 
  observeEvent(input$state, {
    req(connec$connec)
    tryCatch(
      expr = {
        bounds$current <- st_read(connec$connec, query = glue("SELECT * FROM county_carto WHERE state_fips = '{input$state}'"))
      },
      error = function(e){
        message('State change error!')
        showNotification(e, type = "error", duration = NULL)
        print(e)
      },
      warning = function(w){
        message('State change warning!')
        showNotification(w, type = "warning", duration = NULL)
        print(w)
      }
    )
  })
  
  # dynamically updated county name/number for app title
  output$county_name <- renderPrint({HTML(paste0("Geocoded Data in ", filtered_county()$county_name, " County"))})
  
  # set an empty reactive values object to hold edited data
  geodata <- reactiveValues()
  
  output$select_state_ui <- renderUI({
    selectInput("state",
                "Choose State:",
                choices= unique(harmonized_geodata$data$state_fips),
                selected = unique(harmonized_geodata$data$state_fips)[1]
    )
  })
  
  output$select_county_ui <- renderUI({
    selectInput("county",
                "Choose County:",
                choices = sort(bounds$current$county_fips)
    )
  })
  
  output$select_confidence_ui <- renderUI({
    selectizeInput("confidence",
                   "Confidence Level:",
                   choices = harmonized_geodata$data$confidence,
                   selected = c("high", "unreliable", "low", "medium"),
                   multiple=TRUE 
                   # selectize=TRUE
    )
  })
  
  filtered_county <- reactive({
    req(bounds$current)
    # take our df of all the county shapefiles
    bounds$current %>%
      # and filter to only the state/county selected by the user
      filter(state_fips == input$state & county_fips == input$county)
  })
  
  # when any of the data inputs are changed, set the reactive values object to reflect this
  observeEvent(ignoreInit = TRUE, list(input$state, input$county, input$confidence), {
    req(harmonized_geodata$data)
    
    geodata$table_data <- harmonized_geodata$data %>% 
      filter(state_fips == input$state & county_fips == input$county & confidence %in% input$confidence)
  })
  
  # render table
  output$table <- renderDataTable({
    req(geodata$table_data)
    DT::datatable(
      geodata$table_data %>% 
        filter(state_fips == input$state & county_fips == input$county), 
      selection = list(mode = "single"),
      extensions = c('Buttons', 'RowGroup', 'FixedHeader', 'Scroller'),
      # editable = TRUE,
      options = list(
        fixedHeader = TRUE,
        scrollX = T,
        scrollY = "550px",
        dom = "ft", 
        pageLength = nrow(geodata$table_data),
        # hide specific columns from DT output in UI, but preserve them on back end to filter
        columnDefs = list(
          list(
            visible = FALSE,
            targets = c("state_fips", "county_fips", "status", "id")
          )
        )
      )
    )
  })
  
  # define table proxy for hiding functionality
  proxy_table <- DT::dataTableProxy('table')
  
  # when user clicks on hide, remove all edited points that have been marked as "fixed" from map and table
  observeEvent(input$hide, {
    req(geodata$table_data)
    if (input$hide == T) {
      proxy_table %>% 
        DT::replaceData(geodata$table_data %>% 
                          filter(state_fips == input$state & county_fips == input$county & confidence != "fixed"))
      
      leafletProxy("map01") %>% hideGroup("fixed")
      # and when the user un-clicks hide, repopulate 
    } else {
      proxy_table %>% 
        DT::replaceData(geodata$table_data %>% 
                          filter(state_fips == input$state & county_fips == input$county))
      
      leafletProxy("map01") %>% showGroup("fixed")
    }
  })
  
  # save button 
  observeEvent(input$save, {
    
    if(is.null(input$table_rows_selected))
      showModal(modalDialog(
        "Missing selected row of dataframe to update",
        easyClose = TRUE,
        footer = NULL
      )) else if(input$coords == '')
        showModal(modalDialog(
          "Missing new coordinates to plot",
          easyClose = TRUE,
          footer = NULL
        )) else {
          
          # update the lat/lon of the selected row/point
          geodata$table_data[[9]][[input$table_rows_selected]] <<- new_coords()$lat
          geodata$table_data[[10]][[input$table_rows_selected]] <<- new_coords()$lng
          
          # add an edit note to track changes
          geodata$table_data[[11]][[input$table_rows_selected]] <<- "T"
          
          # re-intersect coordinates with county polygon for color change
          row_selected <- geodata$table_data[input$table_rows_selected,]
          row_selected <- row_selected %>% mutate(status = if_else(st_intersects(st_as_sf(., coords = c("lon", "lat"), crs = st_crs(4326)), filtered_county(), sparse = FALSE), "green", "red" ))
          geodata$table_data[[8]][[input$table_rows_selected]] <<- row_selected$status
          
          # change the edited row's icon to indicate it has been fixed
          geodata$table_data[[7]][[input$table_rows_selected]] <<- "fixed"
          
          # convert reactiveValues to dataframe
          x <- as.data.frame(geodata$table_data[input$table_rows_selected,])
          
          # spinner to let user know file is saving
          show_modal_spinner(spin = "atom", color = "green", text = "Saving Edited Table")
          
          # define DB query with placeholder positional indexes
          update <- dbSendQuery(connec$connec, "UPDATE ashe SET lon=?, lat=?, confidence=?, edited=?, status=? WHERE id=?")
          
          # bind query with actual variables to update
          dbBind(update,
                 list(
                   x$lon,
                   x$lat,
                   x$confidence,
                   x$edited,
                   x$status,
                   x$id
                 )
          )
          
          # have to flush the query for next round
          dbClearResult(update)
          
          # terminate spinner
          remove_modal_spinner()
          
          # give user a success message
          showNotification(paste0("Successfully updated data for STATE: ", input$state, " COUNTY: ", input$county, " NAME: ", x$name, " ADDRESS: ", x$address, " ID: ", x$id), duration = 10, type = "default")
          
          # send message to console
          message(glue("Successfully updated data for state: {input$state} county: {input$county} name: {x$name} id: {x$id}."))
        }
  })
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  
  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  
  # when a row is selected in the table
  observeEvent(input$table_rows_selected, {
    # for some reason this successfully autofocuses to the selected marker...
    selectedRow <- input$table_rows_selected
    session$sendCustomMessage("scrollToSelectedRow", selectedRow)
    
    # use the selected row id to replace the map marker with the flag icon
    row_selected <- geodata$table_data[input$table_rows_selected,]
    proxy <- leafletProxy('map01')
    proxy %>%
      addAwesomeMarkers(
        layerId = as.character(row_selected$id),
        lng = row_selected$lon, 
        lat = row_selected$lat,
        icon = my_icon)
    
    # use prev_row() to keep track of how to add the icon back in after a new point is clicked
    if(!is.null(prev_row())){ proxy %>%
        addMarkers(
          data = prev_row(),
          layerId = as.character(prev_row()$id),
          lng = prev_row()$lon,
          lat = prev_row()$lat,
          icon = ~favicons[confidence],
          group = prev_row()$confidence,
          label = paste0(
            strong("Raw Address: "), as.character(prev_row()$address),'<br>',
            strong("Reference Address: "), as.character(prev_row()$matched_address)
          ) %>% lapply(htmltools::HTML)
        )
    }
    
    # set new value to reactiveVal 
    prev_row(row_selected)
  })
  
  # highlight row in df when clicking a marker on the map
  observeEvent(input$map01_marker_click, {
    clickId <- input$map01_marker_click$id
    dataTableProxy("table") %>%
      selectRows(which(geodata$table_data$id == clickId)) %>%
      selectPage(which(input$table_rows_all == clickId) %/% input$table_state$length + 1)
  })
  
  # read in user-input coordinates and parse Google's format
  new_coords <- reactive({
    req(input$coords)
    
    input$coords %>% 
      # split the new coordinates by the comma separator
      str_split(",", simplify = T) %>% 
      # convert to a df
      as.data.frame %>% 
      # rename columns
      rename(lat = V1, lng = V2) %>% 
      # convert to numeric
      mutate(lat = as.numeric(lat), lng = as.numeric(lng))
  })
  
  # when new points button is clicked, create a proxy of the map and append these new points
  observeEvent(input$plot_test_points, {
    
    if(input$coords == '')
      showModal(modalDialog(
        "Missing new coordinates to plot",
        easyClose = TRUE,
        footer = NULL
      ))
    
    leafletProxy("map01") %>%
      addMarkers(data = new_coords(),
                 icon = awesomeIcons(
                   icon = 'ios-close',
                   iconColor = 'black'),
                 # the key here is placing all new points in a new group
                 group = "new")
  })
  
  # when the clear points button is clicked, clear the proxy map of these new points
  observeEvent(input$remove_test_points, {
    
    if(input$coords == '')
      showModal(modalDialog(
        "Missing new coordinates to remove",
        easyClose = TRUE,
        footer = NULL
      ))
    
    leafletProxy("map01") %>%
      # now we only have to remove the group, rather than each point
      clearGroup("new")
  })
  
  # intersection error summary output
  output$outside_county <- renderDataTable({
    req(harmonized_geodata$data)
    DT::datatable(
      harmonized_geodata$data %>%
        group_by(state_fips, county_fips) %>%
        filter(status == "red") %>%
        summarize(intersection_errors = n()), 
      selection = list(mode = "single"),
      extensions = c('Scroller'),
      options = list(
        scrollY = "450px",
        dom = "ft", 
        pageLength = nrow(harmonized_geodata$data)
      ),
      colnames=c("State FIPS", "County FIPS", "Intersection Errors"),
      caption = "Geocoded Intersection Errors"
    )
  })
  
  # low score summary output
  output$low_score <- renderDataTable({
    req(harmonized_geodata$data)
    DT::datatable(
      harmonized_geodata$data %>%
        group_by(state_fips, county_fips) %>%
        filter(confidence %in% c("unreliable", "low", "medium")) %>%
        summarize(low_confidence = n()), 
      selection = list(mode = "single"),
      extensions = c('Scroller'),
      options = list(
        scrollY = "450px",
        dom = "ft", 
        pageLength = nrow(harmonized_geodata$data)
      ),
      colnames=c("State FIPS", "County FIPS", "Low-reliability Scores"),
      caption = "Potential Low-reliability Errors"
    )
  })
  
  # define list of emoji icons for map marker confidence levels
  favicons <- iconList(
    "unreliable" = makeIcon(
      iconUrl = "face-sad-cry-regular.svg",
      iconWidth = 25,
      iconHeight = 25
    ),
    "low" = makeIcon(
      iconUrl = "face-frown-regular.svg",
      iconWidth = 25,
      iconHeight = 25
    ),
    "medium" = makeIcon(
      iconUrl = "face-meh-regular.svg",
      iconWidth = 25,
      iconHeight = 25
    ),
    "high" = makeIcon(
      iconUrl = "face-smile-regular.svg",
      iconWidth = 25,
      iconHeight = 25
    ),
    "exact" = makeIcon(
      iconUrl = "face-grin-stars-regular.svg",
      iconWidth = 25,
      iconHeight = 25
    ),
    "fixed" = makeIcon(
      iconUrl = "thumbs-up-regular.svg",
      iconWidth = 25,
      iconHeight = 25
    )
  )
  
  # render map
  output$map01 <- renderLeaflet({
    req(bounds$current)
    req(geodata$table_data)
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = filtered_county(), 
                  stroke = TRUE, 
                  color = "black", 
                  weight="", 
                  smoothFactor = 0.95,
                  fillOpacity = 0.25,
                  fillColor = "lightblue") %>%
      addCircleMarkers(data = geodata$table_data,
                       color = geodata$table_data$status,
                       group = geodata$table_data$confidence
      ) %>%
      addMarkers(
        data = geodata$table_data,
        layerId = as.character(geodata$table_data$id),
        icon = ~favicons[confidence],
        group = geodata$table_data$confidence,
        label = paste0(
          strong("Raw Address: "), as.character(geodata$table_data$address),'<br>',
          strong("Reference Address: "), as.character(geodata$table_data$matched_address)
        ) %>% lapply(htmltools::HTML)
      )
  })
  
  # automatically disconnect from db on closing the app
  session$onSessionEnded(function() { 
    observe({
      if(!is.null(connec$connec)){
        RPostgreSQL::dbDisconnect(connec$connec)
      }
    })
    print("DB Connection Closed")
  })
}

shinyApp(ui, server)