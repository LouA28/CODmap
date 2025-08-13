library(shiny)
library(dplyr)
library(leaflet)
library(leafgl)
library(shinycssloaders)
library(shinyjs)

# Load preprocessed data
cod_data <- readRDS("CODdata/cod_data.RDS")
country_choices <- readRDS("CODdata/country_choices.RDS")
years_by_country <- readRDS("CODdata/years_by_country.RDS")
chapters_by_country_year <- readRDS("CODdata/chapters_by_country_year.RDS")
cn8_by_country_year_chapter <- readRDS("CODdata/cn8_by_country_year_chapter.RDS")

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$title("Origin-to-UK Tracker"),
    tags$style(HTML("
    /* Full-screen spinner overlay */
    #loading-spinner {
      position: fixed;
      top: 0; left: 0;
      width: 100%; height: 100%;
      background-color: rgba(255, 255, 255, 0.85);
      z-index: 9999;
      display: flex;
      justify-content: center;
      align-items: center;
      flex-direction: column;
    }

    /* Circular spinner */
    .spinner {
      border: 6px solid #f3f3f3;
      border-top: 6px solid #3c7543;
      border-radius: 50%;
      width: 60px;
      height: 60px;
      animation: spin 1s linear infinite;
      margin-bottom: 10px;
    }

    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }

    /* Center filter page content */
    .filter-container {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 80vh;
      text-align: center;
    }

    .filter-box {
      width: 400px;
      padding: 20px;
      border: 1px solid #ddd;
      background: white;
      border-radius: 10px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    }

    /* Center labels and dropdowns */
    .filter-box .shiny-input-container {
      width: 100%;
      text-align: center;
    }

    .filter-box label {
      display: block;
      text-align: center;
      font-weight: bold;
    }

    .filter-box select {
      text-align: center;
    }
    
     .btn-primary {
        background-color: #3c7543;
        color: white;
        border: none;
        font-weight: bold;
        padding: 8px 16px;
        border-radius: 6px; /* Rounded corners */
        cursor: pointer;
      }
      .btn-primary:hover {
        background-color: #2f5c35; /* Slightly darker green on hover */
      }
  "))
  ),
  
  div(id = "loading-spinner",
      div(class = "spinner"),
      div(id = "loading-text", "Loading map, please wait..."),
      style = "display:none;"
  ),
  
  titlePanel(
    tags$div(
      style = "
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; 
      text-align: center;
      user-select: none;
    ",
      tags$h1(
        "Origin-to-UK Tracker",
        style = "
        font-weight: 700; 
        font-size: 2rem; 
        letter-spacing: 1px;
        color: #3c7543; 
        margin: 0;
      "
      )
    )
  ),
  
  tabsetPanel(
    id = "main_tabs",
    type = "hidden",
    
    # FILTER PAGE
    tabPanel("filters",
             fluidRow(
               column(
                 width = 12,
                 div(class = "filter-container",
                     div(class = "filter-box",
                         selectInput("country_filter", "Select Country of Origin:",
                                     choices = country_choices, selected = NULL),
                         selectInput("chapter_filter", "Select Chapter (HS2):", choices = NULL),
                         selectInput("cn8_code", "Select CN8 Code:", choices = NULL),
                         selectInput("year_filter", "Select Year:", choices = NULL),
                         actionButton("show_map", "Show Map", class = "btn-primary", style = "margin-top:10px; width:100%;",disabled = TRUE)
                     )
                 )
               )
             )
    ),
    
    # MAP PAGE
    tabPanel("map",
             fluidRow(
               column(
                 12,
                 actionButton("back_to_filters", "← Back to Filters", class = "btn-secondary", style = "margin: 10px;"),
                 div(
                   style = "display:flex; flex-direction:row; height: calc(100vh - 120px);",
                   div(
                     style = "flex: 1; max-width: 300px; padding: 10px; display: flex; flex-direction: column; gap: 20px;", # <-- added gap here
                     uiOutput("extra_info_box"),
                     uiOutput("info_box")
                   ),
                   div(
                     style = "flex: 4; position: relative;",
                     leafletOutput("map", height = "100%"),
                     absolutePanel(
                       bottom = 10, left = 10, width = "auto", draggable = FALSE,
                       uiOutput("map_legend") # Legend overlay inside map
                     )
                   )
                 )
               )
             )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  updateTabsetPanel(session, "main_tabs", selected = "filters")
  hide("loading-spinner")
  
  # Update chapter choices when country changes
  observeEvent(input$country_filter, {
    filtered_chapters <- chapters_by_country_year %>%
      filter(country_origin == input$country_filter) %>%
      pull(chapters) %>%
      unique()
    
    updateSelectInput(session, "chapter_filter",
                      choices = as.character(unlist(filtered_chapters)),
                      selected = NULL)
  })
  
  # Update CN8 choices when chapter changes
  observeEvent(input$chapter_filter, {
    filtered_cn8_codes <- cn8_by_country_year_chapter %>%
      filter(country_origin == input$country_filter,
             HS2 == input$chapter_filter) %>%
      pull(cn8) %>%
      unique()
    
    updateSelectInput(session, "cn8_code",
                      choices = as.character(unlist(filtered_cn8_codes)),
                      selected = NULL)
  })
  
  # Update year choices when CN8 changes (year is now last)
  observeEvent(input$cn8_code, {
    filtered_years <- years_by_country %>%
      filter(country_origin == input$country_filter) %>%
      pull(years) %>%
      unique()
    
    updateSelectInput(session, "year_filter",
                      choices = as.character(unlist(filtered_years)),
                      selected = NULL)
  })
  
  
  # Enable show map button
  observe({
    is_ready <- !is.null(input$country_filter) && !is.null(input$chapter_filter) && !is.null(input$cn8_code) && !is.null(input$year_filter)
    if (is_ready) enable("show_map") else disable("show_map")
  })
  
  # Filter only when button clicked
  selected_filters <- eventReactive(input$show_map, {
    list(
      country = input$country_filter,
      chapter = input$chapter_filter,
      cn8 = input$cn8_code,
      year = input$year_filter
    )
  })
  
  filtered_data <- reactive({
    f <- selected_filters()
    req(f)
    show("loading-spinner")
    
    data <- cod_data %>% filter(CN8 == f$cn8, country_origin == f$country, Year == f$year) %>% na.omit()
    
    if (nrow(data) == 0) {
      hide("loading-spinner")
      return(NULL)
    }
    
    result <- data %>%
      mutate(
        route_id = paste0(cooalpha, "_", codalpha, "_GB"),
        PUR_numeric = suppressWarnings(as.numeric(PUR)),
        PUR_clean = ifelse(PUR == "Not Eligible", NA, PUR_numeric)
      ) %>%
      group_by(route_id, lat1, lon1, lat2, lon2, lat3, lon3,
               cooalpha, codalpha, ukalpha, country_origin,
               country_dispatch, country_destination, CN8) %>%
      summarise(
        Avg_PUR = mean(PUR_clean, na.rm = TRUE),
        PUR_text = if (all(is.na(PUR_clean))) {
          "Not Eligible"
        } else {
          sprintf("%.2f", mean(PUR_clean, na.rm = TRUE))
        },
        .groups = "drop"
      ) %>%
      mutate(
        route = paste(country_origin, country_dispatch, "UK", sep = " → "),
        PUR = ifelse(PUR_text == "Not Eligible", "Not Eligible", paste0(PUR_text, "%"))
      )
    
    return(result)
  })
  
  loading_messages <- c(
    "Mapping your trade routes…",
    "Generating routes…",
    "Almost there…"
  )
  
  observe({
    invalidateLater(1500, session)  # every 1.5 seconds
    new_text <- sample(loading_messages, 1)
    runjs(sprintf("document.getElementById('loading-text').innerText = '%s';", new_text))
  })
  
  
  observe({
    is_ready <- !is.null(input$country_filter) &&
      !is.null(input$chapter_filter) &&
      !is.null(input$cn8_code) &&
      !is.null(input$year_filter)
    
    if (is_ready) {
      enable("show_map")
    } else {
      disable("show_map")
    }
  })
  
  # Info Box
  output$info_box <- renderUI({
    data <- filtered_data()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    cn8_desc <- cod_data %>%
      filter(CN8 == input$cn8_code) %>%
      pull(CN8_desc) %>%
      unique()
    
    route_PUR_list <- paste0(
      "<strong>", data$route, ":</strong> ",
      ifelse(data$PUR == "Not Eligible", "Not Eligible", data$PUR)
    ) %>% paste(collapse = "<br>")
    
    HTML(paste0(
      "<div style='border: 1px solid #ddd; padding: 12px 15px; background-color: #f9f9f9; 
                 border-radius: 8px; font-family: Segoe UI, sans-serif; font-size: 14px; line-height: 1.6;'>",
      
      "<div style='margin-bottom: 8px;'>
        <strong style='color:#3c7543;'>CN8 Code:</strong> ", input$cn8_code, "<br>
        <strong style='color:#3c7543;'>CN8 Description:</strong> ", ifelse(length(cn8_desc) > 0, cn8_desc, "Unknown"), "<br>
        <strong style='color:#3c7543;'>Country of Origin:</strong> ", input$country_filter, "
      </div>",
      
      "<hr style='border: 0; border-top: 1px solid #ddd; margin: 8px 0;'>",
      
      "<div>
        <strong style='color:#3c7543;'>Routes & PUR Rates:</strong><br>", route_PUR_list, "
      </div>",
      
      "</div>"
    ))
  })
  
  output$extra_info_box <- renderUI({
    req(filtered_data())
    
    HTML(paste0(
      # Toggle button
      "<button id='toggleInfoBtn' onclick='toggleInfoBox()' 
        style='margin-top:10px; background-color:#3c7543; color:white; border:none; padding:6px 10px; border-radius:4px; cursor:pointer;'>
        Show/Hide Logistics Info
    </button>",
      
      # Collapsible info box (initially hidden)
      "<div id='logisticsInfoBox' style='display:none; border: 1px solid #ddd; padding: 12px 15px; margin-top: 10px; margin-bottom: 20px;
                background-color: #fffdee; border-radius: 8px;
                font-family: Segoe UI, sans-serif; font-size: 13px; line-height: 1.5;'>",
      
      "<strong style='color:#3c7543;'>Important Notes:</strong><br><br>",
      
      "Routes are shown as straight lines and may not reflect actual transport paths.<br><br>",
      
      "There may be overlaps between country of origin & country of dispatch on the map, as the country of origin can also be couintry of dispatch.<br><br>",
      
      "<strong>COO:</strong> Country of Origin: represents the country where a product comes from<br>",
      "<strong>COD:</strong> Country of Dispatch: the country where the last commercial transaction took place<br><br>",
      
      "<strong>Data caveats:</strong> This view helps understand logistics-PUR interactions, not meant to replace existing tools.<br><br>",
      
      "<strong>Source:</strong> UK import PUR data 2022–2025 (HMRC),<br><br> <strong>Last Updated:</strong> July 2025<br><br>",
      
      "For details on <strong>COO/COD</strong>, visit the <a href='https://dap-prd2-connect.azure.defra.cloud/country_of_origin/' target='_blank'>Country of origin/dispatch dashboard</a>.<br>",
      "For <strong>PUR details</strong>, explore the <a href='https://dap-prd2-connect.azure.defra.cloud/PUR-app/' target='_blank'>UK Import PUR App</a>.<br>",
      
      "</div>",
      
      # JavaScript to toggle the info box
      "<script>
      function toggleInfoBox() {
        var box = document.getElementById('logisticsInfoBox');
        if (box.style.display === 'none') {
          box.style.display = 'block';
        } else {
          box.style.display = 'none';
        }
      }
    </script>"
    ))
  })
  
  
  # Map legend
  output$map_legend <- renderUI({
    req(filtered_data())
    HTML(paste0(
      "<div style='background: rgba(255,255,255,0.8);
                  padding: 5px 10px;
                  border-radius: 5px;
                  font-size: 14px;
                  display: inline-block;'>",
      "<strong>Legend:</strong><br>",
      "<span style='color: #db992e;'>● Country of Origin</span><br>",
      "<span style='color: #3f99bf;'>● Dispatch Country</span><br>",
      "<span style='color: #b561c2;'>● Final Destination</span>",
      "</div>"
    ))
  })
  
  # Map rendering
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    # Hide spinner only after map update is triggered
    on.exit({
      runjs("setTimeout(function(){ $('#loading-spinner').hide(); }, 300);")
    })
    
    if (is.null(data) || nrow(data) == 0) {
      return(
        leaflet() %>% addProviderTiles(providers$Esri.WorldGrayCanvas)
      )
    }
    
    colors <- c("pink", "black", "grey", "purple", "orange","brown","Cyan","teal","olive", "gold", "darkmagenta","chocolate","orchid","plum","wheat","tan","turquoise")
    data <- data %>%
      filter(!is.na(lon1), !is.na(lat1),
             !is.na(lon2), !is.na(lat2),
             !is.na(lon3), !is.na(lat3)) %>%
      mutate(
        route_number = dense_rank(route_id),
        route_color = colors[((route_number - 1) %% length(colors)) + 1],
        # detect overlap (tolerance can be adjusted)
        origin_dispatch_same = (abs(lat1 - lat2) < 1e-6 & abs(lon1 - lon2) < 1e-6),
        # offset to separate overlapping origin & dispatch markers
        offset_deg = ifelse(origin_dispatch_same, 0.10, 0),
        lat2_adj = lat2 + offset_deg,
        lon2_adj = lon2 + offset_deg
      )
    
    # Prepare small distinct datasets for markers (keep adjusted coordinates)
    coo_pts <- data %>% distinct(cooalpha, .keep_all = TRUE) %>% select(cooalpha, lon1, lat1, PUR, country_origin)
    cod_pts <- data %>% distinct(codalpha, .keep_all = TRUE) %>% select(codalpha, lon2_adj, lat2_adj, country_dispatch)
    uk_pts  <- data %>% distinct(ukalpha,  .keep_all = TRUE) %>% select(ukalpha, lon3, lat3)
    
    map <- leaflet() %>% addProviderTiles(providers$Esri.WorldGrayCanvas)
    
    # draw polylines using adjusted dispatch coords (lon2_adj / lat2_adj)
    for (i in seq_len(nrow(data))) {
      map <- map %>%
        addPolylines(
          lng = c(data$lon1[i], data$lon2_adj[i], data$lon3[i]),
          lat = c(data$lat1[i], data$lat2_adj[i], data$lat3[i]),
          color = "grey",       # default colour for all routes
          weight = 3,
          opacity = 0.8,
          label = paste("Route", data$route_number[i], ":",
                        data$country_origin[i], "→", data$country_dispatch[i], "→ UK"),
          highlightOptions = highlightOptions(
            color = "darkgreen",    # colour when hovered
            weight = 4,          # slightly thicker on hover
            opacity = 1
          )
        )
    }
    
    # add markers (use the adjusted dispatch lat/lon)
    map %>%
      addAwesomeMarkers(
        data = coo_pts,
        lng = ~lon1, lat = ~lat1,
        icon = awesomeIcons(icon = "globe", markerColor = "orange"),
        label = ~HTML(paste0("COO: ", country_origin, "<br>PUR Rate: ", PUR))
      ) %>%
      addAwesomeMarkers(
        data = cod_pts,
        lng = ~lon2_adj, lat = ~lat2_adj,
        icon = awesomeIcons(icon = "truck", markerColor = "blue", iconColor = "white", library = "fa"),
        label = ~paste("Transit Country:", country_dispatch)
      ) %>%
      addAwesomeMarkers(
        data = uk_pts,
        lng = ~lon3, lat = ~lat3,
        icon = awesomeIcons(icon = "star", markerColor = "purple"),
        label = "Final Destination: UK"
      ) %>%
      addLegend(
        position = "bottomright",
        colors = unique(data$route_color),
        labels = paste("Route", unique(data$route_number)),
        title = "Routes"
      )
  })
  
  
  # Show map and go back
  observeEvent(input$show_map, {
    updateTabsetPanel(session, "main_tabs", selected = "map")
  })
  
  observeEvent(input$back_to_filters, {
    updateTabsetPanel(session, "main_tabs", selected = "filters")
  })
}

shinyApp(ui, server)