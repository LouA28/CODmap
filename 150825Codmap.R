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
      
      /* Dropdown box: white background with blue border */
     .filter-box .selectize-control.single .selectize-input {
    background-color: white !important;
    color: black;
    box-shadow: none !important;          /* remove blue glow */
    }

   /* Remove blue glow specifically when clicking into the box */
   .filter-box .selectize-control.single .selectize-input.focus {
    border: 2px solid #3c7543 !important;
    box-shadow: none !important;
   }
  /* Placeholder text color */
  .filter-box .selectize-control.single .selectize-input::placeholder {
    color: #e0e0e0;
   }

  /* Dropdown options hover effect */
  .selectize-dropdown-content .option:hover {
    background-color: #3c7543 !important; /* green hover */
    color: white !important;
  }

  /* Currently selected option in dropdown */
  .selectize-dropdown-content .option.active {
    background-color: #3c7543 !important; /* green background */
    color: white !important;
  }
  
  /* Currently selected option (remove blue, set green) */
  .selectize-dropdown-content .option.selected {
    background-color: #3c7543 !important; /* green */
    color: white !important;
  }
   
   /* Custom scrollbar for info box container */
div[style*='overflow-y: auto']::-webkit-scrollbar {
  width: 8px;
}

div[style*='overflow-y: auto']::-webkit-scrollbar-track {
  background: #f1f1f1;
  border-radius: 4px;
}

div[style*='overflow-y: auto']::-webkit-scrollbar-thumb {
  background: #3c7543;
  border-radius: 4px;
}

div[style*='overflow-y: auto']::-webkit-scrollbar-thumb:hover {
  background: #2d5a32;
}

/* For Firefox */
div[style*='overflow-y: auto'] {
  scrollbar-width: thin;
  scrollbar-color: #3c7543 #f1f1f1;
}

/* Ensure smooth scrolling */
div[style*='overflow-y: auto'] {
  scroll-behavior: smooth;
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
    
    tabPanel("filters",
             fluidRow(
               column(
                 width = 12,
                 div(class = "filter-container",
                     div(class = "filter-box",
                         selectInput("country_filter", "Select Country of Origin:",
                                     choices = country_choices, selected = NULL),
                         selectInput("year_filter", "Select Year:", choices = NULL),
                         selectInput("chapter_filter", "Select Chapter (HS2):", choices = NULL),
                         selectInput("cn8_code", "Select CN8 Code:", choices = NULL),
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
                     style = "flex: 1.5; max-width: 400px; padding: 10px; display: flex; flex-direction: column; gap: 20px;",
                     uiOutput("extra_info_box"),
                     # Add scrollable container for info boxes
                     div(
                       style = "flex: 1; overflow-y: auto; overflow-x: hidden; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background: #f9f9f9;",
                       uiOutput("info_box")
                     )
                   ),
                   div(
                     style = "flex: 3; position: relative;",
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
  observe({
    updateTabsetPanel(session, "main_tabs", selected = "filters")
    hide("loading-spinner")
  })
  
  # Update chapter choices when country changes
  observeEvent(input$country_filter, {
    filtered_years <- years_by_country %>%
      filter(country_origin == input$country_filter) %>%
      pull(years) %>%
      unique()
    
    updateSelectInput(session, "year_filter",
                      choices = as.character(unlist(filtered_years)),
                      selected = NULL)
    
    # Reset downstream
    updateSelectInput(session, "chapter_filter", choices = NULL)
    updateSelectInput(session, "cn8_code", choices = NULL)
  })
  
  # 2. Update chapters when country + year selected
  observeEvent(c(input$country_filter, input$year_filter), {
    req(input$year_filter)
    
    filtered_chapters <- chapters_by_country_year %>%
      filter(country_origin == input$country_filter,
             Year == input$year_filter) %>%
      pull(chapters) %>%
      unique()
    
    updateSelectInput(session, "chapter_filter",
                      choices = as.character(unlist(filtered_chapters)),
                      selected = NULL)
    
    # Reset CN8
    updateSelectInput(session, "cn8_code", choices = NULL)
  })
  
  # 3. Update CN8 when country + year + chapter selected
  observeEvent(c(input$country_filter, input$year_filter, input$chapter_filter), {
    req(input$chapter_filter)
    
    filtered_cn8_codes <- cn8_by_country_year_chapter %>%
      filter(country_origin == input$country_filter,
             Year == input$year_filter,
             HS2 == input$chapter_filter) %>%
      pull(cn8) %>%
      unique()
    
    updateSelectInput(session, "cn8_code",
                      choices = as.character(unlist(filtered_cn8_codes)),
                      selected = NULL)
  })
  
  # 4. Enable show map button
  observe({
    is_ready <- !is.null(input$country_filter) &&
      !is.null(input$year_filter) &&
      !is.null(input$chapter_filter) &&
      !is.null(input$cn8_code)
    
    if (is_ready) enable("show_map") else disable("show_map")
  })
  
  # 5. Collect filters only when button clicked
  selected_filters <- eventReactive(input$show_map, {
    list(
      country = input$country_filter,
      year    = input$year_filter,
      chapter = input$chapter_filter,
      cn8     = input$cn8_code
    )
  })
  
  filtered_data <- reactive({
    f <- selected_filters()
    req(f)
    show("loading-spinner")
    
    data <- cod_data %>%
      filter(
        CN8 == f$cn8,
        country_origin == f$country,
        Year == f$year
      ) %>%
      na.omit()
    
    if (nrow(data) == 0) {
      hide("loading-spinner")
      return(NULL)
    }
    
    result <- data %>%
      mutate(
        combocode_clean = tolower(trimws(ifelse(is.na(combocode), "", combocode))),
        
        # eligible starts
        eligible_start    = grepl("^e[235]", combocode_clean),     # e2/e3/e5
        noneligible_start = grepl("^e1", combocode_clean),         # e1
        
        # endings
        end_use     = grepl("u(20|21|30|31)$", combocode_clean),
        end_nonuse  = grepl("u(10|11)$", combocode_clean),
        
        # classify route types
        route_type = dplyr::case_when(
          eligible_start & end_use       ~ "use",
          eligible_start & end_nonuse    ~ "non_use",
          noneligible_start & end_nonuse ~ "non_eligible",
          TRUE                           ~ NA_character_
        ),
        
        route_id = paste0(cooalpha, "_", codalpha, "_GB"),
        
        # Separate preferential and total trade values
        pref_trade = ifelse(route_type == "use", Total_imp, 0),
        eligible_trade = ifelse(route_type %in% c("use", "non_use"), Total_imp, 0)
      ) %>%
      filter(!is.na(route_type)) %>%
      group_by(
        route_id, lat1, lon1, lat2, lon2, lat3, lon3,
        cooalpha, codalpha, ukalpha, country_origin,
        country_dispatch, country_destination, CN8, CN8_desc, route_type
      ) %>%
      summarise(
        route_import = sum(Total_imp, na.rm = TRUE),
        pref_import = sum(pref_trade, na.rm = TRUE),
        eligible_import = sum(eligible_trade, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        route = paste(country_origin, country_dispatch, "UK", sep = " → ")
      )
    
    return(result)
  })
  
  
  output$info_box <- renderUI({
    data <- filtered_data()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    cn8_desc <- data %>% pull(CN8_desc) %>% unique()
    
    # Calculate overall statistics
    total_eligible_trade <- sum(data$eligible_import[data$route_type %in% c("use", "non_use")], na.rm = TRUE)
    total_pref_trade <- sum(data$pref_import[data$route_type == "use"], na.rm = TRUE)
    total_non_eligible <- sum(data$route_import[data$route_type == "non_eligible"], na.rm = TRUE)
    
    # Calculate overall PUR (Preference Utilisation Rate) - changed z to s
    overall_pur <- if(total_eligible_trade > 0) {
      (total_pref_trade / total_eligible_trade) * 100
    } else 0
    
    unused_pref_trade <- total_eligible_trade - total_pref_trade
    unused_pur <- if(total_eligible_trade > 0) {
      (unused_pref_trade / total_eligible_trade) * 100
    } else 0
    
    # Vectorised function to format values - changed z to s, and m/k to lowercase
    format_millions <- function(values) {
      sapply(values, function(value) {
        if (is.na(value) || value == 0) {
          return("£0")
        } else if (value >= 1000000) {
          return(paste0("£", sprintf("%.1f", value / 1000000), "m"))
        } else if (value >= 1000) {
          return(paste0("£", sprintf("%.0f", value / 1000), "k"))
        } else {
          return(paste0("£", format(round(value), big.mark = ",")))
        }
      })
    }
    
    # Function to format large values for totals (also with lowercase m/k)
    format_total <- function(value) {
      if (is.na(value) || value == 0) {
        return("£0")
      } else if (value >= 1000000) {
        return(paste0("£", sprintf("%.1f", value / 1000000), "m"))
      } else if (value >= 1000) {
        return(paste0("£", sprintf("%.0f", value / 1000), "k"))
      } else {
        return(paste0("£", format(round(value), big.mark = ",")))
      }
    }
    
    # Prepare eligible routes data (combine used and non-used by route)
    eligible_routes <- data %>%
      filter(route_type %in% c("use", "non_use")) %>%
      group_by(route) %>%
      summarise(
        used_trade = sum(ifelse(route_type == "use", route_import, 0), na.rm = TRUE),
        unused_trade = sum(ifelse(route_type == "non_use", route_import, 0), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        used_formatted = format_millions(used_trade),
        unused_formatted = format_millions(unused_trade),
        display = paste0(
          "<div style='margin-bottom: 8px; line-height: 1.4;'>",
          "<strong>", route, ":</strong><br>",
          # Show used trade if > 0
          ifelse(used_trade > 0, 
                 paste0("&nbsp;&nbsp;", used_formatted, " <span style='color:#3c7543; font-weight: bold;'>(used)</span>"), 
                 ""),
          # Add line break if both exist
          ifelse(used_trade > 0 & unused_trade > 0, "<br>", ""),
          # Show unused trade if > 0
          ifelse(unused_trade > 0, 
                 paste0("&nbsp;&nbsp;", unused_formatted, " <span style='color:#cc6666; font-weight: bold;'>(not used)</span>"), 
                 ""),
          "</div>"
        )
      ) %>%
      pull(display)
    
    # Prepare non-eligible routes with better formatting AND millions/thousands conversion
    non_eligible_routes <- data %>%
      filter(route_type == "non_eligible") %>%
      mutate(
        formatted_value = format_millions(route_import), # Apply the same formatting
        display = paste0(
          "<div style='margin-bottom: 8px; line-height: 1.4;'>",
          "<strong>", route, ":</strong><br>",
          "&nbsp;&nbsp;", formatted_value, # Use formatted value instead of raw format
          "</div>"
        )
      ) %>%
      pull(display)
    
    HTML(paste0(
      "<div style='font-family: Segoe UI, sans-serif; font-size: 14px; line-height: 1.5;'>",
      
      # Header information box - increased padding
      "<div style='background: #fff; border: 2px solid #3c7543; border-radius: 8px; padding: 18px; margin-bottom: 15px;'>",
      "<div style='text-align: center; margin-bottom: 12px;'>",
      "<strong style='color: #3c7543; font-size: 17px;'>", input$cn8_code, "</strong>",
      "</div>",
      "<div style='text-align: center; font-size: 12px; color: #666; margin-bottom: 12px; line-height: 1.3;'>",
      ifelse(length(cn8_desc) > 0, cn8_desc, "Unknown description"),
      "</div>",
      "<div style='text-align: center; font-size: 13px; color: #555;'>",
      "<strong>Origin:</strong> ", input$country_filter, "<br><strong>Year:</strong> ", input$year_filter,
      "</div>",
      "</div>",
      
      # Combined Preference Utilisation Summary Box - changed z to s, and formatted total
      if(total_eligible_trade > 0) paste0(
        "<div style='border: 2px solid #3c7543; border-radius: 8px; padding: 22px; margin-bottom: 15px; background: #fff;'>",
        
        # Title
        "<div style='text-align: center; margin-bottom: 22px;'>",
        "<strong style='color: #3c7543; font-size: 17px;'>PREFERENCE UTILISATION SUMMARY</strong>",
        "</div>",
        
        # Two-column layout for Used vs Unused - better spacing
        "<div style='display: flex; justify-content: space-between; margin-bottom: 22px; gap: 20px;'>",
        
        # Preferences Used column
        "<div style='text-align: center; flex: 1; padding: 0 5px;'>",
        "<div style='font-size: 32px; font-weight: bold; color: #3c7543; margin-bottom: 10px;'>",
        sprintf("%.1f%%", overall_pur),
        "</div>",
        "<div style='font-size: 13px; font-weight: bold; color: #666; margin-bottom: 6px;'>",
        "PREFERENCES USED",
        "</div>",
        "<div style='font-size: 12px; color: #888;'>",
        format_total(total_pref_trade), # Now formatted
        "</div>",
        "</div>",
        
        # Preferences Unused column
        "<div style='text-align: center; flex: 1; padding: 0 5px;'>",
        "<div style='font-size: 32px; font-weight: bold; color: #cc6666; margin-bottom: 10px;'>",
        sprintf("%.1f%%", unused_pur),
        "</div>",
        "<div style='font-size: 13px; font-weight: bold; color: #666; margin-bottom: 6px;'>",
        "PREFERENCES UNUSED",
        "</div>",
        "<div style='font-size: 12px; color: #888;'>",
        format_total(unused_pref_trade), # Now formatted
        "</div>",
        "</div>",
        
        "</div>",
        
        # Total Eligible Trade at bottom - now formatted
        "<div style='text-align: center; padding: 14px; background: rgba(60, 117, 67, 0.1); border-radius: 6px;'>",
        "<strong style='color: #3c7543; font-size: 15px;'>Total Eligible Trade: ", format_total(total_eligible_trade), "</strong>",
        "</div>",
        
        "</div>"
      ) else "",
      
      # Combined Eligible Routes - updated text with lowercase m=millions, k=thousands
      if(length(eligible_routes) > 0) paste0(
        "<div style='border: 2px solid #3c7543; border-radius: 8px; padding: 18px; margin-bottom: 15px; background: #fff;'>",
        "<div style='margin-bottom: 12px;'>",
        "<i class='fa fa-route' style='color: #3c7543; margin-right: 8px;'></i>",
        "<strong style='color: #3c7543; font-size: 15px;'>Eligible Routes</strong>",
        "</div>",
        "<div style='font-size: 12px; color: #666; margin-bottom: 15px; line-height: 1.4;'>",
        "<em>Trade values by route (m=millions, k=thousands):</em><br>",
        "<span style='color: #3c7543; font-weight: bold;'>(used)</span> - preferences claimed | ",
        "<span style='color: #cc6666; font-weight: bold;'>(not used)</span> - available but not claimed",
        "</div>",
        paste(eligible_routes, collapse = ""),
        "</div>"
      ) else "",
      
      # Non-eligible routes - NOW WITH SAME FORMATTING AS ELIGIBLE ROUTES
      if(length(non_eligible_routes) > 0) paste0(
        "<div style='border: 2px solid #888; border-radius: 8px; padding: 18px; background: #fff;'>",
        "<div style='margin-bottom: 12px;'>",
        "<i class='fa fa-times-circle' style='color: #888; margin-right: 8px;'></i>",
        "<strong style='color: #666; font-size: 15px;'>Non-Eligible Routes</strong>",
        "</div>",
        "<div style='font-size: 12px; color: #666; margin-bottom: 15px;'>",
        "<em>Trade values where no preferences were available (m=millions, k=thousands):</em>", # Updated to mention formatting
        "</div>",
        paste(non_eligible_routes, collapse = ""),
        "</div>"
      ) else "",
      
      "</div>"
    ))
  })
  
  output$extra_info_box <- renderUI({
    req(filtered_data())
    
    HTML(paste0(
      # Toggle button with better text
      "<button id='toggleInfoBtn' onclick='toggleInfoBox()' 
        style='margin-top:10px; background-color:#3c7543; color:white; border:none; padding:8px 14px; border-radius:6px; cursor:pointer; font-size:12px; font-weight:bold;'>
        Data Descriptions & Definitions
      </button>",
      
      # Collapsible info box (initially hidden)
      "<div id='logisticsInfoBox' style='display:none; border: 1px solid #ddd; padding: 12px 15px; margin-top: 10px; margin-bottom: 20px;
                background-color: #fffdee; border-radius: 8px;
                font-family: Segoe UI, sans-serif; font-size: 13px; line-height: 1.5;'>",
      
      "<strong style='color:#3c7543;'>Important Notes:</strong><br><br>",
      
      "Routes are shown as straight lines and may not reflect actual transport paths.<br><br>",
      
      "There may be overlaps between country of origin & country of dispatch on the map, as the country of origin can also be country of dispatch.<br><br>",
      
      "COO <strong>(Country of Origin)</strong>: represents the country where a product comes from<br>",
      "COD <strong>(Country of Dispatch)</strong>: the country where the last commercial transaction took place<br><br>",
      
      "<strong>Data caveats:</strong> This view helps understand logistics-PUR interactions, not meant to replace existing tools.<br><br>",
      
      "<strong>Source:</strong> UK import PUR data 2022–2025 (HMRC),<br><br> <strong>Last Updated:</strong> August 2025<br><br>",
      
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
        lon2_adj = lon2 + offset_deg,
        # Calculate route-level PUR for display
        route_pur = ifelse(eligible_import > 0, 
                           sprintf("%.1f%%", (pref_import / eligible_import) * 100), 
                           "N/A")
      )
    
    # Prepare small distinct datasets for markers (keep adjusted coordinates)
    coo_pts <- data %>% 
      distinct(cooalpha, .keep_all = TRUE) %>% 
      select(cooalpha, lon1, lat1, country_origin, route_pur, eligible_import)
    
    cod_pts <- data %>% 
      distinct(codalpha, .keep_all = TRUE) %>% 
      select(codalpha, lon2_adj, lat2_adj, country_dispatch)
    
    uk_pts <- data %>% 
      distinct(ukalpha, .keep_all = TRUE) %>% 
      select(ukalpha, lon3, lat3)
    
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
        label = ~HTML(paste0("COO: ", country_origin, 
                             "<br>Eligible Trade: £", format(eligible_import, big.mark = ","),
                             "<br>PUR Rate: ", route_pur))
      ) %>%
      addAwesomeMarkers(
        data = cod_pts,
        lng = ~lon2_adj, lat = ~lat2_adj,
        icon = awesomeIcons(icon = "truck", markerColor = "blue", iconColor = "white", library = "fa"),
        label = ~paste("Dispatch Country:", country_dispatch) 
      ) %>%
      addAwesomeMarkers(
        data = uk_pts,
        lng = ~lon3, lat = ~lat3,
        icon = awesomeIcons(icon = "star", markerColor = "purple"),
        label = "Final Destination: UK"
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