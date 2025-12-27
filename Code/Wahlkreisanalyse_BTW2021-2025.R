# Laden relevanter Packages -----------------------------------------------

library(shiny)      # Dashboard
library(tidyverse)  # Pipes, Data Cleaning
library(sf)         # Einlesen und Verarbeiten Geodaten
library(leaflet)    # Interaktive Map
library(scales)     # Formatierung %-Zeichen
library(htmltools)  # Tooltip


# -------------------- Datenaufbereitung --------------------

# Daten laden
btw25 <- read_delim("BTW2025.csv", delim = ";", trim_ws = TRUE, skip = 9, 
                    locale = locale(decimal_mark = ","))

wk <- st_read("Geodaten/btw25_geometrie_wahlkreise_shp_geo.shp", quiet = TRUE)

# Projektion ändern
if(is.na(st_crs(wk))) st_crs(wk) <- 4326

bundeslaender_fix <- wk %>%
  group_by(LAND_NAME) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Farben festlegen
partei_farben <- c(
  "AfD" = "#009fe1", "CDU" = "black", "CSU" = "#213651", "SPD" = "#d71f1d",
  "GRÜNE" = "#58ab27", "Die Linke" = "#AA00AA", "FDP" = "#ffed00",
  "BSW" = "#800080", "FREIE WÄHLER" = "#FFA500", "SSW" = "#003d90", "NA" = "#aaabad"
)


# -------------------- Hilfsfunktionen --------------------

# Tooltip Balkendiagramm erstellen

generate_chart_html <- function(df, wk_name, wk_nr) {
  html <- sprintf('<div style="min-width:180px; font-size:14px;">
  <strong>%s</strong><br><span>Wahlkreis %s</span><br><br>', wk_name, wk_nr)
  
  for (i in seq_len(nrow(df))) {
    partei <- df$Partei[i]
    prozent <- df$Anteil[i]
    farbe <- ifelse(is.na(partei_farben[partei]), partei_farben["NA"], partei_farben[partei])
    
    html <- paste0(html, sprintf(
      '<div style="margin: 4px 0;">
          <span style="font-weight:bold;">%s:</span> %.1f%%<br>
          <div style="background:#eee;width:100%%;height:8px;border-radius:4px;">
            <div style="width:%.1f%%;height:8px;background:%s;border-radius:4px;"></div>
          </div></div>', partei, prozent, prozent, farbe))
  }
  paste0(html, "</div>")
}

prepare_staerkste <- function(data, stimme, jahr, wk_shp) {
  prozent_var <- ifelse(jahr == "2025", "Prozent", "VorpProzent")
  parteien_data <- data %>%
    filter(Gebietsart == "Wahlkreis", Stimme == stimme, Gruppenart == "Partei") %>%
    select(Gebietsnummer, Gebietsname, Gruppenname, !!sym(prozent_var)) %>%
    rename(Anteil = !!sym(prozent_var)) %>%
    group_by(Gebietsnummer) %>%
    arrange(desc(Anteil)) %>%
    slice_head(n = 6) %>%
    ungroup() %>%
    rename(Partei = Gruppenname)
  
  tooltip_html <- parteien_data %>%
    group_by(Gebietsnummer) %>%
    summarise(popup_text = generate_chart_html(cur_data(), first(Gebietsname), first(Gebietsnummer)), .groups = "drop")
  
  staerkste <- parteien_data %>%
    group_by(Gebietsnummer) %>%
    slice_max(order_by = Anteil, n = 1, with_ties = FALSE) %>%
    rename(Staerkste = Partei)
  
  wk_shp %>%
    left_join(staerkste, by = c("WKR_NR" = "Gebietsnummer")) %>%
    left_join(tooltip_html, by = c("WKR_NR" = "Gebietsnummer")) %>%
    mutate(Staerkste = factor(Staerkste, levels = names(partei_farben))) %>%
    st_as_sf()
}

prepare_partei <- function(data, stimme, jahr, partei, wk_shp) {
  prozent_var <- ifelse(jahr == "2025", "Prozent", "VorpProzent")
  
  if (partei == "CDU/CSU") {
    results <- data %>% 
      filter(Stimme == stimme, Gruppenname %in% c("CDU", "CSU")) %>%
      select(Gebietsnummer, Gruppenname, !!sym(prozent_var))
    
    map_data <- wk_shp %>%
      left_join(results %>% filter(Gruppenname == "CDU") %>% select(Gebietsnummer, Prozent_CDU = !!sym(prozent_var)), by = c("WKR_NR" = "Gebietsnummer")) %>%
      left_join(results %>% filter(Gruppenname == "CSU") %>% select(Gebietsnummer, Prozent_CSU = !!sym(prozent_var)), by = c("WKR_NR" = "Gebietsnummer")) %>%
      mutate(Prozent = ifelse(LAND_NAME == "Bayern", Prozent_CSU, Prozent_CDU),
             popup_text = paste0("<strong>", WKR_NAME, "</strong><br>Anteil: ", format(round(Prozent, 1), decimal.mark = ","), " %"))
  } else {
    map_data <- wk_shp %>%
      left_join(data %>% filter(Stimme == stimme, Gruppenname == partei) %>% select(Gebietsnummer, Prozent = !!sym(prozent_var)), by = c("WKR_NR" = "Gebietsnummer")) %>%
      mutate(popup_text = paste0("<strong>", WKR_NAME, "</strong><br>Anteil: ", format(round(Prozent, 1), decimal.mark = ","), " %"))
  }
  return(st_as_sf(map_data))
}

# -------------------- UI --------------------

ui <- fluidPage(
  title = "Wahlkreisanalyse: Bundestagswahl 2021 und 2025 im Vergleich",
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap');
      html, body { font-family: 'Roboto', sans-serif; background-color: #f9f9f9; }
      .sidebar-custom { 
        background-color: white; border: 1px solid black; border-radius: 10px; 
        padding: 20px; margin-top: 40px; box-shadow: 0 0 5px rgba(0,0,0,0.05);
      }
      .contact-box { position: fixed; bottom: 20px; left: 20px; background: white; padding: 15px; border-radius: 10px; border: 1px solid #000; z-index: 1000; }
      #wahlkarte { height: calc(100vh - 160px) !important; border-radius: 12px; border: 1px solid black; }
      .info.legend i { opacity: 1 !important; margin-right: 8px !important; }
    "))
  ),
  
  titlePanel("Bundestagswahl 2021 vs. 2025: Ergebnisse nach Wahlkreis"),
  
  div(class = "container-fluid",
      div(class = "row",
          div(class = "col-sm-3",
              div(class = "sidebar-custom",
                  uiOutput("partei_auswahl"),
                  selectInput("stimme", "Stimmenart:", choices = c("Erststimme" = 1, "Zweitstimme" = 2), selected = 2),
                  uiOutput("jahr_auswahl"),
                  hr(),
                  tags$h4("Wahlkreis-Suche", style="margin-top:0; font-weight:bold;"),
                  selectizeInput("suche_wk", "Wahlkreis suchen/auswählen:", 
                                 choices = NULL, 
                                 options = list(placeholder = 'Wahlkreis wählen', allowEmptyOption = TRUE)),
                  
                  div(class = "contact-box",
                      HTML("<strong>Autor: Noah Schulz</strong><br>"),
                      tags$a(href = "https://www.linkedin.com/in/noah-schulz-971031301/", target = "_blank", 
                             icon("linkedin", style="color:#0077b5; font-size:22px; margin-right:15px;")),
                      tags$a(href = "https://github.com/SchulzNoah", target = "_blank", 
                             icon("github", style="color:black; font-size:22px;"))
                  )
              )
          ),
          div(class = "col-sm-9",
              uiOutput("kartentitel"),
              leafletOutput("wahlkarte")
          )
      )
  )
)

# -------------------- Server --------------------

server <- function(input, output, session) {
  
  selected_wkr_id <- reactiveVal(NULL)
  
  output$partei_auswahl <- renderUI({
    parteien <- c("Stärkste Partei", "CDU/CSU", "SPD", "GRÜNE", "AfD", "Die Linke", "FDP", "BSW")
    if (!is.null(input$jahr) && input$jahr == "2021") {
      parteien <- parteien[parteien != "BSW"]
    }
    selectInput("anzeige", "Partei:", choices = parteien, selected = input$anzeige)
  })
  
  output$jahr_auswahl <- renderUI({
    jahre <- c("2025", "2021")
    if (!is.null(input$anzeige) && input$anzeige == "BSW") {
      jahre <- "2025"
    }
    selectInput("jahr", "Jahr:", choices = jahre, selected = input$jahr)
  })
  
  current_map_data <- reactive({
    req(input$anzeige, input$stimme, input$jahr)
    if (input$anzeige == "Stärkste Partei") {
      prepare_staerkste(btw25, as.numeric(input$stimme), input$jahr, wk)
    } else {
      prepare_partei(btw25, as.numeric(input$stimme), input$jahr, input$anzeige, wk)
    }
  })
  
  updateSelectizeInput(session, "suche_wk", 
                       choices = c("Bitte wählen" = "", setNames(wk$WKR_NR, paste(wk$WKR_NR, wk$WKR_NAME))), 
                       server = TRUE)
  
  output$kartentitel <- renderUI({
    req(input$anzeige, input$jahr)
    stimme_txt <- ifelse(input$stimme == 1, "Erststimme", "Zweitstimme")
    anzeige_txt <- ifelse(input$anzeige == "Stärkste Partei", paste0(stimme_txt, "nergebnisse"), paste0(stimme_txt, "nanteil ", input$anzeige))
    HTML(paste0("<h4><strong>Bundestagswahl ", input$jahr, " – ", anzeige_txt, "</strong></h4>"))
  })
  
  output$wahlkarte <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 10.45, lat = 51.16, zoom = 6)
  })
  
  observe({
    map_data <- current_map_data()
    req(map_data)
    
    proxy <- leafletProxy("wahlkarte", data = map_data)
    proxy %>% clearShapes() %>% clearControls()
    
    if (input$anzeige == "Stärkste Partei") {
      pal <- colorFactor(palette = partei_farben, domain = map_data$Staerkste)
      fill_val <- ~pal(Staerkste)
      
      proxy %>%
        addPolygons(
          fillColor = fill_val, fillOpacity = 0.85, weight = 1.5, color = "#bebfc2",
          layerId = ~WKR_NR, popup = ~popup_text, label = ~WKR_NAME,
          highlightOptions = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE)
        ) %>%
        addLegend(pal = pal, values = ~Staerkste, title = "Stärkste Partei", position = "bottomright", opacity = 1)
      
    } else {
      req(!all(is.na(map_data$Prozent)))
      base_col <- if(input$anzeige == "CDU/CSU") "black" else partei_farben[input$anzeige]
      pal <- colorNumeric(palette = c("white", base_col), domain = range(map_data$Prozent, na.rm = TRUE))
      fill_val <- ~pal(Prozent)
      
      proxy %>%
        addPolygons(
          fillColor = fill_val, fillOpacity = 0.85, weight = 1.5, color = "#bebfc2",
          layerId = ~WKR_NR, popup = ~popup_text, label = ~WKR_NAME,
          highlightOptions = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE)
        ) %>%
        addLegend(pal = pal, values = ~Prozent, title = paste(input$anzeige), 
                  position = "bottomright", opacity = 1,
                  labFormat = labelFormat(suffix = " %", between = " "))
    }
    
    proxy %>% addPolylines(data = bundeslaender_fix, color = "white", weight = 1.5)
  })
  
  # Reaktion auf Suche & Reinzoomen
  observeEvent(input$suche_wk, {
    if (input$suche_wk == "") {
      selected_wkr_id(NULL)
      leafletProxy("wahlkarte") %>% clearPopups() %>% setView(lng = 10.45, lat = 51.16, zoom = 6)
    } else {
      selected_wkr_id(input$suche_wk)
      sel_geo <- wk %>% filter(WKR_NR == input$suche_wk)
      req(nrow(sel_geo) > 0)
      centroid <- st_coordinates(st_centroid(st_geometry(sel_geo)))
      
      popup_content <- current_map_data() %>% filter(WKR_NR == input$suche_wk) %>% pull(popup_text)
      
      leafletProxy("wahlkarte") %>% 
        flyTo(lng = centroid[1], lat = centroid[2], zoom = 10) %>%
        clearPopups() %>%
        addPopups(lng = centroid[1], lat = centroid[2], popup = popup_content)
    }
  }, ignoreInit = TRUE)
  
  # Tooltip-Update bei Parameter-Wechsel
  observe({
    req(selected_wkr_id(), current_map_data())
    act_data <- current_map_data() %>% filter(WKR_NR == selected_wkr_id())
    req(nrow(act_data) > 0)
    
    sel_geo <- wk %>% filter(WKR_NR == selected_wkr_id())
    coords <- st_coordinates(st_centroid(st_geometry(sel_geo)))
    
    leafletProxy("wahlkarte") %>%
      clearPopups() %>%
      addPopups(lng = coords[1], lat = coords[2], popup = act_data$popup_text)
  })
}


# Start der App -----------------------------------------------------------

shinyApp(ui, server)



