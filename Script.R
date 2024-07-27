# Bibliotheken laden
library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggthemes)
#library(ggthemr)
library(shinyWidgets)
library(plotly)
library(stringr)
library(shinythemes)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(leafletCN)
library(sf)
library(ggh4x)
library(DT)
library(httr)
library(jsonlite)
library(conflicted)
library(scales)
library(shinycssloaders)
library(waiter)


# Pakete bei bestimmten Funktionen vorziehen
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("left_join", "dplyr")

conflict_prefer("aes", "ggplot2")
conflict_prefer("element_text", "ggplot2")
conflict_prefer("element_rect", "ggplot2")
conflict_prefer("element_line", "ggplot2")
conflict_prefer("unit", "ggplot2")

conflict_prefer("layout", "plotly")
conflict_prefer("style", "plotly")
conflict_prefer("config", "plotly")

conflict_prefer("str_split", "stringr")

conflict_prefer("stat_difference", "ggh4x")


month_names_german <- c(
  "Januar", "Februar","März", "April","Mai","Juni", "Juli", "August", "September",
  "Oktober","November", "Dezember"
)

month_names_german <- factor(month_names_german, levels = c(
  "Januar", "Februar","März", "April","Mai","Juni", "Juli", "August", "September",
  "Oktober","November", "Dezember"
))


dts <- st_read("bundeslaender.json")
dts <- st_transform(dts, crs = st_crs(4326))
dts <- st_boundary(dts)
dts <- st_union(dts)

stations <- st_read("stations.geojson")

# Funktionen====================================================================

## Funktion um vieljährige Mittelwerte zu  berechnen=============================
mean_over_time <- function(data, start, end, column_filter, is_month = FALSE, month_filter = NULL, is_day = FALSE) {
  if (is_month) {
    filtered_data <- data %>%
      filter(Jahr >= start & Jahr <= end & Monat_german == month_filter)
  }
  else {
    filtered_data <- data %>%
      filter(Jahr >= start & Jahr <= end)
  }
  if (is_day) {
    avg_value <- filtered_data %>%
      group_by(Jahr) %>%
      summarise(avg_column = sum({{column_filter}}, na.rm = TRUE)) %>%
      summarise(Average_column = mean(avg_column, na.rm = T)) %>%
      pull(Average_column)
  }
  else {
    avg_value <- filtered_data %>%
      group_by(Jahr) %>%
      summarise(avg_column = mean({{column_filter}}, na.rm = T)) %>%
      summarise(Average_column = mean(avg_column, na.rm = T)) %>%
      pull(Average_column)
  }
  
  return(avg_value)
}

## Funktion zum präprozessieren der Datensätze==================================
preprocess_climate_data <- function(data) {
  data <- data %>%
    mutate(
      Date = as.Date(as.character(MESS_DATUM), format = "%Y%m%d"),
      Jahr = year(Date),
      Monat = month(Date),
      Tag = day(Date)
    )
  
  sufficient_months <- data %>%
    group_by(Jahr) %>%
    summarize(has_sufficient = length(unique(Monat)) >= 11) %>%
    ungroup()
  
  valid_data <- data %>%
    inner_join(sufficient_months %>% filter(has_sufficient), by = "Jahr") %>%
    select(-has_sufficient)
  
  # Funktion zum Ersetzen fehlender Werte oder Setzen auf NA
  replace_missing_values <- function(df, column) {
    df$date_id <- paste(df$Monat, df$Tag, sep = "-")
    
    missing_counts <- df %>%
      group_by(Jahr) %>%
      summarize(missing_count = sum(!!sym(column) == -999, na.rm = TRUE))
    
    averages <- df %>%
      filter(!!sym(column) != -999) %>%
      group_by(date_id) %>%
      summarize(avg_value = mean(!!sym(column), na.rm = TRUE))
    
    df <- df %>%
      left_join(averages, by = "date_id") %>%
      left_join(missing_counts, by = "Jahr")
    
    df[[column]] <- case_when(
      df$missing_count >= 60 ~ NA_real_,
      df[[column]] == -999 ~ df$avg_value,
      TRUE ~ as.numeric(df[[column]])
    )
    
    # Datensatz aufräumen
    df %>%
      select(-date_id, -avg_value, -missing_count)
  }
  
  # Liste der zu prozessierenden Spalten
  columns_to_process <- c("SDK", "SHK_TAG", "RSK", "TNK", "TMK", "TXK", "FX", "FM", "NM", "VPM", "PM", "UPM", "TGK")
  
  # Ersetzung auf jede Spalte anwenden
  for (col in columns_to_process) {
    valid_data <- replace_missing_values(valid_data, col)
  }
  
  processed_data <- valid_data %>%
    mutate(
      Day = Date,
      Date_without_year = make_date(year = 2000, month = Monat, day = Tag),
      Monat_german = factor(month_names_german[Monat], levels = month_names_german),
      Regentag = if_else(!is.na(RSK) & RSK >= 0.1, 1L, 0L),
      Sommertag = if_else(!is.na(TXK) & TXK >= 25, 1L, 0L),
      Hitzetag = if_else(!is.na(TXK) & TXK >= 30, 1L, 0L),
      Tropennacht = if_else(!is.na(TNK) & TNK >= 20, 1L, 0L),
      Frosttag = if_else(!is.na(TNK) & TNK < 0, 1L, 0L),
      Eistag = if_else(!is.na(TXK) & TXK < 0, 1L, 0L),
      Schneetag = if_else(!is.na(SHK_TAG) & SHK_TAG > 1, 1L, 0L),
      Heiterertag = if_else(!is.na(NM) & NM < 1.6, 1L, 0L),
      Truebertag = if_else(!is.na(NM) & NM > 6.4, 1L, 0L)
    )
  
  return(processed_data)
}



## Funktion um alle Datensätze zu laden und zu präprozessieren==================
load_all_station_data <- function(file_paths) {
  all_data <- list()
  for (path in file_paths) {

    station_id <- str_extract(path, "\\d{4,5}(?=\\.txt$)")
    
    station_id <- sprintf("%04d", as.integer(station_id))
    
    print(paste("Loading data for station:", station_id))
    
    data <- read.csv(path, header = TRUE, sep = ';')
    preprocessed_data <- preprocess_climate_data(data)
    print(paste("Loaded", nrow(preprocessed_data), "rows for station", station_id))
    
    all_data[[station_id]] <- preprocessed_data
  }
  print("Finished loading all station data")
  print(paste("Total stations loaded:", length(all_data)))
  print("Station IDs loaded:")
  print(names(all_data))
  all_data
}



## Funktion um einen Zeitraum zu erhalten=======================================
get_year_range <- function(period) {
  years <- strsplit(period, "-")[[1]]
  start_year <- as.numeric(years[1])
  end_year <- as.numeric(years[2])
  return(list(start = start_year, end = end_year))
}


## Funktion um eine Kachel zu erstellen=========================================
create_tile <- function(title, value, icon_name,  width = 110, color, tooltip, diff_value = NULL, is_temperature = FALSE, is_rain = FALSE, is_snow = FALSE, is_pressure = FALSE, is_humidity = FALSE) {
  unit <- if (is_temperature) "°C" else if (is_rain) "mm" else if (is_snow) "cm" else if (is_pressure) "hPa" else if (is_humidity) "%" else  ""
  
  diff_text <- if (!is.null(diff_value)) {
    sprintf("%s%s %s", 
            ifelse(diff_value > 0, "+", ""), 
            diff_value, unit)
  } else {
    ""
  }

  tags$div(
    class = "stat-tile",
    style = paste0(
      "background-color: ",
      color,
      "; border-radius: 5px; padding: 5px; margin: 3px; text-align: center; flex: 0 0 auto; width: ", width, "px;"
    ),
    `data-toggle` = "tooltip",
    `data-placement` = "top",
    title = tooltip,
    tags$div(
      style = "display: flex; align-items: center; justify-content: center;",
      icon(name = icon_name, style = "font-size: 14px; margin-right: 4px;"),
      tags$h6(title, style = "margin: 0; font-size: 11px;")
    ),
    tags$p(value, style = "font-size: 14px; font-weight: bold; margin: 3px 0;"),
    tags$p(diff_text, style = "font-size: 9px; margin: 1px 0;")
  )
}

# Custom CSS für die Kacheln
custom_tile_css <- "
.stat-tile {
  transition: transform 0.3s ease-in-out;
}
.stat-tile:hover {
  transform: scale(1.05);
}
"
## Funktion für eine Wetterkachel=====================
create_weather_tile <- function(title, value, icon_name, color, tooltip, type = "default", wind_speed = NULL) {
  value_display <- switch(type,
                          "temperature" = create_gauge_chart(value, " °C", -20, 40, c("#3498db", "#f1c40f", "#e74c3c")),
                          "pressure" = create_gauge_chart(value, " hPa", 950, 1050, c("#3498db", "#2ecc71", "#e74c3c")),
                          "humidity" = create_gauge_chart(value, " %", 0, 100, c("#3498db", "#2ecc71", "#e74c3c")),
                          "wind_direction" = {
                            compass <- create_wind_direction_compass(value)
                            if (!is.null(wind_speed)) {
                              compass <- tagList(
                                compass,
                                tags$p(paste(round(wind_speed, 1), "Km/h"), style = "font-size: 14px; margin: 5px 0 0 0;")
                              )
                            }
                            compass
                          },
                          "precip" = paste(value, "mm/h"),
                          tags$p(value, style = "font-size: 18px; font-weight: bold; margin: 5px 0;")
  )
  
  tags$div(
    class = "weather-tile",
    style = paste0("background-color: ", color, "; border-radius: 10px; padding: 10px; margin: 5px; text-align: center; width: 180px; height: 250px;"),
    `data-toggle` = "tooltip",
    `data-placement` = "top",
    title = tooltip,
    tags$div(
      style = "display: flex; align-items: center; justify-content: center;",
      icon(name = icon_name, style = "font-size: 20px; margin-right: 5px;"),
      tags$h5(title, style = "margin: 0; font-size: 14px;")
    ),
    value_display
  )
}

## Funktion zum Aufrufen der API================================================
### Hilfsfunktion====
deg_to_cardinal <- function(degree) {
  directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                  "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")
  index <- round(degree / 22.5) + 1
  return(directions[index])
}

callAPI <- function(lat, lon) {
  uri <- paste0("https://api.openweathermap.org/data/2.5/find?lat=",
                lat,
                "&lon=",
                lon,
                "&cnt=1", "&lang=de", "&appid=<APIKey>")
  tryCatch({
    res <- GET(uri)
    if (res$status_code != 200) {
      stop("API request failed with status: ", res$status_code)
    }
  },
  warning = function(w) { print("Es gab einen Fehler")},
  error = function(e) { print("Es gab einen Fehler"); return(NULL)}
  )
  
  rawData <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  weatherData <- list(
    time = format(as_datetime(rawData$list$dt, tz = "Europe/Berlin"), "%d. %b. %Y %H:%M"),
    weather = rawData$list$weather[[1]]$description,
    temp = rawData$list$main$temp - 273.15, # In Celsius Umrechnen
    press = rawData$list$main$pressure, # Einheit hPa
    humid = rawData$list$main$humidity, # in %
    wind = rawData$list$wind$speed, # in m/s
    winddir = rawData$list$wind$deg,
    rain = rawData$list$rain
  )
  return(weatherData)
}

## Funktion für eine skala=================
create_gauge_chart <- function(value, suffix, min, max, colors) {
  plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = value,
    title = "",
    gauge = list(
      axis = list(range = list(min, max), tickwidth = 1, tickcolor = "darkblue"),
      bar = list(color = "darkblue"),
      bgcolor = "rgba(0,0,0,0)",
      borderwidth = 2,
      bordercolor = "gray",
      steps = list(
        list(range = c(min, min + (max-min)/3), color = colors[1]),
        list(range = c(min + (max-min)/3, min + 2*(max-min)/3), color = colors[2]),
        list(range = c(min + 2*(max-min)/3, max), color = colors[3])
      )
    ),
    number = list(
      valueformat = ".1f",
      font = list(size = 16),
      suffix = suffix
    )
  ) %>%
    layout(
      margin = list(l=5, r=5, t=25, b=5),
      height = 150,
      width = 160,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
}


## Funktion für einen Kompass============================
create_wind_direction_compass <- function(angle) {
  rad <- angle * pi / 180
  
  tick_marks <- ""
  labels <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
              "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  for (i in 1:16) {
    label_angle <- (i - 1) * 22.5
    rad_label_angle <- label_angle * pi / 180
    x_inner <- 75 + 60 * sin(rad_label_angle)
    y_inner <- 75 - 60 * cos(rad_label_angle)
    x_outer <- 75 + 67.5 * sin(rad_label_angle)
    y_outer <- 75 - 67.5 * cos(rad_label_angle)
    x_label <- 75 + 55 * sin(rad_label_angle)
    y_label <- 75 - 55 * cos(rad_label_angle)
    tick_marks <- paste0(tick_marks, sprintf(
      '<line x1="%f" y1="%f" x2="%f" y2="%f" stroke="#333" stroke-width="1"/>', 
      x_inner, y_inner, x_outer, y_outer))
    tick_marks <- paste0(tick_marks, sprintf(
      '<text x="%f" y="%f" text-anchor="middle" alignment-baseline="middle" fill="#333" font-size="6">%s</text>',
      x_label, y_label, labels[i]))
  }
  
  svg_code <- sprintf('
    <svg width="150" height="150" viewBox="0 0 150 150">
      <circle cx="75" cy="75" r="67.5" fill="none" stroke="#333" stroke-width="2"/>
      %s
      <line x1="75" y1="75" x2="%f" y2="%f" stroke="red" stroke-width="2"/>
    </svg>
  ', tick_marks, 75 + 45 * sin(rad), 75 - 45 * cos(rad))
  
  tags$div(
    HTML(svg_code)
  )
}

calculate_max_sunshine_duration <- function(latitude, year) {
  
  deg_to_rad <- function(deg) {
    return(deg * pi / 180)
  }
  
  rad_to_deg <- function(rad) {
    return(rad * 180 / pi)
  }
  
  day_of_year <- function(date) {
    return(yday(date))
  }
  
  declination_angle <- function(day) {
    return(23.5 * sin(deg_to_rad((360/365) * (day - 81))))
  }
  
  day_length_hours <- function(lat, decl) {
    lat_rad <- deg_to_rad(lat)
    decl_rad <- deg_to_rad(decl)
    cos_hour_angle <- -tan(lat_rad) * tan(decl_rad)
    
    if (cos_hour_angle < -1) {
      return(24)
    } else if (cos_hour_angle > 1) {
      return(0)
    } else {
      hour_angle <- acos(cos_hour_angle)
      return(2 * rad_to_deg(hour_angle) / 15)
    }
  }
  
  dates <- seq(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31")), by = "day")
  
  results <- lapply(dates, function(date) {
    day <- day_of_year(date)
    decl <- declination_angle(day)
    day_length <- day_length_hours(latitude, decl)
    return(list(date = date, sunshine_duration = round(day_length, 2)))
  })
  
  results_df <- do.call(rbind, lapply(results, data.frame))
  results_df$Monat <- factor(month_names_german[month(results_df$date)], levels = month_names_german)
  return(results_df)
}



# UI definieren=================================================================
ui <- page_sidebar(
  use_waiter(),
  autoWaiter(),
  waiterShowOnLoad(),
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  tags$head(
    tags$style(HTML("
      .leaflet-container { background: transparent; }
      .data-source {
        position: absolute;
        bottom: 0px;
        right: 16px;
        font-size: 11px;
        color: #888;
        display: flex;
        align-items: center;
      }
      .data-source a {
        color: #aaa;
        text-decoration: none;
        display: flex;
        align-items: center;
        margin-left: 4px;
      }
      .data-source a:hover {
        text-decoration: underline;
      }
      .data-source img {
        height: 30px;
        margin-left: 3px;
        margin-bottom: 6px;
      }
    "))
  ),
  title =  div(
    style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
    div(
      "Wetterdaten Deutschland",
      style = "font-size: 24px; font-weight: bold; margin: 0;"
    ),
    div(
      style = "display: flex; align-items: center;",
      div(
        h6(
          HTML("Erstellt von Sandro Hoffmann<br>im Rahmen des Moduls Thematische Internetkartographie (SoSe24)<br>
               E-Mail: s81863@bht-berlin.de"),
          style = "font-size: 10px; margin: 0; white-space: normal; text-align: left;"
        ),
        style = "margin-right: 10px;"
      ),
      a(
        href = "https://www.bht-berlin.de/",
        target = "_blank",
        img(
          src = "BHT_Logo_horizontal_Negativ_RGB_288ppi.png",
          height = "40px",
          style = "margin: 0; padding: 0;"
        ),
        style = "margin: 0; padding: 0;"
      )
    )
  ),
  window_title = "Wetterdaten Deutschland",
  sidebar = sidebar(
    width = 300,
    conditionalPanel(
      condition = "input.tabset !== 'maps' & input.tabset !== 'info'",
      uiOutput("stationText"),
      leafletOutput("map", height = "300px")
    ),
    conditionalPanel(
      condition = "input.tabset !== 'Datentabelle' & input.tabset !== 'live' & input.tabset !== 'maps' & input.tabset !== 'info'",
      accordion(
        accordion_panel(
          "Zeiträume",
          radioButtons(
            "monatOption",
            "Betrachtungszeitraum:",
            choices = list("Monat" = "einMonat",
                           "Jahr" = "alleMonate",
                           "Gesamter Zeitraum" = "all"),
            selected = "alleMonate"
          ),
          uiOutput("timeFramePickerUI")
        )
      )
    ),
    conditionalPanel(
      condition = "input.tabset === 'Datentabelle'",
      radioButtons(
        "dataTableOption",
        "Datenansicht:",
        choices = list("Tageswerte" = "daily",
                        "Gemittelte Werte" = "averaged"),
        selected = "daily"
      ),
      conditionalPanel(
        condition = "input.tabset === 'live'"
      ),
      uiOutput("dataTableMonthPickerUI")
    )
  ),
  navset_card_pill(
    id = "tabset",
    nav_panel(
      title = "Temperatur", value = "Temperatur",
      uiOutput("allTiles"),
      plotlyOutput("ClimatePlot", height = "280px"),
      plotOutput("ClimatePlot2", height = "100px"),
      tags$div(class = "data-source", "Quelle: Deutscher Wetterdienst")
    ),
    nav_panel(
      title = "Niederschlag", value = "Niederschlag",
      uiOutput("precipTiles"),
      plotlyOutput("PrecipPlot", height = "280px"),
      plotOutput("PrecipPlot2", height = "100px"),
      tags$div(class = "data-source", "Quelle: Deutscher Wetterdienst")
    ),
    nav_panel(
      title = "Sonnenschein & Bedeckung", value = "Sonnenschein",
      uiOutput("sunTiles"),
      plotlyOutput("sunPlot", height = "280px"),
      plotOutput("SunPlot2", height = "100px"),
      tags$div(class = "data-source", "Quelle: Deutscher Wetterdienst")
    ),
    nav_panel(
      title = "Datentabelle", value = "Datentabelle",
      fluidRow(
        column(
          width = 12,
          DT::dataTableOutput("table")
        )
      ),
      tags$div(class = "data-source", "Quelle: Deutscher Wetterdienst")
    ),
    nav_panel(
      title = "Live-Wetter", value = "live", 
      uiOutput("currentWeather"),
      tags$div(
        class = "data-source", 
        "Wetterdaten bereitgestellt von  ",
        tags$a(
          href = "https://openweathermap.org/",
          target = "_blank",
          "OpenWeather",
          tags$img(src = "OpenWeather-Negative-Logo RGB.png", alt = "OpenWeather Logo")
        )
      )
    ),
    nav_panel(
      title = "Karten", value = "maps",
      leafletOutput("mapDE"),
      tags$div(class = "data-source", "Quelle: Deutscher Wetterdienst")
    ),
    nav_panel(
      title = "Info", value = "info",
      accordion(
        accordion_panel(
          "Über das Dashboard",
          p("Dieses Dashboard zeigt in erster Linie historische Wetter- und Klimadaten von 16 Wetterstationen in Deutschland. Die Stationen befinden sich 
            in den Hauptstädten der Bundesländer. Die Daten können sowohl in grafischer Form als auch in einer interaktiven Tabelle abgerufen und exportiert werden und beziehen sich auf Tageswerte einzelner Monate, einzelner Jahre
            oder auf gemittelte Werte über den gesamten Erfassungszeitraum. Dabei werden die Werte pro Jahr oder pro Monat eines Jahres gemittelt. 
            Zusätzlich gibt es eine interaktive Karte mit Layern für einige Mittelwerte im 1x1 km Raster sowie aktuelle Wetterdaten für jede Station.",
            style = "font-size: 12px;")
        ),
        accordion_panel(
          "Erklärungen zu den im Dashboard abgebildeten Werten",
          p("Ab April 2001 wurde der Zeitraum bzw. Zeitpunkt der Messungen angepasst. Die zuerst genannten Zeiten 
            beziehen sich daher stets auf die aktuellere Variante.", style = "font-size: 12px;"),
          h5("Temperatur"),
          p("Höchsttemperatur: Tagesmaximum der Lufttemperatur in 2 m Höhe zwischen 00:00 und 24:00 UTC bzw.
            zwischen 21:30 am Vortag und 7:30 MEZ.",
            style = "margin-bottom: 3px; font-size: 12px;"),
          p("Tiefsttemperatur: Tagesminimum der Lufttemperatur in 2 m Höhe zwischen 00:00 und 24:00 UTC bzw.
            zwischen 21:30 am Vortag und 7:30 MEZ.",
            style = "margin-bottom: 3px; font-size: 12px;"),
          p("Mittlere Temperatur: Arithmetisches Mittel der Lufttemperatur aus mindestens 21 Stundenwerten.", style = "font-size: 12px;"),
          h5("Niederschlag und Schneehöhe"),
          p("Niederschlag: Tägliche Niederschlagshöhe in mm zwischen 06:00 und 06:00 am Folgetag bzw. 07:30 und 7:30 am Folgetag.",
            style = "margin-bottom: 3px; font-size: 12px;"),
          p("Schneehöhe: Tageswert der Schneehöhe in cm um 06:00 UTC bzw. 07:30 MEZ.", style = "font-size: 12px"),
          h5("Sonnenschein und Bedeckung"),
          p("Sonnenscheindauer: Tagessumme in Stunden gemessen zwischen 00:00 und 24:00 UTC bzw. 00:00 und 24:00 MEZ",
            style = "margin-bottom: 3px; font-size: 12px"),
          p("Bedeckung: Tagesmittel des Bedeckungsgrades des Himmelsgewölbes in Achteln als arithemtisches Mittel aus Messungen um 6:00, 12:00 und 18:00 UTC (0 Achtel = Wolkenloser Himmel; 8 Achtel = 
            Vollkommen bedeckter Himmel).", style = "font-size: 12px")
        ),
        accordion_panel(
          "Umgang mit fehlenden Werten",
          p("Da an vielen Wetterstationen einige Messgrößen erst im Laufe der Zeit erhoben wurden oder aktuell nicht mehr erhoben werden, kommt es häufig 
            zu fehlenden Werten. Teilweise gibt es in den Daten auch Lücken für einige Monate. Fehlende Werte werden durch den Wert -999 gekennzeichnet oder 
            sind in manchen Fällen schlichtweg nicht vorhanden. Um keinen störenden Einfluss auf gemittelte Werte zu haben, werden unvollständige Jahre 
            aus den Daten entfernt, sofern sich die Anzahl der Tage mit fehlenden Werten auf mehr als 60 Tage im Jahr beläuft. Andernfalls werden sie durch die entsprechenden
            Mittelwerte aus den letzten 10 Jahren ersetzt.
            Es kann daher zu lückenhaften Diagrammen kommen. Auch kann der im Titel angegebene
            Zeitraum vom dem des Schiebereglers für die Jahre abweichen.", style = "font-size: 12px")
        ),
        accordion_panel(
          "Erstellung des Dashboards",
          p(
            "Dieses Dashboard wurde mit R (Version 4.1.2) in R-Studio (Version 2024.04.02) erstellt.",
            "Der gesamte Quellcode ist ", 
            a("hier", href = "https://github.com/s81863/WeatherDashboard", target = "_blank"),
            " abruf- und downloadbar.",
            style = "font-size: 12px"
          )
        )
      )
    )
  ), 
  full_screen = TRUE
)
  

# Server-Funktion===============================================================
server <- function(input, output) {
  
  selectedData <- reactiveVal(NULL)
  selectedStation <- reactiveVal(NULL)
  selectedLat <- reactiveVal(NULL)
  selectedLon <- reactiveVal(NULL)
  
  file_paths <- c("produkt_klima_tag_19500101_20231231_00403.txt",
                  "produkt_klima_tag_18810101_20231231_03126.txt",
                  "produkt_klima_tag_18900101_20231231_00691.txt",
                  "produkt_klima_tag_18900101_20231231_04625.txt",
                  "produkt_klima_tag_18930101_20231231_03987.txt",
                  "produkt_klima_tag_19360101_20231231_01975.txt",
                  "produkt_klima_tag_19360101_20231231_02014.txt",
                  "produkt_klima_tag_19480101_20230801_03137.txt",
                  "produkt_klima_tag_19510101_20231231_00150.txt",
                  "produkt_klima_tag_19510101_20231231_01270.txt",
                  "produkt_klima_tag_19510101_20231231_04336.txt",
                  "produkt_klima_tag_19520101_20231231_01078.txt",
                  "produkt_klima_tag_19540601_20231231_03379.txt",
                  "produkt_klima_tag_19580101_20231231_04928.txt",
                  "produkt_klima_tag_19740101_20231231_02564.txt",
                  "produkt_klima_tag_19870601_20231231_05541.txt")
  all_station_data <- load_all_station_data(file_paths)
 
## Auswahl zwischen Monat, Jahr und gesamtem Zeitraum===========================  
  output$timeFramePickerUI <- renderUI({
    data <- climateData()
    if (is.null(data)) return(NULL)
    
    if (input$monatOption == "einMonat") {
      list(
        sliderInput(
          "Jahr",
          "Jahr:",
          min = min(data$Jahr),
          max = max(data$Jahr),
          value = max(data$Jahr),
          step = 1,
          width = '100%'
        ),
        selectInput(
          "Monat",
          "Monat",
          choices = unique(data$Monat_german),
          selected = unique(data$Monat_german)[1]
        ),
        selectInput("Period", "Bezugszeitraum", choices = c("1991-2020",
                                                            "1981-2010",
                                                            "1971-2000",
                                                            "1961-1990"),
                    selected = "1991-2020")
      )
    } else if (input$monatOption == "alleMonate") {
      list(
        sliderInput(
          "Jahr",
          "Jahr:",
          min = min(data$Jahr),
          max = max(data$Jahr),
          value = max(data$Jahr),
          step = 1,
          width = '100%'
        ),
        selectInput("Period", "Bezugszeitraum", choices = c("1991-2020",
                                                            "1981-2010",
                                                            "1971-2000",
                                                            "1961-1990"),
                    selected = "1991-2020")
      )
    } else {
      Monat_german <- c("", unique(as.character(data$Monat_german)))
      
      pickerInput(
        inputId = "Monat",
        label = "Monat",
        choices = unique(Monat_german),
        selected = "",
        options = list(`max-options` = 1, `none-selected-text` = "Gesamtes Jahr"),
        multiple = TRUE
      )
    }
  })
  
## Monatsauswahl für die Datentabelle===========================================  
  output$dataTableMonthPickerUI <- renderUI({
    data <- climateData()
    if (is.null(data)) return(NULL)
    
    if (input$dataTableOption == "averaged") {
      Monat_german <- c("", unique(as.character(data$Monat_german)))
      
      pickerInput(
        inputId = "dataTableMonat",
        label = "Monat",
        choices = unique(Monat_german),
        selected = "",
        options = list(`max-options` = 1, `none-selected-text` = "Gesamtes Jahr"),
        multiple = TRUE
      )
    }
  })
  
## Leaflet-Karte================================================================  
  output$map <- renderLeaflet({
    bbox <- st_bbox(dts)
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 5)) %>%
      setView(lng = 11.03, lat = 51.26, zoom = 5) %>%
      setMaxBounds(lng1 = bbox["xmin"], lat1 = bbox["ymin"], lng2 = bbox["xmax"], lat2 = bbox["ymax"]) %>%
      addPolylines(data = dts,
                   color = "White",
                   stroke = TRUE,
                   weight = 1) %>%
      addCircleMarkers(data = stations,
                       color = "yellow",
                       radius = 5,
                       label = lapply(as.list(stations$name), HTML),
                       layerId = ~paste(name, number, sep = "___")) 
  })

## Klick auf Leaflet-Karte======================================================    
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
    if (!is.null(click)) {
      parts <- strsplit(click$id, "___")[[1]]
      name <- parts[1]
      number <- parts[2]
      print(paste("Clicked station:", name, "- ID:", number))
      
      if (number %in% names(all_station_data)) {
        newData <- all_station_data[[number]]
        selectedData(newData)
        selectedLat(click$lat)
        selectedLon(click$lng)
        
        station <- stations %>% 
          filter(name == !!name & number == !!number) %>%
          as.list()
        
        selectedStation(station)
        print(paste("Selected station:", name, "- ID:", number))
      } else {
        print(paste("No data found for station ID:", number))
      }
    }
  })
  
## Änderung des Kartentitels====================================================
  output$stationText <- renderUI({
    station <- selectedStation()
    if (!is.null(station)) {
      h6(paste("Gewählte Station:", station$name))
    } else {
      h6("Gewählte Station: Berlin Dahlem (FU)")
    }
  })
  
## Änderung des Datensatzes=====================================================  
  climateData <- reactive({
    selected <- selectedData()
    print(paste("SELECTED:",is.null(selected)))
    if (!is.null(selected) && nrow(selected) > 0) {
      print("DEBUG: Using selectedData")
      return(selected)
    } else {
      print("DEBUG: Loading default dataset")
      return(preprocess_climate_data(read.csv(file = "produkt_klima_tag_19500101_20231231_00403.txt", header = TRUE, sep = ';')))
    }
  })
  
  observe({
    climateData()
  })

## Daten filtern================================================================    
  filtered_data <- reactive({
    data <- climateData()
    if (input$monatOption == "alleMonate") {
      data %>% filter(Jahr == input$Jahr)
    } else if (input$monatOption == "einMonat") {
      data %>% filter(Jahr == input$Jahr & Monat_german == input$Monat)
    } else {
      data$Day2 <- data$Day
      data %>%
        {if (input$Monat != "") filter(., Monat_german == input$Monat) else .} %>%
        group_by(Jahr) %>%
        summarise(
          TXK = mean(TXK, na.rm = TRUE),
          TMK = mean(TMK, na.rm = TRUE),
          TNK = mean(TNK, na.rm = TRUE),
          RSK = sum(RSK, na.rm = TRUE),
          NM = mean(NM, na.rm = TRUE),
          Regentage = sum(Regentag == 1, na.rm = TRUE),
          Sommertage = sum(Sommertag == 1, na.rm = TRUE),
          Hitzetage = sum(Hitzetag == 1, na.rm = TRUE),
          Tropennaechte = sum(Tropennacht == 1, na.rm = TRUE),
          Frosttage = sum(Frosttag == 1, na.rm = TRUE),
          Eistage = sum(Eistag == 1, na.rm = TRUE),
          Schneetage = sum(Schneetag == 1, na.rm = TRUE),
          Heiteretage = sum(Heiterertag == 1, na.rm = TRUE),
          Truebetage = sum(Truebertag == 1, na.rm = TRUE),
          SDK = sum(SDK, na.rm = TRUE),
          Day = unique(Jahr)
        ) %>%
        arrange(Jahr) %>%
        mutate(Day = as.Date(paste(Jahr, "01", "01", sep = "-")))
    }
  })
  
  
  filtered_data2 <- reactive({
    data <- climateData()
    if (input$monatOption == "alleMonate") {
      data %>% filter(Jahr == input$Jahr) %>%
        pivot_longer(cols = c("TNK", "TMK", "TXK"), names_to = "Cat")
    } else if (input$monatOption == "einMonat") {
      data %>% filter(Jahr == input$Jahr & Monat_german == input$Monat) %>%
        pivot_longer(cols = c("TNK", "TMK", "TXK"), names_to = "Cat")
    } else {
      data %>%
        {if (input$Monat != "") filter(., Monat_german == input$Monat) else .} %>%
        group_by(Jahr) %>%
        summarise(
          TXK = mean(TXK, na.rm = TRUE),
          TMK = mean(TMK, na.rm = TRUE),
          TNK = mean(TNK, na.rm = TRUE),
          RSK = sum(RSK, na.rm = TRUE),
          Regentage = sum(Regentag == 1, na.rm = TRUE),
          Sommertage = sum(Sommertag == 1, na.rm = T),
          Hitzetage = sum(Hitzetag == 1, na.rm = T),
          Tropennaechte = sum(Tropennacht == 1, na.rm = T),
          Frosttage = sum(Frosttag == 1, na.rm = T),
          Eistage = sum(Eistag == 1, na.rm = T),
          Heiteretage = sum(Heiterertag == 1, na.rm = T),
          Truebetage = sum(Truebertag == 1, na.rm = T),
          SDK = sum(SDK, na.rm = TRUE),
          Day = unique(Jahr)
        ) %>%
        arrange(Jahr) %>%
        mutate(Day = as.Date(paste(Jahr, "01", "01", sep = "-"))) %>%
        pivot_longer(cols = c("TNK", "TMK", "TXK"), names_to = "Cat")
    }
  })
  
  daily_averages <- reactive({
    data <- climateData()
    period <- input$Period
    year_range <- get_year_range(period)
    
    data %>%
      filter(Jahr >= year_range$start & Jahr <= year_range$end & !(month(Date_without_year) == 2 & day(Date_without_year) == 29))
  })
  
  averaged_data <- reactive({
    daily_averages() %>%
      group_by(Month = month(Date_without_year), Day = day(Date_without_year)) %>%
      summarise(Average_TMK = mean(TMK, na.rm = TRUE),
                Average_RSK = sum(RSK, na.rm = T)/30,
                Average_SDK = sum(SDK, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(Month, Day)
  })
    
## Primärer Temperaturplot======================================================  
  output$ClimatePlot <- renderPlotly({
    df <- filtered_data()
    
    if (input$monatOption == "alleMonate") {
      date_breaks <- "1 month"
      date_labels <- "%b"
      plot_title <-
        paste0("Temperaturverlauf ", as.character(input$Jahr))
      
      daily_averages_year <- averaged_data() %>%
        dplyr::mutate(Date_without_year = as.Date(paste(2000, Month, Day, sep = "-"))) %>%
        dplyr::filter(Date_without_year >= min(df$Date_without_year) & Date_without_year <= max(df$Date_without_year))
      
      df <- df %>%
        dplyr::left_join(daily_averages_year %>% select(Date_without_year, Average_TMK), by = "Date_without_year")
      
    } else if (input$monatOption == "einMonat"){
      date_breaks <- "1 day"
      date_labels <- "%d"
      plot_title <-
        paste0("Temperaturverlauf ",
               input$Monat,
               " ",
               as.character(input$Jahr))
      
      daily_averages_year <- averaged_data() %>%
        dplyr::mutate(Date = as.Date(paste(input$Jahr, Month, Day, sep = "-"))) %>%
        dplyr::filter(format(Date, "%B") == input$Monat)
      df <- df %>%
        dplyr::left_join(daily_averages_year %>% select(Date, Average_TMK), by = c("Day" = "Date"))

    } else {
      date_breaks <- "5 year"
      date_labels <- "%Y"
      plot_title <- 
        if (input$Monat == "") {
        paste0("Jahresmitteltemperaturen ", min(df$Jahr), "–", max(df$Jahr))
      } else {
        paste0("Mittlere ", input$Monat, "temperaturen ", min(df$Jahr), "-", max(df$Jahr))
      }
      daily_averages_year <- NULL
      df <- df %>%
        filter(TMK != 0 & TXK != 0 & TNK != 0)
    }
    
    text <- if (input$monatOption == "all") {
      if (input$Monat == "") {
        paste("Jahr:", df$Jahr, "<br>",
              "Höchsttemperatur:", round(df$TXK, 2), "°C", "<br>",
              "Mittlere Temperatur:", round(df$TMK, 2), "°C", "<br>",
              "Tiefsttemperatur:", round(df$TNK, 2), "°C")
      } else {
        paste(input$Monat, df$Jahr, "<br>",
              "Höchsttemperatur:", round(df$TXK, 2), "°C", "<br>",
              "Mittlere Temperatur:", round(df$TMK, 2), "°C", "<br>",
              "Tiefsttemperatur:", round(df$TNK, 2), "°C")
      }
    } else {
      paste("Datum:", format(df$Day, "%d-%m-%Y"), "<br>", 
            "Höchsttemperatur:", round(df$TXK, 2), "°C", "<br>",
            "Mittlere Temperatur:", round(df$TMK, 2), "°C", "<br>",
            "Tiefsttemperatur:", round(df$TNK, 2), "°C")
    }
    
    cols <-
      c(
        "Mittlere Temperatur" = "gray80",
        "Höchsttemperatur" = "red",
        "Tiefsttemperatur" = "blue",
        "Langj. Mittel" = "green"
      )
    
    difference_colors <- c("Zu warm" = "red", "Zu kalt" = "blue")
    
    
    p <- ggplot(df, aes(
      x = Day,
      group = 1,
      text = text
    )) +
      geom_ribbon(aes(ymin = TNK, ymax = TXK, fill = "Temperaturspanne"),
                  alpha = 0.5)
      if (!is.null(daily_averages_year)) {
        p <- p + 
          ggh4x::stat_difference(aes(ymin = Average_TMK, ymax = TMK), levels = c("Zu warm", "Zu kalt"),
                                 alpha = 0.5)
      }
      
    p <- p +
      geom_line(aes(y = TMK, color = "Mittlere Temperatur"), size = 0.35) +
      geom_line(aes(y = TNK, color = "Tiefsttemperatur"), size = 0.25, alpha = 0.5) +
      geom_line(aes(y = TXK, color = "Höchsttemperatur"), size = 0.25, alpha = 0.5) +
      scale_fill_manual(name = NULL,
                        values = c("Temperaturspanne" = "gray60", difference_colors),
                        labels = c("Temperaturspanne", "Zu warm", "Zu kalt"))
    
    if (!is.null(daily_averages_year)) {
      p <- p + geom_line(aes(y = Average_TMK, color = "Langj. Mittel"), 
                         size = 0.25)
    }
    
    p <- p +
      scale_color_manual(name = "", values = cols,
                         labels = c(
                           "Höchsttemperatur",
                           "Mittlere Temperatur",
                           "Tiefsttemperatur",
                           "Langj. Mittel"
                         )
      ) +
      scale_x_date(
        date_breaks = date_breaks,
        date_labels = date_labels,
        name = "",
        expand = c(0.01, 0)
      ) +
      scale_y_continuous(name = "Temperatur (°C)", expand = c(0.01, 0)) +
      coord_cartesian() +
      ggtitle(plot_title) +
      theme(
        text = element_text(colour = "white"),
        plot.title = element_text(size = 16, family = "Arial"),
        axis.text.y = element_text(colour = "white", family = "Courier"),
        axis.text.x = element_text(colour = "white", family = "Courier"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "black"),
        legend.title = element_blank(),
        panel.grid.major = element_line(size = 0.25, colour = "gray"),
        panel.grid.minor = element_line(size = 0.1, colour = "gray"),
        legend.position = "bottom",
        plot.margin = unit(c(1, 1, 2, 1), "lines"))
    
    myplot <- plotly::ggplotly(p, tooltip = c("text")) %>%
      style(hoverinfo = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = '#222',
        paper_bgcolor = '#222',
        font = list(color = '#fff'),
        legend = list(
          x = 0.5,
          y = -0.1,
          xanchor = "center",
          yanchor = "top",
          orientation = 'h'
        )
      ) %>%
      config(locale = 'de')
    
    for (i in 1:length(myplot$x$data)){
      if (!is.null(myplot$x$data[[i]]$name)){
        myplot$x$data[[i]]$name =  gsub("\\(","",str_split(myplot$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    myplot
    
  })
  
## Sekundärer Temperaturplot====================================================  
  output$ClimatePlot2 <- renderPlot({
    df <- filtered_data2()
    df$Cat <- factor(df$Cat, levels = c("TNK", "TMK", "TXK"))
    
    if (input$monatOption == "alleMonate") {
      date_breaks <- "1 month"
      date_labels <- "%b"
    } else if (input$monatOption == "einMonat"){
      date_breaks <- "1 day"
      date_labels <- "%d"
    } else {
      date_breaks <- "5 year"
      date_labels <- "%Y"
      df <- df %>%
        filter(value != 0)
    }
    
    ggplot(df, aes(Day, y = Cat, fill = value)) +
      geom_raster() +
      scale_fill_gradientn(colors = rev(hcl.colors(30, "RdYlBu")), name = "Temperatur (°C)") +
      scale_x_date(date_breaks = date_breaks,
                   date_labels = date_labels,
                   name = "",
                   expand = c(0.01, 0)) +
      scale_y_discrete(labels = c("Tiefsttemp.", "Mittlere Temp.", "Höchsttemp.")) +
      theme(
        text = element_text(colour = "white"),
        plot.title = element_text(size = 16, family = "Arial"),
        axis.text.y = element_text(colour = "white", angle = 45),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = "white", family = "Courier", size = 12),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "black"),
        legend.title = element_text(colour = "white"),
        plot.background  = element_rect(fill = '#222222', colour = "transparent"),
        panel.background = element_rect(fill = '#222222'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.margin=margin(0,0,0,0),
        legend.box.spacing = unit(0, "pt"),
        legend.key.width=unit(3,"cm"),
        plot.margin = unit(c(0,0.7,0,0), "cm")
      )
    
  })
  
  
## Primärer Niederschlagsplot===================================================  
  output$PrecipPlot <- renderPlotly({
    df <- filtered_data()
    
    if (input$monatOption == "alleMonate") {
      date_breaks <- "1 month"
      date_labels <- "%b"
      plot_title <- paste0("Niederschlags- und Schneehöhen ", as.character(input$Jahr))
      x_col <- ~Day
      
      daily_averages_year <- averaged_data() %>%
        mutate(Date_without_year = as.Date(paste(2000, Month, Day, sep = "-"))) %>%
        filter(Date_without_year >= min(df$Date_without_year) & Date_without_year <= max(df$Date_without_year))
      
      df <- df %>%
        left_join(daily_averages_year %>% select(Date_without_year, Average_RSK), by = "Date_without_year")
    } else if (input$monatOption == "einMonat") {
      date_breaks <- "1 day"
      date_labels <- "%d"
      plot_title <- paste0("Niederschlags- und Schneehöhen ", input$Monat, " ", as.character(input$Jahr))
      x_col <- ~Day
      
      daily_averages_year <- averaged_data() %>%
        mutate(Date = as.Date(paste(input$Jahr, Month, Day, sep = "-"))) %>%
        filter(format(Date, "%B") == input$Monat)
      df <- df %>%
        left_join(daily_averages_year %>% select(Date, Average_RSK), by = c("Day" = "Date"))
    } else {
      
      date_breaks <- "5 year"
      date_labels <- "%Y"
      if (input$Monat == "") {
        plot_title <- paste0("Jährliche Niederschlagssummen ", min(df$Jahr), "–", max(df$Jahr))
      } else {
        plot_title <- paste0(input$Monat, " Niederschlagssummen ", min(df$Jahr), "–", max(df$Jahr))
        }
      
      x_col <- ~Jahr
    }
    
    if (input$monatOption != "all") {
      df <- df %>%
        arrange(Day) %>%
        mutate(Cumulative_RSK = cumsum(RSK)) %>%
        mutate(Cumulative_avg_RSK = cumsum(Average_RSK))
    } else {
      df <- df %>%
        filter(RSK != 0) %>%
        arrange(Jahr) %>%
        mutate(Cumulative_RSK = cumsum(RSK))
      plot_title <- ifelse(input$Monat == "",
                           paste0("Jährliche Niederschlagssummen ", min(df$Jahr), "–", max(df$Jahr)),
                           paste0(input$Monat, " Niederschlagssummen ", min(df$Jahr), "–", max(df$Jahr)))
    }
    
    fig <- plot_ly(df) %>%
      add_bars(
        x = ~ Day,
        y = ~ RSK,
        name = "Niederschlag",
        marker = list(color = "skyblue", opacity = 0.8),
        hoverinfo = "text",
        textposition = "none",
        text = if (input$monatOption != "all") {
          ~paste(
            "Datum:", format(Day, "%d.%m.%Y"),
            "<br>Niederschlag:", round(RSK, 2), "mm"
          )
        } else {
          if (input$Monat == "") {
            ~paste(
              "Jahr:", Jahr,
              "<br>Niederschlag:", round(RSK, 2), "mm")
          } else {
            ~paste(
              input$Monat, Jahr,
              "<br>Niederschlag:", round(RSK, 2), "mm")
          }
      
        }
      )
    if (input$monatOption != "all") {
      fig <- fig %>%
        add_trace(
          x = ~ Day,
          y = ~ SHK_TAG,
          name = "Schneehöhe",
          type = "scatter",
          mode = 'lines',
          line = list(color = 'white', width = 1),
          hoverinfo = "text",
          text = ~paste(
            "Datum:", format(Day, "%d.%m.%Y"),
            "<br>Schneehöhe:", round(SHK_TAG, 2), "cm"
          )
        ) %>%
        add_trace(
          x = ~ Day,
          y = ~ Cumulative_RSK,
          name = "Kumulativer Niederschlag",
          type = 'scatter',
          mode = 'lines',
          line = list(color = 'darkblue', width = 2),
          yaxis = "y2",
          hoverinfo = "text",
          text = if (input$monatOption != "all") {
            ~paste(
              "Datum:", format(Day, "%d.%m.%Y"),
              "<br>Kumulativer Niederschlag:", round(Cumulative_RSK, 2), "mm"
            )
          }
        ) %>%
        add_trace(
          x = ~ Day,
          y = ~ Cumulative_avg_RSK,
          name = "Langj. Mittel",
          type = 'scatter',
          mode = 'lines',
          line = list(color = 'green', width = 1),
          yaxis = "y2",
          hoverinfo = "text",
          text = ~paste(
            "Datum:", format(Day, "%d.%m.%Y"),
            "<br>Kumulativer Niederschlag:", round(Cumulative_avg_RSK, 2), "mm"
          ))
        
    }
      
      fig <- fig %>%
      layout(
        title = list(text = plot_title, x = 0.05, font = list(color = "white", size = 21, family = "Arial")),
        xaxis = list(
          title = "",
          tickformat = if (input$monatOption == "all") "%Y" else "%b",
          dtick = if (input$monatOption == "all") "M60" else "M1",
          color = "white",
          tickfont = list(family = "Courier")
        ),
        yaxis = list(
          rangemode = "tozero",
          title = if (input$monatOption == "all") "Jährlicher Niederschlag (mm)" else "Täglicher Niederschlag (mm)<br> Schneehöhe (cm)",
          color = "white",
          tickfont = list(family = "Courier"),
          side = "left"
        ),
        yaxis2 = list(
          rangemode = "tozero",
          constraintoward='bottom',
          title = "Kumulativer Niederschlag (mm)",
          color = "white",
          overlaying = "y",
          side = "right",
          showgrid = F,
          tickfont = list(family = "Courier")
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2,
          yanchor = "top",
          font = list(color = "white")
        ),
        barmode = 'overlay',
        paper_bgcolor = "#222",
        plot_bgcolor = "#222",
        font = list(color = "white", family = "Arial"),
        margin = list(
          l = 50,
          r = 50,
          t = 50,
          b = 50
        ),
        hoverlabel = list(bgcolor = "white", font = list(color = "black"))
      ) %>%
      config(locale = 'de')
    
    fig
  })

## Sekundärer Niederschlagsplot=================================================  
  output$PrecipPlot2 <- renderPlot({
    df <- filtered_data()
    
    if (input$monatOption == "alleMonate") {
      date_breaks <- "1 month"
      date_labels <- "%b"
    } else if (input$monatOption == "einMonat"){
      date_breaks <- "1 day"
      date_labels <- "%d"
    } else {
      date_breaks <- "5 year"
      date_labels <- "%Y"
      df <- df %>% filter(RSK != 0)
    }
    
    # Custom transformation function
    precip_trans <- function() {
      scales::trans_new(
        name = "precip",
        transform = function(x) ifelse(x == 0, 0, log1p(x)),
        inverse = function(x) expm1(x),
        breaks = function(x) {
          brks <- c(0, 0.1, 0.5, 1, 2, 5, 10, 20, 50)
          brks[brks <= max(x)]
        }
      )
    }
    
    max_precip <- max(df$RSK, na.rm = TRUE)
    
    if (input$monatOption != "all") {
      fill_scale <- scale_fill_gradientn(
        colors = rev(hcl.colors(20, "YlGnBu")),
        name = "Niederschlag (mm)",
        trans = precip_trans(),
        breaks = c(0, 0.1, 0.5, 1, 2, 5, 10, 20, 50),
        labels = c(0, 0.1, 0.5, 1, 2, 5, 10, 20, 50),
        limits = c(0, max_precip),
        na.value = "grey50"
      )
    } else {
      fill_scale <- scale_fill_gradientn(
        colors = rev(hcl.colors(20, "YlGnBu")),
        name = "Niederschlag (mm)",
        trans = "log",
        breaks = scales::breaks_log(n = 5),
        labels = scales::label_number(accuracy = 0.1),
        limits = c(min(df$RSK[df$RSK > 0], na.rm = TRUE), max_precip),
        na.value = "grey50"
      )
    }
    
    plot <- ggplot(df, aes(Day, NA, fill = RSK)) +
      geom_tile() +
      fill_scale +
      scale_x_date(date_breaks = date_breaks,
                   date_labels = date_labels,
                   name = "",
                   expand = c(0.01, 0)) +
      theme(
        text = element_text(colour = "white"),
        plot.title = element_text(size = 16, family = "Arial"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = "white", family = "Courier", size = 12),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "black"),
        legend.title = element_text(colour = "white"),
        plot.background  = element_rect(fill = '#222222', colour = "transparent"),
        panel.background = element_rect(fill = '#222222'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.margin = margin(0,0,0,0),
        legend.box.spacing = unit(0, "pt"),
        legend.key.width = unit(3,"cm")
      )
    if (input$monatOption != "all") {
      plot <- plot + 
        theme(plot.margin = unit(c(0,1.4,0,1.8), "cm"))
    } else {
      plot <- plot + 
        theme(plot.margin = unit(c(0,1.5,0,1.35), "cm"))
    }
    plot
  })
  
## Primärer Sonnenstundenplot===================================================  
  output$sunPlot <- renderPlotly({
    df <- filtered_data()
    lat <- ifelse(is.null(selectedLat()), 52.4537, selectedLat())
    maxSun <- calculate_max_sunshine_duration(lat, input$Jahr)
    
    df <- df %>% 
      filter(SDK != -999)
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    if (input$monatOption == "alleMonate") {
      date_breaks <- "1 month"
      date_labels <- "%b"
      plot_title <- paste0("Sonnenscheindauer ", as.character(input$Jahr))
      x_col <- ~Day

    } else if (input$monatOption == "einMonat") {
      date_breaks <- "1 day"
      date_labels <- "%d"
      plot_title <- paste0("Sonnenscheindauer ", input$Monat, " ", as.character(input$Jahr))
      x_col <- ~Day
      maxSun <- maxSun %>%
        filter(Monat == input$Monat)
      
    } else {
      date_breaks <- "5 year"
      date_labels <- "%Y"
      x_col <- ~Jahr
      df <- df %>%
        filter(SDK > 0)
    }
    
    if (input$monatOption != "all") {
      df <- df %>%
        arrange(Day) %>%
        mutate(Cumulative_SDK = cumsum(SDK))
    } else {
      df <- df %>%
        filter(SDK !=0) %>%
        arrange(Jahr) %>%
        mutate(Cumulative_SDK = cumsum(SDK))
      plot_title <- ifelse(input$Monat == "",
                           paste0("Jährliche Sonnenscheindauer ", min(df$Jahr), "–", max(df$Jahr)),
                           paste0("Sonnenscheindauer für ", input$Monat, " im Zeitraum ",  min(df$Jahr), "–", max(df$Jahr)))
    }
    
    fig <- plot_ly(df) %>%
      add_bars(
        x = ~ Day,
        y = ~ SDK,
        name = "Sonnenschein",
        marker = list(color = "yellow", opacity = 0.8),
        hoverinfo = "text",
        textposition = "none",
        text = if (input$monatOption != "all") {
          ~paste(
            "Datum:", format(Day, "%d.%m.%Y"),
            "<br>Sonnenschein:", round(SDK, 2), "Stunden"
          )
        } else {
          ~paste(
            "Jahr:", Jahr,
            "<br>Sonnenschein:", round(SDK, 2), "Stunden"
          )
        }
      )
    if (input$monatOption != "all") {
      fig <- fig %>%
      add_trace(
        x = ~ Day,
        y = ~ Cumulative_SDK,
        name = "Kumulativer Sonnenschein",
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'orange', width = 2),
        yaxis = "y2",
        hoverinfo = "text",
        text = ~paste(
          "Datum:", format(Day, "%d.%m.%Y"),
          "<br>Kumulativer Sonnenschein:", round(Cumulative_SDK, 2), "Stunden"
        )
      ) %>%
      add_trace(data = maxSun, 
                x = ~ date,
                y = ~ sunshine_duration,
                name = "Maximal mögliche Sonnenscheindauer",
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'grey', width = 1),
                yaxis = "y1",
                hoverinfo = "text",
                text = ~paste("Datum:", format(date, "%d.%m.%Y"), "<br>", sunshine_duration, "Stunden")
                )  
      
    }
      
    fig <- fig %>%
      layout(
        title = list(text = plot_title, x = 0.05, font = list(color = "white", size = 21, family = "Arial")),
        xaxis = list(
          title = "",
          tickformat = if (input$monatOption == "all") "%Y" else "%b",
          dtick = if (input$monatOption == "all") "M60" else "M1",
          color = "white",
          tickfont = list(family = "Courier")
        ),
        yaxis = list(
          title = if (input$monatOption == "all") "Jährlicher Sonnenschein (Stunden)" else "Täglicher Sonnenschein (Stunden)",
          color = "white",
          tickfont = list(family = "Courier"),
          side = "left"
        ),
        yaxis2 = list(
          rangemode = "tozero",
          constraintoward='bottom',
          title = "Kumulativer Sonnenschein (Stunden)",
          color = "white",
          overlaying = "y",
          side = "right",
          showgrid = F,
          tickfont = list(family = "Courier")
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2,
          yanchor = "top",
          font = list(color = "white")
        ),
        barmode = 'overlay',
        paper_bgcolor = "#222",
        plot_bgcolor = "#222",
        font = list(color = "white", family = "Arial"),
        margin = list(
          l = 50,
          r = 50,
          t = 50,
          b = 50
        ),
        hoverlabel = list(bgcolor = "white", font = list(color = "black"))
      ) %>%
      config(locale = 'de')
    
    fig
  })
  
## Sekundärer Sonnenstundenplot=================================================  
  output$SunPlot2 <- renderPlot({
    df <- filtered_data()
    
    if (input$monatOption == "alleMonate") {
      date_breaks <- "1 month"
      date_labels <- "%b"
    } else if (input$monatOption == "einMonat"){
      date_breaks <- "1 day"
      date_labels <- "%d"
    } else {
      date_breaks <- "5 year"
      date_labels <- "%Y"
      df <- df %>% filter(SDK != 0)
    }
    
    plot <- ggplot(df, aes(Day, NA, fill = NM)) +
      geom_tile() +
      scale_fill_gradientn(colors = hcl.colors(20, "Blues 3"), name = "Bedeckungsgrad (Achtel)") +
      scale_x_date(date_breaks = date_breaks,
                   date_labels = date_labels,
                   name = "",
                   expand = c(0.01, 0)) +
      theme(
        text = element_text(colour = "white"),
        plot.title = element_text(size = 16, family = "Arial"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = "white", family = "Courier", size = 12),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "black"),
        legend.title = element_text(colour = "white"),
        plot.background  = element_rect(fill = '#222222', colour = "transparent"),
        panel.background = element_rect(fill = '#222222'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.margin=margin(0,0,0,0),
        legend.box.spacing = unit(0, "pt"),
        legend.key.width=unit(3,"cm")
      )
    if (input$monatOption != "all") {
      plot <- plot + 
        theme(plot.margin = unit(c(0,1.55,0,1.45), "cm"))
    } else {
      plot <- plot + 
        theme(plot.margin = unit(c(0,1.45,0,1.55), "cm"))
    }
    plot
  })

## Niederschlagskacheln=========================================================    
  output$precipTiles <- renderUI({
    df <- filtered_data()
    climateData <- climateData()
    
    year_range <- get_year_range(input$Period)
    
    if (input$monatOption == "alleMonate") {
      title <- paste0("Niederschlagsübersicht ", as.character(input$Jahr))
      subtitle <- paste("Abweichung vom Langjährigen Mittel", input$Period)
      sum_values <- df %>%
        summarise(
          RSK_sum = sum(RSK, na.rm = TRUE),
          Regentage = sum(RSK >= 0.1, na.rm = TRUE),
          Schneetage = sum(Schneetag, na.rm = TRUE)
        )
      monthly_mean_pr <- mean_over_time(climateData, year_range$start, year_range$end, RSK, is_day = T)
      pr_diff <- round(sum_values$RSK_sum - monthly_mean_pr, 1)
      monthly_mean_rt <- mean_over_time(climateData, year_range$start, year_range$end, Regentag, is_day = T)
      rt_diff <- round(sum_values$Regentage - monthly_mean_rt, 1)
      monthly_mean_sdt <- mean_over_time(climateData, year_range$start, year_range$end, Schneetag, is_day = T)
      sdt_diff <- round(sum_values$Schneetage - monthly_mean_sdt, 1)
      
      left_tile <- list(
        title = "Summe",
        value = paste(round(sum_values$RSK_sum, 1), "mm"),
        icon = "cloud-rain",
        width = 110,
        pr_diff = pr_diff
      )
      middle_tile <- list(
        title = "Regentage",
        value = sum_values$Regentage,
        width = 110,
        icon = "umbrella",
        rt_diff = rt_diff
      )
      right_tile <- list(
        title = "Schneetage",
        value = sum_values$Schneetage,
        width = 110,
        icon = "snowflake",
        sdt_diff = sdt_diff
      )
    } else if (input$monatOption == "einMonat") {
      title <- paste0("Niederschlagsübersicht ", input$Monat, " ", as.character(input$Jahr))
      subtitle <- paste("Abweichung vom Langjährigen Mittel", input$Period)
      sum_values <- df %>%
        summarise(
          RSK_sum = sum(RSK, na.rm = TRUE),
          Regentage = sum(RSK >= 0.1, na.rm = TRUE),
          Schneetage = sum(Schneetag, na.rm = TRUE)
        )
      monthly_mean_pr <- mean_over_time(climateData, year_range$start, year_range$end, RSK, is_month = T, month_filter = input$Monat, is_day = T)
      pr_diff <- round(sum_values$RSK_sum - monthly_mean_pr, 1)
      monthly_mean_rt <- mean_over_time(climateData, year_range$start, year_range$end, Regentag, is_month = T, month_filter = input$Monat, is_day = T)
      rt_diff <- round(sum_values$Regentage - monthly_mean_rt, 1)
      monthly_mean_sdt <- mean_over_time(climateData, year_range$start, year_range$end, Schneetag, is_month = T, month_filter = input$Monat, is_day = T)
      sdt_diff <- round(sum_values$Schneetage - monthly_mean_sdt, 1)
      
      left_tile <- list(
        title = "Summe",
        value = paste(round(sum_values$RSK_sum, 1), "mm"),
        icon = "cloud-rain",
        width = 110,
        pr_diff = pr_diff
      )
      middle_tile <- list(
        title = "Regentage",
        value = sum_values$Regentage,
        icon = "umbrella",
        width = 110,
        rt_diff = rt_diff
      )
      right_tile <- list(
        title = "Schneetage",
        value = sum_values$Schneetage,
        width = 110,
        icon = "snowflake",
        sdt_diff = sdt_diff
      )
    } else {
      title <- ifelse(input$Monat == "",
                      paste0("Durchschnittswerte im Zeitraum ", min(df$Jahr), "–", max(df$Jahr)),
                      paste0("Durchschnittswerte für ", input$Monat, " im Zeitraum ",  min(df$Jahr), "–", max(df$Jahr)))
      subtitle <- NULL
      avg_values <- df %>%
        filter(RSK != 0) %>%
        summarise(
          Avg_RSK_sum = mean(RSK, na.rm = TRUE),
          Avg_Regentage = mean(Regentage, na.rm = TRUE),
          Avg_Schneetage = mean(Schneetage, na.rm = TRUE)
        )
      left_tile <- list(
        title = ifelse(input$Monat == "", "Ø Niederschlagssumme/Jahr",paste0("Ø Niederschlagssumme/", input$Monat)), 
        value = paste(round(avg_values$Avg_RSK_sum, 1), "mm"),
        width = 200,
        icon = "cloud-rain"
      )
      middle_tile <- list(
        title = "Ø Regentage/Jahr",
        value = round(avg_values$Avg_Regentage, 1),
        width = 200,
        icon = "umbrella"
      )
      right_tile <- list(
        title = "Ø Schneedeckentage/Jahr",
        value = round(avg_values$Avg_Schneetage, 1),
        width = 200,
        icon = "snowflake"
      )
    }
    
    tagList(
      tags$style(custom_tile_css),
      tags$style(
        "
      .tile-container { display: flex; flex-wrap: nowrap; justify-content: center; overflow-x: auto; }
      .tile-group { display: flex; flex-direction: column; align-items: center; }
      .tiles { display: flex; flex-wrap: nowrap; }
    "
      ),
      tags$div(
        class = "tile-container",
        tags$div(
          class = "tile-group",
          tags$h6(title, style = "margin-bottom: 1px; text-align: center;"),
          tags$p(subtitle, style = "margin-bottom: 7px; text-align:center; font-size: 9px;"),
          tags$div(
            class = "tiles",
            create_tile(title = left_tile$title, value = left_tile$value, icon_name = left_tile$icon, color = "#3498db", width = left_tile$width, tooltip = "Summe des jährlichen Niederschlags", diff_value = left_tile$pr_diff, is_rain = T),
            create_tile(title = middle_tile$title, value = middle_tile$value, icon_name =  middle_tile$icon, color = "#2980b9", width = middle_tile$width, tooltip = "Tage mit einer Niederschlagshöhe von mindestens 0.1 mm", diff_value = middle_tile$rt_diff),
            create_tile(title = right_tile$title, value = right_tile$value, icon_name =  right_tile$icon, color = "#bcbcbc", width = right_tile$width, tooltip = "Tage mit einer Schneehöhe von mindestens 1 cm", diff_value = right_tile$sdt_diff)
          )
        )
      )
    )
  })
  
## Sonnenscheinkacheln==========================================================
  output$sunTiles <- renderUI({
    df <- filtered_data()
    climateData <- climateData()
    
    year_range <- get_year_range(input$Period)
    
    if (input$monatOption == "alleMonate") {
      title <- paste0("Sonnenscheinübersicht ", as.character(input$Jahr))
      subtitle <- paste("Abweichung vom Langjährigen Mittel", input$Period)
      sum_values <- df %>%
        summarise(
          SDK_sum = sum(SDK, na.rm = TRUE),
          Heiteretage = sum(Heiterertag, na.rm = TRUE),
          Truebetage = sum(Truebertag, na.rm  = TRUE)
        )
      monthly_mean_su <- mean_over_time(climateData, year_range$start, year_range$end, SDK, is_day = T)
      su_diff <- round(sum_values$SDK_sum - monthly_mean_su, 1)
      monthly_mean_he <- mean_over_time(climateData, year_range$start, year_range$end, Heiterertag, is_day = T)
      he_diff <- round(sum_values$Heiteretage - monthly_mean_he, 1)
      monthly_mean_tr <- mean_over_time(climateData, year_range$start, year_range$end, Truebertag, is_day = T)
      tr_diff <- round(sum_values$Truebetage - monthly_mean_tr, 1)
      
      
      left_tile <- list(
        title = "Summe",
        value = paste(round(sum_values$SDK_sum, 1), "h"),
        icon = "sun",
        width = 110,
        su_diff = su_diff
      )
      middle_tile <- list(
        title = "Heitere Tage",
        value = sum_values$Heiteretage,
        width = 110,
        icon = "cloud",
        he_diff = he_diff
      )
      right_tile <- list(
        title = "Trübe Tage",
        value = sum_values$Truebetage,
        width = 110,
        icon = "cloud",
        tr_diff = tr_diff
      )
    } else if (input$monatOption == "einMonat") {
      title <- paste0("Sonnenscheinübersicht ", input$Monat, " ", as.character(input$Jahr))
      subtitle <- paste("Abweichung vom Langjährigen Mittel", input$Period)
      sum_values <- df %>%
        summarise(
          SDK_sum = sum(SDK, na.rm = TRUE),
          Heiteretage = sum(Heiterertag, na.rm = TRUE),
          Truebetage = sum(Truebertag, na.rm = TRUE)
        )
      monthly_mean_su <- mean_over_time(climateData, year_range$start, year_range$end, SDK, is_month = T, month_filter = input$Monat, is_day = T)
      su_diff <- round(sum_values$SDK_sum - monthly_mean_su, 1)
      monthly_mean_he <- mean_over_time(climateData, year_range$start, year_range$end, Heiterertag, is_month = T, month_filter = input$Monat, is_day = T)
      he_diff <- round(sum_values$Heiteretage - monthly_mean_he, 1)
      monthly_mean_tr <- mean_over_time(climateData, year_range$start, year_range$end, Truebertag, is_month = T, month_filter = input$Monat, is_day = T)
      tr_diff <- round(sum_values$Truebetage - monthly_mean_tr, 1)
      
      left_tile <- list(
        title = "Summe",
        value = paste(round(sum_values$SDK_sum, 1), "h"),
        icon = "sun",
        width = 110,
        su_diff = su_diff
      )
      middle_tile <- list(
        title = "Heitere Tage",
        value = sum_values$Heiteretage,
        width = 110,
        icon = "cloud",
        he_diff = he_diff
      )
      right_tile <- list(
        title = "Trübe Tage",
        value = sum_values$Truebetage,
        width = 110,
        icon = "cloud",
        tr_diff = tr_diff
      )
    } else {
      
      subtitle <- NULL
      print(df$SDK)
      avg_values <- df %>%
        filter(SDK != 0) %>%
        summarise(
          Avg_SDK_sum = mean(SDK, na.rm = TRUE),
          Avg_Heiteretage = mean(Heiteretage, na.rm = TRUE),
          Avg_Truebetage = mean(Truebetage, na.rm = TRUE)
        )
      df <- df %>% filter (SDK != 0)
      title <- ifelse(input$Monat == "",
                      paste0("Durchschnittswerte im Zeitraum ", min(df$Jahr), "–", max(df$Jahr)),
                      paste0("Durchschnittswerte für ", input$Monat, " im Zeitraum ",  min(df$Jahr), "–", max(df$Jahr)))
      left_tile <- list(
        title = ifelse(input$Monat == "", "Ø Sonnenscheinstunden/Jahr",paste0("Ø Sonnenscheinstunden/", input$Monat)), 
        value = paste(round(avg_values$Avg_SDK_sum, 1), "h"),
        width = 200,
        icon = "sun"
      )
      middle_tile <- list(
        title = ifelse(input$Monat == "", "Ø Heitere Tage/Jahr",paste0("Ø Heitere Tage/", input$Monat)),
        value = round(avg_values$Avg_Heiteretage, 1),
        width = 200,
        icon = "sun"
      )
      right_tile <- list(
        title = ifelse(input$Monat == "", "Ø Trübe Tage/Jahr",paste0("Ø Trübe Tage/", input$Monat)),
        value = round(avg_values$Avg_Truebetage, 1),
        width = 200,
        icon = "cloud"
      )
    }
    
    tagList(
      tags$style(custom_tile_css),
      tags$style(
        "
      .tile-container { display: flex; flex-wrap: nowrap; justify-content: center; overflow-x: auto; }
      .tile-group { display: flex; flex-direction: column; align-items: center; }
      .tiles { display: flex; flex-wrap: nowrap; }
    "
      ),
      tags$div(
        class = "tile-container",
        tags$div(
          class = "tile-group",
          tags$h6(title, style = "margin-bottom: 1px; text-align: center;"),
          tags$p(subtitle, style = "margin-bottom: 7px; text-align:center; font-size: 9px;"),
          tags$div(
            class = "tiles",
            create_tile(title = left_tile$title, value = left_tile$value, icon_name = left_tile$icon, color = "#f1c232", width = left_tile$width, tooltip = "Summe der Sonnenscheinstunden", diff_value = left_tile$su_diff),
            create_tile(title = middle_tile$title, value = middle_tile$value, icon_name =  middle_tile$icon, color = "#0072FB", width = middle_tile$width, tooltip = "Tage mit einem Bedeckungsgrad < 1,6 Achtel", diff_value = middle_tile$he_diff),
            create_tile(title = right_tile$title, value = right_tile$value, icon_name =  right_tile$icon, color = "#999999", width = right_tile$width, tooltip = "Tage mit einem Bedeckungsgrad > 6,4 Achtel", diff_value = right_tile$tr_diff)
          )
        )
      )
    )
  })

## Temperaturkacheln    
  output$allTiles <- renderUI({
    df <- filtered_data()
    climateData <- climateData()
    year_range <- get_year_range(input$Period)
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    avg_values <- df %>%
      summarise(
        TNK_avg = mean(TNK, na.rm = TRUE),
        TMK_avg = mean(TMK, na.rm = TRUE),
        TXK_avg = mean(TXK, na.rm = TRUE)
      )
    
    if (input$monatOption == "alleMonate") {
      avg_title <- paste0("Durchschnittstemperaturen ", as.character(input$Jahr))
      char_title <- paste0("Temperaturkenntage ", as.character(input$Jahr))
      subtitle <- paste("Abweichung vom Langjährigen Mittel", input$Period)
      
      sum_values <- df %>%
        summarise(
          Sommertage = sum(TXK >= 25, na.rm = TRUE),
          Hitzetage = sum(TXK >= 30, na.rm = TRUE),
          Tropennaechte = sum(TNK >= 20, na.rm = TRUE),
          Frosttage = sum(TNK < 0, na.rm = TRUE),
          Eistage = sum(TXK < 0, na.rm = TRUE)
        )
      
      monthly_mean_temp <- mean_over_time(climateData, year_range$start, year_range$end, TMK)
      temp_diff <- round(avg_values$TMK_avg - monthly_mean_temp, 1)
      monthly_high_temp <- mean_over_time(climateData, year_range$start, year_range$end, TXK)
      high_temp_diff <- round(avg_values$TXK_avg - monthly_high_temp, 1)
      monthly_low_temp <- mean_over_time(climateData, year_range$start, year_range$end, TNK)
      low_temp_diff <- round(avg_values$TNK_avg - monthly_low_temp, 1)
      monthly_mean_st <- mean_over_time(climateData, year_range$start, year_range$end, Sommertag, is_day = T)
      st_diff <- round(sum_values$Sommertage - monthly_mean_st, 1)
      monthly_mean_ht <- mean_over_time(climateData, year_range$start, year_range$end, Hitzetag, is_day = T)
      ht_diff <- round(sum_values$Hitzetage - monthly_mean_ht, 1)
      monthly_mean_ft <- mean_over_time(climateData, year_range$start, year_range$end, Frosttag, is_day = T)
      ft_diff <- round(sum_values$Frosttage - monthly_mean_ft, 1)
      monthly_mean_et <- mean_over_time(climateData, year_range$start, year_range$end, Eistag, is_day = T)
      et_diff <- round(sum_values$Eistage - monthly_mean_et, 1)
      monthly_mean_tn <- mean_over_time(climateData, year_range$start, year_range$end, Tropennacht, is_day = T)
      tn_diff <- round(sum_values$Tropennaechte - monthly_mean_tn, 1)
      
    } else if (input$monatOption == "einMonat") {
      avg_title <- paste0("Durchschnittstemperaturen ", input$Monat, " ", as.character(input$Jahr))
      char_title <- paste0("Temperaturkenntage ", input$Monat, " ", as.character(input$Jahr))
      subtitle <- paste("Abweichung vom Langjährigen Mittel", input$Period)
      
      sum_values <- df %>%
        summarise(
          Sommertage = sum(TXK >= 25, na.rm = TRUE),
          Hitzetage = sum(TXK >= 30, na.rm = TRUE),
          Tropennaechte = sum(TNK >= 20, na.rm = TRUE),
          Frosttage = sum(TNK < 0, na.rm = TRUE),
          Eistage = sum(TXK < 0, na.rm = TRUE)
        )
      
      monthly_mean_temp <- mean_over_time(climateData, year_range$start, year_range$end, TMK, is_month = T, month_filter = input$Monat)
      temp_diff <- round(avg_values$TMK_avg - monthly_mean_temp, 1)
      monthly_high_temp <- mean_over_time(climateData, year_range$start, year_range$end, TXK, is_month = T, month_filter = input$Monat)
      high_temp_diff <- round(avg_values$TXK_avg - monthly_high_temp, 1)
      monthly_low_temp <- mean_over_time(climateData, year_range$start, year_range$end, TNK, is_month = T, month_filter = input$Monat)
      low_temp_diff <- round(avg_values$TNK_avg - monthly_low_temp, 1)
      monthly_mean_st <- mean_over_time(climateData, year_range$start, year_range$end, Sommertag, is_month = T, month_filter = input$Monat, is_day = T)
      st_diff <- round(sum_values$Sommertage - monthly_mean_st, 1)
      monthly_mean_ht <- mean_over_time(climateData, year_range$start, year_range$end, Hitzetag, is_month = T, month_filter = input$Monat, is_day = T)
      ht_diff <- round(sum_values$Hitzetage - monthly_mean_ht, 1)
      monthly_mean_ft <- mean_over_time(climateData, year_range$start, year_range$end, Frosttag, is_month = T, month_filter = input$Monat, is_day = T)
      ft_diff <- round(sum_values$Frosttage - monthly_mean_ft, 1)
      monthly_mean_et <- mean_over_time(climateData, year_range$start, year_range$end, Eistag, is_month = T, month_filter = input$Monat, is_day = T)
      et_diff <- round(sum_values$Eistage - monthly_mean_et, 1)
      monthly_mean_tn <- mean_over_time(climateData, year_range$start, year_range$end, Tropennacht, is_month = T, month_filter = input$Monat, is_day = T)
      tn_diff <- round(sum_values$Tropennaechte - monthly_mean_tn, 1)
      
    } else {
      avg_title <- paste0("Durchschnittstemperaturen ", min(df$Jahr), "-", max(df$Jahr))
      char_title <- paste0("Durchschnitt Temperaturkenntage ", min(df$Jahr), "-", max(df$Jahr))
      subtitle <- NULL
      sum_values <- df %>%
        summarise(
          Sommertage = round(mean(Sommertage, na.rm = TRUE), 1),
          Hitzetage = round(mean(Hitzetage, na.rm = TRUE), 1),
          Tropennaechte = round(mean(Tropennaechte, na.rm = T), 1),
          Frosttage = round(mean(Frosttage, na.rm = T), 1),
          Eistage = round(mean(Eistage, na.rm = T), 1)
        )
      
      temp_diff <- NULL
      high_temp_diff <- NULL
      low_temp_diff <- NULL
      st_diff <- NULL
      ht_diff <- NULL
      ft_diff <- NULL
      et_diff <- NULL
      tn_diff <- NULL
      
    }
    
    tagList(
      tags$style(custom_tile_css),
      tags$style(
        "
      .tile-container { display: flex; flex-wrap: nowrap; justify-content: center; overflow-x: auto; }
      .tile-group { display: flex; flex-direction: column; align-items: center; }
      .tiles { display: flex; flex-wrap: nowrap; }
    "
      ),
      tags$div(
        class = "tile-container",
        tags$div(
          class = "tile-group",
          tags$h6(avg_title, style = "margin-bottom: 1px; text-align: center;"),
          tags$p(subtitle, style = "margin-bottom: 7px; text-align:center; font-size: 9px;"),
          tags$div(
            class = "tiles",
            create_tile(
              title = "Tagestiefstwerte",
              value = paste(round(avg_values$TNK_avg, 1), "°C"),
              icon_name = "thermometer-quarter",
              color = "#1a5f7a",
              tooltip = "Tagesminimum der Lufttemperatur in 2m Höhe",
              diff_value = low_temp_diff, 
              is_temperature = T
            ),
            create_tile(
              title = "Tagesmittelwerte",
              value = paste(round(avg_values$TMK_avg, 1), "°C"),
              icon_name = "thermometer-half",
              color = "#2c3e50",
              tooltip = "Tagesmittel der Temperatur",
              diff_value = temp_diff, 
              is_temperature = T
            ),
            create_tile(
              title = "Tageshöchstwerte",
              value = paste(round(avg_values$TXK_avg, 1), "°C"),
              icon_name = "thermometer-full",
              color = "#6e2c00",
              tooltip = "Tagesmaximum der Lufttemperatur in 2m Höhe",
              diff_value = high_temp_diff,
              is_temperature = T
            )
          )
        ),
        if (!is.null(sum_values)) {
          tags$div(
            style = "width: 2px; background-color: #888; margin: 0 15px;"
          )
        },
        if (!is.null(sum_values)) {
          tags$div(
            class = "tile-group",
            tags$h6(char_title, style = "margin-bottom: 1px; text-align: center;"),
            tags$p(subtitle, style = "margin-bottom: 7px; text-align:center; font-size: 9px;"),
            tags$div(
              class = "tiles",
              create_tile(title = "Sommertage", value = sum_values$Sommertage, icon_name = "sun", color = "#ff9800", tooltip = "Tag, an dem das Maximum der Lufttemperatur ≥ 25 °C beträgt", diff_value = st_diff),
              create_tile(title = "Heiße Tage", value = sum_values$Hitzetage, icon_name = "fire", color = "#e74c3c", tooltip = "Tag, an dem das Maximum der Lufttemperatur ≥ 30 °C beträgt", diff_value = ht_diff),
              create_tile(title = "Tropennächte", value = sum_values$Tropennaechte, icon_name =  "moon", color = "#8e44ad", tooltip = "Nacht in der das Minimum der Lufttemperatur ≥ 20 °C beträgt", diff_value = tn_diff),
              create_tile(title = "Frosttage", value = sum_values$Frosttage, icon_name = "snowflake", color = "#3498db", tooltip = "Tag, an dem das Minimum der Lufttemperatur unterhalb des Gefrierpunktes (0 °C) liegt", diff_value = ft_diff),
              create_tile(title = "Eistage", value = sum_values$Eistage, icon_name = "icicles", color = "#2980b9", tooltip = "Tag, an dem das Maximum der Lufttemperatur unterhalb des Gefrierpunktes (0 °C) liegt", diff_value = et_diff)
            )          
          )
        }
      )
    )
  })

## Datentabelle=================================================================    
  output$table <- DT::renderDataTable({
    if (input$dataTableOption != "averaged") {
      df <- climateData()
      df <- df %>%
        select(Day, TNK, TMK, TXK, RSK, SDK, NM) %>%
        transmute(
          Datum = Day,
          Tagesttiefsttemperatur = TNK,
          Tagesmitteltemperatur = TMK,
          !!enc2utf8("Tageshöchsttemperatur") := TXK,
          Niederschlag = RSK,
          Sonnenscheindauer = SDK,
          Bedeckungsgrad = NM,
          Temperaturspanne = round(TXK-TNK, 1)
        )
    }
    else {
      df <- climateData()
      df$Day2 <- df$Day
      df <- df %>%
        {if (input$dataTableMonat != "") filter(., Monat_german == input$dataTableMonat) else .} %>%
        group_by(Jahr) %>%
        summarise(
          TXK = mean(TXK, na.rm = TRUE),
          TMK = mean(TMK, na.rm = TRUE),
          TNK = mean(TNK, na.rm = TRUE),
          RSK = sum(RSK, na.rm = TRUE),
          SDK = sum(SDK, na.rm = TRUE),
          NM = mean(NM, na.rm = TRUE),
          Regentage = sum(Regentag == 1, na.rm = TRUE),
          Sommertage = sum(Sommertag == 1, na.rm = T),
          Hitzetage = sum(Hitzetag == 1, na.rm = T),
          Tropennaechte = sum(Tropennacht == 1, na.rm = T),
          Frosttage = sum(Frosttag == 1, na.rm = T),
          Eistage = sum(Eistag == 1, na.rm = T),
          Schneetage = sum(Schneetag == 1, na.rm = TRUE),
          Heiteretage = sum(Heiterertag == 1, na.rm = T),
          Truebetage = sum(Truebertag == 1, na.rm = T),
          Day = unique(Jahr),
          Maximale_Trockenperiode = {
            rle_result <- rle(Regentag)
            max(rle_result$lengths[rle_result$values == 0], na.rm = TRUE)
          },
          Maximale_Regenperiode = {
            rle_result <- rle(Regentag)
            max(rle_result$lengths[rle_result$values == 1], na.rm = TRUE)
          }
        ) %>%
        arrange(Jahr) %>%
        mutate(Day = as.Date(paste(Jahr, "01", "01", sep = "-")))
      
      if (input$dataTableMonat == "") {
        df <- df %>%
          select(Day, TNK, TMK, TXK, RSK, SDK, NM, Sommertage, Hitzetage, Tropennaechte, Frosttage, Eistage, Regentage, Schneetage, Heiteretage, Truebetage, Maximale_Trockenperiode, Maximale_Regenperiode) %>%
          transmute(Jahr = year(Day),
                    Tagesttiefsttemperatur = round(TNK, 1),
                    Tagesmitteltemperatur = round(TMK, 1),
                    !!enc2utf8("Tageshöchsttemperatur") := round(TXK, 1),
                    Niederschlag = round(RSK, 1),
                    Sonnenscheindauer = round(SDK, 1),
                    Bedeckungsgrad = round(NM, 1),
                    Sommertage = Sommertage,
                    Hitzetage = Hitzetage,
                    !!enc2utf8("Tropennächte") := Tropennaechte,
                    Frosttage = Frosttage,
                    Eistage = Eistage,
                    !!enc2utf8("Heitere Tage") := Heiteretage,
                    !!enc2utf8("Trübe Tage") := Truebetage,
                    Regentage = Regentage,
                    Schneetage = Schneetage,
                    Maximale_Trockenperiode = Maximale_Trockenperiode,
                    Maximale_Regenperiode = Maximale_Regenperiode)
      }
      else {
        df <- df %>%
          select(Day, TNK, TMK, TXK, RSK, SDK, NM, Sommertage, Hitzetage, Tropennaechte, Frosttage, Eistage, Regentage, Schneetage, Heiteretage, Truebetage) %>%
          transmute(Monat_Jahr = paste(input$dataTableMonat, year(Day)),
                    Tagesttiefsttemperatur = round(TNK, 1),
                    Tagesmitteltemperatur = round(TMK, 1),
                    !!enc2utf8("Tageshöchsttemperatur") := round(TXK, 1),
                    Niederschlag = round(RSK, 1),
                    Sonnenscheindauer = round(SDK, 1),
                    Bedeckungsgrad = round(NM, 1),
                    Sommertage = Sommertage,
                    Hitzetage = Hitzetage,
                    !!enc2utf8("Tropennächte") := Tropennaechte,
                    Frosttage = Frosttage,
                    Eistage = Eistage,
                    Regentage = Regentage,
                    Schneetage = Schneetage,
                    !!enc2utf8("Heitere Tage") := Heiteretage,
                    !!enc2utf8("Trübe Tage") := Truebetage)
      }
      
    }
    
    datatable(df, 
              extensions = c("Buttons", "FixedHeader", "SearchBuilder", "DateTime"),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                autoWidth = FALSE,
                buttons = list('copy', 'csv', 'excel', list(extend = 'pdf', orientation = 'landscape', pageSize = 'A3'), 'print'),
                fixedHeader = TRUE,
                searchBuilder = TRUE,
                dom = '<"top"Ql>Bfrtip',
                initComplete = JS("
                function(settings, json) {
                  $(this.api().table().container()).css({'font-size': '70%'});
                  $('.dataTables_filter input').css({'font-size': '70%'});
                  $('.dataTables_length select').css({'font-size': '70%'});
                  $('.dataTables_info').css({'font-size': '80%'});
                  $('.dataTables_paginate').css({'font-size': '60%'});
                }
              ")
              ),
              class = 'cell-border stripe',
              rownames = FALSE,
              selection = 'none'
    ) %>%
      DT::formatStyle(names(df), lineHeight='40%')  
  }, server = F)
  
## Live Wetterdaten=============================================================
  output$currentWeather <- renderUI({
    if (is.null(selectedLat()) | is.null(selectedLon())) {
      wetter <- callAPI(52.4537, 13.3017)
    } else {
      wetter <- callAPI(selectedLat(), selectedLon())
    }
    
    station <- selectedStation()
    if (!is.null(station)) {
      title <- paste0("Aktuelles Wetter für ", station$name,":")
      subtitle <- paste(wetter$weather)
    } else {
      title <- paste0("Aktuelles Wetter für Berlin Dahlem (FU):")
      subtitle <- paste(wetter$weather)
    }
    
    print(wetter$weather)
    fluidRow(
      column(12, 
             tags$h4(title, style = "text-align: center; margin-bottom: 5px;"),
             tags$h6(wetter$time, style = "text-align: center; margin-bottom: 5px;"),
             tags$h5(subtitle, style = "margin-bottom: 10px; text-align:center;"),
             tags$div(style = "display: flex; flex-wrap: wrap; justify-content: center;",
                      create_weather_tile(title = "Temperatur", value = wetter$temp, icon_name = "thermometer-half", color = "#FF9800", tooltip = "Aktuelle Temperatur", type = "temperature"),
                      create_weather_tile(title = "Luftdruck", value = wetter$press, icon_name = "tachometer-alt", color = "#4CAF50", tooltip = "Atmosphärendruck", type = "pressure"),
                      create_weather_tile(title = "Luftfeuchtigkeit", value = wetter$humid, icon_name = "tint", color = "#2196F3", tooltip = "Relative Luftfeuchtigkeit", type = "humidity"),
                      create_weather_tile(title = "Wind", value = wetter$winddir, icon_name = "compass", color = "#795548", tooltip = "Windrichtung und -Geschwindigkeit", type = "wind_direction", wind_speed = wetter$wind*3.6),
                      if (!is.null(wetter$rain)) create_weather_tile(title = "Regen", type = "precip", value = ifelse(!is.na(wetter$rain), wetter$rain, "0,0"), icon_name = "cloud-rain", color = "#607D8B", tooltip = "Niederschlag pro Stunde") else NULL
             )
      )
    )
  })
  
## Karten=======================================================================
  output$mapDE <- renderLeaflet({

    layers <- list(
      list(
        name = "dwd:TAMM_17_1961_30",
        alias = "Durchschnittstemperatur (1961-1990)",
        unit = "°C",
        needsConversion = TRUE
      ),
      list(
        name = "dwd:TADXMM_17_1961_30",
        alias = "Maximaltemperatur (1961-1990)",
        unit = "°C",
        needsConversion = TRUE
      ),
      list(
        name = "dwd:TADNMM_17_1961_30",
        alias = "Minimaltemperatur (1961-1990)",
        unit = "°C",
        needsConversion = TRUE
      ),
      list(
        name = "dwd:Summerdays_annual_map_normals_1971_30",
        alias = "Sommertage pro Jahr (1971-2000)",
        unit = "Tage",
        needsConversion = FALSE
      ),
      list(
        name = "dwd:Frostdays_annual_map_normals_1971_30",
        alias = "Frosttage pro Jahr (1971-2000)",
        unit = "Tage",
        needsConversion = FALSE
      ),
      list(
        name = "dwd:RSMS_17_1961_30",
        alias = "Niederschlag pro Jahr (1961-1990)",
        unit = "mm",
        needsConversion = FALSE
      ),
      list(
        name = "dwd:Snowdays_annual_map_normals_1971_30",
        alias = "Schneetage pro Jahr (1971-2000)",
        unit = "Tage",
        needsConversion = FALSE
      ),
      list(
        name = "dwd:SDMS_17_1961_30",
        alias = "Sonnenscheindauer pro Jahr (1961-1990)",
        unit = "Stunden",
        needsConversion = FALSE
      )
    )
    
    map <- leaflet() %>%
      setView(lng = 10, lat = 51.26, zoom = 6) %>%
      addPolylines(data = dts,
                   color = "#222222",
                   stroke = TRUE,
                   weight = 1)
    
    for (layer in layers) {
      map <- map %>%
        addWMSTiles(
          baseUrl = "https://maps.dwd.de/geoserver/ows?SERVICE=WMS&",
          layers = layer$name,
          group = layer$alias,
          options = WMSTileOptions(
            format = "image/png", 
            transparent = TRUE,
            info_format = "application/json"
          )
        )
    }
    
    map <- map %>%
      addLayersControl(
        baseGroups = sapply(layers, function(x) x$alias),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    map <- map %>%
      htmlwidgets::onRender("
        function(el, x) {
          var legendControl = L.control({position: 'bottomright'});
          legendControl.onAdd = function(map) {
            var div = L.DomUtil.create('div', 'info legend');
            div.id = 'legend';
            return div;
          };
          legendControl.addTo(this);
        }
        ")
    
    map <- map %>%
      htmlwidgets::onRender(sprintf("
        function(el, x) {
          var map = this;
          var layerInfo = %s;
        
          function convertValue(value, needsConversion) {
            if (needsConversion) {
              return value / 10; 
            }
            return value; 
          }
        
          function updateLegend(layerName) {
            var legendUrl = 'https://maps.dwd.de/geoserver/ows?service=WMS&version=1.3.0&request=GetLegendGraphic&format=image/png&width=20&height=20&layer=' + layerName +
              '&legend_options=fontSize:16;forceLabels:on;fontAntiAliasing:true';
            var legendDiv = document.getElementById('legend');
            legendDiv.innerHTML = '<img src=\"' + legendUrl + '\" alt=\"legend\" style=\"width: 80px\">';
          }
        
          var legendControl = L.control({position: 'bottomleft'});
          legendControl.onAdd = function(map) {
            var div = L.DomUtil.create('div', 'info legend');
            div.id = 'legend';
            return div;
          };
          legendControl.addTo(map);
        
          var initialLayer = Object.keys(map._layers).find(layerId => map._layers[layerId].options && map._layers[layerId].options.layers);
          if (initialLayer) {
            var initialLayerName = map._layers[initialLayer].options.layers;
            updateLegend(initialLayerName);
          }
        
          map.on('click', function(e) {
            var latlng = e.latlng;
            var activeLayer = Object.values(map._layers).find(layer => layer.options && layer.options.layers && layer._map);
            if (!activeLayer) return;
        
            var layerName = activeLayer.options.layers;
            var url = 'https://maps.dwd.de/geoserver/ows?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetFeatureInfo&LAYERS=' + layerName + '&QUERY_LAYERS=' + layerName + '&BBOX=' + map.getBounds().toBBoxString() + '&FEATURE_COUNT=1&HEIGHT=' + map.getSize().y + '&WIDTH=' + map.getSize().x + '&INFO_FORMAT=application/json&SRS=EPSG:4326&X=' + Math.round(e.containerPoint.x) + '&Y=' + Math.round(e.containerPoint.y);
        
            fetch(url)
              .then(response => response.json())
              .then(data => {
                console.log('GetFeatureInfo response:', data);
                if (data.features && data.features.length > 0) {
                  var properties = data.features[0].properties;
                  var rawValue = properties[Object.keys(properties)[0]];
                  var info = layerInfo.find(l => l.name === layerName);
                  var convertedValue = convertValue(parseFloat(rawValue), info.needsConversion);
                  var formattedValue = convertedValue.toFixed(1);
                  var popupContent = info.alias + ': ' + formattedValue + ' ' + info.unit;
                  L.popup()
                    .setLatLng(latlng)
                    .setContent(popupContent)
                    .openOn(map);
                } else {
                  L.popup()
                    .setLatLng(latlng)
                    .setContent('Keine Daten an diesem Ort verfügbar.')
                    .openOn(map);
                }
              })
              .catch(error => {
                console.error('Error:', error);
                L.popup()
                  .setLatLng(latlng)
                  .setContent('Error fetching data: ' + error.message)
                  .openOn(map);
              });
          });
        
          map.on('baselayerchange', function(e) {
            var layerName = layerInfo.find(l => l.alias === e.name).name;
            updateLegend(layerName);
          });
        }", jsonlite::toJSON(layers, auto_unbox = TRUE)))
    
    map
    
  })
  waiter_hide()
}

shinyApp(ui = ui, server = server)
