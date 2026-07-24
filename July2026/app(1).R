# ================================================================
# Canadian Electricity Generation Explorer
#
# Data sources:
#   Generation : Statistics Canada, Table 25-10-0015-01
#   Supply & disposition (optional):
#               Statistics Canada, Table 25-10-0016-01
#               "Electric power generation, monthly receipts,
#                deliveries and availability"
#               Download from:
#               https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=2510001601
#               Save as "25100016.csv" in the same folder as app.R
#
# HOW TO RUN
#   1. Install required packages (one-time):
#        install.packages(c("shiny","dplyr","tidyr","ggplot2",
#                           "scales","DT","readr","plotly"))
#   2. Put "25100015.csv" (and optionally "25100016.csv") in the
#      SAME folder as this app.R file.
#   3. In R / RStudio run:  shiny::runApp("app.R")
# ================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(DT)
library(readr)
library(plotly)

# ----------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------

unit_divisor <- function(u) switch(u, "MWh" = 1, "GWh" = 1000, "TWh" = 1e6)

read_statcan <- function(path) {
  raw <- read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE)
  raw
}

# ----------------------------------------------------------------
# Load generation data (25-10-0015-01)
# ----------------------------------------------------------------

gen_raw <- read_statcan("25100015.csv")
names(gen_raw) <- c("ref_date","geo","dguid","class","type","uom",
                    "uom_id","scalar_factor","scalar_id","vector",
                    "coordinate","value","status","symbol",
                    "terminated","decimals")

gen <- gen_raw %>%
  mutate(
    value = suppressWarnings(as.numeric(value)),
    year  = as.integer(substr(ref_date, 1, 4)),
    month = as.integer(substr(ref_date, 6, 7)),
    date  = as.Date(paste0(ref_date, "-01"))
  ) %>%
  select(geo, class, type, year, month, date, value)

geo_choices   <- sort(unique(gen$geo))
class_choices <- sort(unique(gen$class))
type_choices  <- sort(unique(gen$type))

# Exclude aggregate "Total..." types from default selection to avoid double-counting
total_types <- c(
  "Total all types of electricity generation",
  "Total electricity production from combustible fuels",
  "Total electricity production from non-renewable combustible fuels",
  "Total electricity production from biomass"
)
default_types <- setdiff(type_choices, total_types)

min_date <- min(gen$date, na.rm = TRUE)
max_date <- max(gen$date, na.rm = TRUE)

# ----------------------------------------------------------------
# Load supply & disposition data (25-10-0016-01) — optional
# ----------------------------------------------------------------

disp_path    <- "25100016.csv"
disp_loaded  <- file.exists(disp_path)

if (disp_loaded) {
  disp_raw <- read_statcan(disp_path)
  names(disp_raw) <- c("ref_date","geo","dguid","char","uom",
                       "uom_id","scalar_factor","scalar_id","vector",
                       "coordinate","value","status","symbol",
                       "terminated","decimals")
  disp <- disp_raw %>%
    mutate(
      value = suppressWarnings(as.numeric(value)),
      year  = as.integer(substr(ref_date, 1, 4)),
      month = as.integer(substr(ref_date, 6, 7)),
      date  = as.Date(paste0(ref_date, "-01"))
    ) %>%
    select(geo, char, year, month, date, value)

  disp_chars <- sort(unique(disp$char))
} else {
  disp        <- NULL
  disp_chars  <- character(0)
}

unit_choices <- c("Megawatt hours (MWh)" = "MWh",
                  "Gigawatt hours (GWh)"  = "GWh",
                  "Terawatt hours (TWh)"  = "TWh")

# ----------------------------------------------------------------
# Caveats panel
# ----------------------------------------------------------------

caveats_ui <- function() {
  fluidPage(
    h4("Data sources"),
    tags$ul(
      tags$li(strong("Generation (Table 25-10-0015-01): "),
              "Statistics Canada — Electric power generation, monthly generation ",
              "by type of electricity generation. ",
              tags$a("View table", href = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=2510001501",
                     target = "_blank")),
      tags$li(strong("Supply & disposition (Table 25-10-0016-01): "),
              "Statistics Canada — Electric power generation, monthly receipts, ",
              "deliveries and availability. Required for the Imports / Exports tab. ",
              tags$a("Download table", href = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=2510001601",
                     target = "_blank"),
              " and save as ", code("25100016.csv"), " in the same folder as ", code("app.R"), ".")
    ),

    h4("Caveats, limitations & assumptions"),
    tags$ul(
      tags$li(strong("Survey coverage: "),
              "The underlying survey covers electric utilities and industrial generators ",
              "reporting to Statistics Canada. Small-scale behind-the-meter or residential ",
              "distributed generation (e.g. rooftop solar with net metering) is largely ",
              "excluded. 'Solar' figures reflect utility-scale / grid-reporting generation only."),

      tags$li(strong("Units: "),
              "Original values are in megawatt hours (MWh). GWh and TWh conversions are ",
              "simple division; no other transformations are applied."),

      tags$li(strong("Category reclassification over time — important: "),
              "Statistics Canada changed its generation type categories in 2016 and again in 2020. ",
              "Up to 2015, fossil-fuel generation was split across 'Conventional steam turbine', ",
              "'Internal combustion turbine', and 'Combustion turbine'. From 2016 these were ",
              "consolidated into 'Total electricity production from combustible fuels'. Biomass and ",
              "non-renewable combustible sub-categories were added from 2020. Apparent jumps or ",
              "gaps around these years often reflect reclassification, not real generation changes."),

      tags$li(strong("Double counting risk: "),
              "The four 'Total...' generation types are aggregate rows. By default they are excluded ",
              "from the type filter; adding them alongside their components will overstate totals in ",
              "stacked charts."),

      tags$li(strong("Missing / suppressed values (NA): "),
              "NAs occur when a generation type does not exist for that geography/period or is ",
              "suppressed for confidentiality. Annual sums use na.rm = TRUE; years with suppressed ",
              "months will understate the true total. The 'exclude incomplete years' checkbox removes ",
              "geo × class × type × year combos with fewer than 12 months of data."),

      tags$li(strong("Most recent year may be partial: "),
              "The data extract may only cover part of the most recent calendar year. Annual sums ",
              "for such years are not comparable to full-year values unless the 'exclude incomplete ",
              "years' checkbox is used."),

      tags$li(strong("Negative values: "),
              "A small number of rows contain negative values, likely reflecting net adjustments or ",
              "data revisions rather than true negative generation. These are retained by default; ",
              "use the 'exclude negative values' checkbox to drop them."),

      tags$li(strong("'Canada' vs. provincial totals: "),
              "'Canada' rows are independently estimated national totals and will not necessarily ",
              "equal the sum of all provinces due to confidentiality suppression or estimation differences."),

      tags$li(strong("Imports / exports (25-10-0016-01): "),
              "Net availability (consumption proxy) = Generation + Receipts − Deliveries − Losses. ",
              "Receipts include inter-provincial transfers and imports from the US; deliveries include ",
              "inter-provincial transfers and exports to the US. These are reported separately where ",
              "available. Values may differ from bilateral trade statistics due to timing and measurement ",
              "differences. The 'Canada' row in Table 25-10-0016-01 reflects net cross-border flows ",
              "with the US only; inter-provincial flows net to zero at the national level."),

      tags$li(strong("General: "),
              "This app is for exploratory analysis only. Consult Statistics Canada's official table ",
              "documentation and footnotes before drawing policy or investment conclusions.")
    )
  )
}

# ----------------------------------------------------------------
# UI
# ----------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Canadian Electricity Generation Explorer"),
  h5(em("Statistics Canada — Tables 25-10-0015-01 and 25-10-0016-01")),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # --- Geography & class
      selectizeInput("geo", "Geography",
                     choices  = geo_choices,
                     selected = "British Columbia",
                     multiple = TRUE,
                     options  = list(plugins = list("remove_button"))),

      selectizeInput("class", "Class of electricity producer",
                     choices  = class_choices,
                     selected = "Total all classes of electricity producer",
                     multiple = TRUE,
                     options  = list(plugins = list("remove_button"))),

      selectizeInput("type", "Type of electricity generation",
                     choices  = type_choices,
                     selected = default_types,
                     multiple = TRUE,
                     options  = list(plugins = list("remove_button"))),

      hr(),

      # --- Date & aggregation
      dateRangeInput("daterange", "Date range",
                     start  = min_date, end = max_date,
                     min    = min_date, max = max_date,
                     format = "yyyy-mm"),

      radioButtons("agg", "Time aggregation",
                   choices  = c("Monthly" = "monthly", "Annual (sum)" = "annual"),
                   selected = "annual"),

      checkboxInput("exclude_partial",
                    "When annual: exclude combos with < 12 months of data",
                    value = TRUE),

      hr(),

      # --- Chart controls
      selectInput("group_var", "Colour / stack series by",
                  choices  = c("Type of electricity generation" = "type",
                               "Geography"                      = "geo",
                               "Class of electricity producer"  = "class"),
                  selected = "type"),

      radioButtons("plot_type", "Chart type",
                   choices  = c("Line"                        = "line",
                                "Stacked area (absolute)"     = "area",
                                "Stacked area (% of total)"   = "percent",
                                "Stacked bar"                 = "bar"),
                   selected = "area"),

      checkboxInput("shared_y",
                    "Same y-axis scale across all facet panels",
                    value = TRUE),

      selectInput("unit", "Units", choices = unit_choices, selected = "GWh"),

      checkboxInput("exclude_negative",
                    "Exclude negative values",
                    value = FALSE),

      hr(),
      downloadButton("download_data", "Download filtered data (CSV)")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Chart",
                 plotlyOutput("main_plot", height = "560px")),

        tabPanel("Imports / Exports",
                 if (!disp_loaded)
                   div(
                     br(),
                     div(class = "alert alert-warning",
                         h4("Supply & disposition data not loaded"),
                         p("To enable this tab, download Statistics Canada Table 25-10-0016-01 and ",
                           "save it as ", code("25100016.csv"), " in the same folder as ", code("app.R"),
                           ", then restart the app."),
                         tags$a("Download Table 25-10-0016-01 from Statistics Canada",
                                href   = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=2510001601",
                                target = "_blank",
                                class  = "btn btn-primary")
                     )
                   )
                 else
                   tagList(
                     br(),
                     fluidRow(
                       column(4,
                              selectizeInput("disp_geo", "Geography",
                                             choices  = sort(unique(disp$geo)),
                                             selected = "British Columbia",
                                             multiple = TRUE,
                                             options  = list(plugins = list("remove_button")))),
                       column(4,
                              radioButtons("disp_agg", "Aggregation",
                                           choices  = c("Monthly" = "monthly",
                                                        "Annual"  = "annual"),
                                           selected = "annual",
                                           inline   = TRUE)),
                       column(4,
                              selectInput("disp_unit", "Units",
                                          choices  = unit_choices,
                                          selected = "GWh"))
                     ),
                     plotlyOutput("disp_balance_plot",  height = "350px"),
                     br(),
                     plotlyOutput("disp_netimport_plot", height = "280px"),
                     br(),
                     DTOutput("disp_table")
                   )
        ),

        tabPanel("Data table",
                 DTOutput("data_table")),

        tabPanel("Summary by series",
                 DTOutput("summary_table")),

        tabPanel("Caveats & assumptions",
                 caveats_ui())
      )
    )
  )
)

# ----------------------------------------------------------------
# Server
# ----------------------------------------------------------------

server <- function(input, output, session) {

  # ---- Filtered generation data --------------------------------

  filtered_raw <- reactive({
    out <- gen %>%
      filter(
        geo   %in% input$geo,
        class %in% input$class,
        type  %in% input$type,
        date  >= input$daterange[1],
        date  <= input$daterange[2]
      )
    if (input$exclude_negative) out <- out %>% filter(is.na(value) | value >= 0)
    out
  })

  agg_data <- reactive({
    fr  <- filtered_raw()
    div <- unit_divisor(input$unit)

    if (input$agg == "monthly") {
      fr %>%
        mutate(value = value / div, time = date) %>%
        select(geo, class, type, time, value)
    } else {
      g <- fr %>%
        group_by(geo, class, type, year) %>%
        summarise(n_months = sum(!is.na(value)),
                  value    = sum(value, na.rm = TRUE),
                  .groups  = "drop")
      if (input$exclude_partial) g <- g %>% filter(n_months == 12)
      g %>%
        mutate(value = value / div, time = year) %>%
        select(geo, class, type, time, n_months, value)
    }
  })

  plot_data <- reactive({
    d  <- agg_data()
    gv <- input$group_var
    names(d)[names(d) == gv] <- "series"

    other_dims  <- setdiff(c("type", "geo", "class"), gv)
    facet_dims  <- other_dims[sapply(other_dims, function(v) length(unique(d[[v]])) > 1)]

    d$facet_label <- if (length(facet_dims) > 0)
      do.call(paste, c(as.list(d[facet_dims]), sep = " | "))
    else ""

    d
  })

  # ---- Main interactive chart (plotly) -------------------------

  output$main_plot <- renderPlotly({
    d <- plot_data()

    if (nrow(d) == 0) {
      p <- ggplot() +
        annotate("text", x = 0, y = 0,
                 label = "No data for current filter selection") +
        theme_void()
      return(ggplotly(p))
    }

    d$series <- factor(d$series)

    p <- ggplot(d, aes(x = time, y = value,
                       colour = series, fill = series,
                       group  = series, text = paste0(
                         "<b>", series, "</b><br>",
                         if (input$agg == "monthly") format(d$time, "%b %Y") else as.character(d$time),
                         "<br>", round(value, 2), " ", input$unit
                       )))

    if (input$plot_type == "line") {
      p <- p + geom_line(linewidth = 0.8) + geom_point(size = 1.5)
    } else if (input$plot_type == "area") {
      p <- p + geom_area(position = "stack", alpha = 0.8, colour = NA)
    } else if (input$plot_type == "percent") {
      p <- p + geom_area(position = "fill",  alpha = 0.8, colour = NA)
    } else if (input$plot_type == "bar") {
      p <- p + geom_col(position = "stack")
    }

    if (input$plot_type == "percent") {
      p <- p + scale_y_continuous(labels = percent_format())
      y_lab <- "Share of total"
    } else {
      p <- p + scale_y_continuous(labels = comma)
      y_lab <- paste0("Generation (", input$unit, ")")
    }

    x_lab <- if (input$agg == "monthly") "Month" else "Year"
    if (input$agg == "monthly") {
      p <- p + scale_x_date(date_labels = "%Y", date_breaks = "2 years")
    } else {
      p <- p + scale_x_continuous(breaks = pretty_breaks())
    }

    facet_scales <- if (input$shared_y) "fixed" else "free_y"
    if (any(d$facet_label != "")) {
      p <- p + facet_wrap(~ facet_label, scales = facet_scales)
    }

    p <- p +
      labs(x = x_lab, y = y_lab, colour = NULL, fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.15)) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE,
             modeBarButtonsToRemove = c("lasso2d","select2d"))
  })

  # ---- Supply & disposition tab --------------------------------

  disp_agg_data <- reactive({
    req(disp_loaded, input$disp_geo)
    div <- unit_divisor(input$disp_unit)

    d <- disp %>%
      filter(geo %in% input$disp_geo)

    # Key categories (StatCan 25-10-0016-01 label names)
    key_chars <- c(
      "Generation",
      "Receipts from other provinces and territories",
      "Receipts from the United States",
      "Deliveries to other provinces and territories",
      "Deliveries to the United States",
      "Losses",
      "Net availability"
    )
    d <- d %>% filter(char %in% key_chars)

    if (input$disp_agg == "annual") {
      d <- d %>%
        group_by(geo, char, year) %>%
        summarise(n = sum(!is.na(value)),
                  value = sum(value, na.rm = TRUE),
                  .groups = "drop") %>%
        filter(n == 12) %>%
        mutate(time = year)
    } else {
      d <- d %>%
        mutate(time = date)
    }

    d %>% mutate(value = value / div)
  })

  output$disp_balance_plot <- renderPlotly({
    req(disp_loaded)
    d <- disp_agg_data()

    supply_chars <- c("Generation",
                      "Receipts from other provinces and territories",
                      "Receipts from the United States")
    demand_chars <- c("Net availability",
                      "Deliveries to other provinces and territories",
                      "Deliveries to the United States",
                      "Losses")

    plot_chars <- c(supply_chars, demand_chars)
    d2 <- d %>% filter(char %in% plot_chars) %>%
      mutate(side = if_else(char %in% supply_chars, "Supply", "Disposition"),
             char = factor(char, levels = plot_chars))

    label_txt <- paste0("<b>", d2$geo, " — ", d2$char, "</b><br>",
                        if (input$disp_agg == "monthly") format(as.Date(paste0(d2$time,"-01")), "%b %Y") else as.character(d2$time),
                        "<br>", round(d2$value, 2), " ", input$disp_unit)

    p <- ggplot(d2, aes(x = time, y = value, colour = char,
                        group = interaction(geo, char),
                        text = label_txt)) +
      geom_line(linewidth = 0.8) +
      facet_wrap(~ geo, scales = if (input$shared_y) "fixed" else "free_y") +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = paste0(input$disp_unit),
           title = "Supply & disposition components",
           colour = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.25)) %>%
      config(displayModeBar = FALSE)
  })

  output$disp_netimport_plot <- renderPlotly({
    req(disp_loaded)
    d <- disp_agg_data()

    net <- d %>%
      filter(char %in% c("Receipts from other provinces and territories",
                          "Receipts from the United States",
                          "Deliveries to other provinces and territories",
                          "Deliveries to the United States")) %>%
      mutate(signed = if_else(
        char %in% c("Receipts from other provinces and territories",
                    "Receipts from the United States"),
        value, -value
      ),
      flow_type = if_else(
        char %in% c("Receipts from other provinces and territories",
                    "Deliveries to other provinces and territories"),
        "Inter-provincial (net)", "US cross-border (net)"
      )) %>%
      group_by(geo, time, flow_type) %>%
      summarise(net_value = sum(signed, na.rm = TRUE), .groups = "drop")

    label_txt <- paste0("<b>", net$geo, " — ", net$flow_type, "</b><br>",
                        as.character(net$time),
                        "<br>Net: ", round(net$net_value, 2), " ", input$disp_unit,
                        "<br>(positive = net importer)")

    p <- ggplot(net, aes(x = time, y = net_value, fill = flow_type,
                         text = label_txt)) +
      geom_col(position = "stack") +
      geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
      facet_wrap(~ geo, scales = if (input$shared_y) "fixed" else "free_y") +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("Inter-provincial (net)" = "#4E79A7",
                                   "US cross-border (net)"  = "#F28E2B")) +
      labs(x = NULL, y = paste0(input$disp_unit),
           title = "Net electricity imports (positive = net importer, negative = net exporter)",
           fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.25)) %>%
      config(displayModeBar = FALSE)
  })

  output$disp_table <- renderDT({
    req(disp_loaded)
    datatable(disp_agg_data(), options = list(pageLength = 12), rownames = FALSE)
  })

  # ---- Data & summary tabs ------------------------------------

  output$data_table <- renderDT({
    datatable(agg_data(), options = list(pageLength = 15), rownames = FALSE)
  })

  output$summary_table <- renderDT({
    d <- agg_data() %>%
      group_by(geo, class, type) %>%
      summarise(
        periods     = n(),
        min         = round(min(value,  na.rm = TRUE), 2),
        mean        = round(mean(value, na.rm = TRUE), 2),
        max         = round(max(value,  na.rm = TRUE), 2),
        total       = round(sum(value,  na.rm = TRUE), 2),
        .groups     = "drop"
      )
    names(d)[5:8] <- paste0(names(d)[5:8], " (", input$unit, ")")
    datatable(d, options = list(pageLength = 15), rownames = FALSE)
  })

  output$download_data <- downloadHandler(
    filename = function() paste0("electricity_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(agg_data(), file)
  )
}

# ----------------------------------------------------------------
shinyApp(ui = ui, server = server)
