# ================================================================
# Canadian Electricity Generation Explorer
#
# Data source: Statistics Canada, Table 25-10-0015-01
#   "Electric power generation, monthly generation by type
#    of electricity generation"
#
# HOW TO RUN
#   1. Install required packages (one-time):
#        install.packages(c("shiny","dplyr","tidyr","ggplot2",
#                            "scales","DT","readr"))
#   2. Put "25100015.csv" in the SAME folder as this app.R file
#      (a copy is provided alongside this script).
#   3. In R / RStudio, run:
#        shiny::runApp("app.R")
# ================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(DT)
library(readr)

# ----------------------------------------------------------------
# Load & prepare data
# ----------------------------------------------------------------

data_path <- "25100015.csv"

raw <- read_csv(data_path, col_types = cols(.default = "c"), show_col_types = FALSE)

# Rename columns positionally to avoid any issues with spaces / BOM
# in the original header row. Order matches the published table.
names(raw) <- c("ref_date", "geo", "dguid", "class", "type", "uom",
                "uom_id", "scalar_factor", "scalar_id", "vector",
                "coordinate", "value", "status", "symbol",
                "terminated", "decimals")

df <- raw %>%
  mutate(
    value = suppressWarnings(as.numeric(value)),
    year  = as.integer(substr(ref_date, 1, 4)),
    month = as.integer(substr(ref_date, 6, 7)),
    date  = as.Date(paste0(ref_date, "-01"))
  ) %>%
  select(geo, class, type, year, month, date, value)

geo_choices   <- sort(unique(df$geo))
class_choices <- sort(unique(df$class))
type_choices  <- sort(unique(df$type))

# Default type selection: leave out the aggregate "Total..." rows so
# a stacked chart doesn't double-count by default. Users can still
# add these back in via the filter.
total_types <- c(
  "Total all types of electricity generation",
  "Total electricity production from combustible fuels",
  "Total electricity production from non-renewable combustible fuels",
  "Total electricity production from biomass"
)
default_types <- setdiff(type_choices, total_types)

min_date <- min(df$date, na.rm = TRUE)
max_date <- max(df$date, na.rm = TRUE)

unit_choices <- c("Megawatt hours (MWh)" = "MWh",
                  "Gigawatt hours (GWh)" = "GWh",
                  "Terawatt hours (TWh)" = "TWh")

unit_divisor <- function(u) switch(u, "MWh" = 1, "GWh" = 1000, "TWh" = 1e6)

# ----------------------------------------------------------------
# Caveats panel content (static) - defined before UI so it can be
# referenced when building the tabset
# ----------------------------------------------------------------

caveats_ui <- function() {
  fluidPage(
    h4("Data source"),
    p("Statistics Canada. Table 25-10-0015-01, ",
      em("Electric power generation, monthly generation by type of electricity generation."),
      " This app uses a single static extract of that table (file 25100015.csv) and will ",
      "not reflect any later revisions or updates from Statistics Canada."),

    h4("Caveats, limitations & assumptions"),
    tags$ul(
      tags$li(strong("Survey coverage: "),
              "The underlying survey collects data from electric utilities and industrial ",
              "establishments that operate generating stations and report to Statistics Canada. ",
              "It is not a household-level survey, so small-scale, behind-the-meter, or ",
              "residential distributed generation (e.g. rooftop solar with net metering) is ",
              "unlikely to be captured. 'Solar' figures should therefore be read as utility-scale ",
              "/ grid-reporting generation, not a comprehensive measure of all solar installed in a region."),

      tags$li(strong("Units: "),
              "Original values are in megawatt hours (MWh). This app lets you display values in ",
              "MWh, GWh, or TWh via simple division - no other unit conversions are applied."),

      tags$li(strong("Category reclassification over time - important: "),
              "The set of 'Type of electricity generation' categories used by Statistics Canada ",
              "has changed over the life of this series:"),
      tags$ul(
        tags$li("Up to 2015, fossil-fuel-related generation was reported under ",
                "'Conventional steam turbine', 'Internal combustion turbine', and 'Combustion turbine'."),
        tags$li("From 2016 onward, those three were largely consolidated into a single ",
                "'Total electricity production from combustible fuels' category, and explicit ",
                "'Wind power turbine', 'Solar', 'Tidal power turbine', and 'Other types of ",
                "electricity generation' categories were introduced."),
        tags$li("From 2020 onward, 'Total electricity production from biomass' and ",
                "'Total electricity production from non-renewable combustible fuels' were further ",
                "split out from 'Total electricity production from combustible fuels'."),
        tags$li("As a result, a single 'type' selected across the full 2008-2026 range may show ",
                "an apparent jump, drop, or gap around 2016 and/or 2020 that reflects a change in ",
                "how Statistics Canada classifies generation, not necessarily a real change in ",
                "the amount of electricity generated.")
      ),

      tags$li(strong("Double counting risk: "),
              "'Total all types of electricity generation', 'Total electricity production from ",
              "combustible fuels', 'Total electricity production from non-renewable combustible ",
              "fuels', and 'Total electricity production from biomass' are themselves sums of other ",
              "rows. By default this app excludes these aggregate categories from the type filter; ",
              "if you add them back in alongside their component types, stacked charts and column ",
              "totals will overstate the true total."),

      tags$li(strong("Missing values (NA): "),
              "Values shown in the original file as not available (often because a given ",
              "generation type does not exist for that geography/period, or for confidentiality) ",
              "are treated as NA. Annual sums use the available months only (na.rm = TRUE); a year ",
              "with some missing months will therefore sum to a smaller total than a fully ",
              "reported year. The 'exclude incomplete years' option removes any ",
              "geography x class x type x year combination with fewer than 12 monthly observations, ",
              "but a 'complete' set of 12 months can still include zeros or unusual values - this ",
              "option reduces, but does not eliminate, comparability issues."),

      tags$li(strong("Most recent year may be partial: "),
              "At the time this extract was produced, the most recent year of data may only cover ",
              "part of the year (e.g. January-March). Its annual sum is not comparable to a full ",
              "year unless you account for this (the 'exclude incomplete years' option will drop it)."),

      tags$li(strong("Negative values: "),
              "A small number of rows in the raw data contain negative values (for example, for ",
              "some combustion-turbine series in some provinces in early years). These likely ",
              "reflect net adjustments, station consumption exceeding gross generation, or data ",
              "revisions rather than true negative generation. They are kept by default; check ",
              "'exclude negative values' to drop them."),

      tags$li(strong("'Canada' vs. provincial/territorial totals: "),
              "'Canada' rows are independently reported national totals and are not guaranteed to ",
              "equal the arithmetic sum of all provinces and territories shown in this dataset, due ",
              "to confidentiality suppression, timing differences, or independent estimation."),

      tags$li(strong("Class of producer totals: "),
              "'Total all classes of electricity producer' should approximately equal ",
              "'Electricity producers, electric utilities' plus 'Electricity producers, industries', ",
              "but small discrepancies can occur due to rounding or confidentiality suppression of ",
              "individual cells."),

      tags$li(strong("General: "),
              "This app is intended for exploratory analysis only and is not a substitute for ",
              "consulting Statistics Canada's official table documentation, footnotes, and ",
              "definitions for Table 25-10-0015-01.")
    )
  )
}

# ----------------------------------------------------------------
# UI
# ----------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Canadian Electricity Generation Explorer"),
  h5(em("Statistics Canada, Table 25-10-0015-01 - Electric power generation, ",
        "monthly generation by type of electricity generation")),
  sidebarLayout(
    sidebarPanel(
      width = 3,

      selectizeInput("geo", "Geography",
                      choices = geo_choices,
                      selected = "British Columbia",
                      multiple = TRUE,
                      options = list(plugins = list("remove_button"))),

      selectizeInput("class", "Class of electricity producer",
                      choices = class_choices,
                      selected = "Total all classes of electricity producer",
                      multiple = TRUE,
                      options = list(plugins = list("remove_button"))),

      selectizeInput("type", "Type of electricity generation",
                      choices = type_choices,
                      selected = default_types,
                      multiple = TRUE,
                      options = list(plugins = list("remove_button"))),

      dateRangeInput("daterange", "Date range",
                      start = min_date, end = max_date,
                      min = min_date, max = max_date,
                      format = "yyyy-mm"),

      radioButtons("agg", "Time aggregation",
                    choices = c("Monthly" = "monthly", "Annual (sum)" = "annual"),
                    selected = "annual"),

      checkboxInput("exclude_partial",
                     "When annual: exclude geo x class x type x year combos with fewer than 12 months of data",
                     value = TRUE),

      selectInput("group_var", "Colour / stack series by",
                   choices = c("Type of electricity generation" = "type",
                                "Geography" = "geo",
                                "Class of electricity producer" = "class"),
                   selected = "type"),

      radioButtons("plot_type", "Chart type",
                    choices = c("Line" = "line",
                                 "Stacked area (absolute)" = "area",
                                 "Stacked area (% of total)" = "percent",
                                 "Stacked bar" = "bar"),
                    selected = "area"),

      checkboxInput("shared_y",
                     "Use the same y-axis scale across all panels (when faceted)",
                     value = TRUE),

      selectInput("unit", "Units", choices = unit_choices, selected = "GWh"),

      checkboxInput("exclude_negative",
                     "Exclude negative values (treat as data anomalies)",
                     value = FALSE),

      downloadButton("download_data", "Download filtered/aggregated data (CSV)")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Chart", plotOutput("main_plot", height = "550px")),
        tabPanel("Data table", DTOutput("data_table")),
        tabPanel("Summary by series", DTOutput("summary_table")),
        tabPanel("Caveats, limitations & assumptions", caveats_ui())
      )
    )
  )
)

# ----------------------------------------------------------------
# Server
# ----------------------------------------------------------------

server <- function(input, output, session) {

  filtered_raw <- reactive({
    out <- df %>%
      filter(
        geo %in% input$geo,
        class %in% input$class,
        type %in% input$type,
        date >= input$daterange[1],
        date <= input$daterange[2]
      )

    if (input$exclude_negative) {
      out <- out %>% filter(is.na(value) | value >= 0)
    }

    out
  })

  agg_data <- reactive({
    fr <- filtered_raw()
    div <- unit_divisor(input$unit)

    if (input$agg == "monthly") {
      fr %>%
        mutate(value = value / div, time = date) %>%
        select(geo, class, type, time, value)
    } else {
      grouped <- fr %>%
        group_by(geo, class, type, year) %>%
        summarise(
          n_months = sum(!is.na(value)),
          value    = sum(value, na.rm = TRUE),
          .groups  = "drop"
        )

      if (input$exclude_partial) {
        grouped <- grouped %>% filter(n_months == 12)
      }

      grouped %>%
        mutate(value = value / div, time = year) %>%
        select(geo, class, type, time, n_months, value)
    }
  })

  plot_data <- reactive({
    d <- agg_data()
    gv <- input$group_var

    names(d)[names(d) == gv] <- "series"

    other_dims <- setdiff(c("type", "geo", "class"), gv)
    facet_dims <- other_dims[sapply(other_dims, function(v) length(unique(d[[v]])) > 1)]

    if (length(facet_dims) > 0) {
      d$facet_label <- do.call(paste, c(as.list(d[facet_dims]), sep = " | "))
    } else {
      d$facet_label <- ""
    }

    d
  })

  output$main_plot <- renderPlot({
    d <- plot_data()

    if (nrow(d) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0,
                   label = "No data available for the current filter selection") +
          theme_void()
      )
    }

    d$series <- factor(d$series)

    p <- ggplot(d, aes(x = time, y = value, colour = series, fill = series, group = series))

    if (input$plot_type == "line") {
      p <- p + geom_line(linewidth = 0.9) + geom_point(size = 1.4)
    } else if (input$plot_type == "area") {
      p <- p + geom_area(position = "stack", alpha = 0.85, colour = NA)
    } else if (input$plot_type == "percent") {
      p <- p + geom_area(position = "fill", alpha = 0.85, colour = NA)
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

    if (input$agg == "monthly") {
      p <- p + scale_x_date(date_labels = "%Y", date_breaks = "2 years")
      x_lab <- "Month"
    } else {
      p <- p + scale_x_continuous(breaks = pretty_breaks())
      x_lab <- "Year"
    }

    if (any(d$facet_label != "")) {
      facet_scales <- if (input$shared_y) "fixed" else "free_y"
      p <- p + facet_wrap(~ facet_label, scales = facet_scales)
    }

    p +
      labs(x = x_lab, y = y_lab, colour = NULL, fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$data_table <- renderDT({
    datatable(agg_data(), options = list(pageLength = 15), rownames = FALSE)
  })

  output$summary_table <- renderDT({
    d <- agg_data() %>%
      group_by(geo, class, type) %>%
      summarise(
        n_periods   = n(),
        min_value   = round(min(value, na.rm = TRUE), 2),
        mean_value  = round(mean(value, na.rm = TRUE), 2),
        max_value   = round(max(value, na.rm = TRUE), 2),
        total_value = round(sum(value, na.rm = TRUE), 2),
        .groups     = "drop"
      )

    names(d)[names(d) == "min_value"]   <- paste0("min (", input$unit, ")")
    names(d)[names(d) == "mean_value"]  <- paste0("mean (", input$unit, ")")
    names(d)[names(d) == "max_value"]   <- paste0("max (", input$unit, ")")
    names(d)[names(d) == "total_value"] <- paste0("total (", input$unit, ")")

    datatable(d, options = list(pageLength = 15), rownames = FALSE)
  })

  output$download_data <- downloadHandler(
    filename = function() paste0("electricity_generation_filtered_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(agg_data(), file)
    }
  )
}

# ----------------------------------------------------------------
shinyApp(ui = ui, server = server)
