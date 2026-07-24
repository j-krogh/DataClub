# =============================================================================
# BC Hydro historical intertie flow loader
# ------------------------------------------------------------------------------
# Downloads all yearly "Actual Flow" XLS files from BC Hydro's historical data
# page, normalizes across vintages (2007-present), and produces `flows`:
#
#   date_local       Date       calendar date, Pacific time
#   hour             int        hour-ending (1-24)
#   bc_us_MWh        num        BC-US intertie flow (BC Hydro convention:
#                               positive = export from BC, negative = import)
#   bc_ab_MWh        num        BC-Alberta intertie flow (same convention)
#   source_year      int        year of the source file
#   datetime_pst     POSIXct    hour-beginning timestamp, America/Vancouver
#
# Sign convention is preserved as-published. Flip downstream if you want the
# "positive = net import into BC" convention.
# =============================================================================

# ---- Configuration ----------------------------------------------------------
HIST_PAGE <- paste0(
  "https://www.bchydro.com/energy-in-bc/operations/transmission/",
  "transmission-system/actual-flow-data/historical-data.html"
)
CACHE_DIR <- "bchydro_cache"

dir.create(CACHE_DIR, showWarnings = FALSE)

# ---- Small utilities --------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

is_excel_bytes <- function(path) {
  if (!file.exists(path) || file.info(path)$size < 8) return(FALSE)
  con <- file(path, "rb"); on.exit(close(con))
  hdr <- readBin(con, "raw", n = 8)
  # XLS (OLE compound):  D0 CF 11 E0
  # XLSX (zip):          50 4B 03 04
  identical(hdr[1:4], as.raw(c(0xD0, 0xCF, 0x11, 0xE0))) ||
    identical(hdr[1:4], as.raw(c(0x50, 0x4B, 0x03, 0x04)))
}

# ---- Date parser ------------------------------------------------------------
# BC Hydro's date column has drifted across years. Match by regex FIRST so the
# wrong format never gets a chance to silently succeed (e.g. %d/%m/%Y "parsing"
# an American M/D/Y string).
parse_bchydro_date <- function(x) {
  x   <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x))
  
  # 1. Excel serial numbers  (e.g. "45658")
  serial <- grepl("^\\d+(\\.\\d+)?$", x)
  out[serial] <- as.Date(as.numeric(x[serial]), origin = "1899-12-30")
  
  # 2. M/D/YYYY  (BC Hydro's dominant format — try this FIRST)
  mdy <- !serial & grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", x)
  out[mdy] <- as.Date(x[mdy], format = "%m/%d/%Y")
  
  # 3. D-Mon-YYYY  (older vintages, e.g. "1-Jan-2007")
  dmy_txt <- !serial & !mdy &
    grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{4}$", x)
  out[dmy_txt] <- as.Date(x[dmy_txt], format = "%d-%b-%Y")
  
  # 4. ISO YYYY-MM-DD
  iso <- !serial & !mdy & !dmy_txt &
    grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  out[iso] <- as.Date(x[iso], format = "%Y-%m-%d")
  
  out
}

# ---- Header-row auto-detection ----------------------------------------------
# Scan the first ~15 rows for one containing BOTH a date-like and hour-like
# header cell. Handles "Date"/"Day"/"Datetime" and "HE"/"HR"/"Hour"/"Hour Ending".
find_header_row <- function(df, max_scan = 15) {
  date_pat <- "^date$|^day$|^datetime$"
  hour_pat <- "^he$|^hr$|^hour$|hourending"
  
  # Aggressively normalize each cell before pattern matching:
  # keep only letters and digits, lower-case.
  # This handles "Date ?", "Date  ", "HE (PT)", "Hour-Ending", etc.
  clean_cell <- function(s) {
    s <- tolower(trimws(as.character(s)))
    gsub("[^a-z0-9]", "", s)
  }
  
  scan_n <- min(max_scan, nrow(df))
  for (i in seq_len(scan_n)) {
    row_vals <- vapply(unlist(df[i, ]), clean_cell, character(1))
    row_vals <- row_vals[!is.na(row_vals) & row_vals != ""]
    if (length(row_vals) == 0) next
    if (any(grepl(date_pat, row_vals)) &&
        any(grepl(hour_pat, row_vals))) {
      return(i)
    }
  }
  stop("Could not find a header row containing Date/Day + HE/Hour.")
}


# ---- Canonical column renaming ----------------------------------------------
# Fold every vintage's column labels into four names: date, he, us_tielines,
# ab_tielines. Everything else (banner remnants, blank cols) is discarded.
canonicalize_columns <- function(raw) {
  rename_map <- c(
    date         = "date",
    day          = "date",
    datetime     = "date",
    he           = "he",
    hr           = "he",
    hour         = "he",
    hour_ending  = "he",
    us_tielines  = "us_tielines",
    us           = "us_tielines",
    bc_us        = "us_tielines",
    united       = "us_tielines",
    ab_tielines  = "ab_tielines",
    ab           = "ab_tielines",
    bc_ab        = "ab_tielines",
    alberta      = "ab_tielines"
  )
  new <- names(raw)
  for (from in names(rename_map)) {
    hits <- grep(paste0("^", from), new, ignore.case = TRUE)
    if (length(hits)) new[hits] <- rename_map[[from]]
  }
  names(raw) <- new
  keep <- names(raw) %in% c("date", "he", "us_tielines", "ab_tielines")
  raw[, keep, drop = FALSE]
}

# ---- Per-year download + parse ----------------------------------------------
read_year <- function(year, url) {
  local <- file.path(CACHE_DIR,
                     gsub(" ", "_", basename(url)))
  
  # Download (cached) --------------------------------------------------------
  if (!file.exists(local) || file.info(local)$size == 0) {
    tmp     <- tempfile(fileext = ".xls")
    url_enc <- utils::URLencode(url)
    
    resp <- GET(
      url_enc,
      write_disk(tmp, overwrite = TRUE),
      add_headers(
        `User-Agent` = "Mozilla/5.0 (compatible; R data-analysis script)",
        Referer      = HIST_PAGE,
        Accept       = "application/vnd.ms-excel,application/octet-stream,*/*"
      ),
      timeout(60)
    )
    if (http_error(resp)) {
      stop(sprintf("HTTP %s for %s", status_code(resp), url_enc))
    }
    ctype <- headers(resp)$`content-type` %||% ""
    if (grepl("text/html", ctype, ignore.case = TRUE)) {
      stop(sprintf("Server returned HTML (not .xls) for %s — file not posted yet.",
                   url_enc))
    }
    if (!is_excel_bytes(tmp)) {
      stop(sprintf("Downloaded file for %d is not a valid Excel file (size = %d bytes).",
                   year, file.info(tmp)$size))
    }
    file.rename(tmp, local)
  }
  
  # Read with NO header row -------------------------------------------------
  raw_all <- read_excel(
    local, sheet = 1,
    col_names = FALSE,
    col_types = "text",     # read everything as text; coerce later
    guess_max = 20000,
    .name_repair = "unique_quiet"
  )
  
  # Promote the true header row --------------------------------------------
  hdr_row  <- find_header_row(raw_all)
  new_names <- as.character(unlist(raw_all[hdr_row, ]))
  new_names <- ifelse(is.na(new_names) | new_names == "",
                      paste0("x", seq_along(new_names)),
                      new_names)
  raw <- raw_all[-seq_len(hdr_row), , drop = FALSE]
  names(raw) <- new_names
  
  # Normalize + canonicalize names -----------------------------------------
  names(raw) <- tolower(gsub("[^a-z0-9]+", "_", tolower(names(raw))))
  names(raw) <- gsub("^_+|_+$", "", names(raw))
  raw <- canonicalize_columns(raw)
  
  # Drop fully-empty rows ---------------------------------------------------
  raw <- raw[rowSums(!is.na(raw)) > 0, , drop = FALSE]
  
  raw$source_year <- year
  raw
}

# Safe wrapper — one bad year doesn't kill the run
read_year_safe <- purrr::possibly(read_year, otherwise = NULL, quiet = FALSE)

# ---- 1. Scrape the historical data page for XLS links -----------------------
links <- read_html(HIST_PAGE) %>%
  html_elements("a") %>%
  { tibble(text = html_text(.), href = html_attr(., "href")) } %>%
  filter(grepl("\\.xls", href, ignore.case = TRUE),
         grepl("Actual Flow", text, ignore.case = TRUE)) %>%
  mutate(
    year = as.integer(str_extract(text, "\\d{4}")),
    url  = ifelse(grepl("^http", href), href,
                  paste0("https://www.bchydro.com", href)),
    url  = utils::URLencode(url)
  ) %>%
  filter(!is.na(year)) %>%
  arrange(year) %>%
  distinct(year, .keep_all = TRUE)

message(sprintf("Found %d yearly files (%d–%d).",
                nrow(links), min(links$year), max(links$year)))

# ---- 2. Download + parse each year, bind into one raw table -----------------
flows_raw <- purrr::pmap(list(links$year, links$url), read_year_safe) |>
  purrr::compact() |>
  purrr::list_rbind()

message(sprintf("Parsed %s rows across %d years.",
                format(nrow(flows_raw), big.mark = ","),
                dplyr::n_distinct(flows_raw$source_year)))

# ---- 3. Coerce types + build the final `flows` table ------------------------
flows <- flows_raw %>%
  transmute(
    date_local  = parse_bchydro_date(date),
    hour        = suppressWarnings(as.integer(he)),
    bc_us_MWh   = suppressWarnings(as.numeric(us_tielines)),
    bc_ab_MWh   = suppressWarnings(as.numeric(ab_tielines)),
    source_year
  ) %>%
  filter(!is.na(date_local),
         !is.na(hour), hour >= 1, hour <= 24) %>%
  mutate(datetime_pst = ymd_h(paste(date_local, hour - 1),
                              tz = "America/Vancouver")) %>%
  arrange(datetime_pst)

# ---- 4. Sanity checks -------------------------------------------------------
message("\n── Sanity checks ──────────────────────────────────────────────")

# Hours per year (leap years 2008/2012/2016/2020/2024 should be 8784)
year_summary <- flows %>%
  group_by(source_year) %>%
  summarise(
    n_hours = n(),
    first   = min(datetime_pst),
    last    = max(datetime_pst),
    .groups = "drop"
  )
print(year_summary, n = Inf)

# Annual net for BC-US intertie (informational, using source convention)
message("\nAnnual BC-US totals (TWh, positive = export per BC Hydro convention):")
flows %>%
  group_by(source_year) %>%
  summarise(bc_us_TWh = sum(bc_us_MWh, na.rm = TRUE) / 1e6,
            bc_ab_TWh = sum(bc_ab_MWh, na.rm = TRUE) / 1e6,
            .groups = "drop") %>%
  print(n = Inf)

message("\nDone. Use `flows` for downstream analysis.\n")



# =============================================================================
# BC Hydro Balancing Authority Load loader
# ------------------------------------------------------------------------------
# Downloads yearly "Balancing Authority Load" XLS files from BC Hydro's
# historical transmission data page, normalizes across vintages, and produces
# `load_df`:
#
#   date_local       Date       calendar date, Pacific time
#   hour             int        hour-ending (1-24)
#   gross_load_MWh   num        BC Hydro balancing-area gross telemetered load
#   source_year      int        year of the source file
#   datetime_pst     POSIXct    hour-beginning timestamp, America/Vancouver
#
# Relies on helpers (parse_bchydro_date, find_header_row, is_excel_bytes, %||%)
# already defined by load_bchydro_flows.R. source() that file first, or paste
# those helpers above this block.
# =============================================================================

library(rvest)
library(httr)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(stringr)

HIST_PAGE_LOAD <- paste0(
  "https://www.bchydro.com/energy-in-bc/operations/",
  "transmission/transmission-system/",
  "balancing-authority-load-data/historical-transmission-data.html"
)
CACHE_DIR_LOAD <- "bchydro_load_cache"
dir.create(CACHE_DIR_LOAD, showWarnings = FALSE)

# ---- Canonical column renaming for load files -------------------------------
canonicalize_load_columns <- function(raw) {
  rename_map <- c(
    date        = "date",
    day         = "date",
    datetime    = "date",
    he          = "he",
    hr          = "he",
    hour        = "he",
    hour_ending = "he",
    load        = "gross_load",
    balancing   = "gross_load",
    gross       = "gross_load",
    total       = "gross_load",
    bc_load     = "gross_load",
    control_area_load = "gross_load",
    control_area      = "gross_load",   # ← safety net
    control           = "gross_load",   # ← safety net
    cal               = "gross_load",   # ← older abbreviation
    ba_load           = "gross_load"    # ← newer abbreviation
    
  )
  new <- names(raw)
  for (from in names(rename_map)) {
    hits <- grep(paste0("^", from), new, ignore.case = TRUE)
    if (length(hits)) new[hits] <- rename_map[[from]]
  }
  names(raw) <- new
  keep <- names(raw) %in% c("date", "he", "gross_load")
  raw[, keep, drop = FALSE]
}

# ---- Per-year download + parse ----------------------------------------------
read_load_year <- function(year, url) {
  local <- file.path(CACHE_DIR_LOAD,
                     gsub(" ", "_", basename(url)))
  
  if (!file.exists(local) || file.info(local)$size == 0) {
    tmp     <- tempfile(fileext = ".xls")
    url_enc <- utils::URLencode(url)
    
    resp <- GET(
      url_enc,
      write_disk(tmp, overwrite = TRUE),
      add_headers(
        `User-Agent` = "Mozilla/5.0 (compatible; R data-analysis script)",
        Referer      = HIST_PAGE_LOAD,
        Accept       = "application/vnd.ms-excel,application/octet-stream,*/*"
      ),
      timeout(60)
    )
    if (http_error(resp)) {
      stop(sprintf("HTTP %s for %s", status_code(resp), url_enc))
    }
    ctype <- headers(resp)$`content-type` %||% ""
    if (grepl("text/html", ctype, ignore.case = TRUE)) {
      stop(sprintf("Server returned HTML (not .xls) for %s.", url_enc))
    }
    if (!is_excel_bytes(tmp)) {
      stop(sprintf("Downloaded file for %d is not a valid Excel file.", year))
    }
    file.rename(tmp, local)
  }
  
  raw_all <- read_excel(
    local, sheet = 1,
    col_names = FALSE,
    col_types = "text",
    guess_max = 20000,
    .name_repair = "unique_quiet"
  )
  
  hdr_row   <- find_header_row(raw_all)
  new_names <- as.character(unlist(raw_all[hdr_row, ]))
  new_names <- ifelse(is.na(new_names) | new_names == "",
                      paste0("x", seq_along(new_names)),
                      new_names)
  raw <- raw_all[-seq_len(hdr_row), , drop = FALSE]
  names(raw) <- new_names
  
  names(raw) <- tolower(gsub("[^a-z0-9]+", "_", tolower(names(raw))))
  names(raw) <- gsub("^_+|_+$", "", names(raw))
  raw <- canonicalize_load_columns(raw)
  
  raw <- raw[rowSums(!is.na(raw)) > 0, , drop = FALSE]
  raw$source_year <- year
  raw
}

read_load_year_safe <- purrr::possibly(read_load_year,
                                       otherwise = NULL, quiet = FALSE)

# ---- 1. Scrape the load historical page for XLS links -----------------------
load_links <- read_html(HIST_PAGE_LOAD) %>%
  html_elements("a") %>%
  { tibble(text = html_text(.), href = html_attr(., "href")) } %>%
  filter(grepl("\\.xls", href, ignore.case = TRUE),
         grepl("Balancing Authority Load|Load", text, ignore.case = TRUE)) %>%
  mutate(
    year = as.integer(str_extract(text, "\\d{4}")),
    url  = ifelse(grepl("^http", href), href,
                  paste0("https://www.bchydro.com", href)),
    url  = utils::URLencode(url)
  ) %>%
  filter(!is.na(year)) %>%
  arrange(year) %>%
  distinct(year, .keep_all = TRUE)

message(sprintf("Found %d yearly load files (%d–%d).",
                nrow(load_links), min(load_links$year), max(load_links$year)))

# ---- 2. Download + parse each year ------------------------------------------
load_raw <- purrr::pmap(list(load_links$year, load_links$url),
                        read_load_year_safe) |>
  purrr::compact() |>
  purrr::list_rbind()

# ---- 3. Coerce types + build `load_df` --------------------------------------
load_df <- load_raw %>%
  transmute(
    date_local     = parse_bchydro_date(date),
    hour           = suppressWarnings(as.integer(he)),
    gross_load_MWh = suppressWarnings(as.numeric(gross_load)),
    source_year
  ) %>%
  filter(!is.na(date_local),
         !is.na(hour), hour >= 1, hour <= 24,
         !is.na(gross_load_MWh)) %>%
  mutate(datetime_pst = ymd_h(paste(date_local, hour - 1),
                              tz = "America/Vancouver")) %>%
  arrange(datetime_pst)

# ---- 4. Sanity checks -------------------------------------------------------
message("\n── Load sanity checks ─────────────────────────────────────────")
load_df %>%
  group_by(source_year) %>%
  summarise(n_hours = n(),
            first   = min(datetime_pst),
            last    = max(datetime_pst),
            peak_MW = max(gross_load_MWh, na.rm = TRUE),
            mean_MW = round(mean(gross_load_MWh, na.rm = TRUE)),
            .groups = "drop") %>%
  print(n = Inf)

message("\nDone. Use `load_df` for downstream analysis.\n")

library(patchwork)   # for stacking the two panels

plot_bc_supply_demand <- function(flows,
                                  load_df,
                                  start_date,
                                  end_date,
                                  aggregate = c("hour", "day", "week", "month"),
                                  layout    = c("stacked", "single")) {
  
  aggregate <- match.arg(aggregate)
  layout    <- match.arg(layout)
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  # ---- Flows: same prep as plot_bc_interties() ----------------------------
  flow_agg <- flows %>%
    filter(date_local >= start_date, date_local <= end_date) %>%
    mutate(`BC-US`      = -bc_us_MWh,
           `BC-Alberta` = -bc_ab_MWh,
           period = lubridate::floor_date(datetime_pst, aggregate)) %>%
    group_by(period) %>%
    summarise(`BC-US`      = sum(`BC-US`,      na.rm = TRUE),
              `BC-Alberta` = sum(`BC-Alberta`, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(net_import = `BC-US` + `BC-Alberta`)
  
  # ---- Load: aggregate the same way ---------------------------------------
  load_agg <- load_df %>%
    filter(date_local >= start_date, date_local <= end_date) %>%
    mutate(period = lubridate::floor_date(datetime_pst, aggregate)) %>%
    group_by(period) %>%
    summarise(load_MWh = sum(gross_load_MWh, na.rm = TRUE),
              .groups  = "drop")
  
  if (nrow(flow_agg) == 0 || nrow(load_agg) == 0) {
    stop(sprintf("No overlapping data between %s and %s.",
                 start_date, end_date))
  }
  
  # ---- Derived: net generation implied by BC ------------------------------
  # generation = load - net_import (positive net_import means less local gen)
  combined <- flow_agg %>%
    inner_join(load_agg, by = "period") %>%
    mutate(net_generation = load_MWh - net_import)
  
  # ---- Long form for intertie stack ---------------------------------------
  flow_long <- combined %>%
    select(period, `BC-US`, `BC-Alberta`) %>%
    pivot_longer(-period, names_to = "intertie", values_to = "MWh") %>%
    mutate(intertie = factor(intertie, levels = c("BC-Alberta", "BC-US")))
  
  # ---- Panel A: intertie flows -------------------------------------------
  y_max_flow <- max(abs(c(combined$`BC-US` + pmax(combined$`BC-Alberta`, 0),
                          combined$`BC-US` + pmin(combined$`BC-Alberta`, 0),
                          combined$net_import)),
                    na.rm = TRUE)
  label_y <- y_max_flow * 0.92
  
  p_flow <- ggplot() +
    geom_area(data = flow_long,
              aes(x = period, y = MWh, fill = intertie),
              position = "stack", alpha = 0.75) +
    geom_line(data = combined,
              aes(x = period, y = net_import, colour = "Net import"),
              linewidth = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey30") +
    annotate("text", x = min(combined$period), y =  label_y,
             label = "← Import of electricity",
             hjust = 0, vjust = 1, fontface = "bold",
             colour = "grey25", size = 3.4) +
    annotate("text", x = min(combined$period), y = -label_y,
             label = "← Export of electricity",
             hjust = 0, vjust = 0, fontface = "bold",
             colour = "grey25", size = 3.4) +
    scale_y_continuous(labels = scales::comma,
                       limits = c(-y_max_flow, y_max_flow) * 1.05,
                       expand = expansion(mult = c(0, 0))) +
    scale_x_datetime(expand = expansion(mult = c(0, 0.01))) +
    scale_fill_manual(values = c("BC-US" = "#3B7DB1",
                                 "BC-Alberta" = "#E08A3C"),
                      name = "Intertie") +
    scale_colour_manual(values = c("Net import" = "black"), name = NULL) +
    labs(y = paste0("Intertie flow (MWh per ", aggregate, ")"),
         x = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank())
  
  # ---- Panel B: load + implied generation --------------------------------
  load_long <- combined %>%
    select(period, `BC gross load` = load_MWh,
           `Net BC generation` = net_generation) %>%
    pivot_longer(-period, names_to = "series", values_to = "MWh") %>%
    mutate(series = factor(series,
                           levels = c("BC gross load", "Net BC generation")))
  
  p_load <- ggplot(load_long, aes(x = period, y = MWh,
                                  colour = series, linewidth = series)) +
    geom_line() +
    scale_y_continuous(labels = scales::comma,
                       expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_datetime(expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = c("BC gross load"     = "#2E2E2E",
                                   "Net BC generation" = "#5A9E5A"),
                        name = NULL) +
    scale_linewidth_manual(values = c("BC gross load" = 0.8,
                                      "Net BC generation" = 0.6),
                           guide = "none") +
    labs(y = paste0("MWh per ", aggregate), x = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank())
  
  # ---- Assemble ----------------------------------------------------------
  title <- sprintf("BC supply & demand: %s to %s",
                   format(start_date, "%b %d, %Y"),
                   format(end_date,   "%b %d, %Y"))
  subtitle <- sprintf(
    "Aggregated by %s. Net generation = load − net imports (implied residual).",
    aggregate)
  
  if (layout == "single") {
    # Overlay everything on one panel using load as a light contextual line
    p <- p_flow +
      geom_line(data = combined,
                aes(x = period, y = load_MWh, linetype = "BC load"),
                colour = "grey20", linewidth = 0.5) +
      scale_linetype_manual(values = c("BC load" = "dashed"), name = NULL) +
      labs(title = title, subtitle = subtitle)
    return(p)
  }
  
  # Default: two stacked panels
  (p_load / p_flow) +
    patchwork::plot_layout(heights = c(1, 1.6)) +
    patchwork::plot_annotation(title = title, subtitle = subtitle,
                               theme = theme(
                                 plot.title    = element_text(face = "bold"),
                                 plot.subtitle = element_text(colour = "grey40")
                               ))
}
`