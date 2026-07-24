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

# ---- Packages ---------------------------------------------------------------
library(rvest)
library(httr)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(stringr)

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
  hour_pat <- "^he$|^hr$|^hour$|hour_?ending|hour ending"
  
  scan_n <- min(max_scan, nrow(df))
  for (i in seq_len(scan_n)) {
    row_vals <- tolower(trimws(as.character(unlist(df[i, ]))))
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

### Analaysis stesps

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

plot_bc_interties <- function(flows,
                              start_date,
                              end_date,
                              aggregate = c("hour", "day", "week", "month")) {
  
  aggregate <- match.arg(aggregate)
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  # ---- 1. Filter and flip sign to "positive = import into BC" -------------
  df <- flows %>%
    filter(date_local >= start_date, date_local <= end_date) %>%
    mutate(
      `BC-US`      = -bc_us_MWh,   # BC Hydro: +export → flip so +import
      `BC-Alberta` = -bc_ab_MWh
    )
  
  if (nrow(df) == 0) {
    stop(sprintf("No data between %s and %s.", start_date, end_date))
  }
  
  # ---- 2. Aggregate to requested resolution -------------------------------
  df <- df %>%
    mutate(period = lubridate::floor_date(datetime_pst, aggregate)) %>%
    group_by(period) %>%
    summarise(`BC-US`      = sum(`BC-US`,      na.rm = TRUE),
              `BC-Alberta` = sum(`BC-Alberta`, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(net = `BC-US` + `BC-Alberta`)
  
  # ---- 3. Reshape for stacked area ----------------------------------------
  long <- df %>%
    select(period, `BC-US`, `BC-Alberta`) %>%
    pivot_longer(-period, names_to = "intertie", values_to = "MWh") %>%
    mutate(intertie = factor(intertie, levels = c("BC-Alberta", "BC-US")))
  
  # ---- 4. Symmetric y-limits so import/export labels sit correctly --------
  y_max  <- max(abs(c(df$`BC-US` + pmax(df$`BC-Alberta`, 0),
                      df$`BC-US` + pmin(df$`BC-Alberta`, 0),
                      df$net)),
                na.rm = TRUE)
  y_lim  <- c(-y_max, y_max) * 1.05
  label_y <- y_max * 0.92
  
  # ---- 5. Plot -------------------------------------------------------------
  ggplot() +
    # Stacked area — geom_area handles signed stacking (up for +, down for −)
    geom_area(data = long,
              aes(x = period, y = MWh, fill = intertie),
              position = "stack", alpha = 0.75) +
    # Net-flow line on top
    geom_line(data = df,
              aes(x = period, y = net, colour = "Net flow"),
              linewidth = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey30") +
    # Import / Export labels
    annotate("text", x = min(df$period), y =  label_y,
             label = "← Import of electricity",
             hjust = 0, vjust = 1, fontface = "bold",
             colour = "grey25", size = 3.6) +
    annotate("text", x = min(df$period), y = -label_y,
             label = "← Export of electricity",
             hjust = 0, vjust = 0, fontface = "bold",
             colour = "grey25", size = 3.6) +
    scale_y_continuous(labels = comma, limits = y_lim,
                       expand = expansion(mult = c(0, 0))) +
    scale_x_datetime(expand = expansion(mult = c(0, 0.01))) +
    scale_fill_manual(values = c("BC-US" = "#3B7DB1",
                                 "BC-Alberta" = "#E08A3C"),
                      name = "Intertie") +
    scale_colour_manual(values = c("Net flow" = "black"),
                        name = NULL) +
    labs(
      title    = sprintf("BC intertie flows, %s to %s",
                         format(start_date, "%b %d, %Y"),
                         format(end_date,   "%b %d, %Y")),
      subtitle = sprintf("Aggregated by %s. Positive = net import into BC.",
                         aggregate),
      x = NULL,
      y = paste0("Flow (MWh per ", aggregate, ")")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position   = "bottom",
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold"),
      plot.subtitle     = element_text(colour = "grey40")
    )
}

