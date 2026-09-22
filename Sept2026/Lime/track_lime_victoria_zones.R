# =============================================================================
# track_lime_victoria_zones.R
#
# Victoria BC's Lime e-bike network uses VIRTUAL parking zones rather than
# physical docks, so GBFS station_status.json carries no useful information
# (see prior discussion). Instead, this script polls free_bike_status.json
# every 60 seconds, which lists each currently-available bike's own lat/lon,
# and detects events by MATCHING BIKES BY POSITION between polls, not by ID.
#
# Lime appears to use "Dynamic Vehicle IDs", where
# every bike's ID is randomly rotated on a fixed timer (observed here as a
# full-fleet ID turnover roughly every 15 minutes), independent of whether
# the bike actually moved. 
#
# Instead, each poll we spatially match every bike in the current snapshot
# to the nearest bike in the previous snapshot (greedy nearest-neighbor,
# mutually exclusive pairing). If a match is within DISTANCE_THRESHOLD_M,
# it's treated as the same physical bike that simply got a new ID -- no
# event is logged. Only genuinely unmatched bikes count as events:
#   - Unmatched in the PREVIOUS snapshot -> inferred checked out ("taken_out")
#   - Unmatched in the CURRENT snapshot  -> inferred just returned ("returned")
#
# Each event's coordinates are snapped to the nearest named zone from
# victoria_ebike_zones.csv (129 point locations extracted from the City of
# Victoria's "E-Bike Share Parking Zones" map) using great-circle distance.
#
# Output columns: bike_latitude, bike_longitude, station_name, event, timestamp
#
# CAVEATS (worth remembering when interpreting results):
#   - GBFS requires bike_id to be rotated after each trip for privacy, so a
#     "taken_out" event and a later "returned" event are NOT guaranteed to
#     be the same physical bike -- don't try to pair them into a trip.
#   - A bike vanishing from the feed can also mean it was picked up by a
#     Lime rebalancing vehicle, not necessarily a rider.
#   - "Nearest zone" is a straight-line nearest-neighbor snap. A bike
#     reported slightly outside every zone's boundary still gets assigned
#     to whichever zone centroid is geographically closest.
# =============================================================================

# ---- Setup ------------------------------------------------------------

required_pkgs <- c("httr", "jsonlite")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(httr)
library(jsonlite)

GBFS_DISCOVERY_URL   <- "https://data.lime.bike/api/partners/v2/gbfs/victoria/gbfs.json"
ZONES_CSV            <- "victoria_ebike_zones.csv"   # zone_name, latitude, longitude
POLL_INTERVAL_SEC    <- 60
OUTPUT_CSV           <- "lime_victoria_zone_events.csv"
REQUEST_TIMEOUT      <- 20  # seconds, per HTTP request
DISTANCE_THRESHOLD_M <- 30  # max distance (meters) to treat two observations
# across polls as the same physical bike rather
# than a checkout/return. GPS on these bikes is
# typically accurate to a few meters when parked;
# 20m gives headroom for drift without being so
# loose that a short walk-and-reparking within a
# zone gets swallowed. Tighten/loosen if your
# event counts look too high/low.

# ---- Helpers ------------------------------------------------------------

fetch_json <- function(url) {
  resp <- GET(url, timeout(REQUEST_TIMEOUT), user_agent("R GBFS tracker (personal use)"))
  stop_for_status(resp)
  fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
}

# Look up a named feed's URL from the GBFS auto-discovery document.
get_feed_url <- function(discovery_url, feed_name) {
  disc <- fetch_json(discovery_url)
  lang <- names(disc$data)[1]                 # e.g. "en"
  feeds <- disc$data[[lang]]$feeds
  url <- feeds$url[feeds$name == feed_name]
  if (length(url) == 0) stop(sprintf("No '%s' feed found in discovery document.", feed_name))
  url
}

# Pull bike_id/lat/lon of every currently available bike.
# GBFS v2.x field is 'bike_id'; v3.0 renames it 'vehicle_id' -- handle both.
fetch_available_bikes <- function(url) {
  raw <- fetch_json(url)
  bikes <- raw$data$bikes
  if (is.null(bikes)) bikes <- raw$data$vehicles   # v3.0 fallback
  
  id_col <- if ("bike_id" %in% names(bikes)) "bike_id" else "vehicle_id"
  
  data.frame(
    bike_id   = as.character(bikes[[id_col]]),
    latitude  = as.numeric(bikes$lat),
    longitude = as.numeric(bikes$lon),
    stringsAsFactors = FALSE
  )
}

# Haversine great-circle distance in meters between two lat/lon points.
haversine_m <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

# Vectorized nearest-zone lookup: for each (lat, lon), return the name of
# the closest zone in `zones`.
snap_to_nearest_zone <- function(lat, lon, zones) {
  vapply(seq_along(lat), function(i) {
    d <- haversine_m(lat[i], lon[i], zones$latitude, zones$longitude)
    zones$zone_name[which.min(d)]
  }, character(1))
}

# Match bikes across two snapshots by POSITION rather than by ID (see header
# note on Dynamic Vehicle IDs). Returns the rows of `prev` and `curr` that
# could NOT be matched to a counterpart within DISTANCE_THRESHOLD_M -- i.e.
# the genuine taken_out / returned events.
#
# Greedy mutual nearest-neighbor: repeatedly pick the closest remaining
# (prev, curr) pair; if it's within the threshold, mark both as matched and
# remove them from further consideration; stop once the closest remaining
# pair exceeds the threshold (or one side runs out). This is O(n^2) per
# poll, which is fine for fleets of a few hundred bikes polled once a
# minute; it would need a smarter algorithm (e.g. a KD-tree) for very large
# fleets.
match_snapshots <- function(prev, curr, threshold_m) {
  if (nrow(prev) == 0 || nrow(curr) == 0) {
    return(list(taken_out = prev, returned = curr))
  }
  
  dist_mat <- matrix(NA_real_, nrow = nrow(prev), ncol = nrow(curr))
  for (i in seq_len(nrow(prev))) {
    dist_mat[i, ] <- haversine_m(prev$latitude[i], prev$longitude[i],
                                 curr$latitude, curr$longitude)
  }
  
  matched_prev <- logical(nrow(prev))
  matched_curr <- logical(nrow(curr))
  
  repeat {
    working <- dist_mat
    working[matched_prev, ] <- Inf
    working[, matched_curr] <- Inf
    
    min_dist <- suppressWarnings(min(working))
    if (!is.finite(min_dist) || min_dist > threshold_m) break
    
    idx <- which(working == min_dist, arr.ind = TRUE)[1, ]
    matched_prev[idx["row"]] <- TRUE
    matched_curr[idx["col"]] <- TRUE
  }
  
  list(
    taken_out = prev[!matched_prev, , drop = FALSE],
    returned  = curr[!matched_curr, , drop = FALSE]
  )
}

append_to_csv <- function(df_rows, path) {
  if (nrow(df_rows) == 0) return(invisible())
  write.table(
    df_rows, path, sep = ",", append = file.exists(path),
    row.names = FALSE, col.names = !file.exists(path)
  )
}

# ---- Load zones -----------------------------------------------------------

zones <- read.csv(ZONES_CSV, stringsAsFactors = FALSE)
message(sprintf("Loaded %d parking zones from %s", nrow(zones), ZONES_CSV))

# ---- Initialize -------------------------------------------------------

message("Discovering free_bike_status feed...")
FREE_BIKE_STATUS_URL <- get_feed_url(GBFS_DISCOVERY_URL, "free_bike_status")
message("Using feed: ", FREE_BIKE_STATUS_URL)

event_log <- data.frame(
  bike_latitude  = numeric(),
  bike_longitude = numeric(),
  station_name   = character(),
  event          = character(),
  timestamp      = as.POSIXct(character()),
  stringsAsFactors = FALSE
)

message("Fetching baseline snapshot...")
previous_snapshot <- fetch_available_bikes(FREE_BIKE_STATUS_URL)
message(sprintf("Baseline captured: %d available bikes at %s",
                nrow(previous_snapshot), format(Sys.time(), "%H:%M:%S")))

# ---- Main polling loop -----------------------------------------------------
# Runs indefinitely -- stop with Ctrl+C (or the Stop button in RStudio).

repeat {
  loop_start <- Sys.time()
  Sys.sleep(POLL_INTERVAL_SEC)
  
  current_snapshot <- tryCatch(
    fetch_available_bikes(FREE_BIKE_STATUS_URL),
    error = function(e) {
      message(sprintf("[%s] fetch failed: %s", format(Sys.time(), "%H:%M:%S"), conditionMessage(e)))
      NULL
    }
  )
  
  if (!is.null(current_snapshot)) {
    now <- Sys.time()
    
    matches <- match_snapshots(previous_snapshot, current_snapshot, DISTANCE_THRESHOLD_M)
    taken_out_rows <- matches$taken_out
    returned_rows  <- matches$returned
    n_events       <- nrow(taken_out_rows) + nrow(returned_rows)
    
    if (n_events > 0) {
      new_rows <- data.frame(
        bike_latitude  = c(taken_out_rows$latitude, returned_rows$latitude),
        bike_longitude = c(taken_out_rows$longitude, returned_rows$longitude),
        event          = c(rep("taken_out", nrow(taken_out_rows)),
                           rep("returned", nrow(returned_rows))),
        timestamp      = rep(now, n_events),   # explicit length avoids
        # recycling errors when
        # n_events would otherwise be 0
        stringsAsFactors = FALSE
      )
      
      new_rows$station_name <- snap_to_nearest_zone(
        new_rows$bike_latitude, new_rows$bike_longitude, zones
      )
      # Reorder to match requested column order
      new_rows <- new_rows[, c("bike_latitude", "bike_longitude",
                               "station_name", "event", "timestamp")]
      
      event_log <- rbind(event_log, new_rows)
      append_to_csv(new_rows, OUTPUT_CSV)
    }
    
    message(sprintf("[%s] %d taken_out, %d returned (bikes online: %d)",
                    format(now, "%H:%M:%S"),
                    nrow(taken_out_rows), nrow(returned_rows),
                    nrow(current_snapshot)))
    
    previous_snapshot <- current_snapshot
  }
}

##
x<-event_log %>% group_by(station_name, event) %>% summarise(count_events = n())
