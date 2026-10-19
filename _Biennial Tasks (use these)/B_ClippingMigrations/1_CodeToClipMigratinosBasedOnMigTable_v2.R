# Clip GPS data to migration windows based on migration table
# Author: Luke Wilde
# Version 2 - Refactored to use CompleteSpring/CompleteFall columns directly
#             and unified Spring/Fall processing into a single function.

library(tidyverse)
library(sf)
library(lubridate)
library(furrr)
library(future)

select    <- dplyr::select
filter    <- dplyr::filter
rename    <- dplyr::rename
summarise <- dplyr::summarise

# ---- Load data ----

load("G:/Other computers/My Laptop/RDH Database/RD2H_GPSCollarData/RDH_GPSdata_HS+AC+LW+KM+MK_05.29.2026.RData")
load("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/AllSpringFallMigrationTiming_2011to2025_05122026.RData")
mig <- full_migtab
rm(full_migtab)

# ---- Prep ----

# Standardize key column types and create id_yr
mig <- mig |>
  mutate(
    AID   = as.character(AID),
    Year  = as.character(Year),
    id_yr = paste0(AID, "_", Year),
    # Ensure all date columns are Date class (not POSIXct)
    across(where(\(x) inherits(x, "POSIXct")), as.Date)
  ) 

#drop any duplicates
#in this case 446_2020 with two summer walk abouts identified
mig <- mig %>% group_by(AID, SpringStartDate) %>% slice_head(n=1) %>% ungroup()

all_rdh_data.sf <- all_rdh_data.sf |>
  mutate(
    AID   = as.character(AID),
    Year  = as.character(Year),
    Date  = as.Date(POSIXct),
    Month = month(POSIXct),
    id_yr = paste0(AID, "_", Year)
  )

# ---- Core clipping function ----

# Clips GPS data for one animal-year to a single migration window.
# For "Yes":     clips start-to-end, returns GPS columns as-is.
# For "Partial": clips start-to-end, appends Notes and PartialReason columns.
# Returns NULL for NA, "No", or missing dates.
clip_season <- function(mig_row, gps, season = c("Spring", "Fall")) {
  season <- match.arg(season)

  start_col    <- paste0(season, "StartDate")
  end_col      <- paste0(season, "EndDate")
  complete_col <- paste0("Complete", season)
  notes_col    <- paste0(season, "Notes")
  reason_col   <- paste0(season, "PartialReason")
  pnotes_col   <- paste0(season, "PartialNotes")

  status <- mig_row[[complete_col]]
  if (is.na(status) || !(status %in% c("Yes", "Partial"))) return(NULL)

  start <- mig_row[[start_col]]
  end   <- mig_row[[end_col]]
  if (is.na(start) || is.na(end)) return(NULL)

  sel <- gps |>
    filter(
      AID == mig_row$AID,
      as.Date(POSIXct) >= as.Date(start),
      as.Date(POSIXct) <= as.Date(end)
    )

  if (nrow(sel) == 0) return(NULL)

  sel$id_yr        <- mig_row$id_yr
  sel$MigComplete  <- status   # season-specific status flag

  if (status == "Partial") {
    sel[[notes_col]]  <- mig_row[[notes_col]]
    sel[[reason_col]] <- mig_row[[reason_col]]
    sel[[pnotes_col]] <- mig_row[[pnotes_col]]
  }

  sel
}

# ---- Helper: run clip_season over a filtered mig subset ----

run_clip <- function(mig_sub, gps, season) {
  plan(sequential)   # swap to multisession if desired
  options(future.globals.maxSize = 15 * 1024^3)
  on.exit(options(future.globals.maxSize = NULL))

  seq_len(nrow(mig_sub)) |>
    future_map(
      \(i) clip_season(mig_row = mig_sub[i, ], gps = gps, season = season),
      .progress = TRUE
    ) |>
    compact() |>
    bind_rows() |>
    arrange(AID, POSIXct)
}

# ---- Helper: build summary table from mig table ----
#
# Pulls start/stop directly from the mig table rather than deriving them from
# GPS timestamps. This avoids a year-boundary split where GPS points in Jan of
# year+1 carry Year = N+1, causing group_by to fragment cross-year migrations.

make_summary <- function(mig_tab, season) {
  start_col    <- paste0(season, "StartDate")
  end_col      <- paste0(season, "EndDate")
  complete_col <- paste0("Complete", season)
  notes_col    <- paste0(season, "Notes")
  reason_col   <- paste0(season, "PartialReason")

  mig_tab |>
    filter(
      !is.na(.data[[complete_col]]),
      .data[[complete_col]] %in% c("Yes", "Partial"),
      !is.na(.data[[start_col]])
    ) |>
    transmute(
      AID, Year, id_yr,
      MigComplete   = .data[[complete_col]],
      start         = as.Date(.data[[start_col]]),
      # For Partial: stop/length are NA — migration wasn't observed to completion
      stop          = if_else(.data[[complete_col]] == "Partial",
                              as.Date(NA), as.Date(.data[[end_col]])),
      length        = if_else(.data[[complete_col]] == "Partial",
                              NA_real_,
                              as.numeric(as.Date(.data[[end_col]]) - as.Date(.data[[start_col]]))),
      Note          = .data[[notes_col]],
      PartialReason = .data[[reason_col]]
    ) |>
    arrange(id_yr)
}

# ========================================================
# Spring Migration
# ========================================================

spring_mig <- mig |>
  filter(
    CompleteSpring %in% c("Yes", "Partial"),
    !is.na(SpringStartDate),
    !is.na(SpringEndDate)
  )

start <- Sys.time()
mig.SpringMigration.sf <- run_clip(spring_mig, gps = all_rdh_data.sf, season = "Spring")
message("Spring done: ", round(difftime(Sys.time(), start, units = "mins"), 1), " min")

Spring.summary <- make_summary(mig, season = "Spring")

message(nrow(Spring.summary %>% distinct(AID,Year)) == nrow(Spring.summary))

# ========================================================
# Fall Migration
# ========================================================

fall_mig <- mig |>
  filter(
    CompleteFall %in% c("Yes", "Partial"),
    !is.na(FallStartDate),
    !is.na(FallEndDate)
  )

start <- Sys.time()
mig.FallMigration.sf <- run_clip(fall_mig, gps = all_rdh_data.sf, season = "Fall")
message("Fall done: ", round(difftime(Sys.time(), start, units = "mins"), 1), " min")

Fall.summary <- make_summary(mig, season = "Fall")

# Flag cross-year migrations (fall start before July means it belongs to prior year)
Fall.summary <- Fall.summary |>
  mutate(Year.mig = if_else(month(start) < 7, as.numeric(Year) - 1, as.numeric(Year)), id_yr.mig = paste0(AID,"_",Year.mig))

#see those that started fall mig after NYE
Fall.summary %>% filter(id_yr != id_yr.mig)

# ========================================================
# Quick checks
# ========================================================

Spring.summary |> dplyr::count(MigComplete)
Fall.summary   |> dplyr::count(MigComplete)



# ========================================================
# Seasonal Ranges
# ========================================================

# Generic row-wise clip runner (reused for summer and winter)
run_season_clip <- function(df, gps, clip_fn) {
  plan(sequential)
  options(future.globals.maxSize = 15 * 1024^3)
  on.exit(options(future.globals.maxSize = NULL))
  
  seq_len(nrow(df)) |>
    future_map(
      \(i) tryCatch(
        clip_fn(df[i, ], gps),
        error = function(e) { message("Issue with row ", i, ": ", conditionMessage(e)); NULL }
      ),
      .progress = TRUE
    ) |>
    compact() |>
    bind_rows() |>
    arrange(AID, POSIXct)
}

#------------------------------#
# Summer Ranges ####
# Window: Spring end → Fall start (same AID/Year)
# Fallback if no Fall start: Spring end → Oct 15 of that year

#has to be a complete spring to reach summer!

joinedS <- Spring.summary |> filter(MigComplete == "Yes") |>
  select(AID, Year, id_yr, startSpring = start, stopSpring = stop) |>
  filter(!is.na(stopSpring)) |>   # must have completed spring migration
  left_join(
    Fall.summary |> select(id_yr = id_yr.mig, startFall = start),
    by = "id_yr"
  )

clip_summer <- function(row, gps) {
  end_date <- if (!is.na(row$startFall)) {
    as.Date(row$startFall)
  } else {
    as.Date(paste0(lubridate::year(row$stopSpring), "-10-15"))
  }
  
  sel <- gps |>
    filter(
      AID == row$AID,
      as.Date(POSIXct) >= as.Date(row$stopSpring),
      as.Date(POSIXct) <= end_date
    )
  
  if (nrow(sel) == 0) return(NULL)
  sel$id_yr <- row$id_yr
  sel
}

start <- Sys.time()
SummerRanges.sf <- run_season_clip(joinedS, gps = all_rdh_data.sf, clip_fn = clip_summer)
message("Summer done: ", round(difftime(Sys.time(), start, units = "mins"), 1), " min")

#------------------------------#
# Winter Ranges ####
# Window: Fall end → Spring start (Fall from Year-1, Spring from Year)
# Fallback if no prior Fall: Feb 1 of Spring start year → Spring start
#
# Uses Year.mig (biological fall year) rather than calendar Year on Fall.summary
# to correctly join late-starting falls (e.g., a migration beginning Feb Y that
# biologically belongs to the Fall Y-1 season).


joinedW <- Spring.summary |>
  select(AID, Year, id_yr, startSpring = start) |>
  mutate(fall_lookup = paste0(AID, "_", as.numeric(Year) - 1)) |>
  left_join(
    Fall.summary |> select(id_yr.mig, stopFall = stop),
    by = c("fall_lookup" = "id_yr.mig")
  ) |>
  select(-fall_lookup)

clip_winter <- function(row, gps) {
  start_date <- if (!is.na(row$stopFall)) {
    as.Date(row$stopFall)
  } else {
    # No prior fall on record — use Feb 1 of the spring year as a conservative start
    as.Date(paste0(lubridate::year(row$startSpring), "-02-01"))
  }
  end_date <- as.Date(row$startSpring)
  
  
  sel <- gps |>
    filter(
      AID == row$AID,
      as.Date(POSIXct) >= start_date,
      as.Date(POSIXct) <= end_date
    )
  
  if (nrow(sel) == 0) return(NULL)
  sel$id_yr <- row$id_yr
  sel
}

start <- Sys.time()
WinterRanges.sf <- run_season_clip(joinedW, gps = all_rdh_data.sf, clip_fn = clip_winter)
message("Winter done: ", round(difftime(Sys.time(), start, units = "mins"), 1), " min")


# ========================================================
# Save
# ========================================================


save(mig.FallMigration.sf, mig.SpringMigration.sf, Spring.summary, Fall.summary, file = "G:/Other computers/My Laptop/RDH Database/Processed Data/RDH_AllMigrations_2011t2025_06012026.RData")

save(WinterRanges.sf, SummerRanges.sf, file = "G:/Other computers/My Laptop/RDH Database/Processed Data/RDH_AllSeasonalRanges_2011t2025_06012026.RData")
