# =============================================================================
# data_quality.R
# Data quality checks and risk flagging for clinical trial data
# Reflects centralized monitoring / RBQM thinking
# =============================================================================

library(dplyr)
library(lubridate)

# -----------------------------------------------------------------------------
# Individual QC checks — each returns the df with a boolean flag column added
# -----------------------------------------------------------------------------

#' Flag studies missing a completion date
flag_missing_completion <- function(df) {
  df |> mutate(
    flag_no_completion_date = is.na(completion_date_parsed)
  )
}

#' Flag studies missing primary outcome definition
flag_missing_primary_outcome <- function(df) {
  df |> mutate(
    flag_no_primary_outcome = is.na(primary_outcome) |
      primary_outcome == "" |
      primary_outcome == "NA"
  )
}

#' Flag studies missing phase information
flag_missing_phase <- function(df) {
  df |> mutate(
    flag_no_phase = phase %in% c("N/A", "", "NA") | is.na(phase)
  )
}

#' Flag studies that are overdue: past completion date but still RECRUITING
flag_overdue_recruiting <- function(df, reference_date = Sys.Date()) {
  df |> mutate(
    flag_overdue = !is.na(completion_date_parsed) &
      completion_date_parsed < reference_date &
      status == "RECRUITING"
  )
}

#' Flag studies terminated without a reason given
flag_terminated_no_reason <- function(df) {
  df |> mutate(
    flag_terminated_no_reason = status == "TERMINATED" &
      (is.na(why_stopped) | why_stopped == "")
  )
}

#' Flag studies with suspiciously large or zero enrollment
flag_enrollment_anomaly <- function(df,
                                     lower = 1,
                                     upper = 100000) {
  df |> mutate(
    flag_enrollment_anomaly = !is.na(enrollment_count) &
      (enrollment_count < lower | enrollment_count > upper)
  )
}

#' Flag studies missing country / location information
flag_missing_location <- function(df) {
  df |> mutate(
    flag_no_location = is.na(countries) | countries == "" | countries == "NA"
  )
}

#' Flag studies with UNKNOWN status (regulatory grey area)
flag_unknown_status <- function(df) {
  df |> mutate(
    flag_unknown_status = status == "UNKNOWN"
  )
}

#' Flag studies that have completed but never posted results
flag_no_results_posted <- function(df, months_grace = 12) {
  cutoff <- Sys.Date() - months(months_grace)
  df |> mutate(
    flag_no_results = status == "COMPLETED" &
      !has_results &
      !is.na(completion_date_parsed) &
      completion_date_parsed < cutoff
  )
}


# -----------------------------------------------------------------------------
# Run all checks and compute composite risk score
# -----------------------------------------------------------------------------

run_quality_checks <- function(df) {

  # Ensure parsed date columns exist regardless of how data was loaded
  if (!"completion_date_parsed" %in% names(df)) {
    df <- df |> mutate(
      start_date_parsed         = parse_date_time(start_date,        orders = c("Y-m-d", "Y-m", "Y")),
      completion_date_parsed    = parse_date_time(completion_date,   orders = c("Y-m-d", "Y-m", "Y")),
      primary_completion_parsed = parse_date_time(primary_completion, orders = c("Y-m-d", "Y-m", "Y"))
    )
  }

  df |>
    flag_missing_completion()     |>
    flag_missing_primary_outcome()|>
    flag_missing_phase()          |>
    flag_overdue_recruiting()     |>
    flag_terminated_no_reason()   |>
    flag_enrollment_anomaly()     |>
    flag_missing_location()       |>
    flag_unknown_status()         |>
    flag_no_results_posted()      |>
    mutate(
      risk_score = rowSums(
        across(c(
          flag_no_completion_date, flag_no_primary_outcome, flag_no_phase,
          flag_overdue, flag_terminated_no_reason, flag_enrollment_anomaly,
          flag_no_location, flag_unknown_status, flag_no_results
        ), as.integer),
        na.rm = TRUE
      ),
      risk_level = case_when(
        risk_score == 0             ~ "Clean",
        risk_score == 1             ~ "Low",
        risk_score %in% c(2, 3)    ~ "Medium",
        risk_score >= 4             ~ "High",
        TRUE                        ~ "Unknown"
      ),
      risk_level = factor(risk_level,
                          levels = c("Clean", "Low", "Medium", "High"))
    )
}


# -----------------------------------------------------------------------------
# Summary table: how many studies triggered each flag
# -----------------------------------------------------------------------------

quality_summary <- function(df_qc) {

  flag_cols <- names(df_qc)[startsWith(names(df_qc), "flag_")]

  tibble(
    check = flag_cols,
    label = c(
      "Missing completion date",
      "Missing primary outcome",
      "Missing phase",
      "Overdue but still recruiting",
      "Terminated — no reason given",
      "Enrollment anomaly (0 or >100k)",
      "No location data",
      "Unknown status",
      "Completed — no results posted"
    )[seq_along(flag_cols)]
  ) |>
    rowwise() |>
    mutate(
      n_flagged = sum(df_qc[[check]], na.rm = TRUE),
      pct       = round(100 * n_flagged / nrow(df_qc), 1)
    ) |>
    ungroup() |>
    arrange(desc(n_flagged)) |>
    select(label, n_flagged, pct)
}


# -----------------------------------------------------------------------------
# Enrollment completeness: actual vs anticipated
# -----------------------------------------------------------------------------

enrollment_analysis <- function(df_qc) {

  df_qc |>
    filter(
      enrollment_type == "ACTUAL",
      !is.na(enrollment_count),
      !is.na(phase),
      !flag_no_phase
    ) |>
    group_by(phase, status) |>
    summarise(
      n_studies         = n(),
      median_enrollment = median(enrollment_count, na.rm = TRUE),
      q1                = quantile(enrollment_count, 0.25, na.rm = TRUE),
      q3                = quantile(enrollment_count, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
}


# -----------------------------------------------------------------------------
# Status timeline: studies per year by status
# -----------------------------------------------------------------------------

status_timeline <- function(df_qc) {

  df_qc |>
    filter(!is.na(start_date_parsed)) |>
    mutate(start_year = year(start_date_parsed)) |>
    filter(start_year >= 2005) |>
    count(start_year, status) |>
    arrange(start_year, status)
}
