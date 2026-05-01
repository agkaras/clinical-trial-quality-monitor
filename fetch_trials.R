# =============================================================================
# fetch_trials.R
# Fetch cardiovascular / vascular metabolism clinical trials
# from ClinicalTrials.gov API v2
# =============================================================================

library(httr2)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

BASE_URL <- "https://clinicaltrials.gov/api/v2/studies"

# Cardiovascular / vascular metabolism search terms 
CV_QUERIES <- list(
  endothelial  = "endothelial dysfunction",
  oxidative    = "oxidative stress cardiovascular",
  mitochondria = "mitochondrial cardiovascular",
  metabolic_cv = "metabolic syndrome vascular",
  hypertension = "hypertension vascular",
  atherosclerosis = "atherosclerosis"
)

# Fields to retrieve from the API
STUDY_FIELDS <- paste(
  "NCTId",
  "BriefTitle",
  "OverallStatus",
  "Phase",
  "StudyType",
  "StartDate",
  "PrimaryCompletionDate",
  "CompletionDate",
  "EnrollmentCount",
  "EnrollmentType",
  "LeadSponsorName",
  "LeadSponsorClass",
  "Condition",
  "InterventionType",
  "PrimaryOutcomeMeasure",
  "LocationCountry",
  "WhyStopped",
  "HasResults",
  sep = ","
)

# -----------------------------------------------------------------------------
# Core fetch function (single query, handles pagination)
# -----------------------------------------------------------------------------

fetch_studies_page <- function(query_term,
                                page_token = NULL,
                                page_size  = 100) {

  req <- request(BASE_URL) |>
    req_url_query(
      `query.cond` = query_term,
      pageSize     = page_size,
      fields       = STUDY_FIELDS,
      format       = "json"
    )

  if (!is.null(page_token)) {
    req <- req |> req_url_query(pageToken = page_token)
  }

  resp <- req |>
    req_retry(max_tries = 3, backoff = ~ 2) |>
    req_perform()

  resp_body_json(resp, simplifyVector = FALSE)
}


# -----------------------------------------------------------------------------
# Parse a single study from the JSON response into a flat tibble row
# -----------------------------------------------------------------------------

parse_study <- function(study) {

  proto  <- study$protocolSection
  id_mod <- proto$identificationModule
  stat_mod <- proto$statusModule
  design_mod <- proto$designModule
  sponsor_mod <- proto$sponsorCollaboratorsModule
  cond_mod   <- proto$conditionsModule
  interv_mod <- proto$armsInterventionsModule
  outcome_mod <- proto$outcomesModule
  locations  <- proto$contactsLocationsModule

  tibble(
    nct_id               = id_mod$nctId %||% NA_character_,
    title                = id_mod$briefTitle %||% NA_character_,
    status               = stat_mod$overallStatus %||% NA_character_,
    why_stopped          = stat_mod$whyStopped %||% NA_character_,
    has_results          = study$hasResults %||% FALSE,
    phase                = paste(design_mod$phases %||% "N/A", collapse = "/"),
    study_type           = design_mod$studyType %||% NA_character_,
    enrollment_count     = design_mod$enrollmentInfo$count %||% NA_integer_,
    enrollment_type      = design_mod$enrollmentInfo$type %||% NA_character_,
    start_date           = stat_mod$startDateStruct$date %||% NA_character_,
    primary_completion   = stat_mod$primaryCompletionDateStruct$date %||% NA_character_,
    completion_date      = stat_mod$completionDateStruct$date %||% NA_character_,
    sponsor_name         = sponsor_mod$leadSponsor$name %||% NA_character_,
    sponsor_class        = sponsor_mod$leadSponsor$`class` %||% NA_character_,
    conditions           = paste(cond_mod$conditions %||% "Unknown", collapse = "; "),
    intervention_types   = paste(
      map_chr(interv_mod$interventions %||% list(), ~ .x$type %||% ""),
      collapse = "; "
    ),
    primary_outcome      = paste(
      map_chr(outcome_mod$primaryOutcomes %||% list(), ~ .x$measure %||% ""),
      collapse = "; "
    ),
    countries            = paste(
      unique(map_chr(
        locations$locations %||% list(),
        ~ .x$locationCountry %||% ""
      )),
      collapse = "; "
    )
  )
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x


# -----------------------------------------------------------------------------
# Fetch all studies for one query term (paginated)
# -----------------------------------------------------------------------------

fetch_all_studies <- function(query_term,
                               max_pages = 5,
                               verbose   = TRUE) {

  if (verbose) message("  Fetching: '", query_term, "'")

  all_studies <- list()
  page_token  <- NULL
  page        <- 1

  repeat {
    if (verbose) message("    Page ", page)
    body <- tryCatch(
      fetch_studies_page(query_term, page_token),
      error = function(e) { warning(e$message); NULL }
    )
    if (is.null(body) || length(body$studies) == 0) break

    parsed <- map(body$studies, safely(parse_study)) |>
      keep(~ is.null(.x$error)) |>
      map(~ .x$result)

    all_studies <- c(all_studies, parsed)

    page_token <- body$nextPageToken
    if (is.null(page_token) || page >= max_pages) break
    page <- page + 1
    Sys.sleep(0.3)   # be polite to the API
  }

  if (length(all_studies) == 0) return(tibble())
  bind_rows(all_studies)
}


# -----------------------------------------------------------------------------
# Main: fetch across all CV query terms and deduplicate
# -----------------------------------------------------------------------------

fetch_cv_trials <- function(max_pages = 5, verbose = TRUE) {

  if (verbose) message("Fetching cardiovascular / vascular metabolism trials...")

  results <- imap(CV_QUERIES, function(term, name) {
    df <- fetch_all_studies(term, max_pages = max_pages, verbose = verbose)
    if (nrow(df) > 0) df$query_source <- name
    df
  })

  combined <- bind_rows(results)

  if (nrow(combined) == 0) {
    warning("No studies retrieved. Check API connectivity.")
    return(tibble())
  }

  # Deduplicate — keep first occurrence per NCT ID
  deduped <- combined |>
    distinct(nct_id, .keep_all = TRUE) |>
    mutate(
      start_date_parsed      = parse_date_time(start_date, orders = c("Y-m-d", "Y-m", "Y")),
      completion_date_parsed = parse_date_time(completion_date, orders = c("Y-m-d", "Y-m", "Y")),
      primary_completion_parsed = parse_date_time(primary_completion, orders = c("Y-m-d", "Y-m", "Y"))
    )

  if (verbose) message("Total unique studies: ", nrow(deduped))
  deduped
}


# -----------------------------------------------------------------------------
# Optional: save / load cache (avoids hitting API repeatedly)
# -----------------------------------------------------------------------------

save_trials_cache <- function(df, path = "data/trials_cache.rds") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(df, path)
  message("Saved ", nrow(df), " studies to ", path)
}

load_trials_cache <- function(path = "data/trials_cache.rds") {
  if (!file.exists(path)) stop("Cache not found. Run fetch_cv_trials() first.")
  readRDS(path)
}


# -----------------------------------------------------------------------------
# Run standalone (source this file directly to populate cache)
# -----------------------------------------------------------------------------

if (!interactive()) {
  df <- fetch_cv_trials(max_pages = 5)
  save_trials_cache(df)
}
