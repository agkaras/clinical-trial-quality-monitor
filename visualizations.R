# =============================================================================
# visualizations.R
# Reusable ggplot2 plots — called both standalone and from Shiny
# =============================================================================

library(ggplot2)
library(dplyr)
library(forcats)
library(scales)

# Shared theme — clean, publication-adjacent
theme_cv_monitor <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 1),
      plot.subtitle    = element_text(color = "grey45", size = base_size - 1),
      axis.title       = element_text(color = "grey30"),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(size = base_size - 1),
      strip.text       = element_text(face = "bold")
    )
}

# Palette: cardiovascular / clinical feel (muted, accessible)
STATUS_COLORS <- c(
  "RECRUITING"           = "#2196F3",
  "COMPLETED"            = "#4CAF50",
  "ACTIVE_NOT_RECRUITING"= "#FF9800",
  "TERMINATED"           = "#F44336",
  "SUSPENDED"            = "#9C27B0",
  "WITHDRAWN"            = "#795548",
  "UNKNOWN"              = "#9E9E9E",
  "NOT_YET_RECRUITING"   = "#00BCD4"
)

RISK_COLORS <- c(
  "Clean"  = "#4CAF50",
  "Low"    = "#8BC34A",
  "Medium" = "#FF9800",
  "High"   = "#F44336"
)


# -----------------------------------------------------------------------------
# 1. Study status distribution — horizontal bar chart
# -----------------------------------------------------------------------------

plot_status_distribution <- function(df) {

  df |>
    count(status) |>
    mutate(status = fct_reorder(status, n)) |>
    ggplot(aes(x = n, y = status, fill = status)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(aes(label = n), hjust = -0.2, size = 3.5, color = "grey30") +
    scale_fill_manual(values = STATUS_COLORS, na.value = "grey70") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title    = "Study Status Distribution",
      subtitle = "Cardiovascular / vascular metabolism trials",
      x = "Number of studies", y = NULL
    ) +
    theme_cv_monitor()
}


# -----------------------------------------------------------------------------
# 2. Phase breakdown — pie / donut via ggplot
# -----------------------------------------------------------------------------

plot_phase_breakdown <- function(df) {

  df |>
    filter(!is.na(phase), phase != "N/A") |>
    count(phase) |>
    mutate(
      pct   = n / sum(n),
      label = paste0(phase, "\n", percent(pct, accuracy = 1))
    ) |>
    ggplot(aes(x = "", y = n, fill = phase)) +
    geom_col(width = 1, color = "white", linewidth = 0.5) +
    coord_polar("y") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Trials by Phase",
      fill  = "Phase",
      x = NULL, y = NULL
    ) +
    theme_cv_monitor() +
    theme(
      axis.text  = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    )
}


# -----------------------------------------------------------------------------
# 3. Enrollment distribution by phase — boxplot
# -----------------------------------------------------------------------------

plot_enrollment_by_phase <- function(df) {

  df |>
    filter(
      !is.na(enrollment_count),
      enrollment_count > 0,
      enrollment_count <= 5000,
      !is.na(phase),
      phase != "N/A"
    ) |>
    mutate(phase = fct_reorder(phase, enrollment_count, median)) |>
    ggplot(aes(x = phase, y = enrollment_count, fill = phase)) +
    geom_boxplot(outlier.alpha = 0.3, outlier.size = 1.5, width = 0.6) +
    scale_y_continuous(labels = comma) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title    = "Enrollment Count by Phase",
      subtitle = "Studies with enrollment ≤ 5,000 (outliers excluded from view)",
      x = "Phase", y = "Enrollment count",
      fill = "Phase"
    ) +
    theme_cv_monitor() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
    )
}


# -----------------------------------------------------------------------------
# 4. Study start timeline — area chart
# -----------------------------------------------------------------------------

plot_start_timeline <- function(df_timeline) {

  df_timeline |>
    ggplot(aes(x = start_year, y = n, fill = status, color = status)) +
    geom_area(alpha = 0.65, position = "stack") +
    scale_fill_manual(values  = STATUS_COLORS, na.value = "grey70") +
    scale_color_manual(values = STATUS_COLORS, na.value = "grey70") +
    scale_x_continuous(breaks = seq(2005, 2025, 5)) +
    scale_y_continuous(labels = comma) +
    labs(
      title    = "New Cardiovascular Trials per Year by Status",
      subtitle = "Studies starting 2005–present",
      x = "Year", y = "Number of studies",
      fill = "Status", color = "Status"
    ) +
    theme_cv_monitor()
}


# -----------------------------------------------------------------------------
# 5. Data quality flag summary — lollipop chart
# -----------------------------------------------------------------------------

plot_quality_flags <- function(summary_df) {

  summary_df |>
    mutate(label_text = fct_reorder(label, n_flagged)) |>
    ggplot(aes(x = n_flagged, y = label_text)) +
    geom_segment(
      aes(x = 0, xend = n_flagged, y = label_text, yend = label_text),
      color = "grey70", linewidth = 0.8
    ) +
    geom_point(aes(color = pct), size = 4) +
    geom_text(
      aes(label = paste0(n_flagged, "  (", pct, "%)")),
      hjust = -0.15, size = 3.3, color = "grey30"
    ) +
    scale_color_gradient(low = "#8BC34A", high = "#F44336",
                         name = "% of studies flagged") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(
      title    = "Data Quality Flags",
      subtitle = "Number of studies triggering each quality check",
      x = "Studies flagged", y = NULL
    ) +
    theme_cv_monitor()
}


# -----------------------------------------------------------------------------
# 6. Risk level distribution — stacked bar by sponsor class
# -----------------------------------------------------------------------------

plot_risk_by_sponsor <- function(df_qc) {

  df_qc |>
    filter(!is.na(sponsor_class)) |>
    count(sponsor_class, risk_level) |>
    group_by(sponsor_class) |>
    mutate(pct = n / sum(n)) |>
    ungroup() |>
    ggplot(aes(x = sponsor_class, y = pct, fill = risk_level)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = RISK_COLORS) +
    scale_y_continuous(labels = percent) +
    labs(
      title    = "Risk Profile by Sponsor Class",
      subtitle = "Proportion of studies at each risk level",
      x = "Sponsor class", y = "Proportion",
      fill = "Risk level"
    ) +
    theme_cv_monitor() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# -----------------------------------------------------------------------------
# 7. Sponsor top-10 — by number of studies
# -----------------------------------------------------------------------------

plot_top_sponsors <- function(df, n = 10) {

  df |>
    filter(!is.na(sponsor_name)) |>
    count(sponsor_name, sort = TRUE) |>
    slice_head(n = n) |>
    mutate(sponsor_name = fct_reorder(sponsor_name, n)) |>
    ggplot(aes(x = n, y = sponsor_name)) +
    geom_col(fill = "#1565C0", alpha = 0.8, width = 0.7) +
    geom_text(aes(label = n), hjust = -0.2, size = 3.5, color = "grey30") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = paste0("Top ", n, " Sponsors"),
      subtitle = "By number of cardiovascular studies",
      x = "Number of studies", y = NULL
    ) +
    theme_cv_monitor()
}

