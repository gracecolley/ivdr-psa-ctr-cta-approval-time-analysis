# =============================================================================
# Figures and tables
#
# This section generates the descriptive figures and tables reported in the
# manuscript from the analysis dataset prepared above. It does not refit the
# primary models.
#
# Figures and tables are exported as high-resolution PNG files and/or vector
# PDF files. Any visual caps or anonymisation used in a figure are for display
# only and do not alter the underlying analysis data.
#
# Dependencies: packages are loaded here for clarity. Package versions should
# be recorded for the publication release.
# =============================================================================

# Required packages -----------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(ggbeeswarm)
library(scales)
library(car)
library(tidyr)
library(purrr)
library(ggridges)
library(rstatix)
library(effsize)
library(grid)
library(tidyverse)
library(patchwork)


# 
# -----------------------------------------------------------------------------
# Figure 1. Country composition by submission type
#
# For each country, the plotted proportion is calculated within submission
# type. The paired points display the country composition of the CTA and PSA
# groups, and the adjacent table reports the percentage-point difference
# (PSA minus CTA).
# -----------------------------------------------------------------------------

country_mix <- approval_df |>
  count(country, submission_type, name = "n") |>
  complete(country, submission_type, fill = list(n = 0)) |>
  group_by(submission_type) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

# Determine the country order once
country_order <- country_mix |>
  group_by(country) |>
  summarise(order = mean(pct), .groups = "drop") |>
  arrange(order) |>
  pull(country)

# Create an explicit numeric row position.
# This is used by both the plot and the table to guarantee alignment.
country_positions <- tibble(
  country = country_order,
  row = seq_along(country_order)
)

country_mix <- country_mix |>
  mutate(country = as.character(country)) |>
  left_join(country_positions, by = "country")

#-------------------------------------------------------
# Prepare percentage-only table values
#-------------------------------------------------------

country_table <- country_mix |>
  select(country, submission_type, pct, row) |>
  pivot_wider(
    names_from = submission_type,
    values_from = pct,
    names_glue = "{submission_type}_{.value}"
  ) |>
  mutate(
    difference_pp = 100 * (PSA_pct - CTA_pct),
    CTA_label = sprintf("%.1f%%", 100 * CTA_pct),
    PSA_label = sprintf("%.1f%%", 100 * PSA_pct),
    difference_label = sprintf("%+.1f pp", difference_pp)
  )

n_countries <- length(country_order)

#-------------------------------------------------------
# Main country-composition plot
#-------------------------------------------------------

country_plot <- country_mix |>
  ggplot(aes(x = pct, y = row, colour = submission_type)) +
  geom_line(
    aes(group = country),
    colour = "grey75",
    linewidth = 0.5
  ) +
  geom_point(size = 3) +
  scale_x_continuous(
    labels = label_percent(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  scale_y_continuous(
    breaks = seq_len(n_countries),
    labels = country_order,
    limits = c(0.5, n_countries + 1.5),
    expand = c(0, 0)
  ) +
  scale_colour_manual(
    values = c(
      "CTA" = "#1c4f27",
      "PSA" = "#81144e"
    )
  ) +
  labs(
    x = "Proportion of submissions within submission type",
    y = NULL,
    colour = NULL,
    title = "Country composition is broadly similar for CTA and PSA submissions",
    subtitle = "Paired points show each country's contribution to the CTA and PSA populations"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(colour = "black"),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)
  )

#-------------------------------------------------------
# Aligned percentage-only table panel
#-------------------------------------------------------

table_panel <- country_table |>
  ggplot(aes(y = row)) +
  
  # CTA percentages
  geom_text(
    aes(x = 1, label = CTA_label),
    colour = "#1c4f27",
    hjust = 0.5,
    size = 3.5
  ) +
  
  # PSA percentages
  geom_text(
    aes(x = 2, label = PSA_label),
    colour = "#81144e",
    hjust = 0.5,
    size = 3.5
  ) +
  
  # Difference in percentage points
  geom_text(
    aes(x = 3, label = difference_label),
    colour = "black",
    hjust = 0.5,
    size = 3.5
  ) +
  
  # Headers placed above the highest country row
  annotate(
    "text",
    x = 1,
    y = n_countries + 1,
    label = "CTA %",
    fontface = "bold",
    hjust = 0.5,
    size = 3.5
  ) +
  annotate(
    "text",
    x = 2,
    y = n_countries + 1,
    label = "PSA %",
    fontface = "bold",
    hjust = 0.5,
    size = 3.5
  ) +
  annotate(
    "text",
    x = 3,
    y = n_countries + 1,
    label = "PSA-CTA (pp)",
    fontface = "bold",
    hjust = 0.5,
    size = 3.5
  ) +
  
  scale_x_continuous(
    limits = c(0.5, 3.5),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  
  # Exactly the same numeric y-scale as the country plot
  scale_y_continuous(
    breaks = seq_len(n_countries),
    limits = c(0.5, n_countries + 1.5),
    expand = c(0, 0)
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_void(base_size = 12) +
  theme(
    plot.margin = margin(5.5, 5.5, 5.5, 0)
  )

#-------------------------------------------------------
# Combine the plot and aligned table
#-------------------------------------------------------

combined_figure <- country_plot + table_panel +
  plot_layout(
    widths = c(1.25, 0.95)
  )

combined_figure

#-------------------------------------------------------
# Save the combined figure for publication
#-------------------------------------------------------

# Publication-quality PNG
ggsave(
  filename = "country_composition_CTA_vs_PSA.png",
  plot = combined_figure,
  width = 8.75,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white",
  type = "cairo-png"
)

# Vector PDF
ggsave(
  filename = "country_composition_CTA_vs_PSA.pdf",
  plot = combined_figure,
  width = 8.75,
  height = 7,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)


# -----------------------------------------------------------------------------
# Table 1. Submission-to-outcome time summaries
#
# Summaries are shown overall and by submission year and study phase.
# Percentiles and measures of spread are calculated from the analysis records.
# Counts above 150 and 180 days are descriptive threshold summaries.
# -----------------------------------------------------------------------------

# ------------------------------------------------------------
# 1. Prepare the analysis dataset
# ------------------------------------------------------------

approval_clean <- approval_df |>
  transmute(
    submission_type = as.character(submission_type),
    year = as.character(year),
    phase = as.character(phase),
    study_code = as.character(study_code),
    country = as.character(country),
    approval_time_days = suppressWarnings(as.numeric(approval_time_days))
  ) |>
  filter(
    !is.na(submission_type),
    !is.na(year),
    !is.na(phase),
    !is.na(study_code),
    !is.na(country),
    !is.na(approval_time_days),
    approval_time_days >= 0
  )

# ------------------------------------------------------------
# 2. Define the phase order
# ------------------------------------------------------------

phase_order <- c(
  "Phase I",
  "Phase Ib-II",
  "Phase I-II",
  "Phase I-IIa",
  "Phase II",
  "Phase IIb",
  "Phase III",
  "Phase IIIb",
  "Phase IV"
)

additional_phases <- approval_clean |>
  distinct(phase) |>
  filter(!phase %in% phase_order) |>
  arrange(phase) |>
  pull(phase)

phase_levels <- c(phase_order, additional_phases)

# ------------------------------------------------------------
# 3. Summary function
# ------------------------------------------------------------

summarise_approval_time_days <- function(data, ...) {
  data |>
    group_by(...) |>
    summarise(
      Observations = n(),
      Studies = n_distinct(study_code),
      Countries = n_distinct(country),
      mean_days = mean(approval_time_days),
      sd_days = sd(approval_time_days),
      median_days = median(approval_time_days),
      q1_days = unname(
        quantile(
          approval_time_days,
          probs = 0.25,
          names = FALSE
        )
      ),
      q3_days = unname(
        quantile(
          approval_time_days,
          probs = 0.75,
          names = FALSE
        )
      ),
      p90_days = unname(
        quantile(
          approval_time_days,
          probs = 0.90,
          names = FALSE
        )
      ),
      p95_days = unname(
        quantile(
          approval_time_days,
          probs = 0.95,
          names = FALSE
        )
      ),
      iqr_days = IQR(approval_time_days),
      mad_days = mad(approval_time_days, constant = 1),
      n_over_150 = sum(approval_time_days > 150),
      pct_over_150 = mean(approval_time_days > 150),
      n_over_180 = sum(approval_time_days > 180),
      pct_over_180 = mean(approval_time_days > 180),
      .groups = "drop"
    )
}

# ------------------------------------------------------------
# 4. Create the table sections
# ------------------------------------------------------------

overall_data <- approval_clean |>
  summarise_approval_time_days(submission_type) |>
  mutate(
    section = "Overall",
    subgroup = "Overall",
    section_order = 1L,
    subgroup_order = 1
  )

year_data <- approval_clean |>
  mutate(
    year_numeric = suppressWarnings(as.numeric(year))
  ) |>
  summarise_approval_time_days(
    submission_type,
    year,
    year_numeric
  ) |>
  mutate(
    section = "By year",
    subgroup = paste0(year),
    section_order = 2L,
    subgroup_order = coalesce(year_numeric, Inf)
  ) |>
  select(-year_numeric)

phase_data <- approval_clean |>
  mutate(
    phase = factor(phase, levels = phase_levels)
  ) |>
  summarise_approval_time_days(
    submission_type,
    phase
  ) |>
  mutate(
    section = "By phase",
    subgroup = as.character(phase),
    section_order = 3L,
    subgroup_order = match(subgroup, phase_levels)
  ) |>
  select(-phase)

# ------------------------------------------------------------
# 5. Combine and order all sections
# ------------------------------------------------------------

table_data <- bind_rows(
  overall_data,
  year_data,
  phase_data
) |>
  mutate(
    section = factor(
      section,
      levels = c("Overall", "By year", "By phase")
    ),
    submission_type = factor(
      submission_type,
      levels = c("CTA", "PSA")
    )
  ) |>
  arrange(
    section_order,
    subgroup_order,
    submission_type
  )

# ------------------------------------------------------------
# 6. Format publication display columns
# ------------------------------------------------------------

table_data_formatted <- table_data |>
  mutate(
    `Mean days ± SD` = if_else(
      is.na(sd_days),
      number(mean_days, accuracy = 0.1),
      paste0(
        number(mean_days, accuracy = 0.1),
        " ± ",
        number(sd_days, accuracy = 0.1)
      )
    ),
    Median = number(
      median_days,
      accuracy = 0.1
    ),
    `IQR (Q1–Q3)` = paste0(
      number(iqr_days, accuracy = 0.1),
      " (",
      number(q1_days, accuracy = 0.1),
      "–",
      number(q3_days, accuracy = 0.1),
      ")"
    ),
    `90th percentile` = number(
      p90_days,
      accuracy = 0.1
    ),
    `95th percentile` = number(
      p95_days,
      accuracy = 0.1
    ),
    MAD = number(
      mad_days,
      accuracy = 0.1
    ),
    `% exceeding 150 days (n)` = paste0(
      percent(pct_over_150, accuracy = 0.1),
      " (",
      comma(n_over_150),
      ")"
    ),
    `% exceeding 180 days (n)` = paste0(
      percent(pct_over_180, accuracy = 0.1),
      " (",
      comma(n_over_180),
      ")"
    ),
    `Submission type` = as.character(submission_type)
  ) |>
  select(
    section,
    subgroup,
    `Submission type`,
    Observations,
    Studies,
    Countries,
    `Mean days ± SD`,
    Median,
    `IQR (Q1–Q3)`,
    `90th percentile`,
    `95th percentile`,
    MAD,
    `% exceeding 150 days (n)`,
    `% exceeding 180 days (n)`
  )

# ------------------------------------------------------------
# 7. Create the publication-quality table
#    with CTA and PSA colour coding
# ------------------------------------------------------------

# Define the AZ-inspired submission-type colours
cta_colour <- "#1C4F27"
psa_colour <- "#81144E"

main_table <- table_data_formatted |>
  gt(
    rowname_col = "subgroup",
    groupname_col = "section"
  ) |>
  tab_header(
    title = md(
      "**Regulatory Approval Timelines by Submission Type, Year, and Phase**"
    ),
    subtitle = "Eligible submissions from 2023–2025"
  ) |>
  cols_label(
    subgroup = "Subgroup",
    `Submission type` = "Submission type",
    Observations = "Observations",
    Studies = "Studies",
    Countries = "Countries",
    `Mean days ± SD` = "Mean ± SD (d)",
    Median = "Median (d)",
    `IQR (Q1–Q3)` = "IQR (Q1–Q3) (d)",
    `90th percentile` = "P90 (d)",
    `95th percentile` = "P95 (d)",
    MAD = "MAD (d)",
    `% exceeding 150 days (n)` =
      "% >150d (n)",
    `% exceeding 180 days (n)` =
      "% >180d (n)"
  ) |>
  fmt_number(
    columns = c(
      Observations,
      Studies,
      Countries
    ),
    decimals = 0,
    use_seps = TRUE
  ) |>
  
  # Bold section headings
  tab_style(
    style = cell_text(
      weight = "bold",
      color = "#000000"
    ),
    locations = cells_row_groups()
  ) |>
  
  # Bold subgroup labels
  tab_style(
    style = cell_text(
      weight = "bold",
      color = "#000000"
    ),
    locations = cells_stub(rows = everything())
  ) |>
  
  # CTA colour
  tab_style(
    style = cell_text(
      weight = "bold",
      color = cta_colour
    ),
    locations = cells_body(
      columns = `Submission type`,
      rows = `Submission type` == "CTA"
    )
  ) |>
  
  # PSA colour
  tab_style(
    style = cell_text(
      weight = "bold",
      color = psa_colour
    ),
    locations = cells_body(
      columns = `Submission type`,
      rows = `Submission type` == "PSA"
    )
  ) |>
  
  # Subtle background shading for CTA rows
  tab_style(
    style = cell_fill(
      color = "#F1F7F2"
    ),
    locations = cells_body(
      columns = everything(),
      rows = `Submission type` == "CTA"
    )
  ) |>
  
  # Subtle background shading for PSA rows
  tab_style(
    style = cell_fill(
      color = "#FAF0F6"
    ),
    locations = cells_body(
      columns = everything(),
      rows = `Submission type` == "PSA"
    )
  ) |>
  
  # Centre-align the numerical and submission-type columns
  cols_align(
    align = "center",
    columns = c(
      `Submission type`,
      Observations,
      Studies,
      Countries,
      `Mean days ± SD`,
      Median,
      `IQR (Q1–Q3)`,
      `90th percentile`,
      `95th percentile`,
      MAD,
      `% exceeding 150 days (n)`,
      `% exceeding 180 days (n)`
    )
  ) |>
  
  # Left-align the subgroup/stub column
  cols_align(
    align = "left",
    columns = subgroup
  ) |>
  
  # Use alternating row shading only for rows without pathway colour
  opt_table_lines(extent = "none") |>
  
  # Table-level styling
  tab_options(
    table.font.names = "Arial",
    table.font.size = px(9),
    
    data_row.padding = px(4),
    
    heading.align = "left",
    heading.title.font.size = px(15),
    heading.subtitle.font.size = px(10),
    heading.title.font.weight = "bold",
    
    row_group.background.color = "#E6E6E6",
    row_group.font.weight = "bold",
    row_group.font.size = px(10),
    row_group.border.top.width = px(1),
    row_group.border.bottom.width = px(1),
    
    column_labels.background.color = "#F2F2F2",
    column_labels.font.weight = "bold",
    column_labels.font.size = px(9),
    column_labels.border.top.width = px(1),
    column_labels.border.bottom.width = px(1),
    
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    
    stub.border.color = "#BDBDBD",
    stub.border.width = px(1)
  ) |>
  
  # Add a coloured bottom border after each section
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "#808080",
      weight = px(1)
    ),
    locations = cells_row_groups()
  )

# Display the table
main_table


# ============================================================
# 8. Export the table as high-quality PDF and PNG
# ============================================================

# Install these packages once if they are not already installed:
# install.packages(c("webshot2", "chromote"))

library(webshot2)

# Create an output folder
output_dir <- "table_exports"

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# File paths
pdf_file <- file.path(
  output_dir,
  "regulatory_approval_timelines_table.pdf"
)

png_file <- file.path(
  output_dir,
  "regulatory_approval_timelines_table.png"
)

# Export to PDF
gtsave(
  data = main_table,
  filename = pdf_file,
  vwidth = 2400,
  vheight = 1500,
  expand = 10
)

# Export to high-resolution PNG
gtsave(
  data = main_table,
  filename = png_file,
  vwidth = 2400,
  vheight = 1500,
  zoom = 3,
  expand = 10
)


# -----------------------------------------------------------------------------
# Figure 2. Submission-to-outcome time distributions
#
# Violin plots display the distribution, boxplots display the median and
# interquartile range, and points display individual records. Horizontal lines
# mark the prespecified time thresholds. Threshold counts and percentages are
# calculated from the uncapped outcome values.
# -----------------------------------------------------------------------------

# Prespecified thresholds
thresholds <- c(150, 180)

# Colours
palette_study <- c(
  "CTA" = "#1c4f27",
  "PSA" = "#81144e"
)

# Numeric x positions
# Bringing the violin/boxplots slightly closer together
x_positions <- c(
  PSA = 1,
  CTA = 2
)

# Midpoint between the two violin/boxplot centres
plot_midpoint <- mean(x_positions)

# Symmetric x-axis limits around the midpoint
x_half_width <- 1.45

x_limits <- c(
  plot_midpoint - x_half_width,
  plot_midpoint + x_half_width
)

# Prepare plotting data
df_plot <- df %>%
  filter(
    !is.na(approval_time_days),
    !is.na(submission_type)
  ) %>%
  mutate(
    submission_type = factor(
      submission_type,
      levels = names(x_positions)
    ),
    x_num = unname(
      x_positions[as.character(submission_type)]
    )
  )

# Plotting range
y_min <- min(df_plot$approval_time_days)
y_max <- max(df_plot$approval_time_days)
y_range <- y_max - y_min

if (y_range == 0) {
  y_range <- 1
}

# Summary statistics by submission type
summary_lab <- df_plot %>%
  group_by(submission_type, x_num) %>%
  summarise(
    Q1 = quantile(approval_time_days, 0.25, names = FALSE),
    Median = median(approval_time_days),
    Q3 = quantile(approval_time_days, 0.75, names = FALSE),
    n = n(),
    max_y = max(approval_time_days),
    .groups = "drop"
  )

# Quantile labels
# PSA labels are placed on the left and CTA labels on the right
quantile_lab <- summary_lab %>%
  select(submission_type, x_num, Q1, Median, Q3) %>%
  pivot_longer(
    cols = c(Q1, Median, Q3),
    names_to = "statistic",
    values_to = "value"
  ) %>%
  mutate(
    label = paste0(
      statistic,
      " = ",
      comma(value, accuracy = 1),
      " days"
    ),
    
    x_label = if_else(
      submission_type == "PSA",
      x_num - 0.43,
      x_num + 0.43
    ),
    
    x_segment_start = if_else(
      submission_type == "PSA",
      x_num - 0.12,
      x_num + 0.12
    ),
    
    x_segment_end = if_else(
      submission_type == "PSA",
      x_label + 0.04,
      x_label - 0.04
    ),
    
    # Stagger labels vertically to avoid overlap
    label_y = case_when(
      statistic == "Q1" ~ value - 0.055 * y_range,
      statistic == "Median" ~ value,
      statistic == "Q3" ~ value + 0.055 * y_range
    ),
    
    # PSA labels are right-justified; CTA labels are left-justified
    label_hjust = if_else(
      submission_type == "PSA",
      1,
      0
    )
  )

# Threshold summary statistics
threshold_summary <- tidyr::crossing(
  summary_lab %>%
    select(submission_type, x_num, n, max_y),
  threshold = thresholds
) %>%
  mutate(
    n_exceed = df_plot$approval_time_days[
      match(submission_type, df_plot$submission_type)
    ]
  )

# Calculate threshold exceedances by submission type
threshold_lab <- df_plot %>%
  group_by(submission_type, x_num) %>%
  summarise(
    n = n(),
    max_y = max(approval_time_days),
    .groups = "drop"
  ) %>%
  tidyr::crossing(
    tibble(
      threshold = thresholds,
      threshold_order = seq_along(thresholds)
    )
  ) %>%
  rowwise() %>%
  mutate(
    n_exceed = sum(
      df_plot$approval_time_days[
        df_plot$submission_type == submission_type
      ] > threshold
    ),
    pct_exceed = 100 * n_exceed / n
  ) %>%
  ungroup() %>%
  mutate(
    label = paste0(
      "> ",
      comma(threshold, accuracy = 1),
      " days: ",
      sprintf("%.1f%%", pct_exceed),
      " (",
      n_exceed,
      "/",
      n,
      ")"
    ),
    
    # Stack the three exceedance labels above each violin
    y_label = y_max +
      (0.04 + 0.065 * (threshold_order - 1)) * y_range
  )

# Sample-size labels
n_lab <- summary_lab %>%
  transmute(
    submission_type,
    x_num,
    label = paste0("n = ", n),
    y_label = y_min - 0.10 * y_range
  )

# Labels for the three threshold lines
threshold_line_labels <- tibble(
  threshold = thresholds,
  label = paste0(threshold, " days")
)

# Theme
base_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(
      margin = margin(t = 12)
    ),
    axis.title.y = element_text(
      margin = margin(r = 10)
    ),
    axis.text = element_text(color = "black"),
    
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0
    ),
    
    legend.position = "none",
    
    # Equal margins preserve the centred appearance
    plot.margin = margin(
      t = 10,
      r = 20,
      b = 20,
      l = 20
    )
  )

# Plot
p <- ggplot(
  df_plot,
  aes(
    x = x_num,
    y = approval_time_days,
    group = submission_type,
    fill = submission_type,
    color = submission_type
  )
) +
  
  # Three horizontal threshold lines
  geom_hline(
    data = threshold_line_labels,
    aes(yintercept = threshold),
    linetype = "dashed",
    linewidth = 0.65,
    color = "grey45"
  ) +
  
  # Threshold labels placed centrally between the two distributions
  geom_label(
    data = threshold_line_labels,
    aes(
      x = plot_midpoint,
      y = threshold,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 0.5,
    vjust = -0.55,
    size = 3.0,
    color = "grey25",
    fill = "white",
    label.size = 0.2,
    label.padding = unit(0.15, "lines"),
    fontface = "italic"
  ) +
  
  # Violin distributions
  geom_violin(
    trim = TRUE,
    width = 0.9,
    alpha = 0.25,
    linewidth = 0.6
  ) +
  
  # Boxplots
  geom_boxplot(
    width = 0.18,
    outlier.shape = NA,
    alpha = 0.9,
    color = "black"
  ) +
  
  # Individual observations
  geom_quasirandom(
    width = 0.18,
    size = 1.3,
    alpha = 0.4,
    shape = 16,
    color = "grey25"
  ) +
  
  # Connector lines from actual quantiles to labels
  geom_segment(
    data = quantile_lab,
    aes(
      x = x_segment_start,
      xend = x_segment_end,
      y = value,
      yend = label_y,
      color = submission_type
    ),
    inherit.aes = FALSE,
    linewidth = 0.4
  ) +
  
  # Q1, median, and Q3 labels
  geom_label(
    data = quantile_lab,
    aes(
      x = x_label,
      y = label_y,
      label = label,
      color = submission_type,
      hjust = label_hjust
    ),
    inherit.aes = FALSE,
    size = 3.1,
    label.size = 0.2,
    label.padding = unit(0.18, "lines"),
    fill = "white",
    show.legend = FALSE
  ) +
  
  # Threshold exceedance labels above each violin
  geom_label(
    data = threshold_lab,
    aes(
      x = x_num,
      y = y_label,
      label = label,
      color = submission_type
    ),
    inherit.aes = FALSE,
    hjust = 0.5,
    vjust = 0,
    fontface = "bold",
    size = 3.2,
    label.size = 0.2,
    label.padding = unit(0.18, "lines"),
    fill = "white",
    show.legend = FALSE
  ) +
  
  # Sample-size labels
  geom_text(
    data = n_lab,
    aes(
      x = x_num,
      y = y_label,
      label = label,
      color = submission_type
    ),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 3.6,
    vjust = 1,
    show.legend = FALSE
  ) +
  
  # Manual colours
  scale_fill_manual(values = palette_study) +
  scale_color_manual(values = palette_study) +
  
  # Symmetric x-axis centred on the two distributions
  scale_x_continuous(
    breaks = unname(x_positions),
    labels = names(x_positions),
    limits = x_limits,
    expand = c(0, 0)
  ) +
  
  # Extra space above and below for annotations
  scale_y_continuous(
    labels = label_comma(accuracy = 1),
    limits = c(
      y_min - 0.18 * y_range,
      y_max + 0.28 * y_range
    ),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Approval Time by Submission Type",
    subtitle = paste0(
      "PSA shows broader central dispersion, while CTA is more concentrated;\n",
      "Threshold exceedance is shown for > ",
      paste(thresholds, collapse = ", "),
      " days"
    ),
    x = "Submission Type",
    y = "Time-to-Approval (days)"
  ) +
  
  base_theme

# Display plot
print(p)

# Save PNG
ggsave(
  filename = "approval_time_days_violin_box_points_two_thresholds.png",
  plot = p,
  width = 8.75,
  height = 8,
  dpi = 320,
  bg = "white"
)

# Save PDF
ggsave(
  filename = "approval_time_days_violin_box_points_two_thresholds.pdf",
  plot = p,
  width = 8.75,
  height = 8,
  device = cairo_pdf,
  bg = "white"
)

# -----------------------------------------------------------------------------
# Figure 3. Submission-to-outcome time by year
#
# Points show individual observations. The central summary marks the median and
# interquartile range; whisker-style bars show the 10th–90th percentiles.
# Values above the display cap are represented at the cap for plotting only.
# -----------------------------------------------------------------------------
#--------------------------------------------------
# Final complete analysis cohort
#--------------------------------------------------

df_f <- approval_df |>
  mutate(
    submission_type = factor(
      submission_type,
      levels = c("PSA", "CTA")
    ),
    year = factor(
      year,
      levels = c("2023", "2024", "2025")
    )
  )

#--------------------------------------------------
# Plot settings
#--------------------------------------------------

# Display cap only; all observations remain included
# in the statistical analyses.
plot_cap <- 250

# Space above the capped plot for three-line labels
label_y <- plot_cap + 12
plot_top <- plot_cap + 48

# Cap observations for visualisation only
df_plot <- df_f |>
  mutate(
    approval_time_days_plot = pmin(
      approval_time_days,
      plot_cap
    ),
    above_cap = approval_time_days > plot_cap
  )

#--------------------------------------------------
# Year-specific summaries and denominators
#--------------------------------------------------

yr_stats <- df_f |>
  group_by(year, submission_type) |>
  summarise(
    n_records = n(),
    n_study_codes = n_distinct(study_code),
    n_countries = n_distinct(country),
    
    p10 = quantile(
      approval_time_days,
      0.10,
      na.rm = TRUE,
      names = FALSE
    ),
    
    q1 = quantile(
      approval_time_days,
      0.25,
      na.rm = TRUE,
      names = FALSE
    ),
    
    med = median(
      approval_time_days,
      na.rm = TRUE
    ),
    
    q3 = quantile(
      approval_time_days,
      0.75,
      na.rm = TRUE,
      names = FALSE
    ),
    
    p90 = quantile(
      approval_time_days,
      0.90,
      na.rm = TRUE,
      names = FALSE
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    # Cap summary statistics for display only
    p10_plot = pmin(p10, plot_cap),
    q1_plot = pmin(q1, plot_cap),
    med_plot = pmin(med, plot_cap),
    q3_plot = pmin(q3, plot_cap),
    p90_plot = pmin(p90, plot_cap),
    
    # Three-line labels
    group_label = paste0(
      "n = ", n_records,
      "\nstudies = ", n_study_codes,
      "\ncountries = ", n_countries
    )
  )

print(yr_stats)

#--------------------------------------------------
# Country composition
#--------------------------------------------------

country_year_counts <- df_f |>
  count(
    submission_type,
    year,
    country,
    name = "n_records"
  ) |>
  arrange(
    submission_type,
    year,
    country
  )

print(country_year_counts)

#--------------------------------------------------
# Colours and positioning
#--------------------------------------------------

palette_study <- c(
  PSA = "#81144e",
  CTA = "#1c4f27"
)

# Increased horizontal separation between PSA and CTA
dodge <- position_dodge(
  width = 0.8
)

#--------------------------------------------------
# Theme matching Figure 2
#--------------------------------------------------

year_plot_theme <- theme_minimal(
  base_size = 12
) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.grid.major.y = element_line(
      colour = "grey88",
      linewidth = 0.35
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.title.x = element_text(
      margin = margin(t = 12)
    ),
    
    axis.title.y = element_text(
      margin = margin(r = 10)
    ),
    
    axis.text = element_text(
      colour = "black"
    ),
    
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.3
    ),
    
    # Same title settings as Figure 2
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0
    ),
    
    # Same subtitle settings as Figure 2
    plot.subtitle = element_text(
      size = 11,
      hjust = 0
    ),
    
    legend.position = "top",
    legend.title = element_text(
      face = "bold"
    ),
    
    # Compact legend spacing
    legend.key.width = unit(
      0.8,
      "cm"
    ),
    
    legend.key.height = unit(
      0.35,
      "cm"
    ),
    
    legend.spacing.y = unit(
      0.05,
      "cm"
    ),
    
    legend.box.spacing = unit(
      0.05,
      "cm"
    ),
    
    legend.margin = margin(
      0,
      0,
      0,
      0
    ),
    
    # Same plot margins as Figure 2
    plot.margin = margin(
      t = 10,
      r = 20,
      b = 20,
      l = 20
    )
  )

#--------------------------------------------------
# Figure 3
#--------------------------------------------------

p_disp <- ggplot(
  yr_stats,
  aes(
    x = year,
    group = submission_type,
    colour = submission_type,
    fill = submission_type
  )
) +
  
  # Display-cap line
  geom_hline(
    yintercept = plot_cap,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey55"
  ) +
  
  # 10th–90th percentile range
  geom_errorbar(
    aes(
      ymin = p10_plot,
      ymax = p90_plot
    ),
    position = dodge,
    width = 0.10,
    linewidth = 0.65,
    colour = "grey35"
  ) +
  
  # IQR and median
  geom_crossbar(
    aes(
      y = med_plot,
      ymin = q1_plot,
      ymax = q3_plot
    ),
    position = dodge,
    width = 0.42,
    alpha = 0.35,
    linewidth = 0.55,
    middle.linewidth = 0.8
  ) +
  
  # Individual observations
  geom_point(
    data = df_plot,
    aes(
      x = year,
      y = approval_time_days_plot,
      group = submission_type,
      shape = above_cap
    ),
    position = position_jitterdodge(
      jitter.width = 0.07,
      dodge.width = 0.8
    ),
    inherit.aes = FALSE,
    colour = "grey30",
    alpha = 0.45,
    size = 1.3
  ) +
  
  # Three-line denominators and composition labels,
  # coloured according to submission type
  geom_text(
    data = yr_stats,
    aes(
      x = year,
      y = label_y,
      label = group_label,
      group = submission_type,
      colour = submission_type
    ),
    position = dodge,
    inherit.aes = FALSE,
    size = 3.5,
    lineheight = 0.85,
    hjust = 0.5,
    vjust = 0,
    show.legend = FALSE
  ) +
  
  scale_fill_manual(
    values = palette_study,
    breaks = c("PSA", "CTA"),
    name = "Submission type"
  ) +
  
  scale_colour_manual(
    values = palette_study,
    breaks = c("PSA", "CTA"),
    name = "Submission type"
  ) +
  
  scale_shape_manual(
    values = c(
      `FALSE` = 16,
      `TRUE` = 24
    ),
    labels = c(
      `FALSE` = "≤250 days",
      `TRUE` = ">250 days"
    ),
    name = "Displayed value"
  ) +
  
  scale_y_continuous(
    breaks = seq(
      0,
      plot_cap,
      by = 50
    ),
    labels = label_number(
      accuracy = 1
    ),
    limits = c(
      0,
      plot_top
    ),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Approval Time by Submission Type and Year",
    subtitle = paste(
      "Central bars show the median and IQR;",
      "error bars show the 10th–90th percentiles."
    ),
    x = "Submission Year",
    y = "Time-to-Approval (days)"
  ) +
  
  year_plot_theme

# Display figure
print(p_disp)

#--------------------------------------------------
# High-resolution exports
#--------------------------------------------------

ggsave(
  filename = "year_on_year_variability_250day_cap.png",
  plot = p_disp,
  width = 8.75,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "year_on_year_variability_250day_cap.pdf",
  plot = p_disp,
  width = 8.75,
  height = 8,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)


# -----------------------------------------------------------------------------
# Figure 4. Submission-to-outcome time by anonymized country
#
# Countries are anonymized and randomly ordered. Displayed outcome values are
# capped at the overall 95th percentile for visualisation only; the source
# values are retained for the statistical analyses.
# -----------------------------------------------------------------------------

# Retain countries with at least five observations in both groups
country_keep <- approval_df |>
  filter(submission_type %in% c("PSA", "CTA")) |>
  count(country, submission_type, name = "n") |>
  pivot_wider(
    names_from = submission_type,
    values_from = n,
    values_fill = 0
  ) |>
  filter(PSA >= 5, CTA >= 5) |>
  pull(country)

df_plot <- approval_df |>
  filter(
    country %in% country_keep,
    submission_type %in% c("PSA", "CTA")
  ) |>
  mutate(
    submission_type = factor(
      trimws(as.character(submission_type)),
      levels = c("PSA", "CTA")
    )
  )

if (nrow(df_plot) == 0) {
  stop("No countries have at least five PSA and five CTA observations.")
}

# Randomise country order and assign anonymous labels
set.seed(20260903)

country_ids <- sort(unique(as.character(df_plot$country)))

country_map <- tibble(
  country = sample(country_ids),
  country_anon = LETTERS[seq_along(country_ids)]
)

df_plot <- df_plot |>
  left_join(country_map, by = "country") |>
  mutate(
    country_anon = factor(
      country_anon,
      levels = rev(country_map$country_anon)
    )
  )

# Cap displayed approval times at the overall 95th percentile
display_cap <- quantile(
  df_plot$approval_time_days,
  probs = 0.95,
  na.rm = TRUE,
  names = FALSE
)

df_plot <- df_plot |>
  mutate(
    approval_time_days_plot = pmin(
      approval_time_days,
      display_cap
    ),
    # Fixed rows: PSA above and CTA below each country
    y_num = as.numeric(country_anon) +
      if_else(submission_type == "PSA", 0.18, -0.18)
  )

# Count observations by anonymous country and submission type
country_labels <- df_plot |>
  count(country_anon, submission_type, name = "n") |>
  mutate(
    y_num = as.numeric(country_anon) +
      if_else(submission_type == "PSA", 0.18, -0.18),
    label = paste0("n = ", n)
  )

# Use one shared x-position so all labels align
label_x <- display_cap * 1.02

palette_study <- c(
  PSA = "#81144e",
  CTA = "#1c4f27"
)

p_country <- ggplot(
  df_plot,
  aes(
    x = approval_time_days_plot,
    y = y_num,
    fill = submission_type,
    colour = submission_type
  )
) +
  geom_boxplot(
    aes(group = interaction(country_anon, submission_type)),
    width = 0.28,
    outlier.shape = NA,
    alpha = 0.35,
    linewidth = 0.45,
    colour = "black"
  ) +
  geom_quasirandom(
    aes(group = submission_type),
    width = 0.16,
    size = 1.25,
    alpha = 0.45,
    shape = 16,
    colour = "grey25",
    orientation = "x"
  )  +
  geom_text(
    data = country_labels,
    aes(
      x = label_x,
      y = y_num,
      label = label,
      colour = submission_type
    ),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.3,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = palette_study,
    breaks = c("PSA", "CTA"),
    name = "Submission type"
  ) +
  scale_colour_manual(
    values = palette_study,
    breaks = c("PSA", "CTA"),
    name = "Submission type"
  ) +
  scale_x_continuous(
    limits = c(NA, display_cap * 1.20),
    labels = label_comma(accuracy = 1),
    expand = expansion(mult = c(0.02, 0))
  ) +
  scale_y_continuous(
    breaks = seq_along(levels(df_plot$country_anon)),
    labels = levels(df_plot$country_anon)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Approval Time by Submission Type and Country",
    subtitle = paste(
      "Countries are anonymized and randomly ordered;",
      "boxes show the median and IQR."
    ),
    x = "Time-to-Approval (days; values capped at the overall 95th percentile)",
    y = "Anonymized country"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = "grey88",
      linewidth = 0.35
    ),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(colour = "black"),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, colour = "grey25"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(t = 10, r = 15, b = 20, l = 20)
  )

print(p_country)

# Save high-resolution PNG and PDF
ggsave(
  filename = "country_approval_times_anonymised.png",
  plot = p_country,
  width = 9.5,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "country_approval_times_anonymised.pdf",
  plot = p_country,
  width = 9.5,
  height = 7,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)


# -----------------------------------------------------------------------------
# Export inventory
# -----------------------------------------------------------------------------

figure_table_files <- c(
  "country_composition_CTA_vs_PSA.png",
  "country_composition_CTA_vs_PSA.pdf",
  "table_exports/regulatory_approval_timelines_table.pdf",
  "table_exports/regulatory_approval_timelines_table.png",
  "approval_time_days_violin_box_points_two_thresholds.png",
  "approval_time_days_violin_box_points_two_thresholds.pdf",
  "year_on_year_variability_250day_cap.png",
  "year_on_year_variability_250day_cap.pdf",
  "country_approval_times_anonymised.png",
  "country_approval_times_anonymised.pdf"
)

figure_table_files