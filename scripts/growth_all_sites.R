############################################################################
### FIGURES FOR H. LENIHAN'S CORAL DEMOGRAPHY DATASET: 2013-2025
### SCRIPT AND FIGURES BY DANIELLE TURNER
### AUGUST 19, 2026
############################################################################
############################################################################

# Load libraries
library(tidyverse)
library(scales)

# Read data
coral <- read_csv("data/coral_tidy_dyn_2013-2025.csv",
                  show_col_types = FALSE)

# Clean data and calculate colony volume
coral_clean <- coral %>%
  mutate(
    # Fix taxon typo where "Acr" is labelled as "A"
    taxa = if_else(taxa == "A", "Acr", taxa),
    
    # Convert dimensions to numeric.
    # Values such as "UK" will become NA.
    length = as.numeric(length),
    width  = as.numeric(width),
    height = as.numeric(height),
    
    # Colony volume in cubic cm
    size_cm3 = length * width * height
  )

# ------------------------------------------------------------
# Make year-to-year growth dataset
# ------------------------------------------------------------

growth_data <- coral_clean %>%
  # Only taxa being plotted
  filter(taxa %in% c("Acr", "Poc", "Por")) %>%
  
  # Arrange observations chronologically for each coral
  arrange(coral_number, year) %>%
  
  group_by(coral_number) %>%
  
  # Previous observation becomes "initial" size
  mutate(
    initial_size = lag(size_cm3),
    initial_year = lag(year),
    current_size = size_cm3
  ) %>%
  
  ungroup() %>%
  
  # Only compare consecutive years.
  # This prevents, for example, comparing 2018 directly with 2021
  # when intermediate observations are missing.
  filter(
    year == initial_year + 1,
    !is.na(initial_size),
    !is.na(current_size),
    initial_size > 0,
    current_size > 0
  )


# ------------------------------------------------------------
# Acropora
# ------------------------------------------------------------

p_acr <- growth_data %>%
  filter(taxa == "Acr") %>%
  ggplot(aes(x = initial_size, y = current_size)) +
  geom_point(alpha = 0.5) +
  
  # 1:1 line
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linewidth = 0.4
  ) +
  
  # Trend line
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  
  scale_x_log10(
    labels = scales::label_log()
  ) +
  scale_y_log10(
    labels = scales::label_log()
  ) +
  
  labs(
    title = "Acropora growth: 2013–2025",
    x = expression("Initial size (cm"^3*", log"[10]*")"),
    y = expression("Current size (cm"^3*", log"[10]*")")
  ) +
  
  theme_classic(base_size = 14)

p_acr


# ------------------------------------------------------------
# Pocillopora
# ------------------------------------------------------------

p_poc <- growth_data %>%
  filter(taxa == "Poc") %>%
  ggplot(aes(x = initial_size, y = current_size)) +
  geom_point(alpha = 0.5) +
  
  # 1:1 line
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linewidth = 0.4
  ) +
  
  # Trend line
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  
  scale_x_log10(
    labels = scales::label_log()
  ) +
  scale_y_log10(
    labels = scales::label_log()
  ) +
  
  labs(
    title = "Pocillopora growth: 2013–2025",
    x = expression("Initial size (cm"^3*", log"[10]*")"),
    y = expression("Current size (cm"^3*", log"[10]*")")
  ) +
  
  theme_classic(base_size = 14)

p_poc


# ------------------------------------------------------------
# Porites
# ------------------------------------------------------------

p_por <- growth_data %>%
  filter(taxa == "Por") %>%
  ggplot(aes(x = initial_size, y = current_size)) +
  geom_point(alpha = 0.5) +
  
  # 1:1 line
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linewidth = 0.4
  ) +
  
  # Trend line
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  
  scale_x_log10(
    labels = scales::label_log()
  ) +
  scale_y_log10(
    labels = scales::label_log()
  ) +
  
  labs(
    title = "Porites growth: 2013–2025",
    x = expression("Initial size (cm"^3*", log"[10]*")"),
    y = expression("Current size (cm"^3*", log"[10]*")")
  ) +
  
  theme_classic(base_size = 14)

p_por


# ------------------------------------------------------------
# Save growth figures
# ------------------------------------------------------------

ggsave(
  filename = file.path("figs", "growth_all_sites", "Acr_growth_all_sites.png"),
  plot = p_acr,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path("figs", "growth_all_sites", "Poc_growth_all_sites.png"),
  plot = p_poc,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path("figs", "growth_all_sites", "Por_growth_all_sites.png"),
  plot = p_por,
  width = 7,
  height = 6,
  dpi = 300
)
