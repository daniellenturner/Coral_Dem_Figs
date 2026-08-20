############################################################################
### SURVIVAL AT ALL SITES 
### FIGURES FOR H. LENIHAN'S CORAL DEMOGRAPHY DATASET: 2013-2025
### SCRIPT AND FIGURES BY DANIELLE TURNER
### AUGUST 20, 2026
############################################################################
############################################################################

# ------------------------------------------------------------
# Load libraries
# ------------------------------------------------------------
library(tidyverse)
library(scales)

# ------------------------------------------------------------
# Read and clean data
# ------------------------------------------------------------

coral <- read_csv(
  "data/coral_tidy_dyn_2013-2025.csv",
  show_col_types = FALSE
)

coral_clean <- coral %>%
  mutate(
    # Fix taxon typo
    taxa = if_else(taxa == "A", "Acr", taxa),
    
    # Convert dimensions to numeric
    # D, UK, etc. become NA
    length = as.numeric(length),
    width  = as.numeric(width),
    height = as.numeric(height),
    
    # Calculate colony volume
    size_cm3 = length * width * height
  ) %>%
  
  # Keep focal taxa
  filter(taxa %in% c("Acr", "Poc", "Por"))


# ------------------------------------------------------------
# Initial size = first valid size ever measured
# ------------------------------------------------------------

initial_survival <- coral_clean %>%
  filter(
    !is.na(size_cm3),
    size_cm3 > 0
  ) %>%
  
  arrange(
    coral_number,
    year
  ) %>%
  
  group_by(coral_number) %>%
  
  # First valid size measurement
  slice_head(n = 1) %>%
  
  ungroup() %>%
  
  select(
    coral_number,
    taxa,
    site,
    habitat,
    initial_year = year,
    initial_size = size_cm3
  )


# ------------------------------------------------------------
# Determine eventual survival/death status
# ------------------------------------------------------------

survival_status <- coral_clean %>%
  group_by(coral_number) %>%
  
  summarise(
    # Did this coral ever have a recorded death?
    ever_died = any(
      dyn_death == 1,
      na.rm = TRUE
    ),
    
    # Last year the coral appears in the dataset
    last_year = max(
      year,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    # Logistic-regression response:
    # 0 = dead
    # 1 = survived
    survived = if_else(
      ever_died,
      0L,
      1L
    )
  )


# ------------------------------------------------------------
# Final survival dataset
# ------------------------------------------------------------

survival_data <- initial_survival %>%
  
  inner_join(
    survival_status,
    by = "coral_number"
  ) %>%
  
  filter(
    !is.na(initial_size),
    initial_size > 0
  )


# ------------------------------------------------------------
# Make Acropora figure
# ------------------------------------------------------------
p_acr_survival <- survival_data %>%
  filter(taxa == "Acr") %>%
  
  ggplot(
    aes(
      x = initial_size,
      y = survived
    )
  ) +
  
  # Raw binary observations: 0 = dead, 1 = alive
  geom_point(
    alpha = 0.3,
    size = 2
  ) +
  
  # Binomial logistic regression
  geom_smooth(
    method = "glm",
    method.args = list(
      family = binomial(link = "logit")
    ),
    se = TRUE,
    linewidth = 1.2
  ) +
  
  scale_x_log10(
    labels = scales::label_log()
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    )
  ) +
  
  labs(
    title = "Acropora survival: 2013–2025",
    x = expression(
      "Initial size (cm"^3*", log"[10]*")"
    ),
    y = "Probability of survival"
  ) +
  
  theme_classic(base_size = 14)

p_acr_survival


# ------------------------------------------------------------
# Make Pocillopora figure
# ------------------------------------------------------------

p_poc_survival <- survival_data %>%
  filter(taxa == "Poc") %>%
  
  ggplot(
    aes(
      x = initial_size,
      y = survived
    )
  ) +
  
  # Raw binary observations: 0 = dead, 1 = alive
  geom_point(
    alpha = 0.3,
    size = 2
  ) +
  
  # Binomial logistic regression
  geom_smooth(
    method = "glm",
    method.args = list(
      family = binomial(link = "logit")
    ),
    se = TRUE,
    linewidth = 1.2
  ) +
  
  # Initial size on log10 scale
  scale_x_log10(
    labels = scales::label_log()
  ) +
  
  # Survival probability
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    )
  ) +
  
  labs(
    title = "Pocillopora survival: 2013–2025",
    x = expression(
      "Initial size (cm"^3*", log"[10]*")"
    ),
    y = "Probability of survival"
  ) +
  
  theme_classic(base_size = 14)

p_poc_survival


# ------------------------------------------------------------
# Make Porites figure
# ------------------------------------------------------------

p_por_survival <- survival_data %>%
  filter(taxa == "Por") %>%
  
  ggplot(
    aes(
      x = initial_size,
      y = survived
    )
  ) +
  
  # Raw binary observations: 0 = dead, 1 = alive
  geom_point(
    alpha = 0.3,
    size = 2
  ) +
  
  # Binomial logistic regression
  geom_smooth(
    method = "glm",
    method.args = list(
      family = binomial(link = "logit")
    ),
    se = TRUE,
    linewidth = 1.2
  ) +
  
  # Initial size on log10 scale
  scale_x_log10(
    labels = scales::label_log()
  ) +
  
  # Survival probability
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    )
  ) +
  
  labs(
    title = "Porites survival: 2013–2025",
    x = expression(
      "Initial size (cm"^3*", log"[10]*")"
    ),
    y = "Probability of survival"
  ) +
  
  theme_classic(base_size = 14)

p_por_survival


# ------------------------------------------------------------
# Create output folder
# ------------------------------------------------------------

fig_dir_survival <- file.path(
  "figs",
  "survival_all_sites"
)

dir.create(
  fig_dir_survival,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# Save survival plots
# ------------------------------------------------------------

ggsave(
  filename = file.path(
    fig_dir_survival,
    "acr_surv_all_sites.png"
  ),
  plot = p_acr_survival,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    fig_dir_survival,
    "poc_surv_all_sites.png"
  ),
  plot = p_poc_survival,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    fig_dir_survival,
    "por_surv_all_sites.png"
  ),
  plot = p_por_survival,
  width = 7,
  height = 6,
  dpi = 300
)
