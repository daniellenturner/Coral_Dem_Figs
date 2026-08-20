############################################################################
### SURVIVAL BY HABITAT (LAGOON & FOREREEF)
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
# Add habitat names to survival dataset
# ------------------------------------------------------------

survival_habitat <- survival_data %>%
  mutate(
    habitat_name = case_when(
      habitat == "BR" ~ "Lagoon",
      habitat == "OR" ~ "Forereef",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(habitat_name))


p_acr_surv_hab <- survival_habitat %>%
  filter(taxa == "Acr") %>%
  ggplot(
    aes(
      x = initial_size,
      y = survived
    )
  ) +
  
  # Light-colored datapoints
  geom_point(
    aes(color = habitat_name),
    alpha = 0.6,
    size = 2
  ) +
  
  scale_color_manual(
    values = c(
      "Lagoon" = "lightpink",
      "Forereef" = "lightblue"
    ),
    name = "Habitat"
  ) +
  
  # Start a new color scale for regression fits
  ggnewscale::new_scale_color() +
  
  # Binomial regression + SE
  # SE shading now matches the growth plots
  geom_smooth(
    aes(color = habitat_name),
    method = "glm",
    method.args = list(
      family = binomial(link = "logit")
    ),
    se = TRUE,
    linewidth = 1.2
  ) +
  
  scale_color_manual(
    values = c(
      "Lagoon" = "red",
      "Forereef" = "blue"
    ),
    name = "Habitat"
  ) +
  
  scale_x_log10(
    labels = scales::label_log()
  ) +
  
  scale_y_continuous(
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    )
  ) +
  
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  
  labs(
    title = "Acropora survival: 2013–2025",
    x = expression(
      "Initial size (cm"^3*", log"[10]*")"
    ),
    y = "Probability of survival"
  ) +
  
  theme_classic(base_size = 14)

p_acr_surv_hab


# ------------------------------------------------------------
# Pocillopora survival by habitat
# ------------------------------------------------------------

p_poc_surv_hab <- survival_habitat %>%
  filter(taxa == "Poc") %>%
  ggplot(
    aes(
      x = initial_size,
      y = survived
    )
  ) +
  
  # Light-colored datapoints
  geom_point(
    aes(color = habitat_name),
    alpha = 0.6,
    size = 2
  ) +
  
  scale_color_manual(
    values = c(
      "Lagoon" = "lightpink",
      "Forereef" = "lightblue"
    ),
    name = "Habitat"
  ) +
  
  # Start a new color scale for regression fits
  ggnewscale::new_scale_color() +
  
  # Binomial regression + SE
  # SE shading matches growth plots
  geom_smooth(
    aes(color = habitat_name),
    method = "glm",
    method.args = list(
      family = binomial(link = "logit")
    ),
    se = TRUE,
    linewidth = 1.2
  ) +
  
  # Strong regression line colors
  scale_color_manual(
    values = c(
      "Lagoon" = "red",
      "Forereef" = "blue"
    ),
    name = "Habitat"
  ) +
  
  # Log10 initial size
  scale_x_log10(
    labels = scales::label_log()
  ) +
  
  # Survival probability
  scale_y_continuous(
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    )
  ) +
  
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  
  labs(
    title = "Pocillopora survival: 2013–2025",
    x = expression(
      "Initial size (cm"^3*", log"[10]*")"
    ),
    y = "Probability of survival"
  ) +
  
  theme_classic(base_size = 14)

p_poc_surv_hab


# ------------------------------------------------------------
# Porites survival by habitat
# ------------------------------------------------------------

p_por_surv_hab <- survival_habitat %>%
  filter(taxa == "Por") %>%
  ggplot(
    aes(
      x = initial_size,
      y = survived
    )
  ) +
  
  # Light-colored datapoints
  geom_point(
    aes(color = habitat_name),
    alpha = 0.6,
    size = 2
  ) +
  
  scale_color_manual(
    values = c(
      "Lagoon" = "lightpink",
      "Forereef" = "lightblue"
    ),
    name = "Habitat"
  ) +
  
  # Start a new color scale for regression fits
  ggnewscale::new_scale_color() +
  
  # Binomial regression + SE
  # SE shading matches growth plots
  geom_smooth(
    aes(color = habitat_name),
    method = "glm",
    method.args = list(
      family = binomial(link = "logit")
    ),
    se = TRUE,
    linewidth = 1.2
  ) +
  
  # Strong regression line colors
  scale_color_manual(
    values = c(
      "Lagoon" = "red",
      "Forereef" = "blue"
    ),
    name = "Habitat"
  ) +
  
  # Log10 initial size
  scale_x_log10(
    labels = scales::label_log()
  ) +
  
  # Survival probability
  scale_y_continuous(
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    )
  ) +
  
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  
  labs(
    title = "Porites survival: 2013–2025",
    x = expression(
      "Initial size (cm"^3*", log"[10]*")"
    ),
    y = "Probability of survival"
  ) +
  
  theme_classic(base_size = 14)

p_por_surv_hab


# ------------------------------------------------------------
# Create output folder
# ------------------------------------------------------------

fig_dir_surv_hab <- file.path(
  "figs",
  "survival_by_hab"
)

dir.create(
  fig_dir_surv_hab,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# Save survival plots
# ------------------------------------------------------------

ggsave(
  filename = file.path(
    fig_dir_surv_hab,
    "acr_surv_hab.png"
  ),
  plot = p_acr_surv_hab,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    fig_dir_surv_hab,
    "poc_surv_hab.png"
  ),
  plot = p_poc_surv_hab,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    fig_dir_surv_hab,
    "por_surv_hab.png"
  ),
  plot = p_por_surv_hab,
  width = 7,
  height = 6,
  dpi = 300
)
