############################################################################
### GROWTH PER HABITAT (LAGOON & FOREREEF) 
### FIGURES FOR H. LENIHAN'S CORAL DEMOGRAPHY DATASET: 2013-2025
### SCRIPT AND FIGURES BY DANIELLE TURNER
### AUGUST 19, 2026
############################################################################
############################################################################

# ------------------------------------------------------------
# Load libraries
# ------------------------------------------------------------
library(tidyverse)
library(scales)
library(ggnewscale)

# ------------------------------------------------------------
# Read in data
# ------------------------------------------------------------
coral <- read_csv("data/coral_tidy_dyn_2013-2025.csv",
                  show_col_types = FALSE)

# ------------------------------------------------------------
# Clean data and calculate colony volume
# ------------------------------------------------------------

coral_clean <- coral %>%
  mutate(
    # Fix taxon typo
    taxa = if_else(taxa == "A", "Acr", taxa),
    
    # Convert dimensions to numeric
    # D, UK, etc. become NA
    length = as.numeric(length),
    width  = as.numeric(width),
    height = as.numeric(height),
    
    # Calculate colony volume (cm^3)
    size_cm3 = length * width * height
  ) %>%
  
  # Keep focal taxa
  filter(taxa %in% c("Acr", "Poc", "Por"))

# ------------------------------------------------------------
# Initial size = size in 2013
# ------------------------------------------------------------

initial_2013 <- coral_clean %>%
  filter(
    year == 2013,
    !is.na(size_cm3),
    size_cm3 > 0
  ) %>%
  select(
    coral_number,
    taxa,
    site,
    habitat,
    initial_size = size_cm3
  )


# ------------------------------------------------------------
# Final size = last valid size recorded for each coral
# ------------------------------------------------------------

final_size <- coral_clean %>%
  filter(
    !is.na(size_cm3),
    size_cm3 > 0
  ) %>%
  
  # Put observations in chronological order
  arrange(coral_number, year) %>%
  
  group_by(coral_number) %>%
  
  # Keep the last year with a valid size measurement
  slice_tail(n = 1) %>%
  
  ungroup() %>%
  
  select(
    coral_number,
    final_year = year,
    final_size = size_cm3
  )


# ------------------------------------------------------------
# Match initial and final size
# ------------------------------------------------------------

growth_data <- initial_2013 %>%
  inner_join(
    final_size,
    by = "coral_number"
  ) %>%
  
  filter(
    !is.na(initial_size),
    !is.na(final_size),
    initial_size > 0,
    final_size > 0
  )

# ------------------------------------------------------------
# Add habitat names to the revised growth dataset
# ------------------------------------------------------------

growth_habitat <- growth_data %>%
  mutate(
    habitat_name = case_when(
      habitat == "BR" ~ "Lagoon",
      habitat == "OR" ~ "Forereef",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(habitat_name))


# ------------------------------------------------------------
# Make Acropora figure
# ------------------------------------------------------------

p_acr_habitat <- growth_habitat %>%
  filter(taxa == "Acr") %>%
  ggplot(aes(x = initial_size, y = final_size)) +
  
  # Light-colored points with legend
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
  
  # Start a new color scale for trend lines
  ggnewscale::new_scale_color() +
  
  # 1:1 line
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linewidth = 0.4
  ) +
  
  # Strong trend lines
  geom_smooth(
    aes(color = habitat_name),
    method = "lm",
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
  
  scale_y_log10(
    labels = scales::label_log()
  ) +
  
  labs(
    title = "Acropora growth: 2013–2025",
    x = expression("Initial size (cm"^3*", log"[10]*")"),
    y = expression("Final size (cm"^3*", log"[10]*")")
  ) +
  
  theme_classic(base_size = 14)

p_acr_habitat


# ------------------------------------------------------------
# Make Pocillopora figure
# ------------------------------------------------------------

p_poc_habitat <- growth_habitat %>%
  filter(taxa == "Poc") %>%
  ggplot(aes(x = initial_size, y = final_size)) +
  
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
  
  ggnewscale::new_scale_color() +
  
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linewidth = 0.4
  ) +
  
  geom_smooth(
    aes(color = habitat_name),
    method = "lm",
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
  
  scale_y_log10(
    labels = scales::label_log()
  ) +
  
  labs(
    title = "Pocillopora growth: 2013–2025",
    x = expression("Initial size (cm"^3*", log"[10]*")"),
    y = expression("Final size (cm"^3*", log"[10]*")")
  ) +
  
  theme_classic(base_size = 14)

p_poc_habitat


# ------------------------------------------------------------
# Make Porites figure
# ------------------------------------------------------------

p_por_habitat <- growth_habitat %>%
  filter(taxa == "Por") %>%
  ggplot(aes(x = initial_size, y = final_size)) +
  
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
  
  ggnewscale::new_scale_color() +
  
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linewidth = 0.4
  ) +
  
  geom_smooth(
    aes(color = habitat_name),
    method = "lm",
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
  
  scale_y_log10(
    labels = scales::label_log()
  ) +
  
  labs(
    title = "Porites growth: 2013–2025",
    x = expression("Initial size (cm"^3*", log"[10]*")"),
    y = expression("Final size (cm"^3*", log"[10]*")")
  ) +
  
  theme_classic(base_size = 14)

p_por_habitat


# ------------------------------------------------------------
# Save habitat growth plots
# ------------------------------------------------------------

ggsave(
  filename = file.path("figs", "growth_by_hab", "acr_growth_hab.png"),
  plot = p_acr_habitat,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path("figs", "growth_by_hab", "poc_growth_hab.png"),
  plot = p_poc_habitat,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path("figs", "growth_by_hab", "por_growth_hab.png"),
  plot = p_por_habitat,
  width = 7,
  height = 6,
  dpi = 300
)
