############################################################################
### RECRUITMENT ALL SITES 
### FIGURES FOR H. LENIHAN'S CORAL DEMOGRAPHY DATASET: 2013-2025
### SCRIPT AND FIGURES BY DANIELLE TURNER
### AUGUST 20, 2026
############################################################################
############################################################################

# ------------------------------------------------------------
# Load libraries
# ------------------------------------------------------------
library(tidyverse)

# ------------------------------------------------------------
# Read and clean data
# ------------------------------------------------------------

coral <- read_csv(
  "data/coral_tidy_dyn_2013-2025.csv",
  show_col_types = FALSE
)

coral_recruit <- coral %>%
  mutate(
    # Fix taxon typo
    taxa = if_else(taxa == "A", "Acr", taxa)
  ) %>%
  
  # Keep focal taxa
  filter(
    taxa %in% c("Acr", "Poc", "Por"),
    
    # Use the four standard transects
    transect %in% c("T01", "T02", "T03", "T04")
  )


# ------------------------------------------------------------
# Count recruits per transect
# ------------------------------------------------------------

recruits_transect <- coral_recruit %>%
  group_by(
    year,
    taxa,
    site,
    habitat,
    transect
  ) %>%
  summarise(
    recruits = sum(
      dyn_recruitment == 1,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  # Make sure zero-recruit combinations are represented
  complete(
    year = 2013:2025,
    taxa = c("Acr", "Poc", "Por"),
    site = c("LTER1", "LTER2", "LTER4", "LTER5"),
    habitat = c("BR", "OR"),
    transect = c("T01", "T02", "T03", "T04"),
    fill = list(
      recruits = 0
    )
  )


# ------------------------------------------------------------
# Mean recruits per site
#
# Each site mean is based on 4 transects
# ------------------------------------------------------------

recruits_site_mean <- recruits_transect %>%
  group_by(
    year,
    taxa,
    site,
    habitat
  ) %>%
  summarise(
    mean_recruits_site = mean(recruits),
    .groups = "drop"
  )


# ------------------------------------------------------------
# Grand mean across 8 sites
#
# 4 LTER sites x 2 habitats = N = 8
# ------------------------------------------------------------

recruits_grand <- recruits_site_mean %>%
  group_by(
    year,
    taxa
  ) %>%
  summarise(
    mean_recruits = mean(mean_recruits_site),
    
    se_recruits =
      sd(mean_recruits_site) /
      sqrt(n()),
    
    n_sites = n(),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# Acropora recruitment - all sites
# ------------------------------------------------------------

p_acr_recruit <- recruits_grand %>%
  filter(taxa == "Acr") %>%
  
  ggplot(
    aes(
      x = factor(year),
      y = mean_recruits
    )
  ) +
  
  geom_col(
    fill = "purple",
    width = 0.75
  ) +
  
  geom_errorbar(
    aes(
      ymin = pmax(mean_recruits - se_recruits, 0),
      ymax = mean_recruits + se_recruits
    ),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  labs(
    title = "Acropora recruitment: 2013–2025",
    x = "Year",
    y = "Mean recruits per site"
  ) +
  
  theme_classic(base_size = 14)

p_acr_recruit


# ------------------------------------------------------------
# Pocillopora recruitment - all sites
# ------------------------------------------------------------

p_poc_recruit <- recruits_grand %>%
  filter(taxa == "Poc") %>%
  
  ggplot(
    aes(
      x = factor(year),
      y = mean_recruits
    )
  ) +
  
  geom_col(
    fill = "purple",
    width = 0.75
  ) +
  
  geom_errorbar(
    aes(
      ymin = pmax(mean_recruits - se_recruits, 0),
      ymax = mean_recruits + se_recruits
    ),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  labs(
    title = "Pocillopora recruitment: 2013–2025",
    x = "Year",
    y = "Mean recruits per site"
  ) +
  
  theme_classic(base_size = 14)

p_poc_recruit


# ------------------------------------------------------------
# Porites recruitment - all sites
# ------------------------------------------------------------

p_por_recruit <- recruits_grand %>%
  filter(taxa == "Por") %>%
  
  ggplot(
    aes(
      x = factor(year),
      y = mean_recruits
    )
  ) +
  
  geom_col(
    fill = "purple",
    width = 0.75
  ) +
  
  geom_errorbar(
    aes(
      ymin = pmax(mean_recruits - se_recruits, 0),
      ymax = mean_recruits + se_recruits
    ),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  labs(
    title = "Porites recruitment: 2013–2025",
    x = "Year",
    y = "Mean recruits per site"
  ) +
  
  theme_classic(base_size = 14)

p_por_recruit


# ------------------------------------------------------------
# Create output folder
# ------------------------------------------------------------

fig_dir_recruit <- file.path(
  "figs",
  "recruit_all_sites"
)

dir.create(
  fig_dir_recruit,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# Save recruitment plots
# ------------------------------------------------------------

ggsave(
  filename = file.path(
    fig_dir_recruit,
    "acr_recruit_all_sites.png"
  ),
  plot = p_acr_recruit,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    fig_dir_recruit,
    "poc_recruit_all_sites.png"
  ),
  plot = p_poc_recruit,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    fig_dir_recruit,
    "por_recruit_all_sites.png"
  ),
  plot = p_por_recruit,
  width = 7,
  height = 6,
  dpi = 300
)
