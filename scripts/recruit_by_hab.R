############################################################################
### RECRUITMENT BY HABITAT 
### FIGURES FOR H. LENIHAN'S CORAL DEMOGRAPHY DATASET: 2013-2025
### SCRIPT AND FIGURES BY DANIELLE TURNER
### AUGUST 20, 2026
############################################################################
############################################################################

# ------------------------------------------------------------
# Load libraries
# ------------------------------------------------------------
library(tidyverse)
library(patchwork)

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
# Step 1: Mean recruits per site
#
# Each site mean is based on the 4 transects
# ------------------------------------------------------------

recruits_site_hab <- recruits_transect %>%
  group_by(
    year,
    taxa,
    site,
    habitat
  ) %>%
  summarise(
    mean_recruits_site = mean(
      recruits,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ------------------------------------------------------------
# Step 2: Grand mean within each habitat
#
# N = 4 site means per habitat
# ------------------------------------------------------------

recruits_habitat <- recruits_site_hab %>%
  group_by(
    year,
    taxa,
    habitat
  ) %>%
  summarise(
    mean_recruits = mean(
      mean_recruits_site,
      na.rm = TRUE
    ),
    
    se_recruits = sd(
      mean_recruits_site,
      na.rm = TRUE
    ) / sqrt(n()),
    
    n_sites = n(),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    habitat_name = case_when(
      habitat == "BR" ~ "Lagoon",
      habitat == "OR" ~ "Forereef"
    )
  )


# ------------------------------------------------------------
# Acropora Lagoon
# ------------------------------------------------------------

p_acr_recruit_lagoon <- recruits_habitat %>%
  filter(
    taxa == "Acr",
    habitat == "BR"
  ) %>%
  
  ggplot(
    aes(
      x = factor(year),
      y = mean_recruits
    )
  ) +
  
  geom_col(
    fill = "red",
    width = 0.75
  ) +
  
  geom_errorbar(
    aes(
      ymin = pmax(
        mean_recruits - se_recruits,
        0
      ),
      ymax = mean_recruits + se_recruits
    ),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  labs(
    title = "Acropora recruitment: Lagoon",
    x = "Year",
    y = "Mean recruits per site"
  ) +
  
  theme_classic(base_size = 14)

p_acr_recruit_lagoon


# ------------------------------------------------------------
# Acropora Forereef
# ------------------------------------------------------------

p_acr_recruit_forereef <- recruits_habitat %>%
  filter(
    taxa == "Acr",
    habitat == "OR"
  ) %>%
  
  ggplot(
    aes(
      x = factor(year),
      y = mean_recruits
    )
  ) +
  
  geom_col(
    fill = "blue",
    width = 0.75
  ) +
  
  geom_errorbar(
    aes(
      ymin = pmax(
        mean_recruits - se_recruits,
        0
      ),
      ymax = mean_recruits + se_recruits
    ),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  labs(
    title = "Acropora recruitment: Forereef",
    x = "Year",
    y = "Mean recruits per site"
  ) +
  
  theme_classic(base_size = 14)

p_acr_recruit_forereef


# ------------------------------------------------------------
# Pocillopora Lagoon
# ------------------------------------------------------------

p_poc_recruit_lagoon <- recruits_habitat %>%
  filter(
    taxa == "Poc",
    habitat == "BR"
  ) %>%
  
  ggplot(
    aes(
      x = factor(year),
      y = mean_recruits
    )
  ) +
  
  geom_col(
    fill = "red",
    width = 0.75
  ) +
  
  geom_errorbar(
    aes(
      ymin = pmax(
        mean_recruits - se_recruits,
        0
      ),
      ymax = mean_recruits + se_recruits
    ),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  labs(
    title = "Pocillopora recruitment: Lagoon",
    x = "Year",
    y = "Mean recruits per site"
  ) +
  
  theme_classic(base_size = 14)

p_poc_recruit_lagoon


# ------------------------------------------------------------
# Pocillopora Forereef
# ------------------------------------------------------------
p_poc_recruit_forereef <- recruits_habitat %>%
  filter(
    taxa == "Poc",
    habitat == "OR"
  ) %>%
  
  ggplot(
    aes(
      x = factor(year),
      y = mean_recruits
    )
  ) +
  
  geom_col(
    fill = "blue",
    width = 0.75
  ) +
  
  geom_errorbar(
    aes(
      ymin = pmax(
        mean_recruits - se_recruits,
        0
      ),
      ymax = mean_recruits + se_recruits
    ),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  labs(
    title = "Pocillopora recruitment: Forereef",
    x = "Year",
    y = "Mean recruits per site"
  ) +
  
  theme_classic(base_size = 14)

p_poc_recruit_forereef


# ------------------------------------------------------------
# Porites Lagoon
# ------------------------------------------------------------

p_por_recruit_lagoon <- recruits_habitat %>%
  filter(
    taxa == "Por",
    habitat == "BR"
  ) %>%
  
  ggplot(
    aes(
      x = factor(year),
      y = mean_recruits
    )
  ) +
  
  geom_col(
    fill = "red",
    width = 0.75
  ) +
  
  geom_errorbar(
    aes(
      ymin = pmax(
        mean_recruits - se_recruits,
        0
      ),
      ymax = mean_recruits + se_recruits
    ),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  labs(
    title = "Porites recruitment: Lagoon",
    x = "Year",
    y = "Mean recruits per site"
  ) +
  
  theme_classic(base_size = 14)

p_por_recruit_lagoon


# ------------------------------------------------------------
# Porites Forereef
# ------------------------------------------------------------

p_por_recruit_forereef <- recruits_habitat %>%
  filter(
    taxa == "Por",
    habitat == "OR"
  ) %>%
  
  ggplot(
    aes(
      x = factor(year),
      y = mean_recruits
    )
  ) +
  
  geom_col(
    fill = "blue",
    width = 0.75
  ) +
  
  geom_errorbar(
    aes(
      ymin = pmax(
        mean_recruits - se_recruits,
        0
      ),
      ymax = mean_recruits + se_recruits
    ),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  labs(
    title = "Porites recruitment: Forereef",
    x = "Year",
    y = "Mean recruits per site"
  ) +
  
  theme_classic(base_size = 14)

p_por_recruit_forereef


# ------------------------------------------------------------
# Prepare data for stitched recruitment figure
# ------------------------------------------------------------

recruits_habitat_stitched <- recruits_habitat %>%
  mutate(
    taxon_name = case_when(
      taxa == "Acr" ~ "Acropora",
      taxa == "Poc" ~ "Pocillopora",
      taxa == "Por" ~ "Porites"
    ),
    
    # Set column order
    taxon_name = factor(
      taxon_name,
      levels = c(
        "Acropora",
        "Pocillopora",
        "Porites"
      )
    ),
    
    # Set row order
    habitat_name = factor(
      habitat_name,
      levels = c(
        "Lagoon",
        "Forereef"
      )
    )
  )



# ------------------------------------------------------------
# Stitched recruitment plot
# ------------------------------------------------------------

p_recruit_hab_stitched <- ggplot(
  recruits_habitat_stitched,
  aes(
    x = factor(year),
    y = mean_recruits
  )
) +
  
  # ----------------------------------------------------------
# Mean recruitment bars
# Lagoon = red
# Forereef = blue
# ----------------------------------------------------------

geom_col(
  aes(fill = habitat_name),
  width = 0.75
) +
  
  # ----------------------------------------------------------
# Grand mean +/- SE
# ----------------------------------------------------------

geom_errorbar(
  aes(
    ymin = pmax(
      mean_recruits - se_recruits,
      0
    ),
    ymax = mean_recruits + se_recruits
  ),
  width = 0.2,
  linewidth = 0.5
) +
  
  # ----------------------------------------------------------
# Taxa across columns
# Habitat across rows
# ----------------------------------------------------------

facet_grid(
  rows = vars(habitat_name),
  cols = vars(taxon_name)
) +
  
  # ----------------------------------------------------------
# Habitat colors
# ----------------------------------------------------------

scale_fill_manual(
  values = c(
    "Lagoon" = "red",
    "Forereef" = "blue"
  ),
  guide = "none"
) +
  
  # ----------------------------------------------------------
# Show only every other year label
#
# Bars are still shown for EVERY year
# ----------------------------------------------------------

scale_x_discrete(
  breaks = as.character(
    seq(2013, 2025, by = 2)
  ),
  labels = paste0(
    "'",
    substr(
      seq(2013, 2025, by = 2),
      3,
      4
    )
  )
) +
  
  # ----------------------------------------------------------
# Shared labels
# ----------------------------------------------------------

labs(
  title = "Coral recruitment: 2013–2025",
  x = "Year",
  y = "Mean recruits per site"
) +
  
  # ----------------------------------------------------------
# Theme
# ----------------------------------------------------------

theme_bw(base_size = 14) +
  
  theme(
    # Grey facet headers
    strip.background = element_rect(
      fill = "grey85",
      color = "grey30"
    ),
    
    # Taxon names across top
    strip.text.x = element_text(
      size = 13,
      face = "plain"
    ),
    
    # Lagoon / Forereef on right
    strip.text.y = element_text(
      size = 13,
      face = "plain"
    ),
    
    # Center overall title
    plot.title = element_text(
      hjust = 0.5,
      size = 16
    ),
    
    # Shared axis titles
    axis.title = element_text(
      size = 14
    ),
    
    # Keep year labels horizontal
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    
    # Small spacing between panels
    panel.spacing = unit(
      0.15,
      "cm"
    )
  )

p_recruit_hab_stitched


# ------------------------------------------------------------
# Save all recruitment-by-habitat figures
# ------------------------------------------------------------

fig_dir_recruit_hab <- file.path(
  "figs",
  "recruit_by_hab"
)

ggsave(
  filename = file.path(
    fig_dir_recruit_hab,
    "acr_recruit_lagoon.png"
  ),
  plot = p_acr_recruit_lagoon,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    fig_dir_recruit_hab,
    "acr_recruit_forereef.png"
  ),
  plot = p_acr_recruit_forereef,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    fig_dir_recruit_hab,
    "poc_recruit_lagoon.png"
  ),
  plot = p_poc_recruit_lagoon,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    fig_dir_recruit_hab,
    "poc_recruit_forereef.png"
  ),
  plot = p_poc_recruit_forereef,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    fig_dir_recruit_hab,
    "por_recruit_lagoon.png"
  ),
  plot = p_por_recruit_lagoon,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    fig_dir_recruit_hab,
    "por_recruit_forereef.png"
  ),
  plot = p_por_recruit_forereef,
  width = 7,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    fig_dir_recruit_hab,
    "recruit_hab_stitched.png"
  ),
  plot = p_recruit_hab_stitched,
  width = 12,
  height = 8,
  dpi = 300
)
