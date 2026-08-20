############################################################################
### RECRUITMENT PER SITE 
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
# Calculate mean recruits + SE for each site
# Each mean is based on the 4 transects
# ------------------------------------------------------------

recruits_per_site <- recruits_transect %>%
  group_by(
    year,
    taxa,
    site,
    habitat
  ) %>%
  summarise(
    mean_recruits = mean(
      recruits,
      na.rm = TRUE
    ),
    
    se_recruits = sd(
      recruits,
      na.rm = TRUE
    ) / sqrt(n()),
    
    n_transects = n(),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# Function for one taxon x site x habitat recruitment plot
# ------------------------------------------------------------

make_recruit_site_plot <- function(data,
                                   taxon_code,
                                   site_code,
                                   habitat_code) {
  
  plot_data <- data %>%
    filter(
      taxa == taxon_code,
      site == site_code,
      habitat == habitat_code
    )
  
  # Full taxon name
  taxon_title <- case_when(
    taxon_code == "Acr" ~ "Acropora",
    taxon_code == "Poc" ~ "Pocillopora",
    taxon_code == "Por" ~ "Porites",
    TRUE ~ taxon_code
  )
  
  # Habitat display name
  habitat_title <- case_when(
    habitat_code == "BR" ~ "Lagoon",
    habitat_code == "OR" ~ "Forereef"
  )
  
  # Bar color
  bar_color <- case_when(
    habitat_code == "BR" ~ "red",
    habitat_code == "OR" ~ "blue"
  )
  
  ggplot(
    plot_data,
    aes(
      x = factor(year),
      y = mean_recruits
    )
  ) +
    
    # Mean recruitment
    geom_col(
      fill = bar_color,
      width = 0.75
    ) +
    
    # SE across the four transects
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
    
    # Label every other year as '13, '15, etc.
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
    
    labs(
      title = paste(
        taxon_title,
        "-",
        site_code,
        "-",
        habitat_title
      ),
      x = "Year",
      y = "Mean recruits"
    ) +
    
    theme_classic(base_size = 14)
}


# ------------------------------------------------------------
# Define all 24 combinations
# ------------------------------------------------------------

recruit_site_combinations <- expand_grid(
  taxa = c(
    "Acr",
    "Poc",
    "Por"
  ),
  site = c(
    "LTER1",
    "LTER2",
    "LTER4",
    "LTER5"
  ),
  habitat = c(
    "BR",
    "OR"
  )
)


# ------------------------------------------------------------
# Generate all 24 plots
# ------------------------------------------------------------

site_recruit_plots <- recruit_site_combinations %>%
  mutate(
    plot = pmap(
      list(
        taxa,
        site,
        habitat
      ),
      function(taxa, site, habitat) {
        
        make_recruit_site_plot(
          data = recruits_per_site,
          taxon_code = taxa,
          site_code = site,
          habitat_code = habitat
        )
      }
    )
  )


# ------------------------------------------------------------
# Create output folder
# ------------------------------------------------------------

fig_dir_recruit_site <- file.path(
  "figs",
  "recruit_per_site"
)

dir.create(
  fig_dir_recruit_site,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# Save all 24 recruitment plots
# ------------------------------------------------------------

pwalk(
  site_recruit_plots,
  function(taxa, site, habitat, plot) {
    
    # Lowercase taxon for filename
    taxon_file <- tolower(taxa)
    
    # Create filename
    # Example: acr_LTER1_BR_recruit.png
    file_name <- paste0(
      taxon_file,
      "_",
      site,
      "_",
      habitat,
      "_recruit.png"
    )
    
    # Save plot
    ggsave(
      filename = file.path(
        fig_dir_recruit_site,
        file_name
      ),
      plot = plot,
      width = 7,
      height = 6,
      dpi = 300
    )
  }
)


# ------------------------------------------------------------
# Prepare data for stitched per-site recruitment figures
# ------------------------------------------------------------

recruit_site_stitched <- recruits_per_site %>%
  mutate(
    habitat_name = case_when(
      habitat == "BR" ~ "Lagoon",
      habitat == "OR" ~ "Forereef",
      TRUE ~ NA_character_
    ),
    
    site = factor(
      site,
      levels = c(
        "LTER1",
        "LTER2",
        "LTER4",
        "LTER5"
      )
    ),
    
    habitat_name = factor(
      habitat_name,
      levels = c(
        "Lagoon",
        "Forereef"
      )
    )
  ) %>%
  filter(
    !is.na(site),
    !is.na(habitat_name)
  )


# ------------------------------------------------------------
# Function for stitched per-site recruitment figure
# ------------------------------------------------------------

make_stitched_recruit <- function(data, taxon_code, taxon_title) {
  
  plot_data <- data %>%
    filter(taxa == taxon_code)
  
  ggplot(
    plot_data,
    aes(
      x = factor(year),
      y = mean_recruits
    )
  ) +
    
    # Bars colored by habitat
    geom_col(
      aes(fill = habitat_name),
      width = 0.75
    ) +
    
    # SE based on 4 transects within each site
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
    
    # LTER sites across columns
    # Lagoon top, Forereef bottom
    facet_grid(
      rows = vars(habitat_name),
      cols = vars(site)
    ) +
    
    # Habitat colors
    scale_fill_manual(
      values = c(
        "Lagoon" = "red",
        "Forereef" = "blue"
      ),
      guide = "none"
    ) +
    
    # Every other year, abbreviated
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
    
    labs(
      title = paste0(
        taxon_title,
        " recruitment: 2013–2025"
      ),
      x = "Year",
      y = "Mean recruits"
    ) +
    
    theme_bw(base_size = 14) +
    
    theme(
      # Grey facet headers like growth figure
      strip.background = element_rect(
        fill = "grey85",
        color = "grey30"
      ),
      
      strip.text.x = element_text(
        size = 13,
        face = "plain"
      ),
      
      strip.text.y = element_text(
        size = 13,
        face = "plain"
      ),
      
      # Center title
      plot.title = element_text(
        hjust = 0.5,
        size = 16
      ),
      
      # Shared axis titles
      axis.title = element_text(
        size = 14
      ),
      
      # Keep abbreviated years horizontal
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5
      ),
      
      # Tight panel spacing
      panel.spacing = unit(
        0.15,
        "cm"
      )
    )
}


# ------------------------------------------------------------
# Acropora
# ------------------------------------------------------------

p_acr_recruit_stitched <- make_stitched_recruit(
  data = recruit_site_stitched,
  taxon_code = "Acr",
  taxon_title = "Acropora"
)

p_acr_recruit_stitched


# ------------------------------------------------------------
# Pocillopora
# ------------------------------------------------------------

p_poc_recruit_stitched <- make_stitched_recruit(
  data = recruit_site_stitched,
  taxon_code = "Poc",
  taxon_title = "Pocillopora"
)

p_poc_recruit_stitched


# ------------------------------------------------------------
# Porites
# ------------------------------------------------------------

p_por_recruit_stitched <- make_stitched_recruit(
  data = recruit_site_stitched,
  taxon_code = "Por",
  taxon_title = "Porites"
)

p_por_recruit_stitched


# ------------------------------------------------------------
# Save stitched recruitment figures
# ------------------------------------------------------------

ggsave(
  filename = file.path(
    "figs",
    "recruit_per_site",
    "acr_recruit_sites_stitched.png"
  ),
  plot = p_acr_recruit_stitched,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(
    "figs",
    "recruit_per_site",
    "poc_recruit_sites_stitched.png"
  ),
  plot = p_poc_recruit_stitched,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(
    "figs",
    "recruit_per_site",
    "por_recruit_sites_stitched.png"
  ),
  plot = p_por_recruit_stitched,
  width = 12,
  height = 7,
  dpi = 300
)
