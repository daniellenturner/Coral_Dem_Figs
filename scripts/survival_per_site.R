############################################################################
### SURVIVAL PER SITE 
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
# Prepare per-site survival data
# ------------------------------------------------------------

survival_site <- survival_data %>%
  filter(
    taxa %in% c("Acr", "Poc", "Por"),
    site %in% c("LTER1", "LTER2", "LTER4", "LTER5"),
    habitat %in% c("BR", "OR")
  )


# ------------------------------------------------------------
# Function to make one taxon × site × habitat survival plot
# ------------------------------------------------------------

make_survival_plot <- function(data,
                               taxon_code,
                               site_code,
                               habitat_code) {
  
  # Filter to this combination
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
  
  # Habitat name
  habitat_title <- case_when(
    habitat_code == "BR" ~ "Lagoon",
    habitat_code == "OR" ~ "Forereef"
  )
  
  # Point colors
  point_color <- case_when(
    habitat_code == "BR" ~ "lightpink",
    habitat_code == "OR" ~ "lightblue"
  )
  
  # Regression colors
  line_color <- case_when(
    habitat_code == "BR" ~ "red",
    habitat_code == "OR" ~ "blue"
  )
  
  # ----------------------------------------------------------
  # Make plot
  # ----------------------------------------------------------
  
  ggplot(
    plot_data,
    aes(
      x = initial_size,
      y = survived
    )
  ) +
    
    # Logistic regression + SE
    geom_smooth(
      method = "glm",
      method.args = list(
        family = binomial(link = "logit")
      ),
      se = TRUE,
      color = line_color,
      linewidth = 1.2
    ) +
    
    # Raw binary observations
    geom_point(
      color = point_color,
      alpha = 0.6,
      size = 2
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
      title = paste(
        taxon_title,
        "-",
        site_code,
        "-",
        habitat_title
      ),
      x = expression(
        "Initial size (cm"^3*", log"[10]*")"
      ),
      y = "Probability of survival"
    ) +
    
    theme_classic(base_size = 14)
}


# ------------------------------------------------------------
# Define all 24 combinations
# ------------------------------------------------------------

survival_combinations <- expand_grid(
  taxa = c("Acr", "Poc", "Por"),
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

site_survival_plots <- survival_combinations %>%
  mutate(
    plot = pmap(
      list(
        taxa,
        site,
        habitat
      ),
      function(taxa, site, habitat) {
        
        make_survival_plot(
          data = survival_site,
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

fig_dir_surv_site <- file.path(
  "figs",
  "survival_per_site"
)

dir.create(
  fig_dir_surv_site,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# Save all 24 survival plots
# ------------------------------------------------------------

pwalk(
  site_survival_plots,
  function(taxa, site, habitat, plot) {
    
    # Make taxon lowercase for filename
    taxon_file <- tolower(taxa)
    
    # Create filename
    file_name <- paste0(
      taxon_file,
      "_",
      site,
      "_",
      habitat,
      "_surv.png"
    )
    
    # Save plot
    ggsave(
      filename = file.path(
        fig_dir_surv_site,
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
# Prepare stitched survival dataset
# ------------------------------------------------------------

survival_stitched <- survival_data %>%
  mutate(
    habitat_name = case_when(
      habitat == "BR" ~ "Lagoon",
      habitat == "OR" ~ "Forereef",
      TRUE ~ NA_character_
    ),
    
    site = factor(
      site,
      levels = c("LTER1", "LTER2", "LTER4", "LTER5")
    ),
    
    habitat_name = factor(
      habitat_name,
      levels = c("Lagoon", "Forereef")
    )
  ) %>%
  filter(
    !is.na(site),
    !is.na(habitat_name)
  )


# ------------------------------------------------------------
# Function for stitched survival figure
# ------------------------------------------------------------

make_stitched_survival <- function(data, taxon_code, taxon_title) {
  
  plot_data <- data %>%
    filter(taxa == taxon_code)
  
  ggplot(
    plot_data,
    aes(
      x = initial_size,
      y = survived
    )
  ) +
    
    # Lagoon logistic regression
    geom_smooth(
      data = plot_data %>%
        filter(habitat_name == "Lagoon"),
      method = "glm",
      method.args = list(
        family = binomial(link = "logit")
      ),
      se = TRUE,
      color = "red",
      linewidth = 1
    ) +
    
    # Forereef logistic regression
    geom_smooth(
      data = plot_data %>%
        filter(habitat_name == "Forereef"),
      method = "glm",
      method.args = list(
        family = binomial(link = "logit")
      ),
      se = TRUE,
      color = "blue",
      linewidth = 1
    ) +
    
    # Lagoon points
    geom_point(
      data = plot_data %>%
        filter(habitat_name == "Lagoon"),
      color = "lightpink",
      alpha = 0.6,
      size = 1.7
    ) +
    
    # Forereef points
    geom_point(
      data = plot_data %>%
        filter(habitat_name == "Forereef"),
      color = "lightblue",
      alpha = 0.6,
      size = 1.7
    ) +
    
    # LTER sites horizontally, habitat vertically
    facet_grid(
      rows = vars(habitat_name),
      cols = vars(site)
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
      title = paste0(
        taxon_title,
        " survival: 2013–2025"
      ),
      x = expression(
        "Initial size (cm"^3*", log"[10]*")"
      ),
      y = "Probability of survival"
    ) +
    
    theme_bw(base_size = 14) +
    
    theme(
      strip.background = element_rect(
        fill = "grey85",
        color = "grey30"
      ),
      
      strip.text = element_text(
        size = 13
      ),
      
      plot.title = element_text(
        hjust = 0.5,
        size = 16
      ),
      
      axis.title = element_text(
        size = 14
      ),
      
      panel.spacing = unit(
        0.15,
        "cm"
      )
    )
}


# ------------------------------------------------------------
# Acropora
# ------------------------------------------------------------

p_acr_surv_stitched <- make_stitched_survival(
  data = survival_stitched,
  taxon_code = "Acr",
  taxon_title = "Acropora"
)

p_acr_surv_stitched


# ------------------------------------------------------------
# Pocillopora
# ------------------------------------------------------------

p_poc_surv_stitched <- make_stitched_survival(
  data = survival_stitched,
  taxon_code = "Poc",
  taxon_title = "Pocillopora"
)

p_poc_surv_stitched


# ------------------------------------------------------------
# Porites
# ------------------------------------------------------------

p_por_surv_stitched <- make_stitched_survival(
  data = survival_stitched,
  taxon_code = "Por",
  taxon_title = "Porites"
)

p_por_surv_stitched


# ------------------------------------------------------------
# Save stitched survival figures
# ------------------------------------------------------------

ggsave(
  filename = file.path(
    "figs",
    "survival_per_site",
    "acr_surv_sites_stitched.png"
  ),
  plot = p_acr_surv_stitched,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(
    "figs",
    "survival_per_site",
    "poc_surv_sites_stitched.png"
  ),
  plot = p_poc_surv_stitched,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(
    "figs",
    "survival_per_site",
    "por_surv_sites_stitched.png"
  ),
  plot = p_por_surv_stitched,
  width = 12,
  height = 7,
  dpi = 300
)
