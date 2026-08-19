############################################################################
### TAXON GROWTH PER SITE
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
# Prepare site-level growth data
# ------------------------------------------------------------

growth_site <- growth_data %>%
  mutate(
    habitat_name = case_when(
      habitat == "BR" ~ "Lagoon",
      habitat == "OR" ~ "Forereef",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(site),
    !is.na(habitat_name)
  )


# ------------------------------------------------------------
# Function to make each site/habitat growth plot
# ------------------------------------------------------------

# ------------------------------------------------------------
# Function for one taxon × site × habitat plot
# ------------------------------------------------------------

make_growth_plot <- function(data, taxon_code, site_code, habitat_code) {
  
  # Filter data for this exact combination
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
  
  # Habitat display name and colors
  habitat_title <- case_when(
    habitat_code == "BR" ~ "Lagoon",
    habitat_code == "OR" ~ "Forereef"
  )
  
  point_color <- case_when(
    habitat_code == "BR" ~ "lightpink",
    habitat_code == "OR" ~ "lightblue"
  )
  
  line_color <- case_when(
    habitat_code == "BR" ~ "red",
    habitat_code == "OR" ~ "blue"
  )
  
  # Plot
  ggplot(
    plot_data,
    aes(
      x = initial_size,
      y = final_size
    )
  ) +
    
    # Scatter points
    geom_point(
      color = point_color,
      alpha = 0.6,
      size = 2
    ) +
    
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
      se = TRUE,
      color = line_color,
      linewidth = 1.2
    ) +
    
    # Log10 axes
    scale_x_log10(
      labels = scales::label_log()
    ) +
    
    scale_y_log10(
      labels = scales::label_log()
    ) +
    
    labs(
      title = paste(
        taxon_title,
        "-",
        site_code,
        "-",
        habitat_title
      ),
      x = expression("Initial size (cm"^3*", log"[10]*")"),
      y = expression("Final size (cm"^3*", log"[10]*")")
    ) +
    
    theme_classic(base_size = 14)
}


# ------------------------------------------------------------
# All 24 taxon × site × habitat combinations
# ------------------------------------------------------------

plot_combinations <- expand_grid(
  taxa = c("Acr", "Poc", "Por"),
  site = c("LTER1", "LTER2", "LTER4", "LTER5"),
  habitat = c("BR", "OR")
)


# ------------------------------------------------------------
# Generate plots
# ------------------------------------------------------------

site_growth_plots <- plot_combinations %>%
  mutate(
    plot = pmap(
      list(
        taxa,
        site,
        habitat
      ),
      function(taxa, site, habitat) {
        
        make_growth_plot(
          data = growth_data,
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

fig_dir_site <- file.path(
  "figs",
  "growth_per_site"
)

dir.create(
  fig_dir_site,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# Create filenames
# ------------------------------------------------------------

site_growth_plots <- site_growth_plots %>%
  mutate(
    filename = paste0(
      str_to_lower(taxa),
      "_",
      site,
      "_",
      habitat,
      "_growth.png"
    )
  )


# ------------------------------------------------------------
# Save all 24 plots
# ------------------------------------------------------------

pwalk(
  list(
    site_growth_plots$filename,
    site_growth_plots$plot
  ),
  function(filename, plot) {
    
    ggsave(
      filename = file.path(
        fig_dir_site,
        filename
      ),
      plot = plot,
      width = 7,
      height = 6,
      dpi = 300
    )
  }
)


# ------------------------------------------------------------
# Prepare data for stitched figures
# ------------------------------------------------------------

growth_stitched <- growth_data %>%
  mutate(
    habitat_name = case_when(
      habitat == "BR" ~ "Lagoon",
      habitat == "OR" ~ "Forereef",
      TRUE ~ NA_character_
    ),
    
    # Set order of columns and rows
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
    !is.na(habitat_name),
    !is.na(site)
  )


# ------------------------------------------------------------
# Function for stitched site-level growth figure
# ------------------------------------------------------------

make_stitched_growth <- function(data, taxon_code, taxon_title) {
  
  plot_data <- data %>%
    filter(taxa == taxon_code)
  
  ggplot(
    plot_data,
    aes(
      x = initial_size,
      y = final_size
    )
  ) +
    
    # --------------------------------------------------------
  # Lagoon points: light pink
  # --------------------------------------------------------
  
  geom_point(
    data = plot_data %>%
      filter(habitat_name == "Lagoon"),
    color = "lightpink",
    alpha = 0.5,
    size = 1.7
  ) +
    
    # --------------------------------------------------------
  # Forereef points: light blue
  # --------------------------------------------------------
  
  geom_point(
    data = plot_data %>%
      filter(habitat_name == "Forereef"),
    color = "lightblue",
    alpha = 0.5,
    size = 1.7
  ) +
    
    # --------------------------------------------------------
  # 1:1 line
  # --------------------------------------------------------
  
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linewidth = 0.4
  ) +
    
    # --------------------------------------------------------
  # Lagoon trend lines
  # --------------------------------------------------------
  
  geom_smooth(
    data = plot_data %>%
      filter(habitat_name == "Lagoon"),
    method = "lm",
    se = TRUE,
    color = "red",
    linewidth = 1
  ) +
    
    # --------------------------------------------------------
  # Forereef trend lines
  # --------------------------------------------------------
  
  geom_smooth(
    data = plot_data %>%
      filter(habitat_name == "Forereef"),
    method = "lm",
    se = TRUE,
    color = "blue",
    linewidth = 1
  ) +
    
    # --------------------------------------------------------
  # Arrange sites horizontally and habitats vertically
  # --------------------------------------------------------
  
  facet_grid(
    rows = vars(habitat_name),
    cols = vars(site)
  ) +
    
    # --------------------------------------------------------
  # Log10 axes
  # --------------------------------------------------------
  
  scale_x_log10(
    labels = scales::label_log()
  ) +
    
    scale_y_log10(
      labels = scales::label_log()
    ) +
    
    # --------------------------------------------------------
  # Labels
  # --------------------------------------------------------
  
  labs(
    title = paste0(taxon_title, " growth: 2013–2025"),
    x = expression("Initial size (cm"^3*", log"[10]*")"),
    y = expression("Final size (cm"^3*", log"[10]*")")
  ) +
    
    # --------------------------------------------------------
  # Theme
  # --------------------------------------------------------
  
  theme_bw(base_size = 14) +
    
    theme(
      # Grey facet labels like your example
      strip.background = element_rect(
        fill = "grey85",
        color = "grey30"
      ),
      
      strip.text = element_text(
        size = 13
      ),
      
      strip.text.x = element_text(
        face = "plain"
      ),
      
      strip.text.y = element_text(
        face = "plain"
      ),
      
      # Title
      plot.title = element_text(
        hjust = 0.5,
        size = 16
      ),
      
      # Axis titles
      axis.title = element_text(
        size = 14
      ),
      
      # Panel spacing
      panel.spacing = unit(
        0.15,
        "cm"
      )
    )
}


# ------------------------------------------------------------
# Acropora
# ------------------------------------------------------------

p_acr_stitched <- make_stitched_growth(
  data = growth_stitched,
  taxon_code = "Acr",
  taxon_title = "Acropora"
)

p_acr_stitched


# ------------------------------------------------------------
# Pocillopora
# ------------------------------------------------------------

p_poc_stitched <- make_stitched_growth(
  data = growth_stitched,
  taxon_code = "Poc",
  taxon_title = "Pocillopora"
)

p_poc_stitched


# ------------------------------------------------------------
# Porites
# ------------------------------------------------------------

p_por_stitched <- make_stitched_growth(
  data = growth_stitched,
  taxon_code = "Por",
  taxon_title = "Porites"
)

p_por_stitched


# ------------------------------------------------------------
# Save stitched figures
# ------------------------------------------------------------

ggsave(
  filename = file.path(
    "figs",
    "growth_per_site",
    "acr_growth_sites_stitched.png"
  ),
  plot = p_acr_stitched,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(
    "figs",
    "growth_per_site",
    "poc_growth_sites_stitched.png"
  ),
  plot = p_poc_stitched,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(
    "figs",
    "growth_per_site",
    "por_growth_sites_stitched.png"
  ),
  plot = p_por_stitched,
  width = 12,
  height = 7,
  dpi = 300
)
