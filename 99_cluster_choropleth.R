source("03_estimate_impacts.R")

cluster_shp <- sf::st_read("data/cluster/cluster.shp")

# Helper to bin the data into "1 in X" categories
round_denom <- function(val, round = 25) {
  if (is.na(val) || val == 0) return("0")
  
  # Calculate denominator and round to nearest round
  denom <- 1 / val
  rounded_denom <- round(denom / round) * round
  
  return(rounded_denom)
}

# Process the data
plot_data <- cluster_shp %>%
  left_join(cluster_impacts, by = join_by(cluster == cluster)) %>%
  mutate(
    rate = excess_mort/tot_ae_adm,
    denom = sapply(rate, round_denom, round = 10),
    rate_bin = str_c("1 in ", denom),
    # Create a numeric version of the bin for sorting purposes
    bin_numeric = 1 / as.numeric(str_extract(rate_bin, "\\d+")),
    precise_denom = round(1 / rate),
    # Tooltip string with the specific '1 in X' value
    tooltip_text = paste0(cluster, "\nRate: 1 in ", precise_denom)
  ) %>%
  ms_simplify(keep = 0.0005)

# Create a color mapping for ONLY the bins present in the data
unique_bins <- plot_data %>% 
  arrange(denom) %>% 
  pull(rate_bin) %>% 
  unique()

breaks <- plot_data %>% 
  arrange(denom) %>% 
  pull(denom) %>% 
  unique()

base_colors <- paletteer::paletteer_d("beyonce::X41")
pal_func <- colorRampPalette(as.character(base_colors))


pal <- pal_func(length(unique_bins))


ggplot(plot_data, aes(fill = 1/denom)) + # Use numeric fill for stepsn
  geom_sf(col = "white", linewidth = 0.3) +
  scale_fill_stepsn(
    # Use the hex codes from paletteer here
    colors = as.character(pal), 
    breaks = 1/breaks,
    values = scales::rescale(1/breaks),
    labels = rate_labeller,
    limits = range(1/breaks),
    guide = guide_colorsteps(
      even.steps = FALSE, 
      show.limits = FALSE,
      title.position = "top",
      barheight = unit(0.05, 'npc'),
      barwidth = unit(1, 'npc') 
    )
  ) +
  theme_void() +
  labs(
    fill = "Risk rate (excess expected deaths)",
    caption = "Rates binned to nearest 1/25 resolution"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 8)
  )



p <- ggplot(plot_data, aes(fill = 1/denom)) + 
  geom_sf_interactive(
    aes(
      tooltip = tooltip_text, 
      data_id = cluster
    ),
    col = "white", 
    linewidth = 0.3
  ) +
  scale_fill_stepsn(
    colors = as.character(pal), 
    breaks = 1/breaks,
    values = scales::rescale(1/breaks), 
    labels = rate_labeller,
    limits = range(1/breaks),
    guide = guide_colorsteps(
      even.steps = FALSE, 
      show.limits = FALSE,
      title.position = "top",
      barheight = unit(0.05, 'npc'),
      barwidth = unit(0.9, 'npc') 
    )
  ) +
  labs(
    fill = "Risk rate (excess expected deaths, per type-1 A&E admission)",
    # caption = "Rates binned to nearest 1/25 resolution"
  ) +
    theme_void() + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 8)
  )

# Render with Fade-Out options
girafe(
  ggobj = p,
  options = list(
    # Keep the hovered item normal
    opts_hover(css = "stroke-width:1.5px; stroke:white;"), 
    # Fade everything else out (lower opacity)
    opts_hover_inv(css = "opacity:0.2; transition: opacity 0.3s;"),
    opts_tooltip(css = "background-color:white;color:black;padding:5px;border-radius:5px;font-family:sans-serif;"),
    opts_toolbar(saveaspng = FALSE)
  ),
  width_svg = 9,
  height_svg = 7
)
