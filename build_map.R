library(tidyverse)
library(sf)
library(scales)
library(ggtext)
library(ggiraph)

# url_geo <-"https://api.dataforsyningen.dk/kommuner?format=geojson"
#
# download.file(url_geo, "data/kommune_geo_data.shp")

geo_data <- read_sf("data/kommune_geo_data.shp")

redlist <-
  readxl::read_excel("data/Revideret liste_2020_sjaeldne makroinvertebrater.xlsx", sheet = 1) %>%
  janitor::clean_names()

vanda <-
  read_delim("data/vanda_miljoedata-artsresultater.csv", delim = ";") %>%
  janitor::clean_names()

filtered_vanda <-
  vanda %>%
  filter(art_latin %in% redlist$videnskabeligt_navn &
    year(dmy_hms(dato)) >= 2013 & year(dmy_hms(dato)) <= 2023)

rare_count <-
  filtered_vanda %>%
  group_by(kommune) %>%
  summarise(unique_species_count = n_distinct(art_latin))

merged_data <-
  geo_data %>%
  left_join(rare_count, by = c("navn" = "kommune")) %>%
  mutate(
    unique_species_count = if_else(is.na(unique_species_count), 0, unique_species_count),
    is_zero = if_else(unique_species_count > 0, "Positive", "Zero"),
    log_n = log1p(unique_species_count)
  )

# Calculate breaks and labels based on the original count scale
original_breaks <- seq(0, max(merged_data$unique_species_count), 5)
log_transformed_breaks <- log1p(original_breaks)
colors <- c("0" = "firebrick", "0.01" = "white", "max" = "darkgreen") # Adjust values and colors as needed
values <- rescale(c(0, 0.01, max(merged_data$log_n)))

p <-
  merged_data %>%
  ggplot() +
  geom_sf(aes(fill = log_n)) +
  scale_fill_gradientn(
    colors = colors,
    values = values,
    breaks = log_transformed_breaks,
    labels = original_breaks
  ) +
  labs(
    title = "Heatmap of Rare Species by Municipality",
    subtitle = "Municipalities colored <span style='color:firebrick; font-weight: bold;'>red</span> represent a count of 0",
    fill = "Number of Species"
  ) +
  theme_minimal(
    base_size = 14
  ) +
  theme(
    text = element_text(family = "Zilla Slab"),
    legend.position = "bottom",
    plot.subtitle = element_markdown()
  ) +
  guides(
    fill = guide_colorsteps(
      title.position = "top",
      barwidth = unit(15, "cm")
    )
  )

p

# ggsave("figures/rare_species_heatmap.png", dpi = 300, height = 12, width = 12, bg = "white")


# INTERACTIVE

all_kommuner <- tibble(kommune = unique(merged_data$navn))

tooltip_labels <-
  filtered_vanda %>% 
  mutate(
    year = year(dmy_hms(dato))
  ) %>%
  count(kommune, art_latin, year) %>%
  group_by(kommune, art_latin) %>%
  reframe(
    n = n,
    years_sighted = toString(unique(year))
  ) %>%
  mutate(label = str_c(art_latin, " sightings in year(s): ", years_sighted)) %>%
  distinct(kommune, label) %>%
  group_by(kommune) %>%
  summarise(
    all_species = paste(label, collapse="\n"),
    .groups = 'drop'
  ) %>% 
    right_join(all_kommuner, by = "kommune") %>% 
    mutate(
      label = ifelse(is.na(all_species), "No sightings", all_species)
    )

p_interactive <-
  merged_data %>%
  left_join(tooltip_labels, by = c("navn" = "kommune")) %>% 
  ggplot() +
  geom_sf_interactive(aes(fill = log_n, tooltip = label, data_id = navn)) +
  scale_fill_gradientn(
    colors = colors,
    values = values,
    breaks = log_transformed_breaks,
    labels = original_breaks
  ) +
  labs(
    title = "Heatmap of Rare Species by Municipality",
    subtitle = "Municipalities colored <span style='color:firebrick; font-weight: bold;'>red</span> represent a count of 0",
    fill = "Number of Species"
  ) +
  ggthemes::theme_map(
    base_size = 14
  ) +
  theme(
    text = element_text(family = "Zilla Slab"),
    legend.position = "bottom",
    plot.subtitle = element_markdown()
  ) +
  guides(
    fill = guide_colorsteps(
      title.position = "top",
      barwidth = unit(10, "cm")
    )
  )

g_obj <- 
  girafe(ggobj = p_interactive,
         width_svg = 12,
         options = list(
           opts_tooltip(
             css = htmltools::css(
               background = "White",
               border = "2px solid black",
               font_weight = 600,
               font_size = 12
             )
           )
         ))

htmltools::save_html(g_obj, file = "figures/interactive_map.html")
