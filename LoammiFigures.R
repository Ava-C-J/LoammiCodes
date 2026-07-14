# This code will generate figures to help visualize the results!
# 1. ranking of habitats based on index, and 
# 2. the protected portions of those same habitats.

# You will need four .csv files:
# 1. all_points_150m.csv
# 2. landcover_rankings.csv
# 3. Top_Results.csv
# 4. CLCStateTable.csv

# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)
library(gt)

# Set WD
# setwd("C:/Users/bluew/OneDrive/Desktop/LoammiCodes/Figures")
# setwd("~/Desktop/Ava Johnson Paper/Ava WD/")

# Read the CSV files
loammi_points <- read.csv("all_points_150m.csv")
landcover_rankings <- read.csv("landcover_rankings.csv")
prop_prot <- read.csv("Top_Results.csv")
landcover_lookup <- read.csv("CLCStateTable.csv")

# Inspect
head(loammi_points)
head(landcover_rankings)
head(prop_prot)
head(landcover_lookup)

# Join land cover names for Point_Landcover and Nearest_Natural
loammi_updated <- loammi_points %>%
  left_join(
    landcover_lookup %>%
      select(Value, NAME_STATE),
    by = c("Point_Landcover" = "Value")
  ) %>%
  left_join(
    landcover_lookup %>%
      select(Value, NAME_STATE) %>%
      rename(name_nearest = NAME_STATE),
    by = c("Nearest_Natural" = "Value")
  ) %>%
  rename(name_point = NAME_STATE) %>%
  relocate(name_point, .after = Point_Landcover) %>%
  relocate(name_nearest, .after = Nearest_Natural)

# Inspect
head(loammi_updated)
nrow(loammi_updated) # 117 correct

dplyr::n_distinct(loammi_updated$Point_Landcover)
dplyr::n_distinct(loammi_points$Point_Landcover)
# 15 land cover classes

sum(is.na(loammi_updated$Point_Landcover)) # 0 NA
sum(is.na(loammi_updated$name_point)) # 0 NA
sum(is.na(loammi_updated$Nearest_Natural)) # 0 NA
sum(is.na(loammi_updated$name_nearest)) # 0 NA

# Save updated CSV
# write_csv(loammi_updated, "Loammi_150m_with_landcover_names.csv")

dplyr::n_distinct(loammi_updated$Nearest_Natural) # top 8 natural habitats
sort(unique(loammi_updated$Nearest_Natural))

# Land cover codes to exclude (top 8)
exclude_classes <- sort(unique(loammi_updated$Nearest_Natural))
print(exclude_classes)

# Subset to remove these 8 land cover classes
loammi_subset <- loammi_updated %>%
  filter(!Point_Landcover %in% exclude_classes)

head(loammi_subset)
nrow(loammi_subset) # 18 points for which majority buffer is not top 8

# Check top 8 successfully removed
sum(loammi_subset$Point_Landcover %in% exclude_classes)
# No excluded classes remaining

# Check retained classes
loammi_subset %>%
  distinct(name_point) %>%
  arrange(name_point)

# Retain only points >75 m from the top 8 natural habitats
loammi_subset <- loammi_subset %>%
  filter(Distance_meters > 75)

# Inspect
head(loammi_subset)
nrow(loammi_subset) # 7 points remaining

# Check that all remaining distances are >75 m
min(loammi_subset$Distance_meters) # 76.15773

# Export filtered dataset
write_csv(
  loammi_subset,
  "Loammi_150m_subset.csv"
)

#-------------------------------------------------------------------------------
# Inspect points outside top 8 natural habitats
distances_filtered <- read.csv("Loammi_150m_subset.csv")
head(distances_filtered)

# Compute summary statistics
mean_dist <- mean(distances_filtered$Distance_meters, na.rm = TRUE)
median_dist <- median(distances_filtered$Distance_meters, na.rm = TRUE)
max_dist <- max(distances_filtered$Distance_meters, na.rm = TRUE)

print(mean_dist) # 211.5642
print(median_dist) # 192.3538
print(max_dist) # 344.093

#-------------------------------------------------------------------------------
# Plot for landcover rankings
# You will need to manually truncate your landcover_rankings file to chart only the
# top x habitats of your choice (i.e., top 5, top 10)

# Rename codes into land cover names
landcover_rankings <- landcover_rankings %>%
  left_join(
    landcover_lookup,
    by = c("landcover_type" = "Value")
  )
landcover_rankings$landcover_type <- landcover_rankings$NAME_STATE

# Inspect
head(landcover_rankings)
nrow(landcover_rankings) # 10

# Fill colors from the maps
habitat_colors <- c(
  "Dry Prairie"                       = "#a6cee3",
  "Palmetto Prairie"                  = "#1f78b4",
  "Mesic Flatwoods"                   = "#aaff00",
  "Cultural - Riverine"               = "#ffff00",
  "Scrubby Flatwoods"                 = "#fb9a99",
  "Cultural - Terrestrial"            = "#e31a1c",
  "Dome Swamp"                        = "#fdbf6f",
  "Prairies and Bogs"                 = "#ff7f00",
  "Isolated Freshwater Marsh"         = "#cab2d6",
  "Scrub"                             = "#6a3d9a"
)

# Create plot
index_plot <- ggplot(landcover_rankings, aes(x = reorder(landcover_type, -RankIndex), y = RankIndex)) +
  
  # Bar chart
  geom_col(
    aes(fill = landcover_type),
    color = "black"
  ) +
  scale_fill_manual(values = habitat_colors) +
  
  # X axis formatting
  scale_x_discrete(
    name = NULL,
    expand = c(0, 0)
  ) +
  
  # Y axis formatting
  scale_y_continuous(
    name = "Index Score",
    limits = c(0, 20),
    breaks = seq(0, 20, by = 4),
    expand = c(0, 0)
  ) +
  
  # Clean theme with black border
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "none"
  )

index_plot

# Save to working directory
ggsave(
  filename = "landcover_rankings.pdf",
  plot = index_plot,
  width = 10,
  height = 6,
  dpi = 300
)

#-------------------------------------------------------------------------------
# Plot for proportions protected

# Inspect
head(prop_prot)
nrow(prop_prot) # 11

# Remove NA row
prop_prot <- prop_prot %>%
  filter(!is.na(Landcover_Type))
nrow(prop_prot) 

# Remove NA column
prop_prot <- prop_prot %>%
  select(where(~ !any(is.na(.))))
head(prop_prot)

# Rename codes into land cover names
prop_prot <- prop_prot %>%
  left_join(
    landcover_lookup,
    by = c("Landcover_Type" = "Value")
  )
prop_prot$Landcover_Type <- prop_prot$NAME_STATE

# Inspect
head(prop_prot)
nrow(prop_prot) # 10

# Create plot
protect_plot <- ggplot(prop_prot, aes(x = reorder(Landcover_Type, -Proportion_Protected), y = Proportion_Protected)) +
  
  # Bar chart using precomputed counts
  geom_col(
    aes(fill = Landcover_Type),
    color = "black"
  ) +
  scale_fill_manual(values = habitat_colors) +
  
  # X axis formatting
  scale_x_discrete(
    name = NULL,
    expand = c(0, 0)
  ) +
  
  # Y axis formatting
  scale_y_continuous(
    name = "Proportion Protected",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = c(0, 0)
  ) +
  
  # Clean theme with black border
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "none"
  )

protect_plot

# Save to working directory
ggsave(
  filename = "Prop_Protected.pdf",
  plot = protect_plot,
  width = 10,
  height = 6,
  dpi = 300
)

#-------------------------------------------------------------------------------
# Combine plots

combined_plot <- index_plot + protect_plot +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(
      face = "bold",
      size = 18
    )
  )

combined_plot

# Save to working directory
ggsave(
  "Figure_2_Combined.pdf",
  combined_plot,
  width = 12,
  height = 6,
  dpi = 300
)

#-------------------------------------------------------------------------------
# Results table

# Create a table of the top-10 habitat types
results_table <- landcover_rankings %>%
  select(
    landcover_type,
    RankIndex,
    Proportion_Protected,
    Area_Acres
  ) %>%
  rename(
    `Land Cover Class` = landcover_type,
    `Index Value` = RankIndex,
    `Protected (%)` = Proportion_Protected,
    `Total Area (Acres)` = Area_Acres
  ) %>%
  gt() %>%
  fmt_number(
    columns = `Index Value`,
    decimals = 2
  ) %>%
  fmt_percent(
    columns = `Protected (%)`,
    decimals = 2
  ) %>%
  fmt_number(
    columns = `Total Area (Acres)`,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  cols_align(
    align = "left",
    columns = `Land Cover Class`
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Index Value`, `Protected (%)`, `Total Area (Acres)`)
  ) %>%
  cols_width(
    `Land Cover Class` ~ px(205),
    `Index Value` ~ px(130),
    `Protected (%)` ~ px(190),
    `Total Area (Acres)` ~ px(180)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) 

results_table

# Save as PNG
gtsave(
  results_table,
  "Results_Table.png"
)
