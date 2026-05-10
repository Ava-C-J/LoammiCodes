# This code will generate figures to help visualize the results!
# 4 plots: 1. Distribution of distances, 2. common nearest natural landcover types,
# 3. ranking of habitats based on index, and 4. the protected portions of those same habitats.
# You will need 3 .csv files, which you should have gotten from previous codes!
# 1. Buffer distances, 2. Land cover rankings, and 3. Proportions Protected of the top habitats.
# You will need an additional .csv table that matches landcover codes with their names,
# Unless you wish to keep the raster codes in your plots.

library(ggplot2)
library(dplyr)

setwd("path/to/datasets")

# Read the CSV file(s)
distances <- read.csv("all_points.csv")
landcover_rankings <- read.csv("landcover_rankings.csv")
prop_prot <- read.csv("Top_Results.csv")
landcover_lookup <- read.csv("CLCStateTable.csv")

# Filter the buffer distances - remove points that already fall within a top habitat
distances <- distances %>%
  dplyr::filter(
    !is.na(Point_Landcover),
    !is.na(Nearest_Natural),
    Point_Landcover != Nearest_Natural,
    !is.na(Distance_meters),
    Distance_meters != 0
  )

# Rename codes into landcover names
distances <- distances %>%
  left_join(
    landcover_lookup,
    by = c("Nearest_Natural" = "Value")
  )
distances <- distances %>%
  left_join(
    landcover_lookup,
    by = c("Point_Landcover" = "Value")
  )

distances$Nearest_Natural <- distances$NAME_STATE
distances$Point_Landcover <- distances$NAME_STATE

write.csv(distances, "distances_filtered.csv", row.names = FALSE)

distances_filtered<- read.csv("distances_filtered.csv")

# Compute summary statistics
mean_dist <- mean(distances_filtered$Distance_meters, na.rm = TRUE)
median_dist <- median(distances_filtered$Distance_meters, na.rm = TRUE)

# Plot for distance distribution
# Create plot
p <- ggplot(distances_filtered, aes(x = Distance_meters)) +
  
  # Histogram (grey bars)
  geom_histogram(
    binwidth = 10,
    fill = "grey",
    color = "black",
    boundary = 0
  ) +
  
  # Mean line
  geom_vline(
    aes(xintercept = mean_dist, linetype = "Mean"),
    color = "black",
    size = 1
  ) +
  
  # Median line
  geom_vline(
    aes(xintercept = median_dist, linetype = "Median"),
    color = "black",
    size = 1
  ) +
  
  # Linetype legend
  scale_linetype_manual(
    name = "Statistics",
    values = c("Mean" = "dashed", "Median" = "dotted")
  ) +
  
  # X axis formatting
  scale_x_continuous(
    name = "Distance (m)",
    limits = c(0, 400),
    breaks = seq(0, 400, by = 40),
    expand = c(0, 0)
  ) +
  
  # Y axis formatting
  scale_y_continuous(
    name = "Count",
    limits = c(0, 4),
    breaks = seq(0, 4, by = 1),
    expand = c(0, 0)
  ) +
  
  # Clean theme with black border
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right"
  )

# Save to working directory
ggsave(
  filename = "distance_distribution_buffer.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)

#-----------------------------------------------------------------------------

# Natural landcover distance ranking
landcover_counts <- distances_filtered %>%
  count(NAME_STATE.x) %>%
  arrange(desc(n))

# Plot for nearest habitat types
# Create plot
p <- ggplot(landcover_counts, aes(x = reorder(NAME_STATE.x, -n), y = n)) +
  
  # Bar chart using precomputed counts
  geom_col(
    fill = "grey",
    color = "black"
  ) +
  
  # X axis formatting
  scale_x_discrete(
    name = "Nearest Natural Landcover Type",
    expand = c(0, 0)
  ) +
  
  # Y axis formatting
  scale_y_continuous(
    name = "Count",
    limits = c(0, 20),
    breaks = seq(0, 20, by = 5),
    expand = c(0, 0)
  ) +
  
  # Clean theme with black border
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right"
  )

# Save to working directory
ggsave(
  filename = "common_landcovers_buffer.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)

#-----------------------------------------------------------------------------

# Plot for landcover rankings
# You will need to manually truncate your landcover_rankings file to chart only the
# top x habitats of your choice (i.e., top 5, top 10)
# You will also need to apply a multiplier to the RankIndex column so the values are not so tiny,
# and then modify the y axis formatting to match if needed.

# Rename codes into landcover names
landcover_rankings <- landcover_rankings %>%
  left_join(
    landcover_lookup,
    by = c("landcover_type" = "Value")
  )
landcover_rankings$landcover_type <- landcover_rankings$NAME_STATE

# Create plot
p <- ggplot(landcover_rankings, aes(x = reorder(landcover_type, -RankIndex), y = RankIndex)) +
  
  # Bar chart using precomputed counts
  geom_col(
    fill = "grey",
    color = "black"
  ) +
  
  # X axis formatting
  scale_x_discrete(
    name = "Landcover Type",
    expand = c(0, 0)
  ) +
  
  # Y axis formatting
  scale_y_continuous(
    name = "Index Ranking",
    limits = c(0, 400),
    breaks = seq(0, 400, by = 50),
    expand = c(0, 0)
  ) +
  
  # Clean theme with black border
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right"
  )

# Save to working directory
ggsave(
  filename = "landcover_rankings.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)

#-----------------------------------------------------------------------------

# Plot for proportions protected

# Rename codes into landcover names
prop_prot <- prop_prot %>%
  left_join(
    landcover_lookup,
    by = c("Landcover_Type" = "Value")
  )
prop_prot$Landcover_Type <- prop_prot$NAME_STATE

# Create plot
p <- ggplot(prop_prot, aes(x = reorder(Landcover_Type, -Proportion_Protected), y = Proportion_Protected)) +
  
  # Bar chart using precomputed counts
  geom_col(
    fill = "grey",
    color = "black"
  ) +
  
  # X axis formatting
  scale_x_discrete(
    name = "Landcover Type",
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
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right"
  )

# Save to working directory
ggsave(
  filename = "Prop_Protected.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)

