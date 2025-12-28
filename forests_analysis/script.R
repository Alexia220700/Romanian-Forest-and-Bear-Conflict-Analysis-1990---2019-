# ===========================================================================
# ROMANIAN FOREST AND BEAR CONFLICT ANALYSIS - COMPLETE SCRIPT
# ===========================================================================

cat("Loading required libraries...\n")
library(dplyr)
library(tidyr)
library(maps)
library(ggplot2)

# ===========================================================================
# SECTION 1: DATA LOADING AND PREPROCESSING
# ===========================================================================

cat("\n=== SECTION 1: LOADING AND PREPROCESSING DATA ===\n")

# Load forest data
forests <- read.csv("forest_surfaces_romania_en_1990_2019.csv")
forests$Category <- NULL
forests <- forests %>% rename(County = Region)

# Region mapping
county_to_region <- data.frame(
  County = c(
    "Alba", "Bihor", "Bistrita-Nasaud", "Brasov", "Bucuresti", "Caras-Severin", "Cluj",
    "Covasna", "Harghita", "Hunedoara", "Maramures", "Mures", "Salaj",
    "Satu Mare", "Sibiu", "Suceava", "Arad", "Timis", "Botosani", "Iasi",
    "Neamt", "Vaslui", "Bacau", "Vrancea", "Galati", "Arges", "Braila",
    "Buzau", "Calarasi", "Constanta", "Dambovita", "Dolj", "Giurgiu",
    "Gorj", "Ialomita", "Ilfov", "Mehedinti", "Olt", "Prahova", "Teleorman",
    "Tulcea", "Valcea"
  ),
  Region = c(
    "Transilvania", "Crișana", "Transilvania", "Transilvania", "Oltenia", "Banat", "Transilvania",
    "Transilvania", "Transilvania", "Transilvania", "Maramureș", "Transilvania", "Transilvania",
    "Maramureș", "Transilvania", "Bucovina", "Crișana", "Banat", "Moldova", "Moldova",
    "Moldova", "Moldova", "Moldova", "Moldova", "Moldova", "Muntenia", "Muntenia",
    "Muntenia", "Muntenia", "Dobrogea", "Muntenia", "Oltenia", "Muntenia",
    "Oltenia", "Muntenia", "Muntenia", "Oltenia", "Oltenia", "Muntenia", "Muntenia",
    "Dobrogea", "Oltenia"
  ),
  stringsAsFactors = FALSE
)

# Map counties to regions
forests_mapped <- forests %>%
  left_join(county_to_region, by = "County") %>%
  filter(!is.na(Region))

# Summarize by Year and Region
forests_mapped <- forests_mapped %>%
  group_by(Year, Region) %>%
  summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop")

# Prepare data for 1990 and 2019 maps
forests_1990_area <- forests_mapped %>%
  filter(Year == 1990) %>%
  group_by(Region) %>%
  summarize(Total_Area = sum(Value), .groups = "drop") %>%
  arrange(desc(Total_Area))

forests_2019_area <- forests_mapped %>%
  filter(Year == 2019) %>%
  group_by(Region) %>%
  summarize(Total_Area = sum(Value), .groups = "drop") %>%
  arrange(desc(Total_Area))

print("1990 Forest Area by Region:")
print(forests_1990_area)

print("2019 Forest Area by Region:")
print(forests_2019_area)

cat("✓ Data preprocessing complete\n")

# ===========================================================================
# SECTION 2: FOREST GROWTH ANALYSIS
# ===========================================================================

cat("\n=== SECTION 2: FOREST GROWTH ANALYSIS ===\n")

# Create growth comparison (1990 vs 2019)
growth_comparison <- forests_mapped %>%
  filter(Year %in% c(1990, 2019)) %>%
  pivot_wider(names_from = Year, values_from = Value) %>%
  mutate(
    Absolute_Change = `2019` - `1990`,
    Percent_Change = ((`2019` - `1990`) / `1990`) * 100,
    Annual_Growth_Rate = (((`2019` / `1990`)^(1/29)) - 1) * 100
  )

# Generate summary statistics
growth_summary <- growth_comparison %>%
  summarize(
    National_Avg_Growth = mean(Annual_Growth_Rate),
    Highest_Growth = max(Annual_Growth_Rate),
    Lowest_Growth = min(Annual_Growth_Rate),
    Most_Stable = Region[which.min(abs(Annual_Growth_Rate - mean(Annual_Growth_Rate)))],
    Most_Volatile = Region[which.max(abs(Annual_Growth_Rate - mean(Annual_Growth_Rate)))]
  )

print("Growth Summary Statistics:")
print(growth_summary)

# ANOVA analysis
forests_annual_growth <- forests_mapped %>%
  group_by(Region) %>%
  mutate(Annual_Growth = (Value / lag(Value) - 1) * 100) %>%
  filter(!is.na(Annual_Growth))

anova_test <- aov(Annual_Growth ~ Region, data = forests_annual_growth)
print("ANOVA Results:")
print(summary(anova_test))

cat("✓ Forest growth analysis complete\n")

# ===========================================================================
# SECTION 3: LIVESTOCK DAMAGE ANALYSIS
# ===========================================================================

cat("\n=== SECTION 3: LIVESTOCK DAMAGE ANALYSIS ===\n")

# Load livestock data
cows <- read.csv("cows_summary_by_year_region.csv")
sheep <- read.csv("sheep_summary_by_year_region.csv")

# Merge all datasets
forests_and_bears <- forests_mapped %>%
  left_join(cows, by = c("Year", "Region")) %>%
  left_join(sheep, by = c("Year", "Region")) %>%
  replace_na(list(
    Total_Cows_Damage = 0,
    Total_Bear_Sightings = 0,
    Total_Sheep_Damage = 0,
    Bear_Sightings_Sheep = 0
  )) %>%
  group_by(Year, Region) %>%
  summarize(
    Forest_Area = sum(Value, na.rm = TRUE),
    Total_Damage = sum(Total_Cows_Damage, na.rm = TRUE) +
      sum(Total_Sheep_Damage, na.rm = TRUE),
    Total_Bear_Sightings = sum(Total_Bear_Sightings, na.rm = TRUE) +
      sum(Bear_Sightings_Sheep, na.rm = TRUE),
    .groups = "drop"
  )

# Identify worst-hit regions
worst_regions <- forests_and_bears %>%
  group_by(Region) %>%
  summarize(
    Total_Damage = sum(Total_Damage, na.rm = TRUE),
    Total_Bear_Sightings = sum(Total_Bear_Sightings, na.rm = TRUE),
    Avg_Forest_Area = mean(Forest_Area, na.rm = TRUE),
    Damage_per_Hectare = Total_Damage / Avg_Forest_Area,
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Damage))

print("Worst-hit regions by total damage:")
print(worst_regions)

cat("✓ Livestock damage analysis complete\n")

# ===========================================================================
# SECTION 4: VISUALIZATION
# ===========================================================================

cat("\n=== SECTION 4: VISUALIZATION ===\n")

# Load map data for Romania
world_map <- map_data("world")
romania <- world_map %>% filter(region == "Romania")

# ===========================================================================
# SUB-SECTION 4.1: FOREST MAP VISUALIZATIONS
# ===========================================================================

cat("Creating forest area maps...\n")

# Function to create forest area maps
create_forest_map <- function(data, year_label) {
  
  # Helper function to safely get region value
  get_region_value <- function(region_name) {
    value <- data$Total_Area[data$Region == region_name]
    if (length(value) == 0) {
      return("N/A")
    } else {
      return(paste0(round(value, 0), "k ha"))
    }
  }
  
  ggplot() +
    geom_polygon(data = romania, aes(x = long, y = lat, group = group),
                 fill = "beige", color = "white") +
    # Transilvania
    annotate("text", x = 24.6, y = 46.5, label = "Transilvania",
             size = 4, fontface = "bold", color = "darkred") +
    annotate("text", x = 24.6, y = 46.2,
             label = get_region_value("Transilvania"),
             size = 4, color = "darkgreen") +
    # Maramureș
    annotate("text", x = 23.5, y = 47.7, label = "Maramureș",
             size = 3.5, color = "darkgreen", fontface = "bold") +
    annotate("text", x = 23.5, y = 47.4,
             label = get_region_value("Maramureș"),
             size = 3.5, color = "darkgreen") +
    # Moldova
    annotate("text", x = 27.0, y = 47.0, label = "Moldova",
             size = 3.5, fontface = "bold", color = "darkgreen") +
    annotate("text", x = 27.0, y = 46.7,
             label = get_region_value("Moldova"),
             size = 3.5, color = "darkgreen") +
    # Bucovina
    annotate("text", x = 25.8, y = 47.7, label = "Bucovina",
             size = 3.5, fontface = "bold", color = "darkgreen") +
    annotate("text", x = 25.8, y = 47.4,
             label = get_region_value("Bucovina"),
             size = 3.5, color = "darkgreen") +
    # Muntenia
    annotate("text", x = 26.2, y = 44.9, label = "Muntenia",
             size = 3.5, fontface = "bold", color = "darkred") +
    annotate("text", x = 26.2, y = 44.6,
             label = get_region_value("Muntenia"),
             size = 3.5, color = "darkgreen") +
    # Oltenia
    annotate("text", x = 23.6, y = 44.7, label = "Oltenia",
             size = 3.5, fontface = "bold", color = "darkred") +
    annotate("text", x = 23.7, y = 44.4,
             label = get_region_value("Oltenia"),
             size = 3.5, color = "darkgreen") +
    # Banat
    annotate("text", x = 21.5, y = 45.7, label = "Banat",
             size = 3.5, fontface = "bold", color = "darkgreen") +
    annotate("text", x = 21.6, y = 45.4,
             label = get_region_value("Banat"),
             size = 3.5, color = "darkgreen") +
    # Dobrogea
    annotate("text", x = 28.5, y = 45.1, label = "Dobrogea",
             size = 3.5, fontface = "bold", color = "darkgreen") +
    annotate("text", x = 28.5, y = 44.8,
             label = get_region_value("Dobrogea"),
             size = 3.5, color = "darkgreen") +
    # Crișana
    annotate("text", x = 22.3, y = 46.8, label = "Crișana",
             size = 3.5, fontface = "bold", color = "darkgreen") +
    annotate("text", x = 22.3, y = 46.5,
             label = get_region_value("Crișana"),
             size = 3.5, color = "darkgreen") +
    coord_fixed(1.3) +
    labs(title = paste0("Forest Area by Region in Romania (", year_label, ")"),
         subtitle = "Area in thousands of hectares (k ha)") +
    theme_void()
}

# Create and display forest maps
map_1990 <- create_forest_map(forests_1990_area, "1990")
map_2019 <- create_forest_map(forests_2019_area, "2019")

cat("Displaying 1990 forest map...\n")
print(map_1990)

cat("Displaying 2019 forest map...\n")
print(map_2019)

# ===========================================================================
# SUB-SECTION 4.2: BAR CHART VISUALIZATIONS
# ===========================================================================

cat("Creating statistical charts...\n")

# 1. Forest growth bar chart
p1 <- ggplot(growth_comparison, aes(x = reorder(Region, Annual_Growth_Rate),
                                    y = Annual_Growth_Rate,
                                    fill = Annual_Growth_Rate)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", Annual_Growth_Rate)),
            hjust = ifelse(growth_comparison$Percent_Change >= 0, -0.2, 1.2),
            size = 3) +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green",
                       midpoint = mean(growth_comparison$Annual_Growth_Rate),
                       name = "Growth Rate (%)") +
  coord_flip() +
  labs(
    title = "Annual Forest Growth Rate by Region (1990-2019)",
    subtitle = paste("National Average:",
                     sprintf("%.2f%%", mean(growth_comparison$Annual_Growth_Rate))),
    x = "Region",
    y = "Annual Growth Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

cat("Displaying forest growth bar chart...\n")
print(p1)

# 2. Damage vs Sightings bar chart
comparison_data <- worst_regions %>%
  select(Region, Total_Damage, Total_Bear_Sightings) %>%
  pivot_longer(
    cols = c(Total_Damage, Total_Bear_Sightings),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = case_when(
      Metric == "Total_Damage" ~ "Total Damage",
      Metric == "Total_Bear_Sightings" ~ "Bear Sightings",
      TRUE ~ Metric
    )
  )

p2 <- ggplot(comparison_data, aes(x = reorder(Region, Value),
                                  y = Value,
                                  fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = format(round(Value, 0), big.mark = ",")),
            position = position_dodge(width = 0.7),
            hjust = -0.2, size = 3, color = "black") +
  scale_fill_manual(values = c("Total Damage" = "#E41A1C",
                               "Bear Sightings" = "#377EB8")) +
  coord_flip() +
  labs(
    title = "Animal Damage vs Bear Sightings by Region",
    subtitle = "Comparison of total damage and bear sightings",
    x = "Region",
    y = "Count",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

cat("Displaying damage vs sightings bar chart...\n")
print(p2)

# ===========================================================================
# SUB-SECTION 4.3: ADDITIONAL VISUALIZATIONS
# ===========================================================================

cat("Creating additional visualizations...\n")

# 3. Forest Area Change Visualization
p3 <- ggplot(growth_comparison, aes(x = reorder(Region, Percent_Change),
                                    y = Percent_Change,
                                    fill = Percent_Change)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%+.1f%%", Percent_Change)),
            hjust = ifelse(growth_comparison$Percent_Change >= 0, -0.2, 1.2),
            size = 3.5) +
  scale_fill_gradient2(low = "red", mid = "white", high = "green",
                       midpoint = 0, name = "Change (%)") +
  coord_flip() +
  labs(
    title = "Forest Area Change by Region (1990-2019)",
    subtitle = "Percentage change from 1990 to 2019",
    x = "Region",
    y = "Percent Change (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

print(p3)


cat("✓ All visualizations complete\n")

# ===========================================================================
# SECTION 5: REPORT GENERATION (Console Output)
# ===========================================================================

cat("\n=== SECTION 5: GENERATING REPORTS ===\n")

# Create summary report in console
report_content <- capture.output({
  cat("\n" , strrep("=", 60), "\n", sep="")
  cat("ANALYSIS SUMMARY REPORT\n")
  cat(strrep("=", 60), "\n\n")
  
  cat("1. FOREST GROWTH (1990-2019)\n")
  cat("National Average:", sprintf("%.2f%%", mean(growth_comparison$Annual_Growth_Rate)), "\n")
  cat("Highest Growth:", growth_comparison$Region[which.max(growth_comparison$Annual_Growth_Rate)],
      " (", sprintf("%.2f%%", max(growth_comparison$Annual_Growth_Rate)), ")\n")
  cat("Lowest Growth:", growth_comparison$Region[which.min(growth_comparison$Annual_Growth_Rate)],
      " (", sprintf("%.2f%%", min(growth_comparison$Annual_Growth_Rate)), ")\n\n")
  
  cat("2. ANIMAL DAMAGE\n")
  cat("Total Damage:", format(sum(worst_regions$Total_Damage), big.mark = ","), "\n")
  cat("Total Bear Sightings:", format(sum(worst_regions$Total_Bear_Sightings), big.mark = ","), "\n\n")
  
  cat("3. REGIONAL HOTSPOTS (Top 3)\n")
  for(i in 1:3) {
    cat(" ", i, ". ", worst_regions$Region[i], ": ",
        format(worst_regions$Total_Damage[i], big.mark = ","),
        " incidents (", round(worst_regions$Damage_per_Hectare[i], 3), " per hectare)\n", sep = "")
  }
  cat("\n")
  
  cat("4. FOREST AREA COMPARISON (1990 vs 2019)\n")
  largest_1990 <- forests_1990_area %>% arrange(desc(Total_Area)) %>% slice(1)
  largest_2019 <- forests_2019_area %>% arrange(desc(Total_Area)) %>% slice(1)
  cat("Largest forest area in 1990: ", largest_1990$Region, " (", largest_1990$Total_Area, "k ha)\n", sep = "")
  cat("Largest forest area in 2019: ", largest_2019$Region, " (", largest_2019$Total_Area, "k ha)\n\n")
  
  cat("5. POLICY RECOMMENDATIONS\n")
  cat("High Priority (Top 3 damage): ", paste(worst_regions$Region[1:3], collapse = ", "), "\n")
  cat("Medium Priority (Next 3): ", paste(worst_regions$Region[4:6], collapse = ", "), "\n")
  cat("Low Priority (Remaining): ", paste(worst_regions$Region[7:9], collapse = ", "), "\n\n")
  
  cat("6. VISUALIZATIONS GENERATED\n")
  cat("• Forest area maps for 1990 and 2019\n")
  cat("• Annual forest growth rate bar chart\n")
  cat("• Animal damage vs bear sightings comparison\n")
  cat("• Forest area change visualization\n")
  cat("• Damage per hectare intensity chart\n\n")
  
  cat(strrep("=", 60), "\n")
  cat("ANALYSIS COMPLETED SUCCESSFULLY\n")
  cat(strrep("=", 60), "\n")
  
  cat("\n✓ All analysis complete!\n")
  cat("\nSUMMARY OF OUTPUTS:\n")
  cat("1. Statistical Analysis: Growth rates, ANOVA, damage statistics\n")
  cat("2. Geospatial Visualizations: Forest area maps for 1990 and 2019\n")
  cat("3. Statistical Charts: Bar charts showing trends and comparisons\n")
  cat("4. Comprehensive Report: Regional analysis and policy recommendations\n")
})
print(report_content) # Print the report to console as before

# ===========================================================================
# SECTION 6: SETUP FOLDERS
# ===========================================================================

cat("\n=== SECTION 6: SETTING UP OUTPUT FOLDERS ===\n")

# Define base and subdirectories
base_dir <- "outputs"
data_dir <- paste0(base_dir, "/data")
visuals_dir <- paste0(base_dir, "/visuals")
reports_dir <- paste0(base_dir, "/reports")

# Create directories recursively (if they don't exist)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(visuals_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

cat("✓ Output folders created: 'outputs/data', 'outputs/visuals', 'outputs/reports'\n")

# ===========================================================================
# SECTION 7: SAVING STATISTICAL OUTPUTS (DATA)
# ===========================================================================

cat("\n=== SECTION 7: SAVING STATISTICAL OUTPUTS ===\n")

# Save tables as CSV files
write.csv(forests_1990_area, paste0(data_dir, "/forest_area_1990.csv"), row.names = FALSE)
write.csv(forests_2019_area, paste0(data_dir, "/forest_area_2019.csv"), row.names = FALSE)
write.csv(growth_comparison, paste0(data_dir, "/regional_growth_comparison.csv"), row.names = FALSE)
write.csv(growth_summary, paste0(data_dir, "/national_growth_summary.csv"), row.names = FALSE)
write.csv(worst_regions, paste0(data_dir, "/bear_conflict_worst_regions.csv"), row.names = FALSE)

# Save ANOVA results (captured from print output)
capture.output(summary(anova_test), file = paste0(data_dir, "/anova_results.txt"))

cat("✓ Statistical tables and ANOVA results saved to 'outputs/data/'\n")

# ===========================================================================
# SECTION 8: SAVING VISUALIZATIONS AND REPORTS
# ===========================================================================

cat("\n=== SECTION 8: SAVING VISUALIZATIONS AND REPORTS ===\n")

# Save Maps
ggsave(filename = paste0(visuals_dir, "/map_forest_area_1990.png"), plot = map_1990, 
       width = 8, height = 7, units = "in", dpi = 300)
ggsave(filename = paste0(visuals_dir, "/map_forest_area_2019.png"), plot = map_2019, 
       width = 8, height = 7, units = "in", dpi = 300)

# Save Bar Charts
ggsave(filename = paste0(visuals_dir, "/chart_annual_growth_rate.png"), plot = p1, 
       width = 10, height = 6, units = "in", dpi = 300)
ggsave(filename = paste0(visuals_dir, "/chart_damage_vs_sightings.png"), plot = p2, 
       width = 10, height = 6, units = "in", dpi = 300)
ggsave(filename = paste0(visuals_dir, "/chart_percent_change.png"), plot = p3, 
       width = 10, height = 6, units = "in", dpi = 300)

# Write the captured report content to a text file in the reports directory
writeLines(report_content, paste0(reports_dir, "/analysis_summary_report.txt"))

cat("✓ All visualizations saved to 'outputs/visuals/'\n")
cat("✓ Summary report saved to 'outputs/reports/analysis_summary_report.txt'\n")
cat("\nFinal script execution complete. Check the 'outputs' folder for results.\n")