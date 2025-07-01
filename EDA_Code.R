# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(corrplot)

# Read the data
df <- read_excel("C:/Users/mm23g24.SOTON/Downloads/eda_data.xlsx")

# Extract Year from 'Year...Month'
df$Year <- as.integer(sub("/.*", "", df$`Year...Month`))

# Grouping by 'Mapped.Event.Categories' and summing 'Deaths'
category_deaths <- df %>%
  group_by(Mapped.Event.Categories) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>%
  arrange(desc(Deaths))

# Calculate percentage labels
category_deaths <- category_deaths %>%
  mutate(Percentage = round((Deaths / sum(Deaths)) * 100, 1))

# Define a visually pleasing color palette
pie_colors <- c("#1E90FF", "#32CD32", "#00FFFF", "#FFA500", "#008080", "pink")

# Creating a pie chart using ggplot2
ggplot(category_deaths, aes(x = "", y = Deaths, fill = Mapped.Event.Categories)) +
  geom_bar(width = 0.5, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = pie_colors) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5), size = 3.5, color = "black", fontface = "bold") +
  labs(title = "Percentage of Total Deaths by Event Category", fill = "Event Categories") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#333333"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Create yearly summary data
yearly_summary <- df %>%
  group_by(Year) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE))

# Yearly trend of deaths with improved styling
ggplot(yearly_summary, aes(x = Year, y = Deaths)) +
  geom_line(color = "#006d5b", size = 0.8) +
  geom_point(color = "orange", size = 2) +
  labs(title = "Yearly Trend of Deaths", 
       x = "Year", 
       y = "Number of Deaths") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Create country summary data
country_summary <- df %>%
  group_by(Country) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE))

# Total deaths per country with enhanced bar chart
ggplot(country_summary, aes(x = reorder(Country, -Deaths), y = Deaths)) +
  geom_bar(stat = "identity", fill = "#006d5b", alpha = 0.7) +
  labs(title = "Total Deaths per Country", 
       x = "Country", 
       y = "Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Filter the data to include only years from 1980 onwards
df_filtered <- df %>% filter(Year >= 1980)

# Grouping by 'Year' and 'Mapped.Event.Categories' to sum 'Deaths', 'Injured', and 'Damages.in.crops.Ha.'
category_deaths <- df_filtered %>%
  group_by(Year, Mapped.Event.Categories) %>%
  summarise(Total_Deaths = sum(Deaths, na.rm = TRUE))

category_injuries <- df_filtered %>%
  group_by(Year, Mapped.Event.Categories) %>%
  summarise(Total_Injured = sum(Injured, na.rm = TRUE))

category_crops_damage <- df_filtered %>%
  group_by(Year, Mapped.Event.Categories) %>%
  summarise(Total_Crop_Damages = sum(Damages.in.crops.Ha., na.rm = TRUE))

# Identifying the most frequent category for each year based on 'Deaths'
most_death_categories <- category_deaths %>%
  group_by(Year) %>%
  filter(Total_Deaths == max(Total_Deaths)) %>%
  ungroup()

# Identifying the most frequent category for each year based on 'Injured'
most_injured_categories <- category_injuries %>%
  group_by(Year) %>%
  filter(Total_Injured == max(Total_Injured)) %>%
  ungroup()

# Identifying the most frequent category for each year based on 'Damages.in.crops.Ha.'
most_crops_damage_categories <- category_crops_damage %>%
  group_by(Year) %>%
  filter(Total_Crop_Damages == max(Total_Crop_Damages)) %>%
  ungroup()

# Plotting the correlation heatmap (Using selected numeric columns)
# Encoding categorical variables (like 'Country') to numeric if required
df_encoded <- df %>%
  mutate(Country = as.numeric(factor(Country)))

# Select columns of interest for correlation calculation
selected_columns <- c('Deaths', 'Losses..Local', 'Country', 'gdp_per_capita', 
                      'infrastructure', 'Education', 'Health.sector', 
                      'Water.supply', 'Communications', 'Transportation', 
                      'Power.and.Energy', 'Education.centers', 'Hospitals')

# Calculate the correlation matrix for the selected columns
correlation_matrix <- cor(df_encoded[, selected_columns], use = "complete.obs")

# Plotting the heatmap for correlations
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", 
         tl.srt = 45, title = "Correlation Heatmap", 
         mar = c(0,0,1,0), addCoef.col = "orange",number.cex = 0.7,number.font = 3)

# Define a custom color palette
custom_colors <- c("orange", "#006d5b", "#FF6347", "#FFD700","cyan")  # Add two more bright colors

# Plotting the most frequent event categories by injuries per year with properly formatted y-axis values
ggplot(most_injured_categories, aes(x = Year, y = Total_Injured, color = Mapped.Event.Categories)) +
  geom_line() +
  geom_point() +
  labs(title = "Year-wise Most Frequent Event Categories by Injuries", 
       x = "Year", 
       y = "Total Injuries") +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +  # Format y-axis values
  scale_color_manual(values = custom_colors) +  # Applying custom colors
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95), 
        legend.justification = c(0, 1),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8))

# Plotting the most frequent event categories by crop damages per year with properly formatted y-axis values
ggplot(most_crops_damage_categories, aes(x = Year, y = Total_Crop_Damages, color = Mapped.Event.Categories)) +
  geom_line() +
  geom_point() +
  labs(title = "Year-wise Most Frequent Event Categories by Crop Damages (Ha)", 
       x = "Year", 
       y = "Total Crop Damages (Ha)") +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +  # Format y-axis values
  scale_color_manual(values = custom_colors) +  # Applying custom colors
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95), 
        legend.justification = c(0, 1),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8))
