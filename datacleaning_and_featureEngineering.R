#install the packages
# install.packages("readxl")    
# install.packages("dplyr")    
# install.packages("purrr")    
# install.packages("corrplot")  
# install.packages("readr")     
# install.packages("ggplot2")   
# install.packages("tidyr")     
# install.packages("caret")     
# install.packages("openxlsx") 

#load the required libraries
library(readxl)   
library(dplyr)   
library(purrr)   
library(corrplot)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(openxlsx)

# DATA CLEANING AND PROCESSING

# Define list of countries
countries <- c(
  "Dominican Republic", "Guatemala", "Mexico", "Pakistan", "Senegal", 
  "Sri Lanka", "Tunisia", "Türkiye", "Uganda", "United Republic of Tanzania", 
  "Vietnam", "Yemen", "Zambia"
)

# Function to read and process data for a single country
country_data <- function(country) {
  # Read the Excel file for the specified country, skipping the first row
  df <- read_excel(paste0(country, ".xls"), skip = 1)
  
  # Add a column for the country name
  df$Country <- country
  
  return(df)
}

# Load and combine data for all countries
df_final <- map_dfr(countries, country_data)
df_final

# Remove rows where the 'Year - Month' column contains "TOTAL"
df_final <- df_final %>%
  filter(`Year - Month` != "TOTAL")

# Transform the 'Event' column to uppercase
df_final <- df_final %>%
  mutate(Event = toupper(Event))

# Read cleaned event data
event_cleaned_df <- read_excel("Event_Cleaned.xlsx")

# Left join to add cleaned event names and categories
df_final <- df_final %>%
  left_join(
    event_cleaned_df[, c("Event", "Updated Event Name", "Mapped Event Categories")], 
    by = "Event"
  )

# Remove rows where all columns from 4 to 14 are NA
df_final <- df_final %>%
  filter(!apply(df_final[, 4:14], 1, function(x) all(is.na(x))))

# Impute missing values in 'Duration (d)' using the median
df_final <- df_final %>%
  left_join(
    df_final %>%
      group_by(`Updated Event Name`) %>%
      summarise(
        median_duration = median(`Duration (d)`, na.rm = TRUE), 
        .groups = "drop"
      ),
    by = "Updated Event Name"
  ) %>%
  mutate(
    `Duration (d)` = ifelse(is.na(`Duration (d)`), median_duration, `Duration (d)`),
    median_duration = NULL
  )

# Impute missing values in specific columns with 0
columns_to_impute <- c(
  'Deaths', 'Injured', 'Missing', 'Houses Destroyed', 'Houses Damaged',
  'Directly affected', 'Indirectly Affected', 'Relocated', 'Evacuated',
  'Losses $USD', 'Damages in crops Ha.', 'Education', 'Agriculture',
  'Health sector', 'Water supply', 'Communications', 'Transportation',
  'Power and Energy', 'Relief', 'Education centers', 'Hospitals'
)

df_final <- df_final %>%
  mutate(across(all_of(columns_to_impute), ~ replace(., is.na(.), 0)))

# Impute missing values in 'Losses $Local' using the median by country
df_final <- df_final %>%
  left_join(
    df_final %>%
      group_by(Country) %>%
      summarise(
        median_losses = median(`Losses $Local`, na.rm = TRUE), 
        .groups = "drop"
      ),
    by = "Country"
  ) %>%
  mutate(
    `Losses $Local` = ifelse(is.na(`Losses $Local`), median_losses, `Losses $Local`),
    median_losses = NULL
  )

# Save the cleaned dataset to a CSV file
write.csv(df_final, file = "df_clean.csv", row.names = FALSE)

# Read the cleaned dataset into a dataframe
df <- read.csv("df_clean.csv")
head(df)
str(df)

#generating summary statistics for the dataset
paste0("No of columns in the dataset = ",ncol(df), sep=" ")
paste0("No of rows in the dataset = ",nrow(df), sep=" ")

# Calculate and display the percentage of NA values in each column
na_perc <- round(colSums(is.na(df)) / nrow(df) * 100,2)
na_perc_df <- data.frame(NA_Percentage = na_perc)
print("Percentage of NA values in each column:")

# Visualize the correlation matrix as a heatmap with circles
cor_matrix <- cor(select_if(df, is.numeric), use = "pairwise.complete.obs")
corrplot::corrplot(cor_matrix, method = "circle", type = "lower", tl.cex = 0.7)


#FEATURE ENGINEERING
#make a copy of dataset to save the original
dfcopy <- data.frame(df)

# OUTLIER HANDLING
#Function to cap outliers based on the upper 90% quantile
# upper 10% (max values) instead of IQR method (which takes 25%)
cap_outlier <- function(col){
  # 10th percentile
  Q1 <- quantile(col, 0.10) 
  # 90th percentile
  Q3 <- quantile(col, 0.90)
  IQR_value <- Q3 - Q1
  # lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  capped_col <- ifelse(col > upper_bound, upper_bound, col)
  return(capped_col)
}

# Apply outlier handling to specific numeric columns
outlier_apply_columns <- c("Deaths", "Duration..d.")
dfcopy[outlier_apply_columns] <- lapply(dfcopy[outlier_apply_columns], function(col) {cap_outlier(col)})


#TIME FEATURE ENGINEERING
# a) DATE COLUMN TRANSFORMATION
# Split the "Year_Month" column into "Year" and "Month"
dfcopy$Year <- as.integer(sub("/.*", "", dfcopy$Year...Month))
dfcopy$Month <- as.integer(sub(".*/", "", dfcopy$Year...Month))

# b) DURATION COLUMN TRANSFORMATION
summary(dfcopy$Duration..d., na.rm=TRUE)
dfcopy$duration_in_yrs = round(dfcopy$Duration..d./365,3)
sapply(dfcopy, class)

#CHECK - IF duration_in_years + Disaster Year > Current Year
dfcopy$duration_check = ifelse(as.integer(dfcopy$duration_in_yrs+dfcopy$Year) > as.integer(format(Sys.Date(), "%Y")), "wrong","good")

# c) DURATION BINNING
# Calculate quantiles for equal cuts
quantiles <- quantile(dfcopy$Duration..d., probs = seq(0, 1, length.out = 4), na.rm = TRUE)
labels <- c("short", "medium", "long")

# Bin the duration column into categories using the quantiles
binned_duration <- cut(dfcopy$Duration..d., breaks = quantiles, labels = labels, include.lowest = TRUE, right = TRUE)
dfcopy$duration_binned <- binned_duration

#GEOGRAPHIC FEATURE ENGINEERING
geo_df <- read.csv(file = "geographic_data.csv", header=TRUE)
geo_df <- geo_df[1:13,1:4]
dfcopy <- merge(dfcopy, geo_df, by = "Country", all.x = TRUE)

#ECONOMIC FEATURE ENGINEERING
#GDP per capita is a measure of a country's economic output per person, calculated by dividing the total GDP of a country by its population

#Adding a column for GDP per Capita Annual Growth [1988 - 2022]
gdp_df <- read.csv("gdp_per_capita_country.csv")

dfcopy$gdp_per_capita <- mapply(function(country, year) {
  year_col <- paste0("X", as.character(year))
  
  # Check if the year column exists
  if (year_col %in% colnames(gdp_df)) {
    value <- round(gdp_df[gdp_df$Country.Name == country, year_col], 3)
    if (length(value) > 0 && !is.na(value)) {
      return(value)
    }
  }
  
  # If year column doesn't exist, find the nearest available year
  available_years <- colnames(gdp_df)[grep("^X[0-9]+", colnames(gdp_df))]
  available_years <- as.numeric(gsub("X", "", available_years))
  
  # Find the closest available year
  closest_year <- available_years[which.min(abs(available_years - year))]
  
  # Get the value for the closest available year
  closest_year_col <- paste0("X", closest_year)
  value <- round(gdp_df[gdp_df$Country.Name == country, closest_year_col], 3)
  if (length(value) > 0 && !is.na(value)) {
    return(value)
  }
  
  # Return NA if no valid value is found
  return(NA)
}, dfcopy$Country, dfcopy$Year)


# Adding a column for Population [1960 - 2022]
pop_df <- read.csv("country_wise_population_data.csv")

dfcopy$population <- mapply(function(country, year) {
  year_col <- paste0("X", as.character(year))
  
  if (year_col %in% colnames(pop_df)) {
    value <- pop_df[pop_df$Country.Name == country, year_col]
    if (length(value) > 0 && !is.na(value)) {
      return(value)
    }
  }
  
  # Find the first available year column with non-NA data for the country
  available_years <- colnames(pop_df)[grep("^X\\d{4}$", colnames(pop_df))]
  first_available_value <- pop_df[pop_df$Country.Name == country, available_years][1, ]
  
  # Filter non-NA values and return the first available year value
  non_na_values <- first_available_value[!is.na(first_available_value)]
  if (length(non_na_values) > 0) {
    return(non_na_values[1])
  } else {
    return(NA)
  }
}, dfcopy$Country, dfcopy$Year)

# Adding a column for country grouping - based on gdp per capita
# Calculate quantiles for equal cuts
quantiles <- quantile(dfcopy$gdp_per_capita, probs = seq(0, 1, length.out = 5), na.rm = TRUE)
labels <- c("Lower", "Lower-Middle", "Upper-Middle", "Upper")
dfcopy <- dfcopy %>%
  mutate(country_gdp_group = cut(gdp_per_capita, 
                                 breaks = quantiles, 
                                 labels = labels, 
                                 include.lowest = TRUE, 
                                 right = TRUE))

dfcopy <- dfcopy %>%
  group_by(Country) %>%
  mutate(country_group = names(sort(table(country_gdp_group), decreasing = TRUE)[1])) %>%
  ungroup()

dfcopy <- dfcopy %>% select(-country_gdp_group)

#creating a new column Infrastructure that combines effect of all institutions
dfcopy$infrastructure <- rowSums(df[, c("Education", "Agriculture", "Health.sector", 
                                        "Water.supply","Communications", "Transportation", 
                                        "Power.and.Energy","Relief","Education.centers","Hospitals")], na.rm = TRUE)

# Calculate the percentage of NA values in each column
na_perc <- round(colSums(is.na(dfcopy)) / nrow(df) * 100,2)
na_perc_dfcopy <- data.frame(NA_Percentage = na_perc)
na_perc_dfcopy

dfcopy[is.na(dfcopy)] = 0


# ONE - HOT ENCODING
dfcopy_encoded <- data.frame(dfcopy)

#duration_binned
length(unique(dfcopy$duration_binned)) 
duration_one_hot_encoded <- model.matrix(~duration_binned-1, data=dfcopy)
dfcopy_encoded <- cbind(dfcopy_encoded, duration_one_hot_encoded)

#events
length(unique(dfcopy$Updated.Event.Name)) #44 unique values
event_one_hot_encoded <- model.matrix(~Updated.Event.Name-1, data=dfcopy)
dfcopy_encoded <- cbind(dfcopy, event_one_hot_encoded)

length(unique(dfcopy$Mapped.Event.Categories)) #13 unique values
mapped_one_hot_encoded <- model.matrix(~Mapped.Event.Categories-1, data=dfcopy)
dfcopy_encoded <- cbind(dfcopy_encoded, mapped_one_hot_encoded)

#country
length(unique(dfcopy$Country)) #13 unique values
country_one_hot_encoded <- model.matrix(~Country-1, data=dfcopy)
dfcopy_encoded <- cbind(dfcopy_encoded, country_one_hot_encoded)

#Continent
length(unique(dfcopy$Continent)) 
continent_one_hot_encoded <- model.matrix(~Continent-1, data=dfcopy)
dfcopy_encoded <- cbind(dfcopy_encoded, continent_one_hot_encoded)

#Tropical.Status
length(unique(dfcopy$Tropical.Status))
ts_one_hot_encoded <- model.matrix(~Tropical.Status-1, data=dfcopy)
dfcopy_encoded <- cbind(dfcopy_encoded, ts_one_hot_encoded)

#Hemisphere
length(unique(dfcopy$Hemisphere)) #10 unique values
hs_one_hot_encoded <- model.matrix(~Hemisphere-1, data=dfcopy)
dfcopy_encoded <- cbind(dfcopy_encoded, hs_one_hot_encoded)

#Country group
length(unique(dfcopy$country_group)) #10 unique values
cg_one_hot_encoded <- model.matrix(~country_group-1, data=dfcopy)
dfcopy_encoded <- cbind(dfcopy_encoded, cg_one_hot_encoded)


# NORMALISE TRANSFORMATION
#Normalize - Transforms the data to fit within a specific range, usually [0, 1]
#Standardize - Standardization transforms the data to have a mean of 0 and a standard deviation of 1, effectively creating a standard normal distributio
df_transformed <- data.frame(dfcopy_encoded)
col_transf_list <- names(df)[sapply(df, is.numeric)]
col_transf_list <- setdiff(col_transf_list, c("X", "DataCards", "Deaths","Losses..Local")) 

for (col in col_transf_list) {
  if (is.numeric(dfcopy_encoded[[col]])) {
    df_transformed[[paste0(col, "_norm")]] <- (dfcopy_encoded[[col]] - min(dfcopy_encoded[[col]], na.rm = TRUE)) / 
      (max(dfcopy_encoded[[col]], na.rm = TRUE) - min(dfcopy_encoded[[col]], na.rm = TRUE))
  }
}

# LOG TRANSFORMATION

for (col in col_transf_list) {
  # Check if the column is numeric
  if (is.numeric(dfcopy_encoded[[col]])) {
    # Log transformation (adding 1 to handle log(0))
    df_transformed[[paste0(col, "_log")]] <- log(dfcopy_encoded[[col]] + 1)
    
  }
}

df_transformed[is.na(df_transformed)] = 0 

# Save the cleaned dataset to a CSV file
write.csv(df_transformed, file = "model_data_2.csv", row.names = FALSE)

