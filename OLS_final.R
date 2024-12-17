# Load necessary libraries
library(MASS)       # For Huber regression
library(tidyverse)  # For data manipulation
library(purrr)      # For working with data frames and pmap
library(dplyr)
library(ggplot2)

# Define Huber loss regression function
huber_regression <- function(data, formula) {
  model <- lm(formula, data = data)
  return(model)
}

rolling_window <- function(data, estimation_periods, formula) {
  results <- tibble()
  
  for (period in seq_len(nrow(estimation_periods))) {
    # Extract period info
    train_start <- estimation_periods$training_start[period]
    train_end <- estimation_periods$training_end[period]
    validation_start <- estimation_periods$validation_start[period]
    validation_end <- estimation_periods$validation_end[period]
    oos_year <- estimation_periods$oos_year[period]
    
    # Debug: Print period details
    cat("Processing period:\n")
    print(list(train_start, train_end, validation_start, validation_end, oos_year))
    
    # Filter and collect data for training, validation, and OOS
    train_data <- data %>%
      filter(year >= train_start & year <= train_end) %>%
      collect()  # Pull into R memory
    validation_data <- data %>%
      filter(year >= validation_start & year <= validation_end) %>%
      collect()  # Pull into R memory
    oos_data <- data %>%
      filter(year == oos_year) %>%
      collect()  # Pull into R memory
    
    # Debug: Check filtered data frames
    cat("Train Data Rows:", nrow(train_data), "\n")
    cat("Validation Data Rows:", nrow(validation_data), "\n")
    cat("OOS Data Rows:", nrow(oos_data), "\n")
    
    # Skip this iteration if training data is empty
    if (nrow(train_data) == 0) {
      cat("Skipping period: No training data\n")
      next
    }
    
    # Train the model
    model <- huber_regression(train_data, formula)
    
    # Predict on validation data (if not empty)
    if (nrow(validation_data) > 0) {
      validation_data <- validation_data %>%
        mutate(predicted = predict(model, newdata = validation_data)) %>%
        mutate(classification = "Validation")
    } else {
      validation_data <- tibble()  # Ensure it’s a valid tibble
    }
    
    # Predict on OOS data (if not empty)
    if (nrow(oos_data) > 0) {
      oos_data <- oos_data %>%
        mutate(predicted = predict(model, newdata = oos_data)) %>%
        mutate(classification = "OOS")
    } else {
      oos_data <- tibble()  # Ensure it’s a valid tibble
    }
    
    # Combine results safely
    results <- bind_rows(results, validation_data, oos_data)
  }
  
  return(results)
}

# Define formula
formula <- ret_excess ~ characteristic_mom12m_x_macro_intercept + 
  characteristic_bm_x_macro_intercept + 
  mktcap_lag

# Define time periods (based on your example)
validation_length <- 12
estimation_periods <- tibble(
  oos_year = 1987:2021,
  validation_end = oos_year - 1,
  validation_start = oos_year - validation_length,
  training_start = 1957,
  training_end = validation_start - 1
)

library(dplyr)
characteristics_prepared <- characteristics_prepared %>%
  mutate(year = year(ymd(month))) # Extract the year from the month column

columns_to_keep <- c("year","ret_excess", "characteristic_mom12m_x_macro_intercept", 
                     "characteristic_bm_x_macro_intercept", "mktcap_lag", "permno", "month")

# Create a filtered dataset with only the required columns
characteristics_prepared_2 <- characteristics_prepared %>%
  dplyr::select(all_of(columns_to_keep))

data <- characteristics_prepared_2 %>%
  arrange(month)  # Ensure data is ordered by time

# Perform rolling window analysis
results <- rolling_window(data, estimation_periods, formula)
write.csv(results, "results_OLS.csv", row.names = FALSE)


library(dplyr)
# Function to calculate MSE, MAE, and R-squared
calculate_metrics <- function(data, actual_col, predicted_col) {
  # Ensure actual and predicted columns exist
  if (!(actual_col %in% names(data)) || !(predicted_col %in% names(data))) {
    stop("Specified columns do not exist in the data.")
  }
  
  actual <- data[[actual_col]]
  predicted <- data[[predicted_col]]
  
  # Mean Squared Error
  mse <- mean((actual - predicted)^2, na.rm = TRUE)
  
  # Mean Absolute Error
  mae <- mean(abs(actual - predicted), na.rm = TRUE)
  
  # R-squared
  ss_total <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  ss_residual <- sum((actual - predicted)^2, na.rm = TRUE)
  rsq <- 1 - (ss_residual / ss_total)
  
  # Return a tibble of metrics
  return(tibble(mse = mse, mae = mae, rsq = rsq))
}

# Ensure summary_table is a tibble
summary_table <- estimation_periods %>%
  rowwise() %>%
  mutate(
    train_start = training_start,
    train_end = training_end,
    val_start = validation_start,
    val_end = validation_end,
    oos_yr = oos_year,
    
    # Whole set metrics
    whole_set_validation_metrics = list({
      val_data <- results %>% 
        filter(year >= val_start & year <= val_end & classification == "Validation")
      if (nrow(val_data) > 0) {
        calculate_metrics(val_data, "ret_excess", "predicted")
      } else {
        tibble(mse = NA, mae = NA, rsq = NA)
      }
    }),
    whole_set_oos_metrics = list({
      oos_data <- results %>% 
        filter(year == oos_yr & classification == "OOS")
      if (nrow(oos_data) > 0) {
        calculate_metrics(oos_data, "ret_excess", "predicted")
      } else {
        tibble(mse = NA, mae = NA, rsq = NA)
      }
    }),
    
    # Top 1000 market cap metrics
    top_1000_validation_metrics = list({
      val_data <- results %>% 
        filter(year >= val_start & year <= val_end & classification == "Validation") %>%
        arrange(desc(mktcap_lag)) %>% 
        slice_head(n = 1000)
      if (nrow(val_data) > 0) {
        calculate_metrics(val_data, "ret_excess", "predicted")
      } else {
        tibble(mse = NA, mae = NA, rsq = NA)
      }
    }),
    top_1000_oos_metrics = list({
      oos_data <- results %>% 
        filter(year == oos_yr & classification == "OOS") %>%
        arrange(desc(mktcap_lag)) %>%
        slice_head(n = 1000)
      if (nrow(oos_data) > 0) {
        calculate_metrics(oos_data, "ret_excess", "predicted")
      } else {
        tibble(mse = NA, mae = NA, rsq = NA)
      }
    }),
    
    # Bottom 1000 market cap metrics
    bottom_1000_validation_metrics = list({
      val_data <- results %>% 
        filter(year >= val_start & year <= val_end & classification == "Validation") %>%
        arrange(mktcap_lag) %>%
        slice_head(n = 1000)
      if (nrow(val_data) > 0) {
        calculate_metrics(val_data, "ret_excess", "predicted")
      } else {
        tibble(mse = NA, mae = NA, rsq = NA)
      }
    }),
    bottom_1000_oos_metrics = list({
      oos_data <- results %>% 
        filter(year == oos_yr & classification == "OOS") %>%
        arrange(mktcap_lag) %>%
        slice_head(n = 1000)
      if (nrow(oos_data) > 0) {
        calculate_metrics(oos_data, "ret_excess", "predicted")
      } else {
        tibble(mse = NA, mae = NA, rsq = NA)
      }
    })
  ) %>%
  unnest_wider(whole_set_validation_metrics, names_sep = "_val_whole") %>%
  unnest_wider(whole_set_oos_metrics, names_sep = "_oos_whole") %>%
  unnest_wider(top_1000_validation_metrics, names_sep = "_val_top1000") %>%
  unnest_wider(top_1000_oos_metrics, names_sep = "_oos_top1000") %>%
  unnest_wider(bottom_1000_validation_metrics, names_sep = "_val_bottom1000") %>%
  unnest_wider(bottom_1000_oos_metrics, names_sep = "_oos_bottom1000") %>%
  mutate(
    Period = oos_yr,
    Training_Start = train_start,
    Training_End = train_end,
    Validation_Start = val_start,
    Validation_End = val_end,
    OOS_Year = oos_yr
  ) %>%
  as_tibble() %>%
  dplyr::select(
    Period, Training_Start, Training_End, Validation_Start, Validation_End, OOS_Year,
    starts_with("whole_set_validation_metrics"), starts_with("whole_set_oos_metrics"),
    starts_with("top_1000_validation_metrics"), starts_with("top_1000_oos_metrics"),
    starts_with("bottom_1000_validation_metrics"), starts_with("bottom_1000_oos_metrics")
  )

# View the summary table
print(summary_table)

# Write the summary table to CSV
write.csv(summary_table, "model_summary_table.csv", row.names = FALSE)
summary_table <- read.csv("model_summary_table.csv")

# Calculate R^2 values dynamically from the summary_table
r2_data <- summary_table %>%
  summarise(
    All = mean(whole_set_oos_metrics_oos_wholersq, na.rm = TRUE),
    Top_1000 = mean(top_1000_oos_metrics_oos_top1000rsq, na.rm = TRUE),
    Bottom_1000 = mean(bottom_1000_oos_metrics_oos_bottom1000rsq, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Group", values_to = "R2")

# Create the bar chart
ggplot(r2_data, aes(x = Group, y = R2, fill = Group)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  scale_fill_manual(values = c("blue", "green", "yellow")) +
  labs(
    title = "Monthly Out-of-Sample Stock-Level Prediction Performance",
    y = expression(R^2["OOS"]),
    x = "Group",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
