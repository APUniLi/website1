library(dplyr)


predictions_combined <- RF_oos_prediction %>%
  inner_join(results %>% dplyr::select(permno, month, predicted), by = c("month", "permno")) %>%
  arrange(month, permno) %>%
  dplyr::select(
    permno,
    month,
    ret_excess,
    mktcap_lag,
    predictions_RF = .pred,     # Rename .pred to predictions_RF
    predictions_OOS = predicted # Rename predicted to predictions_OOS
  )



ranked_data <- predictions_combined %>%
  group_by(month) %>%
  mutate(
    decile_RF = ntile(predictions_RF, 10), # Deciles based on RF predictions
    decile_OOS = ntile(predictions_OOS, 10) # Deciles based on OLS predictions
  ) %>%
  ungroup()

ranked_data <- predictions_combined %>%
  group_by(month) %>%
  mutate(
    decile_RF = ntile(predictions_RF, 10), # Decile for Random Forest
    decile_OLS = ntile(predictions_OOS, 10) # Decile for OLS
  ) %>%
  ungroup()

decile_returns_RF <- ranked_data %>%
  group_by(month, decile_RF) %>%
  summarize(
    predicted_mean = mean(predictions_RF, na.rm = TRUE),
    realized_mean = sum(ret_excess * mktcap_lag, na.rm = TRUE) / sum(mktcap_lag, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(decile_RF) %>%
  summarize(
    predicted_mean = mean(predicted_mean, na.rm = TRUE),
    realized_mean = mean(realized_mean, na.rm = TRUE),
    realized_sd = sd(realized_mean, na.rm = TRUE)
  ) %>%
  mutate(
    sharpe_ratio = realized_mean / realized_sd
  )

# Rename decile_RF to portfolio for clarity
decile_returns_RF <- decile_returns_RF %>%
  rename(portfolio = decile_RF)


decile_returns_OLS <- ranked_data %>%
  group_by(month, decile_OLS) %>%
  summarize(
    predicted_mean = mean(predictions_OOS, na.rm = TRUE),
    realized_mean = sum(ret_excess * mktcap_lag, na.rm = TRUE) / sum(mktcap_lag, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(decile_OLS) %>%
  summarize(
    predicted_mean = mean(predicted_mean, na.rm = TRUE),
    realized_mean = mean(realized_mean, na.rm = TRUE),
    realized_sd = sd(realized_mean, na.rm = TRUE)
  ) %>%
  mutate(
    sharpe_ratio = realized_mean / realized_sd
  )

# Rename decile_OLS to portfolio for clarity
decile_returns_OLS <- decile_returns_OLS %>%
  rename(portfolio = decile_OLS)

#long_short_RF <- decile_returns_RF %>%
#  summarize(
#    portfolio = "H-L",
#    predicted_mean = predicted_mean[10] - predicted_mean[1],
#    realized_mean = realized_mean[10] - realized_mean[1],
#    realized_sd = sd(c(realized_mean[10], realized_mean[1]), na.rm = TRUE),
#    sharpe_ratio = realized_mean / realized_sd
#  )

#decile_returns_RF <- bind_rows(decile_returns_RF, long_short_RF)


decile_returns_RF <- decile_returns_RF %>%
  mutate(portfolio = as.character(portfolio))

decile_returns_RF <- decile_returns_RF %>%
  mutate(portfolio = as.character(portfolio))

long_short_RF <- long_short_RF %>%
  mutate(portfolio = as.character(portfolio)) # Ensure consistency

# Now bind rows
decile_returns_RF <- bind_rows(decile_returns_RF, long_short_RF)


# Ensure portfolio is of the same type
decile_returns_OLS <- decile_returns_OLS %>%
  mutate(portfolio = as.character(portfolio))

long_short_OLS <- long_short_OLS %>%
  mutate(portfolio = as.character(portfolio))
# Bind long-short row to the decile results
decile_returns_OLS <- bind_rows(decile_returns_OLS, long_short_OLS)

print(decile_returns_RF) # For Random Forest
print(decile_returns_OLS) # For OLS

sharpe_comparison <- tibble(
  Model = c("Random Forest", "OLS"),
  Sharpe_Ratio = c(
    decile_returns_RF %>% filter(portfolio == "H-L") %>% pull(sharpe_ratio),
    decile_returns_OLS %>% filter(portfolio == "H-L") %>% pull(sharpe_ratio)
  )
)

print(sharpe_comparison)




##new

# Compute monthly returns for each decile (RF)
monthly_returns_RF <- ranked_data %>%
  group_by(month, decile_RF) %>%
  summarize(
    realized_return = sum(ret_excess * mktcap_lag, na.rm = TRUE) / sum(mktcap_lag, na.rm = TRUE),
    predicted_return = mean(predictions_RF, na.rm = TRUE)
  ) %>%
  ungroup()

# Compute monthly returns for each decile (OLS)
monthly_returns_OLS <- ranked_data %>%
  group_by(month, decile_OLS) %>%
  summarize(
    realized_return = sum(ret_excess * mktcap_lag, na.rm = TRUE) / sum(mktcap_lag, na.rm = TRUE),
    predicted_return = mean(predictions_OOS, na.rm = TRUE)
  ) %>%
  ungroup()
# RF Decile-Level Summaries
decile_returns_RF <- monthly_returns_RF %>%
  group_by(decile_RF) %>%
  summarize(
    predicted_mean = mean(predicted_return, na.rm = TRUE),
    realized_mean = mean(realized_return, na.rm = TRUE),
    realized_sd = sd(realized_return, na.rm = TRUE),
    sharpe_ratio = realized_mean / realized_sd
  ) %>%
  rename(portfolio = decile_RF) # Rename column for clarity
# OLS Decile-Level Summaries
decile_returns_OLS <- monthly_returns_OLS %>%
  group_by(decile_OLS) %>%
  summarize(
    predicted_mean = mean(predicted_return, na.rm = TRUE),
    realized_mean = mean(realized_return, na.rm = TRUE),
    realized_sd = sd(realized_return, na.rm = TRUE),
    sharpe_ratio = realized_mean / realized_sd
  ) %>%
  rename(portfolio = decile_OLS) # Rename column for clarity
# RF Long-Short
long_short_RF <- decile_returns_RF %>%
  summarize(
    portfolio = "H-L",
    predicted_mean = predicted_mean[10] - predicted_mean[1],
    realized_mean = realized_mean[10] - realized_mean[1],
    realized_sd = sd(c(realized_mean[10], realized_mean[1]), na.rm = TRUE),
    sharpe_ratio = realized_mean / realized_sd
  )

decile_returns_RF <- bind_rows(decile_returns_RF, long_short_RF)

# OLS Long-Short
long_short_OLS <- decile_returns_OLS %>%
  summarize(
    portfolio = "H-L",
    predicted_mean = predicted_mean[10] - predicted_mean[1],
    realized_mean = realized_mean[10] - realized_mean[1],
    realized_sd = sd(c(realized_mean[10], realized_mean[1]), na.rm = TRUE),
    sharpe_ratio = realized_mean / realized_sd
  )

decile_returns_OLS <- bind_rows(decile_returns_OLS, long_short_OLS)

# Convert portfolio column to character in decile_returns_OLS
decile_returns_OLS <- decile_returns_OLS %>%
  mutate(portfolio = as.character(portfolio))

# Calculate long-short portfolio

long_short_OLS <- decile_returns_OLS %>%
  summarize(
    portfolio = "H-L", # Ensure this is character
    predicted_mean = predicted_mean[10] - predicted_mean[1],
    realized_mean = realized_mean[10] - realized_mean[1],
    realized_sd = sd(c(realized_mean[10], realized_mean[1]), na.rm = TRUE),
    sharpe_ratio = realized_mean / realized_sd
  )

# Combine the long-short row with decile results
decile_returns_OLS <- bind_rows(decile_returns_OLS, long_short_OLS)




# Assuming your data frame is called df

# Calculate the H-L values
df_ols <- decile_returns_OLS
hl_row <- df_ols[nrow(df_ols), ]  # Copy the structure of the last row
hl_row$portfolio <- "H-L"  # Rename the portfolio column

hl_row$predicted_mean <- df_ols[df$portfolio == 10, "predicted_mean"] - df_ols[df_ols$portfolio == 1, "predicted_mean"]
hl_row$realized_mean <- df_ols[df$portfolio == 10, "realized_mean"] - df_ols[df_ols$portfolio == 1, "realized_mean"]
hl_row$realized_sd <- df_ols[df_ols$portfolio == 10, "realized_sd"] - df_ols[df_ols$portfolio == 1, "realized_sd"]
hl_row$sharpe_ratio <- df_ols[df_ols$portfolio == 10, "sharpe_ratio"] - df_ols[df_ols$portfolio == 1, "sharpe_ratio"]

# Append the H-L row to the data frame
df_ols <- rbind(df_ols, hl_row)

# View the updated data frame
print(df_ols)


# Calculate the H-L values
df_rf <- decile_returns_RF
hl_row <- df_rf[nrow(df_rf), ]  # Copy the structure of the last row
hl_row$portfolio <- "H-L"  # Rename the portfolio column

hl_row$predicted_mean <- df_rf[df_rf$portfolio == 10, "predicted_mean"] - df_rf[df_rf$portfolio == 1, "predicted_mean"]
hl_row$realized_mean <- df_rf[df_rf$portfolio == 10, "realized_mean"] - df_rf[df_rf$portfolio == 1, "realized_mean"]
hl_row$realized_sd <- df_rf[df_rf$portfolio == 10, "realized_sd"] - df_rf[df_rf$portfolio == 1, "realized_sd"]
hl_row$sharpe_ratio <- df_rf[df_rf$portfolio == 10, "sharpe_ratio"] - df_rf[df_rf$portfolio == 1, "sharpe_ratio"]

# Append the H-L row to the data frame
df_rf <- rbind(df_rf, hl_row)

# View the updated data frame
print(df_rf)


# Round numeric columns to two decimal places
df_ols[, 2:5] <- lapply(df_ols[, 2:5], function(x) round(as.numeric(x), 4))
df_rf[, 2:5] <- lapply(df_rf[, 2:5], function(x) round(as.numeric(x), 4))

# Compute H-L (long-short) portfolio returns for RF
portfolio_returns_RF <- monthly_returns_RF %>%
  group_by(month) %>%
  summarize(long_short_RF = realized_return[decile_RF == 10] - realized_return[decile_RF == 1]) %>%
  ungroup()

# Compute H-L (long-short) portfolio returns for OLS
portfolio_returns_OLS <- monthly_returns_OLS %>%
  group_by(month) %>%
  summarize(long_short_OOS = realized_return[decile_OLS == 10] - realized_return[decile_OLS == 1]) %>%
  ungroup()
# Merge RF and OLS portfolio returns by month
portfolio_returns <- portfolio_returns_RF %>%
  left_join(portfolio_returns_OLS, by = "month")

write.csv(portfolio_returns, "portfolio_returns.csv", row.names = FALSE)

portfolio_returns <- portfolio_returns %>%
  mutate(
    cumulative_RF = cumsum(long_short_RF, na.rm = TRUE),
    cumulative_OOS = cumsum(long_short_OOS, na.rm = TRUE)
  )

portfolio_returns <- portfolio_returns %>%
  mutate(
    cumulative_RF = if_else(is.na(long_short_RF), NA_real_, cumsum(long_short_RF)),
    cumulative_OOS = if_else(is.na(long_short_OOS), NA_real_, cumsum(long_short_OOS))
  )
sharpe_RF <- mean(portfolio_returns$long_short_RF, na.rm = TRUE) / sd(portfolio_returns$long_short_RF, na.rm = TRUE)
sharpe_OLS <- mean(portfolio_returns$long_short_OOS, na.rm = TRUE) / sd(portfolio_returns$long_short_OOS, na.rm = TRUE)

setnames(portfolio_returns, "cumulative_OOS", "cumulative_OLS")
setnames(portfolio_returns, "long_short_OOS", "long_short_OLS")


# Load necessary library
library(ggplot2)

# Assuming your data frame is called 'data'
# Replace 'data' with the actual name of your data frame
ggplot(portfolio_returns, aes(x = month)) +
  geom_line(aes(y = cumulative_OLS, color = "Cumulative OLS")) +
  geom_line(aes(y = cumulative_RF, color = "Cumulative RF")) +
  scale_color_manual(values = c("Cumulative OLS" = "blue", "Cumulative RF" = "red")) +
  labs(
    title = "Cumulative Returns Over Time",
    x = "Time (Month)",
    y = "Cumulative Returns",
    color = "Legend"
  ) +
  theme_minimal()

#ols
monthly_returns_OLS_short <- subset(monthly_returns_OLS, decile_OLS == 1)
monthly_returns_OLS_short$realized_return_short <- monthly_returns_OLS_short$realized_return * -1
monthly_returns_OLS_long <- subset(monthly_returns_OLS, decile_OLS == 10)

#rf
monthly_returns_RF_short <- subset(monthly_returns_RF, decile_RF == 1)
monthly_returns_RF_short$realized_return_short <- monthly_returns_RF_short$realized_return * -1
monthly_returns_RF_long <- subset(monthly_returns_RF, decile_RF == 10)

monthly_returns_final <- monthly_returns_OLS_long
monthly_returns_final$realized_return_ols_l <- monthly_returns_OLS_long$realized_return
monthly_returns_final$realized_return_ols_s <- monthly_returns_OLS_short$realized_return
monthly_returns_final$realized_return_rf_l <- monthly_returns_RF_long$realized_return
monthly_returns_final$realized_return_rf_s <- monthly_returns_RF_short$realized_return

monthly_returns_final$realized_return_rf_l <- as.numeric(monthly_returns_final$realized_return_rf_l)
monthly_returns_final$realized_return_rf_s <- as.numeric(monthly_returns_final$realized_return_rf_s)
monthly_returns_final$realized_return_ols_l <- as.numeric(monthly_returns_final$realized_return_ols_l)
monthly_returns_final$realized_return_ols_s <- as.numeric(monthly_returns_final$realized_return_ols_s)

# Calculate cumulative returns
monthly_returns_final$cumulative_realized_rf_l <- cumprod(1 + monthly_returns_final$realized_return_rf_l)
monthly_returns_final$cumulative_realized_rf_s <- cumprod(1 + monthly_returns_final$realized_return_rf_s)
monthly_returns_final$cumulative_realized_ols_l <- cumprod(1 + monthly_returns_final$realized_return_ols_l)
monthly_returns_final$cumulative_realized_ols_s <- cumprod(1 + monthly_returns_final$realized_return_ols_s)



ggplot(monthly_returns_final, aes(x = month)) +
  geom_line(aes(y = cumulative_realized_ols_l, color = "cumulative_realized_ols_l")) +
  geom_line(aes(y = cumulative_realized_ols_s, color = "cumulative_realized_ols_s")) +
  geom_line(aes(y = cumulative_realized_rf_l, color = "cumulative_realized_rf_l")) +
  geom_line(aes(y = cumulative_realized_rf_s, color = "cumulative_realized_rf_s")) +

  scale_color_manual(values = c("cumulative_realized_ols_l" = "blue", "cumulative_realized_ols_s" = "blue", cumulative_realized_rf_l = "red",cumulative_realized_rf_s = "red" )) +
  labs(
    title = "Cumulative Returns Over Time",
    x = "Time (Month)",
    y = "Cumulative Returns",
    color = "Legend"
  ) +
  theme_minimal()


# max drawdown

calculate_max_drawdown <- function(cumulative_returns) {
  # Calculate the drawdown
  drawdown <- cumulative_returns - cummax(cumulative_returns)
  # Only consider negative values (actual drawdowns)
  drawdown <- drawdown[drawdown < 0]
  # Maximum drawdown (most negative value)
  max_drawdown <- ifelse(length(drawdown) > 0, min(drawdown), 0)  # Return 0 if no negative drawdowns
  return(max_drawdown)
}

# Apply the function to cumulative_RF
max_drawdown_RF <- calculate_max_drawdown(portfolio_returns$cumulative_RF)
print(paste("Max Drawdown for RF:", max_drawdown_RF))

# Apply the function to cumulative_OLS
max_drawdown_OLS <- calculate_max_drawdown(data$cumulative_OLS)
print(paste("Max Drawdown for OLS:", max_drawdown_OLS))

