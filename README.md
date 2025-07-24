# Landscape-System-Management
Comparison of Observed and Simulated
# Install and load necessary packages
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("lubridate")

library(ggplot2)
library(dplyr)
library(lubridate)

# 1. Create Synthetic Data that mimics your observed and predicted graphs
# Define the date range (Jan 1, 2001 to Apr 9, 2001 as seen in your graph)
start_date <- ymd("2001-01-01")
end_date <- ymd("2001-04-09")
date_sequence <- seq(start_date, end_date, by = "15 min") # Match your 15-min interval

# Generate synthetic observed flow data
# We'll create a few peaks similar to your observed graph
observed_flow <- rep(0.5, length(date_sequence)) # Baseflow

# Peak 1 (mid-Jan)
peak1_start <- ymd_hms("2001-01-15 00:00:00")
peak1_end <- ymd_hms("2001-01-18 00:00:00")
peak1_indices <- which(date_sequence >= peak1_start & date_sequence <= peak1_end)
observed_flow[peak1_indices] <- 0.5 + pmax(0, -abs(as.numeric(date_sequence[peak1_indices] - peak1_start - days(1.5))) + days(1.5)) * 15 # Shape peak

# Peak 2 (mid-Feb)
peak2_start <- ymd_hms("2001-02-08 00:00:00")
peak2_end <- ymd_hms("2001-02-13 00:00:00")
peak2_indices <- which(date_sequence >= peak2_start & date_sequence <= peak2_end)
observed_flow[peak2_indices] <- 0.5 + pmax(0, -abs(as.numeric(date_sequence[peak2_indices] - peak2_start - days(2.5))) + days(2.5)) * 30

# Peak 3 (early March - largest)
peak3_start <- ymd_hms("2001-03-08 00:00:00")
peak3_end <- ymd_hms("2001-03-13 00:00:00")
peak3_indices <- which(date_sequence >= peak3_start & date_sequence <= peak3_end)
observed_flow[peak3_indices] <- 0.5 + pmax(0, -abs(as.numeric(date_sequence[peak3_indices] - peak3_start - days(2.5))) + days(2.5)) * 60

# Scale observed flow to be roughly 0-900 range as in your graph (adjust as needed)
observed_flow <- observed_flow * (900 / max(observed_flow)) + 5 # Add a small baseflow

# Generate synthetic predicted flow data (higher than observed, similar shape)
predicted_flow <- observed_flow * 2.5 # Make predicted roughly 2.5x higher for demonstration

# Introduce some variation or baseflow differences if desired
predicted_flow[predicted_flow < 50] <- predicted_flow[predicted_flow < 50] * 0.1 # Example: reduce baseflow for predicted

# Combine into a data frame
hydro_data <- data.frame(
  Date = date_sequence,
  Observed = observed_flow,
  Predicted = predicted_flow
)

# 2. Plotting the Hydrograph using ggplot2
ggplot(hydro_data, aes(x = Date)) +
  geom_line(aes(y = Observed, color = "Observed"), size = 1) + # Blue line
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "solid") + # Green line
  scale_color_manual(values = c("Observed" = "#004369", "Predicted" = "#01949A")) + # Custom colors based on your palette
  labs(
    title = "Simulated vs. Observed Discharge at Mission Creek (Junction 4)",
    x = "Date",
    y = "Discharge (CFS)",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  ) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%d/%m/%Y") # Adjust date labels for clarity

# You can save the plot if needed
# ggsave("hydrograph_comparison.png", width = 10, height = 6, dpi = 300)

# Display a subset of the data to verify (optional)
# head(hydro_data)
# tail(hydro_data)
