# Load necessary libraries 
library(ggplot2)
library(plotly)
library(reshape2)

# Load the dataset (replace with your actual data path)
data <- read.csv("sampled_kidney_disease_raw.csv")

# Filter relevant columns
selected_data <- data[, c("age", "bp", "urine_albumin", "blood_urea", "serum_creatine", "hgb", "classification")]

# Ensure classification is a factor
selected_data$classification <- as.factor(selected_data$classification)

# Calculate mean, median, and mode for numerical columns
numerical_data <- selected_data[, c("age", "bp", "urine_albumin", "blood_urea", "serum_creatine", "hgb")]
stats <- data.frame(
  Parameter = colnames(numerical_data),
  Mean = sapply(numerical_data, mean, na.rm = TRUE),
  Median = sapply(numerical_data, median, na.rm = TRUE),
  Mode = sapply(numerical_data, function(x) {
    ux <- unique(na.omit(x))
    ux[which.max(tabulate(match(x, ux)))]
  })
)

# Pie Chart
pie_data <- table(selected_data$classification)
pie_data_df <- as.data.frame(pie_data)  # Convert table to a data frame

# Pie chart using plotly
pie_fig <- plot_ly(
  data = pie_data_df, 
  labels = ~Var1,        # Labels come from the table's first column
  values = ~Freq,        # Values come from the table's second column
  type = "pie",
  textinfo = "label+percent", 
  insidetextorientation = "radial"
) %>%
  layout(
    title = "Distribution of CKD vs Non-CKD",
    paper_bgcolor = "cyan", 
    plot_bgcolor = "white"
  )

# Histogram comparing distributions (BP, Urine Albumin, Blood Urea, Serum Creatinine, HGB)
numerical_columns <- c("bp", "urine_albumin", "blood_urea", "serum_creatine", "hgb")
histogram_data <- selected_data[, c("classification", numerical_columns)]
histogram_data_melt <- melt(histogram_data, id.vars = "classification")

histogram_plot <- ggplot(histogram_data_melt, aes(value, fill = classification)) +
  geom_histogram(position = "dodge", bins = 15, alpha = 0.7) +
  facet_wrap(~variable, scales = "free_x") +
  scale_fill_manual(values = c("ckd" = "red", "notckd" = "blue")) +
  labs(title = "Histograms of Metrics Relative to CKD and Non-CKD",
       x = "Value",
       y = "Frequency") +
  theme_minimal()

# Convert the ggplot histogram to an interactive plot using ggplotly
interactive_histogram <- ggplotly(histogram_plot)

# Individual Histogram (Age distribution for CKD vs Non-CKD)
hist_fig <- ggplot(selected_data, aes(x = age, fill = classification)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity", color = "black") +
  labs(title = "Age Distribution for CKD vs Non-CKD", x = "Age", y = "Count") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "cyan", color = "white")) +
  scale_fill_manual(values = c("green", "darkgreen"))

# Boxplot for Hemoglobin Levels (already in the previous code)
boxplot_fig <- ggplot(selected_data, aes(x = classification, y = hgb, fill = classification)) +
  geom_boxplot(color = "black") +
  labs(title = "Hemoglobin Levels by CKD Classification", x = "Classification", y = "Hemoglobin") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "cyan", color = "white")) +
  scale_fill_manual(values = c("green", "darkgreen"))

# Additional Boxplots for all numeric variables
numeric_columns <- c("age", "bp", "urine_albumin", "blood_urea", "serum_creatine", "hgb")

# Reshape the data for plotting (long format)
long_data <- melt(selected_data[, c("classification", numeric_columns)], 
                  id.vars = "classification", 
                  variable.name = "Variable", 
                  value.name = "Value")

# Create the boxplot using the reshaped data
boxplot_all <- ggplot(long_data, aes(x = classification, y = Value, fill = classification)) +
  geom_boxplot(color = "black") +
  facet_wrap(~Variable, scales = "free_y") +
  labs(title = "Boxplots for All Numeric Variables", x = "Classification", y = "Value") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "cyan", color = "white")) +
  scale_fill_manual(values = c("green", "darkgreen"))

# Heatmap
cor_matrix <- cor(selected_data[, c("age", "bp", "urine_albumin", "blood_urea", "serum_creatine", "hgb")], use = "pairwise.complete.obs")

heatmap_fig <- plot_ly(
  x = colnames(cor_matrix), 
  y = colnames(cor_matrix), 
  z = cor_matrix, 
  type = "heatmap", 
  colorscale = "Viridis", 
  text = round(cor_matrix, 2), 
  textfont = list(size = 10)
) %>% layout(
  title = "Correlation Heatmap of Numerical Variables",
  xaxis = list(title = ""),
  yaxis = list(title = "")
)

# Add annotations for strong correlations (e.g., absolute correlation > 0.7)
for (i in 1:nrow(cor_matrix)) {
  for (j in 1:ncol(cor_matrix)) {
    if (abs(cor_matrix[i, j]) > 0.7) {
      heatmap_fig <- heatmap_fig %>%
        add_annotations(
          x = colnames(cor_matrix)[j],
          y = colnames(cor_matrix)[i],
          text = paste0("Corr:", round(cor_matrix[i, j], 2)),
          showarrow = FALSE,
          font = list(size = 10)
        )
    }
  }
}

# Print the plots
print(pie_fig)
print(hist_fig)
print(boxplot_fig)
print(heatmap_fig)
print(boxplot_all) # Print the combined boxplots for all numeric variables

# Print summary statistics
print(stats)

# Print the interactive histogram
print(interactive_histogram)
