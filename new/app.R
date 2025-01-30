#install plotly
install.packages("plotly")
install.packages(DT)
# Libraries
library(shiny)         # For the Shiny app
library(ggplot2)       # For making charts
library(plotly)        # For interactive plots
library(DT)            # For DataTables
library(reshape2)      # For data reshaping
library(dplyr)         # For data manipulation

# Load dataset (assuming CSV file exists at the specified path)
data <- read.csv("sampled_kidney_disease_raw.csv", fileEncoding = "UTF-8" )

# Convert classification to labels
data$classification <- ifelse(data$classification == "ckd", "CKD", "Non-CKD")

# Select numerical columns for analysis
numerical_columns <- c("age", "bp", "urine_albumin", "blood_urea", "serum_creatine", "hgb")
selected_data <- data[, c(numerical_columns, "classification")]

# Function to calculate mode
calculate_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Calculate mean, median, and mode for numerical columns
summary_table <- data.frame(
  Metric = numerical_columns,
  Mean = sapply(selected_data[, numerical_columns], mean, na.rm = TRUE),
  Median = sapply(selected_data[, numerical_columns], median, na.rm = TRUE),
  Mode = sapply(selected_data[, numerical_columns], calculate_mode)
)

# Define UI
ui <- fluidPage(
  
  # Custom CSS styling
  tags$head(
    tags$style(HTML("body { background: linear-gradient(to right, #eeff66, #ffffff); font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; color: #333; padding: 20px; } h1, h4 { font-family: 'Arial', sans-serif; color: #2c3e50; } .btn-download { background-color: #00bfff; color: white; padding: 10px 20px; border-radius: 5px; border: none; } .btn-download:hover { background-color: #0099cc; } .prediction-result { background-color: #f1f1f1; padding: 10px; border-radius: 5px; font-size: 18px; color: #27ae60; } .nav-tabs .nav-link.active { background-color: #00bfff; color: white; } .nav-tabs .nav-link { color: #555; font-weight: bold; }"))
  ),
  
  titlePanel("CKD Statistics Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      downloadButton("downloadStats", "Download Statistics CSV", class = "btn-download")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Pie Chart", plotlyOutput("pieChart")),
        tabPanel("Histogram", plotlyOutput("histogram")),
        tabPanel("Boxplots", plotlyOutput("boxplot")),
        tabPanel("Heatmap", plotlyOutput("heatmap")),
        tabPanel("Statistics Table", DTOutput("statsTable"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Pie Chart for CKD Distribution
  output$pieChart <- renderPlotly({
    pie_data <- table(selected_data$classification)
    plot_ly(labels = names(pie_data), values = as.numeric(pie_data), type = "pie",
            textinfo = "label+percent", insidetextorientation = "radial") %>%
      layout(title = "Distribution of CKD vs Non-CKD")
  })
  
  # Histogram
  output$histogram <- renderPlotly({
    # Melting the data to long format
    histogram_data_melt <- melt(selected_data, id.vars = "classification")
    
    # Generating the histogram with ggplot
    fig <- ggplot(histogram_data_melt, aes(x = value, fill = classification)) +
      geom_histogram(position = "dodge", bins = 15, alpha = 0.7) +
      facet_wrap(~variable, scales = "free_x") +
      scale_fill_manual(values = c("Non-CKD" = "blue", "CKD" = "#f43")) +
      labs(title = "Histograms of Metrics Relative to CKD and Non-CKD",
           x = "Value", y = "Frequency") +
      theme_minimal()
    
    # Converting ggplot to plotly
    ggplotly(fig)
  })
  
  # Boxplots for all numerical variables
  output$boxplot <- renderPlotly({
    melted_data <- melt(selected_data, id.vars = "classification")
    ggplotly(
      ggplot(melted_data, aes(x = classification, y = value, fill = classification)) +
        geom_boxplot() +
        facet_wrap(~variable, scales = "free_y") +
        labs(title = "Boxplots for Numerical Variables by CKD Classification", x = "Classification", y = "Value") +
        theme_minimal() +
        scale_fill_manual(values = c("Non-CKD" = "#77b5fe", "CKD" = "#f4a460"))
    )
  })
  
  # Heatmap
  output$heatmap <- renderPlotly({
    cor_matrix <- cor(selected_data[, numerical_columns], use = "pairwise.complete.obs")
    plot_ly(x = colnames(cor_matrix), y = colnames(cor_matrix), z = cor_matrix, type = "heatmap",
            colorscale = "Viridis", text = round(cor_matrix, 2)) %>%
      layout(title = "Correlation Heatmap of Numerical Variables")
  })
  
  # Statistics Table
  output$statsTable <- renderDT({
    datatable(summary_table, options = list(pageLength = 5))
  })
  
  # Download Statistics
  output$downloadStats <- downloadHandler(
    filename = function() { "Statistics.csv" },
    content = function(file) {
      write.csv(summary_table, file, row.names = FALSE)
    }
  )
}

# Run the App
shinyApp(ui = ui, server = server)
