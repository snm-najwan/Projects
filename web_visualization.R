#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)
suprmkt_data <- read.csv(file.choose())

dfy = data.frame(suprmkt_data)
dfy$Datee = as.Date(dfy$Date,"%m/%d/%Y")

dfy$Date = dfy$Datee
dfy = subset(dfy,select = -Datee)

dfy = subset(dfy,select = -c(Time,gross.margin.percentage))
data <- dfy[,c(1:dim(dfy)[2])]



levels(data$Gender) <- c("Male", "Female")
levels(data$Branch) <- c("A", "B", "C")
levels(dfy$City) <- c("Yangon", "Naypyitaw", "Mandalay" )
levels(data$Customer.type) <- c("Member", "Normal")
levels(data$Product.line) <- c("Health and beauty", "Electronic accessories", "Home and lifestyle", "Sports and travel", "Food and beverages", "Fashion accessories")
levels(data$Payment) <- c("Ewallet", "Cash", "Credit card")
# ==========

# Define UI of Shiny App
ui <- fluidPage(
  titlePanel("Super Market Visualization"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("continuous_var1", "Continuous Variable:", choices = colnames(data[,c(7, 8, 9, 10, 13, 14, 15)])),
      selectInput("summary_stat1", "Summary Statistic:", choices = c("Total", "Count")),
      selectInput("categorical_var1", "Categorical Variable:", choices = colnames(data[,c(2, 3, 4, 5, 6)])),
      checkboxInput("show_labels1", "Show Labels", value = TRUE),
      
      sliderInput("font_size1", "Font Size:", min = 5, max = 10, value = 8)
      # actionButton("update", "Update Chart")
    ),
    
    mainPanel(
      plotOutput("chart1")

    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("continuous_var2", "Continuous Variable:", choices = colnames(data[,c(7, 8, 9, 10, 13, 14, 15)])),
      
      # selectInput("summary_stat2", "Summary Statistic:", choices = c("Total", "Count")),
      selectInput("categorical_var2", "Categorical Variable:", choices = colnames(data[,c(2, 3, 4, 5, 6)])),
      # checkboxInput("show_labels2", "Show Labels", value = TRUE),
      sliderInput("bins1", "Bin width:", min = 1, max = 100, value = 30),
      sliderInput("font_size2", "Font Size:", min = 5, max = 10, value = 6)
      # actionButton("update", "Update Chart")
    ),
    
    mainPanel(
      
      plotOutput("chart2")
      
    )
  ),
  sidebarLayout(
    sidebarPanel(
      # selectInput("continuous_var", "Continuous Variable:", choices = colnames(data[,c(7, 8, 9, 10, 13, 14, 15)])),
      # selectInput("continuous_var1", "Continuous Variable:", choices = colnames(data[,c(7, 8, 9, 10, 13, 14, 15)])),
      # selectInput("summary_stat3", "Summary Statistic:", choices = c("Total", "Count")),
      selectInput("categorical_var3", "Categorical Variable:", choices = colnames(data[,c(2, 3, 4, 5, 6)])),
      # checkboxInput("show_labels3", "Show Labels", value = TRUE),
      
      sliderInput("font_size3", "Font Size:", min = 5, max = 10, value = 6)
      # actionButton("update", "Update Chart")
    ),
    
    mainPanel(
      
      plotOutput("chart3")
      
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("continuous_var4", "Continuous Variable:", choices = colnames(data[,c(7, 8, 9, 10, 13, 14, 15)])),
      selectInput("continuous_var5", "Continuous Variable:", choices = colnames(data[,c(7, 8, 9, 10, 13, 14, 15)])),
      # selectInput("summary_stat4", "Summary Statistic:", choices = c("Total", "Count")),
      selectInput("categorical_var4", "Categorical Variable:", choices = colnames(data[,c(2, 3, 4, 5, 6)])),
      # checkboxInput("show_labels4", "Show Labels", value = TRUE),
      
      sliderInput("font_size4", "Font Size:", min = 5, max = 10, value = 6)
      # actionButton("update", "Update Chart")
    ),
    
    mainPanel(
      
      
      plotOutput("chart4")
    )
  )
  
  
  
)






# Define server
server <- function(input, output) {
  
  # Create pie chart
  output$chart1 <- renderPlot({
    continuous_var1 <- input$continuous_var1
    summary_stat1 <- input$summary_stat1
    categorical_var1 <- input$categorical_var1
    show_labels1 <- input$show_labels1
    
    summary_data <- data %>%
      group_by(!!sym(categorical_var1)) %>%
      summarize(value = switch(summary_stat1,
                               "Total" = sum(!!sym(continuous_var1)),
                               "Count" = n()))
    
    p <- ggplot(summary_data, aes(x = "", y = value, fill = !!sym(categorical_var1))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme(legend.text = element_text(size = input$font_size1 + 4),
            axis.text = element_text(size = input$font_size1 + 4)) + 
      labs(title = paste("Pie Chart of", continuous_var1, "per", categorical_var1), size = input$font_size1)
    
    if (show_labels1) {
      p <- p + geom_text(aes(label = paste0(round(value, 2), ifelse(summary_stat1 == "Average" || summary_stat1 == "Count", "", ""))), 
                         position = position_stack(vjust = 0.5), color = "black", size = input$font_size1, show.legend = FALSE)
    }
    
    p
    
  })
  
  # Create histogram
  output$chart2 <- renderPlot({
    continuous_var2 <- input$continuous_var2
    # summary_stat2 <- input$summary_stat2
    categorical_var2 <- input$categorical_var2
    # show_labels2 <- input$show_labels2
    bins <- input$bins1

    # group_by(!!sym(categorical_var2)) %>%
    # summarize(value = switch(summary_stat2,
    #                            "Total" = sum(!!sym(continuous_var2)),
    #                            "Count" = n()))
    
    ggplot(data, aes(!!sym(continuous_var2))) +
      geom_histogram(binwidth = bins, fill = "steelblue", color = "white") +
      theme(legend.text = element_text(size = input$font_size2 + 4),
            axis.text = element_text(size = input$font_size2 + 4)) +
      labs(title = paste("Histogram of", continuous_var2), size = input$font_size)
    
    # if (show_labels2) {
    #   q <- q + geom_text(aes(label = paste0(round(value, 2), ifelse(summary_stat2 == "Average" || summary_stat2 == "Count", "", ""))), 
    #                      position = position_stack(vjust = 0.5), color = "black", size = input$font_size2, show.legend = FALSE)
    # }
    # 
    # q
    
  })

  # Create bar chart
  output$chart3 <- renderPlot({
    categorical_var3 <- input$categorical_var3
    continous_var3 <- input$continous_var3
    # summary_stat3 <- input$summary_stat3
    # show_labels3 <- input$show_labels3
    
    
    # group_by(!!sym(categorical_var3)) %>%
    #   summarize(value = switch(summary_stat3,
    #                            "Total" = sum(!!sym(continuous_var3)),
    #                            "Count" = n()))
    
    ggplot(data, aes(x = !!sym(categorical_var3), fill = !!sym(categorical_var3))) +
      geom_bar(width = 0.5) +
      stat_count(geom = "text", aes(label = paste0(round((..count..) / sum(..count..) * 100, 2), "%")),
                 position = position_stack(vjust = 0.5), color = "black", size = input$font_size3, show.legend = FALSE) +
      theme(legend.text = element_text(size = input$font_size3 + 4),
            axis.text = element_text(size = input$font_size3 + 4)) +
      labs(title = paste("Bar chart of", categorical_var3), size = input$font_size)
    # if (show_labels3) {
    #   k <- k + geom_text(aes(label = paste0(round(value, 2), ifelse(summary_stat3 == "Average" || summary_stat3 == "Count", "", ""))), 
    #                      position = position_stack(vjust = 0.5), color = "black", size = input$font_size3, show.legend = FALSE)
    # }
    # 
    # k
    
    
  })
  # scatter plot
  output$chart4 <- renderPlot({
    continuous_var4 <- input$continuous_var4
    continuous_var5 <- input$continuous_var5
    categorical_var4 <- input$categorical_var4
    # summary_stat4 <- input$summary_stat4
    # show_labels4 <- input$show_labels4

    
    # group_by(!!sym(categorical_var4)) %>%
    # summarize(value = switch(summary_stat4,
    #                            "Total" = sum(!!sym(continuous_var4)),
    #                            "Count" = n()))
    
    
    ggplot(data, aes(x=!!sym(continuous_var4),y=!!sym(continuous_var5)))+
      geom_point(size = 2, shape = 23, color = "red") +
      theme(legend.text = element_text(size = input$font_size4 + 4),
            axis.text = element_text(size = input$font_size4 + 4)) +
      labs(title = paste("scatterplot of", continuous_var4), size = input$font_size4)
    
    # if (show_labels4) {
    #   v <- v + geom_text(aes(label = paste0(round(value, 2), ifelse(summary_stat4 == "Average" || summary_stat4 == "Count", "", ""))), 
    #                      position = position_stack(vjust = 0.5), color = "black", size = input$font_size4, show.legend = FALSE)
    # }
    # 
    # v
    
  })
    
  
}

# Run the app
shinyApp(ui, server)