library(shiny)
library(ggplot2)
library(reshape2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$timePlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    plotdata <- melt(data = results2017[, c("Name", "Start", paste0("CPTime", seq(1:15)), "Finish")], id = "Name")
    plotdata <- subset(plotdata, !is.na(value))
    plotdata <- merge(x = plotdata, y = distances, by.x = "variable", by.y = "checkpoint", all = TRUE)
    plotdata$Name <- as.factor(plotdata$Name)
    ggplot(data = plotdata, aes(x = dist, y = value/60, group = Name)) + 
      geom_line(color = "blue", alpha = 0.2) +
      geom_line(data = subset(plotdata, Name == input$runnername), aes(x = dist, y = value/60), color = "red") +
      geom_segment(data = data.frame(dist = c(9.5, 19.8, 31.7, 48.7), time = c(2.5, 6, 8, 14), Name = rep("", 4)), aes(x = dist, y = time, xend = dist, yend = Inf), color = "yellow", alpha = 1) +
      theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.title.x = element_text(color = "white"),  
        axis.title.y = element_text(color = "white"),  
        panel.grid.major = element_line(color = rgb(0.3, 0.3, 0.3)),  
        panel.grid.minor = element_line(color = rgb(0.2, 0.2, 0.2)),
        legend.position = "none"
      ) +
      labs(x = "Distance (km)", y = "Running time (hours)")
    
  })
  
  output$posPlot <- renderPlot({
    positions <- data.frame(Name = subset(results2017, !is.na(Finish) & !(Name %in% c("Bjorn Verduijn", "Peter Toaig")))$Name)
    for(i in 1:16)
    {positions[, paste0("Position", i)] <- rank(subset(results2017, !is.na(Finish) & !(Name %in% c("Bjorn Verduijn", "Peter Toaig")))[, paste0("CPTime", i)])}
    plotdatapos <- melt(data = positions, id = "Name")
    # remove missing value at CP1 of Harry Kingston
    plotdatapos <- subset(plotdatapos, Name != "Harry Kingston" | variable != "Position1")
    plotdatapos <- merge(x = plotdatapos, y = distances, by.x = "variable", by.y = "checkpoint2", all = TRUE)
    ggplot(data = plotdatapos, aes(x = dist, y = value, group = Name)) + 
      geom_line(color = "blue", alpha = 0.4) +
      geom_line(data = subset(plotdatapos, Name == input$runnername), aes(x = dist, y = value), color = "red") +
      theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.title.x = element_text(color = "white"),  
        axis.title.y = element_text(color = "white"),  
        panel.grid.major = element_line(color = rgb(0.3, 0.3, 0.3)),  
        panel.grid.minor = element_line(color = rgb(0.2, 0.2, 0.2)),
        legend.position = "none"
      ) +
      labs(x = "Distance (km)", y = "Position among finishers")
    
  })
  
})
