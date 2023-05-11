#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(shiny)
library(scales)

ui <- fluidPage(

    # Application title
    titlePanel("Density Plots"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            radioButtons("var", "Data",
                         c("Goals" = "g",
                           "Assists" = "a",
                           "Points" = "p"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
           plotOutput("distPlot"),
           
           verbatimTextOutput("distTable")
           
        )
    )
)

server <- function(input, output) {
  
  d <- reactive({
    switch(input$var,
           "g" = 8,
           "a" = 9,
           "p" = 10)
  })

    output$distPlot <- renderPlot({
      
      ggplot(data = nhl, mapping = aes(x = nhl[,d()], y = after_stat(density), fill = Pos, color = Pos)) +
        geom_density(alpha = 0.25) +
        theme_linedraw() +
        scale_color_manual(values = c("#FFA300", "#0082BA")) +
        scale_fill_manual(values = c("#FFA300", "#0082BA"), labels = c("Defense", "Forward")) +
        guides(color = "none") +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
              axis.title = element_text(size = 16),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 16),
              panel.background = element_rect(color = "#A7A8AA", linewidth = 5),
              panel.grid = element_line(color = "#A7A8AA")) +
        geom_vline(xintercept = mean(nhl[nhl$Pos == "F", d()]), color = "#0082BA") +
        geom_vline(xintercept = mean(nhl[nhl$Pos == "D", d()]), color = "#FFA300") +
        labs(x = "Data",
             y = "Density",
             title = "Density Plot for Data Conditioned by Position",
             fill = "Position")
        
    })
    
    output$distTable <- renderPrint({
      tab <- matrix(c(mean(nhl[nhl$Pos == "D", d()]), median(nhl[nhl$Pos == "D", d()]), sd(nhl[nhl$Pos == "D", d()]),
                      mean(nhl[nhl$Pos == "F", d()]), median(nhl[nhl$Pos == "F", d()]), sd(nhl[nhl$Pos == "F", d()])),
                    ncol = 2,
                    nrow = 3,
                    byrow = FALSE)
      
      
      colnames(tab) <- c("Defense", "Offense")
      rownames(tab) <- c("Mean", "Median", "Standard Deviation")
      
      tab
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
