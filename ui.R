library(shiny)

                                        # Define UI for application that plots random distributions
shinyUI(pageWithSidebar(

                                        # Application title
    headerPanel("Stock chart"),

                                        # Sidebar with a slider input for number of observations
    sidebarPanel(
        numericInput("obs", "Which company to view:", 517385)
        ),

                                        # Show a plot of the generated distribution
    mainPanel(
        plotOutput("plot")
        )
    ))
