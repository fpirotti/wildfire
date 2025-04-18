#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("WildFireCE Loaded Data Panel"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel("For internal partner use only.", width=0,

        ),

        # Show a plot of the generated distribution
        mainPanel(
            DTOutput('table')
        )
    )
)
