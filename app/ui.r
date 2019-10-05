#
# This is the user-interface definition of a Shiny web application

library(shiny)
if (!require("shinythemes")) install.packages("shinythemes")
library(leaflet)
library(shinythemes)
# Need a better name for the app
shinyUI(fluidPage
    (theme = shinytheme("flatly"),
    navbarPage(
        "District Education Quality Assesment tool",
               tabPanel(
                   "Introduction",
                   sidebarLayout(
                       position = "right",
                       sidebarPanel("To add side pannel and elements"),
                       mainPanel( 
                           tags$div(
                           h1("** insert name of tool **"),
                           h3("Purpose of the tool"),
                           p("Using publicly available NY data, our tool allows users to visualize
                             the quality of schools per zipcode in the city. Different metrics such as
                             --- and --- go into how we score the quality of a school.")
                           ),
                           tags$div(
                           h3("Who are we creating value for?"),
                           tags$ul(
                               tags$li("Parents looking for the best school districts"),
                               tags$li("The education department looking at school districts that need more work"),
                               tags$li("Colleges looking to recruit students form different districts"),
                               tags$li("Educators in search of districts to work in"),
                               tags$li("Acticists looking at what districts to advocate for")
                           ),
                           tags$h3("Schools that can be analyzed using the tool"),
                           tags$p("Our app analyzes the following types of schools in Manhattan: "),
                           tags$ul(
                               tags$li("Pre-K schools"),
                               tags$li("Elementary schools"),
                               tags$li("High schools")
                           )
                           )
                   )
               )),
               tabPanel("How *tool name* works"),
               tabPanel("Pre-K schools"),
               tabPanel("Elementary School"),
               tabPanel("High School")
    )
))
