#
# This is the user-interface definition of a Shiny web application

library(shiny)
library(varhandle)
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
                       position = "left",
                       sidebarPanel("Select the year you want data for:",
                                    checkboxGroupInput("checkGroup", 
                                                       h3("Year"), 
                                                       choices = list("2016" = 2016, 
                                                                      "2017" = 2017, 
                                                                      "2018" = 2018),
                                                       selected = 1),
                                    tags$hr(),
                                       textInput("text", h6("Select zipcode you want data for:"), 
                                                     value = "")   
                              
                       ),
                       mainPanel( 
                           tags$div(
                           h1("** insert name of tool **"),
                           h3("Purpose of the tool"),
                           p("Using publicly available NY data, our tool allows users to visualize
                             the quality of schools per zipcode in the city. Different metrics such as
                             emotional support and student's academic performance go into how we score the quality of a school.")
                           ),
                           leafletOutput("map"), 
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
               tabPanel("How *tool name* works", 
                        mainPanel(
                          tags$div(
                            h1("** insert name of tool **"),
                            h3("The Data"),
                            p("This dataset comes from the NYC department of education. It includes information about 
                              school emotional support, organization, teacher approval, as well as student performance 
                              after pre-k. While the released dataset includes information from 2014-2015 school year 
                              until the 2017-2018 school year, the first year is incomplete so only 
                              the three most recent years are used.")
                          ),
                          tags$div(
                          h3("Functionalities"),
                          p("The tool allows users to see a summary of the schools of each zip code in New York's 5 
                              borough for many different metrics. Since schools vary drastically 
                              depending on what grades they serve, the different age schools are 
                              on different tabs. In addition, users are able to query by year. ")
                          )
                        )
                      ),
               tabPanel("Pre-K schools",
                        sidebarPanel("Select the year you want data for:",
                                     checkboxGroupInput("checkGroup", 
                                                        h3("Year"), 
                                                        choices = list("2016" = 2016, 
                                                                       "2017" = 2017, 
                                                                       "2018" = 2018),
                                                        selected = 1),
                                     tags$hr(),
                                     selectInput("pre_k_metric", "Evaluation Metric:",
                                                 c("Enrollment" = "Enrollment",
                                                   "Emotional Support" = "CLASS Emotional Support Score",
                                                   "Instruction Quality" = "CLASS Instructional Support Score"
                                                 )
                                     ),
                                     tags$hr(),
                                     textInput("text", h6("Select zipcode you want data for:"), 
                                               value = "")
                        ),
                        mainPanel( 
                        leafletOutput("pre_k_map")
                        )
                        ),
               tabPanel("Elementary School",
                        sidebarPanel("Select the year you want data for:",
                                     checkboxGroupInput("checkGroup", 
                                                        h3("Year"), 
                                                        choices = list("2016" = 2016, 
                                                                       "2017" = 2017, 
                                                                       "2018" = 2018),
                                                        selected = 1),
                                     tags$hr(),
                                     selectInput("elem_metric", "Evaluation Metric:",
                                                 c("Enrollment" = "Enrollment",
                                                   "Overall Achievement" = "Student Achievement - Section Score",
                                                   "Supportive Environment" = "Supportive Environment - Element Score",
                                                   "Average Student English Proficiency" = "Average Grade 8 English Proficiency",
                                                   "Average Student Math Proficiency" = "Average Grade 8 Math Proficiency",
                                                   "Average Student Science Proficiency" = "Average Grade 8 Science Proficiency"
                                                 )
                                     ),
                                     tags$hr(),
                                     textInput("text", h6("Select zipcode you want data for:"), 
                                               value = "")
                        ),
                        mainPanel( 
                        leafletOutput("el_map"))
                        ),
               tabPanel("High School",
                        sidebarPanel("Select the year you want data for:",
                                     checkboxGroupInput("checkGroup", 
                                                        h3("Year"), 
                                                        choices = list("2016" = 2016, 
                                                                       "2017" = 2017, 
                                                                       "2018" = 2018),
                                                        selected = 1),
                                     tags$hr(),
                                     selectInput("hs_metric", "Evaluation Metric:",
                                                 c("Enrollment" = "Enrollment",
                                                   "Overall Achievement" = "Student Achievement - Section Score",
                                                   "Supportive Environment" = "Supportive Environment - Element Score",
                                                   "Average Student English Proficiency" = "Average Grade 8 English Proficiency",
                                                   "Average Student Math Proficiency" = "Average Grade 8 Math Proficiency",
                                                   "Average Student Science Proficiency" = "Average Grade 8 Science Proficiency"
                                                 )
                                     ),
                                     tags$hr(),
                                     textInput("text", h6("Select zipcode you want data for:"), 
                                               value = "")   
                        ),
                        mainPanel( 
                        leafletOutput("hs_map"))
                        )
    )
))
