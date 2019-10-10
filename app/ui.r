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
            "DEQAT",
            tabPanel(
              "Introduction",
              mainPanel( 
                tags$div(
                  h1("DEQAT - District Education Quality Assesment Tool"),
                  h3("Purpose of the tool"),
                  p("Using publicly available NY data, our tool allows users to visualize
                             the quality of schools per zipcode in the city. Different metrics such as
                             emotional support and student's academic performance go into how we score the quality of a school.")
                ),
                leafletOutput("num_schools_map"), 
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
                ),
                tags$div(
                  h2("How DEQAT works"),
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
            tabPanel("Pre-K Schools",
                     sidebarPanel(
                       tags$div(
                         tags$p("What are the filtering metrics:"),
                         tags$ul(
                           tags$li(tags$b("Enrollment:"), "Enrollment in the schools"),
                           tags$li(tags$b("Emotional Support:"), "*add explanation*"),
                           tags$li(tags$b("Instructional Support:"), "*add explanation*"),
                           tags$li(tags$b("ECERS score:")," a classroom assessment tool designed to measure the quality 
                            of group programs for infants and toddlers"
                            ),
                           tags$li(tags$b("Year:"), "The year the data is pulled from"),
                           tags$li(tags$b("Enrollment:"), "Average enrollment in the schools")
                           
                         ),
                         tags$p(" ")
                         ),
                       radioButtons(inputId="pre_k_year", label="Select the year you want data for:", 
                                               choices=c(2016, 2017, 2018), inline = TRUE),
                                  tags$hr(),
                                  textInput("zip_pk", "Explore the schools in a zip code:", 
                                            value = ""),
                                  tags$hr(),
                                  sliderInput("prek_number", "Select the number of schools to display in the zip code:", min = 0, max = 10, value = 5),
                                  actionButton('pk_submit', 'Submit', icon = NULL, width = NULL),
                       
                       h3("Info. Of"),
                       p("Student Achievement:",textOutput("click_prek1",inline = T)),
                       p("Rigrous Instruction:",textOutput("click_prek2",inline = T)),
                       h4("Impact and Performance"),
                       h5("Based on all student achievement metrics, impact measures a school's teaching ability
                    against expected outcomes, adjusted for incoming student factors. Performance is the
                    unadjusted outcomes."),
                       br(),
                       h6("Demographic Categories"),
                       
                       
                       
                     ),
 
                     mainPanel( 
                       tags$div(
                         p("On this page, the tool displays some statistics for pre-k schools. You 
                            can choose which factor to use to vary the heat on the map" )
                           
                           
                         ),
                       leafletOutput("pre_k_map",height = 1200)
                     )
            ),
            tabPanel("Grade Schools",
                     sidebarPanel(
                       tags$div(
                         tags$p("Explanation on the filtering metrics:"),
                         tags$ul(
                           tags$li(tags$b("Type of school")," The type of grade school to display data for"
                           ),
                           tags$li(tags$b("Enrollment:"), "Average enrollment in the schools"),
                           tags$li(tags$b("Year:"), "The year the data is pulled from"),
                           tags$li(tags$b("Zipcode:"), "The zipcode you want display data for"),
                           tags$li(tags$b("Student Achievment"), "*add definition*"),
                           tags$li(tags$b("Rigorous Instruction"), "*add definition*")
                           
                           
                           ),
                         p("You can also filter the number of schools you want to view data for")
                         ),
                       radioButtons("school type", label="Type of school:",
                                               choices = c("Elementary" = "Elementary", 
                                                           "Middle" = "Middle", 
                                                           "K-8" = "K-8",
                                                           "High School" = "High School"),
                                               selected = "Elementary", inline = TRUE),
                       radioButtons(inputId="gd_year", label="Select the year you want data for:", 
                                               choices=c(2016, 2017, 2018), inline = TRUE),
                       textInput("zip_s", "Explore the schools in a zip code:", 
                                            value = ""),
                       sliderInput("s_number", "Select the number of schools to display in the zip code:", min = 0, max = 10, value = 5),

                       actionButton('s_submit', 'Submit', icon = NULL, width = NULL),
                       
                       h3("Info. Of"),
                       p("Student Achievement:",textOutput("click_sa",inline = T)),
                       p("Rigrous Instruction:",textOutput("click_sb",inline = T)),
                       h4("Impact and Performance"),
                       h5("Based on all student achievement metrics, impact measures a school's teaching ability
                    against expected outcomes, adjusted for incoming student factors. Performance is the
                    unadjusted outcomes."),
                       br(),
                       h6("Demographic Categories"),

                       
                       
                       
                       
                       
                       
                       
                       
                                  ),

                     mainPanel( 
                       tags$div(
                         p("On this page, the tool displays some statistics for grade schools. You 
                           can choose which factor to use to vary the heat on the map" )
                         
                         ),
                       leafletOutput("grade_map",height = 1200))
            )
          )
        ))