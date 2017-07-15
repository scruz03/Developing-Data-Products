library(shiny)
library(shinythemes)


# Define UI for random distribution application 
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Results of Chilean Presidential Elections 1989-2013"),
    
    # Sidebar with controls to select the random distribution type
    # and number of observations to generate. Note the use of the br()
    # element to introduce extra vertical spacing
    sidebarPanel(
        selectInput("Year",'Step 1: Select year of election:', choices = levels(datos10$Year)),
        submitButton("Update View"),
        br(),  
        uiOutput("Round"),
        submitButton("Update View"),
        br(),
        selectInput("Region", 'Step 3: Select a region:', choices = unique(datos10$Region)),
        submitButton("Update View"),
        br(),
        uiOutput("County"),
        br(),
        submitButton("Update View"),
        width = 2 ),
      
      # Show a tabset that includes a plot, summary, and table view
      # of the generated distribution
      mainPanel(
          tabsetPanel(
            tabPanel("National level", htmlOutput("view1"), h5("Since 2012 the vote
                    is not mandatory but people can choose whether to vote or not."),
                     h5("Voting age population (VAP)"),tableOutput("tab1"),
                    htmlOutput("pieN")
                    ),
            tabPanel("Regional votes comparison",h5("The columns are colored red and blue to indicate the number 
                        of votes for the two leading candidates in the round"),
                     h5(textOutput("textR1")),h5(textOutput("textR2")),
                     htmlOutput("view4"), htmlOutput("view5")
                     ),
            tabPanel("Maps", h5("Coloured Maps by votes in regions"),htmlOutput("maps"),
                     h5("The regions are colored red and blue to indicate the number of votes
                    for the leading candidates in the round"),
                    h5(textOutput("cand1")),h5(textOutput("cand2")), 
                    h5("Note 1: Before 2007 Arica y Parinacota Region was included in Tarapaca Region"),
                    h5("Note 2: Before 2007 Los Rios Region was included in Los Lagos Region")),
            tabPanel("Regional level",h5("Please, select Region and press Update View"), h5(textOutput("textR4")), htmlOutput("reg4")
                     ),
            tabPanel("Table at county level",h5("Please, select County and press Update View"),tableOutput("table")
                     ),
            tabPanel("County level",h5("Please, select County and press Update View"), htmlOutput("view3")
                     )
           
          )
      )
))