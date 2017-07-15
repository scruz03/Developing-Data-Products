.libPaths(c( "C:/Users/Sergio/Documents/R/win-library/3.4", 
             "C:/Program Files/R/R-3.4.1/library"))


## Libraries needed 
library(shiny)
library(dplyr)
library(googleVis)
library(downloader)
library(openxlsx)

# Download the file
url = "https://www.servel.cl/wp-content/uploads/2016/01/resultados_elecciones_presidenciales_1989_al_2013_1_.xlsx"
download(url, "resultados_elecciones_presidenciales_1989_al_2013_1_.xlsx", mode = "wb")

# Reading the file
datos1 = read.xlsx ("resultados_elecciones_presidenciales_1989_al_2013_1_.xlsx")
# Selecting specific columns and manipulating data
datos2 = datos1 %>% select(3,8,10,12,15,17,22)


################## Simplifying column names

datos2 = datos2 %>% rename(Year = AÑO.ELECCION,
                           Round = VOTACION.PRESIDENCIAL,
                           County = NOMBRE.COMUNA,
                           Region = NOMBRE.REGION,
                           Sex = SEXO.MESA,
                           Candidate = NOMBRE.COMPLETO,
                           Votes = VOTACION.VALIDA)

########### Manipulating data: years of election ##################
## This is because it is the same election

datos4 =  datos2 %>% mutate(Year = replace(Year, 
                                           Year==2000, 1999))
datos5 =  datos4 %>% mutate(Year = replace(Year, 
                                           Year==2006, 2005))
datos6 =  datos5 %>% mutate(Year = replace(Year, 
                                           Year==2010, 2009))

datos7 =  datos6 %>% mutate(Round = replace(Round, 
                                            Round=="UNICA VOTACION", "Unique"))

datos8 =  datos7 %>% mutate(Round = replace(Round, 
                                            Round=="PRIMERA VOTACION", "First"))

datos9 =  datos8 %>% mutate(Round = replace(Round, 
                                            Round=="SEGUNDA VOTACION", "Ballotage"))


###### Year as a factor to facilitate manipulation in Shiny 
datos9 = datos9 %>%
    mutate(Year = as.factor(Year)
    )

### Adjusting region names, as later I want to use a map
##
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region == "METROPOLITANA DE SANTIAGO", "Metropolitana"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region == "DEL BIOBIO", "Bío-Bío"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region == "DE VALPARAISO", "Valparaíso"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region == "DE LA ARAUCANIA", "Araucanía"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region =="DEL MAULE", "Maule"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region == "DE LOS LAGOS", "Los Lagos"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region =="DE COQUIMBO", "Coquimbo"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region =="DE ANTOFAGASTA", "Antofagasta"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region == "DE TARAPACA", "Tarapacá"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region == "DE ATACAMA", "Atacama"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region =="DE MAGALLANES Y ANTARTICA CH.", "Magallanes"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region == "AISEN DEL GRAL. CARLOS IBAÑEZ", "Aisén"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region == "ARICA Y PARINACOTA", "Arica y Parinacota"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region =="DE LOS RIOS", "Los Ríos"))
datos9 =  datos9 %>% mutate(Region = replace(Region, 
                                             Region =="DEL LIBERTADOR BDO. O'HIGGINS", "O'Higgins"))

## I used regions codes because GoogleVis did not recognized all the names
## but it did recognized all the codes

data_CL = data.frame(Region_ID = c("CL-RM","CL-BI","CL-VS","CL-AR","CL-ML","CL-LI","CL-LL"
                                   ,'CL-CO','CL-LR','CL-AN','CL-TA','CL-AT', 'CL-AP','CL-MA',
                                   'CL-AI'), Region = c( "Metropolitana","Bío-Bío","Valparaíso","Araucanía",
                                                         "Maule","O'Higgins","Los Lagos", "Coquimbo","Los Ríos",
                                                         "Antofagasta", "Tarapacá","Atacama","Arica y Parinacota",
                                                         "Magallanes", "Aisén"))

#### I merge the data as the original file did not have the codes
datos10 = merge(datos9,data_CL)

## Change capital names
datos10$County = gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(datos10$County), perl=TRUE)
datos10$Candidate = gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(datos10$Candidate), perl=TRUE)

### Added extra information about tournout (participation) in different
## years
Year = as.integer(c(1989, 1993,1999,1999,2005,2005,2009,2009,2013,2013))
Round = c("Unique","Unique","First","Ballotage","First","Ballotage",
          "First","Ballotage","First","Ballotage")
VAP =as.integer(c(8239545,9052632,10126098, 10126098,11344218,
                  11344218,12268311,12268311,13153415,13153415))
Turnout = c(86.9,81.6,71.8,72.4,63.5,63.1,59.2,58.7,50.9,43.3)
Par =data.frame(Year,Round,VAP,Turnout)
Par


########### As all the data is ready, I start Shiny Server


# In the first part of this code, the program goes to the website of 
# the Chilean Electoral Service and download a specific file which has
# data about the Chilean Presidential elections since 1989 to 2013

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