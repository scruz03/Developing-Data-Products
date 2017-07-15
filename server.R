.libPaths(c( "C:/Users/Sergio/Documents/R/win-library/3.4", 
             "C:/Program Files/R/R-3.4.1/library"))
#### Libraries needed 
require(shiny)
require(dplyr)
require(googleVis)
require(readxl)


# Reading the file
#datos1 = read.xlsx ("resultados_elecciones_presidenciales_1989_al_2013_1_.xlsx")
destfile = "resultados_elecciones_presidenciales_1989_al_2013_1_.xlsx"
f1 = read_xlsx(destfile,sheet="Datos",col_names = TRUE,col_types = NULL )
datos1 = as.data.frame(f1)
# Selecting specific columns and manipulating data
datos2 = datos1 %>% select(3,8,10,12,15,17,22)

################## Simplifying column names

datos2 = datos2 %>% rename(Year = "AÑO ELECCION",
                               Round = "VOTACION PRESIDENCIAL",
                               County = "NOMBRE COMUNA",
                               Region = "NOMBRE REGION",
                               Sex = "SEXO MESA",
                               Candidate = "NOMBRE COMPLETO",
                               Votes = "VOTACION VALIDA")

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
# Define server logic for random distribution application
shinyServer(function(input, output) 

{

    # One thing I needed was to incorporate a dynamic code because
    # some years had first and second round, but others not
    # I used render UI
    ##  Render UI - Getting Round depending on Year
    output$Round = renderUI({    
        
        round = ifelse( input$Year == 1989 | input$Year == 1993, "Unique",
                        as.data.frame(c("First","Ballotage")))
        round = as.data.frame(round)
        selectInput("Round","Step 2: Select a round:", round[,1])
    }) 
    
    # I also used Render UI because I did want to display Counties
    # associated to their regions
    # Render UI showing Counties depending on Regions
    
    output$County <- renderUI({
        
        A = ifelse(input$Year >= 2008 & input$Region %in% "Arica y Parinacota",
                   as.data.frame(c("Arica","Putre","General Lagos","Camarones")),
                   ifelse(input$Year < 2008 & input$Region %in% "Arica y Parinacota",
                          c("Change year because this region was created in 2007"),
                          ifelse(input$Year>= 2008 & input$Region %in% "Los Ríos",
                                 as.data.frame(c("Corral","Lanco","Los Lagos","Máfil",
                                                 "Mariquina","Paillaco","Panguipulli","Valdivia",
                                                 "Futrono","La Unión","Lago Ranco","Río Bueno")),
                                 ifelse(input$Year< 2008 & input$Region %in% "Los Ríos",
                                        c("Change year because this region was created in 2007"),
                                        datos10 %>%
                                            filter(Year %in% input$Year)  %>%
                                            filter(Round %in% input$Round) %>%
                                            filter(Region %in% input$Region) %>%
                                            group_by(County) %>%
                                            summarise(Votes = sum(Votes)) %>%
                                            as.data.frame()
                                 ))))  
        counties = as.data.frame(A)
        selectInput("County", "Step 4: Select a county:", counties[,1])
    })
    
    
    
    ### I start preparing the data for the different tabs
    # National level calculations, depending on Year and Round
    # I get the National results
    
    datos1N = reactive({ 
    datos88 = datos10 %>%
        filter(Year %in% input$Year) %>%
        filter(Round %in% input$Round)
    datosN1 = datos88 %>%
        group_by(Candidate) %>%
        summarise(Votes = sum(Votes)) %>%
        arrange(-Votes)
    datosN1 = as.data.frame(datosN1)
        })
    
    # Depending on Year, Round and show regional results for all the regions
    # Regional level calculations
    datos1R = reactive({ 
    datos77 = datos10 %>%
        filter(Year %in% input$Year) %>%
        filter(Round %in% input$Round)%>%
        filter(Region %in% input$Region)
    datosR1 = datos77 %>%
        group_by(Candidate) %>%
        summarise(Votes = sum(Votes)) %>%
        arrange(-Votes)
    datosR1 = as.data.frame(datosR1)
    
    })
    
    # I need to get County first
    # County level calculations
    datos1C = reactive({  
                datos33 = datos10 %>%
                filter(Year %in% input$Year) %>%
                filter(County %in% input$County) %>%
                filter(Round %in% input$Round)
                datos51 = datos33 %>%
                    group_by(Candidate) %>%
                    summarise(Votes = sum(Votes)) %>%
                    arrange(-Votes)
                datos52 = as.data.frame(datos51)
                n = nrow(datos52)
                datos45 = datos33[1:n,]
                datos46 = datos45 %>%
                select(1:4)
                datos48 = cbind(datos46, datos52)
        
    })

    # Calculating the votes for the two leading candidates of each Round
    # and each Year
    datosTR1 = reactive({  
            datosA = datos10 %>%
            filter(Year %in% input$Year) %>%
            filter(Round %in% input$Round)
            datosA1 = datosA %>%
            group_by(Candidate) %>%
            summarise(Votes=sum(Votes)) %>%
            arrange(-Votes)
            datosA1 = as.data.frame(datosA1)
            datosB = datosA %>%
            group_by(Candidate, Region,Region_ID) %>%
            summarise(Votes=sum(Votes)) %>%
            arrange(-Votes)
            datosB = as.data.frame(datosB)
            datosC = datosB %>%
            filter(Candidate %in% datosA1[1,1])
        })

    datosTR2 = reactive({  
            datosA = datos10 %>%
            filter(Year %in% input$Year) %>%
            filter(Round %in% input$Round)
            datosA1 = datosA %>%
            group_by(Candidate) %>%
            summarise(Votes=sum(Votes)) %>%
            arrange(-Votes)
            datosA1 = as.data.frame(datosA1)
            datosB = datosA %>%
            group_by(Candidate, Region,Region_ID) %>%
            summarise(Votes=sum(Votes)) %>%
            arrange(-Votes)
            datosB = as.data.frame(datosB)
            datosC = datosB %>%
            filter(Candidate %in% datosA1[2,1])
    })
    
    ###### This code prepare the data for the tournout details
    dat2N = reactive({  
       
        
        tab3 = Par %>%
            filter(Year %in% input$Year) %>%
            filter(Round %in% input$Round)
        tab3
    })
    ######################################
    dat3N = reactive({  
        tab1 = datos10 %>%
            filter(Year %in% input$Year) %>%
            filter(Round %in% input$Round)
        datosA1 = tab1 %>%
            group_by(Candidate) %>%
            summarise(Votes=sum(Votes))
        datosA1 = as.data.frame(datosA1)
        votes1 = sum(datosA1$Votes)
        Vot1 =c(votes1)
        Population = Par %>%
            filter(Year %in% input$Year)%>%
            filter(Round %in% input$Round)
        pie2 = data.frame(Vot1,Population)
        Item = c("Tournout","Not voting")
        Total =c(Vot1,pie2$VAP-Vot1)
        pie3 = data.frame(Item,Total)
        pie3
    })
    
#######################################################################   
    
    # Generate outputs

    ########################### Tab 1: National level ##
    
        output$view1 = renderGvis({
            validate(
                need(input$Round != '', 'Please choose a Year, Round, Region and County !')
            )          
             bar1 = gvisBarChart (datos1N(), 
                        xvar="Candidate", yvar="Votes")
                                })
        
        output$tab1 =  renderTable({
            validate(
                need(input$Round != "" , 'Please choose a Year, Round, Region and County !')
            )
            data.frame(dat2N())
        })
        
        output$pieN <- renderGvis({
            validate(
                need(input$Round != '', 'Please choose a Year, Round, Region and County !')
            )
            
            pie = gvisPieChart(dat3N(),labelvar = dat3N()$Item,
                               numvar = dat3N()$Total, 
                               options=list(width=400, height=225))
        })   
        
        
    ##################################  Tab 2 Regional ####################
       
         output$textR1 <- renderText({
           
            a1 = paste("Votes for candidate (in red):", 
                       datosTR1()[1,1])
            
        })
        output$textR2 <- renderText({
            validate(
                need(input$Year != "", 'Please choose a Year and a Round!'),
                need(input$Round != "", 'Please choose a Year and a Round!')
            ) 
            a2= paste("Votes for candidate (in blue):", 
                      datosTR2()[1,1])
            
        })
        
        output$view4 = renderGvis({
            validate(
                need(input$Region != "", "Please choose a Region!")
            )             
            gvisColumnChart (datosTR1(), 
                             xvar="Region", yvar="Votes",
                             options=list(
                                 legend="none",
                                 lineWidth=2, pointSize=0,
                                 title="Votes by Region", vAxis="{title:'Votes'}",
                                 hAxis="{title:'Regions'}",
                                 colors= "['#cd3333']",
                                 width=1000, height=300))
        }) 
        
        output$view5 = renderGvis({
                      
            gvisColumnChart (datosTR2(), 
                             xvar="Region", yvar="Votes",
                             options=list(
                                 legend="none",
                                 lineWidth=2, pointSize=0,
                                 title="Votes by Region", vAxis="{title:'Votes'}",
                                 hAxis="{title:'Regions'}",
                                 colors= "['#0000FF']",
                                 width=1000, height=300))
        })         
        

########################## Tab 3 Maps ###################################################        
    
        output$cand1 <- renderText({
         
            a1 = paste("Votes for candidate (in red):", 
            datosTR1()[1,1])
   
     })

    output$cand2 <- renderText({
         
           a2= paste("Votes for candidate (in blue):", 
              datosTR2()[1,1])
    
    })

        output$maps <- renderGvis({
     
                map1 = gvisGeoChart(datosTR1(),
                 locationvar="Region_ID", colorvar="Votes", hovervar = "Region",
                 options=list(region="CL", displayMode="regions", 
                              resolution="provinces",keepAspectRatio = TRUE,
                              datalessRegionColor = "#fcfcfc",
                              colorAxis="{colors:['#ffebcd', '#cd3333']}"
                                ))     
                map2 = gvisGeoChart(datosTR2(),
                 locationvar="Region_ID", colorvar="Votes", hovervar = "Region",
                 options=list(region="CL", displayMode="regions", 
                              resolution="provinces",keepAspectRatio = TRUE,
                              datalessRegionColor = "#fcfcfc",
                              colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                               ))
                merge = gvisMerge(map1,map2,horizontal=TRUE,tableOptions="cellspacing=10")
                
                return(merge)
                
                })
        ########################## Tab 4 - pie Regional
        
        
        output$textR4 <- renderText({
            validate(
                need(input$Region != '', 'Please choose a Year, Round, Region and County !')
            )     
            a4 = paste("The following is the pie of the electoral results in the Region of:", 
                       datos1C()[1,1])
            
        }) 
        
        output$reg4 <- renderGvis({
            validate(
                need(input$Region != '', 'Please choose a Year, Round, Region and County !')
            )            
            pie = gvisPieChart(datos1R(),labelvar = datos1R()$Candidate,
                               numvar = datos1R()$Votes, 
                               options=list(width=800, height=450))
        })    
        
    ######## Tab 5 County details a Table with the results
    # Generate an HTML table view of the data

        output$table <- renderTable({
            data.frame(datos1C())
                    })

    ########## Tab 6 County details a Coulmn Chart

        output$view3 = renderGvis({
  
        gvisColumnChart (datos1C(), 
                     xvar="Candidate", yvar="Votes")
    
    })     

    ######## This is the end, thanks for your attention

})