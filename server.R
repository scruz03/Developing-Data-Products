library(shiny)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(googleVis)

# Define server logic for random distribution application
shinyServer(function(input, output) 

{
    #############
    ################  Render UI - Getting Round depending on Year ###############
    output$Round = renderUI({    
        
        round = ifelse( input$Year == 1989 | input$Year == 1993, "Unique",
                        as.data.frame(c("First","Ballotage")))
        round = as.data.frame(round)
        selectInput("Round","Step 2: Select a round:", round[,1])
    }) 
    
    ############################ Render UI Getting Counties
    ############################ Depending on Regions
    
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
    
    
    
    ############################################################################ 
    
    
    ###############
     # Calculo Nacional
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
    
    # Calculo Regional
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
    
    # Calculo Comunal
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

    # Calculo para totales regionales para los dos 1eros
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
    
    ######################################
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

    ########################### Tab 1 #######################################
    
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

######## Tab 5 County 1 ####################################################
# Generate an HTML table view of the data

output$table <- renderTable({
    data.frame(datos1C())
    
})

#######################################
########## Tab 6 County 2 ################

output$view3 = renderGvis({
  
    gvisColumnChart (datos1C(), 
                     xvar="Candidate", yvar="Votes")
    
    })     

#####################################

})