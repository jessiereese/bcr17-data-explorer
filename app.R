#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# test edit for github

library(shiny)
library(tidyverse)
library(thematic)
library(wesanderson)


# Data prep --------------------------------------------------------------------

  # Pop Trend --------------------------------------------------------------------

  trend <- readRDS("IMBCR_TrendEStimates2021_BCR17all.rds")

  trend <- trend %>%
  mutate(across(where(is_character), as_factor))



  # Density ----------------------------------------------------------------------
  data <- readRDS("IMBCR_BayesianDensity2021_BCR17all.rds") %>%
             select(2:5,7:9,15) # drop unnecessary columns

  data$Year <- as.factor(data$Year)

# # Filter to species that occur in all 5 strata
# data.rm <- data %>%
#   group_by(Species) %>%
#   summarize(n = n_distinct(Stratum)) %>%
#   filter(n == 4)
# 
# data <- data %>%
#   filter(Species %in% data.rm$Species)

colnames(data) <- c("Stratum", "Species", "Year", "Density", 
                    "CV", "Lower CI", "Upper CI", 
                    "Detections")

data$Stratum <- droplevels(data$Stratum)
levels(data$Stratum) <- c("BCR17", "MT-BCR17", "ND-BCR17", "WY-BCR17", "SD-BCR17")

# Shiny app --------------------------------------------------------------------

# UI ---------------------------------------------------------------------------
ui <- navbarPage("IMBCR Data Explorer",
  
  theme = bslib::bs_theme(bootswatch = "litera"),
  
  # Intro ----------------------------------------------------------------------
  tabPanel("Introduction",
    
    fluidRow(
      column(1,),
      column(4,
             tagAppendAttributes(textOutput("intro"), style = "white-space:pre-wrap;") # allows for line breaks
      ),
      
      column(6,
             imageOutput("bcr17.strata")
      )
    )
  ),
           

  # Pop Trend ------------------------------------------------------------------
  tabPanel("Population Trend",
   
   fluidRow(
     column(1,),
     column(10,
            wellPanel(
              tagAppendAttributes(textOutput("trend_intro"), 
                                style = "white-space:pre-wrap;")) # allows for line breaks
     )
   ),   
   
   
   fluidRow(
     column(1,),
     column(10,
            wellPanel(
              textOutput("trend_instruction"))
     )
   ),
   

   fluidRow(
     column(1,),
     column(4,
            selectInput("sp_trend", label = "Species", 
                        choices = unique(trend$Species),
                        selected = "Chestnut-collared Longspur")),
     column(1,),
     column(4,
            selectInput("sp_trend2", label = "Species",
                        choices = unique(trend$Species),
                        selected = "Lark Bunting"))
   ),
   
   fluidRow(
     column(1,),
     column(4,
            plotOutput("sp_trend_plot")),
     column(1,),
     column(4,
            plotOutput("sp_trend_plot2"))
   )
           
           
  ),
  
  # Density --------------------------------------------------------------------
  tabPanel("Density",
    
   fluidRow(
     column(1,),
     column(10,
            wellPanel(
              tagAppendAttributes(textOutput("density_intro"), 
                                  style = "white-space:pre-wrap;")) # allows for line breaks
     )
   ),   
   
   
   fluidRow(
     column(1,),
     column(10,
            wellPanel(
              tagAppendAttributes(textOutput("density_instructions"), 
                                  style = "white-space:pre-wrap;")) # allows for line breaks
     )
   ),       
           
           
    fluidRow(
      column(4,
        selectInput("species", label = "Species", choices = unique(data$Species),
                    selected = "CCLO")),
      column(2,),
      column(4,
        selectInput("species2", label = "Species", choices = unique(data$Species), 
                    selected = "LARB"))
    ),
    
    fluidRow(
      column(6,
        plotOutput("bcr17.density.plot", width = "410px", height = "250px", click = "hover")),
      column(6,
        plotOutput("bcr17.density.plot2", width = "410px", height = "250px", click = "hover"))
    ),
    
    fluidRow(
      column(5,
        tableOutput("info")),
      column(1,),
      column(5,
        tableOutput("info2"))
    ),
    
    fluidRow(
      column(6,
             plotOutput("state.density.plot", width = "500px", height = "325px", click = "hover")),
      column(6,
             plotOutput("state.density.plot2", width = "500px", height = "325px", click = "hover"))
      
    ),
    
    fluidRow(
      column(5,
             tableOutput("info3")),
      column(1,),
      column(5,
             tableOutput("info4"))
    ),
    
  )
  
)



# Server -----------------------------------------------------------------------
server <- function(input, output, session){
  
  thematic::thematic_shiny()
  
  # Intro ----------------------------------------------------------------------
  
  output$intro <- renderText("Welcome to the Integrated Monitoring in Bird Conservation Regions (IMBCR) Data Explorer. 
  
The IMBCR program, coordinated by Bird Conservancy of the Rockies, is the second largest breeding bird monitoring program in North America, stretching across private and public land from the Great Plains to the Great Basin.
  
In this app, you can explore population trend and density estimates for birds detected in the Badlands and Prairies Bird Conservation Region (BCR 17) between 2008-2021. You can compare how population metrics differ between species and among the portions of BCR 17 falling in Montana, North Dakota, Wyoming, and South Dakota.
  
In the map on the right, each colored polygon represents a different land management unit (stratum) within BCR 17 where IMBCR has conducted bird surveys.
                             
                             ")
  
  
  output$bcr17.strata <- renderImage({
    list(src = "BCR-17-StrataExtent-2017.png",
         width = 500,
         height = 648)
  }, deleteFile = F)
  
  
  # Pop Trend ------------------------------------------------------------------
  
  output$trend_intro <- renderText("Population trend is the change in population size over time.
  
Trend estimates greater than 1 indicate the species is increasing in population size, while trends less than 1 indicate a species is declining. Estimates right around 1 indicate the population size is stable.")
  

  output$trend_instruction <- renderText("Select two species to compare their population trends in Bird Conservation Region 17.") 
  
  output$sp_trend_plot <- renderPlot({
    ggplot(data = trend %>%
             filter(Species == input$sp_trend),
           aes(x = Stratum, y = Mean)) +
      geom_point() +
      geom_errorbar(aes(ymax = UCI95, ymin = LCI95, width = 0.15)) +
      geom_hline(yintercept = 1, color = "gray70") +
      ylab("Mean Trend Estimate") +
      ylim(-0.5,2.5) +
      coord_flip() +
      theme_bw(base_size = 19)
  })
  
  output$sp_trend_plot2 <- renderPlot({
    ggplot(data = trend %>%
             filter(Species == input$sp_trend2),
           aes(x = Stratum, y = Mean)) +
      geom_point() +
      geom_errorbar(aes(ymax = UCI95, ymin = LCI95, width = 0.15)) +
      geom_hline(yintercept = 1, color = "gray70") +
      ylab("Mean Trend Estimate") +
      coord_flip() +
      ylim(-0.5,2.5) +
      theme_bw(base_size = 19)
  })
  
  
  
  # Density --------------------------------------------------------------------
  
  # Hierarchical select boxes
  
  # species <- reactive({
  #   data %>%
  #     filter(Species == input$species)
  # })
  # 
  # observeEvent(species(), {
  #   choices <- unique(species()$Stratum)
  #   updateSelectInput(inputId = "stratum", choices = choices)
  # })
  # 

  output$density_intro <- renderText("Density is the number of individuals per square kilometer.")
  
  output$density_instructions <- renderText("Select two species to compare their density estimates in Bird Conservation Region 17.
                                            
Click on any point to view a table of data associated with that estimate.")
  
  
  
  output$bcr17.density.plot <- renderPlot({
    ggplot(data = data %>%
             filter(Species == input$species,
                    Stratum == "BCR17"), 
           aes(x = Year, y = Density)) +
      geom_point(aes(color = Stratum)) +
      geom_errorbar(aes(ymax = `Upper CI`, ymin = `Lower CI`, width = 0.3, color = Stratum)) +
      scale_color_manual(values = wes_palette("Darjeeling1", n = 5)) +
      ylab(expression(Individuls/km^2)) +
      facet_wrap(~Stratum, nrow = 1) +
      guides(x = guide_axis(angle = 45)) +
      theme_bw()
  }, 
    res = 96)
  
  output$bcr17.density.plot2 <- renderPlot({
    ggplot(data = data %>%
             filter(Species == input$species2,
                    Stratum == "BCR17"), 
           aes(x = Year, y = Density)) +
      geom_point(aes(color = Stratum)) +
      geom_errorbar(aes(ymax = `Upper CI`, ymin = `Lower CI`, width = 0.3, color = Stratum)) +
      scale_color_manual(values = wes_palette("Darjeeling1", n = 5)) +
      ylab(expression(Individuls/km^2)) +
      facet_wrap(~Stratum, nrow = 1) +
      guides(x = guide_axis(angle = 45)) +
      theme_bw()
  }, 
  res = 96)
  
  output$state.density.plot <- renderPlot({
    ggplot(data = data %>%
             filter(Species == input$species,
                    Stratum != "BCR17"), 
           aes(x = Year, y = Density)) +
      geom_point(aes(color = Stratum)) +
      geom_errorbar(aes(ymax = `Upper CI`, ymin = `Lower CI`, width = 0.3, color = Stratum)) +
      scale_color_manual(values = wes_palette("Darjeeling1", n = 5)) +
      ylab(expression(Individuls/km^2)) +
      facet_wrap(~Stratum, nrow = 2) +
      guides(x = guide_axis(angle = 45)) +
      theme_bw()
  }, 
  res = 96)
  
  output$state.density.plot2 <- renderPlot({
    ggplot(data = data %>%
             filter(Species == input$species2,
                    Stratum != "BCR17"), 
           aes(x = Year, y = Density)) +
      geom_point(aes(color = Stratum)) +
      geom_errorbar(aes(ymax = `Upper CI`, ymin = `Lower CI`, width = 0.3, color = Stratum)) +
      scale_color_manual(values = wes_palette("Darjeeling1", n = 5)) +
      ylab(expression(Individuls/km^2)) +
      facet_wrap(~Stratum, nrow = 2) +
      guides(x = guide_axis(angle = 45)) +
      theme_bw()
  }, 
  res = 96)
  
  
  output$info <- renderTable({
    nearPoints(data%>%
                 filter(Species == input$species),
               input$hover)
  },
  align = "c",
  width = "80%")
  
  
  output$info2 <- renderTable({
    nearPoints(data%>%
                 filter(Species == input$species2),
               input$hover)
  },
  align = "c",
  width = "80%")
  
  
  output$info3 <- renderTable({
    nearPoints(data%>%
                 filter(Species == input$species),
               input$hover)
  },
  align = "c",
  width = "80%")
  
  
  output$info4 <- renderTable({
    nearPoints(data%>%
                 filter(Species == input$species2),
               input$hover)
  },
  align = "c",
  width = "80%")
  
  
}


shinyApp(ui = ui, server = server)

