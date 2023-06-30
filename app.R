# UI_EVI_Avian_Influenza
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.


library(shiny)    
library(shinydashboard)
# library(EVI)
library(ggplot2)
library(dplyr)

ui <- dashboardPage(
  
  skin = "black",
  
  # HEADER
  
  dashboardHeader(
    title = "Epidemic Volatility Index | Avian Influenza",
    titleWidth = 900
    
  ),
  
  # SIDEBAR
  dashboardSidebar(
    width = 300,
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    
    sidebarUserPanel("Version 1.0"),
    
    sidebarMenu(id ="tabs",
      menuItem("Time Period", tabName = "Time_Period", icon = icon("bar-chart")),
              dateRangeInput("rdates_Global", "Range of dates for Avian influenza outbreaks:", start =nth( as.Date(EVI_Global$Days, origin="1970-01-01"), n =-365), end = max(as.Date(EVI_Global$Days, origin="1970-01-01"))), # input$rdates_Global
              
              
      
            
       #menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board")),
      #menuItem("About", tabName = "about", icon = icon("question")),
      hr(),
      menuItem("Plot Features", tabName = "dashboard", icon = icon("line-chart"),
               
               checkboxInput("rlines", "Draw lines instead of points (EVI plots)", value = FALSE), # input$rlines==FALSE
               checkboxInput("rlog", "Logarithmic scale", value = FALSE)
               
               #      numericInput("rsize", "Size of points", value = 1.5) # input$rsize
               
              )
      
               
      

      
      
    ),
    
    hr(),
    h4("References"),
    helpText(tags$b(""),"Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021)",
             tags$a(href="https://www.nature.com/articles/s41598-021-02622-3", 
                    "Read it here")),
    helpText(tags$b(""),"Meletis E. et al. EVI R-package: Epidemic Volatility Index as an Early-Warning Tool",
             tags$a(href="https://cran.r-project.org/web/packages/EVI/EVI.pdf", 
                    "R-package Info")),
    helpText(tags$b(""),"Pateras K., et al. The convergence epidemic index (cEVI) an early warning tool for identifying waves in an epidemic.",
             tags$a(href="https://www.sciencedirect.com/science/article/pii/S2468042723000349", 
                    "Read it here")),
    
    
    tabPanel("Info", tabName = "Widgets", icon = icon("th"),
             
             h4("Figures Explained"),
             h5("Upper left:"),
             h6("Observations (updated daily), presented on the original scale, with red dots corresponding to dates that, according to EVI, an early warning was issued."),
             h5("Upper right:"),
             h6("Observations (updated daily), presented on the logarithmic scale, which facilitates the comparison of the steepness of the epidemic curve between the different waves."),
             hr(),
             h5("Model Predictive value"),
             h6("Positive predictive value (PPV) for the days that an early warning was issued. Higher color intensity corresponds to PPV closer to the value of 1."),
             h6("Negative predictive values (NPV) for the days that an early warning was not issued. Higher color intensity corresponds to NPV closer to the value of 1.")
             
    )
  ),
  
  
  dashboardBody(
    
    tabBox(width=NULL,
    
      tabPanel(h5("Avian Influenza"),
               fluidRow(
                 box(plotOutput("boxGlobal1"))#,
                 #box(plotOutput("boxGlobal2"))
               )
      ),

  


    )
  )
)


#server.R


server <- function(input, output) {
  
evirlap <- function(Index1,Index2, ln=T, type="p",size.index=1,
                      Index1.lab="EVI1",Index2.lab="EVI2",Index3.lab="EVI-",Index.country=NULL, origin="1970-01-01") {
    
    Index1$Days = as.Date(Index1$Days, origin="1970-01-01")
    Index1$Index=Index1$Index
    Index2$Days = as.Date(Index2$Days, origin="1970-01-01")
    Index2$Index=Index2$Index*2
    Index=Index1
    Index$Index=Index$Index+Index2$Index
    if(length(table(Index$Index))<3)
      Index$Index[1:3]<-1:3
    Index$cases_1=Index$Cases*Index$Index
    Index$cases_1[Index$cases_1 == 0] <- NA
    Index$cases_0=Index$Cases*(1-Index$Index)
    Index$cases_0[Index$cases_0 == 0] <- NA
    
    Index$npv=Index$npv*(1-Index$Index)
    Index$npv[Index$npv == 0] <- NA
    Index$ppv=Index$ppv*Index$Index
    Index$ppv[Index$ppv == 0] <- NA
    Index$variable<-"x"
    Index$Index[is.na(Index$Index)]<-0
    Index$Index<-factor(Index$Index,labels = c("No warning",paste(Index1.lab,"alone"),paste(Index2.lab,"alone"), Index3.lab))
    if (ln==F) {
      sp3<-ggplot(Index, aes_string(x="Days",group="variable"))+
        list(
          geom_point(aes_string(y=("Cases"), color="Index"), size=size.index),
          #scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
          scale_colour_grey(start = 1,end = 0),
          scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
          labs(title = paste0("Graph combining outputs ",Index1.lab,", ", Index2.lab," and ", Index3.lab," - ",Index.country), y = "Cases", x="Days"),
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size=8),
                legend.key.height = unit(0, 'cm')),
          if (type=="l")  geom_path(aes_string(y="Cases",colour="Index"),size=size.index)
        )
    }
    
    if (ln==T) {
      sp3<-ggplot(Index, aes_string(x="Days",group="variable"))+
        list(
          geom_point(aes_string(y="log(Cases)", color="Index"), size=size.index),
          #scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
          scale_colour_grey(start = 1,end = 0),
          scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
          labs(title = paste0("Graph combining outputs ",Index1.lab,", ", Index2.lab," and ", Index3.lab," - ",Index.country), y = "log(Cases)", x="Days"),
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size=8),
                legend.key.height = unit(0, 'cm')),
          if (type=="l")  geom_path(aes_string(y="log(Cases)",colour="Index"), size=size.index)
        )
    }
    print(sp3)
    
  }
  output$boxGlobal1 <- renderPlot({
      par(mfrow=c(1,1))
      LL=ifelse(identical(which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[1]),integer(0)), yes = which(as.Date(EVI_Global$Days, origin="1970-01-01")==(as.Date(input$rdates_Global[1]+1))), no = which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[1])) 
      UL=ifelse(identical(which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[2]),integer(0)), yes = which(as.Date(EVI_Global$Days, origin="1970-01-01")==(as.Date(input$rdates_Global[2]-1))), no = which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[2])) 
      evirlap(Index1 = EVI_Global[LL:UL,], Index2 = cEVI_Global[LL:UL,],
              Index1.lab = "EVI", Index2.lab = "cEVI", Index3.lab = "EVI-", ln = ifelse(test=input$rlog, T , F), Index.country = "HPAI", type = ifelse(test = input$rlines,"l","p"))
      
  })
  
#  output$boxGlobal2 <- renderPlot({
#    LL=ifelse(identical(which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[1]),integer(0)), yes = which(as.Date(EVI_Global$Days, origin="1970-01-01")==(as.Date(input$rdates_Global[1]+1))), no = which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[1])) 
#    UL=ifelse(identical(which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[1]),integer(0)), yes = which(as.Date(EVI_Global$Days, origin="1970-01-01")==(as.Date(input$rdates_Global[2]-1))), no = which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[2])) 
#    evirlap(Index1 = EVI_Global, Index2 = cEVI_Global,
#            Index1.lab = "EVI", Index2.lab = "cEVI", Index3.lab = "EVI-", ln = T, Index.country = "HPAI", type = ifelse(test = input$rlines,"l","p"))
#  })
  

  
  # as.Date(EVI_Global$Days, origin="2024-12-31")
  
}

shinyApp(ui=ui, server = server)

