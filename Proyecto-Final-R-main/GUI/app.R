

library(shiny)
library(dplyr)
library(class)
library(stringr)
library(shinydashboard)
library(ggplot2)
setwd("D:/Program Files/RStudio/TRY1/www")
pData<-read.csv("match.data.csv")
choiceV<- names(pData)

ui<- fluidPage(
    dashboardPage(
        dashboardHeader(title = "Postwork 8"),
        
        dashboardSidebar(
            
            sidebarMenu(
                
                menuItem("Data Table",tabName = "data_table", icon = icon("table")),
                menuItem("Postwork 3", tabName = "img",icon = icon("file-picture-o")),
                menuItem("Graficas de Barras", tabName = "Dashboard", icon = icon("dashboard")),
                menuItem("Frecuencias Relativas", tabName = "FR", icon = icon("file-picture-o"))
            )
        ),
        
        dashboardBody(
            tabItems(
                tabItem( tabName = "data_table",
                         fluidRow(
                             titlePanel(h5("Data Table")),
                             dataTableOutput("data_table")
                         )),
                tabItem(tabName = "img",
                        titlePanel(h5("Imagenes")),
                        img(src="PW31.png",
                                   height=400,
                                   width=400),
                        img(src="PW33.png",
                            height=400,
                            width=400)
                            ),
                tabItem(tabName = "Dashboard",
                        fluidRow(
                            titlePanel(h3("Gráficos de barras")),
                            selectInput("x","Seleccionar variable X",
                                        choices = c(choiceV[3],choiceV[5])),
                            plotOutput("plot1", height = 400, width = 700)
                        )),
                tabItem(tabName = "FR",
                        titlePanel(h4("Gráficos del código momios")),
                        img(src="MMS1.png", height=400, width=700),
                        img(src="MMS2.png", height=400, width=700))
                )
            )
        )
    )


server<- function(input, output){
    output$data_table<-renderDataTable({pData},
                                       options=list(aLengthMenu=c(20,50,80),
                                                    iDisplayLength=10)
                                       )
    output$plot1<-renderPlot({
        x<-pData[,input$x]
        pData%>% ggplot(aes(x))+
            geom_bar()+
            facet_wrap("pData$away.team")+
            labs(x=input$x, y="goles")+
            ylim(0,100)
    })
    
}
shinyApp(ui,server)