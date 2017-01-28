
library(plyr)
library(reshape2)
library(dplyr)
library(rmarkdown)
library(ggthemes)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(shiny)
library(ggmap)
library(zipcode)

lou<-ggmap(get_map(location = c(lon = -85.7, lat = 38.2), zoom = 11, color = 'bw'))

data("zipcode")

zipdf<-c(40023, 40047, 40059, 40109, 40118, 40177, 40202, 40203, 40204, 40205, 40206, 40207, 40208, 40209 ,40210, 40211, 40212, 40213, 
         40214, 40215, 40216, 40217, 40218, 40219, 40220, 40222, 40223, 40025, 40228, 40229, 40231, 40280, 40041, 40241, 40242, 
         40243, 40245, 40258, 40272, 40291, 40299)

zips<-zipcode[zipcode$zip %in% zipdf,]
colnames(zips)<-c("zip", "city", "state", "lat", "lon")
x<-get_googlemap(center = c(lon = -85.7, lat = 38.2), zoom = 10, color = 'bw',maptype=c("roadmap"))


load("./data/EdData.Rdata")
load("./data/EdxData.Rdata")
load("./data/EdyData.Rdata")
dfy1$Year<-paste("'",dfy1$Year, sep = "")

#Shiny
server <- function(input, output){
    output$plot<-renderPlotly({
      
      
    df1<-df%>%
               filter(`Zip Code` %in% input$Zipcode) %>%
                filter(Classification %in% input$`Data Selection`)
    
    ggplotly(ggplot(df1, aes(x=Year, y=`Value`, color = `Demographic`,group=`Demographic`))+ 
               geom_line(size=1) + geom_point(size=2) + scale_colour_brewer(palette = "Set1")
               + ggtitle(paste(input$`Data Selection`, input$Zipcode, sep = " "))
             +theme_fivethirtyeight(base_size = 10))
      
    })
    
    output$plot1<-renderPlotly({
      
      df2<-dfx%>%
        filter(Classification %in% input$`Data Selection`)
      
      ggplotly(ggplot(df2, aes(x=Year, y=`Value`, color = `Demographic`,group=`Demographic`))+ 
                 geom_line(size=1) + geom_point(size=2) + scale_colour_brewer(palette = "Set1")
               + ggtitle(paste(input$`Data Selection`))
               +theme_fivethirtyeight(base_size = 10))
      
      
    })
    
    output$plot2<-renderPlot({
      if(input$`Zipcode`!= "All"){
      where<-zips[zips$zip == input$Zipcode,]
      
    (lou
        +   geom_point(aes(x = lon, y = lat,  color = "red", size = 10), data = where)
      +theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = "none"))
      }
    })
    
    output$plot3<-renderPlotly({
      dfy<-(dfy1%>%
              filter(Variable == input$`Data Selection`)%>%
              filter(Year == input$Year)
      )
      p=ggmap(x)
      p <- ggplotly(p +geom_polygon(data=dfy,aes(x=long,y=lat,group=`Zip Code`, fill= Percentage))+
                      theme_fivethirtyeight(base_size = 10)+
                      theme(axis.title.x=element_blank(),
                            axis.text.x=element_blank(),
                            axis.ticks.x=element_blank(),
                            axis.title.y=element_blank(),
                            axis.text.y=element_blank(),
                            axis.ticks.y=element_blank()) +
                      ggtitle(paste("Percentage of ", input$`Data Selection`, " who are a Minority")))
                    
    })
    
    output$moreControls <- renderUI({
      tagList(
        sliderInput("n", "N", 1, 1000, 500),
        textInput("label", "Label")
      )
    })
    
    
    output$text1 <- renderText({ 
        
     # Descriptions
      if (input$`Data Selection`=="KHEAA Number Grads"){
      "The number of high school graduates has been on the rise over the past 11 years across all three demographics show growth."
        }else if(input$`Data Selection`=="Free or Reduced Price Lunch"){
      "The number of students eligible for free or reduced price lunch has risen every year, with the exception of 2012, for White and Black or African American students. Hispanic students have had more students on free or reduced price lunch every year."
        }else if(input$`Data Selection`=="Number KEES Earned >0"){
      "Over 11 years the number of White students receiving KEES has increased by 16%. The number of Black or African American students receiving KEES is up 59%. The number of Hispanic students receiving KEES is up 258%."
        }else if(input$`Data Selection`=="Mean KEES Earned"){
       "The average amount of KEES money awarded to students who earned it has increased over the time frame. Notably, students all earned less money in 2007 compared to 2006 and all demographics saw an increase in 2016. The Hispanic population is most volatile with large up and down swings, due to smaller population size."
        }else if(input$`Data Selection`=="Number ACT Exam Takers"){
       "The number of students who took the ACT swelled following The Great Recession as more students planned to go to college."
        }else if(input$`Data Selection`=="Mean ACT  equivalent "){
       "The average ACT score or equivalent in Jefferson County has remained stable over the time frame. The average score for White students, while the highest of any demographic, is still below 23 which is the national benchmark for college readiness."
        }else if(input$`Data Selection`=="Number AP Exam Takers"){
       "The number of white students taking AP Exams increased dramatically from 2010 to 2014. Hispanic and Black or African American students are also taking more AP Tests."
        }else if(input$`Data Selection`=="Number AP Exams Passed"){
       "The number of AP Exams that were passed correlates with the number of students taking tests."
        }else if(input$`Data Selection`=="Number FAFSA Filers"){
       "The number of FAFSA filers in Jefferson County increased steadily from 2006 to 2011 but has plateaued since then."
        }else if(input$`Data Selection`=="Number FAFSA Filers Pell eligible"){
       "The number of FAFSA filers who were Pell Eligible increased as the number of students filing the FAFSA increased."
        }else if(input$`Data Selection`=="Mean Pell for FT Attendance"){
       "The maximum amount of money available to individuals who were Pell eligible changed as a part of the American Recovery and Reinvestment Act of 2009."
        }else{
          ""
        }
})
}

ui <- shinyUI(
  fluidPage(
  titlePanel("Jefferson County Education Data 2006-2016"),
  
  sidebarLayout(
    sidebarPanel(
      #input fuctions
      selectInput(inputId = "Data Selection", label = "Choose what data to present", choices = c("KHEAA Number Grads","Free or Reduced Price Lunch", "Number KEES Earned >0","Mean KEES Earned"                                 
                                                                                                 , "Number ACT Exam Takers","Mean ACT  equivalent " , "Number AP Exam Takers","Number AP Exams Passed"                           
                                                                                                 , "Number FAFSA Filers","Number FAFSA Filers Pell eligible" , "Mean Pell for FT Attendance"
                                                                                                 ), selected = "KHEAA Number Grads"),
      textOutput(outputId = "text1"),
      p(""),
      
      selectInput(inputId = "Zipcode", label = "Choose a Zipcode", choices = c( 40023, 40047, 40059, 40109, 40118, 40177, 40202, 40203, 40204, 40205, 40206, 40207, 40208, 40209 ,40210, 
                                                                               40211, 40212, 40213, 40214, 40215, 40216, 40217, 40218, 40219, 40220, 40222, 40223, 40225, 40228, 40229, 
                                                                               40241, 40242, 40243, 40245, 40258, 40272,40291, 40299), selected = "40217"),
      p(strong("Location of Zip Code")),
      plotOutput(outputId = "plot2"),
      h3(paste("")),
      h3(paste("")),
      h3(paste("")),
      h3(paste("")),
      selectInput(inputId = "Year", label = "Choose a Year to display in the map", choices = c("'16","'15","'14","'13","'12","'11","'10","'09","'08","'07","'06")),
      h3(paste("")),
      h3(paste("")),
      h3(paste(""))
      
    ),
    
  mainPanel(
  #output fuctions
      
      plotlyOutput(outputId = "plot1"),
      h3(paste("")),
      plotlyOutput(outputId = "plot"),
      h3(paste("")),
      plotlyOutput((outputID = "plot3"), width = "600px", height = "600px"),
      h6(paste("*Data from KHEAA - Nov '16"))
)
)
))

shinyApp(server = server, ui = ui)

