#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinythemes)
library(gridExtra)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(readxl)

#--------------------------------------------------------
wasaData18 <- read_excel("wasaData.xlsx", sheet = "2018")
wasaData17 <- read_excel("wasaData.xlsx", sheet = "2017")
wasaData16 <- read_excel("wasaData.xlsx", sheet = "2016")
wasaData15 <- read_excel("wasaData.xlsx", sheet = "2015")

wasaData18 <- rename(wasaData18, Division ="20")
cleandata <- function(x,y) {
  x %>%
    rename(category ="Division",age_group = "Place/Division",genderP ="Place/Gender")%>%
    separate(age_group,into = c("AgePosition","ToalAge"), sep = "/")%>%
    separate(genderP,into = c("GenderPosition","ToaGender"), sep = "/")%>%
    separate(Place,into = c("Position","TotalP"), sep = "/")%>%
    fill(Swim, T1,Bike,T2,Run)%>%
    select(-c("Event","Race #","ToalAge","ToaGender","TotalP"))%>%
    mutate_at(c(5:6,1),as.numeric)%>%
    filter(str_detect(category,"(^F|^M)(?=\\d)"))%>%
    mutate(category=as.factor(category))%>%
    mutate(gender = ifelse(str_detect(category,"F"),"F","M"))%>%
    mutate(year = as.factor(y))%>%
    mutate(category = str_replace(category,"[A-Z]",""))%>%
    filter(!is.na(Time))
}

wasaData18c <- cleandata(wasaData18,2018)
wasaData17c <- cleandata(wasaData17,2017)
wasaData16c <- cleandata(wasaData16,2016)
wasaData15c <- cleandata(wasaData15,2015)

wasaData <- bind_rows(wasaData18c,wasaData17c,wasaData16c,wasaData15c)
wasaData$year <- as.factor(wasaData$year) 

yLabels <- function(x)
{
  x <- seconds_to_period(x)
  paste(hour(x),minute(x),second(x), sep = ':')
}
#------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- navbarPage( #theme = shinytheme("superhero"),
   
   # Application title
   title = "Wasa 2018 Triathlon EDA",
   
   # Sidebar with a slider input for number of bins 
      tabPanel( "Home",
               h1("2018 Olimpic Wasa Triathlon"),
              hr(),
              p( "In its promotional campaign, the race is described as: “Gerick Sports Wasa Lake Triathlon features elite prize money and attracts some of the fastest triathletes in the West. Age groupers and elites compete together on a scenic and fast course.” Wasa Lake is located in the beautiful province of British Columbia just west from the Rocky Mountains and 4 hours away from Calgary.
                 In this exploratory data analysis project, we will explore the 2018 results for the Olympic race distance (1.5 km swim, 40 km bike ,10 km run). I retrieved the data from the official timing company’s websiteand a copy of the file is available in the GitHub folderof the project."
              ),
              p("To check the reulst you have tow option: Overall results and by Categories. Select the Tab you prfer and have Fun!!")
             
          ),
      tabPanel("Overall",
         h1("Overall Results",align="center"),
         flowLayout(selectInput( inputId = "GenderMain",
                      label = "Select Gender:",
                      choices = c("All" ="all", "Female" = "F", "Male" = "M"),
                      selected = "all"
         ),
         checkboxGroupInput(inputId = "selected_year",
                            label = "Select year:",
                            choices = c("2018","2017","2016","2015"),
                            selected = "2018"
         
      )),
         div(plotOutput(outputId = "mainPlot",width = "100%"),align="center"),
         hr(),
         plotOutput("histogram")
         
      ),
      tabPanel (title ="Categories",
                flowLayout(selectInput( inputId = "Gender",
                             label = "Select Gender:",
                             choices = c("All" = "all", "Female" = "F", "Male" = "M"),
                             selected = "M"
                ),
                selectInput( inputId = "AgeGroup",
                             label = "Select Age Group:",
                             choices = c("18-24" = "1824", "25-29" = "2529", "30-34" = "3034",
                                         "35-39" = "3539", "40-44" = "4044", "45-49" = "4549",
                                         "50-54" = "5054", "55-59" = "5559","60-64" = "6064", "All" = "all" 
                             ),
                             selected = "3034"
                ),
                checkboxGroupInput(inputId = "selected_year2",
                                   label = "Select year:",
                                   choices = c("2018","2017","2016","2015"),
                                   selected = "2018"
                                   
                )),
             plotOutput("timePlot2"),
             hr(),
             plotOutput("histogram2")
    )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
          
          wasaDataF <-reactive({
            req(input$selected_year)
            if(input$Gender != "all"){
              if(input$AgeGroup !="all"){
                wasaData %>%
                filter(year == input$selected_year2)%>%
                filter((category==input$AgeGroup & gender==input$Gender))
              }
              else {
                wasaData %>% filter(year == input$selected_year2)%>%
                filter(gender==input$Gender)
              }
            }else if(input$AgeGroup != "all"){
              wasaData %>% filter(year == input$selected_year2)%>%
              filter(category==input$AgeGroup)
              } 
             else{
               wasaData %>% filter(year == input$selected_year2)
            }
              })
          
          wasafilter <- reactive({
            req(input$selected_year)
            if(input$GenderMain !="all"){
              wasafilter1 <- wasaData %>%
                  filter(year == input$selected_year)%>%
                  filter(gender == input$GenderMain)%>%
                  group_by(category)%>%
                  summarise(avg_swim = mean(Swim),avg_bike = mean(Bike),avg_run = mean(Run))%>%
                  gather(key="sport",value = "averageTime", 2:4)
                 wasafilter1%>%
                  mutate(averageTime = duration(hour = hour(averageTime), minute = minute(averageTime), 
                                                second = second(averageTime)))%>%
                  mutate(averageTime = as.numeric(averageTime))
            
            } else 
              wasafilter1 <- wasaData %>%
                  filter(year == input$selected_year)%>%
                  group_by(category)%>%
                  summarise(avg_swim = mean(Swim),avg_bike = mean(Bike),avg_run = mean(Run))%>%
                  gather(key="sport",value = "averageTime", 2:4)
                wasafilter1%>% 
                mutate(averageTime = duration(hour = hour(averageTime), minute = minute(averageTime), 
                                              second = second(averageTime)))%>%
                mutate(averageTime = as.numeric(averageTime))
          })
          
          wasafilterGender <-reactive({
            if(input$GenderMain != "all"){
                filter(wasaData,(gender==input$GenderMain))
            }
              else {
                 wasaData
            }
          })
         wasafilterAll <- reactive ({
           req(input$selected_year)
           if(input$selected_year != 'all'){
             filter(wasafilterGender(),(year == input$selected_year))
           }else {
             wasafilterGender()
           }
         })
          
          m1 <- reactive({ggplot(data = wasafilterAll(), aes(x= Position, y = Time, color=year))+
            geom_point()+
            theme_light()+
            theme(text = element_text(size=15))
          })
        
          
          m2 <- reactive({
          
            ggplot(wasafilter(), aes(x=category, y=averageTime,fill=sport))+
              geom_bar(stat="identity")+
              scale_y_continuous(labels = yLabels)+
              coord_flip()
          })
          
          h1 <- reactive({
            ggplot(data = wasafilterAll(), aes(x = Swim,fill= gender))+
            geom_histogram(alpha=0.5)+
              theme(text = element_text(size=15))
          })
          h2 <- reactive({
            ggplot(data = wasafilterAll(), aes( x = Bike,fill=gender))+
              geom_histogram(alpha=0.5)+
              theme(text = element_text(size=15))
          })
          h3 <- reactive({
            ggplot(data = wasafilterAll(), aes( x = Run,fill= gender))+
              geom_histogram(alpha=0.5)+
              theme(text = element_text(size=15))
          })
          l1 <- reactive({
            ggplot(data = wasaDataF(), aes( y = Swim, x =AgePosition,color=year))+
              geom_point()+
              geom_line()
          })
          l2 <- reactive({
            ggplot(data = wasaDataF(), aes( y = Bike, x= AgePosition,color=year))+
              geom_line()+
              geom_point()
          })
          l3 <- reactive({
            ggplot(data = wasaDataF(), aes( y = Run, x=AgePosition,color =year))+
              geom_line()+
              geom_point()
          })
          
          
      output$mainPlot <- renderPlot({
      #plot(data_dateRange$AccidentYear,data_dateRange[,test2])
      grid.arrange(m1(),m2(),ncol =2)
   })
      output$histogram <- renderPlot({
        #plot(data_dateRange$AccidentYear,data_dateRange[,test2])
      grid.arrange(h1(),h2(),h3(), ncol = 3)
      })
      output$timePlot2 <- renderPlot({
        #plot(data_dateRange$AccidentYear,data_dateRange[,test2])
        ggplot(data = wasaDataF(), aes(x= AgePosition, y = Time, color=year))+
          geom_point()+
          geom_line()
      })
      output$histogram2 <- renderPlot({
        #plot(data_dateRange$AccidentYear,data_dateRange[,test2])
        grid.arrange(l1(),l2(),l3(), ncol = 3)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)


