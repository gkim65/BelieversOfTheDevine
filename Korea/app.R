# Made my shiny app! Has all the various graphs for my project; will try to add
# my maps later!

# Need our shiny library!

library(shiny)
library(readr)
library(nnet)
library(rgdal)
library(tidyverse)
library(leaflet)

# Need to download the various data files for the various graphs 
# have the world data rds file, the filtered out gender data rds file, as well as
# the age filtered out data file

vertical_world =  read_rds("world.rds")
gender_data_vertical =  read_rds("gender.rds")
age_data =  read_rds("age.rds")
world = read_rds("worldMap.rds")

# change column names to join with other data set later
colnames(world)[1]<-"NAME"

# Read this shape file with the rgdal library. 
world_spdf <- readOGR("raw-data/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp", stringsAsFactors = F)
world_spdf@data[world_spdf@data$ISO2 == "AX","NAME"] <- "Aland Islands"

world_spdf@data <- inner_join(world_spdf@data,world,by = "NAME")

# Define UI for application that contains the plots and charts of our various
# graphs based upon the world and gender

ui <- fluidPage(
    
    # Need to provide the main title within our shiny app
    
    navbarPage("Believers in the Divine: The Religions of South Korea",
    
    # Several main tabs made, first one is the world to compare the statistics of 
    # south korea to the entire world
    
    tabPanel("The World",
             
        # Have a mini panel set within the main world tab to have both mapping
        # and graphs be shown
        
        tabsetPanel(
            
            # Religion mapping tab for the world page
            
            tabPanel("Interactive Religion Mapping",
                     
                     
                     # Have my little title here:
                     
                     h3("Distributions of Religions by Country"),
                     br(),
                     
                     
                     leafletOutput("worldMapPlot", width = "auto", height = "550"),
                     # Has a select input function to choose which religion to try
                     # mapping
                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                   draggable = TRUE, top = 250, left = "auto", right = 20,width = 330,
                         selectInput("region",
                                     "Religion:", 
                                     
                                     # Needed to specify the various religions you can
                                     # choose, had them equal the various variables for
                                     # in age data
                                     
                                     (c("Christian" = "christian",
                                        "Muslim" = "muslim",
                                        "Unaffiliated" = "unaffiliated",
                                        "Hindu" = "hindu",
                                        "Buddhist" = "buddhist",
                                        "Folk" = "folk",
                                        "Other" = "other",
                                        "Jewish" = "jewish"))
                         )))
                    ,
                        
            
             tabPanel("The World V.S. South Korea",
                      
                      # Only one graph within this panel
                      
                      mainPanel(
                          plotOutput(outputId = "worldPlot")
                      ))
             )),
    
    # Second main tab for Korean Demographics data compared with religion
    
    tabPanel("Demographics and Religion",
        
        # Made a inner panel for the 2nd main demographics panel, with gender and age
        
        tabsetPanel(
            
             tabPanel("Gender",
                      
                      # Made a gender circular bar graph plot 
                      
                      mainPanel(
                        plotOutput(outputId = "genderPlot")
                      )),
             
            tabPanel("Age",
                     
                     # Age tab for the demographics page
                     
                     h3("Religion distribution based on age"),
                     br(),
                     
                     # Has a select input function to choose which religion to try
                     # graphing a bar graph with a quadratic model fit onto it
                     
                     sidebarPanel(
                         selectInput("variable",
                                     "Religion:", 
                                     
                                     # Needed to specify the various religions you can
                                     # choose, had them equal the various variables for
                                     # in age data
                                     
                                     (c("All Religions" = "religious_total",
                                        "Buddhism" = "buddhism",
                                        "Christianity Protestant" = "christianity_protestant",
                                        "Christianity Catholic" = "christianity_catholic",
                                        "Won Buddhism" = "won_buddhism",
                                        "Confucianism" = "confucianism",
                                        "Cheondoism" = "cheondoism",
                                        "No Religion" = "no_religion"))
                     )),
                     
                     # Plot that comes out of it is based on the variable that the
                     # user chooses from the select tool
                     
                     mainPanel(
                         plotOutput(outputId = "agePlot")
                     ),
                     )
        )),
    
    # Included an About” tab with name, contact information, GitHub repo and data
    # source information.
    
        tabPanel("About",
                 mainPanel(
                     
                     # Provided information about the project itself and data sources
                     
                     h3("The Data"),
                     h5("These graphics were created based on data from both the Pew Research Center's", a("The Global Religious Landscape", href="https://www.pewforum.org/2012/12/18/global-religious-landscape-exec/"), " , as well as the KOSIS KOrean Statistical Information Service's ", a("2015 Gender/Age/Religion Survey", href="http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1PM1502&conn_path=I2")),
                     
                     # Ensure that the minimum relevant background is provided to
                     # understand what is being presented
                     
                     h5("As a first generation immigrant from South Korea, I wanted to explore more of the history and culture behind the traditions of the South Korean people. Learning about the religions of my home country made me realize many different influences from the west and the surviving cultural traditions of the Korean people."),
                     h5("Due to the scope and timeframe of this project, it was not possible to explore all the various factors that could affect religion, but I was able to see the affects age, gender, and region could have on the various religions that individuals proacticed."),
                     
                     # Include information about me so they can know who wrote the project
                     
                     h3("About Me: Grace Kim"),
                     h5("I am a Harvard undergraduate studying mechanical engineering and passionate about data and computer science."),
                     h5("Contact me at gracekim@college.harvard.edu or connect with me on LinkedIn", a("HERE", href="https://www.linkedin.com/in/grace-k-767483132")),
                     
                     # Use the citation for the data provided by the authors to provide
                     # proper credit
                     
                     h3("Citations"),
                     h5("Table: Religious Composition by Country, in Percentages. (2017, November 16). Retrieved from https://www.pewforum.org/2012/12/18/table-religious-composition-by-country-in-percentages/."),
                     h5("성별/연령별/종교별 인구-시군구. (2015, January 5). Retrieved from http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1PM1502&conn_path=I2."),
                     
                     # Include a link to the Source Code for reproducibility and credibility
                     
                     h3("Source Code"),
                     h5("The source code for this Shiny App can be found at my GitHub", a("HERE", href="https://github.com/gkim65/milestone_8"))
                 ))
    ))

# Define server logic required to draw out all of our graphs from the rds data

server <- function(input, output, session) {

    # GRAPH FOR THE RELIGION MAPPING FUNCTION DATA
    
    # Needed a leaflet to make the world map highlighting the various countries
    # Got the input variable from the select tool from the app
    # needed to use aes_string since the input only gives out a string
    
    map = leaflet(world_spdf)%>% 
        addTiles()

    
    observe({
        colorReligion <- world_spdf[[input$region]]
        
            pal <- colorNumeric("YlGnBu", colorReligion)
            
            mytext <- paste(
                "Country: ", world_spdf@data$NAME,"<br/>", 
                "Religion: ", colorReligion, "<br/>", 
                "Population: ", round(as.numeric(as.character(world_spdf@data$POP2005)), 2), 
                sep="") %>%
                lapply(htmltools::HTML)
            
            output$worldMapPlot <- renderLeaflet({map %>%
                addTiles()%>%
                setView(lat=10, lng=0 , zoom=2) %>%
                addPolygons(stroke = FALSE, 
                            fillOpacity = 0.9, 
                            smoothFactor = 0.5, 
                            color = ~pal(colorReligion),
                            label = mytext,
                            labelOptions = labelOptions( 
                                style = list("font-weight" = "normal", padding = "3px 8px"), 
                                textsize = "13px", 
                                direction = "auto"
                            )) %>% 
                addLegend("bottomright", 
                          pal = pal, 
                          values = ~colorReligion,
                          title = "Religion",
                          opacity = 1)    
            
        
    })
    })
    

    # GRAPH FOR THE AGE SELECT FUNCTION DATA
    
    output$agePlot <- renderPlot({
        
        # Needed a ggplot to make the bar graph and the quadratic regression model
        # Got the input variable from the select tool from the app
        # needed to use aes_string since the input only gives out a string
        
            ggplot(age_data, aes(x = as.numeric(age_number), fill = age_number))+
                aes_string(y = input$variable)+
                geom_bar(stat = "identity")+
                stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)
    })
    
    
    
    # GRAPH OF GENDER PLOT DATA
    
    output$genderPlot <- renderPlot({
        
        # ggplot of the gender graph, a circular bar graph for bot hmale and female genders
        
        ggplot(gender_data_vertical, aes(x = reorder(as.factor(religion),value), y = value, fill = gender)) +       
            
            # need to set stat as identity so we use the actual y values of the 
            # bar plot; scaled y by log in order to have all of the various values be 
            # represented on the graph
            
            geom_bar(stat="identity") +
            scale_y_log10()+
            theme_minimal()+
            theme(
                axis.title = element_blank(),
                panel.grid = element_blank(),
                plot.margin = unit(rep(-1,4), "cm") 
            ) +
            
            # This makes the coordinate polar instead of cartesian.
            
            coord_polar(start = 0)
    })
    
    # GRAPH OF THE WORLD VS KOREA PLOT
    
    output$worldPlot <- renderPlot({
        
        # ggplot of the world and korea grouped barplot
        
        ggplot(vertical_world, aes(x = key, y=value, fill = country))+ 
            geom_bar(position="dodge", stat="identity")+
            labs(x = "Religion",
                 y = "Percentages of Religious Followers",
                 fill = "",
                 title = "The Religions of the World vs. South Korea")  
    })    
    
    
}

# Run the application 

shinyApp(ui = ui, server = server)
