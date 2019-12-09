# Made my shiny app! Has all the various graphs for my project; will try to add
# my maps later!

##### Need our libraries! #####

library(shiny)
library(readr)
library(nnet)
library(dplyr)
library(rgdal)
library(tidyverse)
library(leaflet)
library(scales)
library(shinythemes)
library(plotly)


#### DATA DOWNLOAD ####

# Need to download the various data files for the various graphs 
# have the world data rds file, the filtered out gender data rds file, as well as
# the age filtered out data file

vertical_world =  read_rds("world.rds")
gender_data_vertical =  read_rds("gender.rds")
age_data =  read_rds("age.rds")
world = read_rds("worldMap.rds")
joined = read_rds("koreaMap.rds")
combined = read_rds("lineplot.rds")

    
# change column names to join with other data set later
colnames(world)[1]<-"NAME"

# Read this shape file with the rgdal library. 
world_spdf <- readOGR("raw-data/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp", stringsAsFactors = F)
world_spdf@data[world_spdf@data$ISO2 == "AX","NAME"] <- "Aland Islands"

world_spdf@data <- inner_join(world_spdf@data,world,by = "NAME")

#### Start of UI Code ####

# Define UI for application that contains the plots and charts of our various
# graphs based upon the world and gender

ui <- fluidPage(theme = shinytheme("flatly"),
    
    # Need to provide the main title within our shiny app
    
    navbarPage("Believers in the Divine: The Religions of South Korea",
    
    #### World Tab Code ####
               
               
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
                     
                     # The leaflet map that spans across the whole panel
                     
                     leafletOutput("worldMapPlot", width = "auto", height = "550"),
                     
                     # Has a select input function to choose which religion to try
                     # mapping
                     # inside an absolute panel so the user can drag it to wherever
                     # they would like to see it, located at bottom left for now
                     absolutePanel(bottom = 20, left = 40, width = 300,
                                   draggable = TRUE,
                                   wellPanel(
                                   h3("Religion makeup based on country"),
                                   
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
                                        "Jewish" = "jewish"))),
                                   
                                   h5("The decimal values of the religion for each country represent 
                                      the percentage of how many people in that nation practice that 
                                      specific religion."),
                                   h5("You can zoom in, zoom out, and move the world map with your cursor;
                                      in addition, you can choose which religion you want mapped on the world 
                                      map by choosing one of the religions on the dropdown selector. Individual
                                      statistics of each country can be seen by hovering on a specific nation.")
                         ),
                         style = "opacity: 0.92"
                         ))
                    ,
                        
            
             tabPanel("The World V.S. South Korea",
                      
                      # Only one graph within this panel
                          h2("The Religions of the World vs. South Korea"),
                          plotOutput(outputId = "worldPlot"),
                      
                      # Need to give a slight explanation
                      
                      h5("Each of the major world religions and the prevalance of each in the world and
                         South Korea are listed above in the graphic. The world percentages were 
                         calculated based on the percentages makeup of each nation. Thus, each nation 
                         was treated at an equal level, which is not representative of the number of 
                         religious followers by population."),
                      h5("However, this percentage representation lets us compare South Korea to the 
                         other religous makeups for each nation. Interesting observations in the data 
                         are the large numbers of unaffiliated individuals with religion in South Korea
                         in contrast to the rest of the nations in the world. There is also a large 
                         Buddhist population, which is understandable seeing as South Korea is in the
                         region of East Asia; there is also a large population of Christian individuals,
                         but not to the similar extent of the Christian populations in the rest of the 
                         world.")
                      ),
            
            tabPanel("South Korea Trends Over Time",
                     
                     # Only one graph within this panel
                     h2("Religions in South Korea Over Time"),
                     plotlyOutput(outputId = "linePlot"),
                     
                     # Need to give a slight explanation
                     
                     h5("I wanted to explore the various changes in the various religious populations
                        of two specific religions: Christianity and Buddhism. I understood that the 
                        Christian population had increased with the increasing influence and power
                        that western nations in South Korea, but did not know how this affected Buddhism:
                        one of the largest religious populations in South Korea. "),
                     h5("While Protestant Christianity and no religious populations seem to be on the rise,
                        in the post millenial time period, the levels of Buddhism and Catholic Christianity 
                        are seen to decrease during this time period. These sudden changes are mainly seen in
                        the beginning of the 2000s, moving into the 2015s breaking the steady trends starting from
                        1985 to 2000s.")
            )
             )),
    
    #### Second main tab for Korean Demographics data compared with religion ####
    
    tabPanel("Demographics and Religion",
        
        # Made a inner panel for the 2nd main demographics panel, with gender and age
        
        tabsetPanel(
            
             tabPanel("Gender",
                      fluidRow(
                          style = "max-height: 80vh; overflow-y: auto;", 
                      # Made a gender proportional bar graph plot 
                      h3("Male and Female Religous Data"),
                      plotOutput(outputId = "genderPlot"),
                      
                      h5("In this grouped percentage based gender religion plot, many more South 
                         Korean females are found to practice religion than their male counterparts. 
                         The only sections that South Korean males pass the 50% mark against females
                         "),
                      # And a number representative bar graph plot
                      
                      plotOutput(outputId = "gender2Plot")
                      
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
                                        "No Religion" = "no_religion"))
                     )),
                     
                     # Plot that comes out of it is based on the variable that the
                     # user chooses from the select tool
                     
                     mainPanel(
                         plotOutput(outputId = "agePlot")
                     )
                     ),
                tabPanel("Geographic Regions of South Korea",

                               selectInput("imageMap",
                                           "Religion:", 
                                           
                                           # Needed to specify the various religions you can
                                           # choose, had them equal the various variables for
                                           # the names of the images of each map
                                           
                                           (c("All Religions" = "graphics/allReligionsKorea.png",
                                              "Buddhism" = "graphics/buddhismKorea.png",
                                              "Christianity Protestant" = "graphics/christProtesKorea.png",
                                              "Christianity Catholic" = "graphics/christCathKorea.png",
                                              "Won Buddhism" = "graphics/wonBuddhismKorea.png",
                                              "Confucianism" = "graphics/confucianismKorea.png",
                                              "Cheondoism" = "graphics/cheondoismKorea.png",
                                              "No Religion" = "graphics/noReligionKorea.png"))
                               ),
                               
                               h5("Choose from this dropdown menu to explore the religious makeup of the 
                                  various administrative regions in South Korea."),
                         
                         fillPage(fillRow(imageOutput("imagePop"),
                                          imageOutput("imageReligions")
                         ))
                           
                         
                )
        )),
    
    #### About tab page ####
    
    # Included an About” tab with name, contact information, GitHub repo and data
    # source information.
    
        tabPanel("About",

                     # Provided information about the project itself and data sources
                     
                     h3("The Data"),
                     h5("These graphics were created based on data from both the Pew Research Center's",
                        a("The Global Religious Landscape", href="https://www.pewforum.org/2012/12/18/global-religious-landscape-exec/"), 
                        " , as well as the KOSIS KOrean Statistical Information Service's ", a("2015 Gender/Age/Religion Survey", 
                        href="http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1PM1502&conn_path=I2")),
                     
                     # Ensure that the minimum relevant background is provided to
                     # understand what is being presented
                     
                     h5("As a first generation immigrant from South Korea, I wanted to explore more of the
                        history and culture behind the traditions of the South Korean people. Learning about
                        the religions of my home country made me realize many different influences from the 
                        west and the surviving cultural traditions of the Korean people."),
                     h5("Due to the scope and timeframe of this project, it was not possible to explore 
                        all the various factors that could affect religion, but I was able to see the 
                        affects age, gender, and region could have on the various religions that individuals
                        proacticed."),
                     
                     # Include information about me so they can know who wrote the project
                     
                     h3("About Me: Grace Kim"),
                     h5("I am a Harvard undergraduate studying mechanical engineering and passionate about 
                        data and computer science."),
                     h5("Contact me at gracekim@college.harvard.edu or connect with me on LinkedIn", 
                        a("HERE", href="https://www.linkedin.com/in/grace-k-767483132")),
                     
                     # Use the citation for the data provided by the authors to provide
                     # proper credit
                     
                     h3("Citations"),
                     h5("Table: Religious Composition by Country, in Percentages. (2017, November 16). Retrieved from https://www.pewforum.org/2012/12/18/table-religious-composition-by-country-in-percentages/."),
                     h5("성별/연령별/종교별 인구-시군구. (2015, January 5). Retrieved from http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1PM1502&conn_path=I2."),
                     
                     # Include a link to the Source Code for reproducibility and credibility
                     
                     h3("Source Code"),
                     h5("The source code for this Shiny App can be found at my GitHub", 
                        a("HERE", href="https://github.com/gkim65/milestone_8"))
                 )
    ))

# Define server logic required to draw out all of our graphs from the rds data

##### Start of Server Code #####

server <- function(input, output, session) {

    #### GRAPH FOR THE RELIGION MAPPING FUNCTION DATA ####
    
    # Needed a leaflet to make the world map highlighting the various countries
    
    map = leaflet(world_spdf)%>% 
        addTiles()

    # Need an observe function to ensure that the graph changes according to the different
    # religions that people choose in the tabs
    
    observe({
        
        # Got the input variable from the select tool from the app
        
        colorReligion <- world_spdf[[input$region]]
        
        # Set the color palette to Yellow Green Blue according to scales of the colorReligion
        
            pal <- colorNumeric("YlGnBu", colorReligion)
            
            # Hovering text for the leaflet data, every country will have a specific textbox
            # that has all the data numbers for country name, population, and religious data
            
            mytext <- paste(
                "Country: ", world_spdf@data$NAME,"<br/>", 
                "Religion: ", (colorReligion*100), "% <br/>", 
                "Population: ", round(as.numeric(as.character(world_spdf@data$POP2005)), 2), 
                sep="") %>%
                lapply(htmltools::HTML)
            
            # Change the actual plot physically with every change in the chosen labels
            # need to do directly through the output function of the worldMapPlot
            
            output$worldMapPlot <- renderLeaflet({map %>%
                    
                # Add the tiles for the various country sections
                
                addTiles()%>%
                    
                # Set view to the entire world map 
                    
                setView(lat=10, lng=0 , zoom=2) %>%
                    
                # Polygon colors set based on numbers of the various religion percentages
                    
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
                    
                # Legend for the percentages on the bottom 
                    
                addLegend("bottomright", 
                          pal = pal, 
                          values = ~(colorReligion),
                          title = "Religion",
                          opacity = 1)    
            
        
    })
    })
    

    ##### GRAPH FOR THE AGE SELECT FUNCTION DATA #####
    
    output$agePlot <- renderPlot({
        
        # Needed a ggplot to make the bar graph and the quadratic regression model
        # Got the input variable from the select tool from the app
        # needed to use aes_string since the input only gives out a string
        
            ggplot(age_data, aes(x = as.numeric(age_number), fill = age_number))+
                aes_string(y = input$variable)+
                geom_bar(stat = "identity")+
            labs(x = "Age", y = "Population Per Religion")+
            stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
            scale_y_continuous(limits = c(0,2500000),labels = comma)+
            theme(legend.position = "none") 
    })
    
    #### GRAPH FOR THE LINEPLOT TRENDS OVER YEARS DATA #####
    
    
    # line plot for the year by data change of the various line plots
    
    output$linePlot <- renderPlotly({
        
        # needed to group by the various religions which are all in one identifier column
        # use year as my x and population as my y
        
        ggplot(combined, aes(x=year, 
                   y=as.numeric(population), 
                   group=religion, 
                   color=religion)) +
        
        # increase line thickness
            
        geom_line(size = 1.5) + 
        geom_point()+
        
        # get the y scale to have labels with commas
            
        scale_y_continuous(limits = c(0,50000000), labels = comma)+
        labs(x = "Year", y = "Population")
    })
    
    ##### GRAPH OF GENDER PLOT DATA #####
    
    output$genderPlot <- renderPlot({
        
        # Percentage wise values to compare between males and females who practice more religion
        
        ggplot(gender_data_vertical,aes(x =key, y = value, fill = gender)) +
            
            # fill bar graph that allows me to compare based on proportional data
            
            geom_bar(stat = "identity",position="fill") +
            
            # flip coordinates for the bars to go horizontally
            
            coord_flip() +
            
            # y intercept to be 0.5 
            
            geom_hline(yintercept = 0.5) +
            
            # colors of salmon and cyan ;D
            
            scale_fill_manual(values = c("#FF5733","#15C9C1")) +
            
            # Need to set neat, correct religion labels for the data
            
            scale_x_discrete(name="Religion", labels = c("Buddhism", 
                                                         "Cheondoism", 
                                                         "Christianity Catholic", 
                                                         "Christianity Protestant", 
                                                         "Confucianism",
                                                         "Daesun Truth Association", 
                                                         "Extra", 
                                                         "No Religion", 
                                                         "Unknown", 
                                                         "Won Buddhism"))+
            labs(title = "Percentage Based Representation", x = "Religions", y = "Percentage Practiced by Population")
    })
    
    ##### MORE GRAPHS FOR GENDER PLOTTING DATA #####
    
    output$gender2Plot <- renderPlot({
        
        # this graph is for the actual population number data instead of percentages
        # uses only the geom_bar identity function but still flips the coordinates so
        # the names of the bars can be easily read
        # same colors as previous bar graph for easy reference
        
    ggplot(gender_data_vertical,aes(x = reorder(as.factor(religion),value), y = value, fill = gender)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = c("#FF5733","#15C9C1")) +
        scale_x_discrete(name="Religion", labels = c("Unknown", 
                                                     "Unknown", 
                                                     "Daesun Truth", 
                                                     "Daesun Truth", 
                                                     "Cheondoism", 
                                                     "Confucianism", 
                                                     "Won Buddhism", 
                                                     "Cheondoism", 
                                                     "Extra",
                                                     "Confucianism",
                                                     "Won Buddhism",
                                                     "Extra",
                                                     "Christianity Catholic",
                                                     "Christianity Catholic",
                                                     "Buddhism",
                                                     "Buddhism",
                                                     "Christianity Protestant",
                                                     "Christianity Protestant",
                                                     "No Religion", 
                                                     "No Religion"
                                                     ))+
            scale_y_continuous(labels = comma)+
        labs(title = "Population Based Representation", x = "Religions", y = "Population that practices Religion")
    })
    
    ##### GRAPH OF THE WORLD VS KOREA PLOT #####
    
    output$worldPlot <- renderPlot({
        
        # ggplot of the world and korea grouped barplot
        # compare the percentages of the religons of the world with those of South Korea
        
        ggplot(vertical_world, aes(x = key, y=value, fill = country))+ 
            geom_bar(position="dodge", stat="identity")+
            labs(fill = "",
                 title = "Based on percentage makeup of religious following in each nation")+
            scale_x_discrete(name="Religion", labels = c("Buddhist", "Christian", "Folk", "Hindu", "Jewish", "Muslim", "Other", "Unaffiliated"))+
            scale_y_continuous(name = "Percentages of Religious Followers", breaks = c(0.2,0.4,0.6), labels = c("20%", "40%", "60%"))+
            scale_fill_brewer(palette="Paired")+
            theme_minimal()+
            geom_text(aes(label=percent(round(value, digits = 3))), vjust=-0.2, color="Black",
                      position = position_dodge(0.9), size=3.5)
        
            
    })    
    
    #### Korea Map Images ####
    
    output$imagePop <- renderImage(
        
        # image of the ggplot map for the population density in South Korea
        
        list(src = "graphics/populationKorea.png",width="100%"),
        deleteFile = FALSE)
    
    #### Korea Religion Map Images ####
    
    output$imageReligions <- renderImage(
        
        # image of the ggplot map of the various religion population densities of the
        # different South Korean administrative areas
        
        list(src = as.character(input$imageMap),width="100%"),
        deleteFile = FALSE)

}

#### Run the application ####

shinyApp(ui = ui, server = server)
