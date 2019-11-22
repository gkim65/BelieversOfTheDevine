#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

vertical_world =  read_rds("world.rds")
gender_data_vertical =  read_rds("gender.rds")
age_data =  read_rds("age.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("Believers in the Divine: The Religions of South Korea",
    tabPanel("The World",
        tabsetPanel(
             tabPanel("The World V.S. South Korea",
                      mainPanel(
                          plotOutput(outputId = "worldPlot")
                      )),
             tabPanel("Major World Religions",
                      mainPanel(
                      )))),
    tabPanel("Demographics and Religion",
        tabsetPanel(
             tabPanel("Gender",
                      mainPanel(
                          plotOutput(outputId = "genderPlot")
                      )),
            tabPanel("Age",
                     h3("Religion distribution based on age"),
                     br(),
                     sidebarPanel(
                         selectInput("variable",
                                     "Religion:", 
                                     (c("All Religions" = "religious_total",
                                        "Buddhism" = "buddhism",
                                        "Christianity Protestant" = "christianity_protestant",
                                        "Christianity Catholic" = "christianity_catholic",
                                        "Won Buddhism" = "won_buddhism",
                                        "Confucianism" = "confucianism",
                                        "Cheondoism" = "cheondoism",
                                        "No Religion" = "no_religion"))
                     )),
                     mainPanel(
                         plotOutput(outputId = "agePlot")
                     ),
                     )
        )),
    
    # Include an About” tab with name, contact information, GitHub repo and data
    # source information.
        tabPanel("About",
                 mainPanel(
                     
                     # Provide information about the project itself and data sources
                     
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

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$agePlot <- renderPlot({
            ggplot(age_data, aes(x = as.numeric(age_number), fill = age_number))+
                aes_string(y = input$variable)+
                geom_bar(stat = "identity")+
                stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)
    })
    
    output$genderPlot <- renderPlot({
        ggplot(gender_data_vertical, aes(x = reorder(as.factor(religion),value), y = value, fill = gender)) +       
            
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
    
    output$worldPlot <- renderPlot({
        
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
