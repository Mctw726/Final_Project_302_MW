library(shinydashboard)
library(shiny)
library(tidyverse)
library(jsonlite)
library(leaflet)
library(geojson)
library(leaflet.minicharts)
library(DT)
library(htmltools)
library(janitor)
library(sf)

#Load in Data  --------------------------------------------------------------------------------------
police_killings <- read_csv("data/processed/police_killings_clean.csv") %>% 
  janitor::clean_names()

#Load info for maps (I created in my R scripts)
load("data/wrangled/data_wrangling_new.rda")

#Load in race data for maps (I created in my R scripts)
load("data/wrangled/race_info_4_map.rda")
load("data/wrangled/state_info_pov.rda")

# Changes to state_2 (for maps)
#add in poverty
#add in poverty information
real_poverty <- data.frame(
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "DC", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  `poverty` = c("18.5%", "10.3%", "17.4%", "19.1%", "15.3%", "11.5%", "10.5%", "12.4%", "17.3%", "15.7%", "17.0%", "10.6%", "15.1%", "13.6%", "14.5%", "12.2%", "13.0%", "18.5%", "19.6%", "13.4%", "9.7%", "11.5%", "15.8%", "10.2%", "22.0%", "14.8%", "14.6%", "12.6%", "14.7%", "8.2%", "10.8%", "20.4%", "15.4%", "16.4%", "11.0%", "14.8%", "16.1%", "15.4%", "13.2%", "13.9%", "16.6%", "13.7%", "16.7%", "15.9%", "11.3%", "10.2%", "11.2%", "12.2%", "17.9%", "12.1%", "11.1%")
)

# Remove the % sign from the "Poverty rate, 2015" column
real_poverty$poverty <- gsub("%", " ", real_poverty$poverty)
indices_to_remove3 <- c(9)
real_poverty <- real_poverty[-indices_to_remove3, ]
state_2$pov_real <- real_poverty$poverty
state_2$pov_real <- as.numeric(state_2$pov_real)


##Are lower income neighbrohoods targetied?
# Create a new column "Difference" with the difference between two columns
state_2 <- st_as_sf(state_2, wkt = "geometry")
state_2$difference <- as.numeric(state_2$pov_real) - as.numeric(state_2$pov_avg)
state_2$perc_difference <- (as.numeric(state_2$pov_real) - as.numeric(state_2$pov_avg)) / as.numeric(state_2$pov_avg) * 100
state_2 <- full_join(state_2, state_list)

#last min change to a label
labels_state_pov <- sprintf(
  "In <strong>%s</strong><br/> the percentage differnence in the state's poverty to the county <br/> where the person was killed by the police is <strong>%g<strong>",
  # "In <strong>%s</strong><br/><strong>%g<strong> people were killed by the police<br/>from January to May 2015 ",
  state_2$state, state_2$perc_difference
) %>% lapply(htmltools::HTML) 



#  most_recurring_month <- police_killings %>%
police_killings <- police_killings %>%
  mutate(month = month.name[month])

#unarmed data
police_killings$unarmed <- 1

# Update the 'unarmed' column with 'Yes' where the 'armed' column has anything but 'No'
police_killings$unarmed[police_killings$armed != "No"] <- 0



most_recurring_month <- police_killings %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(1) %>%
  pull(month)

# Ui --------------------------------------------------------------------------------------
ui <- (
  dashboardPage(skin = "black",
                
                # Header --------------------------------------------------------------------------------------
                
                dashboardHeader(
                  title = "Police Killings from 01/01/2015 - 01/06/2015",
                  titleWidth = 600
                ),
                # End Header
                
                # Sidebar -------------------------------------------------------------------------------------
                
                dashboardSidebar(
                  width = 200, 
                  sidebarMenu(
                    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                    menuItem("Map", tabName = "Map", icon = icon("dashboard")),
                    menuItem("About", tabName = "about", icon = icon("info-circle"))
                  ),
                  br(),
                  column(
                    width = 12
                  )
                ),
                # End Sidebar
                
                # Body ----------------------------------------------------------------------------------------
                dashboardBody(
                  # Dashboard ----------------------------------------------------------------------------------------
                  tabItems(
                    # Start main dashboard
                    tabItem(
                      tabName = "dashboard",
                      # fluidRow(
                      #   box(width = 12,
                      #       sliderInput("plot.month", label = "Monthly Period", min = 1, max = 6, 
                      #                   value = c(1,6)), round = TRUE, step = 1,
                      #                   animate = animationOptions())
                      #   ),
                      box(
                        # fluidPage(
                        fluidRow(
                          valueBoxOutput("unarmed_count"),
                          valueBoxOutput("month_count"),
                          valueBoxOutput("death_count")
                        ),
                        fluidRow(
                          width = 6,
                          height = 470,
                          leafletOutput("mymap")
                        ),
                        fluidRow(
                          # titlePanel("Demographics by Race"),
                          
                          box(
                            # width = 12,
                            #   sliderInput("plot.age", label = "Age", min = 1, max = 100,
                            #               value = c(1,100)), round = TRUE, step = 1,
                            #               animate = animationOptions())
                            selectInput("var",
                                        label = "Choose a variable to display with Racial Demographics",
                                        choices = list("Poverty" = "pov",
                                                       "Income" = "p_income", 
                                                       "Population" = "pop"),
                                        selected = "pop")
                          )
                        ),
                        
                        fluidRow(
                          # tabBox(
                          height = 470,
                          side = "left",
                          # tabPanel("Full",
                          #          plotOutput("barchart")),
                          # tabPanel("Share",
                          #          plotOutput("barchart.share")),
                          tabPanel("Demographics",
                                   plotOutput("race_density"))
                          
                          # )
                        )),
                      # fluidPage(
                      box(
                        # fluidRow(
                        DTOutput(outputId = "table")                           
                        # ),
                      )
                      
                    ),
                    # About ----------------------------------------------------------------------------------------
                    
                    # Start about tab
                    tabItem(
                      tabName = "about",
                      fluidRow(
                        box(
                          title = p(icon("info-circle"), "About"),
                          h3("Madeleine Williams"),
                          h4("Data Science Student - STAT 302"),
                          br(),
                          p(strong("Email: "), a("madeleinewilliams2025@u.northwestern.edu", href = "mailtoR:madeleinewilliams2025@u.northwestern.edu")),
                          hr(),     
                          # title = p(icon("info-circle"), "About"),
                      
                          h4("Github Repo"),
                          p("Please access my full code on my github repo", 
                            a("here:", 
                              href = "https://github.com/Mctw726/Final_Project_302_MW"),
                            "."),
                          h4("Published Link"),
                          p("View published app", 
                            a("here:", 
                              href = "https://mctw726.shinyapps.io/Final_Project/"),
                            "."),
                          hr(),   
   
                          h4(strong("Source")),
                          p("The Data was sourced from FiveThirtyEight", 
                            a("Police Killings 2015", 
                              href = "https://github.com/fivethirtyeight/data/blob/master/police-killings/police_killings.csv"),
                            "."),
                          p("The secondary data was sourced from Economic Policy Institute", 
                            a("Poverty Information by State", 
                              href = "https://www.epi.org/blog/poverty-rates-decrease-states-2015/"),
                            "."),
                          p("The final dataset used is state shape data", 
                            a("State Polynominal Information", 
                              href = "https://rstudio.github.io/leaflet/json/us-states.geojson"),
                            "."),
                          
                          br(),
                          # What is it for
                          h4(strong("Content Warning")),
                          p(
                            "These visualiztions cover a senitive topic and may not be apporiate for all viewers. Trigger warning: violence, guns, death, police"
                          )
                          
                        ),
                        br(),
                        fluidPage(
                        box(
                          title = p(icon("info-circle"), "Additional Information"),
                          # Source
                          h4(strong("Dashboard")),
                          p("The point of the dashboard is to provide information on the killings done by the police from 01/01/2015 to 01/06/2015. 
                          The first portion of the dashboard is the value boxes; the goal is to provide key facts about the dataset and set up the viewer 
                          with some information. The second portion of the dashboard is the dataset itself which is scrollable across the columns; 
                          the goal of this was to give access to viewers of the whole picture, as my plots do not touch on each variable. 
                          The map on the dashboard represents the concentration of where the deaths took place, and if you press on play, it highlights
                          which happened in which month. I was unable to get the months to play in order despite my most strenuous effort, so they 
                          showcase the deaths in a nonchronological monthly order. Finally, at the bottom, I utilized a widget to change through poverty, 
                          personal income of the deceased, and the population of the county where they were from, all compared to their racial/ethnic information. 
                          The goal was to see if patterns appeared. Are the Black people who are killed of lower socioeconomic means? Do all people killed by the 
                          police live in counties with high poverty rates? I invite you to look at the graphs on the dashboard. The dashboard was created to
                          serve as an introduction to the dataset and also attempt to answer common thoughts about who tends to be killed by the police.
                            "
                          ),
                          br(),
                          h4(strong("Map")),
                          p("I created a map that has a multitude of functions. The first is that it showcases the locations of the deceased based on their race, 
                            which you can easily toggle between. I also added other layers where you could explore which states have the highest number of people killed
                            by police and also a layer (which, in my opinion, is more interesting) that took in an outside dataset to examine the state's poverty rate compared
                            to the average poverty rates of the counties where people were killed by the police. A lighter red state (as the legend shows) means that the people that died
                            in that state tended to be from poorer areas than the state's average. Put together; you can see if certain races have been killed more in certain states
                            than others or if they tend to be lower income than their state's average. I originally wanted to merge the table on the 
                            dashboard with this one, but leaflet.minicharts is not very well built out and does not have layering capabilities (through grouping).
                            "
                            
                          ),
                          br(),
                          h4(strong("Final Words")),
                          p("If you look into the actual code, you will see I have some commented out, I kept it in, as I might want to revisit some of the features I did not implement in at a later date and wanted to keep the code.
                              I hope you have enjoyed my app!
                            
                            "),
                          hr()
                        )),
                     #    box(
                     # ), 
                        br(),
                        # hr(),
                        # fluidRow(
                        # width = '100%',
                       
                        # fluidRow(
                        #   # width = '100%',
                        #   box(
                        #     title = p(icon("info-circle"), "Additional Information"),
                        #     # Source
                        #     h4(strong("Map")),
                        #     p("I created a map that has a multitude of functions. The first being that it showcases the locations of the deceased based on their race, 
                        #     which you can easily toggle between. I also added in other layers where you can explore which states have the highest number of people killed
                        #     by police and also a layer (which in my opinon is more intersting), that took in an outisde dataset to exmine the states poverty rate compared
                        #     the averge poverty rates of the counties where people were killed by the police. A lighter red state (as the legend shows) means that the people killed
                        #     in that state tended to be from poorer areas than the states average. Put together you can see if certain races have been killed more in certain states 
                        #     than others, or if they tend to be killed in states where they might be from poorer areas than the state's average. I orginally wanted to merge the table on the 
                        #     dashboard with this one, but leaflet.minicharts is not very well built out and does not have layering capabilities (through groupping).
                        #     I hope you have enjoyed my app!
                        #     "
                        #       
                        #     ),
                        #     # What is the core concept(s) or insight(s) into the data that you believe the visualization communicates? 
                        #     #   If you choose to include animation then explain why animation helps. 
                        #     # If you build an app then explain your choice of widgets and what about the data it help users understand
                        #     hr()
                        #   ))
                        # )
                      )
                    ),# End about tab
                    # Map ----------------------------------------------------------------------------------------
                    
                    tabItem(
                      tabName = "Map",
                      fluidPage(
                        # width = 10,
                        # height = 600,
                        leafletOutput("map_map", width = '100%', height = 1075)
                        
                        
                      )
                    )
                  )
                ) # End Body
  )
)


# Define server logic required to draw a histogram--------------------------------------------------
var <- c("Poverty" = "pov",
         "Income" = "p_income", 
         "Population" = "pop")
server <- (function(input, output, session) {
  
  
  # Plot data ----------------------------------------------------------------------------------
  
  output$table <- renderDT(police_killings, options = list(scrollX = TRUE, scrollY = TRUE))  
  # Map(Tab #1) - initial part --------------------------------------------------------------------------
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-98.483330, 38.712046, zoom = 4) %>% 
      addMinicharts(lng = killings_per_month$longitude, 
                    lat = killings_per_month$latitude, 
                    time = killings_per_month$month,
                    chartdata = killings_per_month[, c("total")],
                    transitionTime = 2
      )
  }) # End Map - initial part
  
  # Map(Tab #2 complexe) --------------------------------------------------------------------------
  output$map_map <- renderLeaflet({
    map_map <- leaflet(state_2) %>%
      setView(-98.483330, 38.712046, zoom = 4) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(
        fillColor = ~pal(state_2$total),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        dashArray = "2",
        group= "Police Killings per State",
        highlightOptions = highlightOptions(
          weight = 5,
          color = "black",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>% 
      addLegend(pal = pal, 
                values = deaths_state$total, 
                position = "bottomright", 
                title = "Police Violence",
                group = "Police Killings per State", 
                className = "info legend total") %>% 
      #add in cicles for black folks
      addCircleMarkers(lng = police_kill_black$longitude, 
                       lat = police_kill_black$latitude,
                       fillColor = ~pal_black(police_kill_black$total),
                       group = "Black Victims") %>% 
      addCircleMarkers(lng = police_kill_latinx$longitude, 
                       lat = police_kill_latinx$latitude,
                       fillColor = ~pal_latinx(police_kill_latinx$total),
                       group = "Latinx Victims") %>% 
      addCircleMarkers(lng = police_kill_white$longitude, 
                       lat = police_kill_white$latitude,
                       fillColor = ~pal_white(police_kill_white$total),
                       group = "White Victims") %>% 
      addCircleMarkers(lng = police_kill_aapi$longitude, 
                       lat = police_kill_aapi$latitude,
                       fillColor = ~pal_aapi(police_kill_aapi$total),
                       group = "Asian/Pacific Islanders Victims") %>% 
      addCircleMarkers(lng = police_kill_native_american$longitude, 
                       lat = police_kill_native_american$latitude,
                       fillColor = ~pal_native(police_kill_native_american$total),
                       group = "Native Americans Victims") %>% 
      addCircleMarkers(lng = police_kill_Unknown$longitude, 
                       lat = police_kill_Unknown$latitude,
                       fillColor = ~pal_unknown(police_kill_Unknown$total),
                       group = "Unknown Victims") %>% 
      # addMinicharts(lng = killings_per_month$longitude, 
      #               lat = killings_per_month$latitude, 
      #               layerId = as.character(killings_per_month$longitude),
      #               fillColor = "black",
      #               time = killings_per_month$month,
      #               chartdata = killings_per_month[, c("total")],
      #               colorPalette = pal_mini,
      #               transitionTime = 2
      # ) %>%
      addPolygons(
        fillColor = ~pal_pov_2(state_2$perc_difference),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        dashArray = "3",
        group= "Poverty Difference Per State",
        highlightOptions = highlightOptions(
          # weight = 5,
          color = "purple",
          dashArray = "2",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels_state_pov,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>% 
      addLegend(pal = pal_pov_2, 
                values = poverty_state$pov, 
                position = "bottomleft", 
                title = "Poverty Difference %",
                group = "Poverty Difference Per State", 
                className = "info legend total") %>% 
      addLayersControl(
        baseGroups = c("White Victims", "Black Victims", "Latinx Victims", "Asian/Pacific Islanders Victims", "Native Americans Victims", 'Unknown Victims'),
        overlayGroups = c("Police Killings per State", "Poverty Difference Per State"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      hideGroup("Police Killings per State")
  })
  
  
  # Boxes ----------------------------------------------------------------------------
  
  output$unarmed_count <- renderValueBox({
    count_stats <- police_killings %>%
      # filter(month == input$plot.month) %>%
      summarise(deaths = n(),
                unarmed = sum(unarmed == 1))
    
    valueBox("# of Unarmed People killed by the police Jan-Jun 2015",
             value = count_stats$unarmed,
             color = "maroon")
  })
  
  
  output$death_count <- renderValueBox({
    count_stats <- police_killings %>%
      # filter(month == input$plot.month) %>%
      summarise(deaths = n(),
                unarmed = sum(unarmed == 1))
    
    valueBox("# of People killed by the Police Jan-Jun 2015",
             value = count_stats$deaths,
             color = "yellow")
  })
  
  
  output$month_count <- renderValueBox({
    most_recurring_month <- police_killings %>%
      group_by(month) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) 
    
    valueBox("is the month when the most people were killed by the police between Jan-Jun 2015",
             value = "March",
             color = "blue")
  })
  
  
  
  # End Value Boxes
  
  # Barcharts ------------------------------------------------------------------------------------
  
  output$race_density <- renderPlot({
    xaxisgraph <- police_killings[[input$var]]
    xaxisname <- names(var)[var == input$var]
    
    
    police_killings %>%
      ggplot(aes(x = xaxisgraph, fill = raceethnicity)) +
      geom_density(color = "black") +
      facet_wrap(~ raceethnicity) +
      
      # labs(x = "Age", y = "Density", title = "Age Distribution by Race", fill = "Race / Ethnicity") +
      labs(
        x = ifelse(xaxisname == "pov",
                   "Poverty level in the neighborhood where the person was killed",
                   ifelse(
                     xaxisname == "Income",
                     "Income of the deceased",
                     ifelse(
                       xaxisname == "pop",
                       "Income of the deceased",
                       "Population size of county where the deceased lived"
                     ))),
        # title = ifelse(xaxisname == "pov",
        #            "Poverty level by Race of deceased",
        #            ifelse(
        #              xaxisname == "Income",
        #              "Income by Race of deceased",
        #              "Age by Race of deceased"
        #            )),
        y = "Density",
        fill = "Race & Ethnicity",
        title = "Demographic information by Race/Ethncity of those killed by the Police"
        # fill = fillname
      )+
      theme_minimal() +
      scale_fill_brewer(palette = "Blues")+
      theme(
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_blank(),  # Set y-axis text to blank
        # axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(color = "black"),
        legend.text = element_text(color = "black"),
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5)
        # Set hjust to 0.5 for center alignment
      )
  })
  
  # End Barcharts
  
})
# Close --------------------------------------------------------------------




shinyApp(ui = ui, server = server)
