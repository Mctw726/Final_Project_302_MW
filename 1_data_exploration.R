#Load Packages -----------------------------------------------------------------
library(tidymodels)
library(readr)
library(ggplot2)
library(viridis)
library(tigris)
library(usmap)
library(leaflet)
library(leaflet.minicharts)
library(sf)
library(tidyverse)
library(maptools)
library(ggplot2)

police_killings <- read_csv("data/processed/police_killings_clean.csv") %>% 
  janitor::clean_names()
#Load info for maps (I created in my R scripts)
load("data/wrangled/data_wrangling.rda")

#Load in race data for maps (I created in my R scripts)
load("data/wrangled/race_info_4_map.rda")
load("data/wrangled/state_info_pov.rda")

state_shape <- sf::read_sf("data/processed/state_data.dbf")
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#simple explorations------------------------------------------------------------
# Police killings by age
police_killings %>% 
ggplot(aes(x = age)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(x = "Age", y = "Density", title = "Age Distribution") + 
  theme_minimal()

# Police killings by age by race
age_shiny <- police_killings %>% 
  ggplot(aes(x = age, fill = raceethnicity)) +
  geom_density(color = "black") +
  facet_wrap(~ raceethnicity) +
  labs(x = "Age", y = "Density", title = "Age Distribution by Race", fill = "Race / Ethnicity") + 
  theme_minimal() 


police_killings %>%
  ggplot(aes(x = age, fill = raceethnicity)) +
  geom_density(color = "black") +
  facet_wrap(~ raceethnicity) +
  labs(x = "Age", y = "Density", title = "Age Distribution by Race", fill = "Race / Ethnicity") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.background = element_rect(fill = "white"),
    legend.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5)  # Set hjust to 0.5 for center alignment
  )

# police_killings %>%
#   ggplot(aes(x = age, fill = raceethnicity)) +
#   facet_wrap(~ raceethnicity) +
#   geom_col() +
#   labs(x = "Race / Ethnicity", y = "Age", title = "Age Distribution by Race", fill = "Race / Ethnicity") +
#   theme_minimal()+
#   theme(
#     panel.background = element_rect(fill = "white"),  # Set the background color of the panel
#     panel.grid.major = element_blank(),  # Remove major grid lines
#     panel.grid.minor = element_blank(),  # Remove minor grid lines
#     axis.line = element_line(color = "black"),  # Set the color of axis lines
#     axis.text = element_text(color = "black"),  # Set the color of axis text
#     axis.title = element_text(color = "black"),  # Set the color of axis titles
#     legend.background = element_rect(fill = "white"),  # Set the background color of the legend
#     legend.title = element_text(color = "black"),  # Set the color of the legend title
#     legend.text = element_text(color = "black"),  # Set the color of the legend text
#     plot.title = element_text(color = "black", size = 14, face = "bold")  # Set the color, size, and font face of the plot title
#   )


save(age_shiny, file = "shiny_graph.rda")

#Police killings by Gender
police_killings %>% 
  ggplot(aes(x = gender, fill = gender)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(x = "Gender", y = "Count", title = "Gender Distribution") +
  theme_bw()

#Police killings by race
# Calculate the count of each raceethnicity category
race_counts <- police_killings %>%
  count(raceethnicity)

# Order the levels of raceethnicity based on count
police_killings <- police_killings %>%
  mutate(raceethnicity = factor(raceethnicity, levels = race_counts$raceethnicity[order(race_counts$n)]))

# Create the bar plot with reordered bars
police_killings %>%
  ggplot(aes(x = raceethnicity, fill = raceethnicity)) +
  geom_bar(color = "black", width = 0.6, alpha = 0.8) +
  labs(x = "Race", y = "Count", title = "Race Distribution") +
  scale_fill_manual(values = c("#3366CC", "#FF9900", "#109618", "#990099", "#DC3912", "#0099C6")) +
  geom_text(stat = "count", aes(label = ..count..), hjust = -0.2, vjust = 0.5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold",hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none") +
  coord_flip()

#This map works:
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
#####################
state_info <- police_killings %>% 
  select(longitude, latitude, state)

police_killings_month <- police_killings %>%
  group_by(month, state) %>%
  summarize(total=n())

police_killings_state <- police_killings %>%
  group_by(state) %>%
  summarize(total=n())

killings_per_month <- inner_join(state_info, police_killings_month)


  map_per_month <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addMinicharts(lng = killings_per_month$longitude, 
                lat = killings_per_month$latitude, 
                time = killings_per_month$month,
                chartdata = killings_per_month[, c("total")],
                transitionTime = 2
  )

save(map_per_month, file = "map_mini_charts.rda")
map_basic <- leaflet(states) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron")

deaths_state <- police_killings %>%
  group_by(state) %>%
  summarize(=n())

bin <- c(0, 5, 10, 15, 25, 50, 75, 100, Inf)
pal <- colorBin("Paired", domain = deaths_state$total, bins = bins)




leaflet(states) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
    fillColor = ~pal(deaths_state$total),
    weight = 2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    dashArray = "2",
    highlightOptions = highlightOptions(
      weight = 5,
      color = "black",
      dashArray = "",
      fillOpacity = 1,
      bringToFront = TRUE),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
    ) %>% 
  addLegend(pal = pal, 
            values = deaths_state$total, 
            position = "bottomright", 
            title = "Police Violence",
            group = "total", 
            className = "info legend total") 


addMinicharts(map_basic,lng = killings_per_month$longitude, 
                lat = killings_per_month$latitude, 
                time = killings_per_month$month,
                chartdata = killings_per_month[, c("total")],
                transitionTime = 2
  )%>% 
updateMinicharts(
  layerID = "Months"
)

#-------------------------------------------------------------------------------

map_3 <- leaflet(states) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
    fillColor = ~pal(deaths_state$total),
    weight = 2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    dashArray = "2",
    group= "total",
    highlightOptions = highlightOptions(
      weight = 5,
      color = "black",
      dashArray = "",
      fillOpacity = 1,
      bringToFront = TRUE),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>% 
  addLegend(pal = pal, 
            values = deaths_state$total, 
            position = "bottomright", 
            title = "Police Violence",
            layerId = "total", 
            className = "info legend total") %>% 
  addMinicharts(lng = killings_per_month$longitude, 
                lat = killings_per_month$latitude, 
                time = killings_per_month$month,
                chartdata = killings_per_month[, c("total")],
                transitionTime = 2
  ) 
map_3

map_basic %>% 
  addTimeslider(data = killings_per_month,
                options = timesliderOptions(
                  position = "topright",
                  timeAttribute = "month",
                  range = TRUE)) 

#####---- Try to make mini charts into circles
addMinicharts(lng = killings_per_month$longitude, 
              lat = killings_per_month$latitude, 
              time = killings_per_month$month,
              chartdata = killings_per_month[, c("total")],
              transitionTime = 2
) 
#####---- With widget-----------------------------------------------------------
states <- states(cb = F)

labels <- sprintf(
  "In <strong>%s</strong><br/><strong>%g<strong> people were killed by the police<br/>from January to May 2015 ",
  deaths_state$state, deaths_state$total
) %>% lapply(htmltools::HTML)


map_widget  <-leaflet(states) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
    fillColor = ~pal(deaths_state$total),
    weight = 2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    dashArray = "2",
    group= "total",
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
      direction = "auto")) %>% 
  addLegend(pal = pal, 
            values = deaths_state$total, 
            position = "bottomright", 
            title = "Police Violence",
            layerId = "total", 
            className = "info legend total")   

save(map_widget, labels, file = "map_with_widget.rda")

###_____________ PA OUTSIDE TRY
police_killings_race <- police_killings %>%
  group_by(raceethnicity, state) %>%
  summarize(total=n()) %>% 
  pivot_wider(names_from = raceethnicity, values_from = total)

state_info_race <- police_killings %>% 
  select(longitude, latitude, state, raceethnicity)

killings_per_month_race <- inner_join(state_info_race, police_killings_race)


  addPolygons(
    fillColor = ~pal(deaths_state$total),
    weight = 2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    dashArray = "2",
    group= "total",
    highlightOptions = highlightOptions(
      weight = 5,
      color = "black",
      dashArray = "",
      fillOpacity = 1,
      bringToFront = TRUE),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) 


  map_3 %>% 
  addLayersControl(
    overlayGroups = c("total", "month"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

  
save(map_3, file = "map_3.rda")

state_info <- police_killings %>% 
  select(longitude, latitude, state)

police_killings_month <- police_killings %>%
  group_by(month, state) %>%
  summarize(total=n())

killings_per_month_2 <- inner_join(state_info, police_killings_month)
##try 2
pal_2 <- colorBin("", domain = killings_per_month_2$total, bins = bins)

map_basic %>% 
  addPolygons(data = states, layerId = states$id) %>%
addMinicharts(lng = killings_per_month_2$longitude, 
              lat = killings_per_month_2$latitude, 
              time = killings_per_month_2$month,
              fillColor = ~pal_2(killings_per_month_2$total),
              chartdata = killings_per_month_2[, c("total")],
              transitionTime = 2
            
)
#### New thing? Pie chart?
leaflet(states) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addMinicharts(lng = killings_per_month_2$longitude, 
                lat = killings_per_month_2$latitude, 
                time = killings_per_month_2$month,
                fillColor = ~pal_2(killings_per_month_2$total),
                chartdata = killings_per_month_2[, c("total")],
                transitionTime = 2
                
  )
##########----------------------------------------------------------------------
#Data wrangling 


# Black
police_kill_black <- subset(
  x = police_killings,
  subset = police_killings$raceethnicity == "Black"
)
police_kill_black <- police_kill_black %>%
  group_by(state, raceethnicity) %>%
  summarize(total=n()) 

state_info_race <- police_killings %>% 
  select(longitude, latitude, state, raceethnicity)

police_kill_black <- inner_join(state_info_race, police_kill_black)

pal_black <- colorBin("BrBG", domain = police_kill_black$total)

#White
police_kill_white <- subset(
  x = police_killings,
  subset = police_killings$raceethnicity == "White"
)
police_kill_white <- police_kill_white %>%
  group_by(state, raceethnicity) %>%
  summarize(total=n()) 

police_kill_white <- inner_join(state_info_race, police_kill_white)

pal_white <- colorBin("BuPu", domain = police_kill_white$total)

#Hispanic/Latino
police_kill_latinx <- subset(
  x = police_killings,
  subset = police_killings$raceethnicity == "Hispanic/Latino"
)

police_kill_latinx <- police_kill_latinx  %>%
  group_by(state, raceethnicity) %>%
  summarize(total=n()) 

police_kill_latinx <- inner_join(state_info_race, police_kill_latinx)

pal_latinx <- colorBin("Blues", domain = police_kill_latinx$total)

#Asian Pacific Islander
police_kill_aapi <- subset(
  x = police_killings,
  subset = police_killings$raceethnicity == "Asian/Pacific Islander"
)

police_kill_aapi <- police_kill_aapi %>%
  group_by(state, raceethnicity) %>%
  summarize(total=n()) 

police_kill_aapi <- inner_join(state_info_race, police_kill_aapi)

pal_aapi <- colorBin("Reds", domain = police_kill_aapi$total)

#Native American
police_kill_native_american <- subset(
  x = police_killings,
  subset = police_killings$raceethnicity == "Native American"
)

police_kill_native_american <- police_kill_native_american %>%
  group_by(state, raceethnicity) %>%
  summarize(total=n()) 

police_kill_native_american <- inner_join(state_info_race, police_kill_native_american)

pal_native <- colorBin("YIGn", domain = police_kill_native_american$total)
#Unkown
police_kill_Unknown <- subset(
  x = police_killings,
  subset = police_killings$raceethnicity == "Unknown"
)
police_kill_Unknown  <- police_kill_Unknown  %>%
  group_by(state, raceethnicity) %>%
  summarize(total=n()) 

police_kill_Unknown  <- inner_join(state_info_race, police_kill_Unknown )

pal_unknown <- colorBin("Oranges", domain = police_kill_Unknown$total)


save(pal_unknown, police_kill_Unknown, police_kill_native_american, pal_native, pal_aapi, police_kill_aapi, police_kill_latinx, pal_latinx, pal_black, 
     police_kill_black, police_kill_white, pal_white, file = "data/wrangled/race_info_4_map.rda")
#Poly 1 info

labels <- sprintf(
  "In <strong>%s</strong><br/><strong>%g<strong> people were killed by the police<br/>from January to May 2015 ",
  state_2$state, state_2$total
) %>% lapply(htmltools::HTML)
bins <- c(0, 5, 10, 15, 25, 50, 75, 100, Inf)
pal <- colorBin("Greens", domain = state_2$total, bins = bins)
deaths_state <- deaths_state[-indices_to_remove2, ]
deaths_state_2 <- 
deaths_state <- 
non_overlapping_states <- deaths_state$state[!(state_2$state %in% deaths_state$state)]
state_list$state <- state_list$State

state_2 <- full_join(state_2, state_list)


#end of data wrangling
leaflet(state_2) %>%
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

save(map_map, file = "done_map.rda")


save(state_2, deaths_state, state_list, killings_per_month, killings_per_state, 
     pal_pov, pal_pov_2, pal, poverty_state, labels_state_pov, labels, real_poverty,
     bins_pov_2, bins_pov, bins, poverty_state, state_2, file = "data/wrangled/data_wrangling_new.rda")

write.csv(state_2, file = "data/processed/state_2.csv")

load("data/wrangled/data_wrangling.rda")


         

#save it 
save( map_7_func, file = "map_killing_race.rda")

#---------
pal_mini = c("darkred", "lightgreen")
map_basic %>% 
addMinicharts(lng = killings_per_month$longitude, 
              lat = killings_per_month$latitude, 
              layerId = as.character(killings_per_month$longitude),
              time = killings_per_month$month,
              chartdata = killings_per_month[, c("total")],
              colorPalette = "colors",
              transitionTime = 2,
              popup = 
) 


#---- Poverty Rate
poverty_state <- police_killings %>% 
  group_by(state_fp) %>% 
  select(state_fp, state, pov)

poverty_state <- poverty_state[order(poverty_state$state_fp, decreasing = FALSE), ]

bins_pov <- c(0, 5, 10, 15, 25, 50, 75, 100)
pal_pov <- colorBin("Reds", domain = poverty_state$pov, bins = bins_pov_2)

# states_shape <- states_shape %>% 
#   filter(states_shape)

# pivoted_data <- police_killings %>%
#   select(state, pov) %>% 
#   group_by(state) %>%
#   # mutate(row_id = row_number()) %>%
#   pivot_wider(names_from = row_id, values_from = pov)
state_list <- data.frame(
  State = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
    "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
    "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
    "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
    "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ),
  Abbreviation = c(
    "AL", "AK", "AZ", "AR", "CA",
    "CO", "CT", "DE", "FL", "GA",
    "HI", "ID", "IL", "IN", "IA",
    "KS", "KY", "LA", "ME", "MD",
    "MA", "MI", "MN", "MS", "MO",
    "MT", "NE", "NV", "NH", "NJ",
    "NM", "NY", "NC", "ND", "OH",
    "OK", "OR", "PA", "RI", "SC",
    "SD", "TN", "TX", "UT", "VT",
    "VA", "WA", "WV", "WI", "WY"
  ),
  stringsAsFactors = FALSE
)

pivoted_data <- aggregate(pov ~ state, data = police_killings, FUN = function(x) paste(x, collapse = ","))
selected_columns <- pivoted_data [, grep("pov", names(pivoted_data ))]
pov_list <- strsplit(selected_columns, ",")
sum_per_group <- sapply(pov_list, function(x) mean(as.numeric(x)))
pivoted_data$pov_avg <- sum_per_group
pivoted_data <- pivoted_data[, -which(names(pivoted_data) == "pov")]
overlapping_states <- pivoted_data$state[pivoted_data$state %in% state_list$Abbreviation]
non_overlapping_states <- state_list$Abbreviation[!(state_list$Abbreviation %in% pivoted_data$state)]

indices_to_remove <- c(34, 39, 41, 45)  
state_list <- state_list[-indices_to_remove, ]
indices_to_remove2 <- c(8)  
pivoted_data <- pivoted_data [-indices_to_remove2, ]

pivoted_data$state <- state_list$State
state_shape$state <- str_to_title(state_shape$state)

state_2 <- full_join(state_shape, pivoted_data)

save(state_2, file = "state_info_pov.rda")
load("data/wrangled/state_info_pov.rda")

labels_state_pov <- sprintf(
  "In <strong>%s</strong><br/> the percentage differnence in the state's poverty to the neighborhoods <br/>a person was killed by the police is <strong>%g<strong>",
  # "In <strong>%s</strong><br/><strong>%g<strong> people were killed by the police<br/>from January to May 2015 ",
  state_2$state, state_2$perc_difference
) %>% lapply(htmltools::HTML) 
bins_pov_2 <- c(-Inf, -50, -35, -15, -5, 0, 15, 35, 50, Inf)
pal_pov_2 <- colorBin("Reds", domain = state_2$perc_difference, bins = bins_pov_2)


leaflet(state_2) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  # addPolygons(label = states$name) %>%
  addPolygons(
    fillColor = ~pal_pov(poverty_state$pov),
    weight = 2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    dashArray = "3",
    group= "Poverty Per State",
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
  addLegend(pal = pal_pov, 
            values = poverty_state$pov, 
            position = "bottomright", 
            title = "Poverty %",
            group = "Poverty Per State", 
            className = "info legend total") 


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
#
# Create a new column "Difference" with the difference between two columns
state_2$difference <- as.numeric(state_2$pov_real) - as.numeric(state_2$pov_avg)
state_2$perc_difference <- (as.numeric(state_2$pov_real) - as.numeric(state_2$pov_avg)) / as.numeric(state_2$pov_avg) * 100
bins_pov_2 <- c(-75, -50, -35, -15, -5, 0, 15, 50)
pal_pov <- colorBin("Paired",  bins = bins_pov_2)

poverty_layer <- leaflet(state_2) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  # addPolygons(label = states$name) %>%
  addPolygons(
    fillColor = ~pal_pov(state_2$perc_difference),
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
  addLegend(pal = pal_pov, 
            values = poverty_state$pov, 
            position = "bottomright", 
            title = "Poverty Differnce",
            group = "Poverty Difference Per State", 
            className = "info legend total") 

save(poverty_layer, file = "poverty_layer.rda")

##Education -- (Go in an add education rate)


leaflet(state_2) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  # addPolygons(label = states$name) %>%
  addPolygons(
    fillColor = ~pal_pov(state_2$perc_difference),
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
  addLegend(pal = pal_pov, 
            values = poverty_state$pov, 
            position = "bottomright", 
            title = "Poverty Differnce",
            group = "Poverty Difference Per State", 
            className = "info legend total") 

#https://www.epi.org/blog/poverty-rates-decrease-states-2015/

####---------------------


#Interactive Map Creation ------------------------------------------------------
# 
# class(states)
# 
# #1 create a map that shows how white each state is
# library(censusapi)
# Sys.setenv(CENSUS_KEY="5751ee86b72f03b9ea3fbd1f7bd9ff64fe7fc4ae")
# 
# # Reload .Renviron
# readRenviron("~/.Renviron")
# # Check to see that the expected key is output in your R console
# Sys.getenv("CENSUS_KEY")
# 
# state_pop <-  getCensus(name="acs/acs5", 
#                         vintage = 2015,
#                         key = Sys.getenv("CENSUS_KEY"),
#                         vars=c("NAME", "B01003_001E"), 
#                         region="state:*")
# colnames(state_pop) <- c("state_fp", "state", "population")
# state_pop$state_fp <- as.numeric(state_pop$state_fp)
# 
# states <- states(cb=T)
# 
# states_coord <- states %>% 
#   st_point_on_surface() %>% 
#   st_coordinates() %>% 
#   as_tibble() %>% 
#   rename(lon = 1, lat = 2) %>% 
#   bind_cols(states %>% st_drop_geometry())
# 
# killing_by_state <- police_killings %>%
#   group_by(state) %>%
#   summarize(total=n())
# 
# pv_state_pop <- left_join(killing_by_state, state_pop)
# 
# states_merged_pv_pop <- geo_join(states, pv_state_pop, "STUSPS", "state")
# 
# leaflet()
# addProviderTiles("CartoDB.Positron") %>%
#   setView(-98.483330, 38.712046, zoom = 4) %>% 
# addPolygons(data = states_merged_pv_pop , 
#             # fillColor = ~pal_pop(states_merged_pv_pop$per_capita), 
#             fillOpacity = 0.9, 
#             weight = 0.2, 
#             smoothFactor = 0.2,
#             highlight = highlightOptions(
#               weight = 5,
#               color = "#666",
#               fillOpacity = 0.7,
#               bringToFront = TRUE),
#             # label=popup_pv,#edit popup
#             labelOptions = labelOptions(
#               style = list("font-weight" = "normal", padding = "3px 8px"),
#               textsize = "15px",
#               direction = "auto"),
#             group = "Per Capita") %>% 
#   addLayersControl(
#     baseGroups = c("Per Capita"), #could add groups: race and gender
#     options = layersControlOptions(collapsed = FALSE)
#   )
# 
# map_basic <- leaflet()
# addProviderTiles("CartoDB.Positron") %>%
#   setView(-98.483330, 38.712046, zoom = 4)
# 
#   
# 
# #data wrangling 
# state_info_white <- police_killings %>% 
#   select(longitude, latitude, state, share_white)
# 
# state_info_white <- as.data.frame(state_info_white)
# 
# map_per_month <- leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   setView(-98.483330, 38.712046, zoom = 4) %>% 
#   addMinicharts(lng = killings_per_month$longitude, 
#                 lat = killings_per_month$latitude, 
#                 time = killings_per_month$month,
#                 chartdata = killings_per_month[, c("total")],
#                 transitionTime = 2
#   ) 