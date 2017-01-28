
###  Leaflet, Plotly and Shiny: Weather Forecasts In The Northeast

Integrating JavaScript libraries with R help create interactive visualizations. This blog post uses Leaflet, which is the leading open-source JavaScript library for interactive maps, and plotly to create weather forecast visualizations.


```R
library(jsonlite)
library(data.table)
library(dplyr)

while(TRUE){
    
    all_data=fread("zip_codes_states.csv")# downloaded from https://www.gaslampmedia.com/download-zip-code-latitude-longitude-city-state-county-csv/
    all_data$city=tolower(all_data$city)
    
    all_data=filter(all_data, state%in%c("MD","VA","NY","NJ","MA","CT","DE","ME","NH","PA","RI","WV"))
    
    cities_of_interest=c("anapolis","Virginia Beach","Washington","philadelphia","hartford","dover","Augusta","Albany","Harrisburg","Syracuse","Buffalo","newark","oneonta","erie","toms river","Pittsburgh")
    
    cities_of_interest=tolower(cities_of_interest)
    data_I_want=filter(all_data, city%in%cities_of_interest)
    data_I_want=distinct(data_I_want,city,.keep_all = TRUE)
    key <- "&mykey"  # This is a private key that is obtained by registering in the website
    
    collected_data= c()
    for(i in 1:nrow(data_I_want)){
        
       url <- paste0("http://api.openweathermap.org/data/2.5/forecast/daily?lat=",data_I_want$latitude[i],"&lon=",data_I_want$longitude[i],"&cnt=16")
       
       req <- fromJSON(paste0(url,key))
       city=req$city$name
       city_other=data_I_want$city[i]
       temp=req$list
       date = as.POSIXct(as.numeric(as.character(temp$dt)),origin="1970-01-01",tz="GMT")
       rain=temp$rain
       clouds=temp$clouds
       snow=temp$snow
       pressure=temp$pressure
       humidty=temp$humidity
       wind_speed=temp$speed
       z=temp$temp
       day_temperature=z$day   
       min_temperature =z$min 
       max_temperature =z$max 
       night_temperature=z$night
       eve_temperature =z$eve 
       morn_temperature=z$morn 
       zz=temp$weather
        
        condition = c()
        for(i in 1:length(zz)){
                x=zz[[i]]
                x=x$main
                show(x)
                condition= c(condition,x)
        }

        this_data=data.frame(date=date,city=city,city_other=city_other,
                             condition=tolower(condition), 
                             rain=rain, 
                             clouds  = clouds,
                             snow=snow,
                             pressure =pressure,
                             humidty=humidty,
                             wind_speed=wind_speed,
                             day_temperature=day_temperature,
                             min_temperature =min_temperature,
                             max_temperature=max_temperature,
                             night_temperature=night_temperature,
                             eve_temperature=eve_temperature,
                             morn_temperature=morn_temperature)
         collected_data=rbind(collected_data, this_data)
       }
    
    collected_data= merge(collected_data,data_I_want,by.x="city_other",by.y="city")
    save(collected_data,file="data/collected_data.Rdata")
    Sys.sleep(12*60*60)  # collect data every 12 hrs
    
    }
```

##### server.R


```R
library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(png)
library(stringi)

load("data/collected_data.Rdata")

collected_data$new_city=gsub(" ","_",stri_trans_totitle(collected_data$city_other))

collected_data$maps=paste0("https://www.google.com/maps/place/",collected_data$new_city,"+",collected_data$state)

collected_data$wikipedia= paste0("https://en.wikipedia.org/wiki/",collected_data$new_city,",_",collected_data$state)


myicon=function(condition){
                makeIcon(
                iconUrl = paste0("data/",condition,".png"),
                iconWidth = 70, iconHeight = 70,
                iconAnchorX = 22, iconAnchorY = 94
                )}

collected_data_tomorrow_distinct_city=distinct(collected_data,city_other,.keep_all = TRUE)


shinyServer(function(input, output) {

  output$leaflet <- renderLeaflet({
          top = max(collected_data_tomorrow_distinct_city$latitude,na.rm = T) # north lat
          left = min(collected_data_tomorrow_distinct_city$longitude,na.rm = T) # west long
          right = max(collected_data_tomorrow_distinct_city$longitude,na.rm = T) # west long
          bottom = min(collected_data_tomorrow_distinct_city$latitude,na.rm = T) # south lat
      
          leaflet(collected_data_tomorrow_distinct_city)%>%setMaxBounds(right,bottom,left,top)%>% addTiles(options = providerTileOptions(minZoom = 6, maxZoom = 8)) %>%
                  addMarkers(
                          data = collected_data_tomorrow_distinct_city,
                          icon = myicon(collected_data_tomorrow_distinct_city$condition),
                          popup=~ sprintf(
                                  'City = %s <br/> County = %s <br/> <a href=%s  target="_blank">Open in Google maps</a> <br/> <a href=%s  target="_blank"> Open in Wikipedia</a>', stri_trans_totitle(city_other), county,maps,wikipedia
                                  )
                  )
          })
  
  
  clicked_leaflet <- reactiveValues(clickedMarker=NULL)
  observeEvent(input$leaflet_marker_click,{
          clicked_leaflet$clickedMarker <- input$leaflet_marker_click
                  })
  
  
  selected_coordinates= reactive(({
       c(clicked_leaflet$clickedMarker$lng,clicked_leaflet$clickedMarker$lat)
     }))
  
  output$fish=renderTable({
          selected_data()
  })
    
  selected_data= reactive(({
          if(is.null(clicked_leaflet$clickedMarker))
                  return(NULL)
          
          filter(collected_data, longitude == as.numeric(as.character(selected_coordinates()[1])),latitude==as.numeric(as.character(selected_coordinates()[2])))
  }))

output$max_min_temperature_plotly_16days=renderPlotly({
        temp=selected_data()
        if(is.null(temp))
                return(NULL)
        plot_ly() %>%
                add_lines(x = temp$date, y = temp$max_temperature, name = "Maximum Temperature") %>%
                add_lines(x = temp$date, y = temp$min_temperature, name = "Minimum Temperature")%>%
                add_lines(x = temp$date, y = temp$day_temperature, name = "Day Temperature") %>%
                add_lines(x = temp$date, y = temp$night_temperature, name = "Night Temperature") %>%
                add_lines(x = temp$date, y = temp$eve_temperature, name = "Evening Temperature") %>%
                add_lines(x = temp$date, y = temp$morn_temperature, name = "Morning Temperature") %>%
                layout(title = '',
                       legend = list(orientation = 'h'),
                       xaxis = list(title = ""))
                })
    
  output$max_min_temperature_plotly_3days=renderPlotly({
          temp=selected_data()
          if(is.null(temp))
                  return(NULL)
          
          temp=temp[1:3,]
          plot_ly() %>%
                  add_lines(x = temp$date, y = temp$max_temperature, name = "Maximum Temperature") %>%
                  add_lines(x = temp$date, y = temp$min_temperature, name = "Minimum Temperature")%>%
                  add_lines(x = temp$date, y = temp$day_temperature, name = "Day Temperature") %>%
                  add_lines(x = temp$date, y = temp$night_temperature, name = "Night Temperature") %>%
                  add_lines(x = temp$date, y = temp$eve_temperature, name = "Evening Temperature") %>%
                  add_lines(x = temp$date, y = temp$morn_temperature, name = "Morning Temperature") %>%
                  layout(title = '',
                         legend = list(orientation = 'h'),
                         xaxis = list(title = ""))
  })
  
  output$max_min_temperature_plotly_5days=renderPlotly({
          temp=selected_data()
          if(is.null(temp))
                  return(NULL)
          temp=temp[1:5,]
          plot_ly() %>%
                  add_lines(x = temp$date, y = temp$max_temperature, name = "Maximum Temperature") %>%
                  add_lines(x = temp$date, y = temp$min_temperature, name = "Minimum Temperature")%>%
                  add_lines(x = temp$date, y = temp$day_temperature, name = "Day Temperature") %>%
                  add_lines(x = temp$date, y = temp$night_temperature, name = "Night Temperature") %>%
                  add_lines(x = temp$date, y = temp$eve_temperature, name = "Evening Temperature") %>%
                  add_lines(x = temp$date, y = temp$morn_temperature, name = "Morning Temperature") %>%
                  layout(title = '',
                         legend = list(orientation = 'h'),
                         xaxis = list(title = ""))  
  })
  
  
  output$humidty_rain_cloudness_16days=renderPlotly({
          temp=selected_data()
          if(is.null(temp))
                  return(NULL)
          
          plot_ly(temp, x = ~date, y = ~clouds, type = 'bar', name = 'Clouds') %>%
                  add_trace(y = ~humidty, name = 'Humidity') %>%
                  layout(yaxis = list(title = '%'), barmode = 'group')%>%
                  add_trace(x = ~date, y = ~rain, type = 'scatter', mode = 'lines', name = 'Rainfall', yaxis = 'y2',
                            line = list(color = '#45171D'),
                            hoverinfo = "text",
                            text = ~paste(rain, '°F')) %>%
                  layout(title = '',
                         xaxis = list(title = ""),
                         yaxis = list(side = 'left', title = 'Humidity, clouds (%)', showgrid = FALSE, zeroline = FALSE),
                         yaxis2 = list(side = 'right', overlaying = "y", title = 'Rainfall', showgrid = FALSE, zeroline = FALSE))
})
  
  
  output$humidty_rain_cloudness_5days=renderPlotly({
          temp=selected_data()
          if(is.null(temp))
                  return(NULL)
          temp=temp[1:5,]
          plot_ly(temp, x = ~date, y = ~clouds, type = 'bar', name = 'Clouds') %>%
                  add_trace(y = ~humidty, name = 'Humidity') %>%
                  layout(yaxis = list(title = '%'), barmode = 'group')%>%
                  add_trace(x = ~date, y = ~rain, type = 'scatter', mode = 'lines', name = 'Rainfall', yaxis = 'y2',
                            line = list(color = '#45171D'),
                            hoverinfo = "text",
                            text = ~paste(rain, '°F')) %>%
                  layout(title = '',
                         xaxis = list(title = ""),
                         yaxis = list(side = 'left', title = 'Humidity, clouds (%)', showgrid = FALSE, zeroline = FALSE),
                         yaxis2 = list(side = 'right', overlaying = "y", title = 'Rainfall', showgrid = FALSE, zeroline = FALSE))      
  })
  
  
  output$humidty_rain_cloudness_3days=renderPlotly({
          temp=selected_data()
          if(is.null(temp))
                  return(NULL)
          temp=temp[1:3,]
          plot_ly(temp, x = ~date, y = ~clouds, type = 'bar', name = 'Clouds') %>%
                  add_trace(y = ~humidty, name = 'Humidity') %>%
                  layout(yaxis = list(title = '%'), barmode = 'group')%>%
                  add_trace(x = ~date, y = ~rain, type = 'scatter', mode = 'lines', name = 'Rainfall', yaxis = 'y2',
                            line = list(color = '#45171D'),
                            hoverinfo = "text",
                            text = ~paste(rain, '°F')) %>%
                  layout(title = '',
                         xaxis = list(title = ""),
                         yaxis = list(side = 'left', title = 'Humidity, clouds (%)', showgrid = FALSE, zeroline = FALSE),
                         yaxis2 = list(side = 'right', overlaying = "y", title = 'Rainfall', showgrid = FALSE, zeroline = FALSE))     
                     })
      
  condition1<-reactive({
          if(is.null(selected_data())){
                  result=0
          }else{
                  result=1
          }
          result
      })
  
  output$condition1 <- renderText({
          condition1()
  })
      
  outputOptions(output, 'condition1', suspendWhenHidden=FALSE)

})
```

##### ui.R


```R
library(leaflet)
library(plotly)
shinyUI(fluidPage(
  fluidRow(
      column(width=6,align="center",
             br(),
             tags$h4(tags$strong("Leaflet, Plotly and Shiny: Weather Forecasts In The Northeast",
                     style="text-align:center;color: #0000ff")),
             
               leafletOutput("leaflet",height = 500)
                      ),
      
      column(width=6,align="center",
             conditionalPanel(
                     
                     condition = "output.condition1 == 0",
                     br(),
                     tags$h4("About",style='color:blue'),
                     tags$p('This Shiny App helps visualize weather forecasts in the US Northeast for the next two weeks.
                            The icons show weather conditions for tomorrow. You can click on an icon and then select any of the tabs that show up to see
                            the weather conditions for the next three, five and sixteen days. The weather forecast data includes temperature (minimum, maximum, day time, night time, morning and evening), precipitation, humidity and clouds. 
                            If you click on an icon and then click "Open in Google maps" or "Open in Wikipedia", you will be directed to Google maps or Wikipedia and you can get other information about the specific city. 
                            The app uses the JavaScript libraries Leaflet and Plotly. The data is from',span(tags$a(href="https://openweathermap.org/forecast16", "OpenWeatherMap."),'You can also make the app to update in realtime if you have a server that provides that service. All the plots are interactive.') ,style="text-align:left;color:black;font-size:140%")
                        ),
                     
             
              conditionalPanel(   
                 condition = "output.condition1 == 1",
                  tabsetPanel(
                      tabPanel(tags$em("Temperature",style="font-size:120%"),
                               tags$hr(style="border-color: #ffc266;"),
                               tabsetPanel(
                               tabPanel(tags$em("Three Days"),plotlyOutput("max_min_temperature_plotly_3days")),
                               tabPanel(tags$em("Five Days"), plotlyOutput("max_min_temperature_plotly_5days")),
                               tabPanel(tags$em("Two Weeks"), plotlyOutput("max_min_temperature_plotly_16days"))
                          )),
                      
                      tabPanel(tags$em("Rainfall, Humidity and Clouds",style="font-size:120%"),
                               tags$hr(style="border-color:  #d27979;"),
                               tabsetPanel(
                                       tabPanel(tags$em("Three Days"),plotlyOutput("humidty_rain_cloudness_3days")),
                                       tabPanel(tags$em("Five Days"),plotlyOutput("humidty_rain_cloudness_5days")),
                                       tabPanel(tags$em("Two Weeks"),plotlyOutput("humidty_rain_cloudness_16days"))
                               ))
              )))
        ))
)
```
