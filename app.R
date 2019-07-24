library(shiny)
library(plotly)
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)

#all county data
county = read.csv('MotorVehicleFuelTypes_County.csv')
  cou <- st_read('CA_Counties_TIGER2016.shp')
  cou <- cou[order(cou$NAME),]
  cou['EV Percentage'] = as.numeric(sub("%","",county[['X.BEV.PEV']]))
  county_map <- tm_shape(cou)+
    tm_polygons("EV Percentage",id='NAME',palette='RdYlBu')

city = read.csv('place_dmv_census.csv')
city['EV Percentage'] = as.numeric(sub("%","",city[[137]]))
  cit = st_read('cb_2017_06_place_500k.shp')
  citys <- suppressWarnings(inner_join(cit,city,by=c('NAME'='City')))
  city_map <- tm_shape(citys)+
    tm_polygons('EV Percentage',id='NAME',palette='RdYlBu')+
    tmap_options(max.categories = 251)

zip = read.csv('2016_5YR_ZCTA_CENSUSxDMV_ZIP(CALIFORNIAONLY).csv')
zip[,'Geo_ZCTA5'] <-factor(zip[,'Geo_ZCTA5'])
zi <- st_read('tl_2010_06_zcta510.shp')
  zips = suppressWarnings(inner_join(zi,zip,by=c('ZCTA5CE10'='Geo_ZCTA5')))
   a=zip[[90]]
   zipB=suppressWarnings(as.numeric(levels(a))[a])
   b=zip[[97]]
   zipP=suppressWarnings(as.numeric(levels(b))[b])
   c=zip[[104]]
   zipG=suppressWarnings(as.numeric(levels(c))[c])
   zips['EV Percentage'] = (100*(zipB+zipP)/zipG)
  zip_map <- tm_shape(zips)+
    tm_polygons('EV Percentage',id='ZCTA5CE10',palette='RdYlBu')

zcc  <- c('County', 'City/Place','Zip Code')
out  <- c('Electric Vehicle Ownership','Vehicle Makeup','Average Income vs. EV Ownership','Vehicles Per Household',"EV's per 100,000 People")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("type","Location Scale:",zcc),
      selectInput("figure","Data:",out)
    ),
    mainPanel(
      conditionalPanel(condition= "input.figure=='Average Income vs. EV Ownership'",plotlyOutput('income')), 
      conditionalPanel(condition= "input.figure=='Electric Vehicle Ownership'",leafletOutput("map")),
      conditionalPanel(condition= "input.figure=='Vehicle Makeup'",plotlyOutput('makeup')),
      conditionalPanel(condition= "input.figure=='Vehicles Per Household'",plotlyOutput('household'))
)
))

server <- function(input,output) {
  output$map <- renderLeaflet({
    if ((input$type == zcc[1])&(input$figure == out[1])) {
      tmap_leaflet(county_map)
    } else if ((input$type == zcc[2])&(input$figure == out[1])) {
      tmap_leaflet(city_map)
    } else if ((input$type == zcc[3])&(input$figure == out[1])) {
      tmap_leaflet(zip_map)
    }
  })
  output$makeup <- renderPlotly({
    if ((input$type == zcc[1])&(input$figure == out[2])) {
      plot_ly(x=as.numeric(sub("%","",county[['Gasoline.1']])),y=as.character(county[['County']]),name='Gasoline %',type='bar', orientation = 'h',height=1000) %>%
        add_trace(x=as.numeric(sub("%","",county[['X.BEV.PEV']])),name= 'BEV-PEV %') %>%
        add_trace(x=as.numeric(sub("%","",county[['Diesel.1']])),name= 'Diesel %') %>%
        add_trace(x=as.numeric(sub("%","",county[['Hybrid.Gas.1']])),name= 'Hybrid Gas %') %>%
        add_trace(x=as.numeric(sub("%","",county[['Ethanol.1']])),name= 'Ethanol %') %>%
        add_trace(x=as.numeric(sub("%","",county[['Diesel.Hybrid.1']]))+as.numeric(sub("%","",county[['Fuel.Cell.1']]))+as.numeric(sub("%","",county[['Butane.1']]))+as.numeric(sub("%","",county[['Compressed.Natural.Gas.1']]))+as.numeric(sub("%","",county[['Propane.1']]))+as.numeric(sub("%","",county[['Diesel.Hybrid.1']]))+as.numeric(sub("%","",county[['Methanol.1']]))+as.numeric(sub("%","",county[['Methane.1']])),name= 'Other %') %>%
        layout(barmode = 'stack')
    } else if ((input$type == zcc[2])&(input$figure == out[2])) {
      plot_ly(x=as.numeric(sub("%","",city[['Gasoline.1']])),y=as.character(city[['City']]),name='Gasoline %',type='bar', orientation = 'h',height=1000) %>%
        add_trace(x=as.numeric(sub("%","",city[['X.BEV.PEV']])),name= 'BEV-PEV %') %>%
        add_trace(x=as.numeric(sub("%","",city[['Diesel.1']])),name= 'Diesel %') %>%
        add_trace(x=as.numeric(sub("%","",city[['Hybrid.Gas.1']])),name= 'Hybrid Gas %') %>%
        add_trace(x=as.numeric(sub("%","",city[['Ethanol.1']])),name= 'Ethanol %') %>%
        add_trace(x=as.numeric(sub("%","",city[['Diesel.Hybrid.1']]))+as.numeric(sub("%","",city[['Fuel.Cell.1']]))+as.numeric(sub("%","",city[['Butane.1']]))+as.numeric(sub("%","",city[['Compressed.Natural.Gas.1']]))+as.numeric(sub("%","",city[['Propane.1']]))+as.numeric(sub("%","",city[['Diesel.Hybrid.1']]))+as.numeric(sub("%","",city[['Methanol.1']]))+as.numeric(sub("%","",city[['Methane.1']])),name= 'Other %') %>%
        layout(barmode = 'stack',xaxis=list(range = c(0, 100)))
    } else if ((input$type == zcc[3])&(input$figure == out[2])) {
      plot_ly(x=zip[[23]],y=(100*(zipB+zipP)/zipG),type='scatter',mode='markers',color = I("darkolivegreen3"),text = zip[[4]]) %>%
        layout(yaxis = list(range = c(0, 7)))
    }
  })
  output$income <- renderPlotly({
    if ((input$type == zcc[1])&(input$figure == out[3])) {
      plot_ly(x=county[[49]],y=county[[32]],type='scatter',mode='markers',color = I("darkolivegreen3"),text = county[[1]])
    } else if ((input$type == zcc[2])&(input$figure == out[3])) {
      plot_ly(x=city[[33]],y=city[['EV Percentage']],type='scatter',mode='markers',color=I("darkolivegreen3"),text=city[[5]])
    } else if ((input$type == zcc[3])&(input$figure == out[3])) {
      plot_ly(x=zip[[23]],y=(100*(zipB+zipP)/zipG),type='scatter',mode='markers',color = I("darkolivegreen3"),text = zip[[4]]) %>%
      layout(yaxis = list(range = c(0, 7)))
    }
  })
  output$household <- renderPlotly({
    if ((input$type == zcc[1])&(input$figure == out[4])) {
      plot_ly(x=as.numeric(county[["Gasoline"]])/county[["HOUSEHOLDS"]],y=as.character(county[['County']]),name='Gasoline per HH',type='bar', orientation = 'h',height=1000)%>%
        add_trace(x=as.numeric(county[['Battey.Electric']]+county[['Plug.in.Hybrid']])/county[["HOUSEHOLDS"]],name= 'BEV-PEV per HH') %>%
        layout(barmode = 'stack')
    } else if ((input$type == zcc[2])&(input$figure == out[4])) {
      plot_ly(x=city[[33]],y=city[['EV Percentage']],type='scatter',mode='markers',color=I("darkolivegreen3"),text=city[[5]])
    } else if ((input$type == zcc[3])&(input$figure == out[4])) {
      plot_ly(x=zip[[23]],y=(100*(zipB+zipP)/zipG),type='scatter',mode='markers',color = I("darkolivegreen3"),text = zip[[4]]) %>%
        layout(yaxis = list(range = c(0, 7)))
    }
  })
  
}

shinyApp(ui=ui,server = server)