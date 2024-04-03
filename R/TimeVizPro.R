TimeVizPro<-function(Edata,v1,v2,v3,v4, headr="GPD",Number=32,Fyear=1990){
Countries = Country = Value = ccValue = group = lat = long = oc = c()
utils::globalVariables(c("."))

mpdata <- data.frame(Country=v1, year=v2, Value=v3, iso3=v4)
countriesname <- c(unique(mpdata$Country))
wMap <- map_data("world")
wMap = wMap %>%
  dplyr::mutate(iso3  =
                  countrycode::countrycode(
                    sourcevar = wMap$region, origin = 'country.name', 'iso3c')
  )

ui <- fluidPage(

div(id='page',
tags$head(tags$style('#page {background-color:rgb(22, 46, 74); width:1800px}')),

  titlePanel(h2('Nowcast', style='color:lightblue')),

  sidebarLayout(
sidebarPanel(
tags$head(tags$style('.well {background-color:rgb(10, 20, 50); width: 400px;}')),

selectInput('countries', h4('Country name:', style='color:white') , c('',countriesname),width=500, selectize=FALSE),

selectInput('year', h4('Year:', style='color:white') , c('',Fyear:tail(mpdata$year,1)),width=500, selectize=FALSE)

),
      mainPanel( style="margin-left:20px;",

tabsetPanel(type = "tabs",

tabPanel("Plot",
p('Black points are real values and blue points are predicted ones', style='color:white'),
br(),
fluidRow(
plotOutput("tsp1")
         )),

tabPanel("Table",
div(id="box1",
DT::dataTableOutput("table1")
    ),
tags$body(tags$style('#box1 {background-color:white; margin-right:2%; margin-left:2%;
padding-right:5%; border-radius:5px; width:auto; height:auto;
}'))
),

tabPanel("Map",
ggiraphOutput("mapplot", width = "102%")))
                )

)
)
)



server <- function(input,output){

#### Time series plot:

 output$tsp1 <- renderPlot({
if( input$year==''){
 validate(
     need(input$year!='' , ''))
 }

 if(length(mpdata$Value)<=Number){
 plot(Fyear:input$year, unlist(c(subset(mpdata, mpdata$Country==input$countries & mpdata$year<=input$year,Value))), main=paste(headr, input$countries), xlab='Year', ylab=headr, type='o', pch=19)} else{
 plot(Fyear:input$year, unlist(c(subset(mpdata, mpdata$Country==input$countries & mpdata$year<=input$year,Value))), main=paste(headr, input$countries), xlab='Year', ylab=headr, type='o', pch=19, col=rep(c(1,4),c(Number,length(mpdata$Value)-Number)))
 mtext('blue points are predicted values', side = 4, col=4)
}

})


#### Drawing a Table with sparkline:

    output$table1 <- renderDataTable({
        df <- mpdata %>% 
            mutate(Countries=Country) %>%
            group_by(Countries) %>%
            summarise(Mean = round(mean(Value),0)
            )
        
    })


#### Map:

 output$mapplot <- renderggiraph({
if( input$year==''){
 validate(
     need(input$year!='' , 'Please wait a little after selecting the year'))
 }

mpdata=mpdata[mpdata$year==input$year,]

tempo = dplyr::as_tibble(mpdata)

tempo = dplyr::left_join(tempo, wMap, by = "iso3")

########### tempo=tempo[-c(which(tempo$group %in% NA)),]


tempo$oc = "alert(this.getAttribute(\"data-id\"))"
tempo$ccValue =c(paste(tempo$Country,tempo$Value))

  gg_poly_1=ggplot(data=tempo, aes(x = long, y = lat, group = group)) +
  borders("world", , fill='white') +
  geom_polygon_interactive(aes(fill = Value, tooltip = ccValue, data_id = Value, onclick = oc)) +
  scale_fill_gradientn(colours = rainbow(4)) +
  labs(title = paste(headr,input$year))

   girafe(code = print(gg_poly_1), width_svg = 14, height_svg = 8)

})

}
return(shinyApp(ui=ui, server=server))
}
