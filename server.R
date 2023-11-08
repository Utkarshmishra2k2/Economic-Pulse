 function(input,output) {
   darkmode(label = "🌙")
  final_data <- reactive({
    (data_01 = subset(data_00,continent==input$continent )) #Here Asia Can Be use in drop down menu
  
    return(data_01)
  })
  
  data_02 <- reactive({
    (data_02 = subset(data_00))
    
    return(data_02)
  })
  
  output$data<-renderDataTable(head({final_data()},3))
  
  output$fig_01<-renderPlotly({ 
    data_01 = final_data()
    fig_01 = plot_ly( x = data_01$gdp , y = data_01$life_expectancy , size = data_01$population , color = data_01$country , frame = data_01$year , text = data_01$country , hoverinfo = "text" , type = "scatter" ,mode = "markers" )
    fig_01 = fig_01 %>% layout(title = "Comparison of GDP and Life Expectancy with population of the Country ",xaxis = list(title = "GDP",type = "log"),yaxis = list(title = "Life Expectancy"))
   return( fig_01 )
    
    
  }) 
  output$fig_02 = renderPlotly({
    data_01 = final_data()
    fig_02 = plot_ly( x = data_01$fertility , y = data_01$country , color = data_01$country , frame = data_01$year , text = data_01$country , hoverinfo = "text" , type = "bar" , mode = "line" )
    fig_02 = fig_02 %>% layout(title = "Comparison of Fertility rate of Country with Year ",xaxis = list(title = "Fertility Rate",type = "log"),yaxis = list(title = "Country"))
    ( fig_02 )
  })
  output$fig_03 = renderPlotly({
    data_01 = final_data()
    fig_03 = plot_ly(data_01 , labels = ~country , values = ~(gdp/population) , type = "pie" )
    fig_03 =  fig_03 %>% layout(title = "GDP per capita  of Countries", xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE) , yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    return( fig_03 )
  })
  
  output$tab02<-renderDataTable(data_02())
}