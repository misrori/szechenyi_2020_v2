library(shiny)
library(DT)
library(plotly)
library(data.table)
library(rgdal)
library(leaflet)
source('load_map.R')

function(input, output, session) {
  adat <- fread('szechenyi2020_adatok.csv', stringsAsFactors = F)
  adat <- adat[,c(1:8, 15, 9:14), with=F]
  adat$datum <- as.Date(adat$datum)
  adat$forras<- as.factor(adat$forras)
  adat$operativ_program <- as.factor(adat$operativ_program)
  adat$varos <- as.factor(adat$varos)
  adat$nyertes <- as.factor(adat$nyertes)
  adat$Jogallas <- as.factor(adat$Jogallas)
  adat$Megye <- as.factor(adat$Megye)
  adat$Kisterseg <- as.factor(adat$Kisterseg)
  adat$tipus <- as.factor(adat$tipus)
  adat$roma_onkormanyzat <- as.factor(adat$roma_onkormanyzat)
  adat$Lako_nepesseg <- as.numeric(adat$Lako_nepesseg)
  
  regio_map <- readOGR("geo_map/regio.geojson", "OGRGeoJSON")
  megye_map <-  readOGR("geo_map/megye.geojson", "OGRGeoJSON")
  kisterseg_map <- readOGR("geo_map/kist.geojson", "OGRGeoJSON")
  helyseg_map <-  readOGR("geo_map/helyseg.geojson", "OGRGeoJSON")
  helyseg_map$osszeg<- as.numeric(helyseg_map$osszeg)
  geo_adatok <- fread('geo_map/geo_adatok.csv')
  geo_adatok$ev <- as.factor(year(geo_adatok$datum))
  
  
  osszes_nyertes <- reactive({
   adatom <- adat
   setorder(adatom, -datum )
    names(adatom) <- c('Forrás', 'Operatív program', 'Program', 'Város', 'Nyertes', 'Leírás',
                     'Megítélés dátuma', 'Megítélt összeg (millió Ft)','Megítélés éve' ,'Település jogállása','Megye', 'Kistérség', 'Népesség',
                     'Roma önkormányzat', 'Hátrányos helyzet besorolás' )
    
    return(adatom)
  })
  
  output$summary <- renderPrint({
   my_text
  })
  
  output$table <- DT::renderDataTable(
    DT::datatable(osszes_nyertes(),extensions = c('Buttons','FixedHeader'),class = 'cell-border stripe',rownames = FALSE,
                  filter = 'top', options = list(dom = 'Blfrtip', fixedHeader = TRUE,pageLength = 50,lengthMenu = c(10,50,500,5000, 10000, 25000 ),
                                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                 columnDefs = list())) %>%
      formatCurrency(8, '')
  )
  
  g_by <- reactive({
    as.character(input$group_by)
  })
  g_by2 <- reactive({
    as.character(input$group_by2)
  })
  g_by3 <- reactive({
    as.character(input$group_by3)
  })
  g_by_plot <- reactive({
    as.character(input$plot_osszegzo)
  })
  
  final_data <- reactive({
    by1 <- g_by()
    by2 <- g_by2()
    by3 <- g_by3()
    osszeitendo_adat <- adat
    if(by1=="" & by2==""& by3==''){
      return(osszes_nyertes())
    }
    else if(by1!=''& by2=='' & by3==''){
      return(osszeitendo_adat[, list('Megítélt összeg (millió Ft)'= round(sum(osszeg),2),'Nyertes pályázatok száma'=.N), by=by1])
    }
    else if(by1!=''& by2!='' & by3==''){
      return(adat[,list('Megítélt összeg (millió Ft)'= round(sum(osszeg),2),'Nyertes pályázatok száma'=.N), by=c(by1, by2)])
    }
    else if(by1!=''& by2!='' & by3!=''){
      return(adat[,list('Megítélt összeg (millió Ft)'= round(sum(osszeg),2),'Nyertes pályázatok száma'=.N), by=c(by1, by2, by3)])
    }
  })
  
  output$eredmeny <- DT::renderDataTable(
    DT::datatable(final_data(),extensions = c('Buttons','FixedHeader'),class = 'cell-border stripe',rownames = FALSE,
                  filter = 'top', options = list(dom = 'Blfrtip', fixedHeader = TRUE,pageLength = 50,lengthMenu = c(10,50,500,5000, 10000, 25000 ),
                                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                 columnDefs = list(list(className = 'dt-right',
                                                                        targets = 0:2)))) %>%
      formatCurrency(which(colnames(final_data())=="Megítélt összeg (millió Ft)"), '')
  )  
  
 
  my_p_plotly<- reactive({
    by_plot <- g_by_plot()
    plot_adat <- adat
    m <- list(
      l = 100,
      r = 100,
      b = 200,
      t = 150,
      pad = 4
    )
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title =by_plot,
      titlefont = f,
      categoryarray = ~`Összeg (millió Ft)`, 
      categoryorder = "array"
    )
    y <- list(
      title = "Milliárd Ft",
      titlefont = f
    )
    adat_to_plotly <- plot_adat[, list('Összeg (millió Ft)'= sum(osszeg)/1000,'Nyertes pályázatok száma'=.N), by=by_plot]
    setorder(adat_to_plotly, -`Összeg (millió Ft)`)
    p <- plot_ly(adat_to_plotly, x =~get(by_plot), y = ~`Összeg (millió Ft)`, type = 'bar')%>%
      layout(autosize = F, width = 1000, height = 800, margin = m, yaxis = y, xaxis = x )
    return(p)
  })
  
  output$summary_plot <- renderPlotly({
    my_p_plotly()
  })
  
  output$downloadData <- downloadHandler(
    
    filename = 'szechenyi2020data.csv' , content = function(file) {
      
      write.csv(osszes_nyertes(), file,  row.names = FALSE,  fileEncoding = "UTF-8")
    }
  )
  ###############################################################################################
  #######                                  Map                                            #######
  ###############################################################################################

  my_reactive_map <- reactive({
    if(input$map_valaszto=='REGIO'){
      return(get_regio(regio_map = regio_map))
    }
    if(input$map_valaszto=='MEGYE'){
      return(get_megye(megye_map = megye_map))
    }    
    if(input$map_valaszto=='KISTERSEG'){
      return(get_kisterseg(kisterseg_map = kisterseg_map))
    }
    if(input$map_valaszto=='varos'){
      return(get_helyseg(helyseg_map=helyseg_map))
    }
    
  })
  
  output$mymap <- renderLeaflet({
    my_reactive_map()
  })
  
  ###############################################################################################
  #######                                  Mouse                                            #######

  my_mouse_on <- reactive({
    
    if(input$map_valaszto=='REGIO'){
      return(paste( input$mymap_shape_mouseover$id, "Régió", sep = " "))
    }
    if(input$map_valaszto=='MEGYE'){
      return(paste( input$mymap_shape_mouseover$id, "Megye", sep = " "))
    }
    
    if(input$map_valaszto=='KISTERSEG'){
      return(paste( input$mymap_shape_mouseover$id, "Kistérség", sep = " "))
    }
   
    if(input$map_valaszto=='varos'){
      return( input$mymap_shape_mouseover$id)
    }
  })
  
  output$ezenvagyok <- renderText(my_mouse_on())
  
  ###############################################################################################
  #######                                  Mouse   cliked                                         #######
  
  my_mouse_cliked <- reactive({
    
    if(input$map_valaszto=='REGIO'){
      return(paste( input$mymap_shape_click$id, "Régió", sep = " "))
    }
    if(input$map_valaszto=='MEGYE'){
      return(paste( input$mymap_shape_click$id, "Megye", sep = " "))
    }
    if(input$map_valaszto=='KISTERSEG'){
      return(paste( input$mymap_shape_click$id, "Kistérség", sep = " "))
    }
    if(input$map_valaszto=='varos'){
      return(input$mymap_shape_click$id)
    }
    
  })
  
  output$eztklikkelted <- renderText(my_mouse_cliked())
  
  
  ###############################################################################################
  #######                                  Money                                            #######

  
  my_money_on <- reactive({
    #kezdeti
    if(is.null(input$mymap_shape_mouseover$id)==T){
      return(" ")
    }
    else{ 
      #váltás
    if(input$map_valaszto=='REGIO'){
     temp_res <- geo_adatok[REGIO==input$mymap_shape_mouseover$id, list('osszeg'= round(sum(osszeg, na.rm = T)/1000,0),'number'=.N), by=REGIO]
     if (is.na(temp_res$osszeg[1])==T){
       return("0 nyertes pályázat ")
     }
     else{
     return(paste(temp_res$osszeg[1], " milliárd   ", temp_res$number[1] , " nyertes pályázat", sep = " "))
     }
    }
    if(input$map_valaszto=='MEGYE'){
      temp_res <- geo_adatok[MEGYE==input$mymap_shape_mouseover$id, list('osszeg'= round(sum(osszeg,na.rm = T)/1000,0),'number'=.N), by=REGIO]
      if (is.na(temp_res$osszeg[1])==T){
        return("0 nyertes pályázat  ")
      }
      else{
      return(paste(temp_res$osszeg[1], " milliárd   ", temp_res$number[1] , " nyertes pályázat", sep = " "))
      }
    }
      
      if(input$map_valaszto=='KISTERSEG'){
        temp_res <- geo_adatok[KISTERSEG==input$mymap_shape_mouseover$id, list('osszeg'= round(sum(osszeg,na.rm = T)/1000,0),'number'=.N), by=KISTERSEG]
        if (is.na(temp_res$osszeg[1])==T){
          return("0 nyertes pályázat  ")
        }
        else{
          return(paste(temp_res$osszeg[1], " milliárd   ", temp_res$number[1] , " nyertes pályázat", sep = " "))
        }
      }
      
      
      if(input$map_valaszto=='varos'){
        temp_res <- geo_adatok[varos==input$mymap_shape_mouseover$id, list('osszeg'= round(sum(osszeg,na.rm = T),0),'number'=.N), by=varos]
        if (is.na(temp_res$osszeg[1])==T){
          return("0 nyertes pályázat  ")
        }
        else{
          return(paste(temp_res$osszeg[1], " millió   ", temp_res$number[1] , " nyertes pályázat", sep = " "))
        }
      }
    }#else
  })
  
  output$enyiazanyi <- renderText(my_money_on())
  ###############################################################################################
  #######                                  Plotok                                            #######
  
  cliked <- reactive({
    input$mymap_shape_click$id

  })
 
  ######################
  ###   bal felso  #######
  
  bal_felso_reactive_plot <- reactive({
    x <- list(
      title ='Operatív program'
      
    )
    y <- list(
      title = "Milliárd Ft"
    )
    
    if(input$map_valaszto=='REGIO'){
      adatom <- geo_adatok[REGIO ==cliked(),list('osszeg'=sum(osszeg, na.rm = T)/1000,'nyertes_palyazat'=.N), by=c('REGIO','operativ_program' )]
      p <- plot_ly(adatom, x =~operativ_program, y = ~osszeg, type = 'bar')%>%
        layout( yaxis = y, xaxis = x  )
      return(p)
    }
    if(input$map_valaszto=='MEGYE'){
      adatom <- geo_adatok[MEGYE ==cliked(),list('osszeg'=sum(osszeg, na.rm = T)/1000,'nyertes_palyazat'=.N), by=c('MEGYE','operativ_program' )]
      p <- plot_ly(adatom, x =~operativ_program, y = ~osszeg, type = 'bar')%>%
        layout( yaxis = y, xaxis = x  )
      return(p)
    }

  })
  
  output$bal_felso_plot <- renderPlotly({
    bal_felso_reactive_plot()
  })

  ######################
  ###   jobb felso  #######
  jobb_felso_reactive_plot <- reactive({
    x <- list(
      title ='Év'
      
    )
    y <- list(
      title = "Milliárd Ft"
    )
    if(input$map_valaszto=='REGIO'){
      adatom <- geo_adatok[REGIO ==cliked(),list('osszeg'=sum(osszeg, na.rm = T)/1000,'nyertes_palyazat'=.N), by=c('REGIO','ev' )]
      p <- plot_ly(adatom, x =~ev, y = ~osszeg, type = 'bar')%>%
        layout( yaxis = y, xaxis = x  )
      return(p)
    }
    if(input$map_valaszto=='MEGYE'){
      adatom <- geo_adatok[MEGYE ==cliked(),list('osszeg'=sum(osszeg, na.rm = T)/1000,'nyertes_palyazat'=.N), by=c('MEGYE','ev' )]
      p <- plot_ly(adatom, x =~ev, y = ~osszeg, type = 'bar')%>%
        layout( yaxis = y, xaxis = x  )
      return(p)
    }
    
  })
  output$jobb_felso_plot <- renderPlotly({
    jobb_felso_reactive_plot()
  })
  
  
  
}
