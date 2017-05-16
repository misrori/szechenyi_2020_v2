
get_regio <-function(regio_map){
pal <- colorNumeric('YlOrRd',regio_map$osszeg)

regio_plot <- 
leaflet(regio_map) %>%
  addPolygons( weight = 1, smoothFactor = 0.5,
               opacity = 1.0, fillOpacity = 0.5, label=~LOCALNAME,
               labelOptions = labelOptions(noHide = T),
               fillColor = pal(regio_map$osszeg),layerId = ~LOCALNAME,
               highlightOptions = highlightOptions(color = "white", weight = 2,
                                                   bringToFront = TRUE))%>%
addLegend(position = "bottomright",pal = pal, values = ~osszeg, title="Megitélt összeg<br>(Millió Ft)")
                                                                                  
return(regio_plot)
}

get_megye <-function(megye_map){

  pal <- colorNumeric('YlOrRd',megye_map$osszeg)

  megye_plot <- 
    leaflet(megye_map) %>%
    addPolygons( weight = 1, smoothFactor = 0.5,
                 opacity = 1.0, fillOpacity = 0.5, label=~MEGY_NEV,
                 labelOptions = labelOptions(noHide = T),
                 fillColor = pal(megye_map$osszeg),layerId = ~MEGY_NEV,
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = TRUE))%>%
    addLegend(position = "bottomright",pal = pal, values = ~osszeg, title="Megitélt összeg<br>(Millió Ft)")
  
  return(megye_plot)
}

get_kisterseg <-function(kisterseg_map){
 
  pal <- colorNumeric('YlOrRd',kisterseg_map$osszeg)
  
  kisterseg_plot <- 
    leaflet(kisterseg_map) %>%
    addPolygons( weight = 1, smoothFactor = 0.5,
                 opacity = 1.0, fillOpacity = 0.5, label=~KIST_NEV,
                 labelOptions = labelOptions(noHide = T),
                 fillColor = pal(kisterseg_map$osszeg),layerId = ~KIST_NEV,
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = TRUE))%>%
    addLegend(position = "bottomright",pal = pal, values = ~osszeg, title="Megitélt összeg<br>(Millió Ft)")
  
  return(kisterseg_plot)
}


get_helyseg <-function(helyseg_map){
  pal <- colorNumeric('YlOrRd',helyseg_map$osszeg)
  
  helyseg_plot <- 
    leaflet(helyseg_map) %>%
    addPolygons( weight = 1, smoothFactor = 0.5,
                 opacity = 1.0, fillOpacity = 0.5, label=~TEL_NEV,
                 labelOptions = labelOptions(noHide = T),
                 fillColor = pal(helyseg_map$osszeg),layerId = ~TEL_NEV,
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = TRUE))%>%
    addLegend(position = "bottomright",pal = pal, values = ~osszeg, title="Megitélt összeg<br>(Millió Ft)")
  
  return(helyseg_plot)
}





get_my_plot <- function(adat, csop, agregalstsag){
  
  adatom <- adat[,list('osszeg'=sum(osszeg, na.rm = T)/1000,'nyertes_palyazat'=.N), by=c(agregalstsag,csop )]
  p <- plot_ly(adatom, x =~get(csop), y = ~osszeg, type = 'bar')
  return(p)
}


