
get_regio <-function(regio_map){
pal <- colorNumeric(c("blue", "red"), 1:max(regio_map$osszeg, na.rm = T))

regio_plot <- 
leaflet(regio_map) %>%
  addPolygons( weight = 1, smoothFactor = 0.5,
               opacity = 1.0, fillOpacity = 0.5, label=~LOCALNAME,
               labelOptions = labelOptions(noHide = T),
               fillColor = pal(regio_map$osszeg),layerId = ~LOCALNAME,
               highlightOptions = highlightOptions(color = "white", weight = 2,
                                                   bringToFront = TRUE))
return(regio_plot)
}

get_megye <-function(megye_map){
  pal <- colorNumeric(c("blue", "red"), 1:max(megye_map$osszeg, na.rm = T))
  

  megye_plot <- 
    leaflet(megye_map) %>%
    addPolygons( weight = 1, smoothFactor = 0.5,
                 opacity = 1.0, fillOpacity = 0.5, label=~MEGY_NEV,
                 labelOptions = labelOptions(noHide = T),
                 fillColor = pal(megye_map$osszeg),layerId = ~MEGY_NEV,
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = TRUE))
  return(megye_plot)
}

get_kisterseg <-function(kisterseg_map){
  pal <- colorNumeric(c("blue", "red"), 1:max(kisterseg_map$osszeg, na.rm = T))

  kisterseg_plot <- 
    leaflet(kisterseg_map) %>%
    addPolygons( weight = 1, smoothFactor = 0.5,
                 opacity = 1.0, fillOpacity = 0.5, label=~KIST_NEV,
                 labelOptions = labelOptions(noHide = T),
                 fillColor = pal(kisterseg_map$osszeg),layerId = ~KIST_NEV,
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = TRUE))
  return(kisterseg_plot)
}


get_helyseg <-function(helyseg_map){
  pal <- colorNumeric(c("blue", "red"), 1:max(helyseg_map$osszeg, na.rm = T))
  
  helyseg_plot <- 
    leaflet(helyseg_map) %>%
    addPolygons( weight = 1, smoothFactor = 0.5,
                 opacity = 1.0, fillOpacity = 0.5, label=~TEL_NEV,
                 labelOptions = labelOptions(noHide = T),
                 fillColor = pal(helyseg_map$osszeg),layerId = ~TEL_NEV,
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = TRUE))
  return(helyseg_plot)
}





get_my_plot <- function(adat, csop, agregalstsag){
  
  adatom <- adat[,list('osszeg'=sum(osszeg, na.rm = T)/1000,'nyertes_palyazat'=.N), by=c(agregalstsag,csop )]
  p <- plot_ly(adatom, x =~get(csop), y = ~osszeg, type = 'bar')
  return(p)
}


