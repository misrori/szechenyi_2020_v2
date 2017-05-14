library(shiny)
library(DT)
library(plotly)
library(data.table)

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
  
  osszes_nyertes <- reactive({
   adatom <- adat
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
      return(osszeitendo_adat[, list('Összeg (millió Ft)'= round(sum(osszeg),2),'Nyertes pályázatok száma'=.N), by=by1])
    }
    else if(by1!=''& by2!='' & by3==''){
      return(adat[,list('Összeg (millió Ft)'= round(sum(osszeg),2),'Nyertes pályázatok száma'=.N), by=c(by1, by2)])
    }
    else if(by1!=''& by2!='' & by3!=''){
      return(adat[,list('Összeg (millió Ft)'= round(sum(osszeg),2),'Nyertes pályázatok száma'=.N), by=c(by1, by2, by3)])
    }
  })
  
  output$eredmeny <- DT::renderDataTable(
    DT::datatable(final_data(),extensions = c('Buttons','FixedHeader'),class = 'cell-border stripe',rownames = FALSE,
                  filter = 'top', options = list(dom = 'Blfrtip', fixedHeader = TRUE,pageLength = 50,lengthMenu = c(10,50,500,5000, 10000, 25000 ),
                                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                 columnDefs = list(list(className = 'dt-right',
                                                                        targets = 0:2)))) %>%
      formatCurrency(c(2:8), '')
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
}
