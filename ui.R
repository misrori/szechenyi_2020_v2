library(shiny)
library(plotly)
library(DT)
library(data.table)
library(rgdal)
library(leaflet)
source('load_map.R')
adat <- fread('szechenyi2020_adatok.csv', stringsAsFactors = F)
navbarPage(
           title="Széchenyi 2020",fluid = TRUE,
           tabPanel("Leírás",
                    #h1("Széchenyi 2020 nyertes pályázatok", align = "center"),
                    h2("Magyarország ", round(sum(adat$osszeg)/1000, 2), "milliárd forintot fizetett ki", align="center"),
                    h2( nrow(adat), "nyertes pályázatra" ,align="center"),
                    h2( 'a Széchenyi 2020 program keretében EU-s forrásból!',align="center"),
                    br(),
                    br(),
                    tags$div(
                      h3('Az adatok forrása',align="center"), #tags$a(href="https://www.palyazat.gov.hu/tamogatott_projektkereso", "https://www.palyazat.gov.hu/tamogatott_projektkereso", style="text-align: center;")
                      HTML(' <center> <a target="_blank", href="https://www.palyazat.gov.hu/tamogatott_projektkereso">https://www.palyazat.gov.hu</a> </center>')
                      ),
                    br(),
                    h3('Az adatok utolsó frissítési dátuma', align="center"),
                    h4('2017-04-25', align="center"),
                    br(),
                    tags$div(
                      h3('Az oldalt készítette',align="center"), #tags$a(href="https://www.palyazat.gov.hu/tamogatott_projektkereso", "https://www.palyazat.gov.hu/tamogatott_projektkereso", style="text-align: center;")
                      HTML(' <center> <a target="_blank", href="http://www.orsosmihaly.com">Orsós Mihály</a> </center>')
                    )
           ),
           tabPanel("Nyertes pályázatok",
                    dataTableOutput("table")
           ),
           tabPanel("Elemzés",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("group_by", label = "Összegzés", choices = c("","Nyertes"="nyertes", "Város"= "varos", "Forrás"= "forras", "Operatív program" = "operativ_program", "Program"= "program","Év" = "ev",
                                                                                 "Jogállás" ="Jogallas", "Megye"= "Megye", "Kistérség"="Kisterseg", 
                                                                                 "Hátrányos besorolás"= "tipus", 'Roma önkormányzat'='roma_onkormanyzat'), selected = ""), 
                        selectInput("group_by2", label = "További összegzés", choices = c("","Nyertes"="nyertes", "Város"= "varos", "Forrás"= "forras", "Operatív program" = "operativ_program", "Program"= "program","Év" = "ev",
                                                                                 "Jogállás" ="Jogallas", "Megye"= "Megye", "Kistérség"="Kisterseg", 
                                                                                 "Hátrányos besorolás"= "tipus", 'Roma önkormányzat'='roma_onkormanyzat'), selected = ""), 
                        selectInput("group_by3", label = "További összegzés", choices = c("","Nyertes"="nyertes", "Város"= "varos", "Forrás"= "forras", "Operatív program" = "operativ_program", "Program"= "program","Év" = "ev",
                                                                                 "Jogállás" ="Jogallas", "Megye"= "Megye", "Kistérség"="Kisterseg", 
                                                                                  'Roma önkormányzat'='roma_onkormanyzat',"Hátrányos besorolás"= "tipus"), selected = ""),
                        downloadButton("downloadData","Összes adat letöltés")
                      ),
                      mainPanel(
                        dataTableOutput("eredmeny")
                      )
                    )
           ),
           tabPanel("Grafikonok",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("plot_osszegzo", label = "Összegzés", choices = c("", "Forrás"= "forras", "Operatív program" = "operativ_program", "Program"= "program","Év" = "ev",
                                                                                 "Jogállás" ="Jogallas", "Megye"= "Megye", "Kistérség"="Kisterseg", 
                                                                                 "Hátrányos besorolás"= "tipus", 'Roma önkormányzat'='roma_onkormanyzat'), selected = "operativ_program")
                        
                      ),
                      mainPanel(
                        plotlyOutput('summary_plot')
                      )
                      
                    )
                    
           ),
           tabPanel("Térkép", fluidPage(

                    div(h3(textOutput('ezenvagyok'), align = "center")),
                    div(h4(textOutput('enyiazanyi'), align = "center")),
                    leafletOutput("mymap"),
                    absolutePanel(top = 200, right = 0,
                                  radioButtons("map_valaszto", "Vállasz",
                                               c("Régió" = "REGIO",
                                                 "Megye" = "MEGYE",
                                                 "Kistérség" = "KISTERSEG",
                                                 "Helység" = "varos"), selected = "REGIO")
                                  )
                    
                 
                    ),
                    div(h4(textOutput('eztklikkelted'), align = "center")),
                    fluidRow(
                      column(6, plotlyOutput('bal_felso_plot', height = 220),plotlyOutput('ball_also_plot', height = 220)),
                      column(6, plotlyOutput('jobb_felso_plot', height = 220),plotlyOutput('jobb_also_plot', height = 220))
                    )
           
          ),
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
           )# http://bootswatch.com/#Grafikon_tab
           )#nav
