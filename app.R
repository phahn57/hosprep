

library(shiny)
library(shinydashboard)
library(tidyverse)
library(reshape2)
library(lubridate)
library(knitr)
library(plotly)
library(ggpubr)
library(grid)
library(gridExtra)
library(kableExtra)
library(leaflet)
library(sf)
library(geojsonio)
library(geojsonsf)


## function in extra source

source("function.R", local=TRUE)

### Load preprocessed data
load("y_prev.RData")
load("y_akt.RData")

### join fall for long time evaluation

y_long <- y_prev$fall %>% filter(jahr==2018)
y_long <- rbind(y_long,y_akt$fall)

### Sequenz for x- axis
brk <- seq.int(1,12,1)
## Color Palette for  Ar-DB
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#0033FF")

## bins for map
bins =  c(1,2,4,8,16,32,64,128,256)



# Define UI ----
ui <- dashboardPage(

# App title ----
        dashboardHeader(title= paste("Reporting: 2020/2019")),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Departement", tabName = "report", icon = icon("dashboard")),
                        menuItem("Hospital", tabName = "klinik", icon = icon("hospital-o")),
                        menuItem("Referring-docs", tabName="zuweiser", icon=icon("user-md")),
                        menuItem("Referred-cases", tabName="zfaelle", icon=icon("user-md")),
                        menuItem("Endo-Ortho",tabName = "end_o", icon=icon("medkit")),
                        menuItem("Operations",tabName = "chir", icon=icon("user-o")),
                        menuItem("Duration",tabName="snzeit",icon=icon("hand-scissors-o")),
                        menuItem("Preparations",tabName="vorb",icon=icon("hourglass")),
                        menuItem("map",tabName = "kart",icon=icon("map-o")),
                        radioButtons("yearset", "Choose year",
                                     c( "actual" = "akt", "previous" = "prev")
                        )
                )
        ),

        dashboardBody(
                tabItems(
                        tabItem(tabName="report",
                fluidRow(
                        box(width=4,plotOutput("pstat")),
                        box(width=4,plotOutput("eff")),
                        box(width=4,plotOutput("verw"))),

                fluidRow(
                        column(width=4,
                        box(width=NULL,plotOutput("aop"))
                        ),
                        column(width=4,
                        box(width=NULL,plotOutput("amb"))
                        ),
                        column(width=4,
                        box(title="Stationär",width=NULL,tableOutput("tab")),
                        box(title="AOP",width=NULL,tableOutput("taop"))
                                )),
                fluidRow(
                        column(width=8,
                        box(width=NULL,
                                radioButtons("inVar",
                                             label="Department",
                                    choices = c("ORTH", "H_CHI", "S_CHI","F_CHI","WS","Kinder","K_CHI"),inline=TRUE)
                        )),
                        column(width=4,
                               box(title="Ambulant",width=NULL,tableOutput("tamb")))

                )),

                tabItem(tabName="zuweiser",
                        fluidRow(
                        box(plotOutput("zuw"),width=12, height = "auto")
                        ),
                        fluidRow(
                                column(width=8,
                                box(width=NULL,
                                        radioButtons("kv",
                                                    label = "KV-Ermächtigung",
                                                    choices = c("kvbest","kvmart","kvbern","kvhch","kvsech", "kvfuss","kvws","kvkind","kvouch","all"), inline=TRUE),
                                    box(title="Rank", width=NULL,
                                    sliderInput("rg","Rang",min=1,max=70,value=c(1,35),dragRange = FALSE)))),
                                column(width=4,
                                       box(title="Explanation",width=NULL,
                                           h4("Referring doctors are sorted by ascending mean of the last 12 month")))
                )),

                tabItem(tabName="zfaelle",
                        fluidRow(
                        box(plotOutput("zuwfaelle"),width=12, height = "auto")
                        ),
                        fluidRow(
                                column(width=8,
                                       box(width=NULL,
                                           radioButtons("inVar5",
                                                        label="Department",
                                                        choices = c("ORTH", "H_CHI", "S_CHI","F_CHI","WS","Kinder","K_CHI"),inline=TRUE),
                                           box(title="Rank", width=NULL,
                                               sliderInput("rg1","Rang",min=1,max=70,value=c(1,35),dragRange = FALSE)))),
                                column(width=4,
                                       box(title="Explanation",width=NULL,
                                           h4("Referred patients resulting in hospital treatment or outpatient surgery.",
                                                tags$br(),
                                                "Referring doctors are sorted by ascending mean of the last 12 month")))
                                           )),

                ### Tab für Klinik
                tabItem(tabName="Hospital",
                        fluidRow(
                                box(width=3,plotOutput("kfall")),
                                box(width=3,plotOutput("keff")),
                                box(width=6,plotOutput("kverw"))),

                        fluidRow(
                                box(width = 3,plotOutput("kaop")),
                                box(width = 3,plotOutput("kamb")),
                                box(title=" stat.Leistung", width = 6,tableOutput("koff")))),
                tabItem(tabName = "end_o",
                        h4("Knee and hip endoprotheses"),
                        fluidRow(
                                box(width=4,plotOutput("endo_hp")),
                                box(width=4,plotOutput("endo_kp")),
                                box(width=4,tableOutput("drg_47"))),
                        fluidRow(
                                box(width=4,plotOutput("endo_hs")),
                                box(width=4,plotOutput("endo_ks")))),
                tabItem(tabName = "chir",
                        h4("Operations by surgeon"),
                        fluidRow(
                                box(width=6,tableOutput("chir_name")),
                                box(width=6, plotOutput("operat"))),
                        fluidRow(
                                box(width=6,
                                    radioButtons("inVar2",
                                                label = "Departement:",
                                                choices = c("ORTH", "H_CHI", "S_CHI","F_CHI","Kinder","K_CHI"), inline=TRUE)))
                        ),

                tabItem(tabName = "snzeit",
                        h4("Cut-Suture time (top)"),
                        fluidRow(
                                box(width=12,plotOutput("snz"))),
                        fluidRow(
                                box(width=6,
                                    radioButtons("inVar3",
                                                label = "Department:",
                                                choices = c("ORTH", "H_CHI", "S_CHI","F_CHI","Kinder","K_CHI"),inline=TRUE)))
                ),

                tabItem(tabName = "vorb",
                        h4("Preparation time Top- OP"),
                        fluidRow(
                                box(width=12,plotOutput("vorb"))),
                        fluidRow(
                                column(width=6,
                                box(width=NULL,
                                    radioButtons("inVar4",
                                                 label = "Department:",
                                                 choices = c("ORTH", "H_CHI", "S_CHI","F_CHI","Kinder","K_CHI"),inline=TRUE))),
                              column(width=4,
                              box(title="Explanation",width=NULL,
                                   h4("Time difference from OP- preparation until cut")))
                )),

                tabItem(tabName = "kart",
                         h4("Regional patient map of in- and outpatient surgery"),
                        fluidRow(
                        column(width=10,
                          leafletOutput("mapping",height=1000)
                        ),
                        column(width=2,
                               radioButtons("inVar7",
                                 label = "Department:",
                                 choices = c("ORTH", "H_CHI", "S_CHI","F_CHI","Kinder","K_CHI","WS"),inline=FALSE))
                               )
                        )

                ) ## closes tabItems
                ))

# Define server logic  ----
server <- function(input, output) {
        ## Wechsel zwischen den Jahren
        y_data <- reactive({
                if (input$yearset == "akt")
                        y_akt
                else if (input$yearset == "prev")
                        y_prev
        })


        selectedAbt <- reactive({
                abteil(y_data()$fall,input$inVar)
        })

        selectedkv <- reactive({
                ermaech(y_data()$fall,input$kv)
        })

        selectedkv1 <- reactive({
                ermaech(y_long,input$kv1)  ##y_data()$fall
        })

        selectedmap <- reactive({
                 y_data()$map %>% select(note,plz,geometry,input$inVar7) %>% rename(number=input$inVar7)
        })

        ## observer for map

        observe({
          selectedmap()
          pop_up <- paste0("Ort:",selectedmap()$note,"<br>Anzahl:",selectedmap()$number )
          pal <- colorBin("plasma", domain=selectedmap()$number, bins = bins)
          leafletProxy("mapping", data= selectedmap())  %>%
            clearShapes() %>%
            clearControls() %>%
            addPolygons(
            fillColor = ~pal(number),
            weight = 2,
            opacity= 1,
            color="white",
            dashArray = "1",
            fillOpacity = 0.4,popup=pop_up)  %>% addLegend(pal = pal,values=~density, opacity=0.9, title=NULL)
        })

#define dataframe "stationär"
       df_stat <- reactive({
                selectedAbt() %>% filter(form == "voll")
        })

       df_aop <- reactive({
               selectedAbt() %>% filter(form=="ambulantes operieren")
       })

       df_amb <- reactive({
               selectedAbt() %>% filter(form=="ambulant")
        })


# Output Reporting----
       output$pstat <- renderPlot({
               plotter_abt(df_stat(), mon, faelle, "Cases")
        })

        output$eff <- renderPlot({
                plotter_abt(df_stat(),mon, drg, "eff_weigth")
        })

        output$verw <- renderPlot({
                plotter_abt(df_stat(),mon, verweil, "inhouse_days")
        })

        output$aop <- renderPlot({
                plotter_abt(df_aop(),mon,faelle,"outpatient operations")
        })


        output$amb <- renderPlot({
                plotter_abt(df_amb(),mon,faelle,"Ambulant Patients")
        })


        output$tab <- renderTable({
                x <- df_stat() %>% filter(mon <= y_data()$bmon) %>% group_by(jahr) %>% summarise(effgewicht = sum(drg), faelle = sum(faelle), verweil = sum(verweil), S_Naht = sum(sn))
                x <- melt(x, id.vars = c("jahr")) %>% dcast(variable ~ jahr)
                colnames(x) <- y_data()$col_tabs
                x <- x %>% mutate(diff = get(y_data()$coln_akt)-get(y_data()$coln_vor), proz_diff = 100 * diff / get(y_data()$coln_vor))
                x
        })

        output$taop <- renderTable({
                x <- df_aop() %>% filter(mon <= y_data()$bmon) %>% group_by(jahr) %>% summarise(faelle = sum(faelle), S_Naht = sum(sn))
                if(length(unique(x$jahr))==1){
                        var=unique(x$jahr)
                        x <- melt(x, id.vars = c("jahr")) %>% dcast(variable ~ jahr)
                        colnames(x) <- c("variable",var)
                } else
                {
                        x <- melt(x, id.vars = c("jahr")) %>% dcast(variable ~ jahr)
                        colnames(x) <- y_data()$col_tabs
                        x <- x %>% mutate(diff = get(y_data()$coln_akt)-get(y_data()$coln_vor), proz_diff = 100 * diff / get(y_data()$coln_vor))
                }
                x
        })

        output$tamb <- renderTable({
                x <- df_amb() %>% filter(mon <= y_data()$bmon)  %>% group_by(jahr) %>% summarise(faelle = sum(faelle))
                x <- melt(x, id.vars = c("jahr")) %>% dcast(variable ~ jahr)
                colnames(x) <- y_data()$col_tabs
                x <- x %>% mutate(diff = get(y_data()$coln_akt)-get(y_data()$coln_vor), proz_diff = 100 * diff / get(y_data()$coln_vor))
                x
        })

# Tab Klinik----
        output$kfall <-  renderPlot({
                plotter(y_data()$df.stat,abt,faelle,"Department","Cases",y_data()$akt,y_data()$vor)
        })

        output$keff <- renderPlot({
                plotter(y_data()$df.stat,abt,drg,"Departement","Eff_weigth",y_data()$akt,y_data()$vor)
        })

        output$kverw <- renderPlot({
                plotter(y_data()$df.stat,abt,verweil,"Department","inhouse days",y_data()$akt,y_data()$vor)
        })

        output$kaop <- renderPlot({
                y_data()$df.klinik %>% filter(form=="ambulantes operieren") %>% plotter(abt,faelle,"Department","AOP",y_data()$akt,y_data()$vor)
        })

        output$kamb <- renderPlot({
                y_data()$df.klinik %>% filter(form=="ambulant") %>% plotter(abt,faelle,"Department","Ambulant Pat",y_data()$akt,y_data()$vor)
        })

        output$koff <- renderTable(y_data()$off,spacing = "xs",striped = TRUE, width="200")

### Tab Zuweiser----
        output$zuw <- renderPlot({
        plotter_zuweiser(selectedkv(),input$rg)
})

### Tab Zuweiser Fälle
        output$zuwfaelle <- renderPlot({
        plotter_faelle(y_data()$fall,input$inVar5,input$rg1)
        })

### Tab Zuweiser Praxis
        output$prax <- renderPlot({
                plotter_praxis(selectedkv1(),y_data()$bmon,praxis)
        })

### Tab Endo Orthopädie----
        output$endo_hp <- renderPlot({
                plotter_ops(y_data()$fall,"ORTH","5-820",y_data()$bmon)
        })
        output$endo_kp <- renderPlot({
                plotter_ops(y_data()$fall,"ORTH","5-822",y_data()$bmon)
        })
        output$endo_hs <- renderPlot({
                plotter_ops(y_data()$fall,"ORTH","5-821",y_data()$bmon)
        })
        output$endo_ks <- renderPlot({
                plotter_ops(y_data()$fall,"ORTH","5-823",y_data()$bmon)
        })

        output$drg_47 <- renderTable({
          drg <- y_data()$fall %>% filter(abt=="ORTH")  %>% filter(jahr==2019) %>% filter(kat=="stat") %>%
            filter(drg=="I47C") %>%  group_by(drg,mon_auf) %>% tally()
          drg <- drg %>% mutate(soll=72) %>% mutate(diff=n-soll)
          drg <- rbind(drg,colSums(drg[,3:5]))
          drg[nrow(drg),1:2] <- c("Summe"," ")
          drg
        })

### Tab Operateur
        output$chir_name <- renderTable({
                y_data()$fall %>% filter(jahr == y_data()$akt_jahr & op == "J") %>% filter(abt == input$inVar2) %>% group_by(arzt1) %>%
                        summarise(Anzahl = n(), Schnitt_Naht = sum(snzeit, na.rm = TRUE))
        })

        output$operat <- renderPlot({
          sel_abt <- y_data()$op_tab %>% filter(abt==input$inVar2)
          plotter_abt(sel_abt,mon,n,"Operations")
        })

### Tab Schnitt-Naht Zeit
        output$snz <- renderPlot({
                plot_sn(y_data()$fall,input$inVar3)
        })


### Tab Vorbereitung Zeit
        output$vorb <- renderPlot({
                plot_vor(y_data()$fall,input$inVar4)
        })

### Tab DB3
        output$pdb3 <- renderPlot({
                plotter_db3(y_data()$solist,input$inVar6,y_data()$sjahr)
        })

        output$tabdb3 <- renderTable({
                table_db3(y_data()$solist,input$inVar6)
        })

### Tab karte

        output$mapping <- renderLeaflet({leaflet() %>% setView(9.0807,49.2244, zoom=10) %>%
          addTiles()
        })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)