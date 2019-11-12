if (!require("pacman")) install.packages("pacman")
pacman::p_load("shiny",
               "shinydashboard",
               "shinyjs",
               "tidyverse",
               "lubridate",
               "purrr",
               "parallel",
               "xtable",
               "grid",
               "gridExtra",
               "scales",
               "ggthemes",
               "RColorBrewer",
               "ggiraph",
               "xts",
               "dygraphs",
               "DT",
               "openxlsx")


if(!require("accelerator")){
  pacman::p_load("devtools")
  install_github("fjbaron/accelerator")
}
library("accelerator")



###############################################################################################################################
#### The configuration of the project is done in the next file
###############################################################################################################################
source("parameters.R")



###############################################################
# We can predefine some colors for the every WHAT or else
# some random color will be chosen for you
###############################################################

what2Color=list(
  nonWear=list(order=0,   color="#888888"),
  bed=list(order=100,     color="#2171B5"),
  SIB=list(order=120,     color="#6BAED6"),
  INAC_20m=list(order=200,  color="#00FF77"),
  LIG_10m=list(order=250,  color="#00FF00"),
  MVPA_05m=list(order=300, color="#FF7777"),
  MVPA_10m=list(order=340,color="#FF5555"),
  VIG_01m=list(order=380,  color="#FF2222"),
  isOn=list(order=10,   color="#FAFAFA")
  )


# The WHAT with colors not predefined, will receive some other colour

whatColorUndefined=setdiff(names(defineWhat), names(what2Color))
if(length(whatColorUndefined)>0){

  what2Color=what2Color %>% append(
    list(order=what2Color %>% map_dbl(~.[["order"]]) %>% max(na.rm=T)+20*seq_along(whatColorUndefined),
         color=terrain.colors(length(whatColorUndefined))) %>% transpose() %>% set_names(whatColorUndefined)
  )  
}



epochColumnNames=c("enmo","anglez","anglex","angley")
epochColumnColor=c(enmo="black",anglez="lightgray")

epochColumnUndefined=setdiff(epochColumnNames,names(epochColumnColor))

if(length(epochColumnUndefined)>0){
  
  epochColumnColor=epochColumnColor %>% 
    c(terrain.colors(length(epochColumnUndefined)) %>% set_names(epochColumnUndefined))  
}




formatoTiempo="%Y-%m-%d %H:%M:%S"
timezone="Europe/Madrid"








################################################################################
# Probably you do not need to touch below here
#
#  ||
#  ||
#  \/
################################################################################


###Let us find the epochs available




if(file.exists("preProcessed.RData")){
  load("preProcessed.RData")
}

if(!exists("dfEpoch")) {
 # dfEpoch=getEpochGGIR(baseEpoch)
  dfEpoch = getInfoOfFiles(str_c(baseEpoch,"/meta/basic"),prefix="^meta_",suffix="\\.bin") %>%
    mutate (base=baseEpoch) %>% 
    select(base, RAW,path,size)
  pb=progress_estimated(nrow(dfEpoch), min_time = 0)
  dfEpoch=dfEpoch %>% mutate(meta=map(path, ~ infoGGIRmeta(.x,pb))) %>% unnest(cols = c(meta))
  
  preProcessed=NULL
}

###Initial values for "When"
#
if(!is.null(preProcessed$when)){
  vectorOfWhen=preProcessed$when %>% map(names) %>% unlist() %>% unique()
} else vectorOfWhen = c("daily")

if( "daily" %in% vectorOfWhen) defaultWhen="daily" else defaultWhen=vectorOfWhen[[1]]


     














arreglaDateTime<-function(texto,tz, alternativo){
  resultado=ymd_hms(texto,tz=timezone)
  if (is.na(resultado)) resultado=ymd_hm(texto,tz=timezone)
  if (is.na(resultado)) resultado=ymd_h(texto,tz=timezone)
  if (is.na(resultado)) resultado=ymd_hms(str_c(texto, "00:00:00"),tz=timezone)
  if(is.na(resultado)) resultado=alternativo
  resultado
}













  ui <-  dashboardPage(
    dashboardHeader(title = "DataHunter.es GGIR visualizer",titleWidth = 350),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Selection", tabName = "tabSelect", icon = icon("folder")),
        menuItem("Visualization", tabName = "tabVisualization", icon = icon("eye")),
        menuItem("Time series", tabName = "tabTimeSeries", icon = icon("chart-line")),
        menuItem("Results (data)", tabName = "tabResult", icon = icon("database")),
        menuItem("Results (chart)", tabName = "tabChart", icon = icon("chart-pie"))
      ),
      radioButtons("whenChosen", label="WHEN", choices = vectorOfWhen, selected = defaultWhen),
      checkboxInput("justValids",label=("Just valids"),value = FALSE),
      checkboxGroupInput("Variables", "WHAT",
                         names(what2Color),
                         selected = names(what2Color)),
      actionButton("preprocess", "Pre-process",icon = icon("calculator")),
      verbatimTextOutput("debug")
    ),
    dashboardBody(
      tabItems(
        tabItem("tabSelect",
                actionButton("selectAll", "Select all",icon = icon("hand-rock")),
                actionButton("selectNone", "Select none",icon = icon("hand-paper")),
                DT::dataTableOutput("tablaEpoch")),
        tabItem("tabVisualization",      
                div(style="width:1200px;height:600px;",
                ggiraphOutput("graficoIntervalos")),
                checkboxGroupInput("showIntervals", "Show intervals",
                                   c("When","What"),
                                   selected = "",inline = TRUE),
        DT::dataTableOutput("tablaWhen"),
        DT::dataTableOutput("tablaWhat")),
        tabItem("tabTimeSeries",
                dygraphOutput("graficoTemporal")),
        tabItem("tabResult",
                radioButtons("tableFormat","Format",c("Long","Wide","Long summary","Wide summary"),selected = "Long",inline = TRUE),
                downloadLink("downloadTable", "Download"),
                DT::dataTableOutput("tablaResult")),
        tabItem("tabChart", 
                radioButtons("summaryChosen", label="Summary", choices = c("duration"="duration","start"="hfrom","end"="hto"), selected = "duration",inline = TRUE),
                div(style="width:1200px;height:1200px;",
                    plotOutput("plotHistograms"))
      ))
  )
  )
  




server <- function(input, output, session) {
  


  observeEvent(input$preprocess, {
    preProcessed=dfEpoch %>%
      mutate( what  = ggir2What(.)  ) %>%
      mutate( when  =  getWhen(.)   ) %>% 
      mutate(summaryByWhen = getSummaryWhatByWhen(what,when) ) %>%
      mutate(invalidWhen=getInvalidWhen(summaryByWhen)) %>%
      mutate(validWhen=getValidWhen(when,invalidWhen))
      
    save(preProcessed,dfEpoch,file="preProcessed.RData")
    })
  
  observeEvent(input$selectNone, {
    selectRows(dataTableProxy("tablaEpoch"), selected=NULL)
      })
  
  observeEvent(input$selectAll, {
    selectRows(dataTableProxy("tablaEpoch"), selected=1:nrow(dfEpoch))
  })
  
  
  epochSelected<-reactive({
    x1=input$tablaEpoch_rows_selected
    req(!is.null(x1))
    x1
  })
  
  
  whatAndWhen=reactive({
    if(!is.null(preProcessed)){
      result=preProcessed %>% slice(epochSelected())
      if(input$justValids) result=result %>% select(-when) %>% rename(when=validWhen)
    } else {
  result=dfEpoch %>% slice(epochSelected()) %>%
        mutate( what  = ggir2What(.)  ) %>%
        mutate( when  =  getWhen(.)   ) 
    }
    
    result %>% mutate(what=map(.[["what"]],~.x[names(.x) %in% input$Variables]))
  })
  
   summaryLong=reactive({
     if(!is.null(preProcessed)){
       result=preProcessed %>% slice(epochSelected())
       if(input$justValids) result=result %>% select(-when) %>% rename(when=validWhen)
       result=result %>% select(RAW,what,when) %>% 
         mutate(summaryByWhen=map2(.[["what"]],.[["when"]],summaryWhatByWhen)) %>%
         select(RAW,summaryByWhen) %>% unnest(cols = c(summaryByWhen)) %>% filter(what %in% input$Variables) %>%
         mutate(time=as.duration(duration)/durationToUserUnits(what,when))
     }
   })

 summarySelected=reactive({
   resultado=summaryLong()
   if(input$tableFormat=="Wide") resultado=resultado %>% pivot_wider(id_cols=c(RAW,day,when),names_from=what,values_from=c(time))
   if(input$tableFormat=="Long summary") resultado= resultado %>% select(-day) %>% group_by(RAW,when,what) %>% summarise_all(~ round(mean(.x,na.rm=T),2))
   if(input$tableFormat=="Wide summary") resultado= resultado %>% select(-day) %>% group_by(RAW,when,what) %>% summarise(time=round(mean(time,na.rm=T),2)) %>% pivot_wider(id_cols=c(RAW,when),names_from=what,values_from=c(time))
   resultado
 })

 
 
 
plotListHistograms=reactive({
  df=summaryLong()%>% filter(when==input$whenChosen)
 map(df %>% split(df["what"]),
      function(.df){ggplot(.df, aes(x=time)) +geom_histogram()+
          xlab(.df %>% pluck("what",1))})
  })


plotListDateTimeHistograms=reactive({
  if(input$summaryChosen=="duration") timescale=1
  if(input$summaryChosen=="hfrom") timescale=3600
  if(input$summaryChosen=="hto") timescale=3600

  df=summaryLong()%>% filter(when==input$whenChosen) %>% mutate(hora= today()+dseconds(.[[input$summaryChosen]]*timescale))
  map(df %>% split(df["what"]),
      function(.df){
        elQue=.df$what %>% first()

        miPaleta=what2Color %>% pluck(elQue,"color") 
        names(miPaleta)=elQue
        print(miPaleta)
        ggplot(.df, aes(x=hora,fill=what,color=what)) + geom_histogram(fill=miPaleta)+
          xlab(str_c("Start of ",.df %>% pluck("what",1)))+
          scale_x_datetime(labels=date_format("%H:%M:%S",tz="UTC"))+
#          scale_color_manual(miPaleta)+
          theme_stata() +
          theme(
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x=element_text(size=8),
            panel.grid.major.x=element_line(colour = 'lightblue'),
            axis.title.y=element_blank(),
            legend.position="right",
            legend.title = element_blank(),
            strip.text.y = element_text(size = 7,margin=margin()),
            panel.spacing.y=unit(0.05, "lines")
          )
        }
      )
})



timeSeries=reactive({
  req(whatAndWhen())
  req(nrow(whatAndWhen())==1)
  ggir2ts(baseEpoch,whatAndWhen()$RAW)
})




output$plotHistograms <- renderPlot({
  req(plotListHistograms())
  n <- length(plotListHistograms())
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(plotListDateTimeHistograms(), ncol=nCol))
})



output$graficoIntervalos <- renderggiraph({
  grafico=plotOfIntervals(whatAndWhen(),whatColumn="what",whenColumn="when",whenConcept=input$whenChosen,what2Color,epochColumnColor)
  ggiraph(code = print(grafico), zoom_max = 10, width_svg=16, height_svg=8, selection_type  = "multiple",width=1)
})






 output$graficoTemporal= renderDygraph({
  df=timeSeries()
  req(nrow(df>2))
  enmo=xts(df$ENMO*1000,order.by=df$timestamp)
  anglez=xts(df$anglez+90,order.by=df$timestamp)
  todo=data.frame(enmo,anglez)

  tmin=df$timestamp %>% first()
  tmax=df$timestamp %>% last()
     acumulaColor=c()
  for(v in intersect(input$Variables,whatAndWhen() %>% pluck("what",1) %>% names()) %>% setdiff("isOn")){
        if(nrow(whatAndWhen() %>% pluck("what",1,v))>0){
        valores=interval2criterio(df$timestamp, whatAndWhen() %>% pluck("what",1,v))*(what2Color %>% pluck(v,"order"))
        valores[valores==0]=NaN
         todo[[v]]=xts(valores, order.by=df$timestamp)
         acumulaColor=c(acumulaColor,v)
        }
  }
     
      coloresVariables= input$Variables %>% map_chr( ~ what2Color[[.x]][["color"]]) %>% set_names(input$Variables)
      #colores=c(epochColumnColor[c("enmo","anglez")],coloresVariables)
      colores=c(epochColumnColor[c("enmo")],coloresVariables[acumulaColor],epochColumnColor[c("anglez")])
      print(colores)
      names(colores)=NULL
   #  names(colores)=c("enmo",unique(acumulaColor),"anglez")   
   #  names(colores)=NULL
   #  message("COLORES:",colores)
   #  message(names(todo))


   desde=tmin
   hasta=tmax
   
   grafico=dygraph(todo, main = "ENMO") %>%
          dyRangeSelector(
            dateWindow = c(desde,hasta)) %>%  # 
     dyRoller(rollPeriod = 1) %>%
     dyAxis("y", label = "mili ENMO", valueRange = c(0, 500)) %>%
     dySeries("anglez", axis = "y2") %>%
     dyOptions(colors=colores, drawGrid = FALSE, fillGraph = TRUE)
   grafico
 })
 
 
  

 output$tablaValidos <- DT::renderDataTable({
   tablaValidos=analisisValidez()$valido
   tablaValidos %>% group_by(day,calendario) %>% summarise(Variable=paste(variable,sep=" ",collapse=" "))
 },caption="Variables Válidas")
 

 
 output$tablaEpoch <- DT::renderDataTable({
   dfEpoch  %>% select(RAW,serial,start,end,size,hz)
 },caption="Epochs available")
 

 output$tablaWhen <- DT::renderDataTable({
   req("When" %in% input$showIntervals)
   whatAndWhen() %>% select(RAW,when) %>% mutate(when=map(when,~ .x[[input$whenChosen]])) %>% unnest(cols = c(when))  %>%
     mutate(from=as.character(from),to=as.character(to))
 },caption="When")
 

 output$tablaWhat <- DT::renderDataTable({
   req("What" %in% input$showIntervals)
   whatAndWhen() %>% select(RAW,what) %>%
     mutate(what=map(what,~ { .x[names(.x) %in% input$Variables] %>% bind_rows(.id = "What")})) %>%
     unnest(cols = c(what)) %>%
     mutate(from=as.character(from),to=as.character(to))
 },caption="What")
 
 
 output$tablaResult <- DT::renderDataTable({
    summarySelected()
 },caption="Numerical Result")
 
  
 output$tableWhatAndWhen <- DT::renderDataTable({
   whatAndWhen()
 },caption="Epochs computed")

output$debug <- renderText({
  epochSelected()
})



output$downloadTable <- 
  downloadHandler(
    filename = function() {
      "results.xlsx"
    },
    content = function(file) {

      
      wb <- createWorkbook()
      addWorksheet(wb, "Results")
      writeData(wb, sheet = 1, summarySelected())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

 
}


shinyApp(ui, server)
