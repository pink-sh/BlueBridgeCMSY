#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinysky)
library(plotly)
library(RCurl)
library(shinyjs)
#setwd("/srv/shiny-server/BlueBridgeCmsyWebApp/")
source(paste0(getwd(),"//CmsyFunction.R"))

jscode <- "shinyjs.toTop = function() {document.body.scrollTop = 0;}"

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("CMSY Test page"),
  busyIndicator(wait = 1000),
  tags$head(tags$style(
    type="text/css",
    "#renderCmsyDlmToolsAnalysisChart {max-width: 100%; width: 100%; height: auto !important;} #renderCmsyDlmToolsManagementChart {max-width: 100%; width: 100%; height: auto !important;} "
  )),
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      extendShinyjs(text = jscode),
      textInput("vreUsername", "VRE Username", "enrico.anello"),
      textInput("vreToken", "VRE Token", "5b0f903a-3cb1-4424-a2bd-2700c9f1d4ed"),
      
      checkboxInput("cmsyDlmtools", "Run with CMSY for DLMTools", TRUE),
      checkboxInput("cmsyLegacy", "Run with legacy CMSY", FALSE),
      uiOutput("downloadButton"),
      fileInput("file1", "Choose Stock CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      uiOutput("fill"),
      textInput("region", "Region", "Mediterranean"),
      textInput("subregion", "SubRegion", "Adriatic sea"),
      
      textInput("group", "Group", "Plankton feeders"),
      textInput("name", "Name", "Sand smelt in Adriatic Sea"),
      textInput("englishName", "English Name", "Big scale sand smelt"),
      textInput("scientificName", "Scientific Name", "Atherina boyeri"),
      textInput("source", "Source", "-"),
      
      numericInput("minOfYear", "Min of year", 1970, min = 1900, max = 2020, step=1),
      numericInput("maxOfYear", "Max of year", 2014, min = 1900, max = 2020, step=1),
      numericInput("startYear", "Start year", 1970, min = 1900, max = 2020, step=1),
      numericInput("endYear", "End year", 2014, min = 1900, max = 2020, step=1),
      
      textInput("flim", "F limit", "NA"),
      textInput("fpa", "FPA", "NA"),
      textInput("blim", "B Limit", "NA"),
      textInput("bpa", "BPA", "NA"),
      textInput("bmsy", "B-MSY", "NA"),
      textInput("fmsy", "F-MSY", "NA"),
      textInput("msy", "MSY", "NA"),
      textInput("msyBTrigger", "MSY Trigger", "NA"),
      textInput("b40", "B 40", "NA"),
      textInput("m", "M", "NA"),
      textInput("fofl", "FOFL", "NA"),
      textInput("last_f", "Last F", "NA"),
      textInput("resiliance", "Resiliance", "Medium"),
      textInput("r.low", "R Low", "NA"),
      textInput("r.hi", "R Hi", "NA"),
      
      numericInput("stb.low", "Stb Low", 0.2, min = 0, max = 10, step=0.1),
      numericInput("stb.hi", "Stb Hi", 0.6, min = 0, max = 10, step=0.1),
      
      textInput("int.yr", "Int Yr", "NA"),
      textInput("intb.low", "Intb low", "NA"),
      textInput("intb.hi", "Intb hi", "NA"),
      
      numericInput("endb.low", "Endb Low", 0.01, min = 0, max = 10, step=0.01),
      numericInput("endb.hi", "Endb Hi", 0.4, min = 0, max = 10, step=0.1),
      
      textInput("q.start", "Q Start", "NA"),
      textInput("q.end", "Q End", "NA"),
      textInput("btype", "BType", "None"),
      
      checkboxInput("force.cmsy", "Force cmsy", FALSE),
      
      textInput("comments", "Comments", "landings"),
      
      actionButton("go", "Go")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #htmlOutput("renderInfo"),
      br(),
      htmlOutput("titleDlmTools"),
      br(),
      htmlOutput("renderDlmtoolsLog"),
      br(),
      htmlOutput("renderCmsyDlmToolsInfo"),
      htmlOutput("titleDlmToolsManagementChart"),
      imageOutput("renderCmsyDlmToolsManagementChart"),
      htmlOutput("renderDlmToolsSpace1"),
      htmlOutput("titleDlmToolsAnalisysChart"),
      imageOutput("renderCmsyDlmToolsAnalysisChart"),
      htmlOutput("renderDlmToolsSpace2"),
      htmlOutput("titleLegacy"),
      br(),
      htmlOutput("renderLegacyLog"),
      br(),
      htmlOutput("renderCmsyLegacyInfo"),
      htmlOutput("titleLegacyManagementChart"),
      imageOutput("renderCmsyLegacyManagementChart"),
      htmlOutput("renderLegacySpace"),
      htmlOutput("titleLegacyAnalisysChart"),
      imageOutput("renderCmsyLegacyAnalysisChart")
      
      #plotlyOutput("plotA", width="85%"),br(),
      #plotlyOutput("plotB", width="85%"),br(),
      #plotlyOutput("plotC", width="80%"),br(),
      #plotlyOutput("plotD", width="80%"),br(),
      #plotlyOutput("plotE", width="80%"),br(),
      #plotlyOutput("plotF", width="80%"))
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  cmsy <- reactiveValues()
  output$fill <- renderUI({
    inFile1 <- input$file1
    
    if (is.null(inFile1)) {
      return(NULL)
    }
    a <- read.csv(inFile1$datapath)
    
    selectInput("stock", "Select a stock", sort(unique(a$Stock)))
  })
  
  output$downloadButton <- renderUI({
    if (!is.null(cmsy$dlmTools) || !is.null(cmsy$legacy)) {
      downloadButton("downloadReport", label = "Download Report")
    }
  })
  
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("cmsy_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "cmsyReport.Rmd")
      file.copy("cmsyReport.Rmd", tempReport, overwrite = TRUE)
      
      
      if (!is.null(cmsy$dlmTools$analisysChartUrl)) {
        fileAnalisysChart <- tempfile(fileext=".jpg")
        download.file(cmsy$dlmTools$analisysChartUrl, fileAnalisysChart)
        cmsy$dlmTools$analisysChart <- fileAnalisysChart
      }
      if (!is.null(cmsy$dlmTools$analisysChartUrl)) {
        fileManagementChart <- tempfile(fileext=".jpg")
        download.file(cmsy$dlmTools$managementChartUrl, fileManagementChart)
        cmsy$dlmTools$managementChart <- fileManagementChart
      }
      if (!is.null(cmsy$legacy$analisysChartUrl)) {
        fileAnalisysChart <- tempfile(fileext=".jpg")
        download.file(cmsy$legacy$analisysChartUrl, fileAnalisysChart)
        cmsy$legacy$analisysChart <- fileAnalisysChart
      }
      if (!is.null(cmsy$legacy$analisysChartUrl)) {
        fileManagementChart <- tempfile(fileext=".jpg")
        download.file(cmsy$legacy$managementChartUrl, fileManagementChart)
        cmsy$legacy$managementChart <- fileManagementChart
      }
      
      # Set up parameters to pass to Rmd document
      params <- list(cmsy = cmsy)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  
  output$renderDlmtoolsSpace1 <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        space <- "<br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/>"
        space
      } else { "" }
    } else { "" }
  })
  
  output$renderDlmtoolsSpace2 <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        space <- "<br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/>"
        space
      } else { "" }
    } else { "" }
  })
  
  output$renderLegacySpace <- renderText({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        space <- "<br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/>"
        space
      } else { "" }
    } else { "" }
  })
  
  output$renderDlmtoolsLog <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        log <- paste0("<a href='", cmsy$dlmTools$log, "'>Download the log of the computation</a>")
        log
      } else { "" }
    } else { "" }
  })
  output$renderLegacyLog <- renderText({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        log <- paste0("<a href='", cmsy$legacy$log, "'>Download the log of the computation</a>")
        log
      } else { "" }
    } else { "" }
  })
  
  output$titleDlmTools <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        title <- "<h1> CMSY For DLM Tools - Results</h1>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$titleDlmToolsManagementChart <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        title <- "<h2> Management Charts </h2>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$titleDlmToolsAnalisysChart <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        title <- "<h2> Analisys Charts </h2>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$renderCmsyDlmToolsInfo <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        cmsy$dlmTools$text <- gsub("\n\r", "<br/>", cmsy$dlmTools$text)
        cmsy$dlmTools$text <- gsub("\n", "<br/>", cmsy$dlmTools$text)
        cmsy$dlmTools$text <- gsub("----------------------------------------------------------", "", cmsy$dlmTools$text)
        cmsy$dlmTools$text
      } else {  "" }
    } else {  "" }
  })
  output$renderCmsyDlmToolsManagementChart <- renderImage({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        w1 <- session$clientData$output_renderCmsyDlmToolsManagementChart_width
        h1 <- (w1*3)/4
        list(src = cmsy$dlmTools$managementChart,
             contentType = 'image/jpg',
             width = w1,
             height = h1)
      } else {
        list(src = "NULL")
      }
    } else {
      list(src = "NULL")
    }
  })
  output$renderCmsyDlmToolsAnalysisChart <- renderImage({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        w2 <- session$clientData$output_renderCmsyDlmToolsAnalysisChart_width
        h2 <- (w2*3)/4
        list(src = cmsy$dlmTools$analisysChart,
             contentType = 'image/jpg',
             width = w2,
             height = h2)
      } else {
        list(src = "NULL")
      }
    } else {
      list(src = "NULL")
    }
  })
  
  #--------------------------------------------------------------------------
  
  output$titleLegacy <- renderText({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        title <- "<h1> Legacy CMSY - Results</h1>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$titleLegacyManagementChart <- renderText({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        title <- "<h2> Management Charts </h2>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$titleLegacyAnalisysChart <- renderText({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        title <- "<h2> Analisys Charts </h2>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$renderCmsyLegacyInfo <- renderText({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        cmsy$legacy$text <- gsub("\n\r", "<br/>", cmsy$legacy$text)
        cmsy$legacy$text <- gsub("\n", "<br/>", cmsy$legacy$text)
        cmsy$legacy$text <- gsub("----------------------------------------------------------", "", cmsy$legacy$text)
        cmsy$legacy$text
      } else {  "" }
    } else {  "" }
  })
  output$renderCmsyLegacyManagementChart <- renderImage({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        w3 <- session$clientData$output_renderCmsyLegacyManagementChart_width
        h3 <- (w3*3)/4
        list(src = cmsy$legacy$managementChart,
             contentType = 'image/jpg',
             width = w3,
             height = h3)
      } else {
        list(src = "NULL")
      }
    } else {
      list(src = "NULL")
    }
  })
  output$renderCmsyLegacyAnalysisChart <- renderImage({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        w4 <- session$clientData$output_renderCmsyLegacyManagementChart_width
        h4 <- (w4*3)/4
        list(src = cmsy$legacy$analisysChart,
             contentType = 'image/jpg',
             width = w4,
             height = h4)
      } else {
        list(src = "NULL")
      }
    } else {
      list(src = "NULL")
    }
  })
  
  #--------------------------------------------------------------------------
  
  observeEvent(input$go, {
    infile1 <- input$file1
    
    if (is.null(infile1)) {
      return(NULL)
    }
    
    js$toTop();
    
    inputCsvFile <- infile1$datapath
    
    templateFileDlmTools <- "cmsyForDlmToolsTemplate.xml"
    templateFileLegacy <- "cmsyLegacyTemplate.xml"
    
    cmsy$dlmTools <- list()
    cmsy$legacy <- list()
    if (input$cmsyDlmtools) {
      
      #contents <- getURL("http://localhost/BlueBridgeCMSY/text.txt")
      #contents <- gsub("\n\r", "<br/>", contents)
      #contents <- gsub("\n", "<br/>", contents)
      #contents <- gsub("----------------------------------------------------------", "", contents)
      #print (contents)
      #cmsy$dlmTools$text <- contents
      
      #fileAnalisysChart <- tempfile()
      #fileManagementChart <- tempfile()
      #download.file("http://data.d4science.org/dUJRNUtvS1N3WVpYSW9FY010WkJuelJFQkd3RTVxR0ZHbWJQNStIS0N6Yz0-VLT", fileManagementChart)
      #download.file("http://data.d4science.org/VGRWaHU2c3pSOVpYSW9FY010WkJuM2N6eU5IazZnYmxHbWJQNStIS0N6Yz0-VLT", fileAnalisysChart)
      #cmsy$dlmTools$managementChart <- fileManagementChart
      #cmsy$dlmTools$analisysChart <- fileAnalisysChart
      cat(file=stderr(),inputCsvFile, "\n")
      cat(file=stderr(), templateFileDlmTools, "\n")      
      ret <- runCmsy(input$region,input$subregion,input$stock,input$group,input$name,input$englishName,input$scientificName,input$source,input$minOfYear,input$maxOfYear,input$startYear,input$endYear,input$flim,input$fpa,input$blim,input$bpa,input$bmsy,input$fmsy,input$msy,input$msyBTrigger,input$b40,input$m,input$fofl,input$last_f,input$resiliance,input$r.low,input$r.hi,input$stb.low,input$stb.hi,input$int.yr,input$intb.low,input$intb.hi,input$endb.low,input$endb.hi,input$q.start,input$q.end,input$btype,input$force.cmsy,input$comments, input$vreUsername, input$vreToken, inputCsvFile, templateFileDlmTools)
      for(i in 1:nrow(ret)) {
        row <- ret[i,]
        if (row$description == "estimates") {
          contents <- getURL(row$url)
          cmsy$dlmTools$textRaw <- contents
          contents <- gsub("\n\r", "<br/>", contents)
          contents <- gsub("\n", "<br/>", contents)
          contents <- gsub("----------------------------------------------------------", "", contents)
          cmsy$dlmTools$text <- contents
        }
        if (row$description == "analysis_charts") {
          fileAnalisysChart <- tempfile(fileext=".jpg")
          download.file(row$url, fileAnalisysChart)
          cmsy$dlmTools$analisysChart <- fileAnalisysChart
          cmsy$dlmTools$analisysChartUrl <- row$url
        }
        if (row$description == "management_charts") {
          fileManagementChart <- tempfile(fileext=".jpg")
          download.file(row$url, fileManagementChart)
          cmsy$dlmTools$managementChart <- fileManagementChart
          cmsy$dlmTools$managementChartUrl <- row$url
        }
        if (row$description == "Log of the computation") {
          cmsy$dlmTools$log <- row$url
        }
      }
    } else {
      cmsy$dlmTools <- NULL
    }
    if (input$cmsyLegacy) {
      ret <- runCmsy(input$region,input$subregion,input$stock,input$group,input$name,input$englishName,input$scientificName,input$source,input$minOfYear,input$maxOfYear,input$startYear,input$endYear,input$flim,input$fpa,input$blim,input$bpa,input$bmsy,input$fmsy,input$msy,input$msyBTrigger,input$b40,input$m,input$fofl,input$last_f,input$resiliance,input$r.low,input$r.hi,input$stb.low,input$stb.hi,input$int.yr,input$intb.low,input$intb.hi,input$endb.low,input$endb.hi,input$q.start,input$q.end,input$btype,input$force.cmsy,input$comments, input$vreUsername, input$vreToken, inputCsvFile, templateFileLegacy)
      for(i in 1:nrow(ret)) {
        row <- ret[i,]
        if (row$description == "estimates") {
          contents <- getURL(row$url)
          cmsy$legacy$textRaw <- contents
          contents <- gsub("\n\r", "<br/>", contents)
          contents <- gsub("\n", "<br/>", contents)
          contents <- gsub("----------------------------------------------------------", "", contents)
          cmsy$legacy$text <- contents
        }
        if (row$description == "analysis_charts") {
          fileAnalisysChart <- tempfile(fileext=".jpg")
          download.file(row$url, fileAnalisysChart)
          cmsy$legacy$analisysChart <- fileAnalisysChart
          cmsy$legacy$analisysChartUrl <- row$url
        }
        if (row$description == "management_charts") {
          fileManagementChart <- tempfile(fileext=".jpg")
          download.file(row$url, fileManagementChart)
          cmsy$legacy$managementChart <- fileManagementChart
          cmsy$legacy$managementChartUrl <- row$url
        }
        if (row$description == "Log of the computation") {
          cmsy$legacy$log <- row$url
        }
      }
    } else {
      cmsy$legacy <- NULL
    }
    
    
    #ret = runCMSY(inFile1$datapath, inFile2$datapath, input$stocks, input$uncert, input$sigmaR, input$kv_pairs, input$ni, input$ni, input$ni, input$nab, T, F, NA, F)
    
  })
  
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$plotA <- renderPlotly({ 
    #TODO: Needs abline
    if ("res" %in% names(cmsy)) {
      vectorizedPlot <- plot_ly(as.data.frame(cmsy$res$plotA$coordinates), x = ~x, y = ~y, 
                                mode = 'lines', type='scatter',
                                colors = "#858585") %>%
        layout(xaxis =  list(title=cmsy$res$plotA$xlab), 
               yaxis =  list(range = cmsy$res$plotA$ylim, title=cmsy$res$plotA$ylab), 
               title=cmsy$res$plotA$main)
      
      for (i in 1:length(names(cmsy$res$plotA$plot$lines))) {
        d<-as.data.frame(names(cmsy$res$plotA$plot$lines)[i])
        
        vectorizedPlot <- add_trace(
          vectorizedPlot, 
          x=cmsy$res$plotA$plot$lines[[names(cmsy$res$plotA$plot$lines)[i]]]$x, 
          y=cmsy$res$plotA$plot$lines[[names(cmsy$res$plotA$plot$lines)[i]]]$y, 
          mode='lines', 
          type='scatter', 
          line = list(
            color = cmsy$res$plotA$plot$lines[[names(cmsy$res$plotA$plot$lines)[i]]]$color, 
            width = 1, 
            dash = cmsy$res$plotA$plot$lines[[names(cmsy$res$plotA$plot$lines)[i]]]$lty)
        )
      }
      
      for (i in 1:length(names(cmsy$res$plotA$plot$points))) {
        d<-as.data.frame(names(cmsy$res$plotA$plot$points)[i])
        
        vectorizedPlot <- add_trace(
          vectorizedPlot, 
          x=cmsy$res$plotA$plot$points[[names(cmsy$res$plotA$plot$points)[i]]]$x, 
          y=cmsy$res$plotA$plot$points[[names(cmsy$res$plotA$plot$points)[i]]]$y, 
          mode='markers', 
          marker = list(
            color = cmsy$res$plotA$plot$points[[names(cmsy$res$plotA$plot$points)[i]]]$color, 
            width = 1)
        )
      }
      
      vectorizedPlot
    } else {
      return (NULL)
    }
  })
  
  output$plotB <- renderPlotly({ 
    #TODO: Needs abline
    if ("res" %in% names(cmsy)) {
      vectorizedPlot <- plot_ly(as.data.frame(cmsy$res$plotB$coordinates), x = ~x, y = ~y, 
                                mode = 'marker', type='scatter',
                                colors = "#858585") %>%
        layout(xaxis =  list(title=cmsy$res$plotB$xlab, type="log"), 
               yaxis =  list(range = cmsy$res$plotB$ylim, title=cmsy$res$plotB$ylab), 
               title=cmsy$res$plotB$main)
      
      for (i in 1:length(names(cmsy$res$plotB$plot$lines))) {
        d<-as.data.frame(names(cmsy$res$plotB$plot$lines)[i])
        
        vectorizedPlot <- add_trace(
          vectorizedPlot, 
          x=cmsy$res$plotB$plot$lines[[names(cmsy$res$plotB$plot$lines)[i]]]$x, 
          y=cmsy$res$plotB$plot$lines[[names(cmsy$res$plotB$plot$lines)[i]]]$y, 
          mode='lines', 
          type='scatter', 
          line = list(
            color = cmsy$res$plotB$plot$lines[[names(cmsy$res$plotB$plot$lines)[i]]]$color, 
            width = 1, 
            dash = cmsy$res$plotB$plot$lines[[names(cmsy$res$plotB$plot$lines)[i]]]$lty)
        )
      }
      
      for (i in 1:length(names(cmsy$res$plotB$plot$points))) {
        d<-as.data.frame(names(cmsy$res$plotB$plot$points)[i])
        
        vectorizedPlot <- add_trace(
          vectorizedPlot, 
          x=cmsy$res$plotB$plot$points[[names(cmsy$res$plotB$plot$points)[i]]]$x, 
          y=cmsy$res$plotB$plot$points[[names(cmsy$res$plotB$plot$points)[i]]]$y, 
          mode='markers', 
          marker = list(
            color = cmsy$res$plotB$plot$points[[names(cmsy$res$plotB$plot$points)[i]]]$color, 
            width = 1)
        )
      }
      
      vectorizedPlot
    } else {
      return (NULL)
    }
  })
  
  output$plotC <- renderPlotly({ 
    #TODO: Needs abline
    if ("res" %in% names(cmsy)) {
      vectorizedPlot <- plot_ly(as.data.frame(cmsy$res$plotC$coordinates), x = ~x, y = ~y, 
                                mode = 'marker', type='scatter',
                                colors = "#858585") %>%
        layout(xaxis =  list(title=cmsy$res$plotC$xlab, type="log"), 
               yaxis =  list(range = cmsy$res$plotC$ylim, title=cmsy$res$plotC$ylab), 
               title=cmsy$res$plotC$main)
      
      for (i in 1:length(names(cmsy$res$plotC$plot$lines))) {
        d<-as.data.frame(names(cmsy$res$plotC$plot$lines)[i])
        
        vectorizedPlot <- add_trace(
          vectorizedPlot, 
          x=cmsy$res$plotC$plot$lines[[names(cmsy$res$plotC$plot$lines)[i]]]$x, 
          y=cmsy$res$plotC$plot$lines[[names(cmsy$res$plotC$plot$lines)[i]]]$y, 
          mode='lines', 
          type='scatter', 
          line = list(
            color = cmsy$res$plotC$plot$lines[[names(cmsy$res$plotC$plot$lines)[i]]]$color, 
            width = 1, 
            dash = cmsy$res$plotC$plot$lines[[names(cmsy$res$plotC$plot$lines)[i]]]$lty)
        )
      }
      
      for (i in 1:length(names(cmsy$res$plotC$plot$points))) {
        d<-as.data.frame(names(cmsy$res$plotC$plot$points)[i])
        
        vectorizedPlot <- add_trace(
          vectorizedPlot, 
          x=cmsy$res$plotC$plot$points[[names(cmsy$res$plotC$plot$points)[i]]]$x, 
          y=cmsy$res$plotC$plot$points[[names(cmsy$res$plotC$plot$points)[i]]]$y, 
          mode='markers', 
          marker = list(
            color = cmsy$res$plotC$plot$points[[names(cmsy$res$plotC$plot$points)[i]]]$color, 
            width = 8, 
            dash = cmsy$res$plotC$plot$points[[names(cmsy$res$plotC$plot$points)[i]]]$lty)
        )
      }
      
      vectorizedPlot
    } else {
      return (NULL)
    }
  })
  
  output$plotD <- renderPlotly({ 
    #TODO: Needs abline
    if ("res" %in% names(cmsy)) {
      vectorizedPlot <- plot_ly(as.data.frame(cmsy$res$plotD$coordinates), x = ~x, y = ~y, 
                                mode = 'lines', type='scatter',
                                color = cmsy$res$plotD$color) %>%
        layout(xaxis =  list(title=cmsy$res$plotD$xlab), 
               yaxis =  list(range = cmsy$res$plotD$ylim, title=cmsy$res$plotD$ylab), 
               title=cmsy$res$plotD$main)
      
      for (i in 1:length(names(cmsy$res$plotD$plot$lines))) {
        d<-as.data.frame(names(cmsy$res$plotD$plot$lines)[i])
        
        vectorizedPlot <- add_trace(
          vectorizedPlot, 
          x=cmsy$res$plotD$plot$lines[[names(cmsy$res$plotD$plot$lines)[i]]]$x, 
          y=cmsy$res$plotD$plot$lines[[names(cmsy$res$plotD$plot$lines)[i]]]$y, 
          mode='lines', 
          type='scatter', 
          line = list(
            color = cmsy$res$plotD$plot$lines[[names(cmsy$res$plotD$plot$lines)[i]]]$color, 
            width = 1, 
            dash = cmsy$res$plotD$plot$lines[[names(cmsy$res$plotD$plot$lines)[i]]]$lty)
        )
      }
      
      for (i in 1:length(names(cmsy$res$plotD$plot$points))) {
        d<-as.data.frame(names(cmsy$res$plotD$plot$points)[i])
        
        vectorizedPlot <- add_trace(
          vectorizedPlot, 
          x=cmsy$res$plotD$plot$points[[names(cmsy$res$plotD$plot$points)[i]]]$x, 
          y=cmsy$res$plotD$plot$points[[names(cmsy$res$plotD$plot$points)[i]]]$y, 
          mode='markers', 
          marker = list(
            color = cmsy$res$plotD$plot$points[[names(cmsy$res$plotD$plot$points)[i]]]$color, 
            width = 1)
        )
      }
      vectorizedPlot
    } else {
      return (NULL)
    }
  })
  
  output$plotE <- renderPlotly({ 
    #TODO: Needs abline
    if ("res" %in% names(cmsy)) {
      vectorizedPlot <- plot_ly(as.data.frame(cmsy$res$plotE$coordinates), x = ~x, y = ~y, 
                                mode = 'lines', type='scatter',
                                color = cmsy$res$plotF$color) %>%
        layout(xaxis =  list(title=cmsy$res$plotE$xlab), 
               yaxis =  list(range = cmsy$res$plotE$ylim, title=cmsy$res$plotE$ylab), 
               title=cmsy$res$plotE$main)
      
      for (i in 1:length(names(cmsy$res$plotE$plot$lines))) {
        d<-as.data.frame(names(cmsy$res$plotE$plot$lines)[i])
        
        vectorizedPlot <- add_trace(
          vectorizedPlot, 
          x=cmsy$res$plotE$plot$lines[[names(cmsy$res$plotE$plot$lines)[i]]]$x, 
          y=cmsy$res$plotE$plot$lines[[names(cmsy$res$plotE$plot$lines)[i]]]$y, 
          mode='lines', 
          type='scatter', 
          line = list(
            color = cmsy$res$plotE$plot$lines[[names(cmsy$res$plotE$plot$lines)[i]]]$color, 
            width = 1, 
            dash = cmsy$res$plotE$plot$lines[[names(cmsy$res$plotE$plot$lines)[i]]]$lty)
        )
      }
      
      for (i in 1:length(names(cmsy$res$plotE$plot$points))) {
        d<-as.data.frame(names(cmsy$res$plotE$plot$points)[i])
        
        vectorizedPlot <- add_trace(
          vectorizedPlot, 
          x=cmsy$res$plotE$plot$points[[names(cmsy$res$plotE$plot$points)[i]]]$x, 
          y=cmsy$res$plotE$plot$points[[names(cmsy$res$plotE$plot$points)[i]]]$y, 
          mode='markers', 
          marker = list(
            color = cmsy$res$plotE$plot$points[[names(cmsy$res$plotE$plot$points)[i]]]$color, 
            width = 1)
        )
      }
      vectorizedPlot
    } else {
      return (NULL)
    }
  })
  
  output$plotF <- renderPlotly({ 
    if ("res" %in% names(cmsy)) {
      vectorizedPlot <- plot_ly(as.data.frame(cmsy$res$plotF$coordinates), x = ~x, y = ~y, 
                                mode = 'lines', type='scatter',
                                color = cmsy$res$plotF$color) %>%
        add_trace(y = cmsy$res$plotF$plot$points$y, x = cmsy$res$plotF$plot$points$x, mode = 'markers') %>%
        layout(xaxis =  list(range = cmsy$res$plotF$xlim, title=cmsy$res$plotF$xlab), 
               yaxis =  list(range = cmsy$res$plotF$ylim, title=cmsy$res$plotF$ylab), 
               title=cmsy$res$plotF$main)
      
      for (i in 1:length(names(cmsy$res$plotF$plot$lines))) {
        d<-as.data.frame(names(cmsy$res$plotF$plot$lines)[i])
        
        vectorizedPlot <- add_trace(
          vectorizedPlot, 
          x=cmsy$res$plotF$plot$lines[[names(cmsy$res$plotF$plot$lines)[i]]]$x, 
          y=cmsy$res$plotF$plot$lines[[names(cmsy$res$plotF$plot$lines)[i]]]$y, 
          mode='lines', 
          type='scatter', 
          line = list(
            color = cmsy$res$plotF$plot$lines[[names(cmsy$res$plotF$plot$lines)[i]]]$color, 
            width = 1, 
            dash = cmsy$res$plotF$plot$lines[[names(cmsy$res$plotF$plot$lines)[i]]]$lty)
        )
      }
      
      for (i in 1:length(names(cmsy$res$plotF$plot$points))) {
        d<-as.data.frame(names(cmsy$res$plotF$plot$points)[i])
        
        vectorizedPlot <- add_trace(
          vectorizedPlot, 
          x=cmsy$res$plotF$plot$points[[names(cmsy$res$plotF$plot$points)[i]]]$x, 
          y=cmsy$res$plotF$plot$points[[names(cmsy$res$plotF$plot$points)[i]]]$y, 
          mode='markers', 
          marker = list(
            color = cmsy$res$plotF$plot$points[[names(cmsy$res$plotF$plot$points)[i]]]$color, 
            width = 1)
        )
      }
      vectorizedPlot
    } else {
      return (NULL)
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

