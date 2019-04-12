library(shiny)
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)
library(tools)

shinyApp(
  shinyUI(
    pageWithSidebar(
      headerPanel(h1("",
                     style = "font-family: 'Arial', font-weight: bold; line-height: 1.1; color: #FF0000;")),
      sidebarPanel(
        fluidRow(column(width = 5, img(src = 'heatmapgenerator_icon.png', width = "100%", height = "100%")),
                 column(width = 2, h4(HTML('<br><br>'), 'HEATMAP', HTML('<br>'), 'GENERATOR'))),
        # img(src = 'heatmapgenerator_icon.png', width = "30%", height = "30%", style="display: block; margin-left: auto; margin-right: auto;"),
        # h4('HEATMAPGENERATOR', style = 'font-weight: bold', align = 'center'),
        helpText(HTML('<br>'), 'Sample file downloads:'),
        downloadButton("downloadSmallData", label = "Small sample file", style = 'padding-right: 49px'),
        downloadButton("downloadMidData", label = "Mid-sized sample file", style = 'padding-right: 22px'),
        downloadButton("downloadHugeData", label = "Huge sample file", style = 'padding-right: 50px'),
        helpText(HTML('<br>')),
        fileInput("filename", "Choose File to Upload:", accept = c('.csv')),
        selectInput("lowColor", "Low Value:", c("green", "blue", "purple", "red", "orange", "yellow", "white", "black")),
        # selectInput("midColor", "Mid Value:", c("none", "black", "green", "blue", "purple", "red", "orange", "yellow", "white")),
        selectInput("highColor", "High Value:", c("red", "orange", "yellow", "green", "blue", "purple", "orange", "white", "black")),
        selectInput("dendrogram", "Apply Clustering:", c("none", "row", "column", "both")),
        #selectInput("correlationCoefficient", "Distance Metric (Option A):", c("pearson", "kendall", "spearman")),
        selectInput("distanceMethod", "Distance Metric:", c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
        selectInput("agglomerationMethod", "Linkage Algorithm:", c("complete", "single", "average", "centroid", "median", "mcquitty", "ward.D", "ward.D2")),
        selectInput("scale", "Apply Scaling:", c("row", "column", "none")),
        selectInput("key", "Color Key:", c("TRUE", "FALSE")),
        selectInput("trace", "Make Trace:", c("none", "column", "row", "both")),
        sliderInput("xfontsize", "Choose Y Font Size:", min = 0.3, max = 2, value = 0.5),
        sliderInput("yfontsize", "Choose X Font Size:", min = 0.3, max = 2, value = 0.72),
        downloadButton("downloadHeatmap", "Download heatmap", style = 'padding-right: 71px'),
        downloadButton("downloadClusteredInput", "Download clustered input file"),
        helpText(HTML('<br>')),
        actionButton('show', 'About')
      ),
      mainPanel(
        tags$head(includeScript("google-analytics.js")),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        tabsetPanel(
          # tabPanel("Instructions", htmlOutput("linebreak1"), textOutput("text1"), htmlOutput("linebreak2"), img(src='excel.png', width = '80%', height = '80%'), htmlOutput("linebreak3"), textOutput("text2"), htmlOutput("linebreak4"),textOutput("text3"), htmlOutput("linebreak5"), img(src='benchmarks.png', width="80%", height="80%"), htmlOutput("linebreak6"), textOutput("text4")),
          tabPanel("Heatmap", uiOutput(outputId = "image"), uiOutput("sorry"), htmlOutput("linebreak8"), plotOutput("static"), uiOutput("xzoom"), uiOutput("yzoom"))
          #tabPanel("Interactive Heatmap", uiOutput(outputId = "image2"), uiOutput("sorry2"), plotlyOutput("interactive", height = "700px"))
        )
      )
    )
  ),

  shinyServer(
    function(input, output) {
      # about button
      observeEvent(input$show, {
        showModal(modalDialog(
          title = 'About',
          'Copyright 2014-2016 Khomtchouk et al.',
          'This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public Lciense as published by the Free Software Foundation, either version 3 of the license, or (at your option) any later version.',
          easyClose = TRUE,
          footer = NULL
        ))
      })

      # instructions tab
      output$linebreak1 <- renderUI({ HTML('<br>') })
      output$linebreak2 <- renderUI({ HTML('<br>') })
      output$linebreak3 <- renderUI({ HTML('<br>') })
      output$linebreak4 <- renderUI({ HTML('<br>') })
      output$linebreak5 <- renderUI({ HTML('<br>') })
      output$linebreak6 <- renderUI({ HTML('<br>') })
      output$linebreak7 <- renderUI({ HTML('<br>') })
      output$linebreak8 <- renderUI({ HTML('<br>') })
      output$text1 <- renderText({ "(0) You can easily make a .csv file by simply saving your Microsoft Excel workbook as a .csv through 'Save As'.  Before saving as a .csv, your Excel file should look something like:" })
      output$text2 <- renderText({ "Please note that all cell values must be positive (i.e., corresponding to raw gene expression values, i.e., read counts per gene per sample) from a ChIP-seq or RNA-seq experiment.  Some sample .csv files are provided for download.  You may download these files to your computer, then click the 'Browse...' button to upload either of them.  In offbeat cases where the input file is a combination of various standard (or non-standard) delimiters, simply use the 'Text to Columns' feature in Microsoft Excel under the 'Data' tab to parse the file before using shinyheatmap." })
      output$text3 <- renderText({ "(1) After uploading a .csv file, both a static and interactive heatmap will be produced in their respective panels.  Otherwise, if the input dataset is too large, the user is prompted to navigate to shinyheatmap's high performance web server (called fastheatmap), which is especially useful for interactively examining extremely large biological input datasets (e.g., tens or hundreds of thousands of rows).  Fastheatmap is the fastest interactive heatmap software in the world (>100,000 times faster than competitors):" })
      output$text4 <- renderText({ "(2) You may customize your static heatmap parameters in the left sidebar panel and download the heatmap to your computer.  Likewise, you may customize the interactive heatmap parameters in its own dedicated panel (which will appear on-hover under the 'Interactive Heatmap' tab in the top right corner) and download your heatmap.  This 'Interactive Heatmap' tab is great for zooming in and out of the contents of the heatmaps (both from the interior of the heatmap itself, as well as from the dendrogram panes)." })

      # sample file download (small dataset)
      output$downloadSmallData <- downloadHandler(
        filename <- function() {
          paste('small', 'Genes', 'File', '.csv', sep='')
        },
        content <- function(file) {
          file.copy("smallGenesFile.csv", file)
        },
        contentType = "text/csv"
      )

      # sample file download (mid-sized dataset)
      output$downloadMidData <- downloadHandler(
        filename <- function() {
          paste('mid', 'Genes', 'File', '.csv', sep='')
        },
        content <- function(file) {
          file.copy("midGenesFile.csv", file)
        },
        contentType = "text/csv"
      )

      # sample file download (huge dataset)
      output$downloadHugeData <- downloadHandler(
        filename <- function() {
          paste('huge', 'Genes', 'File', '.csv', sep='')
        },
        content <- function(file) {
          file.copy("hugeGenesFile.csv", file)
        },
        contentType = "text/csv"
      )

      # file upload
      datasetInput <- reactive({
        validate(
          need(input$filename != 0, "To begin drawing a heatmap, please select a file for input")
        )
        inFile <- input$filename
        if (is.null(inFile)) return(NULL)
        read.csv(inFile$datapath)
      })

      output$static <- renderPlot({
        genexp <- datasetInput()
        genexp_df <- as_data_frame(genexp)
        firstcolname <- names(genexp_df)[1]
        n <- dim(genexp_df)[1]
        if (input$xvar == 'all' & input$yvar == 'all') {
          static_plot <- genexp_df %>%
            select(gene_name = !!firstcolname, everything()) %>%
            mutate(gene_name = as.character(gene_name)) %>% melt() %>%
            ggplot(aes(x = variable, y = gene_name)) +
            geom_raster(aes(fill = value)) +
            scale_fill_gradient(low = input$lowColor, high = input$highColor)
        }
        if (input$xvar == 'all' & input$yvar != 'all') {
          static_plot <- genexp_df %>%
            select(gene_name = !!firstcolname, everything()) %>%
            mutate(gene_name = as.character(gene_name)) %>% melt() %>%
            ggplot(aes(x = variable, y = gene_name)) +
            geom_raster(aes(fill = value)) +
            scale_fill_gradient(low = input$lowColor, high = input$highColor) +
            ylim(input$yvar, input$yvar)
        }
        if (input$xvar != 'all' & input$yvar == 'all') {
          static_plot <- genexp_df %>%
            select(gene_name = !!firstcolname, everything()) %>%
            mutate(gene_name = as.character(gene_name)) %>% melt() %>%
            ggplot(aes(x = variable, y = gene_name)) +
            geom_raster(aes(fill = value)) +
            scale_fill_gradient(low = input$lowColor, high = input$highColor) +
            xlim(input$xvar, input$xvar)
        }
        if (input$xvar != 'all' & input$yvar != 'all') {
          static_plot <- genexp_df %>%
            select(gene_name = !!firstcolname, everything()) %>%
            mutate(gene_name = as.character(gene_name)) %>% melt() %>%
            ggplot(aes(x = variable, y = gene_name)) +
            geom_raster(aes(fill = value)) +
            scale_fill_gradient(low = input$lowColor, high = input$highColor) +
            xlim(input$xvar, input$xvar) +
            ylim(input$yvar, input$yvar)
        }
        withProgress(message = 'Making static heatmap:', value = 0, {
          for (i in 1:n) {
            incProgress(1/n, detail = 'Please wait...')
          }
        })
        static_plot
      })

      output$xzoom <- renderUI({
        genexp <- datasetInput()
        genexp_df <- as_data_frame(genexp)
        xvars <- c('all', unique(names(genexp_df)[-1]))
        selectInput('xvar', 'Column of Interest: ', xvars)
      })

      output$yzoom <- renderUI({
        genexp <- datasetInput()
        genexp_df <- as_data_frame(genexp)
        yvars <- c('all', unique(genexp_df$Gene_Name))
        selectInput('yvar', 'Row of Interest: ', yvars)
      })

      # re-sort input file after hierarchical clustering (prep stage)
      clusteredInput <- function(){
        heatmap_object <- staticHeatmap()
        genexp_df_mat[rev(heatmap_object$rowInd), heatmap_object$colInd]
      }

      # re-sort input file after hierarchical clustering (download stage)
      output$downloadClusteredInput <- downloadHandler(
        filename = function() {
          paste(basename(file_path_sans_ext(input$filename)), '_clustered', '.csv', sep='')
        },
        content = function(file) {
          write.csv(clusteredInput(), file)
        }
      )

      # static heatmap download
      output$downloadHeatmap <- downloadHandler(
        filename <- function() {
          paste0(basename(file_path_sans_ext(input$filename)), '_heatmap', '.png', sep='')
        },
        content <- function(file) {
          png(file)
          tiff(
            file,
            width = 4000,
            height = 2000,
            units = "px",
            pointsize = 12,
            res = 300
          )
          staticHeatmap()
          dev.off()
        }
      )
    }
  )
)
