# Interactive diagnostic plots for outliers in linear regression based on recomendations of
# Field, Andy P, J Miles, and Z Field. 2012. Discovering Statistics Using R. London; Thousand Oaks, Calif.: Sage.



# Johann Popp
# 2017-11-05
#########################################################

# Load example data (Record sales)
# dat <- read.delim("https://studysites.uk.sagepub.com/dsur/study/DSUR Data Files/Chapter 7/Album Sales 02.dat")

# Build example model
# model <- lm(sales ~ adverts + airplay + attract, data = dat)





#' Interactive Diagnostic Plots for Linear Regression Models
#'
#'
#'
#' @param x A model of class \code{lm}.
#'
#' @return
#'    An interactive shiny app with plots of residuals and influence statistics as well as thresholds recommended by Field 2005.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' DiagPlotLM(lm(sales ~ adverts + airplay + attract, data = mod)
#' }

#'
diagPlotLM <- function(x){
  ########
  # Preliminaries
  model <- x

#  library(shiny)
#  library(ggplot2)

  # Labels to plot
  axLabs <- c("id", "Fitted Values", "Standardized Residuals" , "Absolute Std. Residuals", paste("Absolute Std. DFBeta -", names(stats::coef(model))),paste("DFBeta -", names(stats::coef(model))), "Standardized DFFit", "Absolute Std. DFFit", "Covariance Ratio", "Cook's Distance", "Leverage",names(model$model))

  # Extract data frame from the model and add diagnostic statistics
  outData <- data.frame(id = as.numeric(rownames(model$model)), "FittedValues" = model$fitted.values, stResid = stats::rstandard(model), absStdResid = abs(stats::rstandard(model)), abs(stats::dfbetas(model)), stats::dfbeta(model), stats::dffits(model), abs(stats::dffits(model)), stats::covratio(model), stats::cooks.distance(model), stats::hatvalues(model))

  infNames <- c(paste("dfb", names(stats::coef(model)), sep = "."), paste("Z-dfb", names(stats::coef(model)), sep = "."), "dffits", "abs.dffits", "cov.r", "cook.d", "leverage")
  names(outData)[5:(4+length(infNames))] <- infNames

  outData <- data.frame(outData,  model$model)

  ###########################
  # Shiny User Interface
  ###########################
  ui <- shiny::fluidPage(
    # Setting seleced headings in line.
    htmltools::tags$head(
      htmltools::tags$style(type="text/css",
                 "label.control-label,
               .selectize-control.single{ display: table-cell;}
               .form-group { display: table-row;}")
    ),

    htmltools::h1("Outlier Diagnostics for Linear Regression"),
    # Introduction

    htmltools::p("With these plots you can search for and investigate extreme and influential cases in a linear regression model as recommended by Field et al 2012\u00b9. Inspect points that are way out of line of the others. Mark them by brushing (hold left mouse button and move the pointer). This will show you the variable values of the selected cases and highlight them in all the other plots. Try to find out what makes these covariate patterns so special. Perhaps they are data entry errors or they are an important combination of variables that needs to be considered by an interaction in the model. See what happens to the model if you would exclude them. But in the end", htmltools::tags$strong("never exclude data just because it does not fit your model.")),
    htmltools::p("Critical thresholds are marked with lines:",
                 htmltools::tags$ul(
                   htmltools::tags$li("Standardized residuals: +-3 standard deviations"),
                   htmltools::tags$li("Leverage: 2 and 3 times the mean leverage"),
                   htmltools::tags$li("Cook's distance: 1"),
                   htmltools::tags$li("Covariance ratio: 1 +-[3(p)/n]  (p = number of model parameters; n = sampe size )"),
                   htmltools::tags$li("Standardized DFBetas & DFFit: 1")
      )),
    htmltools::tags$hr(),


    # Show plots
    shiny::fluidRow(
      shiny::column(4,
             shiny::plotOutput("plot1", height = 250,
                               brush = shiny::brushOpts(id = "brP")),
             shiny::plotOutput("plot4", height = 250,
                               brush = shiny::brushOpts(id = "brP"))
      ),
      shiny::column(4,
             shiny::plotOutput("plot2", height = 250,
                        brush = shiny::brushOpts(id = "brP")),
             shiny::plotOutput("plot5", height = 250,
                        brush = shiny::brushOpts(id = "brP"))
      ),
      shiny::column(4,
             shiny::plotOutput("plot3", height = 250,
                        brush = shiny::brushOpts(id = "brP")),
             shiny::plotOutput("plot6", height = 250,
                        brush = shiny::brushOpts(id = "brP")),
             shiny::column(12, offset = 1,
                    shiny::selectInput("y6", "Y-axis: ", choices = axLabs,
                                selected = axLabs[grep("DFBeta", axLabs)[1]],
                                selectize = FALSE, width = "80%"))
      )
    ), # end of plotting fluid row 1 +2
    shiny::fluidRow(
      shiny::column(4,
             shiny::plotOutput("plot7", height = 250,
                        brush = shiny::brushOpts(id = "brP")),
             shiny::column(12, offset = 1,
                           shiny::selectInput("y7", "Y-axis: ", choices = axLabs,
                                selected = axLabs[grep("DFBeta", axLabs)[2]],
                                selectize = FALSE, width = "80%"),
                           shiny::selectInput("x1", "X-axis (for all the plots): ",
                                choices = c("Fitted Values", "id"),
                                selectize = FALSE, width = "80%"))
      ),
      shiny::column(4,
                    shiny::plotOutput("plot8", height = 250,
                        brush = shiny::brushOpts(id = "brP")),
                    shiny::column(12, offset = 1,
                                  shiny::selectInput("y8", "Y-axis: ", choices = axLabs,
                                selected = axLabs[grep("DFBeta", axLabs)[3]],
                                selectize = FALSE, width = "80%"))
      ),
      shiny::column(4,
                    shiny::plotOutput("plot9", height = 250,
                        brush = shiny::brushOpts(id = "brP")),
                    shiny::column(12, offset = 1,
                                  shiny::selectInput("y9", "Y-axis: ", choices = axLabs,
                                selected = axLabs[grep("DFBeta", axLabs)[4]],
                                selectize = FALSE, width = "80%"))
      )
    ), # end of plotting row 3
    htmltools::h3("Selected Cases"),
    shiny::tableOutput("brushed"),
    htmltools::h3("Change in Model Coefficients When Seleced Cases are Erased"),
    shiny::tableOutput("modelCompare"),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::verbatimTextOutput("full")),
      shiny::column(6,
                    shiny::verbatimTextOutput("reduced"))
    ),

    shiny::verbatimTextOutput("info"),

    "Programmed with R\u00b2, shiny\u00b3 and ggplot2\u2074 by Johann Popp. Please feel free to comment and contribute at ", htmltools::a(href = "https://github.com/JohannPopp/outlierPlot/issues", "https://github.com/JohannPopp/outlierPlot/issues", target = "_blank"), "or send an e-mail to", htmltools::a(href="mailto:johann.popp@haw-hamburg.de", "johann.popp@haw-hamburg.de"),
    htmltools::tags$hr(),
    "\u00b9 Field, Andy P, J Miles, and Z Field. 2012. Discovering Statistics Using R. London; Thousand Oaks, Calif.: Sage.", htmltools::br(),
    "\u00B2 R Core Team. R: A Language and Environment for Statistical Computing. Vienna, Austria: R Foundation for Statistical Computing, 2017. https://www.R-project.org/.
             ", htmltools::br(),
    "\u00b3 Chang, Winston, Joe Cheng, J. J. Allaire, Yihui Xie, und Jonathan McPherson. shiny: Web Application Framework for R, 2017. https://CRAN.R-project.org/package=shiny.
             ", htmltools::br(),
    "\u2074 Wickham, Hadley. Ggplot2 Elegant Graphics for Data Analysis. Dordrecht; New York: Springer, 2009.
             "
  )

  #############################
  # Shiny Server
  #############################
  server <- function(input, output, session){




    # Brushed rows of the data frame per covariate pattern
    brushTable <- shiny::reactive({
      shiny::brushedPoints(outData, input$brP)
    })

    ### Plots

    # Standardizes residuals vs. fitted
    output$plot1 <- shiny::renderPlot({
      xv <- names(outData)[axLabs == input$x1]

      p1 <- ggplot2::ggplot(data = outData, ggplot2::aes_string(x = xv, y = "stResid"))
      p1 +  ggplot2::geom_hline(yintercept = c(-3, 3), linetype = "dashed", colour = "red") +
        ggplot2::annotate("label", x = min(p1$data[,names(p1$data) == p1$labels$x]),
                 y = min(outData$stResid)*1.3,
                 label = paste(
                   round(
                     sum(prop.table(table(cut(outData$stResid, c(-Inf, -1.96, 1.96, Inf))))[c(1,3)]) * 100,
                     2),
                   "% of points >+-1.96 SD", sep = ""),
                 hjust = "left", fill = "grey90") +
        ggplot2::annotate("label", x = max(p1$data[,names(p1$data) == p1$labels$x]),
                 y = min(outData$stResid)*1.3,
                 label = paste(
                   round(
                     sum(prop.table(table(cut(outData$stResid, c(-Inf, -2.58, 2.58, Inf))))[c(1,3)]) * 100,
                     2),
                   "% of points >+-2.58 SD", sep = ""),
                 hjust = "right", fill = "grey90") +
        ggplot2::geom_text(data = brushTable(),
                  label = paste("  ", rownames(brushTable()), "  "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        ggplot2::ylim(min(outData$stResid, na.rm = TRUE)*1.3, max(outData$stResid, na.rm = TRUE)) +
        ggplot2::labs(x = input$x1, y = "Standardized Redsiduals")
    })

    # Leverage vs. fitted
    output$plot2 <- shiny::renderPlot({
      xv <- names(outData)[axLabs == input$x1]

      ggplot2::ggplot(data = outData, ggplot2::aes_string(x = xv, y = "leverage")) +
        ggplot2::geom_hline(yintercept = mean(outData$leverage, na.rm = TRUE) * c(2,3),
                   linetype = "dashed", colour = c("darkred","red")) +
        ggplot2::geom_text(data = brushTable(),
                  label = paste("  ", rownames(brushTable()), "  "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        ggplot2::ylim(0, max(outData$leverage, na.rm = TRUE)) +
        ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(trans = ~., breaks = mean(outData$leverage)*c(2,3), labels = c("2mean", "3mean"))) +
        ggplot2::labs(x = input$x1, y = "Leverage")
    })

    # Cook's distance vs. fitted
    output$plot3 <- shiny::renderPlot({
      xv <- names(outData)[axLabs == input$x1]

      ggplot2::ggplot(data = outData, ggplot2::aes_string(x = xv, y = "cook.d")) +
        ggplot2::geom_hline(yintercept = 1,
                   linetype = "dashed", colour = "red") +
        ggplot2::geom_text(data = brushTable(),
                  label = paste("  ", rownames(brushTable()), "  "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        ggplot2::ylim(0, max(outData$cook.d, na.rm = TRUE)) +
        ggplot2::labs(x = input$x1, y = "Cook's Distance")
    })

    # Convariance ratio vs. fitted
    output$plot4 <- shiny::renderPlot({
      xv <- names(outData)[axLabs == input$x1]

      ggplot2::ggplot(data = outData, ggplot2::aes_string(x = xv, y = "cov.r")) +
        ggplot2::geom_hline(yintercept = 1 + (3 * ncol(model$model) / nrow(model$model)) * c(-1, 1),
                   linetype = "dashed", colour = "red") +
        ggplot2::geom_text(data = brushTable(),
                  label = paste("  ", rownames(brushTable()), "  "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        #      ylim(min(outData$cov.r), max(outData$cov.r)) +
        ggplot2::labs(x = input$x1, y = "Covariance Ratio")
    })

    # dffit vs. fitted
    output$plot5 <- shiny::renderPlot({
      xv <- names(outData)[axLabs == input$x1]

      ggplot2::ggplot(data = outData, ggplot2::aes_string(x = xv, y = "dffits")) +
        ggplot2::geom_hline(yintercept = c(-1,1),
                   linetype = "dashed", colour = "red") +
        ggplot2::geom_text(data = brushTable(),
                  label = paste("  ", rownames(brushTable()), "  "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        ggplot2::labs(x = input$x1, y = "Standardized DFFit") +
        ggplot2::ylim(min(outData$dffits), max(outData$dffits))
    })

    # Std-dfbeta-intercept vs. fitted
    output$plot6 <- shiny::renderPlot({
      xv <- names(outData)[axLabs == input$x1]
      yv <- names(outData)[axLabs == input$y6]
      yval <- outData[,yv]

      p6 <- ggplot2::ggplot(data = outData, ggplot2::aes_string(x = xv, y = yv))
      if(grepl("Absolute Std. DF", input$y6) == TRUE){
        p6 <- p6 + ggplot2::geom_hline(yintercept = 1,
                              linetype = "dashed", colour = "red")
      }
      p6 + ggplot2::geom_text(data = brushTable(),
                     label = paste("  ", rownames(brushTable()), "  "),
                     hjust = "inward", colour = "darkblue", size = 3) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        ggplot2::labs(x = input$x1, y = input$y6) +
        ggplot2::ylim(min(yval), max(yval))
    })

    # dfbeta var 1 vs. fitted
    output$plot7 <- shiny::renderPlot({
      xv <- names(outData)[axLabs == input$x1]
      yv <- names(outData)[axLabs == input$y7]
      yval <- outData[,yv]

      p7 <-ggplot2::ggplot(data = outData, ggplot2::aes_string(x = xv, y = yv))
      if(grepl("Absolute Std. DF", input$y6) == TRUE){
        p7 <- p7 + ggplot2::geom_hline(yintercept = 1,
                              linetype = "dashed", colour = "red")
      }
      p7 + ggplot2::geom_text(data = brushTable(),
                     label = paste("  ", rownames(brushTable()), "  "),
                     hjust = "inward", colour = "darkblue", size = 3) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        ggplot2::labs(x = input$x1, y = input$y7) +
        ggplot2::ylim(min(yval), max(yval))
    })

    # dfbeta var 2 vs. fitted
    output$plot8 <- shiny::renderPlot({
      xv <- names(outData)[axLabs == input$x1]
      yv <- names(outData)[axLabs == input$y8]
      yval <- outData[,yv]

      p8 <- ggplot2::ggplot(data = outData, ggplot2::aes_string(x = xv, y = yv))
      if(grepl("Absolute Std. DF", input$y6)){
        p8 <- p8 + ggplot2::geom_hline(yintercept = 1,
                              linetype = "dashed", colour = "red")
      }
      p8 + ggplot2::geom_text(data = brushTable(),
                     label = paste("  ", rownames(brushTable()), "  "),
                     hjust = "inward", colour = "darkblue", size = 3) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        ggplot2::labs(x = input$x1, y = input$y8) +
        ggplot2::ylim(min(yval), max(yval))
    })

    # dfbeta var 3 vs. fitted
    output$plot9 <- shiny::renderPlot({
      xv <- names(outData)[axLabs == input$x1]
      yv <- names(outData)[axLabs == input$y9]
      yval <- outData[,yv]

      p9 <- ggplot2::ggplot(data = outData, ggplot2::aes_string(x = xv, y = yv))
      if(grepl("Absolute Std. DF", input$y6) == TRUE){
        p9 <- p9 + ggplot2::geom_hline(yintercept = 1,
                              linetype = "dashed", colour = "red")
      }
      p9 + ggplot2::geom_text(data = brushTable(),
                     label = paste("  ", rownames(brushTable()), "  "),
                     hjust = "inward", colour = "darkblue", size = 3) +
        ggplot2::geom_point(alpha = 0.3) +
        ggplot2::geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        ggplot2::labs(x = input$x1, y = input$y9) +
        ggplot2::ylim(min(yval), max(yval))
    })

    ######
    # Table of brushed cases
    brushTable <- shiny::reactive({
      shiny::brushedPoints(outData, input$brP)
    })

    output$brushed <- shiny::renderTable(
      brushTable()[,c(1,2,((ncol(brushTable())-ncol(model$model))+1):ncol(brushTable()))])

    ####
    # compare models

    # Data with brushed excluded
    trimmedData <- shiny::reactive({
      model$model[-shiny::brushedPoints(outData, input$brP)$id,]
    })


    # trimmed model
    tModel <- shiny::reactive({
      stats::update(model, data = trimmedData())
    })

    # Table to compare coefficients
    coefTab <- shiny::reactive({
      data.frame("Full data" = stats::coef(model),
                 "p-value.full" = summary(model)$coef[,4],
                 "Reduced data" = stats::coef(tModel()),
                 "p-value.red" = summary(tModel())$coef[,4],
                 "Difference" = stats::coef(tModel()) - stats::coef(model),
                 "Perc.Change" = (stats::coef(tModel()) - stats::coef(model)) * 100 / abs(stats::coef(model))
                 )
    })

    output$modelCompare <- shiny::renderTable({
      if(nrow(brushTable()) == 0){
        return()
      }
      coefTab()
    }, rownames = TRUE)

    output$full <- shiny::renderPrint({
      summary(model)
    })

    output$reduced <- shiny::renderPrint({
      if(nrow(brushTable()) == 0){
        return()
      }
      summary(tModel())
    })

  }

  # Start interactive session
  shiny::shinyApp(ui, server)

}

# DiagPlotLM(model)
