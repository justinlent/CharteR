
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#


# sc <- sparkR.init(master="local[8]", appName="SparkTest", sparkHome="/Users/jlent/spark-0.9-devprev")
# orig_input_data <- map( textFile(sc, "/Users/jlent/Dropbox/HistoricalData/Kaggle/NCAA/tourney_results_data.csv") ,
#                         FUN=function(x){x<-strsplit(x=x,split=",")}
#                )

library(ggplot2)
library(RColorBrewer)
library(shiny)
library(shinyAce)
# library(shinyIncubator)


# --------------------------------------
# Chart plotting functiona
plotHistogram<-function( values, barColor="Blues", barColorDarkness=3, chartTitle=NULL, chartTitleSize=16, 
                         showMeanLine=FALSE, showStDevLines=FALSE, 
                         xAxisLabel=NULL, yAxisLabel="Count", 
                         showHorizontalBars=FALSE,
                         useDefaultName=FALSE) {
  #below is a pallette from library(RColorBrewer)
  mypalette<-brewer.pal(7, barColor)
  
  DF<-as.data.frame(values)
  colnames(DF)<-"x"
  localEnv <- environment()
  xAxisLabelStr <- NULL
  yAxisLabelStr <- yAxisLabel
  
  if( useDefaultName ){
    xAxisLabelStr <-  names(values)
  } else {
    if( !is.null(xAxisLabel) ) {
      xAxisLabelStr <- xAxisLabel
    }  
  }
  
  
  ggPlotFormula<-ggplot(data=DF, aes(x=x)) + 
    scale_color_brewer(type="seq") +
    geom_histogram(fill=paste(mypalette[barColorDarkness]),colour="grey30") + ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) +  
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
    theme(plot.title=element_text(size=chartTitleSize))
  
  if( showHorizontalBars ){
    ggPlotFormula<-ggPlotFormula + coord_flip()
  }
  if( showMeanLine ){
    tempMean=mean(values)
    ggPlotFormula<-ggPlotFormula +
      geom_vline(xintercept = tempMean, colour=paste(mypalette[5]), linetype = "dashed", size=1.5)
    if( showHorizontalBars ){
      ggPlotFormula<-ggPlotFormula + 
        annotate(geom="text", x=tempMean, y=0.0, label="avg", vjust=1.5, hjust=1.05, size=3.5, color="grey40" )        
    } else {
      ggPlotFormula<-ggPlotFormula + 
        annotate(geom="text", x=tempMean, y=0.0, label="avg", vjust=1.5, hjust=-0.1, size=3.5, color="grey40" )        
    } 
  }
  if( showStDevLines ){
    tempMean<-mean(values)
    if( class(values)[1] == "xts" || class(values)[1] == "zoo" ){
      tempStdDev<-apply(values,2,sd)  
    } else {
      tempStdDev<-sd(values)
    }
    if( showHorizontalBars ){
      ggPlotFormula<-ggPlotFormula + 
        geom_vline(xintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5 ) +
        annotate(geom="text", x=tempMean-tempStdDev, y=0.0, label="-1sd", vjust=1.5, hjust=1.2, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+tempStdDev, y=0.0, label="+1sd", vjust=1.5, hjust=1.2, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean-2*tempStdDev, y=0.0, label="-2sd", vjust=1.5, hjust=1.2, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+2*tempStdDev, y=0.0, label="+2sd", vjust=1.5, hjust=1.2, size=4, color="grey40" )
    } else {
      ggPlotFormula<-ggPlotFormula +
        geom_vline(xintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5 ) +
        annotate(geom="text", x=tempMean-tempStdDev, y=0.0, label="-1sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+tempStdDev, y=0.0, label="+1sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean-2*tempStdDev, y=0.0, label="-2sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+2*tempStdDev, y=0.0, label="+2sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" )
    }
  }
  return(ggPlotFormula)
}

plotBarChart<-function( values, categoryNames, barColor="Blues", barColorDarkness=4, 
                        chartTitle=NULL, chartTitleSize=18, xAxisLabel=NULL, yAxisLabel="Values", 
                        showMeanLine=FALSE, showStDevLines=FALSE, showHorizontalBars=FALSE, sortAsc=FALSE, sortDesc=FALSE ) {
  #below is a pallette from library(RColorBrewer)
  mypalette<-brewer.pal(7, barColor)
  
  DF<-data.frame(xVal=values, categoryNames=categoryNames)
  localEnv <- environment()
  yAxisLabelStr<-yAxisLabel
  if( !sortAsc && !sortDesc ){
    ggPlotFormula<-ggplot(data=DF, aes(x=categoryNames, y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE ))
  } else {
    if( sortAsc ){
      ggPlotFormula<-ggplot(data=DF, aes(x=reorder(categoryNames,xVal), y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE )) 
    } else {
      ggPlotFormula<-ggplot(data=DF, aes(x=reorder(categoryNames,-xVal), y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE ))
    } 
  }
  #  ggPlotFormula<-ggplot(data=DF, aes(x=reorder(categoryNames,-xVal), y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE )) + 
  ggPlotFormula<-ggPlotFormula +
    geom_bar(fill=paste(mypalette[barColorDarkness]),colour="grey30",stat="identity") + ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) +  
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
    theme(plot.title=element_text(size=chartTitleSize))
  
  if( showHorizontalBars ){
    ggPlotFormula<-ggPlotFormula + coord_flip()
  }
  
  if( showMeanLine ){
    tempMean<-mean(values)
    minXcoord<-min(values)
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = tempMean, colour=paste(mypalette[barColorDarkness+2]), linetype = "dashed", size=1.0, alpha=1.0) +
      annotate(geom="text", x=minXcoord, y=tempMean, label="avg", vjust=-0.5, hjust=-0.1, size=5, color="grey20" )  
  }
  
  if( showStDevLines) {
    
    tempMean<-mean(values)
    minXcoord<-min(values)
    if( class(values)[1] == "xts" || class(values)[1] == "zoo" ){
      tempStdDev=apply(values,2,sd)  
    } else {
      tempStdDev=sd(values)
    }
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5 ) +
      annotate(geom="text", x=minXcoord, y=tempMean-tempStdDev, label="-1sd", vjust=1.5, hjust=-0.1, size=4, color="grey20" ) +
      annotate(geom="text", x=minXcoord, y=tempMean+tempStdDev, label="+1sd", vjust=1.5, hjust=-0.1, size=4, color="grey20" ) +
      annotate(geom="text", x=minXcoord, y=tempMean-2*tempStdDev, label="-2sd", vjust=1.5, hjust=-0.1, size=4, color="grey20" ) +
      annotate(geom="text", x=minXcoord, y=tempMean+2*tempStdDev, label="+2sd", vjust=1.5, hjust=-0.1, size=4, color="grey20" )
  }
  
  return(ggPlotFormula)
} 

plotScatterplot<-function( xValues, yValues, pointLabelNames=NULL, showPointLabels=FALSE, 
                           barColor="Blues", barColorDarkness=5, pointSize=4, 
                           chartTitle=NULL, chartTitleSize=16,  
                           xAxisLabel=NULL, yAxisLabel=NULL, 
                           showRegressionLine=FALSE, showConfidenceInterval=FALSE, 
                           showRegressionAdjRSQ=FALSE, labelPositionRSQ="upper-left", labelRSQfontSize=6, 
                           showXmeanLine=FALSE, showYmeanLine=FALSE, 
                           showXstDevLines=FALSE, showYstDevLines=FALSE, 
                           labelPositionRSQoverrideX=0, labelPositionRSQoverrideY=0,
                           pointColorValues=NULL,
                           lowShadingColor=NULL,
                           highShadingColor=NULL,
                           legendLabel=""
) {
  # parameter "barColor" can be 'Blues', 'Greens', 'Reds'
  
  # inputs "xValues", "yValues", and "pointLabelNames" should be vectors all of the same length
  if( is.null(pointLabelNames) && showPointLabels==TRUE ){
    print("You specified showPointLabels=TRUE but did not pass in a vector of point label names to the pointLabelNames parameter.")
    return()
  }
  
  #below is a pallette from library(RColorBrewer)
  mypalette<-brewer.pal(8, barColor)
  
  #DF<-as.data.frame(values)
  if( showPointLabels ) {
    DF<-data.frame(x=xValues, y=yValues, pointLabels=pointLabelNames)
    colnames(DF)<- c("x","y","pointLabels")
  } else {
    DF<-data.frame(x=xValues, y=yValues)
    colnames(DF)<- c("x","y")
  }
  
  if( ! is.null(pointColorValues) ){
    DF<-cbind( DF, pointColors=pointColorValues )
  } else {
    DF<-cbind( DF, pointColors=barColorDarkness )
  }
  
  localEnv <- environment()
  
  yAxisLabelStr<-yAxisLabel
  ggPlotFormula <- NULL
  if( is.null(pointColorValues) ) {
    ggPlotFormula <- ggplot(data=DF, aes(x=x, y=y ) )
  } else {
    ggPlotFormula <- ggplot(data=DF, aes(x=x, y=y, color=pointColors))
  }
  ggPlotFormula <- ggPlotFormula + 
    # scale_color_brewer(type="seq") +
    # geom_point(show_guide=FALSE, shape=19, size=pointSize) + 
    #geom_point(show_guide=FALSE, color=paste(mypalette[barColorDarkness]), shape=19, size=pointSize) + 
    # geom_point(color=pointColorValues, show_guide=FALSE, shape=19, size=pointSize) + 
    # scale_color_gradient(colours=rainbow(7))
    ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) + 
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
    #    geom_text(data=DF, aes(label=pointLabels), vjust=-0.8) +
    theme(plot.title=element_text(size=chartTitleSize)) 
  
  if( is.null(pointColorValues) ) {
    ggPlotFormula <- ggPlotFormula +
      geom_point(show_guide=FALSE, color=paste(mypalette[barColorDarkness]), shape=19, size=pointSize)
  } else {
    ggPlotFormula <- ggPlotFormula +
      geom_point(show_guide=FALSE, shape=19, size=pointSize) +
      labs(colour=legendLabel) + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="bottom")
    
    if( !is.null(lowShadingColor) & !is.null(highShadingColor) ) {
      ggPlotFormula <- ggPlotFormula + 
        scale_color_gradient(low=lowShadingColor, high=highShadingColor)
    }
  }
  
  if( showPointLabels ){
    ggPlotFormula<-ggPlotFormula +
      geom_text(data=DF, aes(label=pointLabels), vjust=-0.8)
  }
  if( showRegressionLine ){
    if( showConfidenceInterval ){
      ggPlotFormula<-ggPlotFormula +
        geom_smooth(color="black", linetype="dashed", fill="pink", alpha=0.25, method=lm, show_guide=FALSE)
    } else {
      ggPlotFormula<-ggPlotFormula +
        geom_smooth(color="black", linetype="dashed", method=lm, se=FALSE, show_guide=FALSE)
    }
  }
  if( showRegressionAdjRSQ ) {
    tempLM<-lm(DF[,2] ~ DF[,1])
    tempAdjRSQ<-summary(tempLM)$adj.r.squared
    tempAdjRSQ<-round(tempAdjRSQ,2)
    
    if( labelPositionRSQ=="lower-left" ){
      coordStartX<-min(DF[,1])
      coordStartY<-min(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =", tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=0.0+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    if( labelPositionRSQ=="lower-right" ){
      coordStartX<-max(DF[,1])
      coordStartY<-min(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =",tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=1.0+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    if( labelPositionRSQ=="upper-right" ){
      coordStartX<-max(DF[,1])
      coordStartY<-max(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =",tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=1.0+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    if( labelPositionRSQ=="upper-left" ){
      coordStartX<-min(DF[,1])
      coordStartY<-max(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =",tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=-0.5+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    
  }
  
  if( showXmeanLine ){
    tempMean=mean(xValues)
    minYcoord<-min(DF[,2])
    ggPlotFormula<-ggPlotFormula +
      geom_vline(xintercept = tempMean, colour=paste(mypalette[5]), linetype = "dashed", size=1.0, alpha=0.3) +
      annotate(geom="text", x=tempMean, y=minYcoord, label="avg(X)", angle=90, vjust=-1.0, hjust=0.2, size=4, color="grey40" )  
  }
  if( showYmeanLine ){
    tempMean=mean(yValues)
    minXcoord<-min(DF[,1])
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = tempMean, colour=paste(mypalette[5]), linetype = "dashed", size=1.0, alpha=0.3) +
      annotate(geom="text", x=minXcoord, y=tempMean, label="avg(Y)", vjust=-1.0, hjust=0.1, size=4, color="grey40" )  
  }
  
  if( showXstDevLines ){
    tempMean=mean(xValues)
    tempStdDev=sd(xValues)
    ggPlotFormula<-ggPlotFormula +
      geom_vline(xintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5, alpha=0.3 )
  }
  if( showYstDevLines ){
    #     tempMean=mean(yValues)
    tempStdDev=sd(yValues)
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5, alpha=0.3 )
  }
  return(ggPlotFormula)
}

# --------------------------------------

readInputData <- function( pathStr, hasHeaders=FALSE, fieldSep="," ) {
  # inputData <- read.csv( file=pathStr, header=hasHeaders, sep=fieldSep )
  inputData <- read.table( file=pathStr, header=hasHeaders, sep=fieldSep )
  
  return( inputData )
}

getDataColumnHeaders <- function(pathStr, fieldSep=",") {
  # dataHeaders <- read.csv(file="/Users/jlent/Dropbox/HistoricalData/Kaggle/NCAA/tourney_results_headers.csv",header=FALSE)
  dataHeaders <- read.csv( file=pathStr,header=FALSE, sep=fieldSep )
  dataHeaders <- c(t(dataHeaders))
  
  return( dataHeaders )  
}

orig_input_data <<- NULL

operationsStack <<- list()
savedRes <<- list()
previous_result <<- NULL
sampleOrigDF <<- list()
sampleCurrDF <<- data.frame()
currStackVal <<- 0

dataHeaders <<- NULL
prevSave <<- 0

prev_revertOrigData_AB <<- 0

shinyServer(function(input, output, session) {
  
  # Generates the data shown in the "Current Sample" tab 
  output$tab_currResults <- renderPrint( {
    if( input$runOper_AB > 0 ) {
      isolate( {
        
        if( input$revertOrigData_CB ) {
          previous_result <<- orig_input_data
          operationsStack <<- list()
          savedRes <<- list()
          savedRes[[ "orig_input_data" ]] <<- orig_input_data
          prev_revertOrigData_AB <<- input$revertOrigData_AB 
          currStackVal <<- 0
          updateCheckboxInput(session, "revertOrigData_CB", value=FALSE)
        } 
        
        ##### TO-DO -- might still need to update previous_result assignment below to take the user-specified variable
        if( input$doOper_SI == "Select Specific Rows from Dataframe" ) {
          expressionStr <- paste("previous_result <<- " , input$filterCondition_TI, sep="")
          eval( expr=parse(text=expressionStr) )
        }
        
        currStackVal <<- currStackVal + 1
        operationsStack[[ currStackVal ]] <<- paste("operation #", currStackVal, " (", input$doOper_SI, "): ", input$newStep_TI, " <<-", strsplit(expressionStr,"<<-")[[1]][2], sep="")
        savedRes[[ input$newStep_TI ]] <<- previous_result
        savedRes[[ "previous_result" ]] <<- previous_result
        
        sampleCurrDF <<- head(previous_result, n=10)
        
        updateSelectInput( session=session,inputId="dfToUse_SI",label="Select a variable to view details (tab to the right) :", choices=names(savedRes) )
        updateSelectInput( session=session,inputId="vizOrigOrCurrent_SI",label="Choose a dataframe to Visualize:", choices=names(savedRes) )
        updateTextInput(session=session,inputId="newStep_TI",label="Save results to a new R variable?",value="previous_result")
        
        return( print(sampleCurrDF) )
        
        } )
      
    }
    
    # isolate( {} )
    # if( input$loadData_AB )
      
    if( input$loadData_AB == 1 ) {
      if( is.null(orig_input_data) ) {
        orig_input_data <<- readInputData( pathStr=input$pathToData_TI, hasHeaders=input$inputDataHasHeaders_CB, fieldSep=input$fieldSepChar_TI )
        # print( names(orig_input_data) )
        if( input$inputDataHasHeaders_CB ) {
          dataHeaders <<- names(orig_input_data)
        } else {
          # if( is.null(input$pathToDataColHeaders_TI) || length(input$pathToDataColHeaders_TI) < 2 ) {
          if( str_length(input$pathToDataColHeaders_TI) < 2 ) {
            # print(str_length(input$pathToDataColHeaders_TI))
            dataHeaders <<- names(orig_input_data)
          } else {
            dataHeaders <<- getDataColumnHeaders(pathStr=input$pathToDataColHeaders_TI, fieldSep=input$fieldSepChar_TI)
            names(orig_input_data) <<- dataHeaders
          }
        }

        previous_result <<- orig_input_data
        savedRes[[ "orig_input_data" ]] <<- orig_input_data
        savedRes[[ "previous_result" ]] <<- previous_result
        
        sampleOrigDF <<- head(orig_input_data, n=10)
        sampleCurrDF <<- sampleOrigDF
        
        return( print(sampleCurrDF) ) 
      }       
    }
    
  } )
  
  output$tab_OrigData_dt <- renderDataTable( {
    if( input$loadData_AB == 1 ) {
      # return( sampleOrigDF )
      return( orig_input_data )
    } else {
      if( input$runOper_AB >= 1 ) {
        # return( sampleCurrDF )
        return( orig_input_data )
      } else { 
        return( data.frame() )
      }
    }
  } , options = list(iDisplayLength=5, aLengthMenu=c(5,10,20,50,100)) 
  )
  
  output$tab_ViewOrigData_dt <- renderDataTable( {
    if( input$loadData_AB == 1 ) {
      # return( sampleOrigDF )
      return( orig_input_data )
    } else {
      if( input$runOper_AB >= 1 ) {
        # return( sampleCurrDF )
        return( orig_input_data )
      } else { 
        return( data.frame() )
      }
    }
  } , options = list(iDisplayLength=5, aLengthMenu=c(5,10,20,50,100)) 
  )
  
  output$tab_ViewCurrData_dt <- renderDataTable( {
    if( input$runOper_AB >= 1 ) {
      return( previous_result )
    } else { 
      return( data.frame() )
    }
  } , options = list(iDisplayLength=5, aLengthMenu=c(5,10,20,50,100)) 
  )
  
  output$tab_ViewVariable_dt <- renderDataTable( {
  
    return( savedRes[[ input$dfToUse_SI ]] )
  
  } , options = list(iDisplayLength=5, aLengthMenu=c(5,10,20,50,100)) 
  )
  
  observe( {
    if( input$saveResults_AB > prevSave ){
      save(savedRes,orig_input_data, operationsStack, file=input$fileSavePath_TI)
      # updateCheckboxInput(session, "collectAndSave_CB", value=FALSE)
      prevSave <<- prevSave + 1
    }
  })
  
  output$tab_viz <- renderPlot( {
    if( input$createViz_AB > 0 ) {
      
      isolate( {
        
        tempDF <- data.frame()
    #    if( input$vizOrigOrCurrent_RB == "useOrigData_RB") {
    #      tempDF <- orig_input_data
    #    } else {
    #      tempDF <- previous_result
    #    }
        tempDF <- savedRes[[ input$vizOrigOrCurrent_SI ]]
        
    # updateTabsetPanel( session, "outputTabs", selected="selectVizTab" )
        
        if( input$vizType_RB == "vizHisto_RB") {
          return( print( plotHistogram( values=as.numeric(tempDF[,input$origX_TI]), showMeanLine=TRUE, showStDevLines=TRUE, chartTitle=input$chartTitle_TI, xAxisLabel=input$chartXlabel_TI, yAxisLabel=input$chartYlabel_TI ) ) )      
        }
        if( input$vizType_RB == "vizBar_RB") {
          return( print( plotBarChart( values=as.numeric(tempDF[,input$origY_TI]), categoryNames=tempDF[,input$origX_TI], chartTitle=input$chartTitle_TI, xAxisLabel=input$chartXlabel_TI, yAxisLabel=input$chartYlabel_TI ) ) )      
        }
        if( input$vizType_RB == "vizScatter_RB") {
          return( print( plotScatterplot(yValues=as.numeric(tempDF[,input$origY_TI]),xValues=tempDF[,input$origX_TI], showRegressionLine=TRUE, showRegressionAdjRSQ=TRUE, chartTitle=input$chartTitle_TI, xAxisLabel=input$chartXlabel_TI, yAxisLabel=input$chartYlabel_TI ) ) )    
        }
        
        
        
      } )
    }
  } )
  
  output$tab_OrigData <- renderPrint( {
    if( input$runOper_AB >= 0 ){
      return( print(sampleOrigDF) )
    }
  } )
  
  observe( {
    output$tab_operStack <- renderPrint( {
      if( input$runOper_AB > 0 | input$revertOrigData_CB ){
        return( print(operationsStack) )
      }
    } )
  } )
  
  # Generate the summary at bottom of webpage of the code used to create the current data
  output$summaryStatus <- renderPrint({
    if( input$runOper_AB > 0 ){
      
      
    } else {
      outputText <- "First load some data -- The area above will show a sample after the most recent operation, and the area below will show the original data for convenience."
      print(outputText)  
    }
    
  })
  
  
})
