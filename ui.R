
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinyAce)
# library(shinyIncubator)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("CharteR - Quick Data Manipulation and Visualization in R"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    radioButtons( inputId="loadFilterOrChart_RB",label=" ",
                  choices=c("Load Data" = "loadData_RB" , 
                            "Manipulate Data" = "filterAgg_RB" ,
                            "Visualize Data" = "createViz_RB" ,
                            "Save Results in R format" = "saveResults_RB"
                  ) 
    ) ,
    
    # actionButton(inputId="revertOrigData_AB",label="Delete all variables and start from original data.") ,
    
    conditionalPanel(
      condition = "input.loadFilterOrChart_RB == 'createViz_RB'",
      wellPanel( 
        helpText("Create a visualization based on any saved variables.") ,
        selectInput(inputId="vizOrigOrCurrent_SI", label="Choose a dataframe to Visualize:", 
                    choices = c("orig_input_data")
                    )
        
      #  radioButtons( inputId="vizOrigOrCurrent_RB",label=" ",
      #                choices=c("Plot from Original Data" = "useOrigData_RB" ,
      #                          "Plot from Current Results" = "useCurrentResults_RB" 
      #                          ) 
      #  ) 
      ) ,
      
      wellPanel( 
        numericInput(inputId="origX_TI", label="Column for X-axis values: (for Barchart and Histogram these are the values plotted)", value=1) ,
        numericInput(inputId="origY_TI", label="Column for Y-axis values: (for Barchart these are the categories; for Histogram this field is not used)", value=2) 
      ) ,
      
      wellPanel(
        helpText("Choose a chart type.") ,
        radioButtons( inputId="vizType_RB",label=" ",
                      choices=c("Histogram" = "vizHisto_RB" ,
                                "Bar Chart" = "vizBar_RB" , 
                                "Scatterplot" = "vizScatter_RB" 
                                #   "Line Graph" = "vizLine_RB"
                      ) 
        ) 
  #      textInput(inputId="chartTitle_TI", label="Chart Title", value="") ,
  #      textInput(inputId="chartXlabel_TI", label="X-axis Label", value="") ,
  #      textInput(inputId="chartYlabel_TI", label="Y-Axis Label", value="")
      ) ,
      
      actionButton(inputId="createViz_AB",label="Create Chart") ,
      helpText("Click 'VISUALIZATION' tab on right to view chart") ,
      
      wellPanel(
        textInput(inputId="chartTitle_TI", label="Chart Title", value="") ,
        textInput(inputId="chartXlabel_TI", label="X-axis Label", value="") ,
        textInput(inputId="chartYlabel_TI", label="Y-Axis Label", value="")
      )
      
      
    ),
    
    conditionalPanel(
      condition = "input.loadFilterOrChart_RB == 'saveResults_RB'",
      wellPanel( 
        helpText("Enter Path & Filename of where to save your data (will be saved as an R list-of-lists)") ,
        tags$textarea(id="fileSavePath_TI", rows=5, cols=150, "~/myData_Rformat") ,
        actionButton(inputId="saveResults_AB",label="Save") ,
        helpText("When loading the file later: Data will be restored into variable names 'orig_input_data', 'savedRes' (a list containing all saved variables), Additionally, the operations performed on the original data to get these results are restored into the varialbe 'operationsStack' ") 
      )
    ),
    
    conditionalPanel(
      condition = "input.loadFilterOrChart_RB == 'loadData_RB'",
      wellPanel( 
        helpText("Path to text file to load:") ,
        # tags$textarea(id="pathToData_TI", rows=5, cols=150, "~/Dropbox/HistoricalData/Kaggle/NCAA/tourney_results_data.csv") ,
        aceEditor("pathToData_TI", mode="r", theme="idle_fingers", height=50, value="~/Dropbox/HistoricalData/Kaggle/NCAA/tourney_results_data.csv" ) ,
        checkboxInput(inputId="inputDataHasHeaders_CB",label="Data file contains headers?", value=FALSE) ,
        helpText("Path to column headers"),
        # tags$textarea(id="pathToDataColHeaders_TI", rows=5, cols=150, "~/Dropbox/HistoricalData/Kaggle/NCAA/tourney_results_headers.csv") ,  
        aceEditor("pathToDataColHeaders_TI", mode="r", theme="idle_fingers", height=50, value="~/Dropbox/HistoricalData/Kaggle/NCAA/tourney_results_headers.csv" ) ,
        
        textInput(inputId="fieldSepChar_TI", label="Field Separator:", value=",") ,
        actionButton(inputId="loadData_AB",label="Load Data") 
      )
    ),
    
    conditionalPanel(
      condition = "input.loadFilterOrChart_RB == 'filterAgg_RB'" ,
      
      wellPanel(
          condition = "input.doOper_SI != 'CUSTOM...'" ,
          selectInput(inputId="dfToUse_SI", label="Select a variable to view details (tab to the right) :", 
                      choices = c("orig_input_data")
                    )
        ),
      
      wellPanel(
        checkboxInput(inputId="revertOrigData_CB",label="Delete current work & apply an operation to the original data", value=FALSE) ,
        selectInput(inputId="doOper_SI", label="Choose an operation:", 
                    choices = c("Select Specific Rows from Dataframe", "Aggregate Rows By Category","_____________", "Create Custom Aggregation Table" 
                    )
        ) ,
        
        
        
        conditionalPanel(
          condition = "input.doOper_SI == 'CUSTOM_Aggregation_Table...'" ,
          helpText("Enter an R expression that returns a dataframe of aggregated values. All previously saved dataframes are available as variables, as well as 'orig_input_data' and 'previous_res'.") ,
          tags$textarea(id="customAggTable_TI", rows=12, cols=80, "function() { return( 
                        map( orig_input_data, 
                        function(x) { 
                        c( x[[1]][1] , x[[1]][2] ) 
                        } ) 
          ) }")  
        ) ,
        
        conditionalPanel(
          condition = "input.doOper_SI == 'Select Specific Rows from Dataframe'" ,
          helpText("Enter R function code to return dataframe rows based on condition(s):") ,
          # tags$textarea(id="filterCondition_TI", rows=6, cols=120, "orig_input_data[ orig_input_data[2] > 10 , ]") ,
          aceEditor("filterCondition_TI", mode="r", theme="idle_fingers", height=100, value="orig_input_data[ orig_input_data[2] > 150 , ]" ) , 
          helpText("_____________________") ,
          helpText("EXAMPLES: (You can reference any previously saved variables in your filter expression)") ,
          helpText("1) Get all rows where column 2 values are > 150:") ,
          helpText("orig_input_data[ orig_input_data[2] > 150 , ]  ") ,
          helpText("2) same as above except using column 'name' instead of column #:") ,
          helpText("orig_input_data[ orig_input_data['columnName2'] > 150 , ]  ") ,
          helpText("3) Get all rows where column 2 values are > 10 & column 4 values are > 50:") ,
          helpText("orig_input_data[ orig_input_data[2] > 10 & orig_input_data[4] > 50 , ] ") ,
          helpText("_____________________")
          
        ) ,
        
        actionButton(inputId="runOper_AB",label="Run Operation") ,
        textInput(inputId="newStep_TI", label="Save results to a new R variable?", value="previous_result"),
        helpText("(Otherwise result will just overwrite 'previous_result')")
    )  
) 

  ),

# Show a plot of the generated distribution
mainPanel(

#  wellPanel(
#    h4("Original Imported Data") ,
#    dataTableOutput(outputId="tab_OrigData_dt") 
#  ) ,
  
  wellPanel(
    h4("Dataframe Viewer") ,
    tabsetPanel(id="outputTabs_top",
               tabPanel(title="Original Data", dataTableOutput(outputId="tab_ViewOrigData_dt"), value=5) ,
               tabPanel(title="Current Results", dataTableOutput(outputId="tab_ViewCurrData_dt"), value=6) ,
               tabPanel(title="Selected Variable (from left)", dataTableOutput(outputId="tab_ViewVariable_dt"), value=7)
    )
  ) ,
  
  wellPanel( 
    # h4("Current Data After the Applied Operations, Etc.") ,
    h4("Results Visualizer") ,
    tabsetPanel(id="outputTabs",
                # tabPanel(title="Current RDD Table", dataTableOutput(outputId="tab_OrigData_dt"), value=5) ,
                tabPanel(title="Current Results (head)", verbatimTextOutput(outputId="tab_currResults"), value=1) ,
    #            tabPanel(title="Previous Operations Applied", verbatimTextOutput(outputId = "tab_operStack"), value=2) ,
    #            tabPanel(title="Original Data (head)", verbatimTextOutput(outputId = "tab_OrigData"), value=3) ,
                # tabPanel(title="Current RDD Sample", plotOutput(outputId="tab_RDD", width="100%", height="700px"),value=1) , 
                tabPanel(title="VISUALIZATION", plotOutput(outputId="tab_viz",width="100%",height="500px"), value=4, inputId="selectVizTab") 
                #tabPanel(title="Export", tableOutput("table_data"),value=3) ,
                #tabPanel(title="Underlying Data", verbatimTextOutput(outputId="table_data"),value=4)
    ) 
  ) ,
  
  wellPanel( 
    # h4("Current Data After the Applied Operations, Etc.") ,
    tabsetPanel(id="outputTabs_hist",
                tabPanel(title="Previous Operations Applied", verbatimTextOutput(outputId = "tab_operStack"), value=2)
    ) 
  )
  
  
)
) ) 
