#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(gridExtra)
library(tools)
library(mnormt)
library(readxl)
library(writexl)
library(tidyr)
library(car)
library(stringr)
library(clipr)
library(SuppDists)
library(e1071)

source("uiHeaderText.R") # headers to each tab

source("uiHelp.R")
source("uiHypothesis.R")
source("uiDesign.R")
source("uiEvidence.R")
source("uiExplore.R")
source("uiFiles.R")
source("uiLikelihood.R")

source("uiHypothesisDiagram.R")
source("uiPopulationDiagram.R")
source("uiMainGraph.R")
source("uiMainReport.R")

shinyUI(fluidPage(
    useShinyjs(),
    
    tags$script('
    pressedKeyCount = 0;
    $(document).on("keydown", function (e) {
       Shiny.onInputChange("pressedKey", pressedKeyCount++);
       Shiny.onInputChange("keypress", e.which);
    });
    $(document).on("keyup", function (e) {
       Shiny.onInputChange("releasededKey", pressedKeyCount++);
    });
    '),
    
    tags$head(
        tags$script(
            "$(document).on('shiny:inputchanged', function(event) {
          if (event.name != 'changed') {
            Shiny.setInputValue('changed', event.name);
          }
        });"
        )
    ),
    
    tags$head(
        tags$style(type = 'text/css', paste("#SampleReport         {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#DescriptiveReport    {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#InferentialReport    {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#ExpectedReport    {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#ExploreReport    {background-color: ", maincolours$graphC, ";}"))
    ),
    tags$head(
        tags$style(type = 'text/css', paste("#SampleGraph         {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#DescriptiveGraph    {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#InferentialGraph    {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#ExpectedGraph    {background-color: ", maincolours$graphC, ";}")),
        tags$style(type = 'text/css', paste("#ExploreGraph    {background-color: ", maincolours$graphC, ";}"))
    ),
    
    tags$style(type="text/css", ".shiny-file-input-progress { display: none }"),
    
    tags$head(
        tags$style(
            HTML(".shiny-notification {position:fixed;top: calc(20%);left: calc(55%);}")
        )
    ),
    
    # 
    # tags$style("#DataTabset { display:none; }"), 
    # 
    tags$head(
        tags$style("label{font-size: 8pt;}"),
        tags$style(HTML( # textInput
            ".form-control {font-size: 8pt; height:20px; padding:0px 0px;}"
        )),
        tags$style(HTML( # selectInput
            ".selectize-input {font-size: 8pt; height:20px; padding:0px 0px; margin-right:0px; margin-top:-20px;margin-bottom:-5px; min-height:10px;}",
            ".selectize-dropdown { font-size: 8pt;}"
        )),
        tags$style(HTML( # helpText
            ".help-block {height:20px; padding:0px 0px; margin-top:20px; margin-bottom:-3px; min-height:10px;}"
            )),
        tags$style(HTML( # slider bar
            ".irs {margin:0px; margin-bottom:-15px; padding:0px;padding-bottom:-15px; height:30px;}",
            ".irs-with-grid {height:50px;}",
            ".irs-bar {height:6.5px; top:15.5px;}",
            ".irs-bar-edge {border-color: transparent; background-color: transparent;}",
            ".irs-grid-text {font-size:4pt;}",
            ".irs-max {font-size: 5pt;}",
            ".irs-min {font-size: 5pt;}",
            ".irs-single {font-size: 5pt;}",
            ".irs-slider {width: 10px; height: 10px; top:14px;padding:0px;}",
            ".irs-line {top: 15px;}",
            ".irs-grid {height: 21px; top: 23px}"
        )),
        tags$style(HTML( # specific slider bar
            "#r .irs-bar {background-color: transparent; border-color: transparent; }"
        )),
        tags$style(HTML( # action button
            ".col-sm-3 button {font-size:8pt;font-weight:Bold;color:white; background-color: #005886;height:20px;padding-top:0px;padding-bottom:0px;padding-left:4px;padding-right:4px;margin-bottom:4px;margin-right:12px;margin-top:4px;margin-left:0px}"
        )),
        tags$style(HTML( # tab panels
            ".tabbable > .nav > li > a {font-weight: normal; font-size: 8pt; padding:2px; margin:1px; color:#222222; background-color:#dddddd}",
            ".tabbable > .nav > .active > a {font-weight: bold; color:black;  background-color:white; }",
            ".nav-tabs {font-size: 8pt; padding:0px; margin-bottom:0px;} ",
        )),
        tags$style(HTML( # well panels
                ".well {padding:8px; margin:0px;margin-bottom:8px;margin-0px:0px;margin-right:0px;} ",
        )),
        tags$style(HTML(paste(".help-block b {color:", maincolours$panelC,  "!important;margin:0px;padding:0px;margin-bottom:8px;font-size:12pt; font-weight:bold;}"))),
        tags$style(HTML(paste(".help-block a {color:", maincolours$panelC,  "!important;margin:0px;padding:0px;margin-bottom:8px;font-size:8pt; font-weight:normal;font-style: italic;}"))),
        tags$style(HTML(".btn-file {padding:1px; margin: 0px; font-size:8pt; }"
        )),
        tags$style(HTML(
            ".checkbox {line-height: 10px;margin:0px;padding:0px;}"
        )),
    ),
    tags$head( # input tables
        tags$style(type="text/css",".table label{ display: table-cell; text-align: center;vertical-align: middle; } .myTable .form-group { display: table-row;}")
    ),

    # Application title
    # titlePanel("CrawStats"),
    
    # basic controls
    sidebarLayout(
                 sidebarPanel(
                     style = paste("background: ",maincolours$panelC), 
                     verticalLayout(
                         HelpTab
                         # Hypothesis panel                
                         ,HypothesisTab
                         # Design panel
                         ,DesignTab
                         # Evidence panel
                         ,EvidenceTab
                         # Explore panel
                         ,ExploreTab
                         # Likelihood panel
                         # ,LikelihoodTab
                         # Files panel
                         ,FilesTab
                     ),
                     width = 3
                 ),
                 # 
                 # results`
                 mainPanel(
                     style = paste("background: ",maincolours$windowC), 
                     column(width=4,
                            HypothesisDiagram,
                            PopulationDiagram
                     ),
                     column(width=8,
                            MainGraphs,
                            MainReports
                     ),
                     width = 9
                 ),
        ),
        setBackgroundColor(maincolours$windowC)
    )
)
