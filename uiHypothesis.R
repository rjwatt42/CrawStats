HypothesisTab <-
  
  wellPanel(id="HypothesisTabset",
    style = paste("background: ",panelcolours$hypothesisC), 
    # h5("Hypothesis"),
    fluidRow(headerText("Build a hypothesis: variables & effect-size")),
    tabsetPanel(id="Hypothesis",
                # Hypothesis tab
                tabPanel("Hypothesis:",
                         style = paste("background: ",subpanelcolours$hypothesisC)
                ),
                
                # variables tab
                tabPanel("Variables",id="Variables",
                         style = paste("background: ",subpanelcolours$hypothesisC), 
                         wellPanel(
                           style = paste("background: ",subpanelcolours$hypothesisC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$td(width = "5%", tags$div(style = localStyle, "IV:")),
                                        tags$td(width = "45%", selectInput("IVchoice", label = NULL,
                                                                           choices=IV$name,
                                                                           selected=IV$name,
                                                                           selectize=FALSE
                                        )),
                                        tags$td(width = "25%", actionButton("editIV","Edit IV")),
                                        tags$td(width = "5%", tags$div(style = localStyle, " "))
                                      ),
                                      tags$tr(
                                        tags$td(width = "5%", tags$div(style = localStyle, "IV2:")),
                                        tags$td(width = "45%", selectInput("IV2choice", label = NULL,
                                                                           choices=IV2$name,
                                                                           selected=IV2$name,
                                                                           selectize=FALSE
                                        )),
                                        tags$td(width = "25%", 
                                                conditionalPanel(condition = "input.IV2choice != 'none'",
                                                actionButton("editIV2","Edit IV2")
                                                )),
                                        tags$td(width = "5%", tags$div(style = localStyle, " "))
                                      ),
                                      tags$tr(
                                        tags$td(width = "5%", tags$div(style = localStyle, "DV:")),
                                        tags$td(width = "45%", selectInput("DVchoice", label = NULL,
                                                                           choices=DV$name,
                                                                           selected=DV$name,
                                                                           selectize=FALSE
                                        )),
                                        tags$td(width = "25%", actionButton("editDV","Edit DV")),
                                        tags$td(width = "25%", actionButton("hypothesisApply","Apply"))
                                        )
                           ),
                           width="100%"
                         )
                ),
                
                # prediction tab
                tabPanel("Effects",id="Prediction",
                         style = paste("background: ",subpanelcolours$hypothesisC), 
                         wellPanel(
                           style = paste("background: ",subpanelcolours$hypothesisC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$td(width = "35%", tags$div(style = localStyle, "IV",HTML("&rarr;"),"DV :")),
                                        tags$td(width = "20%", numericInput("rIV", label = NULL,
                                                                            min = -1,
                                                                            max = 1,
                                                                            step = 0.01,
                                                                            value = effect$rIV
                                        )),
                                        tags$td(width = "45%", tags$div(style = localStyle, " "))
                                        
                                      ),
                                      tags$tr(id="IV2E2",
                                        tags$td(width = "35%", 
                                                conditionalPanel(condition="input.IV2choice != 'none'",
                                                                 tags$div(style = localStyle, "IV2",HTML("&rarr;"),"DV :")
                                                )),
                                        tags$td(width = "20%", 
                                                conditionalPanel(condition="input.IV2choice != 'none'",
                                                                 numericInput("rIV2", label = NULL,
                                                                            min = -1,
                                                                            max = 1,
                                                                            step = 0.01,
                                                                            value = effect$rIV2
                                        ))),
                                        tags$td(width = "45%", 
                                                tags$div(style = localStyle, " ")
                                                )
                                        
                                      ),
                                      tags$tr(id="IV2E3",
                                        tags$td(width = "35%", 
                                                conditionalPanel(condition="input.IV2choice != 'none'",
                                                                 tags$div(style = localStyle, "IV",HTML("&rarr;"),"IV2 :")
                                                )),
                                        tags$td(width = "20%", 
                                                conditionalPanel(condition="input.IV2choice != 'none'",
                                                                 numericInput("rIVIV2", label = NULL,
                                                                            min = -1,
                                                                            max = 1,
                                                                            step = 0.01,
                                                                            value = effect$rIVIV2
                                        ))),
                                        tags$td(width = "45%", tags$div(style = localStyle, " "))
                                        
                                      ),
                                      tags$tr(id="IV2E4",
                                        tags$td(width = "35%", 
                                                conditionalPanel(condition="input.IV2choice != 'none'",
                                                                 tags$div(style = localStyle, "IV*IV2",HTML("&rarr;"),"DV :")
                                                )),
                                        tags$td(width = "20%", 
                                                conditionalPanel(condition="input.IV2choice != 'none'",
                                                                 numericInput("rIVIV2DV", label = NULL,
                                                                            min = -1,
                                                                            max = 1,
                                                                            step = 0.01,
                                                                            value = effect$rIVIV2DV
                                        ))),
                                        tags$td(width = "45%", tags$div(style = localStyle, " "))
                                      )
                           ),
                           width="100%"
                         )
                )
                # help tab
                ,tabPanel("?",
                          style = paste("background: ",subpanelcolours$hypothesisC),
                          wellPanel(
                            style = paste("background: ",subpanelcolours$hypothesisC,";"),
                            tags$table(width = "100%",class="myTable",
                                       tags$tr(
                                         tags$div(style = helpStyle, 
                                                  tags$br(HTML("<b>"),"Variables:",HTML("</b>")),
                                                  tags$br(HTML('&emsp;'), '1. choose one or two IVs and a DV by name'),
                                                  tags$br(HTML('&emsp;'), '2. edit the variable name/type/details as desired'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), 'eg. mean, sd , skew, kurtosis'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), 'or no cases, case names, proportions'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), '(watch the Hypothesis diagram)'),
                                                  tags$br(HTML("<b>"),"Effects: ",HTML("</b>")),
                                                  tags$br(HTML('&emsp;'), '3. select effect size or sizes (for 2 IVs)'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), 'these are normalized and range from -1 to +1'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), '(watch the Population or Prediction diagram)')
                                         ),
                                       )
                            )
                          )
                )
    )
  )
