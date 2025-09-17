library(shiny)


library(multinma)
library(RColorBrewer)
library(reshape2)
library(ggpubr)
library(colourpicker)
library(colorspace)
library(shinycssloaders)
library(shinydashboard)
library(reactable)
library(robvis)
library("data.table")
library("meta")
library(ComplexHeatmap)
library(circlize)
library(plyr)
library(metamisc)
library("mada")
library("shinyFeedback")
library("shinyMatrix")
library(DT)
library(svglite)
library(slickR)
library(XML)
library("shinyLP")
library(colourpicker)
library(shinycssloaders)
library(shinyjs)
library(shinydisconnect)
library(metawho)



#extra
library(jsonlite)
library(ggplot2)
library(dplyr)
library(reshape2)
library(shinyWidgets)
library(shinyBS)
library(emayili)




ui <- fluidPage(
  useShinyjs(),  # 启用 shinyjs
  disconnectMessage(
    text = "An error occurred. Please refresh the page and try again.",
    refresh = "Refresh",
    background = "#FFFFFF",
    colour = "#444444",
    refreshColour = "#337AB7",
    overlayColour = "#000000",
    overlayOpacity = 0.6,
    width = 600,
    top = 200,
    size = 22,
    css = ""
  ),
  tags$head(
    #主题设置----
    tags$script(HTML("
      $(document).ready(function() {
        $('.navbar-brand').on('click', function() {
          // Trigger click on the Home tab
          $('a[data-value=\"Home\"]').click();
        });
      });
      
    ")),
    tags$style(HTML("
    
    
    .blue-stripe-box {
        border-left: 5px solid #4682B4;
        background-color: #f8f9fa;
        padding: 10px;
        border-radius: 5px;
        color: #000000;
        margin-bottom: 10px;
        font-size: 16px;
      }
    .sunrise-effect {
        font-size: 36px;
        color: #000000;
        position: relative;
        animation: sunrise 4s ease-out;
        text-align: center;
      }
      @keyframes sunrise {
        from {
          opacity: 0;
          bottom: -50px;
        }
        to {
          opacity: 1;
          bottom: 0;
        }
      }
    .navbar-brand img {
        margin-top: 10px;
        transition: transform 0.6s ease-in-out;
      }
    .navbar-brand img:hover {
        transform: scale(1.1);
      }
    .nav > li > a:hover {
        background: linear-gradient(135deg, #ADD8E6, #4682B4);
        color: white !important;
        transition: background 0.5s;
      }
      
      .nav > li[class=active] > a {
        background: linear-gradient(135deg, #4682B4, #5F9EA0) !important;
        color: white !important;
        transition: background 0.5s;
      }
      .nav > li > a {
        border-top-left-radius: 15px;
        border-top-right-radius: 15px;
        margin-right: 5px;
        transition: background 0.5s;
      }
      .nav > li[class=active] > a {
        background: linear-gradient(135deg, #4682B4, #5F9EA0) !important;
        color: white !important;
      }
    .action-button:hover {
        background-color: #7FB3D5 !important; /* 鼠标悬停时的颜色 */
        transition: background-color 0.3s;
      }
      /* 在这里插入你的CSS内容 */
      /* navbar */
      .navbar {
          background-color: #ADD8E6; /* 天蓝色 */
          -webkit-transition: padding .3s;
          -moz-transition: padding .3s;
          transition: padding .3s;
          border: none;
      }
      .navbar .navbar-nav {
          float: right;
          padding-right: 100px;
          padding-top: 25px;
      }
      .navbar-default .navbar-nav>li>a {
          font-family: 'Open Sans', sans-serif;
          font-size: 15px;
          color: #000;
          text-transform: uppercase;
          background: #FFF;
          position: relative;
          display: block;
          padding: 10px 15px;
      }
      .navbar-default .navbar-brand {
          font-size: 50px;
          color: #4682B4; /* 深一点的天蓝色 */
      }
      .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover {
          color: #ffffff;
          background-color: #5F9EA0; /* 暗一点的天蓝色 */
      }
      .navbar-default .navbar-nav>li>a:focus, .navbar-default .navbar-nav>li>a:hover {
          background-color: rgb(222, 222, 222);
      }
      .nav-tabs {
          padding-top: 50px;
      }
      /* body */
      body {
        font-family: 'Source Sans Pro', sans-serif;
        margin-bottom : 150px;
      }
      /* caixas */
      a {
          color: #5F9EA0; /* 暗一点的天蓝色 */
      }
      a:focus, a:hover {
          color: #5F9EA0; /* 暗一点的天蓝色 */
          text-decoration: none;
      }
      .nav-tabs>li>a {
          margin-right: 2px;
          line-height: 1.42857143;
          border: 1px solid #ffffff;
          background-color: #ADD8E6; /* 天蓝色 */
          color: white;
          border-radius: 4px 4px 0 0;
      }
      .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
          color: #fff;
          cursor: default;
          background-color: #4682B4; /* 深一点的天蓝色 */
          border: 1px solid #ddd;
          border-bottom-color: transparent;
      }
      /* footer */
      .footer {
          position: fixed;
          height: 120px;
          background-color: #ADD8E6; /* 天蓝色 */
          bottom: 0px;
          left: 0px;
          right: 0px;
          margin-bottom: 0px;
          text-align: center;
      }
      .img_footer {
          padding-top: 10px;
          padding-right: 100px;
          padding-left: 100px;
      }
      .imgContainerFooter {
          padding-top: 20px;
          float: right;
      }
      .imgContainerFooter img {
         height: 40px;
         padding: 5px 5px 5px 5px;
      }
      .logoContainerFooter {
         padding-top: 0px;
         float: left;
      }
      .logoContainerFooter img {
         height: 50px;
      }
      .h3, h3 {
          line-height: 0.9;
      }
      .h1, .h2, .h3, h1, h2 {
          line-height: 1.5;
      }
      .h4, h4 {
        line-height: 1.5;
        text-align: justify !important;
      }
      /* Sobre */
      .col2 {
          columns: 2 350px;         
          -webkit-columns: 2 350px; 
          -moz-columns: 2 350px;
          text-align: center !important;
      }
      .descricao {
          text-align: justify !important;
      } 
      .foto {
        width: 300px;
        height: 250px;
        margin: auto !important;
      }
      th {
          background: #6aae7a;
          color: #fff;
      }
      td {
          padding-left: 5px;
      }
      td, 
      th {
          vertical-align: top;
      }
      th {
          font-size: 12px;
          text-align: left;
      }
      .leaflet-container.leaflet-touch-drag.leaflet-touch-zoom {
          position: sticky !important;
      }
    "))
  ),
  
  
  navbarPage(
    title = div(
      a(href = "#shiny-tab-Home", 
        img(src = "onlinemeta_logo.png", height = "80px", alt = "Logo", style = "margin-left: 60px;"))
    ),
    ##Home----
    tabPanel("Home",
             hr(),
             #HTML("<h1 class='typing-effect'></center>WELCOME TO <b>ONLINEMETA V2.0</b></center></h1>"),
             #HTML("<h1><center>WELCOME TO <b>ONLINEMETA V2.0</b> </center></h1>"),
             HTML("<h1 class='sunrise-effect'>WELCOME TO <b>ONLINEMETA V1.1</b></h1>"),
             br(), 
             slickROutput("my_slick",width='60%',height='400px'),
             br(), br(), br(), br(),br(),hr(),
             column(width = 3, align = "center",
                    actionButton("analise_risk", 
                                 label = "Risk bias", 
                                 style = "background-color: #ADD8E6; width: 100%; height: 200px; color: white; border: none; font-size: 30px;",
                                 onclick = "$('a[data-value=\"Risk Bias\"]').tab('show');")
             ),
             
             column(width = 3, align = "center",
                    actionButton("analise_meta", 
                                 label = "Meta-analysis", 
                                 style = "background-color: #4682B4; width: 100%; height: 200px; color: white; border: none; font-size: 30px;",
                                 onclick = "$('a[data-value=\"Meta Analysis\"]').tab('show');")
             ),
             column(width = 3, align = "center",
                    actionButton("analise_networkmeta", 
                                 label = HTML("Network<br>Meta-analysis"), 
                                 style = "background-color: #5F9EA0; width: 100%; height: 200px; color: white; border: none; font-size: 30px;",
                                 onclick = "$('a[data-value=\"Network Meta Analysis\"]').tab('show');")
             ),
             column(width = 3, align = "center",
                    actionButton("help", 
                                 label = "Help", 
                                 style = "background-color: #3B5998; width: 100%; height: 200px; color: white; border: none; font-size: 30px;", 
                                 onclick = "$('a[data-value=\"Help\"]').tab('show');")
             ),
             column(width = 12,
                    br(), br(), br(), br(),
                    wellPanel(
                      HTML("<h1><b>OnlineMeta V1.1</b></h1>"),
                      HTML("<h4><b>OnlineMeta V1.1</b> is an online meta-analysis web tool that requires no code or software, with zero code implementation for R language meta-analysis.
                               </h4>"),
                      HTML("<h4>After uploading data, there is no need to use any code or download any tools to perform risk bias analysis, meta-analysis and subgroup analysis of 6 types of data, and network meta-analysis of 2 types of data.
                               </h4>"),
                      HTML("<h4>If you encounter any issues while using the product, please refer to the <b>Help</b> page for guidance. If the issue persists or you have additional feedback, you are welcome to contact us via the Comment Box at the bottom of the Help page or by emailing our developers at the address listed there.
                               </h4>"),
                      HTML("<h4>Alternatively, you may join our online feedback group for more immediate and convenient support: <b>QQ Group (ID: 981578710)</b>.
                               </h4>")
                    ),
                    wellPanel(
                      HTML("<h1><b>Info</b></h1>"),
                      panel_div("info", "Application Maintainers",
                                content = HTML('<p>Weiyuntian Dai: davidyt@i.smu.edu.cn</p>',
                                               '<p>Peng Luo: luopeng@smu.edu.cn</p>' ,
                                               '<p>Yonglin Yi: yiyonglin515@163.com</p>')
                      ),
                      panel_div("info", "Artical Publication",
                                content = HTML('<p>Dai W, Yi Y, Lin A, Zhou C, Zhang J, Wang S, Sun M, Luo P. Onlinemeta: A Web Server for Meta-Analysis Based on Shiny Framework. bioRxiv. 2022 Apr 14:2022-04.（doi: https://doi.org/10.1101/2022.04.13.488126v4）</p>'
                                               )
                      ),
                      panel_div("info", "Updates & News",
                                content = div(style = "max-height: 300px; overflow-y: auto; padding-right: 10px;",
                                  HTML("<p>Updates: 2025.1.22</p>",
                                       "<p>- Fix issues with the download functionality,</p>",
                                       "<p>- Add additional formatting guidelines,</p>",
                                       "<p>- Modify some errors. </p>",
                                               "<p><b>Major Updates: 2024.11.7</b></p>",
                                               "<p>- Update to ver1.1!</p>",
                                               "<p>- Beautify the user interface.</p>",
                                               "<p>- Add new ways to analyse survival data.</p>",
                                               "<p>- Add sensitivity analysis.</p>",
                                               "<p>- Add effect quantity comparison table.</p>",
                                               "<p>- Add email sending and receiving function.</p>",
                                               "<p>- Add heatmap to network analysis.</p>",
                                               "<p>- Add new user guide.</p>",
                                               "<p>- Modify some errors.</p>",
                                               
                                               "<p>Updates: 2023.8.10</p>",
                                               "<p>- Add network analysis,</p>",
                                               "<p>- Modify some errors. </p>",
                                               "<p>Updates: 2022.10.27</p>",
                                               "<p>- Modify error message. </p>",
                                               "<p>- Add the result of test for overall effect and 'Model' parameter in deft analysis.</p>",
                                               "<p>Updates: 2022.10.11</p>",
                                               "<p>- Add deft analysis.</p>",
                                               "<p>Updates: 2022.6.6</p>",
                                               "<p>- Add reset function.</p>",
                                               "<p>- Automatically canceling sample data when uploading local data.</p>",
                                               "<p>Updates: 2022.5.6</p>",
                                               "<p>- Add new functions, including trim-and-fill method, Egger's test, and Begg's test.</p>",
                                               "<p>- Add user guide.</p>"
                                               )
                                ))
                    ),
                    br(), 
                    br(), 
                    br(), 
                    br(), 
                    tags$div(
                      style = "width:360px; height:360px; margin:0 auto;",   # ← 想多大改这里
                      tags$script(
                        id  = "clstr_globe",
                        src = "//clustrmaps.com/globe.js?d=96753rzoiHpZa8fT6VZOwWv7TxUDMz9QYGj3xxkc014"
                      )
                    )
                    
                    
             )
    ),
    ##Risk Bias----
    tabPanel("Risk Bias",
             hr(),
             br(), br(),
             tabsetPanel(
               ###Histogram----
               tabPanel(title = "Histogram", 
                        value = "histogram",
                        fluidRow(
                          column(
                            width = 9,
                            hr(),
                            box(style = 'overflow-x: scroll',width=NULL,
                                # DT::DTOutput("head")%>% withSpinner(type=4),
                                withSpinner(type=4,DT::DTOutput("head")),
                                tags$head(
                                  tags$style(
                                    HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }
             "
                                    )
                                  )
                                )
                            ),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                box(width=NULL,style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                    verticalLayout(#cellWidths = c("70%","30%"),
                                      # tags$div(withSpinner(plotOutput('distPlot'))),
                                      tags$div(hidden(div(id="h",withSpinner(type =4,plotOutput('distPlot'))))),
                                      tags$div(fluidRow(downloadButton("downloadp1", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp1png", "Save PNG", icon = icon("download")))
                                               
                                      )
                                    )
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                actionButton("toggleAdvanced", "Data Format",
                                             style = "color: white; 
                       background-color:#3c8dbc; 
                       position: relative; 
                       Bottom: 4px;
                       height: 35px;
                       width: 110px;"
                                ),
                                # a(id = "toggleAdvanced", "Data Format",href = "#"), 
                                shinyjs::hidden(
                                  div(id = "advanced",
                                      strong("Please make sure all your files are in",
                                             span("' .csv '",style = "color:blue"),
                                             "format"),
                                      br(),
                                      h5("1.Rownames: Study, D1, D2 ,..., Overall, Weight"),
                                      p("- Study: Name of the study"),
                                      p("- D1, D2 ,... Is the content of the study quality assessment scale, like AHRQ. You can customize the name."),
                                      p("- Overall: Overall risk of the study. If is null,please fill this coloumn with '0'."),
                                      p("- Weight: The weight of this study. If is null,please fill this coloumn with '0'."),
                                      h5("2.The risk should be expressed as 'Low', 'Medium', 'Unclear', 'High'."),
                                      br()
                                  )
                                ),
                                hr(),
                                uiOutput('file1'),
                                actionBttn(
                                  inputId = "run", 
                                  label = "Plot", 
                                  style = "fill", 
                                  color = "success", 
                                  icon = icon("paper-plane"), 
                                  size = "sm"
                                ),
                                actionBttn(
                                  inputId = "reset", 
                                  label = "Reset File", 
                                  style = "fill", 
                                  color = "danger", 
                                  icon = icon("trash"), 
                                  size = "sm"
                                ),
                                tags$br(),
                                tags$br(),
                                downloadBttn(
                                  outputId = "downloadBtn", 
                                  label = "Download Example Data", 
                                  style = "fill", 
                                  color = "warning",
                                  size = "sm"
                                ),
                                tags$br(),
                                checkboxInput("exam","Example Data",TRUE),
                                
                              )
                            ),
                            hr(),
                            bsCollapse(
                              id = "collapse1", open = NULL,
                              bsCollapsePanel(
                                "Customize the Histogram", class = NULL , style = "info",
                                textInput("maintitle",
                                          span("Main Tile",
                                               style='font-family: Arial, Helvetica, sans-serif;',
                                               tags$a(
                                                 tags$i(class='fa fa-question-circle'),
                                                 href = "#",
                                                 onclick="alert('Title of the plot');return false;"
                                               )
                                               
                                          )
                                          
                                ),
                                selectizeInput('overall',
                                               span("Overall",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('An option to include a bar for overall risk-of-bias in the figure.');return false;"
                                                    )),
                                               choices =c("TRUE","FALSE"),
                                               selected = "TRUE"),
                                selectizeInput('weight',
                                               span("Weight",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('An option to specify whether weights should be used in the barplot.');return false;"
                                                    )),
                                               choices =c("TRUE","FALSE"),
                                               selected = "FALSE"),
                                #= input$overall,input$weighted,
                                
                                sliderInput("Msize", "Tile Fontsize of title",
                                            
                                            min = 0, max = 30, value = 15),
                                selectizeInput('colpal', 
                                               span("Histogram Color",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('Define colour scheme');return false;"
                                                    )),
                                               selected = "Pastel2",
                                               choices = rownames(brewer.pal.info[brewer.pal.info$category=="qual",])),
                                
                                sliderInput("plotheight",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth", 
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr()
                          )
                        )
               ),
               ##Heatmap----
               tabPanel(title = "Heatmap", 
                        value = "heatmap",
                        fluidRow(
                          column(
                            width = 9,
                            hr(),
                            box(width=NULL,
                                style = 'overflow-x: scroll',
                                withSpinner(type =4,DT::DTOutput("head_ris")),
                                # DT::DTOutput("head_ris"),
                                tags$head(
                                  tags$style(
                                    HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }
             "
                                    )
                                  )
                                )
                            ),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                box(style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                    width=NULL,
                                    verticalLayout(#cellWidths = c("70%","30%"),
                                      tags$div(hidden(div(id="h_ris",withSpinner(type =4,plotOutput('distPlot_heatmap_ris'))))),
                                      #tags$div( withSpinner(type =4,plotOutput('distPlot_heatmap_ris'))),
                                      #tags$div(tableOutput("table")),#zcz
                                      tags$div(fluidRow(downloadButton("downloadp2_ris", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp2_rispng", "Save PNG", icon = icon("download")))
                                               
                                      )
                                    )
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                actionButton("toggleAdvanced_ris", "Data Format",
                                             style = "color: white; 
                       background-color:#3c8dbc; 
                       position: relative; 
                       Bottom: 4px;
                       height: 35px;
                       width: 110px;"
                                ),
                                #a(id = "toggleAdvanced_ris", "Data Format", href = "#"),
                                shinyjs::hidden(
                                  div(id = "advanced_ris",
                                      strong("Please make sure all your files are in",
                                             span("' .csv '",style = "color:blue"),
                                             "format"),
                                      br(),
                                      p("1.Rownames: Study, D1, D2 ,..., Overall, Weight"),
                                      p("- Study: Name of the study"),
                                      p("- D1, D2 ,... Is the content of the study quality assessment scale, like AHRQ. You can customize the name."),
                                      p("- Overall: Overall risk of the study. If is null,please fill this coloumn with '0'."),
                                      p("- Weight: The weight of this study. If is null,please fill this coloumn with '0'."),
                                      p("2.The risk should be expressed as 'Low', 'Medium', 'Unclear', 'High'."),
                                      br()
                                      
                                  )
                                ),
                                hr(),
                                uiOutput('file1_ris'),
                                actionBttn(
                                  inputId = "run_ris", 
                                  label = "Plot", 
                                  style = "fill", 
                                  color = "success", 
                                  icon = icon("paper-plane"), 
                                  size = "sm"
                                ),
                                actionBttn(
                                  inputId = "reset_ris", 
                                  label = "Reset File", 
                                  style = "fill", 
                                  color = "danger", 
                                  icon = icon("trash"), 
                                  size = "sm"
                                ),
                                tags$br(),
                                tags$br(),
                                downloadBttn(
                                  outputId = "downloadBtn_ris", 
                                  label = "Download Example Data", 
                                  style = "fill", 
                                  color = "warning",
                                  size = "sm"
                                ),
                                tags$br(),
                                checkboxInput("exam_ris","Example Data",TRUE),
                                
                              )
                            ),
                            hr(),
                            bsCollapse(
                              id = "collapse_ris", open = NULL,
                              bsCollapsePanel(
                                "Customize the Heatmap", class = NULL , style = "info",
                                colourpicker::colourInput("gp_bar1",
                                                          span("Bar Color1",
                                                               style='font-family: Arial, Helvetica, sans-serif;',
                                                               tags$a(
                                                                 tags$i(class='fa fa-question-circle'),
                                                                 href = "#",
                                                                 onclick="alert('Color of the annotation above');return false;"
                                                               )),
                                                          "orange"),
                                colourpicker::colourInput("gp_bar2",
                                                          span("Bar Color2",
                                                               style='font-family: Arial, Helvetica, sans-serif;',
                                                               tags$a(
                                                                 tags$i(class='fa fa-question-circle'),
                                                                 href = "#",
                                                                 onclick="alert('Color of the annotation on the right');return false;"
                                                               )),
                                                          "#2100FA"),#,palette ="limited"
                                
                                selectizeInput('Heatmapcolor_ris',
                                               span( "Heatmap Color",
                                                     style='font-family: Arial, Helvetica, sans-serif;',
                                                     tags$a(
                                                       tags$i(class='fa fa-question-circle'),
                                                       href = "#",
                                                       onclick="alert('Selelct color of the heatmap from the predefined colour schemes');return false;"
                                                     )),
                                               selected = "Paired",
                                               choices = rownames(brewer.pal.info[brewer.pal.info$category=="qual",])),
                                
                                sliderInput("legend_height_ris", "Height of legend",
                                            
                                            min = 0, max = 5, value = 2, step = 0.5
                                ),
                                sliderInput("row_names_ris","Fontsize of rownames",
                                            
                                            min = 0, max = 20, value = 8, step = 1
                                ),
                                sliderInput("column_names_ris","Fontsize of colnames",
                                            
                                            min = 0, max = 20, value = 8, step = 1
                                ),
                                
                                sliderInput("plotheight_ris",
                                            span(    "Height (pixels)",
                                                     style='font-family: Arial, Helvetica, sans-serif;',
                                                     tags$a(
                                                       tags$i(class='fa fa-question-circle'),
                                                       href = "#",
                                                       onclick="alert('Plot size in units (pixels)');return false;"
                                                     )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_ris", 
                                            span(    "Width (pixels)",
                                                     style='font-family: Arial, Helvetica, sans-serif;',
                                                     tags$a(
                                                       tags$i(class='fa fa-question-circle'),
                                                       href = "#",
                                                       onclick="alert('Plot size in units (pixels)');return false;"
                                                     )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr()
                          )
                        )
               )
             )
    ),
    ##Meta Analysis----
    tabPanel("Meta Analysis",
             hr(),
             br(), br(),
             tabsetPanel(
               ###Dichotomous Variable----
               tabPanel(title = "Dichotomous Variable", 
                        value = "dichotomous variable",
                        fluidRow(
                          column(
                            width = 9,
                            tabBox(
                              width = NULL,
                              tabPanel(
                                "Primary File",
                                DTOutput('head_dic'),
                                tags$head(
                                  tags$style(
                                    HTML(".shiny-notification {
                    position:fixed;
                    top: calc(50%);
                    left: calc(50%);
                    width: 100%;
                    max-width: 300px;
                    margin-left: auto;
                    margin-right: auto;
                  }")
                                  )
                                )
                              ),
                              tabPanel(
                                "Group File",
                                DTOutput('head2_dic'),
                                tags$head(
                                  tags$style(
                                    HTML(".shiny-notification {
                    position:fixed;
                    top: calc(50%);
                    left: calc(50%);
                    width: 100%;
                    max-width: 300px;
                    margin-left: auto;
                    margin-right: auto;
                  }")
                                  )
                                )
                              )
                            ),
                            hr(),
                            fluidRow(
                              column(
                                width = 6,
                                box(width =NULL,style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    "Forest plot",
                                    verticalLayout(#tags$div(plotOutput('distPlot_dic')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="h_dic",withSpinner(type =4,plotOutput('distPlot_dic'))))),
                                      tags$div(fluidRow(downloadButton("downloadp1_dic", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp1png_dic", "Save PNG", icon = icon("download"))
                                                        )
                                               )
                                      ) 
                                    ),
                                box(width=NULL,"Funnel plot",style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    verticalLayout(#tags$div(plotOutput('Funnelplot_dic')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="f_dic",withSpinner(type =4,plotOutput('Funnelplot_dic'))))),
                                      tags$div(
                                        textOutput("Egger_p_text"),
                                        textOutput("Begg_p_text")
                                      ),
                                      tags$div(fluidRow(downloadButton("downloadp2_dic", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp2png_dic", "Save PNG", icon = icon("download"))
                                      )
                                      )
                                    )
                                )
                                ),
                              
                              column(
                                width = 6,
                                box(width = NULL, "Influence Analysis Plot", style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    verticalLayout(
                                      tags$div(hidden(div(id="i_dic", withSpinner(type = 4, plotOutput('influencePlot_dic'))))),
                                      tags$div(fluidRow(downloadButton("downloadInfluence_pdf", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadInfluence_png", "Save PNG", icon = icon("download"))
                                      ))
                                    )
                                ),
                                box(width = NULL, style = 'height:300px; overflow-x: scroll; overflow-y: scroll',
                                    "Comparison Table",
                                    dataTableOutput("comparisonTable")
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                actionButton("toggleAdvanced_dic", "Data Format",
                                             style = "color: white; 
                       background-color:#3c8dbc; 
                       position: relative; 
                       Bottom: 4px;
                       height: 35px;
                       width: 110px;"
                                ),
                                shinyjs::hidden(
                                  div(id = "advanced_dic",
                                      strong("Please make sure all your files are in",
                                             span("' .csv '",style = "color:blue"),
                                             "format"),
                                      br(),
                                      h4("1.Primary file"),
                                      p("- Rownames: study, event.e, n.e, event.c, n.c"),
                                      strong(h4("2.Group file")),
                                      p("- Rownames: study, group"),
                                      p("- If subgroup analysis is required, please upload the Group File in 'Choose Group File'. If not, this box can be empty."),
                                      p("- The number of study should be same as that in primary file."),
                                      strong(h4("3.Attention!")),
                                      p("- When uploading a file, please ensure that the data format matches the example data and pay close attention to the meaning represented by each column."),
                                  )),
                                hr(),
                                uiOutput('file2_dic'),
                                uiOutput('group_dic'),
                                actionBttn(
                                  inputId = "run_dic", 
                                  label = "Plot", 
                                  style = "fill", 
                                  color = "success", 
                                  icon = icon("paper-plane"), 
                                  size = "sm"
                                ),
                                actionBttn(
                                  inputId = "reset_dic", 
                                  label = "Reset File", 
                                  style = "fill", 
                                  color = "danger", 
                                  icon = icon("trash"), 
                                  size = "sm"
                                ),
                                tags$br(),
                                tags$br(),
                                downloadBttn(
                                  outputId = "downloadBtn_dic", 
                                  label = "Download Example Data", 
                                  style = "fill", 
                                  color = "warning",
                                  size = "sm"
                                ),
                                tags$br(),
                                checkboxInput("exam_dic","Example Data",TRUE),
                                #uiOutput('exam_dig')
                              )
                            ),
                            hr(),
                            bsCollapse(
                              id = "collapse_dic1", open = NULL,
                              bsCollapsePanel(
                                  "Customize the Forest Plot", class = NULL , style = "info",
                              selectInput("model_dic",
                                          span(   "Model",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Choose fixed effect or random effect models');return false;"
                                                  )),
                                          choices = c("fixed effects model","random effects model")),
                              selectInput("sm_dic", 
                                          span(   "Summary Measure",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Summary Measure is to be used for pooling of studies. \\nRR:Risk ratio \\nOR:Odds ratio \\nRD:Risk difference \\nASD:Arcsine difference \\nOR:Diagnostic Odds ratio');return false;"
                                                  )),
                                          choices = c("RR","OR","RD","ASD","DOR")),
                              selectInput("trimfill_dic",
                                          span(   "Trim-and-fill method",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('A Method for estimating and adjusting for the number and outcomes of missing studies in a meta-analysis.');return false;"
                                                  )),
                                          choices = c("TRUE","FALSE")),
                              textInput("label.e_dic",
                                        span(   "Name of label1",
                                                style='font-family: Arial, Helvetica, sans-serif;',
                                                tags$a(
                                                  tags$i(class='fa fa-question-circle'),
                                                  href = "#",
                                                  onclick="alert(' Label for experimental group.');return false;"
                                                ))),
                              textInput("label.c_dic",
                                        span(  "Name of label2",
                                               style='font-family: Arial, Helvetica, sans-serif;',
                                               tags$a(
                                                 tags$i(class='fa fa-question-circle'),
                                                 href = "#",
                                                 onclick="alert(' Label for control group.');return false;"
                                               ))),
                              colourpicker::colourInput("col.square_dic",
                                                        span( "Color of square",
                                                              style='font-family: Arial, Helvetica, sans-serif;',
                                                              tags$a(
                                                                tags$i(class='fa fa-question-circle'),
                                                                href = "#",
                                                                onclick="alert('The colour for squares reflecting the weight of study in the meta analysis.');return false;"
                                                              )),"skyblue"),
                              colourpicker::colourInput("col.study_dic",
                                                        span( "Color of CI",
                                                              style='font-family: Arial, Helvetica, sans-serif;',
                                                              tags$a(
                                                                tags$i(class='fa fa-question-circle'),
                                                                href = "#",
                                                                onclick="alert('The colour for individual study results and confidence limits');return false;"
                                                              )),palette ="limited","#000000"),
                              sliderInput("fontsize_dic",
                                          span(  "Fontsize",
                                                 style='font-family: Arial, Helvetica, sans-serif;',
                                                 tags$a(
                                                   tags$i(class='fa fa-question-circle'),
                                                   href = "#",
                                                   onclick="alert('The size of text (in points)');return false;"
                                                 )),
                                          min = 5, max = 15, value = 9.5, step = 0.1
                              ),
                              sliderInput("plotheight_dic", 
                                          span(  "Height (pixels)",
                                                 style='font-family: Arial, Helvetica, sans-serif;',
                                                 tags$a(
                                                   tags$i(class='fa fa-question-circle'),
                                                   href = "#",
                                                   onclick="alert('Plot size in units (pixels)');return false;"
                                                 )),
                                          min = 400, max = 5000, value = 460, step = 20
                              ),
                              sliderInput("plotwidth_dic",
                                          span(   "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                          min = 400, max = 1000, value = 800, step = 20
                              )
                            )),
                            hr(),
                            bsCollapse(
                              id = "collapse_dic2", open = NULL,
                              bsCollapsePanel(
                                "Customize the Infulence Analysis Plot", class = NULL , style = "info",
                              
                              sliderInput("plotheight_influence",
                                          span("Height (Influence) (pixels)",
                                               style='font-family: Arial, Helvetica, sans-serif;',
                                               tags$a(tags$i(class='fa fa-question-circle'), href = "#",
                                                      onclick="alert('Plot height for influence analysis');return false;")),
                                          min = 400, max = 5000, value = 460, step = 20),
                              sliderInput("plotwidth_influence",
                                          span("Width (Influence) (pixels)",
                                               style='font-family: Arial, Helvetica, sans-serif;',
                                               tags$a(tags$i(class='fa fa-question-circle'), href = "#",
                                                      onclick="alert('Plot width for influence analysis');return false;")),
                                          min = 400, max = 1000, value = 800, step = 20),
                              colourpicker::colourInput("col.square_influence", 
                                                        span("Color of square (Influence)", style='font-family: Arial, Helvetica, sans-serif;',
                                                             tags$a(tags$i(class='fa fa-question-circle'), href = "#", 
                                                                    onclick="alert('The colour for squares reflecting the weight of study in the influence analysis.');return false;")),
                                                        "skyblue"),
                              colourpicker::colourInput("col.study_influence", 
                                                        span("Color of CI (Influence)", style='font-family: Arial, Helvetica, sans-serif;',
                                                             tags$a(tags$i(class='fa fa-question-circle'), href = "#", 
                                                                    onclick="alert('The colour for individual study results and confidence limits in the influence analysis.');return false;")),
                                                        palette = "limited", "#000000"),
                              sliderInput("fontsize_influence",
                                          span("Fontsize (Influence)", style='font-family: Arial, Helvetica, sans-serif;',
                                               tags$a(tags$i(class='fa fa-question-circle'), href = "#", 
                                                      onclick="alert('The size of text (in points) for influence analysis.');return false;")),
                                          min = 5, max = 15, value = 11.5, step = 0.1)
                            )
                          )
                          )
                        )
                        ),
               ###Continuous Variable----
               tabPanel(title = "Continuous Variable", 
                        value = "continuous variable",
                        fluidRow(
                          column(
                            width = 9,
                            tabBox(
                              width = NULL,
                              tabPanel("Primary File",DT::DTOutput('head_con'),tags$head(
                                tags$style(
                                  HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }")
                                  
                                ))),
                              tabPanel("Group File",DT::DTOutput('head2_con'),tags$head(
                                tags$style(
                                  HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }")
                                  
                                ))
                                
                              )
                            ),
                            hr(),
                            fluidRow(
                              column(
                                width = 6,
                                box(width =NULL,style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    "Forest plot",
                                    verticalLayout(#tags$div(plotOutput('distPlot_con')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="h_con",withSpinner(type =4,plotOutput('distPlot_con'))))),
                                      tags$div(fluidRow(downloadButton("downloadp1_con", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp1png_con", "Save PNG", icon = icon("download"))
                                      )
                                      )
                                    )
                                    
                                ),
                                box(width =NULL,"Funnel plot",style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    verticalLayout(#tags$div(plotOutput('Funnelplot_con')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="f_con",withSpinner(type =4,plotOutput('Funnelplot_con'))))),
                                      #tags$div(hidden(div(id="h_con",withSpinner(type =4,plotOutput('distPlot_con'))))),
                                      tags$div(
                                        textOutput("Egger_p_text_con"),
                                        textOutput("Begg_p_text_con")
                                      ),
                                      tags$div(fluidRow(downloadButton("downloadp2_con", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp2png_con", "Save PNG", icon = icon("download"))
                                      )
                                      )
                                    )
                                    
                                )
                              ),
                              
                              column(
                                width = 6,
                                box(width = NULL, "Influence Analysis Plot", style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    verticalLayout(
                                      tags$div(hidden(div(id="i_con", withSpinner(type = 4, plotOutput('influencePlot_con'))))),
                                      tags$div(fluidRow(downloadButton("downloadInfluence2_pdf", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadInfluence2_png", "Save PNG", icon = icon("download"))
                                      ))
                                    )
                                ),
                                box(width = NULL, style = 'height:300px; overflow-x: scroll; overflow-y: scroll',
                                    "Comparison Table",
                                    dataTableOutput("comparisonTable_con")
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                actionButton("toggleAdvanced_con", "Data Format",
                                             style = "color: white; 
                       background-color:#3c8dbc; 
                       position: relative; 
                       Bottom: 4px;
                       height: 35px;
                       width: 110px;"
                                ),
                                shinyjs::hidden(
                                  div(id = "advanced_con",
                                      strong("Please make sure all your files are in",
                                             span("' .csv '",style = "color:blue"),
                                             "format"),
                                      br(),
                                      h4("1.Primary file"),
                                      p("- Rownames: study, n.e, mean.e, sd.e, n.c, mean.c, sd.c"),
                                      strong(h4("2.Group file")),
                                      p("- Rownames: study, group"),
                                      p("- If subgroup analysis is required, please upload the Group File in 'Choose Group File'. If not, this box can be empty."),
                                      p("- The number of study should be same as that in primary file."),
                                      strong(h4("3.Attention!")),
                                      p("- When uploading a file, please ensure that the data format matches the example data and pay close attention to the meaning represented by each column."),
                                  )),
                                hr(),
                                uiOutput('file2_con'),
                                uiOutput('group_con'),
                                actionBttn(
                                  inputId = "run_con", 
                                  label = "Plot", 
                                  style = "fill", 
                                  color = "success", 
                                  icon = icon("paper-plane"), 
                                  size = "sm"
                                ),
                                actionBttn(
                                  inputId = "reset_con", 
                                  label = "Reset File", 
                                  style = "fill", 
                                  color = "danger", 
                                  icon = icon("trash"), 
                                  size = "sm"
                                ),
                                tags$br(),
                                tags$br(),
                                downloadBttn(
                                  outputId = "downloadBtn_con", 
                                  label = "Download Example Data", 
                                  style = "fill", 
                                  color = "warning",
                                  size = "sm"
                                ),
                                tags$br(),
                                checkboxInput("exam_con","Example Data",TRUE),
                                #uiOutput('exam_dig')
                              )
                            ),
                            hr(),
                            bsCollapse(
                              id = "collapse_con1", open = NULL,
                              bsCollapsePanel(
                                "Customize the Forest Plot", class = NULL , style = "info",
                                selectInput("model_con",
                                            span( "Model",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Choose fixed effect or random effect models');return false;"
                                                  )
                                            ),
                                            choices = c("fixed effects model","random effects model")
                                ),
                                selectInput("sm_con",
                                            span( "Summary Measure",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('A character string indicating which summary measure is to be used for pooling of studies.\\nMD:Mean difference\\nSMD:Standardised mean difference\\nROM:Ratio of means');return false;"
                                                  )
                                            ),
                                            choices = c( "MD", "SMD", "ROM")
                                ),
                                selectInput("trimfill_con",
                                            span(  "Trim-and-fill method",
                                                   style='font-family: Arial, Helvetica, sans-serif;',
                                                   tags$a(
                                                     tags$i(class='fa fa-question-circle'),
                                                     href = "#",
                                                     onclick="alert('A Method for estimating and adjusting for the number and outcomes of missing studies in a meta-analysis.');return false;"
                                                   )
                                            ),
                                            choices = c("TRUE","FALSE")
                                ),
                                textInput("label.e_con",
                                          span(  "Name of label1",
                                                 style='font-family: Arial, Helvetica, sans-serif;',
                                                 tags$a(
                                                   tags$i(class='fa fa-question-circle'),
                                                   href = "#",
                                                   onclick="alert('Label for experimental group.');return false;"
                                                 )
                                          )
                                ),
                                textInput("label.c_con",
                                          span(  "Name of label2",
                                                 style='font-family: Arial, Helvetica, sans-serif;',
                                                 tags$a(
                                                   tags$i(class='fa fa-question-circle'),
                                                   href = "#",
                                                   onclick="alert('Label for control group.');return false;"
                                                 ))
                                ),
                                colourpicker::colourInput("col.square_con",
                                                          span(  "Color of square",
                                                                 style='font-family: Arial, Helvetica, sans-serif;',
                                                                 tags$a(
                                                                   tags$i(class='fa fa-question-circle'),
                                                                   href = "#",
                                                                   onclick="alert('The colour for squares reflecting the weight of study in the meta analysis.');return false;"
                                                                 )),"skyblue"),
                                colourpicker::colourInput("col.study_con",
                                                          span( "Color of CI",
                                                                style='font-family: Arial, Helvetica, sans-serif;',
                                                                tags$a(
                                                                  tags$i(class='fa fa-question-circle'),
                                                                  href = "#",
                                                                  onclick="alert('The colour for individual study results and confidence limits');return false;"
                                                                )),palette ="limited","#000000"),
                                sliderInput("fontsize_con", 
                                            span( "Fontsize",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('The size of text (in points)');return false;"
                                                  )),
                                            min = 5, max = 15, value = 9.5, step = 0.1
                                ),
                                sliderInput("plotheight_con",
                                            span(  "Height (pixels)",
                                                   style='font-family: Arial, Helvetica, sans-serif;',
                                                   tags$a(
                                                     tags$i(class='fa fa-question-circle'),
                                                     href = "#",
                                                     onclick="alert('Plot size in units (pixels)');return false;"
                                                   )),
                                            min = 400, max = 5000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_con", 
                                            span(  "Width (pixels)",
                                                   style='font-family: Arial, Helvetica, sans-serif;',
                                                   tags$a(
                                                     tags$i(class='fa fa-question-circle'),
                                                     href = "#",
                                                     onclick="alert('Plot size in units (pixels)');return false;"
                                                   )),
                                            min = 400, max = 1000, value = 800, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_con2", open = NULL,
                              bsCollapsePanel(
                                "Customize the Infulence Analysis Plot", class = NULL , style = "info",
                                
                                sliderInput("plotheight_influence_con",
                                            span("Height (pixels)",
                                                 style='font-family: Arial, Helvetica, sans-serif;'),
                                            min = 400, max = 5000, value = 460, step = 20),
                                sliderInput("plotwidth_influence_con", 
                                            span("Width (pixels)",
                                                 style='font-family: Arial, Helvetica, sans-serif;'),
                                            min = 400, max = 1000, value = 800, step = 20),
                                colourpicker::colourInput("col.square_influence_con",
                                                          span("Color of square",
                                                               style='font-family: Arial, Helvetica, sans-serif;'),
                                                          "skyblue"),
                                colourpicker::colourInput("col.study_influence_con",
                                                          span("Color of CI",
                                                               style='font-family: Arial, Helvetica, sans-serif;'),
                                                          palette ="limited","#000000"),
                                sliderInput("fontsize_influence_con", 
                                            span("Fontsize",
                                                 style='font-family: Arial, Helvetica, sans-serif;'),
                                            min = 5, max = 15, value = 11.5, step = 0.1)
                              )
                            )
                          )
                        )
               ),
               ###Single Dichotomous Variable----
               tabPanel(title = "Single Dichotomous Variable", 
                        value = "single dichotomous variable",
                        fluidRow(
                          column(
                            width = 9,
                            tabBox(
                              width = NULL,
                              tabPanel("Primary File",DT::DTOutput('head_sig'),tags$head(
                                tags$style(
                                  HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }")
                                  
                                ))),
                              tabPanel("Group File",DT::DTOutput('head2_sig'),tags$head(
                                tags$style(
                                  HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }")
                                  
                                ))
                                
                              )),
                            hr(),
                            fluidRow(
                              column(
                                width = 6,
                                box(width=NULL,style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    "Forest plot",
                                    verticalLayout(#tags$div(plotOutput('distPlot_sig')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="h_sig",withSpinner(type =4,plotOutput('distPlot_sig'))))),
                                      tags$div(fluidRow(downloadButton("downloadp1_sig", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp1png_sig", "Save PNG", icon = icon("download"))
                                      )
                                      )
                                    )
                                ),
                                box(width=NULL,style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    "Funnel plot",
                                    verticalLayout(#tags$div(plotOutput('Funnelplot_sig')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="f_sig",withSpinner(type =4,plotOutput('Funnelplot_sig'))))),
                                      tags$div(
                                        textOutput("Egger_p_text_sig"),
                                        textOutput("Begg_p_text_sig")
                                      ),
                                      tags$div(fluidRow(downloadButton("downloadp2_sig", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp2png_sig", "Save PNG", icon = icon("download"))
                                      )
                                      )
                                    )
                                )
                              ),
                              
                              column(
                                width = 6,
                                box(width=NULL,style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    "Sensitivity Analysis Forest Plot",
                                    verticalLayout(
                                      tags$div(hidden(div(id="i_sig", withSpinner(type = 4, plotOutput('influencePlot_sig'))))),
                                      tags$div(fluidRow(downloadButton("downloadInfluencePlot_sig", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadInfluencePlotPng_sig", "Save PNG", icon = icon("download"))
                                      )
                                      )
                                    )
                                ),
                                box(width=NULL,style = 'height:485px; overflow-x: scroll; overflow-y: scroll',
                                    "Subgroup Comparison Table",
                                    DT::DTOutput('comparisonTable_sig'),
                                    #tags$div(fluidRow(downloadButton("downloadComparisonTable_sig", "Download Table", icon = icon("download"))
                                   # )
                                    #)
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                actionButton("toggleAdvanced_sig_dic", "Data Format",
                                             style = "color: white; 
                       background-color:#3c8dbc; 
                       position: relative; 
                       Bottom: 4px;
                       height: 35px;
                       width: 110px;"
                                ),
                                shinyjs::hidden(
                                  div(id = "advanced_sig_dic",
                                      strong("Please make sure all your files are in",
                                             span("' .csv '",style = "color:blue"),
                                             "format"),
                                      br(),
                                      h4("1.Primary file"),
                                      p("- Rownames: study, case, number"),
                                      strong(h4("2.Group file")),
                                      p("- Rownames: study, group"),
                                      p("- If subgroup analysis is required, please upload the Group File in 'Choose Group File'. If not, this box can be empty."),
                                      p("- The number of study should be same as that in primary file."),
                                      strong(h4("3.Attention!")),
                                      p("- When uploading a file, please ensure that the data format matches the example data and pay close attention to the meaning represented by each column.")
                                      
                                  )
                                ),
                                hr(),
                                uiOutput('file2_sig'),
                                uiOutput('group_sig'),
                                actionBttn(
                                  inputId = "run_sig", 
                                  label = "Plot", 
                                  style = "fill", 
                                  color = "success", 
                                  icon = icon("paper-plane"), 
                                  size = "sm"
                                ),
                                actionBttn(
                                  inputId = "reset_sig_dic", 
                                  label = "Reset File", 
                                  style = "fill", 
                                  color = "danger", 
                                  icon = icon("trash"), 
                                  size = "sm"
                                ),
                                tags$br(),
                                tags$br(),
                                downloadBttn(
                                  outputId = "downloadBtn_sig", 
                                  label = "Download Example Data", 
                                  style = "fill", 
                                  color = "warning",
                                  size = "sm"
                                ),
                                tags$br(),
                                checkboxInput("exam_sig_dic","Example Data",TRUE),
                                #uiOutput('exam_dig')
                              )
                            ),
                            hr(),
                            bsCollapse(
                              id = "collapse_sig_dic1", open = NULL,
                              bsCollapsePanel(
                                "Customize the Forest Plot", class = NULL , style = "info",
                                selectInput("model_sig",
                                            span(   "Model",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('Choose fixed effect or random effect models');return false;"
                                                    )),choices = c("fixed effects model","random effects model")),
                                selectInput("trimfill_sig",
                                            span(   "Trim-and-fill method",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('A Method for estimating and adjusting for the number and outcomes of missing studies in a meta-analysis.');return false;"
                                                    )),
                                            choices = c("TRUE","FALSE")),
                                selectInput("sm_sig", 
                                            span(   "Summary Measure",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('A character string indicating which summary measure is to be used for pooling of studies.\\nPLOGIT:Logit transformation\\nPAS:Arcsine transformation\\nPFT:Freeman-Tukey Double arcsine transformation\\nPLN:Log transformation\\nPRAW:Raw, i.e. untransformed, proportions');return false;"
                                                    )),choices = c( "PLOGIT", "PAS", "PFT", "PLN","PRAW")),
                                colourpicker::colourInput("col.square_sig",
                                                          span(   "Color of square",
                                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                                  tags$a(
                                                                    tags$i(class='fa fa-question-circle'),
                                                                    href = "#",
                                                                    onclick="alert('The colour for squares reflecting the weight of study in the meta analysis.');return false;"
                                                                  )),"skyblue"),
                                colourpicker::colourInput("col.study_sig",
                                                          span(   "Color of CI",
                                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                                  tags$a(
                                                                    tags$i(class='fa fa-question-circle'),
                                                                    href = "#",
                                                                    onclick="alert('The colour for individual study results and confidence limits');return false;"
                                                                  )),palette ="limited","#000000"),
                                sliderInput("fontsize_sig",
                                            span(   "Fontsize",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('The size of text (in points)');return false;"
                                                    )),
                                            min = 5, max = 15, value = 9.5, step = 0.1
                                ),
                                sliderInput("plotheight_sig",
                                            span(   "Height (pixels)",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('Plot size in units (pixels)');return false;"
                                                    )),
                                            min = 400, max = 5000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_sig", 
                                            span(   "Width (pixels)",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('Plot size in units (pixels)');return false;"
                                                    )),
                                            min = 400, max = 1000, value = 800, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_con2", open = NULL,
                              bsCollapsePanel(
                                "Customize the Infulence Analysis Plot", class = NULL , style = "info",
                                
                                colourpicker::colourInput("col.square_sensitivity",
                                                          "Color of square","skyblue"),
                                colourpicker::colourInput("col.study_sensitivity",
                                                          "Color of CI","#000000"),
                                sliderInput("fontsize_sensitivity",
                                            "Fontsize",min = 5, max = 15, value = 11.5, step = 0.1),
                                sliderInput("plotheight_sensitivity",
                                            "Height (pixels)",min = 400, max = 5000, value = 460, step = 20),
                                sliderInput("plotwidth_sensitivity", 
                                            "Width (pixels)",min = 400, max = 1000, value = 800, step = 20)
                              )
                            )
                          )
                        )
               ),
               ###Single Continuous Variable----
               tabPanel(title = "Single Continuous Variable", 
                        value = "single continuous variable",
                        fluidRow(
                          column(
                            width = 9,
                            tabBox(
                              width = NULL,
                              tabPanel("Primary File",DT::DTOutput('head_sig_con'),tags$head(
                                tags$style(
                                  HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }")
                                  
                                ))),
                              tabPanel("Group File",DT::DTOutput('head2_sig_con'),tags$head(
                                tags$style(
                                  HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }")
                                  
                                ))
                                
                              )),
                            hr(),
                            fluidRow(
                              column(
                                width = 6,
                                box(width=NULL,style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    "Forest plot",
                                    verticalLayout(#tags$div(plotOutput('distPlot_sig_con')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="h_sig_con",withSpinner(type =4,plotOutput('distPlot_sig_con'))))),
                                      tags$div(fluidRow(downloadButton("downloadp1_sig_con", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp1png_sig_con", "Save PNG", icon = icon("download"))
                                      )
                                      )
                                    )
                                ),
                                box(width=NULL,style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    "Funnel plot",
                                    verticalLayout(#tags$div(plotOutput('Funnelplot_sig_con')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="f_sig_con",withSpinner(type =4,plotOutput('Funnelplot_sig_con'))))),
                                      tags$div(
                                        textOutput("Egger_p_text_sig_con"),
                                        textOutput("Begg_p_text_sig_con")
                                      ),
                                      tags$div(fluidRow(downloadButton("downloadp2_sig_con", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp2png_sig_con", "Save PNG", icon = icon("download"))
                                      )
                                      )
                                    )
                                    
                                    
                                )
                              ),
                              
                              column(
                                width = 6,
                                #box(width=NULL, style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    #"Sensitivity Analysis Forest Plot",
                                    #plotOutput('influencePlot_sigcon'),
                                    #fluidRow(
                                      #downloadButton("downloadInfluencePlot_sigcon", "Save PDF", icon = icon("download")),
                                      #downloadButton("downloadInfluencePlotPng_sigcon", "Save PNG", icon = icon("download"))
                                    #)
                                #),
                                box(width=NULL,style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    "Sensitivity Analysis Forest Plot",
                                    verticalLayout(
                                      tags$div(hidden(div(id="i_sig_con", withSpinner(type = 4, plotOutput('influencePlot_sigcon'))))),
                                      tags$div(fluidRow(downloadButton("downloadInfluencePlot_sigcon", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadInfluencePlotPng_sigcon", "Save PNG", icon = icon("download"))
                                      )
                                      )
                                    )
                                ),
                                box(width=NULL, style = 'height:485px; overflow-x: scroll; overflow-y: scroll',
                                    "Subgroup Comparison Table",
                                    DT::DTOutput('comparisonTable_sigcon')
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                actionButton("toggleAdvanced_sig_con", "Data Format",
                                             style = "color: white; 
                       background-color:#3c8dbc; 
                       position: relative; 
                       Bottom: 4px;
                       height: 35px;
                       width: 110px;"
                                ),
                                shinyjs::hidden(
                                  div(id = "advanced_sig_con",
                                      strong("Please make sure all your files are in",
                                             span("' .csv '",style = "color:blue"),
                                             "format"),
                                      br(),
                                      h4("1.Primary file"),
                                      p("- Rownames: study, n, mean, sd"),
                                      strong(h4("2.Group file")),
                                      p("- Rownames: study, group"),
                                      p("- If subgroup analysis is required, please upload the Group File in 'Choose Group File'. If not, this box can be empty."),
                                      p("- The number of study should be same as that in primary file."),
                                      strong(h4("3.Attention!")),
                                      p("- When uploading a file, please ensure that the data format matches the example data and pay close attention to the meaning represented by each column.")
                                  )
                                ),
                                hr(),
                                uiOutput('file2_sig_con'),
                                uiOutput('group_sig_con'),
                                actionBttn(
                                  inputId = "run_sig_con", 
                                  label = "Plot", 
                                  style = "fill", 
                                  color = "success", 
                                  icon = icon("paper-plane"), 
                                  size = "sm"
                                ),
                                actionBttn(
                                  inputId = "reset_sig_con", 
                                  label = "Reset File", 
                                  style = "fill", 
                                  color = "danger", 
                                  icon = icon("trash"), 
                                  size = "sm"
                                ),
                                tags$br(),
                                tags$br(),
                                downloadBttn(
                                  outputId = "downloadBtn_sig_con", 
                                  label = "Download Example Data", 
                                  style = "fill", 
                                  color = "warning",
                                  size = "sm"
                                ),
                                tags$br(),
                                checkboxInput("exam_sig_con","Example Data",TRUE),
                                #uiOutput('exam_dig')
                              )
                            ),
                            hr(),
                            bsCollapse(
                              id = "collapse_sig_con1", open = NULL,
                              bsCollapsePanel(
                                "Customize the Forest Plot", class = NULL , style = "info",
                                selectInput("model_sig_con", 
                                            span(  "Model",
                                                   style='font-family: Arial, Helvetica, sans-serif;',
                                                   tags$a(
                                                     tags$i(class='fa fa-question-circle'),
                                                     href = "#",
                                                     onclick="alert('Choose fixed effect or random effect models');return false;"
                                                   )),choices = c("fixed effects model","random effects model")),
                                selectInput("sm_sig_con", 
                                            span( "Summary Measure",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('A character string indicating which summary measure is to be used for pooling of studies.\\nMRAW:Raw, i.e. untransformed, means\\nMLN:Log transformed means');return false;"
                                                  )),choices = c("MRAW","MLN")),
                                selectInput("trimfill_sig_con",
                                            span(   "Trim-and-fill method",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('A Method for estimating and adjusting for the number and outcomes of missing studies in a meta-analysis.');return false;"
                                                    )),
                                            choices = c("TRUE","FALSE")),
                                
                                colourpicker::colourInput("col.square_sig_con",
                                                          span( "Color of square",
                                                                style='font-family: Arial, Helvetica, sans-serif;',
                                                                tags$a(
                                                                  tags$i(class='fa fa-question-circle'),
                                                                  href = "#",
                                                                  onclick="alert('The colour for squares reflecting the weight of study in the meta analysis');return false;"
                                                                )),"skyblue"),
                                colourpicker::colourInput("col.study_sig_con",
                                                          span("Color of CI",
                                                               style='font-family: Arial, Helvetica, sans-serif;',
                                                               tags$a(
                                                                 tags$i(class='fa fa-question-circle'),
                                                                 href = "#",
                                                                 onclick="alert('The colour for individual study results and confidence limits');return false;"
                                                               )),palette ="limited","#000000"),
                                sliderInput("fontsize_sig_con", 
                                            span("Fontsize",
                                                 style='font-family: Arial, Helvetica, sans-serif;',
                                                 tags$a(
                                                   tags$i(class='fa fa-question-circle'),
                                                   href = "#",
                                                   onclick="alert('The size of text (in points)');return false;"
                                                 )),
                                            min = 5, max = 15, value = 9.5, step = 0.1
                                ),
                                sliderInput("plotheight_sig_con",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 5000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_sig_con", 
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 800, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_sig_con2", open = NULL,
                              bsCollapsePanel(
                                "Customize the Infulence Analysis Plot", class = NULL , style = "info",
                                
                                colourpicker::colourInput("col.square_sensitivity_sigcon", "Color of square", "skyblue"),
                                colourpicker::colourInput("col.study_sensitivity_sigcon", "Color of CL", "#000000"),
                                sliderInput("fontsize_sensitivity_sigcon", "Fontsize", min = 5, max = 15, value = 11.5, step = 0.1),
                                sliderInput("plotheight_sensitivity_sigcon", "Height (pixels)", min = 400, max = 5000, value = 460, step = 20),
                                sliderInput("plotwidth_sensitivity_sigcon", "Width (pixels)", min = 400, max = 1000, value = 800, step = 20)
                              )
                            )
                          )
                        )
               ),
               ###Deft Analysis----
               tabPanel(title = "Deft Analysis", 
                        value = "deft analysis",
                        fluidRow(
                          column(
                            width = 9,
                            br(),
                            box(style = 'overflow-x: scroll',
                                DT::DTOutput("head_de")%>% withSpinner(type=4),width=NULL,
                                tags$head(
                                  tags$style(
                                    HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }
             "
                                    )
                                  )
                                )
                            ),
                            hr(),
                            fluidRow(
                              column(
                                width = 6,
                                box(width=NULL,style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                    verticalLayout(
                                      #tags$div(plotOutput('distPlot_de')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="p1_de",withSpinner(type =4,plotOutput('distPlot_de'))))),
                                      tags$div(fluidRow(downloadButton("downloadp1_de", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp1png_de", "Save PNG", icon = icon("download")))
                                               
                                      )
                                    )
                                )
                              ),
                              
                              column(
                                width = 6,
                                box(width=NULL,style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                    verticalLayout(
                                      #tags$div(plotOutput('distPlot_de')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="p2_de",withSpinner(type =4,plotOutput('distPlot_de_sen'))))),
                                      tags$div(fluidRow(downloadButton("downloadp2_de", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp2png_de", "Save PNG", icon = icon("download")))
                                               
                                      )
                                    )
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                actionButton("toggleAdvanced_de", "Data Format",
                                             style = "color: white; 
                       background-color:#3c8dbc; 
                       position: relative; 
                       Bottom: 4px;
                       height: 35px;
                       width: 110px;"
                                ),
                                
                                shinyjs::hidden(
                                  div(id = "advanced_de",
                                      strong("Please make sure all your files are in",
                                             span("' .csv '",style = "color:blue"),
                                             "format"),
                                      br(),
                                      h5("1.Rownames: trial,subgroup,hr,ci.lb,ci.ub,ni"),
                                      p("- trial: Name of the study"),
                                      p("- subgroup:level of subgroup that you want to compare"),
                                      p("- hr:specify hazard ratios (hr)"),
                                      p("- ci.lb:specify lower bound for hr confidence intervals"),
                                      p("- ci.ub:specify upper bound for hr confidence intervals"),
                                      p("- ni:specify sample number")
                                      
                                  )
                                ),
                                hr(),
                                uiOutput('file1_de'),
                                actionBttn(
                                  inputId = "run_de", 
                                  label = "Plot", 
                                  style = "fill", 
                                  color = "success", 
                                  icon = icon("paper-plane"), 
                                  size = "sm"
                                ),
                                actionBttn(
                                  inputId = "reset_de", 
                                  label = "Reset File", 
                                  style = "fill", 
                                  color = "danger", 
                                  icon = icon("trash"), 
                                  size = "sm"
                                ),
                                tags$br(),
                                tags$br(),
                                downloadBttn(
                                  outputId = "downloadBtn_de", 
                                  label = "Download Example Data", 
                                  style = "fill", 
                                  color = "warning",
                                  size = "sm"
                                ),
                                tags$br(),
                                checkboxInput("exam_de","Example Data",TRUE),
                                
                              )
                            ),
                            hr(),
                            bsCollapse(
                              id = "collapse_de1", open = NULL,
                              bsCollapsePanel(
                                "Customize the Forest Plot", class = NULL , style = "info",
                                selectInput("sm_de", 
                                            span(  "Model",
                                                   style='font-family: Arial, Helvetica, sans-serif;',
                                                   tags$a(
                                                     tags$i(class='fa fa-question-circle'),
                                                     href = "#",
                                                     onclick="alert('Choose fixed effect or random effect models');return false;"
                                                   )),choices = list("fixed effects model"="FE",
                                                                     "random effect models"="REML")
                                ),                              
                                sliderInput("cex_de",
                                            span( "cex ",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default. ');return false;"
                                                  )),
                                            min =0.5, max = 1.1, value = 0.9, step =0.05
                                ),
                                
                                sliderInput("plotheight_de",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_de", 
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr()
                          )
                        )
               ),
               ###Survival Variable----
               tabPanel(title = "Survival Variable", 
                        value = "survival variable",
                        tabsetPanel(
                          id = "n_tabs",
                          tabPanel("With HR",
                                   
                        fluidRow(
                          column(
                            width = 9,
                            
                            tabBox(
                              width = NULL,
                              tabPanel("Primary File",DT::DTOutput('head_rate'),tags$head(
                                tags$style(
                                  HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }")
                                  
                                ))),
                              tabPanel("Group File",DT::DTOutput('head2_rate'),tags$head(
                                tags$style(
                                  HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }")
                                  
                                ))
                                
                              )),
                            hr(),
                            fluidRow(
                              column(
                                width = 6,
                                box(width=NULL,
                                    "Forest plot",style = 'height:485px; overflow-x: scroll; overflow-y: scroll',
                                    verticalLayout(#tags$div(plotOutput('distPlot_rate')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="h_rate",withSpinner(type =4,plotOutput('distPlot_rate'))))),
                                      tags$div(fluidRow(downloadButton("downloadp1_rate", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp1png_rate", "Save PNG", icon = icon("download"))
                                      )
                                      )
                                    )
                                ),
                                box(width=NULL,
                                    "Funnel plot",style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                    verticalLayout(#tags$div(plotOutput('Funnelplot_rate')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="f_rate",withSpinner(type =4,plotOutput('Funnelplot_rate'))))),
                                      tags$div(
                                        textOutput("Egger_p_text_rate"),
                                        textOutput("Begg_p_text_rate")
                                      ),
                                      tags$div(fluidRow(downloadButton("downloadp2_rate", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp2png_rate", "Save PNG", icon = icon("download"))
                                      )
                                      )
                                    )
                                    
                                )
                              ),
                              
                              column(
                                width = 6,
                                box(width=NULL,
                                    "Sensitivity Forest plot",style = 'height:485px; overflow-x: scroll; overflow-y: scroll',
                                    verticalLayout(#tags$div(plotOutput('distPlot_rate')%>% withSpinner(type=4)),
                                      tags$div(hidden(div(id="s_rate",withSpinner(type =4,plotOutput('sensitivity_forest_rate'))))),
                                      tags$div(fluidRow(downloadButton("downloadp3_rate", "Save PDF", icon = icon("download")),
                                                        downloadButton("downloadp3png_rate", "Save PNG", icon = icon("download"))
                                      )
                                      )
                                    )
                                ),
                                #box(width = NULL, 
                                    #title = "Comparison Table", style = 'height:300px; overflow-x: scroll; overflow-y: scroll',
                                box(width=NULL, 
                                    style = 'height:485px; overflow-x: scroll; overflow-y: scroll',
                                    "Subgroup Comparison Table",
                                    DT::DTOutput('comparison_table')  # 新增的表格显示区域
                                )
                              )
                            )
                          ),
                          column(
                            width = 3,
                            br(),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                actionButton("toggleAdvanced_rate", "Data Format",
                                             style = "color: white; 
                       background-color:#3c8dbc; 
                       position: relative; 
                       Bottom: 4px;
                       height: 35px;
                       width: 110px;"
                                ),
                                shinyjs::hidden(
                                  div(id = "advanced_rate",
                                      strong("Please make sure all your files are in",
                                             span("' .csv '",style = "color:blue"),
                                             "format"),
                                      br(),
                                      h4("1.Primary file"),
                                      p("- Rownames: study, HR, lower, upper"),
                                      strong(h4("2.Group file")),
                                      p("- Rownames: study, group"),
                                      p("- If subgroup analysis is required, please upload the Group File in 'Choose Group File'. If not, this box can be empty."),
                                      p("- The number of study should be same as that in primary file."),
                                      strong(h4("3.Attention!")),
                                      p("- When uploading a file, please ensure that the data format matches the example data and pay close attention to the meaning represented by each column.")
                                  )
                                ),
                                hr(),
                                uiOutput('file2_rate'),
                                uiOutput('group_rate'),
                                actionBttn(
                                  inputId = "run_rate", 
                                  label = "Plot", 
                                  style = "fill", 
                                  color = "success", 
                                  icon = icon("paper-plane"), 
                                  size = "sm"
                                ),
                                actionBttn(
                                  inputId = "reset_rate", 
                                  label = "Reset File", 
                                  style = "fill", 
                                  color = "danger", 
                                  icon = icon("trash"), 
                                  size = "sm"
                                ),
                                tags$br(),
                                tags$br(),
                                downloadBttn(
                                  outputId = "downloadBtn_rate", 
                                  label = "Download Example Data", 
                                  style = "fill", 
                                  color = "warning",
                                  size = "sm"
                                ),
                                tags$br(),
                                checkboxInput("exam_rate","Example Data",TRUE),
                              )
                            ),
                            hr(),
                            bsCollapse(
                              id = "collapse_rate1", open = NULL,
                              bsCollapsePanel(
                                "Customize the Forest Plot", class = NULL , style = "info",
                                selectInput("model_rate", 
                                            span( "Model",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Choose fixed effect or random effect models');return false;"
                                                  )),choices = c("fixed effects model","random effects model")),
                                selectInput("sm_rate", 
                                            span( "Summary Measure",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('A character string indicating underlying summary measure.Please choose the measure you use in the primary file.');return false;"
                                                  )),choices = c( "RD", "RR", "OR", "ASD", "HR", "MD", "SMD", "ROM"), "HR"),
                                selectInput("trimfill_rate",
                                            span(   "Trim-and-fill method",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('A Method for estimating and adjusting for the number and outcomes of missing studies in a meta-analysis.');return false;"
                                                    )),
                                            choices = c("TRUE","FALSE")),
                                
                                colourpicker::colourInput("col.square_rate",
                                                          span( "Color of square",
                                                                style='font-family: Arial, Helvetica, sans-serif;',
                                                                tags$a(
                                                                  tags$i(class='fa fa-question-circle'),
                                                                  href = "#",
                                                                  onclick="alert('The colour for squares reflecting the weight of study in the meta analysis.');return false;"
                                                                )),"skyblue"),
                                colourpicker::colourInput("col.study_rate",
                                                          span( "Color of CI",
                                                                style='font-family: Arial, Helvetica, sans-serif;',
                                                                tags$a(
                                                                  tags$i(class='fa fa-question-circle'),
                                                                  href = "#",
                                                                  onclick="alert('The colour for individual study results and confidence limits');return false;"
                                                                )),palette ="limited","#000000"),
                                sliderInput("fontsize_rate", 
                                            span( "Fontsize",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('The size of text (in points)');return false;"
                                                  )),
                                            min = 5, max = 15, value = 9.5, step = 0.1
                                ),
                                sliderInput("plotheight_rate", 
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 5000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_rate", 
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 800, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_rate2", open = NULL,
                              bsCollapsePanel(
                                "Customize the Influence Analysis Plot", class = NULL , style = "info",
                                colourpicker::colourInput("col.square_sensitivity_rate",
                                                          "Color of square","skyblue"),
                                colourpicker::colourInput("col.study_sensitivity_rate",
                                                          "Color of CI","#000000"),
                                sliderInput("fontsize_sensitivity_rate",
                                            "Fontsize",min = 5, max = 15, value = 11.5, step = 0.1),
                                sliderInput("plotheight_sensitivity_rate",
                                            "Height (pixels)",min = 400, max = 5000, value = 460, step = 20),
                                sliderInput("plotwidth_sensitivity_rate", 
                                            "Width (pixels)",min = 400, max = 1000, value = 800, step = 20)
                              )),
                            hr()
                          )
                        )
                          ),
                        tabPanel("With O-E",
                                 
                                 fluidRow(
                                   column(
                                     width = 9,
                                     
                                     tabBox(
                                       width = NULL,
                                       tabPanel("Primary File",DT::DTOutput('head_rate2'),tags$head(
                                         tags$style(
                                           HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }")
                                           
                                         ))),
                                       tabPanel("Group File",DT::DTOutput('head2_rate2'),tags$head(
                                         tags$style(
                                           HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }")
                                           
                                         ))
                                         
                                       )),
                                     hr(),
                                     fluidRow(
                                       column(
                                         width = 6,
                                         box(width=NULL,
                                             "Forest plot",style = 'height:485px; overflow-x: scroll; overflow-y: scroll',
                                             verticalLayout(#tags$div(plotOutput('distPlot_rate')%>% withSpinner(type=4)),
                                               tags$div(hidden(div(id="h_rate2",withSpinner(type =4,plotOutput('distPlot_rate2'))))),
                                               tags$div(fluidRow(downloadButton("downloadp1_rate2", "Save PDF", icon = icon("download")),
                                                                 downloadButton("downloadp1png_rate2", "Save PNG", icon = icon("download"))
                                               )
                                               )
                                             )
                                         ),
                                         box(width=NULL,
                                             "Funnel plot",style = 'height:515px; overflow-x: scroll; overflow-y: scroll',
                                             verticalLayout(#tags$div(plotOutput('Funnelplot_rate')%>% withSpinner(type=4)),
                                               tags$div(hidden(div(id="f_rate2",withSpinner(type =4,plotOutput('Funnelplot_rate2'))))),
                                               tags$div(
                                                 textOutput("Egger_p_text_rate2"),
                                                 textOutput("Begg_p_text_rate2")
                                               ),
                                               tags$div(fluidRow(downloadButton("downloadp2_rate2", "Save PDF", icon = icon("download")),
                                                                 downloadButton("downloadp2png_rate2", "Save PNG", icon = icon("download"))
                                               )
                                               )
                                             )
                                             
                                         )
                                       ),
                                       
                                       column(
                                         width = 6,
                                         box(width=NULL,
                                             "Sensitivity Forest plot",style = 'height:485px; overflow-x: scroll; overflow-y: scroll',
                                             verticalLayout(#tags$div(plotOutput('distPlot_rate')%>% withSpinner(type=4)),
                                               tags$div(hidden(div(id="s_rate2",withSpinner(type =4,plotOutput('sensitivity_forest_rate2'))))),
                                               tags$div(fluidRow(downloadButton("downloadp3_rate2", "Save PDF", icon = icon("download")),
                                                                 downloadButton("downloadp3png_rate2", "Save PNG", icon = icon("download"))
                                               )
                                               )
                                             )
                                         ),
                                         #box(width = NULL, 
                                         #title = "Comparison Table", style = 'height:300px; overflow-x: scroll; overflow-y: scroll',
                                         box(width=NULL, 
                                             style = 'height:485px; overflow-x: scroll; overflow-y: scroll',
                                             "Subgroup Comparison Table",
                                             DT::DTOutput('comparison_table2')  # 新增的表格显示区域
                                         )
                                       )
                                     )
                                   ),
                                   column(
                                     width = 3,
                                     br(),
                                     hr(),
                                     fluidRow(
                                       column(
                                         width = 12,
                                         actionButton("toggleAdvanced_rate2", "Data Format",
                                                      style = "color: white; 
                       background-color:#3c8dbc; 
                       position: relative; 
                       Bottom: 4px;
                       height: 35px;
                       width: 110px;"
                                         ),
                                         shinyjs::hidden(
                                           div(id = "advanced_rate2",
                                               strong("Please make sure all your files are in",
                                                      span("' .csv '",style = "color:blue"),
                                                      "format"),
                                               br(),
                                               h4("1.Primary file"),
                                               p("- Rownames: study, OE, V"),
                                               strong(h4("2.Group file")),
                                               p("- Rownames: study, group"),
                                               p("- If subgroup analysis is required, please upload the Group File in 'Choose Group File'. If not, this box can be empty."),
                                               p("- The number of study should be same as that in primary file."),
                                               strong(h4("3.Attention!")),
                                               p("- When uploading a file, please ensure that the data format matches the example data and pay close attention to the meaning represented by each column.")
                                           )
                                         ),
                                         hr(),
                                         uiOutput('file2_rate2'),
                                         uiOutput('group_rate2'),
                                         actionBttn(
                                           inputId = "run_rate2", 
                                           label = "Plot", 
                                           style = "fill", 
                                           color = "success", 
                                           icon = icon("paper-plane"), 
                                           size = "sm"
                                         ),
                                         actionBttn(
                                           inputId = "reset_rate2", 
                                           label = "Reset File", 
                                           style = "fill", 
                                           color = "danger", 
                                           icon = icon("trash"), 
                                           size = "sm"
                                         ),
                                         tags$br(),
                                         tags$br(),
                                         downloadBttn(
                                           outputId = "downloadBtn_rate2", 
                                           label = "Download Example Data", 
                                           style = "fill", 
                                           color = "warning",
                                           size = "sm"
                                         ),
                                         tags$br(),
                                         checkboxInput("exam_rate2","Example Data",TRUE),
                                       )
                                     ),
                                     hr(),
                                     bsCollapse(
                                       id = "collapse_rate", open = NULL,
                                       bsCollapsePanel(
                                         "Customize the Forest Plot", class = NULL , style = "info",
                                         selectInput("model_rate2", 
                                                     span( "Model",
                                                           style='font-family: Arial, Helvetica, sans-serif;',
                                                           tags$a(
                                                             tags$i(class='fa fa-question-circle'),
                                                             href = "#",
                                                             onclick="alert('Choose fixed effect or random effect models');return false;"
                                                           )),choices = c("fixed effects model","random effects model")),
                                         selectInput("sm_rate2", 
                                                     span( "Summary Measure",
                                                           style='font-family: Arial, Helvetica, sans-serif;',
                                                           tags$a(
                                                             tags$i(class='fa fa-question-circle'),
                                                             href = "#",
                                                             onclick="alert('A character string indicating underlying summary measure.Please choose the measure you use in the primary file.');return false;"
                                                           )),choices = c( "RD", "RR", "OR", "ASD", "HR", "MD", "SMD", "ROM"), "HR"),
                                         selectInput("trimfill_rate2",
                                                     span(   "Trim-and-fill method",
                                                             style='font-family: Arial, Helvetica, sans-serif;',
                                                             tags$a(
                                                               tags$i(class='fa fa-question-circle'),
                                                               href = "#",
                                                               onclick="alert('A Method for estimating and adjusting for the number and outcomes of missing studies in a meta-analysis.');return false;"
                                                             )),
                                                     choices = c("TRUE","FALSE")),
                                         
                                         colourpicker::colourInput("col.square_rate2",
                                                                   span( "Color of square",
                                                                         style='font-family: Arial, Helvetica, sans-serif;',
                                                                         tags$a(
                                                                           tags$i(class='fa fa-question-circle'),
                                                                           href = "#",
                                                                           onclick="alert('The colour for squares reflecting the weight of study in the meta analysis.');return false;"
                                                                         )),"skyblue"),
                                         colourpicker::colourInput("col.study_rate2",
                                                                   span( "Color of CI",
                                                                         style='font-family: Arial, Helvetica, sans-serif;',
                                                                         tags$a(
                                                                           tags$i(class='fa fa-question-circle'),
                                                                           href = "#",
                                                                           onclick="alert('The colour for individual study results and confidence limits');return false;"
                                                                         )),palette ="limited","#000000"),
                                         sliderInput("fontsize_rate2", 
                                                     span( "Fontsize",
                                                           style='font-family: Arial, Helvetica, sans-serif;',
                                                           tags$a(
                                                             tags$i(class='fa fa-question-circle'),
                                                             href = "#",
                                                             onclick="alert('The size of text (in points)');return false;"
                                                           )),
                                                     min = 5, max = 15, value = 9.5, step = 0.1
                                         ),
                                         sliderInput("plotheight_rate2", 
                                                     span( "Height (pixels)",
                                                           style='font-family: Arial, Helvetica, sans-serif;',
                                                           tags$a(
                                                             tags$i(class='fa fa-question-circle'),
                                                             href = "#",
                                                             onclick="alert('Plot size in units (pixels)');return false;"
                                                           )),
                                                     min = 400, max = 5000, value = 460, step = 20
                                         ),
                                         sliderInput("plotwidth_rate2", 
                                                     span( "Width (pixels)",
                                                           style='font-family: Arial, Helvetica, sans-serif;',
                                                           tags$a(
                                                             tags$i(class='fa fa-question-circle'),
                                                             href = "#",
                                                             onclick="alert('Plot size in units (pixels)');return false;"
                                                           )),
                                                     min = 400, max = 1000, value = 800, step = 20
                                         )
                                       )),
                                     hr(),
                                     bsCollapse(
                                       id = "collapse_rate2", open = NULL,
                                       bsCollapsePanel(
                                         "Customize the Influence Analysis Plot", class = NULL , style = "info",
                                         colourpicker::colourInput("col.square_sensitivity_rate2",
                                                                   "Color of square","skyblue"),
                                         colourpicker::colourInput("col.study_sensitivity_rate2",
                                                                   "Color of CI","#000000"),
                                         sliderInput("fontsize_sensitivity_rate2",
                                                     "Fontsize",min = 5, max = 15, value = 11.5, step = 0.1),
                                         sliderInput("plotheight_sensitivity_rate2",
                                                     "Height (pixels)",min = 400, max = 5000, value = 460, step = 20),
                                         sliderInput("plotwidth_sensitivity_rate2", 
                                                     "Width (pixels)",min = 400, max = 1000, value = 800, step = 20)
                                       )),
                                     hr()
                                   )
                                 )   
                        )
                          )
               ),
               ###Diagnostic Test----
               tabPanel(title = "Diagnostic Test", 
                        value = "diagnostic test",
                        fluidRow(
                          column(
                            width = 9,
                            br(),
                            box(DT::DTOutput("head_dig"),width=NULL,
                                tags$head(
                                  tags$style(
                                    HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }
             "
                                    )
                                  )
                                )
                            ),
                            tabBox(width =NULL,
                                   tabPanel( "Forest plot",style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                             #fluidRow(plotOutput('stp1_dig')%>% withSpinner(type=4)),
                                             fluidRow(hidden(div(id="h1_dig",withSpinner(type =4,plotOutput('stp1_dig'))))),
                                             fluidRow(downloadButton("downloadp1_dig", "Save PDF", icon = icon("download")),
                                                      downloadButton("downloadp1png_dig", "Save PNG", icon = icon("download"))
                                             )
                                   )
                                   ,
                                   tabPanel(width =NULL,"SROC plot",style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                            #fluidRow(plotOutput('stp2_dig')%>% withSpinner(type=4)),
                                            fluidRow(hidden(div(id="h2_dig",withSpinner(type =4,plotOutput('stp2_dig'))))),
                                            fluidRow(downloadButton("downloadp2_dig", "Save PDF", icon = icon("download")),
                                                     downloadButton("downloadp2png_dig", "Save PNG", icon = icon("download"))
                                            )
                                   ),
                                   tabPanel(width = NULL,"Crosshair plot",style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                            #fluidRow(plotOutput('stp3_dig')%>% withSpinner(type=4)),
                                            fluidRow(hidden(div(id="h3_dig",withSpinner(type =4,plotOutput('stp3_dig'))))),
                                            fluidRow(downloadButton("downloadp3_dig", "Save PDF", icon = icon("download")),
                                                     downloadButton("downloadp3png_dig", "Save PNG", icon = icon("download"))
                                            )
                                   )
                            ),
                            hr()
                          ),
                          column(
                            width = 3,
                            br(),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                actionButton("toggleAdvanced_dig", "Data Format",
                                             style = "color: white; 
                       background-color:#3c8dbc; 
                       position: relative; 
                       Bottom: 4px;
                       height: 35px;
                       width: 110px;"
                                ),
                                
                                shinyjs::hidden(
                                  div(id = "advanced_dig",
                                      strong("Please make sure all your files are in",
                                             span("' .csv '",style = "color:blue"),
                                             "format"),
                                      br(),
                                      p("1.Rownames: studynames, TP, FN, TN, FP"),  
                                      p("2.Study number: there must be more than 5 studies to be included.")
                                  )
                                ),
                                hr(),
                                uiOutput('file3_dig'),
                                actionBttn(
                                  inputId = "run_dig", 
                                  label = "Plot", 
                                  style = "fill", 
                                  color = "success", 
                                  icon = icon("paper-plane"), 
                                  size = "sm"
                                ),
                                actionBttn(
                                  inputId = "reset_dig", 
                                  label = "Reset File", 
                                  style = "fill", 
                                  color = "danger", 
                                  icon = icon("trash"), 
                                  size = "sm"
                                ),
                                tags$br(),
                                tags$br(),
                                downloadBttn(
                                  outputId = "downloadBtn_dig", 
                                  label = "Download Example Data", 
                                  style = "fill", 
                                  color = "warning",
                                  size = "sm"
                                ),
                                tags$br(),
                                checkboxInput("exam_dig","Example Data",TRUE),
                              )
                            ),
                            hr(),
                            bsCollapse(
                              id = "collapse_dig1", open = NULL,
                              bsCollapsePanel(
                                "Customize the Forest Plot", class = NULL , style = "info",
                                colourpicker::colourInput("col.diamond",
                                                          span(" Diamond Color",
                                                               style='font-family: Arial, Helvetica, sans-serif;',
                                                               tags$a(
                                                                 tags$i(class='fa fa-question-circle'),
                                                                 href = "#",
                                                                 onclick="alert('The filling color for the diamond representing the summary estimate.');return false;"
                                                               )),
                                                          "white"),
                                colourpicker::colourInput("col.predint",
                                                          span(" Prediction Interval Color",
                                                               style='font-family: Arial, Helvetica, sans-serif;',
                                                               tags$a(
                                                                 tags$i(class='fa fa-question-circle'),
                                                                 href = "#",
                                                                 onclick="alert('Line color for the prediction interval.');return false;"
                                                               )),
                                                          "black"),
                                sliderInput("size.study", 
                                            span( "Study Size",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Line width for the study results in mm.');return false;"
                                                  )),
                                            min = 0, max =1, value = 0.5, step = 0.1
                                ),
                                sliderInput("size.predint", 
                                            span( "Prediction Interval Size",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Line width for the prediction interval in mm.');return false;"
                                                  )),
                                            min = 0, max =2, value = 1, step = 0.2
                                ),
                                sliderInput("refline", 
                                            span( "Reference Line",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Optional numeric specifying a reference line');return false;"
                                                  )),
                                            min = 0, max =1, value = 0.5, step = 0.1
                                ),
                                sliderInput("lty.ref", 
                                            span( "Reference line type",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('1. solld; 2. dashed; 3. dotted; 4. dotdash; 5. longdash; 6. twodash');return false;"
                                                  )),
                                            min = 1, max = 6, value = 3, step = 1
                                ),
                                sliderInput("plotheight_dig", 
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 5000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_dig", 
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_dig2", open = NULL,
                              bsCollapsePanel(
                                "Customize the SROC Plot", class = NULL , style = "info",
                                colourpicker::colourInput("col1",
                                                          span("Point Estimates Color",
                                                               style='font-family: Arial, Helvetica, sans-serif;',
                                                               tags$a(
                                                                 tags$i(class='fa fa-question-circle'),
                                                                 href = "#",
                                                                 onclick="alert('Color of point estimates');return false;"
                                                               )),
                                                          "red"),
                                colourpicker::colourInput("ellipsecol",
                                                          span(" Prediction Interval Color",
                                                               style='font-family: Arial, Helvetica, sans-serif;',
                                                               tags$a(
                                                                 tags$i(class='fa fa-question-circle'),
                                                                 href = "#",
                                                                 onclick="alert('The color used for plotting the ellipses.');return false;"
                                                               )),
                                                          "#4D1C1C"),
                                colourpicker::colourInput("color2",
                                                          span("SROC Color",
                                                               style='font-family: Arial, Helvetica, sans-serif;',
                                                               tags$a(
                                                                 tags$i(class='fa fa-question-circle'),
                                                                 href = "#",
                                                                 onclick="alert('Color of SROC.');return false;"
                                                               )),
                                                          "#DE2A2A"),
                                
                                sliderInput("lty", 
                                            span( "line type",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('1. solld; 2. dashed; 3. dotted; 4. dotdash; 5. longdash; 6. twodash');return false;"
                                                  )),
                                            min = 1, max = 6, value = 2, step = 1
                                ),
                                
                                
                                sliderInput("lwd","line width",
                                            min = 1, max = 6, value = 2, step = 1
                                ),
                                sliderInput("plotheight_dig1", 
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_dig1", 
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_dig3", open = NULL,
                              bsCollapsePanel(
                                "Customize the Crosshair Plot", class = NULL , style = "info",
                                selectInput("method", 
                                            span( "Method",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Method used to calculate the confidence intervals for sensitivities,specificities and false positive rates.');return false;"
                                                  )),
                                            choices = c( "wald", "wilson", "agresti-coull",
                                                         "jeffreys", "modified wilson",
                                                         "modified jeffreys", "clopper-pearson",
                                                         "arcsine", "logit", "witting" ),"wilson"),
                                
                                sliderInput("pch", 
                                            span( "Point Estimates Symbol",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Symbol used to plot point estimates');return false;"
                                                  )),
                                            min = 1, max =25, value = 1, step = 1
                                ),
                                sliderInput("plotheight_dig2", 
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_dig2", 
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              ))
                          )
                        )
                        )
             )
             ),
                        
    ##Network Meta Analysis----
    tabPanel("Network Meta Analysis",
             hr(),
             br(), br(),
             tabsetPanel(
               ###network_dic----
               tabPanel(title = "Dichotomous Variable", 
                        value = "dichotomous variable2",
                        fluidRow(
                          column(
                            width = 9,
                            br(),
                            box(style = 'overflow-x: scroll',
                                DT::DTOutput("head_net_dic")%>% withSpinner(type=4),width=NULL,
                                tags$head(
                                  tags$style(
                                    HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }
             "
                                    )
                                  )
                                )
                            ),
                            
                            tabBox(width =NULL,
                                   tabPanel( "Network plot",style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                             fluidRow(hidden(div(id="h1_net_dic",withSpinner(type =4,plotOutput('stp1_net_dic'))))),
                                             fluidRow(downloadButton("downloadp1_net_dic", "Save PDF", icon = icon("download")),
                                                      downloadButton("downloadp1png_net_dic", "Save PNG", icon = icon("download")))
                                   )
                                   ,
                                   tabPanel(width =NULL,"Forest plot",style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                            fluidRow(hidden(div(id="h2_net_dic",withSpinner(type =4,plotOutput('stp2_net_dic'))))),
                                            fluidRow(downloadButton("downloadp2_net_dic", "Save PDF", icon = icon("download")),
                                                     downloadButton("downloadp2png_net_dic", "Save PNG", icon = icon("download")))
                                   ),
                                   tabPanel(width = NULL,"SUCRA",style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                            fluidRow(hidden(div(id="h3_net_dic",withSpinner(type =4,plotOutput('stp3_net_dic'))))),
                                            fluidRow(downloadButton("downloadp3_net_dic", "Save PDF", icon = icon("download")),
                                                     downloadButton("downloadp3png_net_dic", "Save PNG", icon = icon("download")))
                                   ),
                                   tabPanel(width = NULL,"Rank plot",style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                            fluidRow(hidden(div(id="h4_net_dic",withSpinner(type =4,plotOutput('stp4_net_dic'))))),
                                            fluidRow(downloadButton("downloadp4_net_dic", "Save PDF", icon = icon("download")),
                                                     downloadButton("downloadp4png_net_dic", "Save PNG", icon = icon("download")))
                                   ),
                                   tabPanel(width = NULL, "Heatmap", style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                            fluidRow(hidden(div(id="h5_net_dic", withSpinner(type = 4, plotOutput('stp5_net_dic'))))),
                                            fluidRow(downloadButton("downloadp5_net_dic", "Save PDF", icon = icon("download")),
                                                     downloadButton("downloadp5png_net_dic", "Save PNG", icon = icon("download")))
                                   )
                            ),
                            br(),
                            div(class = "blue-stripe-box", "This analysis takes a long time, please wait about 30 seconds."),
                            div(class = "blue-stripe-box", "After clicking the download button, please wait for a moment as the image file takes some time to generate."),
                            hr()
                          ),
                          column(
                            width = 3,
                            br(),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                actionButton("toggleAdvanced_net_dic", "Data Format",
                                             style = "color: white;
                       background-color:#3c8dbc;
                       position: relative;
                       Bottom: 4px;
                       height: 35px;
                       width: 110px;"
                                ),
                                # a(id = "toggleAdvanced", "Data Format",href = "#"),
                                shinyjs::hidden(
                                  div(id = "advanced_net_dic",
                                      strong("Please make sure all your files are in",
                                             span("' .csv '",style = "color:blue"),
                                             "format"),
                                      br(),
                                      h5("1.Rownames: study,treatment,trt_class,r,n"),
                                      p("- study: Name of the study"),
                                      p("- treatment:treatment (as factor) "),
                                      p("- trt_class:treatment class (as factor), if specified. If the class is null, this box can be empty and please choose False in 'Treatment class'"),
                                      p("- r:event count (discrete)"),
                                      p("- n:event count denominator (discrete, agd_arm only),n will be used as the sample size by default. "),
                                      h5("2.Study in the first row will be used as the reference treatment for the network by default"),
                                      br()
                                  )
                                ),
                                hr(),
                                uiOutput('file1_net_dic'),
                                actionBttn(
                                  inputId = "run_net_dic", 
                                  label = "Plot", 
                                  style = "fill", 
                                  color = "success", 
                                  icon = icon("paper-plane"), 
                                  size = "sm"
                                ),
                                actionBttn(
                                  inputId = "reset_net_dic", 
                                  label = "Reset File", 
                                  style = "fill", 
                                  color = "danger", 
                                  icon = icon("trash"), 
                                  size = "sm"
                                ),
                                tags$br(),
                                tags$br(),
                                downloadBttn(
                                  outputId = "downloadBtn_net_dic", 
                                  label = "Download Example Data", 
                                  style = "fill", 
                                  color = "warning",
                                  size = "sm"
                                ),
                                tags$br(),
                                checkboxInput("exam_net_dic","Example Data",TRUE),
                              )
                            ),
                            hr(),
                            bsCollapse(
                              id = "collapse_net_dic1", open = NULL,
                              bsCollapsePanel(
                                "Customize the Network Plot", class = NULL , style = "info",
                                selectInput("weight_edges_dic",
                                            span(   "Weight edges",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('	Weight edges by the number of studies. Default is TRUE.');return false;"
                                                    )),
                                            choices = c("TRUE","FALSE"),"TRUE"),
                                selectInput("show_trt_class_dic",
                                            span(   "Treatment class",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('Colour treatment nodes by class. if trt_class is set');return false;"
                                                    )),
                                            choices = c("TRUE","FALSE"),"TRUE"),
                                
                                sliderInput("plotheight_net_dic",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_net_dic",
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_net_dic2", open = NULL,
                              bsCollapsePanel(
                                "Customize the Forest Plot", class = NULL , style = "info",
                                selectInput("model_net_dic",
                                            span(   "Model",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('Choose fixed effect or random effect models');return false;"
                                                    )),
                                            choices = c("fixed effects model","random effects model")),
                                selectInput("consistency_dic",
                                            span(   "Consistency",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('Character string specifying the type of (in)consistency model to fit,');return false;"
                                                    )),
                                            choices = c("consistency", "ume","nodesplit")),
                                sliderInput("plotheight_net_dic2",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_net_dic2",
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_net_dic3", open = NULL,
                              bsCollapsePanel(
                                "Customize the SUCRA Plot", class = NULL , style = "info",
                                sliderInput("plotheight_net_dic3",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_net_dic3",
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_net_dic4", open = NULL,
                              bsCollapsePanel(
                                "Customize the Rank Plot", class = NULL , style = "info",
                                sliderInput("plotheight_net_dic4",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_net_dic4",
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_net_dic5", open = NULL,
                              bsCollapsePanel(
                                "Customize the Heatmap Plot", class = NULL , style = "info",
                                sliderInput("plotheight_net_dic5",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_net_dic5",
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                ),
                                
                                colourpicker::colourInput("col.square_net1",
                                                          span( "Color of the highest level",
                                                                style='font-family: Arial, Helvetica, sans-serif;',
                                                                tags$a(
                                                                  tags$i(class='fa fa-question-circle'),
                                                                  href = "#",
                                                                  onclick="alert('The colour for squares reflecting the weight of study in the meta analysis.');return false;"
                                                                )),"red"
                                ),
                                colourpicker::colourInput("col.square_net2",
                                                          span( "Color of the least level",
                                                                style='font-family: Arial, Helvetica, sans-serif;',
                                                                tags$a(
                                                                  tags$i(class='fa fa-question-circle'),
                                                                  href = "#",
                                                                  onclick="alert('The colour for squares reflecting the weight of study in the meta analysis.');return false;"
                                                                )),"white"
                                ),
                                colourpicker::colourInput(
                                  "col.text_net_dic",
                                  span("Color of the numbers", style = 'font-family: Arial, Helvetica, sans-serif;',
                                       tags$a(tags$i(class = 'fa fa-question-circle'),
                                              href = "#",
                                              onclick = "alert('The colour for numbers inside the heatmap squares.');return false;")),
                                  "black"
                                )
                              )),
                            hr()
                          )
                        )
               ),
               ###network_con----
               tabPanel(title = "Continuous Variable", 
                        value = "continuous variable2",
                        fluidRow(
                          column(
                            width = 9,
                            br(),
                            box(style = 'overflow-x: scroll',
                                DT::DTOutput("head_net_con")%>% withSpinner(type=4),width=NULL,
                                tags$head(
                                  tags$style(
                                    HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             width: 100%;
             max-width: 300px;
             margin-left: auto;
             margin-right: auto;
             }
             "
                                    )
                                  )
                                )
                            ),
                            tabBox(width =NULL,
                                   tabPanel( "Network plot",style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                             fluidRow(hidden(div(id="h1_net_con",withSpinner(type =4,plotOutput('stp1_net_con'))))),
                                             fluidRow(downloadButton("downloadp1_net_con", "Save PDF", icon = icon("download")),
                                                      downloadButton("downloadp1png_net_con", "Save PNG", icon = icon("download")))
                                   )
                                   ,
                                   tabPanel(width =NULL,"Forest plot",style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                            fluidRow(hidden(div(id="h2_net_con",withSpinner(type =4,plotOutput('stp2_net_con'))))),
                                            fluidRow(downloadButton("downloadp2_net_con", "Save PDF", icon = icon("download")),
                                                     downloadButton("downloadp2png_net_con", "Save PNG", icon = icon("download")))
                                   ),
                                   tabPanel(width = NULL,"SUCRA",style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                            fluidRow(hidden(div(id="h3_net_con",withSpinner(type =4,plotOutput('stp3_net_con'))))),
                                            fluidRow(downloadButton("downloadp3_net_con", "Save PDF", icon = icon("download")),
                                                     downloadButton("downloadp3png_net_con", "Save PNG", icon = icon("download")))
                                   ),
                                   tabPanel(width = NULL,"Rank plot",style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                            fluidRow(hidden(div(id="h4_net_con",withSpinner(type =4,plotOutput('stp4_net_con'))))),
                                            fluidRow(downloadButton("downloadp4_net_con", "Save PDF", icon = icon("download")),
                                                     downloadButton("downloadp4png_net_con", "Save PNG", icon = icon("download")))
                                   ),
                                   tabPanel(width = NULL, "Heatmap", style = 'height:465px; overflow-x: scroll; overflow-y: scroll',
                                            fluidRow(hidden(div(id="h5_net_con", withSpinner(type = 4, plotOutput('stp5_net_con'))))),
                                            fluidRow(downloadButton("downloadp5_net_con", "Save PDF", icon = icon("download")),
                                                     downloadButton("downloadp5png_net_con", "Save PNG", icon = icon("download")))
                                   )
                            ),
                            br(),
                            div(class = "blue-stripe-box", "This analysis takes a long time, please wait about 30 seconds."),
                            div(class = "blue-stripe-box", "After clicking the download button, please wait for a moment as the image file takes some time to generate."),
                            hr()
                          ),
                          column(
                            width = 3,
                            br(),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                actionButton("toggleAdvanced_net_con", "Data Format",
                                             style = "color: white;
                       background-color:#3c8dbc;
                       position: relative;
                       Bottom: 4px;
                       height: 35px;
                       width: 100px;"
                                ),
                                # a(id = "toggleAdvanced", "Data Format",href = "#"),
                                shinyjs::hidden(
                                  div(id = "advanced_net_con",
                                      strong("Please make sure all your files are in",
                                             span("' .csv '",style = "color:blue"),
                                             "format"),
                                      br(),
                                      h5("1.Rownames: study,treatment,mean,std.dev,n,trt_class"),
                                      p("- study: Name of the study"),
                                      p("- treatment:treatment (as factor) "),
                                      p("- trt_class:treatment class (as factor), if specified. If the class is null, this box can be empty and please choose False in 'Treatment class'"),
                                      p("- std.dev:standard deviation"),
                                      p("- mean:mean，or other continuous outcomea"),
                                      p("- n:sample size"),
                                      h5("2.Study in the first row will be used as the reference treatment for the network by default"),
                                      br()
                                  )
                                ),
                                hr(),
                                uiOutput('file1_net_con'),
                                actionBttn(
                                  inputId = "run_net_con", 
                                  label = "Plot", 
                                  style = "fill", 
                                  color = "success", 
                                  icon = icon("paper-plane"), 
                                  size = "sm"
                                ),
                                actionBttn(
                                  inputId = "reset_net_con", 
                                  label = "Reset File", 
                                  style = "fill", 
                                  color = "danger", 
                                  icon = icon("trash"), 
                                  size = "sm"
                                ),
                                tags$br(),
                                tags$br(),
                                downloadBttn(
                                  outputId = "downloadBtn_net_con", 
                                  label = "Download Example Data", 
                                  style = "fill", 
                                  color = "warning",
                                  size = "sm"
                                ),
                                tags$br(),
                                checkboxInput("exam_net_con","Example Data",TRUE),
                              )
                            ),
                            hr(),
                            bsCollapse(
                              id = "collapse_net_con1", open = NULL,
                              bsCollapsePanel(
                                "Customize the Network Plot", class = NULL , style = "info",
                                selectInput("weight_edges_con",
                                            span(   "Weight edges",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('	Weight edges by the number of studies. Default is TRUE.');return false;"
                                                    )),
                                            choices = c("TRUE","FALSE"),"TRUE"),
                                selectInput("show_trt_class_con",
                                            span(   "Treatment class",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('Colour treatment nodes by class. if trt_class is set');return false;"
                                                    )),
                                            choices = c("TRUE","FALSE"),"TRUE"),
                                
                                sliderInput("plotheight_net_con",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_net_con",
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_net_con2", open = NULL,
                              bsCollapsePanel(
                                "Customize the Forest Plot", class = NULL , style = "info",
                                selectInput("model_net_con",
                                            span(   "Model",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('Choose fixed effect or random effect models');return false;"
                                                    )),
                                            choices = c("fixed effects model","random effects model")),
                                selectInput("consistency_con",
                                            span(   "Consistency",
                                                    style='font-family: Arial, Helvetica, sans-serif;',
                                                    tags$a(
                                                      tags$i(class='fa fa-question-circle'),
                                                      href = "#",
                                                      onclick="alert('Character string specifying the type of (in)consistency model to fit,');return false;"
                                                    )),
                                            choices = c("consistency", "ume","nodesplit")),
                                sliderInput("plotheight_net_con2",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_net_con2",
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_net_con3", open = NULL,
                              bsCollapsePanel(
                                "Customize the SUCRA Plot", class = NULL , style = "info",
                                sliderInput("plotheight_net_con3",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_net_con3",
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_net_con4", open = NULL,
                              bsCollapsePanel(
                                "Customize the Rank Plot", class = NULL , style = "info",
                                sliderInput("plotheight_net_con4",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_net_con4",
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                )
                              )),
                            hr(),
                            bsCollapse(
                              id = "collapse_net_con5", open = NULL,
                              bsCollapsePanel(
                                "Customize the Heatmap Plot", class = NULL , style = "info",
                                sliderInput("plotheight_net_con5",
                                            span( "Height (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 460, step = 20
                                ),
                                sliderInput("plotwidth_net_con5",
                                            span( "Width (pixels)",
                                                  style='font-family: Arial, Helvetica, sans-serif;',
                                                  tags$a(
                                                    tags$i(class='fa fa-question-circle'),
                                                    href = "#",
                                                    onclick="alert('Plot size in units (pixels)');return false;"
                                                  )),
                                            min = 400, max = 1000, value = 600, step = 20
                                ),
                                
                                colourpicker::colourInput("col.square_net_con1",
                                                          span( "Color of the highest level",
                                                                style='font-family: Arial, Helvetica, sans-serif;',
                                                                tags$a(
                                                                  tags$i(class='fa fa-question-circle'),
                                                                  href = "#",
                                                                  onclick="alert('The colour for squares reflecting the weight of study in the meta analysis.');return false;"
                                                                )),"red"
                                ),
                                colourpicker::colourInput("col.square_net_con2",
                                                          span( "Color of the least level",
                                                                style='font-family: Arial, Helvetica, sans-serif;',
                                                                tags$a(
                                                                  tags$i(class='fa fa-question-circle'),
                                                                  href = "#",
                                                                  onclick="alert('The colour for squares reflecting the weight of study in the meta analysis.');return false;"
                                                                )),"white"
                                ),
                                colourpicker::colourInput(
                                  "col.text_net_con",
                                  span("Color of the numbers", style = 'font-family: Arial, Helvetica, sans-serif;',
                                       tags$a(tags$i(class = 'fa fa-question-circle'),
                                              href = "#",
                                              onclick = "alert('The colour for numbers inside the heatmap squares.');return false;")),
                                  "black"
                                )
                              )),
                            hr()
                          )
                        )
               )
             )
    ),
    ##Help----
    tabPanel("Help",
             hr(),
             br(), br(),
             fluidPage(
               wellPanel(
                 style = "background-color: #f7f7f7; padding: 20px; border-radius: 10px;",
                 h2("User Guide"),
                 p("There are tutorials about how to use Onlinemeta. Only Chinese is available provisionally, please use Google or other translators if needed."),
                 tags$ul(
                   tags$li(a(href = "https://mp.weixin.qq.com/s/nl8VwzPIszJTWA3LXoFvVQ", "Introduction", target = "_top")),
                   tags$li(a(href = "https://mp.weixin.qq.com/s/_CPg2ZXLp0A4CrXDPxII4Q", "Risk Bias Analysis", target = "_top")),
                   tags$li(a(href = "https://mp.weixin.qq.com/s/gx3R9iQHYvzcrRYK1Um63w", "Meta for Dichotomous Variable or Single Dichotomous Variable", target = "_top")),
                   tags$li(a(href = "https://mp.weixin.qq.com/s/bcoeyd6Cqd1A5EeJaxIJUg", "Meta for Continuous Variable or Single Continuous Variable", target = "_top")),
                   tags$li(a(href = "https://mp.weixin.qq.com/s/BmsTyycwSPHMSlYG6nladg", "Meta for Survival Variable and Diagnostic Test", target = "_top"))
                 ),
                 p("If the issue persists or you have additional feedback, you are welcome to contact us via the Comment Box at the bottom of this page or join our online feedback group for more immediate and convenient support: "),
                 tags$strong("QQ Group (ID: 981578710)."),
               ),
               wellPanel(
                 style = "background-color: #e7e7f7; padding: 20px; border-radius: 10px;",
                 h2("FAQ"),
                 tags$strong("1. Why does the error 'Maximum upload size exceeded' appear?"),
                 p("The file size should be less than 30MB."),
                 tags$strong("2. Why does the notification 'Please upload a valid CSV file' appear?"),
                 tags$ol(
                   tags$li("Please check whether you have uploaded the file."),
                   tags$li("Please check whether the file is in '.csv' format."),
                   tags$li("Please check whether the file column names meet the requirements (Details are listed at 'Data Format').")
                 ),
                 tags$strong("3. Why does the column name occur unreadable code?"),
                 p("This may be caused by saving an Excel file as a CSV file."),
                 p("Please click 'File - Export - Change File Type' in the file interface to check whether the File Type is '.csv'."),
                 tags$strong("4. How to choose fixed or random effect models?"),
                 p("You can choose them from 'Model' in Customize.")
               ),
               wellPanel(
                 style = "background-color: #ADD8E6; color: white; padding: 15px;",
                 strong("Comment Box", style = 'font-size:18px;'),
                 icon("envelope"),
                 fluidRow(
                   column(8,
                          textInput(inputId = "contact", 
                                    label = "Name/Email (optional)", width = "60%"),
                          textAreaInput(inputId = "comment", 
                                        label = "Comment", placeholder = "Enter your comment here", width = "100%", height = "100px"),
                          actionButton("submit_commentbtn", 
                                       "Submit Comment")
                   )
                 )
               )
             )
  )
)
)

