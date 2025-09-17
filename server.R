library(shiny)


library(multinma)#网状meta分析的主心骨
library(RColorBrewer)#提供颜色调色板
library(reshape2)
library(ggpubr)
library(colourpicker)#颜色选择
library(colorspace)
library(shinycssloaders)
library(shinydashboard)
library(reactable)
library(robvis)#风险偏倚分析的主心骨
library("data.table")
library("meta")#meta分析的主心骨
library(ComplexHeatmap)#用于绘制复杂热图
library(circlize)#提供颜色映射功能
library(plyr)
library(metamisc)
library("mada")
library("shinyFeedback")
library("shinyMatrix")
library(DT)#数据表格显示
library(svglite)
library(slickR)#轮播图
library(XML)
library("shinyLP")
library(colourpicker)#颜色选择
library(shinycssloaders)#加载显示器
library(shinyjs)#用于增强Shiny应用的交互性
library(shinydisconnect)
library(metawho)



#extra
library(jsonlite)
library(ggplot2)
library(dplyr)
library(reshape2)
library(shinyWidgets)
library(emayili)

server <- function(input, output, session) {
  #很重要的定义,td 是一个临时目录路径
  td <- tempdir()
  
  my_images <- c("www/Mate1.jpg",
                 "www/Mate2.jpg",
                 "www/Mate3.jpg",
                 "www/Mate4.jpg",
                 "www/Mate5.jpg")
  output$my_slick <- renderSlickR({
    slickR(
      my_images,
      slideId = 'slick_images',
      width='60%'
    )+settings(dots=T,autoplay=T,autoplaySpeed=3000,centerPadding='80px')
  }) 
  #全局设置----
  nms_group <- c("study", "group") 
  nms_dic <- c( "study", "event.e", "n.e", "event.c", "n.c") 
  nms_con <- c( "study", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c") 
  nms_dig <- c("studynames", "TP","FP","TN","FN")
  nms_sig_dic <- c( "study", "case", "number")
  nms_sig_con <- c("study", "n", "mean", "sd")
  nms_rate<-c("study", "HR", "lower", "upper")
  nms_rate2 <- c("study", "OE", "V")
  nms_de<-c("trial","subgroup","hr","ci.lb","ci.ub")
  nms_net_dic<-c(	"study",	"treatment",	"trt_class",	"r","n")
  nms_net_con<-c(	"study",	"treatment",	"mean",	"std.dev",	"n",	"trt_class")
  #点击显示格式信息-----------
  shinyjs::onclick("toggleAdvanced",
                   shinyjs::toggle(id = "advanced", anim = TRUE))
  shinyjs::onclick("toggleAdvanced_ris",
                   shinyjs::toggle(id = "advanced_ris", anim = TRUE))
  shinyjs::onclick("toggleAdvanced_dic",
                   shinyjs::toggle(id = "advanced_dic", anim = TRUE))
  shinyjs::onclick("toggleAdvanced_con",
                   shinyjs::toggle(id = "advanced_con", anim = TRUE))
  shinyjs::onclick("toggleAdvanced_sig_con",
                   shinyjs::toggle(id = "advanced_sig_con", anim = TRUE))
  shinyjs::onclick("toggleAdvanced_sig_dic",
                   shinyjs::toggle(id = "advanced_sig_dic", anim = TRUE))
  shinyjs::onclick("toggleAdvanced_dig",
                   shinyjs::toggle(id = "advanced_dig", anim = TRUE))
  shinyjs::onclick("toggleAdvanced_de",
                   shinyjs::toggle(id = "advanced_de", anim = TRUE))
  shinyjs::onclick("toggleAdvanced_rate",
                   shinyjs::toggle(id = "advanced_rate", anim = TRUE))
  shinyjs::onclick("toggleAdvanced_rate2",
                   shinyjs::toggle(id = "advanced_rate2", anim = TRUE))
  shinyjs::onclick("toggleAdvanced_net_dic",
                   shinyjs::toggle(id = "advanced_net_dic", anim = TRUE))
  shinyjs::onclick("toggleAdvanced_net_con",
                   shinyjs::toggle(id = "advanced_net_con", anim = TRUE))
  
  #邮件收发
  labelMandatory <- function(label) {
    tagList(
      label,
      span("*", class = "mandatory_star")
    )
  }
  
  observeEvent(input$submit_commentbtn, {
    req(input$comment)
    smtp <- emayili::server(
      host = "smtp.163.com",
      port = 25,
      username = "david_yr@163.com",
      password = "QRmEaRWXGY6aSApT"
    )
    
    email <- envelope() %>%
      from("david_yr@163.com") %>%
      to("david_yr@163.com") %>%
      cc("luopeng@smu.edu.cn") %>%
      subject(paste("ONLINEMETA-SHINY_FEEDBACK:", input$contact)) %>%
      text(paste(input$comment, "from:", input$contact))
    
    smtp(email)
    
    show_alert(
      title = "Success",
      text = "Thanks, your response was submitted successfully! We will get back to you as soon as possible.",
      type = "success"
    )
  })
  
  
  #risk_bias----
  
  ##histogram----
  dt1<- reactive({
    inFile1 <-input$file1
    ext <- tools::file_ext(inFile1$datapath) 
    if(input$exam){
      data <- read.csv("www/Risk bias.csv",sep=",")
      return(data)
    }else{
      if (!is.null(inFile1)&& ext == "csv"){
        dat <-read.csv(inFile1$datapath,sep=",")
        return(dat)
      }else{
        shiny::showNotification("Primary file: Please upload a valid csv file", type = "error")
        NULL
      }
    }
    
  })
   
  output$file1 <- renderUI({
    fileInput('file1', 'Choose CSV File',accept = ".csv")
    
  })
  observeEvent(input$reset, {
    output$file1 <- renderUI({
      fileInput('file1', 'Choose CSV File',accept = ".csv") #placeholder = "You didn't choose any files to test, so select beside"
      
    })
  })
  output$exam <- renderUI({
    checkboxInput("exam","Example Data",TRUE)
  })
  observeEvent(input$file1, {
    output$exam <- renderUI({
      checkboxInput("exam","Example Data",FALSE)
    })
  }) 
  
  
  # Reset checkbox when file is uploaded
  observeEvent(input$file1, {
    updateCheckboxInput(session, "exam", value = FALSE)  # 重置复选框为未选中
  })
  
  output$head<- renderDT(
    dt1(),
    options = list(pageLength = 5)
  )
  
  plot_fun<-function(raw,chrx,Msize,color_histogram) {
    robvis::rob_summary(raw,tool="ROB1",overall = input$overall,
                        weighted = input$weight,
                        colour=color_histogram)+
      ggtitle(chrx)+
      theme(plot.title = element_text(hjust=0.5,size=Msize))
  }
  
  #download 
  output$downloadBtn <- downloadHandler(
    filename = function() {
      paste("risk bias", "csv",sep='.')
    },
    content = function(file) {
      myfile <- srcpath <- 'www/Risk bias.csv'
      file.copy(myfile, file)
    }
  )
  output$downloadBtn_ris <- downloadHandler(
    filename = function() {
      paste("risk bias", "csv",sep='.')
    },
    content = function(file) {
      myfile <- srcpath <- 'www/Risk bias.csv'
      file.copy(myfile, file)
    }
  )
  observeEvent(input$run,{
    
    p1<- eventReactive(input$run, { 
      my_color <-colorRampPalette(brewer.pal(8,input$colpal))(3)
      p1<-plot_fun(dt1(), input$maintitle,input$Msize,my_color)
      pdf(paste0(td,"/p1.pdf"),
          width = input$plotwidth/72,
          height = input$plotheight/72)
      print(p1)
      dev.off()
      
      png(paste0(td,"/p1.png"),
          width =5*input$plotwidth,
          height =5*input$plotheight,
          res=300)
      print(p1)
      dev.off()
      p1
    }
    )
    shinyjs::show("h")
    
    output$distPlot <- renderPlot({
      print(p1())
    }) 
  })
  
  
  
  
  
  
  #Heatmap--------------------------
  #文件输入与示例数据勾选
  output$file1_ris <- renderUI({
    fileInput('file1_ris', 'Choose CSV File',accept = ".csv")
  })
  observeEvent(input$reset_ris, {
    output$file1_ris <- renderUI({
      fileInput('file1_ris', 'Choose CSV File',accept = ".csv")
    })
  })
  output$exam_ris <- renderUI({
    checkboxInput("exam_ris","Example Data")
  })
  
  observeEvent(input$file1_ris, {
    output$exam_ris <- renderUI({
      checkboxInput("exam_ris","Example Data",FALSE)
    })
  }) 
  
  # Reset checkbox when file is uploaded
  observeEvent(input$file1_ris, {
    updateCheckboxInput(session, "exam_ris", value = FALSE)  # 重置复选框为未选中
  })

  
 
  
  #数据处理
  raw_ris<- reactive({
    inFile1_ris <- input$file1_ris
    ext <- tools::file_ext(inFile1_ris$datapath)
    
    
    if(input$exam_ris){
      data <- read.csv("www/Risk bias.csv",sep=",")
      return(data) 
    }else{
      if (!is.null(input$file1_ris)&& ext == "csv"){
        raw_ris<-read.csv(input$file1_ris$datapath,sep=",",fileEncoding="GBK")
        return(raw_ris)
      }else{
        shiny::showNotification("Primary file: Please upload a valid csv file", type = "error")
        NULL
      }
    }
    
  })
  
  output$head_ris <- renderDT(
    raw_ris(),
    options = list(pageLength = 5)
  )
  
  # 定义绘图函数
  plot_heatmap_with_bars <- function() {
    # 读取数据
    if(input$exam_ris) {
      raw_ris<-read.csv("www/Risk bias.csv",sep=",")
    } else {
      raw_ris<-read.csv(input$file1_ris$datapath,sep=",",fileEncoding="GBK")
    }
    
    # 数据预处理
    risk_ris <- raw_ris[-1]
    a <- as.numeric(ncol(risk_ris))
    risk_ris <- risk_ris[,-c(a-1,a)]
    
    for (i in (1:ncol(risk_ris))) {
      risk_ris[, i] <- gsub("Unclear", "0", risk_ris[, i])
      risk_ris[, i] <- gsub("Low", "1", risk_ris[, i])
      risk_ris[, i] <- gsub("Medium", "2", risk_ris[, i])
      risk_ris[, i] <- gsub("High", "3", risk_ris[, i])
    }
    
    # 转换为数值并排序
    dt_ris <- as.data.frame(lapply(risk_ris, as.numeric))
    dt_ris$sum <- rowSums(dt_ris)
    row.names(dt_ris) <- raw_ris[, 1]
    dt_ris <- dt_ris[order(dt_ris$sum), ]
    dt_ris <- dt_ris[,-ncol(dt_ris)]
    
    # 列排序
    dt_ris <- rbind(dt_ris, colSums(dt_ris))
    column_order <- order(as.numeric(dt_ris[nrow(dt_ris), ]))
    dt_ris <- dt_ris[, column_order]
    dt_ris <- dt_ris[-nrow(dt_ris), ]
    
    # 颜色函数
    col_fun_ris <- colorRamp2(c(0, 1, 2, 3), brewer.pal(n = 4, input$Heatmapcolor_ris))
    
    # 计算列和行的平均值
    dt1_ris <- as.data.frame(colSums(dt_ris) / ncol(dt_ris))
    colnames(dt1_ris) <- "Column_Average"
    
    dt2_ris <- as.data.frame(rowSums(dt_ris) / nrow(dt_ris))
    colnames(dt2_ris) <- "Row_Average"
    
    # 创建颜色条
    plot_bar1_ris <- function(dt, barcolor) {
      HeatmapAnnotation(df = dt, show_annotation_name = FALSE,
                        col = list(Column_Average = colorRamp2(c(0, 3), c("white", barcolor))))
    }
    
    plot_bar2_ris <- function(dt, barcolor) {
      rowAnnotation(df = dt, show_annotation_name = FALSE,
                    col = list(Row_Average = colorRamp2(c(0, 3), c("white", barcolor))))
    }
    
    # 设置颜色
    barcolor1 <- input$gp_bar1 # 替换为你想要的颜色
    barcolor2 <- input$gp_bar2 # 替换为你想要的颜色
    
    # 创建注释对象
    bar1_ris <- plot_bar1_ris(dt1_ris, barcolor1)
    bar2_ris <- plot_bar2_ris(dt2_ris, barcolor2)
    
    # 将数据框转换为矩阵
    dt_ris_matrix <- as.matrix(dt_ris)
    
    # 绘制热图并添加颜色条
    heatmap_plot <- Heatmap(dt_ris_matrix, 
                            col = col_fun_ris, 
                            top_annotation = bar1_ris,
                            right_annotation = bar2_ris,
                            cluster_columns = FALSE,
                            cluster_rows = FALSE,
                            show_row_dend = FALSE,
                            rect_gp = gpar(col = "white", lwd = 1),
                            row_names_side = "left",
                            row_names_gp = gpar(fontsize = input$row_names_ris),
                            column_names_gp = gpar(fontsize = input$column_names_ris),
                            heatmap_legend_param = list(
                              title = "Risk Bias",
                              at = c(0, 1, 2, 3),
                              legend_height = unit(input$legend_height_ris, "cm"),
                              labels = c("Unclear", "Low", "Medium", "High")
                            ))
    
    # 显示热图
    draw(heatmap_plot)
  }
  
  # 显示
  observeEvent(input$run_ris, {
    shinyjs::show("h_ris")
    output$distPlot_heatmap_ris <- renderPlot({
      print(plot_heatmap_with_bars())
      
    })
    
    pdf(paste0(td,"/p1_ris.pdf"),
        width = input$plotwidth_ris/72,
        height = input$plotheight_ris/72)
    print(plot_heatmap_with_bars())
    dev.off()
    
    png(paste0(td,"/p1_ris.png"),
        width =5*input$plotwidth_ris,
        height =5*input$plotheight_ris,
        res=300)
    print(plot_heatmap_with_bars())
    dev.off()
    
  })
  
  
  
  #meta_dic----
  settings.meta("revman5")
  output$file2_dic <- renderUI({
    fileInput('file2_dic', 'Choose Primary File', accept = ".csv")
  })
  observeEvent(input$reset_dic, {
    output$file2_dic <- renderUI({
      fileInput('file2_dic', 'Choose Primary File', accept = ".csv")
    })
  })
  output$group_dic <- renderUI({
    fileInput('group_dic', 'Choose Group File', accept = ".csv")
  })
  observeEvent(input$reset_dic, {
    output$group_dic <- renderUI({
      fileInput('group_dic', 'Choose Group File', accept = ".csv")
    })
  })
  
  output$exam_dic <- renderUI({
    checkboxInput("exam_dic","Example Data",TRUE)
  })
  observeEvent(input$file2_dic, {
    updateCheckboxInput(session, "exam_dic", value = FALSE)
    #output$exam_dic <- renderUI({
      #checkboxInput("exam_dic","Example Data",FALSE)
    #})
  }) 
  observeEvent(input$group_dic, {
    updateCheckboxInput(session, "exam_dic", value = FALSE)
    #output$exam_dic <- renderUI({
      #checkboxInput("exam_dic","Example Data",FALSE)
    #})
  }) 
  
 
  
  
  
  raw_dic1<-reactive({
    req(input$file2_dic)
    try(read.csv(input$file2_dic$datapath, header = TRUE,fileEncoding="GBK"))
  })
  
  d1_dic <- reactive({
    inFile1_dic <- input$file2_dic
    ext <- tools::file_ext(inFile1_dic$datapath)
    if(input$exam_dic){
      data <- read.csv("www/forest_dic.csv",sep=",")
      return(data) }else{
        if (!is.null(input$file2_dic)&& ext == "csv" &&
            all(nms_dic%in% names(raw_dic1()))){
          return(raw_dic1())
        }else{
          shiny::showNotification("Primary file: Please upload a valid csv file", type = "error")
          NULL
        }
      }
  })
  
  
  
  output$head_dic <- renderDT(
    d1_dic(),
    options = list(pageLength = 5)
  )
  
  group_dic1<-reactive({
    req(input$group_dic)
    try(read.csv(input$group_dic$datapath, header = TRUE,fileEncoding="GBK"))
  })
  d2_dic <- reactive({
    inFile2_dic<- input$group_dic
    ext_group <- tools::file_ext(inFile2_dic$datapath)
    if(input$exam_dic){
      data_gp <- read.csv("www/group_dic.csv",sep=",")
      return(data_gp) }else{
        if (!is.null(input$group_dic)&& ext_group == "csv"&&
            all(nms_group%in% names(group_dic1())) ){
          return(group_dic1())
        }else{
          shiny::showNotification("Group file: Please upload a valid csv file ", type = "error")
          NULL
        }
      }
  })
  
  output$head2_dic <- renderDT(
    d2_dic(),
    options = list(pageLength = 5)
  )
  
  output$downloadBtn_dic <- downloadHandler(
    filename = function() {
      paste("meta_dic", "zip",sep='.')
    },
    content = function(file) {
      myfile <- srcpath <- 'www/meta_dic.zip'
      file.copy(myfile, file)
    }
  )
  ##定义一个进行meta分析的函数（没有子组）
  meta<-function(metadata,sm,a,b,label.e,label.c,
                 group = NULL){                                     
    metabin(event.e, n.e,event.c,n.c,
            data=metadata,sm=sm,
            label.e = label.e,label.c = label.c,
            random =a,common =b,studlab=study,overall=T,overall.hetstat=T) }
  ##定义一个进行带子组的meta分析的函数
  meta_group<-function(metadata,sm,a,b,label.e,label.c,group){    
    metabin(event.e, n.e,event.c,n.c,
            data=metadata,sm=sm,
            label.e = label.e,label.c = label.c,
            random =a,common =b,
            subgroup = group,studlab=study,overall=T,overall.hetstat=T) }
  
  ##当按下“run_dic”按钮时触发的事件
  metaresult_dic<- eventReactive(input$run_dic, {
    if(input$exam_dic) {
      metafile_dic<-read.csv("www/forest_dic.csv",sep=",")
    } else {
      metafile_dic<-read.csv(input$file2_dic$datapath,sep=",",fileEncoding="GBK")
    }
    
    sm_dic <- input$sm_dic
    b_dic<-ifelse(input$model_dic=="fixed effects model",TRUE,FALSE)
    a_dic<-ifelse(input$model_dic=="random effects model",TRUE,FALSE)
    label.e_dic<-input$label.e_dic
    label.c_dic<-input$label.c_dic
    if(!is.null(input$group_dic)|input$exam_dic){
      
      if(input$exam_dic) {
        group_dic<-read.csv("www/group_dic.csv",sep=",")[,2]
      } else {
        group_dic<-read.csv(input$group_dic$datapath,sep=",",fileEncoding="GBK")[,2]
      }
      
      
      metaresult_dic<-meta_group(metafile_dic,sm_dic,a_dic,b_dic,label.e_dic,label.c_dic, group_dic)
      
    }else{
      metaresult_dic<-meta(metafile_dic,sm_dic,a_dic,b_dic,label.e_dic,label.c_dic)
    }
    # 生成森林图的PDF和PNG文件
    pdf(paste0(td,"/p1_dic.pdf"),
        width = input$plotwidth_dic/72,
        height = input$plotheight_dic/72
    )
    par(oma=c(0,5,1,1), mar=c(0,2,2,2))#c(bottom, left, top, right)
    meta::forest(metaresult_dic,digits=3,col.square=input$col.square_dic,
                 col.square.lines='white',col.study=input$col.study_dic,
                 fontsize=input$fontsize_dic
    )
    dev.off()
    
    png(paste0(td,"/p1_dic.png"),
        width = 5*input$plotwidth_dic,
        height =5* input$plotheight_dic,
        res=300)
    par(oma=c(2,0,1,0), mar=c(0,5,2,2))#c(bottom, left, top, right)
    
    meta::forest(metaresult_dic,digits=3,col.square=input$col.square_dic,
                 col.square.lines='white',col.study=input$col.study_dic,
                 fontsize=input$fontsize_dic
    )
    dev.off()
    trimfill_dic<-ifelse(input$trimfill_dic=="TRUE","T","F")
    pdf(paste0(td,"/p2_dic.pdf"),
        width = input$plotwidth_dic/72,
        height = input$plotheight_dic/72)
    par(mar=c(2,3,1,3))
    par(pty = "s")  # 修改：设置图形区域为正方形
    if(trimfill_dic=="T"){
      meta::funnel(trimfill(metaresult_dic))
    }else{
      meta::funnel(metaresult_dic)
    }
    dev.off()
    
    png(paste0(td,"/p2_dic.png"),
        width = 5*input$plotwidth_dic,
        height = 5*input$plotheight_dic,
        res=300)
    par(mar=c(2,3,1,3))
    par(pty = "s")  # 修改：设置图形区域为正方形
    if(trimfill_dic=="T"){
      meta::funnel(trimfill(metaresult_dic))
    }else{
      meta::funnel(metaresult_dic)
    }
    dev.off()
    metaresult_dic
  })
  
  
  # 显示比较结果表格
  output$comparisonTable <- renderDT({
    meta_results <- metaresult_dic()
    
    req(meta_results)
    group_levels <- unique(meta_results$byvar)
    
    comparison_results <- data.frame(
      Comparison = character(),
      Difference_in_Effect = numeric(),
      Confidence_Interval = character(),
      P_Value = numeric(),
      stringsAsFactors = FALSE
    )
    
    if (length(group_levels) > 1) {
      for (i in 1:(length(group_levels) - 1)) {
        for (j in (i + 1):length(group_levels)) {
          effect1 <- if (input$model_dic == "random effects model") meta_results$TE.random.w[i] else meta_results$TE.fixed.w[i]
          se1 <- if (input$model_dic == "random effects model") meta_results$seTE.random.w[i] else meta_results$seTE.fixed.w[i]
          
          effect2 <- if (input$model_dic == "random effects model") meta_results$TE.random.w[j] else meta_results$TE.fixed.w[j]
          se2 <- if (input$model_dic == "random effects model") meta_results$seTE.random.w[j] else meta_results$seTE.fixed.w[j]
          
          effect_diff <- effect1 - effect2
          se_diff <- sqrt(se1^2 + se2^2)
          
          alpha <- 0.05
          z_value <- qnorm(1 - alpha/2)
          
          ci_lower <- effect_diff - z_value * se_diff
          ci_upper <- effect_diff + z_value * se_diff
          ci_text <- paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")
          
          z_score <- effect_diff / se_diff
          p_value <- 2 * (1 - pnorm(abs(z_score)))
          
          comparison_results <- rbind(comparison_results, data.frame(
            Comparison = paste0(group_levels[i], " vs ", group_levels[j]),
            Difference_in_Effect = round(effect_diff, 2),
            Confidence_Interval = ci_text,
            P_Value = round(p_value, 4)
          ))
        }
      }
    }
    
    rownames(comparison_results) <- NULL
    datatable(comparison_results)
  })
  
  
  
  
  
  # 事件：创建森林图
  p1_dic<- eventReactive(input$run_dic, {
    p1_dic<-meta::forest(metaresult_dic(),digits=3,col.square=input$col.square_dic,
                         col.square.lines='white',col.study=input$col.study_dic,
                         fontsize=input$fontsize_dic
    )
    p1_dic
  })
  
  # 事件：确定是否应用trim and fill
  trimfill_dic<-eventReactive(input$run_dic, {
    ifelse(input$trimfill_dic=="TRUE","T","F")
  })
  # 事件：计算偏倚检验的meta分析结果
  metaresult_dic_bias<-eventReactive(input$run_dic, {
    if(input$exam_dic) {
      metafile_dic<-read.csv("www/forest_dic.csv",sep=",")
    } else {
      metafile_dic<-read.csv(input$file2_dic$datapath,sep=",",fileEncoding="GBK")
    }
    sm_dic <- input$sm_dic
    b_dic<-ifelse(input$model_dic=="fixed effects model",TRUE,FALSE)
    a_dic<-ifelse(input$model_dic=="random effects model",TRUE,FALSE)
    label.e_dic<-input$label.e_dic
    label.c_dic<-input$label.c_dic
    metaresult_dic_bias<-meta(metafile_dic,sm_dic,a_dic,b_dic,label.e_dic,label.c_dic)
    metaresult_dic_bias
  })
  # 事件：计算Begg偏倚检验的P值
  Begg_p<-eventReactive(input$run_dic, {
    round(metabias(metaresult_dic_bias(), k.min = 3,method.bias = "Begg")$pval,4)
  })
  # 事件：计算Egger偏倚检验的P值
  Egger_p<-eventReactive(input$run_dic, {
    round(metabias(metaresult_dic_bias(), k.min = 3,method.bias = "Egger")$pval,4)
  })
  # 观察事件，显示结果并更新UI
  observeEvent(input$run_dic,{
    shinyjs::show("h_dic")
    shinyjs::show("f_dic")
    shinyjs::show("i_dic")
    output$distPlot_dic <- renderPlot({
      print(p1_dic()) })
    output$Funnelplot_dic <- renderPlot({
      par(pty = "s")  # 修改：设置图形区域为正方形
      if(trimfill_dic()=="T"){
        meta::funnel(trimfill(metaresult_dic()))
      }else{
        meta::funnel(metaresult_dic())
      }
    })
    output$Begg_p_text<-renderText(print(paste0("P value of Begg's test: ", Begg_p())))
    output$Egger_p_text<-renderText({print(paste0("P value of Egger's test: ", Egger_p()))})
    
  })
  
  
  
  
  
  # 处理影响分析的逻辑
  #influence_result <- eventReactive(input$run_dic, {
  #if (input$exam_dic) {
  #metafile_dic <- read.csv("www/forest_dic.csv", sep = ",")
  # } else {
  #   metafile_dic <- read.csv(input$file2_dic$datapath, sep = ",", fileEncoding = "GBK")
  # }
  
  # sm_dic <- input$sm_dic
  # b_dic <- ifelse(input$model_dic == "fixed effects model", TRUE, FALSE)
  #  a_dic <- ifelse(input$model_dic == "random effects model", TRUE, FALSE)
  # metaresult_dic <- meta(metafile_dic, sm_dic, a_dic, b_dic, input$label.e_dic, input$label.c_dic)
  
  
  influence_result <- eventReactive(input$run_dic, {
    if (input$exam_dic) {
      metafile_dic <- read.csv("www/forest_dic.csv", sep = ",")
    } else {
      metafile_dic <- read.csv(input$file2_dic$datapath, sep = ",", fileEncoding = "GBK")
    }
    
    sm_influence <- input$sm_dic
    b_influence <- ifelse(input$model_dic == "fixed effects model", TRUE, FALSE)
    a_influence <- ifelse(input$model_dic == "random effects model", TRUE, FALSE)
    metaresult_dic <- meta(metafile_dic, sm_influence, a_influence, b_influence, input$label.e_dic, input$label.c_dic)
    pooled_method <- ifelse(input$model_dic == "random effects model", "random", "fixed")
    
    # 执行影响分析
    
    metainf(metaresult_dic, pooled = pooled_method)
  })
  
  # 生成影响分析的森林图
  #output$influencePlot_dic <- renderPlot({
  #  print(meta::forest(influence_result(), digits = 3,
  #                     col.square = input$col.square_influence,
  #                     col.square.lines = 'white',
  #                     col.study = input$col.study_influence,
  #                     fontsize = input$fontsize_influence))
  #})
  
  output$influencePlot_dic <- renderPlot({
    print(meta::forest(influence_result(), digits = 3,
                       col.bg = input$col.square_influence,
                       col.border = 'white',
                       col = input$col.study_influence,
                       fontsize = input$fontsize_influence,
                       height = input$plotheight_influence / 72,
                       width = input$plotwidth_influence / 72,
                       spacing = 1.2))
  })
  
  
  
  # 添加下载 PDF 处理程序
  #output$downloadInfluence_pdf <- downloadHandler(
  #  filename = function() {
  #    paste("influence_analysis", "pdf", sep = '.')
  #  },
  #  content = function(file) {
  #    pdf(file, width = input$plotwidth_dic / 72, height = input$plotheight_dic / 72)
  #    par(oma = c(0, 5, 1, 1), mar = c(0, 2, 2, 2))
  #    meta::forest(influence_result(), digits = 3,
  #                 col.square = input$col.square_influence,
  #                 col.square.lines = 'white',
  #                 col.study = input$col.study_influence,
  #                 fontsize = input$fontsize_influence)
  #    dev.off()
  #  }
  # )
  output$downloadInfluence_pdf <- downloadHandler(
    filename = function() {
      paste("influence_analysis", "pdf", sep = '.')
    },
    content = function(file) {
      pdf(file, width = input$plotwidth_influence / 72, height = input$plotheight_influence / 72)
      par(oma = c(0, 5, 1, 1), mar = c(0, 2, 2, 2))
      meta::forest(influence_result(), digits = 3,
                   col.bg = input$col.square_influence,
                   col.border = 'white',
                   col = input$col.study_influence,
                   fontsize = input$fontsize_influence)
      dev.off()
    }
  )
  
  
  
  
  # 添加下载 PNG 处理程序
  #output$downloadInfluence_png <- downloadHandler(
  #  filename = function() {
  #    paste("influence_analysis", "png", sep = '.')
  #  },
  #  content = function(file) {
  #    png(file, width = 5 * input$plotwidth_dic, height = 5 * input$plotheight_dic, res = 300)
  #    par(oma = c(2, 0, 1, 0), mar = c(0, 5, 2, 2))
  #    meta::forest(influence_result(), digits = 3,
  #                 col.square = input$col.square_influence,
  #                 col.square.lines = 'white',
  #                 col.study = input$col.study_influence,
  #                 fontsize = input$fontsize_influence)
  #    dev.off()
  #  }
  # ) 
  
  output$downloadInfluence_png <- downloadHandler(
    filename = function() {
      paste("influence_analysis", "png", sep = '.')
    },
    content = function(file) {
      png(file, width = 5 * input$plotwidth_influence, height = 5 * input$plotheight_influence, res = 300)
      par(oma = c(2, 0, 1, 0), mar = c(0, 5, 2, 2))
      meta::forest(influence_result(), digits = 3,
                   col.bg = input$col.square_influence,
                   col.border = 'white',
                   col = input$col.study_influence,
                   fontsize = input$fontsize_influence)
      dev.off()
    }
  )
  
  #meta_con----
  
  #文件上传和重置
  output$file2_con <- renderUI({
    fileInput('file2_con', 'Choose Primary File', accept = ".csv")
  })
  observeEvent(input$reset_con, {
    output$file2_con <- renderUI({
      fileInput('file2_con', 'Choose Primary File', accept = ".csv")
    })
  })
  output$group_con <- renderUI({
    fileInput('group_con', 'Choose Group File', accept = ".csv")
  })
  observeEvent(input$reset_con, {
    output$group_con <- renderUI({
      fileInput('group_con', 'Choose Group File', accept = ".csv")
    })
  })
  #示例数据选择
  output$exam_con <- renderUI({
    checkboxInput("exam_con","Example Data",TRUE)
  })
  
  observeEvent(input$file2_con, {
    output$exam_con <- renderUI({
      checkboxInput("exam_con","Example Data",FALSE)
    })
  }) 
  observeEvent(input$group_con, {
    output$exam_con <- renderUI({
      checkboxInput("exam_con","Example Data",FALSE)
    })
  }) 
  
  
  observeEvent(input$file2_con, {
    updateCheckboxInput(session, "exam_con", value = FALSE)
  }) 
  observeEvent(input$group_con, {
    updateCheckboxInput(session, "exam_con", value = FALSE)
  }) 
  
  
  
  
  
  
  # 读取主文件的反应式函数
  raw_con1<-reactive({
    req(input$file2_con)
    try(read.csv(input$file2_con$datapath, header = TRUE,fileEncoding="GBK"))
  })
  # 处理主数据文件
  d1_con<- reactive({
    inFile1_con <- input$file2_con
    ext_con <- tools::file_ext(inFile1_con$datapath)  
    if(input$exam_con){
      data <- read.csv("www/forest_con.csv",sep=",")
      return(data) }else{
        if (!is.null(input$file2_con)&& ext_con  == "csv"&&
            all(nms_con%in% names(raw_con1()))){
          return(raw_con1())
        }else{
          shiny::showNotification("Primary file: Please upload a valid csv file", type = "error")
          NULL
        }
      }
    
  })
  # 显示数据表
  output$head_con<- renderDT(
    d1_con(),
    options = list(pageLength = 5)
  )
  #download 
  output$downloadBtn_con<- downloadHandler(
    filename = function() {
      paste("meta_con", "zip",sep='.')
    },
    content = function(file) {
      myfile <- srcpath <- 'www/meta_con.zip'
      file.copy(myfile, file)
    }
  )
  # 读取分组文件的反应式函数
  group_con1<-reactive({
    req(input$group_con)
    try(read.csv(input$group_con$datapath, header = TRUE,fileEncoding="GBK"))
  })
  # 处理分组数据文件
  d2_con<- reactive({
    inFile2_con <- input$group_con
    ext_con2 <- tools::file_ext(inFile2_con$datapath)  
    if(input$exam_con){
      data_gp <- read.csv("www/group_con.csv",sep=",")
      return(data_gp) }else{
        if (!is.null(input$group_con)&& ext_con2  == "csv"&&
            all(nms_group%in% names(group_con1()))){
          return(group_con1())
        }else{
          shiny::showNotification("Group file: Please upload a valid csv file", type = "error")
          NULL
        }
      }
  })
  # 显示分组数据表
  output$head2_con<- renderDT(
    d2_con(),
    options = list(pageLength = 5)
  )
  # Meta分析函数定义
  meta_count<-function(metadata,sm,a,b,label.e,label.c){
    metacont(n.e,mean.e,sd.e,n.c,mean.c,sd.c, 
             data=metadata,sm=sm, 
             label.e = label.e, label.c = label.c, 
             random=a,common=b,studlab=study) }
  meta_count_group<-function(metadata,sm,a,b,label.e,label.c,group){
    metacont(n.e,mean.e,sd.e,n.c,mean.c,sd.c, 
             data=metadata,sm=sm, 
             label.e = label.e, label.c = label.c, 
             random=a,common=b, subgroup = group,studlab=study) }
  # 运行Meta分析并生成图形
  metaresult_con <- eventReactive(input$run_con, {
    if(input$exam_con) {
      raw_con<-read.csv("www/forest_con.csv",sep=",")
    } else {
      raw_con<-read.csv(input$file2_con$datapath,sep=",",fileEncoding="GBK") #row=1,header=T
    }
    b_con<-ifelse(input$model_con=="fixed effects model",TRUE,FALSE)
    a_con<-ifelse(input$model_con=="random effects model",TRUE,FALSE)
    sm_con<-input$sm_con
    label.e_con<-input$label.e_con
    label.c_con<-input$label.c_con
    if(!is.null(input$group_con)|input$exam_con){
      if(input$exam_con) {
        group_con<-read.csv("www/group_con.csv",sep=",")[,2]
      } else {
        group_con<-read.csv(input$group_con$datapath,sep=",",fileEncoding="GBK")[,2]
      }
      metaresult_con<- meta_count_group(raw_con,sm_con,a_con,b_con,label.e_con,label.c_con, group_con)  #group_con$
      
    }else{
      metaresult_con<-meta_count(raw_con,sm_con,a_con,b_con,label.e_con,label.c_con)
    }
    # 生成森林图的PDF和PNG文件
    pdf(paste0(td,"/p1_con.pdf"),
        width = input$plotwidth_con/72,
        height = input$plotheight_con/72
    )
    par(oma=c(0,5,1,1), mar=c(0,2,2,2))#c(bottom, left, top, right)
    meta::forest(metaresult_con,digits=3,col.square.lines='white',
                 col.square=input$col.square_con,col.study=input$col.study_con,
                 fontsize=input$fontsize_con
    )
    dev.off()
    
    png(paste0(td,"/p1_con.png"),
        width = 5*input$plotwidth_con,
        height = 5*input$plotheight_con,
        res=300)
    par(oma=c(2,0,1,0), mar=c(0,5,2,2))
    meta::forest(metaresult_con,digits=3,col.square.lines='white',col.square=input$col.square_con,col.study=input$col.study_con,fontsize=input$fontsize_con
    )
    dev.off()
    # 生成漏斗图的PDF和PNG文件
    trimfill_con<-ifelse(input$trimfill_con=="TRUE","T","F")
    pdf(paste0(td,"/p2_con.pdf"),
        width = input$plotwidth_con/72,
        height = input$plotheight_con/72)
    par(mar=c(2,3,1,3))
    par(pty = "s")  # 修改：设置图形区域为正方形
    if(trimfill_con=="T"){
      meta::funnel(trimfill(metaresult_con))
    }else{
      meta::funnel(metaresult_con)
    }
    dev.off()
    
    png(paste0(td,"/p2_con.png"),
        width = 5*input$plotwidth_con,
        height =5* input$plotheight_con,
        res=300)
    par(mar=c(2,3,1,3))
    par(pty = "s")  # 修改：设置图形区域为正方形
    if(trimfill_con=="T"){
      meta::funnel(trimfill(metaresult_con))
    }else{
      meta::funnel(metaresult_con)
    }
    dev.off()
    
    metaresult_con
  })
  # 添加亚组间比较表格的代码
  output$comparisonTable_con <- renderDT({
    meta_results <- metaresult_con()
    
    req(meta_results)  # 确保数据可用
    group_levels <- unique(meta_results$byvar)  # 获取唯一值
    
    comparison_results <- data.frame(
      Comparison = character(),
      Difference_in_Effect = numeric(),
      Confidence_Interval = character(),
      P_Value = numeric(),
      stringsAsFactors = FALSE
    )
    
    # 根据用户选择的模型提取数据
    if (input$model_con == "random effects model") {
      effect_field <- "TE.random.w"
      se_field <- "seTE.random.w"
    } else if (input$model_con == "fixed effects model") {
      effect_field <- "TE.fixed.w"
      se_field <- "seTE.fixed.w"
    }
    
    if (length(group_levels) > 1) {
      for (i in 1:(length(group_levels) - 1)) {
        for (j in (i + 1):length(group_levels)) {
          md1 <- meta_results[[effect_field]][i]
          se1 <- meta_results[[se_field]][i]
          
          md2 <- meta_results[[effect_field]][j]
          se2 <- meta_results[[se_field]][j]
          
          md_diff <- md1 - md2
          se_diff <- sqrt(se1^2 + se2^2)
          
          alpha <- 0.05
          z_value <- qnorm(1 - alpha/2)
          
          ci_lower <- md_diff - z_value * se_diff
          ci_upper <- md_diff + z_value * se_diff
          ci_text <- paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")
          
          z_score <- md_diff / se_diff
          p_value <- 2 * (1 - pnorm(abs(z_score)))
          
          comparison_results <- rbind(comparison_results, data.frame(
            Comparison = paste0(group_levels[i], " vs ", group_levels[j]),
            Difference_in_Effect = round(md_diff, 2),
            Confidence_Interval = ci_text,
            P_Value = round(p_value, 4)
          ))
        }
      }
    }
    rownames(comparison_results) <- NULL
    datatable(comparison_results)
  })
  # 偏倚调整选项
  trimfill_con<-eventReactive(input$run_con, {
    ifelse(input$trimfill_con=="TRUE","T","F")
  })
  # 计算偏倚检验结果
  metaresult_con_bias<-eventReactive(input$run_con, {
    if(input$exam_con) {
      raw_con<-read.csv("www/forest_con.csv",sep=",")
    } else {
      raw_con<-read.csv(input$file2_con$datapath,sep=",",fileEncoding="GBK") #row=1,header=T
    }
    b_con<-ifelse(input$model_con=="fixed effects model",TRUE,FALSE)
    a_con<-ifelse(input$model_con=="random effects model",TRUE,FALSE)
    sm_con<-input$sm_con
    label.e_con<-input$label.e_con
    label.c_con<-input$label.c_con
    metaresult_con_bias<-meta_count(raw_con,sm_con,a_con,b_con,label.e_con,label.c_con)
    metaresult_con_bias
  })
  
  # 生成森林图
  p1_con<- eventReactive(input$run_con, {
    p1_con<-meta::forest(metaresult_con(),col.square.lines='white',digits=3,col.square=input$col.square_con,col.study=input$col.study_con,fontsize=input$fontsize_con
    )
    p1_con
  })
  # 计算Begg和Egger偏倚检验的p值
  Begg_p_con<-eventReactive(input$run_con, {
    round(metabias(metaresult_con_bias(), k.min = 3,method.bias = "Begg")$pval,4)
  })
  
  Egger_p_con<-eventReactive(input$run_con, {
    round(metabias(metaresult_con_bias(), k.min = 3,method.bias = "Egger")$pval,4)
  })
  
  
  
  #定义敏感性分析函数
  sensitivity_analysis_con <- function(meta_result) {
    metainf(meta_result)
  }
  #生成敏感性分析结果
  sensitivity_result_con <- eventReactive(input$run_con, {
    meta_result <- metaresult_con()
    sensitivity_analysis_con(meta_result)
  })
  
  #绘制敏感性分析森林图
  #output$influencePlot_con <- renderPlot({
  # result <- sensitivity_result_con()
  #plot(result, col.inside = "white",
  #     col.square = input$col.square_influence_con,
  #    col.study = input$col.study_influence_con,
  #    fontsize = input$fontsize_influence_con)
  # })
  
  
  #output$influencePlot_con <- renderPlot({
  #print(meta::forest(sensitivity_result_con(), digits = 3,
  #col.square = input$col.square_influence_con,
  #col.square.lines = "white",
  #col.study = input$col.study_influence_con,
  #fontsize = input$fontsize_influence_con,
  #height = input$plotheight_influence_con / 72,
  #width = input$plotwidth_influence_con / 72,
  #spacing = 0.5))
  #})
  
  
  
  
  
  
  
  
  # 运行分析后更新UI
  observeEvent(input$run_con,{
    shinyjs::show("h_con")
    shinyjs::show("f_con")
    shinyjs::show("i_con")
    output$distPlot_con <- renderPlot({
      print(p1_con())
    })
    
    output$Funnelplot_con <- renderPlot({
      par(pty = "s")  # 修改：设置图形区域为正方形
      if(trimfill_con()=="T"){
        meta::funnel(trimfill(metaresult_con()))
      }else{
        meta::funnel(metaresult_con())
      }
    })
    output$Begg_p_text_con<-renderText(print(paste0("P value of Begg's test: ", Begg_p_con())))
    output$Egger_p_text_con<-renderText({print(paste0("P value of Egger's test: ", Egger_p_con()))})
    output$influencePlot_con <- renderPlot({
      print(meta::forest(sensitivity_result_con(), digits = 3,
                         col.bg = input$col.square_influence_con,
                         col.border = "white",
                         col = input$col.study_influence_con,
                         fontsize = input$fontsize_influence_con,
                         height = input$plotheight_influence_con / 72,
                         width = input$plotwidth_influence_con / 72,
                         spacing = 1.2))
      
    })
  })
  
  
  #敏感性分析森林图下载
  output$downloadInfluence2_pdf <- downloadHandler(
    filename = function() {
      paste("influence_plot", "pdf", sep = '.')
    },
    content = function(file) {
      pdf(file, width = input$plotwidth_influence_con / 72, height = input$plotheight_influence / 72)
      print(meta::forest(sensitivity_result_con(), digits = 3,
                         col.bg = input$col.square_influence_con,
                         col.border = "white",
                         col = input$col.study_influence_con,
                         fontsize = input$fontsize_influence_con,
                         height = input$plotheight_influence_con / 72,
                         width = input$plotwidth_influence_con / 72,
                         spacing = 1.2))
      #plot(p1_con_influence())
      dev.off()
    }
  )
  
  output$downloadInfluence2_png <- downloadHandler(
    filename = function() {
      paste("influence_plot", "png", sep = '.')
    },
    content = function(file) {
      png(file, width = 5 * input$plotwidth_influence_con, height = 5 * input$plotheight_influence, res = 300)
      print(meta::forest(sensitivity_result_con(), digits = 3,
                         col.bg = input$col.square_influence_con,
                         col.border = "white",
                         col = input$col.study_influence_con,
                         fontsize = input$fontsize_influence_con,
                         height = input$plotheight_influence_con / 72,
                         width = input$plotwidth_influence_con / 72,
                         spacing = 1.2))
      #plot(p1_con_influence())
      dev.off()
    }
  )
  
  
  #meta_sig_dic----
  
  output$file2_sig <- renderUI({
    fileInput('file2_sig', 'Choose Primary File', accept = ".csv")
  })
  observeEvent(input$reset_sig_dic, {
    output$file2_sig <- renderUI({
      fileInput('file2_sig', 'Choose Primary File', accept = ".csv")
    })
  })
  output$group_sig <- renderUI({
    fileInput('group_sig', 'Choose Group File', accept = ".csv")
  })
  observeEvent(input$reset_sig_dic, {
    output$group_sig <- renderUI({
      fileInput('group_sig', 'Choose Group File', accept = ".csv")
    })
  })
  
  output$exam_sig_dic <- renderUI({
    checkboxInput("exam_sig_dic","Example Data",TRUE)
  })
  observeEvent(input$file2_sig, {
    output$exam_sig_dic <- renderUI({
      checkboxInput("exam_sig_dic","Example Data",FALSE)
    })
  }) 
  observeEvent(input$group_sig, {
    output$exam_sig_dic <- renderUI({
      checkboxInput("exam_sig_dic","Example Data",FALSE)
    })
  }) 
  
  observeEvent(input$file2_sig, {
    updateCheckboxInput(session, "exam_sig_dic", value = FALSE)
  }) 
  observeEvent(input$group_sig, {
    updateCheckboxInput(session, "exam_sig_dic", value = FALSE)
  }) 
  
  
  
  
  # 读取主文件的反应式函数
  raw_sig1<-reactive({
    req(input$file2_sig)
    try(read.csv(input$file2_sig$datapath, header = TRUE,fileEncoding="GBK"))
  })
  # 处理主数据文件
  d1_sig<- reactive({
    inFile1_sig <- input$file2_sig
    ext_sig <- tools::file_ext(inFile1_sig$datapath)  
    if(input$exam_sig_dic){
      data <- read.csv("www/forest_sig_dic.csv",sep=",")
      return(data) }else{
        if (!is.null(input$file2_sig)&& ext_sig  == "csv" &&
            all(nms_sig_dic%in% names(raw_sig1())) ){
          return(raw_sig1())
        }else{
          shiny::showNotification("Primary file: Please upload a valid csv file", type = "error")
          NULL
        }
      }
    
  })
  # 显示数据表
  output$head_sig<- renderDT(
    d1_sig(),
    options = list(pageLength = 5)
  )
  #download 
  output$downloadBtn_sig <- downloadHandler(
    filename = function() {
      paste("meta_sig_dic", "zip",sep='.')
    },
    content = function(file) {
      myfile <- srcpath <- 'www/meta_sig_dic.zip'
      file.copy(myfile, file)
    }
  )
  # 读取分组文件的反应式函数
  group_sig1<-reactive({
    req(input$group_sig)
    try(read.csv(input$group_sig$datapath, header = TRUE,fileEncoding="GBK"))
  })
  # 处理分组数据文件
  d2_sig<- reactive({
    inFile2_sig <- input$group_sig
    ext_sig2 <- tools::file_ext(inFile2_sig$datapath)  
    if(input$exam_sig_dic){
      data_gp <- read.csv("www/group_sig_dic.csv",sep=",")
      return(data_gp) }else{
        if (!is.null(input$group_sig)&& ext_sig2  == "csv"&&
            all(nms_group%in% names(group_sig1())) ){
          return(group_sig1())
        }else{
          shiny::showNotification("Group file: Please upload a valid csv file", type = "error")
          NULL
        }
      }
    
  })
  # 显示分组数据表
  output$head2_sig<- renderDT(
    d2_sig(),
    options = list(pageLength = 5)
  )
  # 定义Meta分析函数
  meta_sig<-function(metadata,sm,a,b,group = NULL){  
    metaprop(case,number,data=metadata,sm=sm, 
             random =a,common =b,studlab=study,
             incr=0.5,method.incr = "all") }
  meta_sig_group<-function(metadata,sm,a,b,group){                                     
    metaprop(case,number,data=metadata,sm=sm,
             random =a,common =b,
             subgroup = group,studlab=study,
             incr=0.5,method.incr = "all") }
  
  # 运行Meta分析并生成图形
  metaresult_sig <- eventReactive(input$run_sig, {
    if(input$exam_sig_dic) {
      raw_sig<-read.csv("www/forest_sig_dic.csv",sep=",")
    } else {
      raw_sig<-read.csv(input$file2_sig$datapath,sep=",",fileEncoding="GBK") 
    }
    b_sig<-ifelse(input$model_sig=="fixed effects model",TRUE,FALSE)
    a_sig<-ifelse(input$model_sig=="random effects model",TRUE,FALSE)
    sm_sig<-input$sm_sig
    if(!is.null(input$group_sig)|input$exam_sig_dic){
      if(input$exam_sig_dic) {
        group_sig<-read.csv("www/group_sig_dic.csv",sep=",")[,2]
      } else {
        group_sig<-read.csv(input$group_sig$datapath,sep=",",fileEncoding="GBK")[,2]
      }
      
      metaresult_sig<- meta_sig_group(raw_sig,sm_sig,a_sig,b_sig,group_sig)  #group_sig$
    }else{
      metaresult_sig<-meta_sig(raw_sig,sm_sig,a_sig,b_sig)
    }
    pdf(paste0(td,"/p1_sig.pdf"),
        width = input$plotwidth_sig/72,
        height = input$plotheight_sig/72
    )
    par(oma=c(0,5,1,1), mar=c(0,2,2,2))#c(bottom, left, top, right)
    meta::forest(metaresult_sig,digits=3,col.square.lines='white',col.square=input$col.square_sig,col.study=input$col.study_sig,fontsize=input$fontsize_sig
    )
    dev.off()
    
    png(paste0(td,"/p1_sig.png"),
        width = 5*input$plotwidth_sig,
        height = 5*input$plotheight_sig,
        res=300)
    par(oma=c(2,0,1,0), mar=c(0,5,2,2))#c(bottom, left, top, right)
    
    meta::forest(metaresult_sig,digits=3,col.square.lines='white',col.square=input$col.square_sig,col.study=input$col.study_sig,fontsize=input$fontsize_sig
    )
    dev.off()
    trimfill_sig<-ifelse(input$trimfill_sig=="TRUE","T","F")
    pdf(paste0(td,"/p2_sig.pdf"),
        width = input$plotwidth_sig/72,
        height = input$plotheight_sig/72)
    par(mar=c(2,3,1,3))
    par(pty = "s")  # 修改：设置图形区域为正方形
    if(trimfill_sig=="T"){
      meta::funnel(trimfill(metaresult_sig))
    }else{
      meta::funnel(metaresult_sig)
    }
    dev.off()
    
    png(paste0(td,"/p2_sig.png"),
        width = 5*input$plotwidth_sig,
        height =5* input$plotheight_sig,
        res=300)
    par(mar=c(2,3,1,3))
    par(pty = "s")  # 修改：设置图形区域为正方形
    if(trimfill_sig=="T"){
      meta::funnel(trimfill(metaresult_sig))
    }else{
      meta::funnel(metaresult_sig)
    }
    dev.off()
    
    metaresult_sig
  })
  
  
  
  # 生成敏感性分析结果
  sensitivity_result_sig <- eventReactive(input$run_sig, {
    meta_result <- metaresult_sig()
    metainf(meta_result)
  })
  # 绘制敏感性分析森林图
  output$influencePlot_sig <- renderPlot({
    print(meta::forest(sensitivity_result_sig(), digits = 3,
                       col.bg = input$col.square_sensitivity,
                       col.border = "white",
                       col = input$col.study_sensitivity,
                       fontsize = input$fontsize_sensitivity,
                       height = input$plotheight_sensitivity / 72,
                       width = input$plotwidth_sensitivity / 72,
                       spacing = 1.2))
  })
  # 生成亚组间比较表格
  output$comparisonTable_sig <- renderDT({
    meta_results <- metaresult_sig()
    req(meta_results)
    group_levels <- unique(meta_results$byvar)
    
    comparison_results <- data.frame(
      Comparison = character(),
      Difference_in_Effect = numeric(),
      Confidence_Interval = character(),
      P_Value = numeric(),
      stringsAsFactors = FALSE
    )
    
    if (input$model_sig == "random effects model") {
      effect_field <- "TE.random.w"
      se_field <- "seTE.random.w"
    } else if (input$model_sig == "fixed effects model") {
      effect_field <- "TE.fixed.w"
      se_field <- "seTE.fixed.w"
    }
    
    if (length(group_levels) > 1) {
      for (i in 1:(length(group_levels) - 1)) {
        for (j in (i + 1):length(group_levels)) {
          md1 <- meta_results[[effect_field]][i]
          se1 <- meta_results[[se_field]][i]
          
          md2 <- meta_results[[effect_field]][j]
          se2 <- meta_results[[se_field]][j]
          
          md_diff <- md1 - md2
          se_diff <- sqrt(se1^2 + se2^2)
          
          alpha <- 0.05
          z_value <- qnorm(1 - alpha/2)
          
          ci_lower <- md_diff - z_value * se_diff
          ci_upper <- md_diff + z_value * se_diff
          ci_text <- paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")
          
          z_score <- md_diff / se_diff
          p_value <- 2 * (1 - pnorm(abs(z_score)))
          
          comparison_results <- rbind(comparison_results, data.frame(
            Comparison = paste0(group_levels[i], " vs ", group_levels[j]),
            Difference_in_Effect = round(md_diff, 2),
            Confidence_Interval = ci_text,
            P_Value = round(p_value, 4)
          ))
        }
      }
    }
    rownames(comparison_results) <- NULL
    datatable(comparison_results)
  })
  
  # 下载敏感性分析森林图
  output$downloadInfluencePlot_sig <- downloadHandler(
    filename = function() {
      paste("influence_plot", "pdf", sep = '.')
    },
    content = function(file) {
      pdf(file, width = input$plotwidth_sensitivity / 72, height = input$plotheight_sensitivity / 72)
      print(meta::forest(sensitivity_result_sig(), digits = 3,
                         col.bg = input$col.square_sensitivity,
                         col.border = "white",
                         col = input$col.study_sensitivity,
                         fontsize = input$fontsize_sensitivity,
                         height = input$plotheight_sensitivity / 72,
                         width = input$plotwidth_sensitivity / 72,
                         spacing = 1.2))
      #plot(sensitivity_result_sig())
      dev.off()
    }
  )
  
  output$downloadInfluencePlotPng_sig <- downloadHandler(
    filename = function() {
      paste("influence_plot", "png", sep = '.')
    },
    content = function(file) {
      png(file, width = 5 * input$plotwidth_sensitivity, height = 5 * input$plotheight_sensitivity, res = 300)
      print(meta::forest(sensitivity_result_sig(), digits = 3,
                         col.bg = input$col.square_sensitivity,
                         col.border = "white",
                         col = input$col.study_sensitivity,
                         fontsize = input$fontsize_sensitivity,
                         height = input$plotheight_sensitivity / 72,
                         width = input$plotwidth_sensitivity / 72,
                         spacing = 1.2))
      #plot(sensitivity_result_sig())
      dev.off()
    }
  )
  
  # 下载亚组间比较表格
  output$downloadComparisonTable_sig <- downloadHandler(
    filename = function() {
      paste("comparison_table", "csv", sep = '.')
    },
    content = function(file) {
      write.csv(output$comparisonTable_sig(), file)
    }
  )
  
  
  
  
  
  trimfill_sig<-eventReactive(input$run_sig, {
    ifelse(input$trimfill_sig=="TRUE","T","F")
  })
  metaresult_sig_bias<-eventReactive(input$run_sig, {
    if(input$exam_sig_dic) {
      raw_sig<-read.csv("www/forest_sig_dic.csv",sep=",")
    } else {
      raw_sig<-read.csv(input$file2_sig$datapath,sep=",",fileEncoding="GBK") 
    }
    sm_sig <- input$sm_sig
    b_sig<-ifelse(input$model_sig=="fixed effects model",TRUE,FALSE)
    a_sig<-ifelse(input$model_sig=="random effects model",TRUE,FALSE)
    metaresult_sig_bias<-meta_sig(raw_sig,sm_sig,a_sig,b_sig)
    metaresult_sig_bias
  })
  
  p1_sig<- eventReactive(input$run_sig, {
    p1_sig<-meta::forest(metaresult_sig(),col.square.lines='white',digits=3,col.square=input$col.square_sig,col.study=input$col.study_sig,fontsize=input$fontsize_sig
    )
    p1_sig
  })
  
  
  Begg_p_sig<-eventReactive(input$run_sig, {
    round(metabias(metaresult_sig_bias(), k.min = 3,method.bias = "Begg")$pval,4)
  })
  
  Egger_p_sig<-eventReactive(input$run_sig, {
    round(metabias(metaresult_sig_bias(), k.min = 3,method.bias = "Egger")$pval,4)
  })
  observeEvent(input$run_sig,{
    shinyjs::show("h_sig")
    shinyjs::show("f_sig")
    shinyjs::show("i_sig")
    output$distPlot_sig <- renderPlot({
      
      print(p1_sig())
    })
    output$Funnelplot_sig <- renderPlot({
      par(pty = "s")  # 修改：设置图形区域为正方形
      if(trimfill_sig()=="T"){
        meta::funnel(trimfill(metaresult_sig()))
      }else{
        meta::funnel(metaresult_sig())
      }
    })
    output$Begg_p_text_sig<-renderText(print(paste0("P value of Begg's test: ", Begg_p_sig())))
    output$Egger_p_text_sig<-renderText({print(paste0("P value of Egger's test: ", Egger_p_sig()))})
  })
  
  
  
  #meta_sig_con------------------------
  output$file2_sig_con <- renderUI({
    fileInput('file2_sig_con', 'Choose Primary File', accept = ".csv")
  })
  observeEvent(input$reset_sig_con, {
    output$file2_sig_con <- renderUI({
      fileInput('file2_sig_con', 'Choose Primary File', accept = ".csv")
    })
  })
  output$group_sig_con <- renderUI({
    fileInput('group_sig_con', 'Choose Group File', accept = ".csv")
  })
  observeEvent(input$reset_sig_con, {
    output$group_sig_con <- renderUI({
      fileInput('group_sig_con', 'Choose Group File', accept = ".csv")
    })
  })
  
  
  output$exam_sig_con <- renderUI({
    checkboxInput("exam_sig_con","Example Data",TRUE)
  })
  observeEvent(input$file2_sig_con, {
    output$exam_sig_con <- renderUI({
      checkboxInput("exam_sig_con","Example Data",FALSE)
    })
  }) 
  observeEvent(input$group_sig_con, {
    output$exam_sig_con <- renderUI({
      checkboxInput("exam_sig_con","Example Data",FALSE)
    })
  })
  
  observeEvent(input$file2_sig_con, {
    updateCheckboxInput(session, "exam_sig_con", value = FALSE)
  }) 
  observeEvent(input$group_sig_con, {
    updateCheckboxInput(session, "exam_sig_con", value = FALSE)
  }) 
  
  
  
  
  
  raw_sig_con1<-reactive({
    req(input$file2_sig_con)
    try(read.csv(input$file2_sig_con$datapath, header = TRUE,fileEncoding="GBK"))
  })
  d1_sig_con<- reactive({
    inFile1_sig_con <- input$file2_sig_con
    ext_sig_con <- tools::file_ext(inFile1_sig_con$datapath) 
    if(input$exam_sig_con){
      data <- read.csv("www/forest_sig_con.csv",sep=",")
      return(data) }else{
        if (!is.null(input$file2_sig_con)&& ext_sig_con  == "csv"&&
            all(nms_sig_con%in% names(raw_sig_con1())) ){
          return(raw_sig_con1())
        }else{
          shiny::showNotification("Primary file: Please upload a valid csv file", type = "error")
          NULL
        }
      }
  })
  output$head_sig_con<- renderDT(
    d1_sig_con(),
    options = list(pageLength = 5)
  )
  #download 
  output$downloadBtn_sig_con<- downloadHandler(
    filename = function() {
      paste("meta_sig_con", "zip",sep='.')
    },
    content = function(file) {
      myfile <- srcpath <- 'www/meta_sig_con.zip'
      file.copy(myfile, file)
    }
  )
  group_sig_con1<-reactive({
    req(input$group_sig_con)
    try(read.csv(input$group_sig_con$datapath, header = TRUE,fileEncoding="GBK"))
  })
  d2_sig_con<- reactive({
    inFile2_sig_con <- input$group_sig_con
    ext_sig_con2 <- tools::file_ext(inFile2_sig_con$datapath)  
    if(input$exam_sig_con){
      data_gp <- read.csv("www/group_con-sig.csv",sep=",")
      return(data_gp) }else{
        if (!is.null(input$group_sig_con)&& ext_sig_con2  == "csv"&&
            all(nms_group%in% names(group_sig_con1())) ){
          return(group_sig_con1())
        }else{
          shiny::showNotification("Group file: Please upload a valid csv file", type = "error")
          NULL
        }
      }
  })
  output$head2_sig_con<- renderDT(
    d2_sig_con(),
    options = list(pageLength = 5)
  )
  
  
  meta_sig_con<-function(metadata,sm,a,b,group = NULL){  
    metamean(n,mean,sd,data=metadata,sm=sm,
             random =a,fixed =b,
             studlab=study
    ) }
  meta_sig_con_group<-function(metadata,sm,a,b,group){                                     
    metamean(n,mean,sd,data=metadata,sm=sm,
             random =a,fixed =b,
             subgroup = group,studlab=study
    ) }
  metaresult_sig_con <- eventReactive(input$run_sig_con, {
    if(input$exam_sig_con) {
      raw_sig_con<-read.csv("www/forest_sig_con.csv",sep=",")
    } else {
      raw_sig_con<-read.csv(input$file2_sig_con$datapath,sep=",",fileEncoding="GBK") #row=1,header=T
    }
    b_sig_con<-ifelse(input$model_sig_con=="fixed effects model",TRUE,FALSE)
    a_sig_con<-ifelse(input$model_sig_con=="random effects model",TRUE,FALSE)
    sm_sig_con<-input$sm_sig_con
    if(!is.null(input$group_sig_con)|input$exam_sig_con){
      if(input$exam_sig_con) {
        group_sig_con<-read.csv("www/group_con-sig.csv",sep=",")[,2]
      } else {
        group_sig_con<-read.csv(input$group_sig_con$datapath,sep=",",fileEncoding="GBK")[,2]
      }
      metaresult_sig_con<- meta_sig_con_group(raw_sig_con,sm_sig_con,a_sig_con,b_sig_con,group_sig_con)  #group_sig_con$
      
    }else{
      metaresult_sig_con<-meta_sig_con(raw_sig_con,sm_sig_con,a_sig_con,b_sig_con)
    }
    pdf(paste0(td,"/p1_sig_con.pdf"),
        width = input$plotwidth_sig_con/72,
        height = input$plotheight_sig_con/72
    )
    par(oma=c(0,5,1,1), mar=c(0,2,2,2))#c(bottom, left, top, right)
    meta::forest(metaresult_sig_con,col.square.lines='white',digits=3,
                 col.square=input$col.square_sig_con,col.study=input$col.study_sig_con,
                 fontsize=input$fontsize_sig_con
    )
    dev.off()
    
    png(paste0(td,"/p1_sig_con.png"),
        width = 5*input$plotwidth_sig_con,
        height = 5*input$plotheight_sig_con,
        res=300)
    par(oma=c(2,0,1,0), mar=c(0,5,2,2))#c(bottom, left, top, right)
    meta::forest(metaresult_sig_con,digits=3,col.square.lines='white',
                 col.square=input$col.square_sig_con,col.study=input$col.study_sig_con,
                 fontsize=input$fontsize_sig_con
    )
    dev.off()
    trimfill_sig_con<-ifelse(input$trimfill_sig_con=="TRUE","T","F")
    pdf(paste0(td,"/p2_sig_con.pdf"),
        width = input$plotwidth_sig_con/72,
        height = input$plotheight_sig_con/72)
    par(mar=c(2,3,1,3))
    par(pty = "s")  # 修改：设置图形区域为正方形
    if(trimfill_sig_con=="T"){
      meta::funnel(trimfill(metaresult_sig_con))
    }else{
      meta::funnel(metaresult_sig_con)
    }
    dev.off()
    
    png(paste0(td,"/p2_sig_con.png"),
        width = 5*input$plotwidth_sig_con,
        height =5* input$plotheight_sig_con,
        res=300)
    par(mar=c(2,3,1,3))
    par(pty = "s")  # 修改：设置图形区域为正方形
    if(trimfill_sig_con=="T"){
      meta::funnel(trimfill(metaresult_sig_con))
    }else{
      meta::funnel(metaresult_sig_con)
    }
    dev.off()
    
    metaresult_sig_con
  })
  
  
  
  # 生成敏感性分析结果
  sensitivity_result_sigcon <- eventReactive(input$run_sig_con, {
    meta_result <- metaresult_sig_con()
    metainf(meta_result)
  })
  # 绘制敏感性分析森林图
  output$influencePlot_sigcon <- renderPlot({
    print(meta::forest(sensitivity_result_sigcon(), digits = 3,
                       col.bg = input$col.square_sensitivity_sigcon,
                       col.border = "white",
                       col = input$col.study_sensitivity_sigcon,
                       fontsize = input$fontsize_sensitivity_sigcon,
                       height = input$plotheight_sensitivity_sigcon / 72,
                       width = input$plotwidth_sensitivity_sigcon / 72,
                       spacing = 1.2))
  })
  # 生成亚组间比较表格
  output$comparisonTable_sigcon <- renderDT({
    meta_results <- metaresult_sig_con()
    req(meta_results)
    group_levels <- unique(meta_results$byvar)
    
    comparison_results_sigcon <- data.frame(
      Comparison = character(),
      Difference_in_Effect = numeric(),
      Confidence_Interval = character(),
      P_Value = numeric(),
      stringsAsFactors = FALSE
    )
    
    if (input$model_con == "random effects model") {
      effect_field <- "TE.random.w"
      se_field <- "seTE.random.w"
    } else if (input$model_con == "fixed effects model") {
      effect_field <- "TE.fixed.w"
      se_field <- "seTE.fixed.w"
    }
    
    
    if (length(group_levels) > 1) {
      for (i in 1:(length(group_levels) - 1)) {
        for (j in (i + 1):length(group_levels)) {
          md1 <- meta_results[[effect_field]][i]
          se1 <- meta_results[[se_field]][i]
          
          md2 <- meta_results[[effect_field]][j]
          se2 <- meta_results[[se_field]][j]
          
          md_diff <- md1 - md2
          se_diff <- sqrt(se1^2 + se2^2)
          
          alpha <- 0.05
          z_value <- qnorm(1 - alpha/2)
          
          ci_lower <- md_diff - z_value * se_diff
          ci_upper <- md_diff + z_value * se_diff
          ci_text <- paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")
          
          z_score <- md_diff / se_diff
          p_value <- 2 * (1 - pnorm(abs(z_score)))
          
          comparison_results_sigcon <- rbind(comparison_results_sigcon, data.frame(
            Comparison = paste0(group_levels[i], " vs ", group_levels[j]),
            Difference_in_Effect = round(md_diff, 2),
            Confidence_Interval = ci_text,
            P_Value = round(p_value, 4)
          ))
        }
      }
    }
    rownames(comparison_results_sigcon) <- NULL
    datatable(comparison_results_sigcon)
  })
  
  # 下载敏感性分析森林图
  output$downloadInfluencePlot_sigcon <- downloadHandler(
    filename = function() {
      paste("influence_plot", "pdf", sep = '.')
    },
    content = function(file) {
      pdf(file, width = input$plotwidth_sensitivity_sigcon / 72, height = input$plotheight_sensitivity_sigcon / 72)
      print(meta::forest(sensitivity_result_sigcon(), digits = 3,
                         col.bg = input$col.square_sensitivity_sigcon,
                         col.border = "white",
                         col = input$col.study_sensitivity_sigcon,
                         fontsize = input$fontsize_sensitivity_sigcon,
                         height = input$plotheight_sensitivity_sigcon / 72,
                         width = input$plotwidth_sensitivity_sigcon / 72,
                         spacing = 1.2))
      #plot(sensitivity_result_sigcon())
      dev.off()
    }
  )
  
  output$downloadInfluencePlotPng_sigcon <- downloadHandler(
    filename = function() {
      paste("influence_plot", "png", sep = '.')
    },
    content = function(file) {
      png(file, width = 5 * input$plotwidth_sensitivity_sigcon, height = 5 * input$plotheight_sensitivity_sigcon, res = 300)
      print(meta::forest(sensitivity_result_sigcon(), digits = 3,
                         col.bg = input$col.square_sensitivity_sigcon,
                         col.border = "white",
                         col = input$col.study_sensitivity_sigcon,
                         fontsize = input$fontsize_sensitivity_sigcon,
                         height = input$plotheight_sensitivity_sigcon / 72,
                         width = input$plotwidth_sensitivity_sigcon / 72,
                         spacing = 1.2))
      #plot(sensitivity_result_sigcon())
      dev.off()
    }
  )
  
  # 下载亚组间比较表格
  output$downloadComparisonTable_sigcon <- downloadHandler(
    filename = function() {
      paste("comparison_table", "csv", sep = '.')
    },
    content = function(file) {
      write.csv(output$comparisonTable_sigcon(), file)
    }
  )
  
  
  
  
  
  trimfill_sig_con<-eventReactive(input$run_sig_con, {
    ifelse(input$trimfill_sig_con=="TRUE","T","F")
  })
  metaresult_sig_con_bias<-eventReactive(input$run_sig_con, {
    if(input$exam_sig_con) {
      raw_sig_con<-read.csv("www/forest_sig_con.csv",sep=",")
    } else {
      raw_sig_con<-read.csv(input$file2_sig_con$datapath,sep=",",fileEncoding="GBK") #row=1,header=T
    }
    b_sig_con<-ifelse(input$model_sig_con=="fixed effects model",TRUE,FALSE)
    a_sig_con<-ifelse(input$model_sig_con=="random effects model",TRUE,FALSE)
    sm_sig_con<-input$sm_sig_con
    metaresult_sig_con_bias<-meta_sig_con(raw_sig_con,sm_sig_con,a_sig_con,b_sig_con)
    metaresult_sig_con_bias
  })
  p1_sig_con<- eventReactive(input$run_sig_con, {
    p1_sig_con<-meta::forest(metaresult_sig_con(),col.square.lines='white',
                             digits=3,col.square=input$col.square_sig_con,
                             col.study=input$col.study_sig_con,fontsize=input$fontsize_sig_con
    )
    p1_sig_con
  })
  
  
  Begg_p_sig_con<-eventReactive(input$run_sig_con, {
    round(metabias(metaresult_sig_con_bias(), k.min = 3,method.bias = "Begg")$pval,4)
  })
  
  Egger_p_sig_con<-eventReactive(input$run_sig_con, {
    round(metabias(metaresult_sig_con_bias(), k.min = 3,method.bias = "Egger")$pval,4)
  })
  
  
  observeEvent(input$run_sig_con,{
    shinyjs::show("h_sig_con")
    shinyjs::show("f_sig_con")
    shinyjs::show("i_sig_con")
    output$distPlot_sig_con <- renderPlot({
      print(p1_sig_con())
    })
    
    output$Funnelplot_sig_con <- renderPlot({
      par(pty = "s")  # 修改：设置图形区域为正方形
      if(trimfill_sig_con()=="T"){
        meta::funnel(trimfill(metaresult_sig_con()))
      }else{
        meta::funnel(metaresult_sig_con())
      }
    })
    output$Egger_p_text_sig_con<-renderText(print(paste0("P value of Egger's test: ", Egger_p_sig_con()))) 
    output$Begg_p_text_sig_con<-renderText(print(paste0("P value of Begg's test: ", Begg_p_sig_con())))
    output$influencePlot_sigcon <- renderPlot({
      print(meta::forest(sensitivity_result_sigcon(), digits = 3,
                         col.bg = input$col.square_sensitivity_sigcon,
                         col.border = "white",
                         col = input$col.study_sensitivity_sigcon,
                         fontsize = input$fontsize_sensitivity_sigcon,
                         height = input$plotheight_sensitivity_sigcon / 72,
                         width = input$plotwidth_sensitivity_sigcon / 72,
                         spacing = 1.2))
    })
  })
  
  #deft----
  output$file1_de <- renderUI({
    fileInput('file1_de', 'Choose CSV File',accept = ".csv")
  })
  observeEvent(input$reset_de, {
    output$file1_de <- renderUI({
      fileInput('file1_de', 'Choose CSV File',accept = ".csv")
    })
  })
  output$exam_de <- renderUI({
    checkboxInput("exam_de","Example Data",TRUE)
  })
  observeEvent(input$file1_de, {
    output$exam_de <- renderUI({
      checkboxInput("exam_de","Example Data",FALSE)
    })
  }) 
  
  observeEvent(input$file1_de, {
    updateCheckboxInput(session, "exam_de", value = FALSE)
  }) 
  
  
  
  d1_de<-reactive({
    req(input$file1_de)
    try(read.csv(input$file1_de$datapath, header = TRUE,fileEncoding="GBK"))
  })
  raw_de1<- reactive({
    inFile1_de <- input$file1_de
    ext <- tools::file_ext(inFile1_de$datapath)
    if(input$exam_de){
      data <- read.csv("www/deft.csv",sep=",") 
      return(data) 
    }else{
      if (!is.null(input$file1_de)&& ext == "csv"&&
          all(nms_de%in% names(d1_de()))
      ){
        return(d1_de())
      }else{
        shiny::showNotification("Primary file: Please upload a valid csv file", type = "error")
        NULL
      }
    }
    
  })
  
  output$head_de <- renderDT(
    raw_de1(),
    options = list(pageLength = 5)
  )
  #download 
  output$downloadBtn_de <- downloadHandler(
    filename = function() {
      paste("deft", "csv",sep='.')
    },
    content = function(file) {
      myfile <- srcpath <- 'www/deft.csv'
      file.copy(myfile, file)
    }
  )
  p1_de<- eventReactive(input$run_de, {
    if(input$exam_de) {
      raw_de<-read.csv("www/deft.csv",sep=",")
    } else {
      raw_de<-read.csv(input$file1_de$datapath,sep=",",fileEncoding="GBK")
    }
    dt_de<-deft_prepare(raw_de)
    res_de<-deft_do(dt_de, group_level = unique(as.character(dt_de$subgroup)),method=input$sm_de)
    par(cex = input$cex_de, font = 2)
    p1_de <-deft_show(res_de, element = "subgroup")
    
    pdf(paste0(td,"/p1_de.pdf"),
        width = input$plotwidth_de/72,
        height = input$plotheight_de/72)
    print(p1_de)
    dev.off()
    
    png(paste0(td,"/p1_de.png"),
        width =5*input$plotwidth_de,
        height =5*input$plotheight_de,
        res=300)
    print(p1_de)
    dev.off()
    p1_de
  })
  
  
  
  p2_de<- eventReactive(input$run_de, {
    if(input$exam_de) {
      raw2_de<-read.csv("www/deft.csv",sep=",")
    } else {
      raw2_de<-read.csv(input$file1_de$datapath,sep=",",fileEncoding="GBK")
    }
    raw2_de$logHR <- log(raw2_de$hr)
    raw2_de$varLogHR <- ((log(raw2_de$ci.ub) - log(raw2_de$ci.lb)) / (2 * 1.96))^2
    # 初始化留一法数据框
    leave_one_out_df <- data.frame(
      Estimate = numeric(nrow(raw2_de)),
      LowerCI = numeric(nrow(raw2_de)),
      UpperCI = numeric(nrow(raw2_de)),
      Trial = character(nrow(raw2_de))
    )
    print(head(raw2_de)) # 确认数据正确读取
    # 进行留一法分析
    for (i in 1:nrow(raw2_de)) {
      subset_data <- raw2_de[-i, ]
      if ("logHR" %in% names(subset_data) && "varLogHR" %in% names(subset_data)) {
        loo_result <- rma(yi = subset_data$logHR, vi = subset_data$varLogHR, method="REML")
        
        leave_one_out_df$Estimate[i] <- loo_result$b
        leave_one_out_df$LowerCI[i] <- loo_result$ci.lb
        leave_one_out_df$UpperCI[i] <- loo_result$ci.ub
        leave_one_out_df$Trial[i] <- raw2_de$trial[i]
      } else {
        stop("Columns 'logHR' or 'varLogHR' not found in the data.")
      }
    }
    # 更新列名
    names(leave_one_out_df) <- c("Estimate", "Lower CI", "Upper CI", "Excluded Study")
    
    # 转换数据类型
    leave_one_out_df$Estimate <- as.numeric(as.character(leave_one_out_df$Estimate))
    leave_one_out_df$`Lower CI` <- as.numeric(as.character(leave_one_out_df$`Lower CI`))
    leave_one_out_df$`Upper CI` <- as.numeric(as.character(leave_one_out_df$`Upper CI`))
    
    # 绘制敏感性分析森林图
    metafor::forest(leave_one_out_df$Estimate, ci.lb = leave_one_out_df$`Lower CI`, 
                    ci.ub = leave_one_out_df$`Upper CI`, 
                    slab = leave_one_out_df$`Excluded Study`, 
                    xlab = "Effect Size (Log Hazard Ratio)", 
                    alim = c(-1, 1),  
                    main = "Leave-One-Out Sensitivity Analysis")
    
    
    pdf(paste0(td, "/p2_de.pdf"),
        width = input$plotwidth_de / 72,
        height = input$plotheight_de / 72)
    metafor::forest(leave_one_out_df$Estimate, ci.lb = leave_one_out_df$`Lower CI`, 
                    ci.ub = leave_one_out_df$`Upper CI`, 
                    slab = leave_one_out_df$`Excluded Study`, 
                    xlab = "Effect Size (Log Hazard Ratio)", 
                    alim = c(-1, 1),  
                    main = "Leave-One-Out Sensitivity Analysis")
    dev.off()
    
    png(paste0(td,"/p2_de.png"),
        width =5*input$plotwidth_de,
        height =5*input$plotheight_de,
        res=300)
    metafor::forest(leave_one_out_df$Estimate, ci.lb = leave_one_out_df$`Lower CI`, 
                    ci.ub = leave_one_out_df$`Upper CI`, 
                    slab = leave_one_out_df$`Excluded Study`, 
                    xlab = "Effect Size (Log Hazard Ratio)", 
                    alim = c(-1, 1),  
                    main = "Leave-One-Out Sensitivity Analysis")
    dev.off()
    p2_de
  })
  
  
  observeEvent(input$run_de,{
    shinyjs::show("p1_de")
    shinyjs::show("p2_de")
    output$distPlot_de <- renderPlot({
      print(p1_de())
    })
    output$distPlot_de_sen <- renderPlot({
      print(p2_de())
    })
  })    
  # p1_de<- eventReactive(input$run_de, {
  #   res_de<-deft_do(dt_de(), group_level = unique(as.character(dt_de()$subgroup)),method=input$sm_de)
  #   par(cex = input$cex_de, font = 2)
  #   p1_de = deft_show(res_de, element = "subgroup")
  #   
  #   pdf(paste0(td,"/p1_de.pdf"),
  #       width = input$plotwidth_de/72,
  #       height = input$plotheight_de/72)
  #   print(p1_de)
  #   dev.off()
  #   
  #   png(paste0(td,"/p1_de.png"),
  #       width =5*input$plotwidth_de,
  #       height =5*input$plotheight_de,
  #       res=300)
  #   print(p1_de)
  #   dev.off()
  #   
  #   p1_de
  # })
  # 
  
  #meta_surv----
  output$file2_rate <- renderUI({
    fileInput('file2_rate', 'Choose Primary File', accept = ".csv")
  })
  observeEvent(input$reset_rate, {
    output$file2_rate <- renderUI({
      fileInput('file2_rate', 'Choose Primary File', accept = ".csv")
    })
  })
  output$group_rate <- renderUI({
    fileInput('group_rate', 'Choose Group File', accept = ".csv")
  })
  observeEvent(input$reset_rate, {
    output$group_rate <- renderUI({
      fileInput('group_rate', 'Choose Group File', accept = ".csv")
    })
  })
  output$exam_rate <- renderUI({
    checkboxInput("exam_rate","Example Data",TRUE)
  })
  observeEvent(input$file2_rate, {
    output$exam_rate <- renderUI({
      checkboxInput("exam_rate","Example Data",FALSE)
    })
  }) 
  observeEvent(input$group_rate, {
    output$exam_rate <- renderUI({
      checkboxInput("exam_rate","Example Data",FALSE)
    })
  }) 
  
  observeEvent(input$file2_rate, {
    updateCheckboxInput(session, "exam_rate", value = FALSE)
  }) 
  observeEvent(input$group_rate, {
    updateCheckboxInput(session, "exam_rate", value = FALSE)
  }) 

  
  
  
  raw_rate1<-reactive({
    req(input$file2_rate)
    try(read.csv(input$file2_rate$datapath, header = TRUE,fileEncoding="GBK"))
  })
  d1_rate<- reactive({
    inFile1_rate <- input$file2_rate
    ext_rate <- tools::file_ext(inFile1_rate$datapath)  
    if(input$exam_rate){
      data <- read.csv("www/survival rate.csv",sep=",")
      return(data) }else{
        if (!is.null(input$file2_rate)&& ext_rate  == "csv" &&
            all(nms_rate%in% names(raw_rate1())) ){
          return(raw_rate1())
        }else{
          shiny::showNotification("Primary file: Please upload a valid csv file", type = "error")
          NULL
        }
      }
  })
  output$head_rate<- renderDT(
    d1_rate(),
    options = list(pageLength = 5)
  )
  #download 
  output$downloadBtn_rate<- downloadHandler(
    filename = function() {
      paste("meta_rate", "zip",sep='.')
    },
    content = function(file) {
      myfile <- srcpath <- 'www/meta_rate.zip'
      file.copy(myfile, file)
    }
  )
  group_rate1<-reactive({
    req(input$group_rate)
    try(read.csv(input$group_rate$datapath, header = TRUE,fileEncoding="GBK"))
  })
  
  d2_rate<- reactive({
    inFile2_rate <- input$group_rate
    ext_rate2 <- tools::file_ext(inFile2_rate$datapath)  
    if(input$exam_rate){
      data <- read.csv("www/group_rate.csv",sep=",")
      return(data) }else{
        if (!is.null(input$group_rate)&& ext_rate2  == "csv"&&
            all(nms_group%in% names(group_rate1())) ){
          return(group_rate1())
        }else{
          shiny::showNotification("Group file: Please upload a valid csv file", type = "error")
          NULL
        }
      }
  })
  output$head2_rate<- renderDT(
    d2_rate(),
    options = list(pageLength = 5)
  )
  
  
  meta_rate<-function(metadata,sm,a,b,group = NULL){  
    metagen(log, 
            selog,
      #TE = log(metadata$HR), 
      #seTE = (log(metadata$upper) - log(metadata$lower)) / (2 * 1.96), 
      n.e = metadata$n.e,  # 使用样本量
            data=metadata,
            random =a,
            fixed =b,
            studlab=study,
            sm=sm)
  }
  meta_rate_group<-function(metadata,sm,a,b,group){                                     
    metagen(log, selog, n.e = metadata$n.e, data=metadata,
            random =a,fixed =b,studlab=study,
            sm=sm,subgroup = group) }
  
  metaresult_rate <- eventReactive(input$run_rate, {
    if(input$exam_rate) {
      raw_rate<-read.csv("www/survival rate.csv",sep=",")
      data_rate<-raw_rate
      data_rate$log<-log(data_rate[,2])
      data_rate$loglower<-log(data_rate[,3])
      data_rate$logupper<-log(data_rate[,4])
      data_rate$selog<-(data_rate$logupper-data_rate$loglower)/(2*1.96)
      raw_rate<-data_rate[,c(1,5,8)]
      #raw_rate <- data_rate[, c("study", "log", "selog", "n.e")]  # 修改
    } else {
      raw_rate<-read.csv(input$file2_rate$datapath,sep=",",fileEncoding="GBK") #row=1,header=T
      data_rate<-raw_rate
      data_rate$log<-log(data_rate[,2])
      data_rate$loglower<-log(data_rate[,3])
      data_rate$logupper<-log(data_rate[,4])
      data_rate$selog<-(data_rate$logupper-data_rate$loglower)/(2*1.96)
      raw_rate<-data_rate[,c(1,5,8)]
      #raw_rate <- data_rate[, c("study", "log", "selog", "n.e")]  # 修改
      
    }
    b_rate<-ifelse(input$model_rate=="fixed effects model",TRUE,FALSE)
    a_rate<-ifelse(input$model_rate=="random effects model",TRUE,FALSE)
    sm_rate<-input$sm_rate
    if(!is.null(input$group_rate)|input$exam_rate){
      if(input$exam_rate) {
        group_rate<-read.csv("www/group_rate.csv",sep=",")[,2]
      }else{
        group_rate<-read.csv(input$group_rate$datapath,sep=",",fileEncoding="GBK")[,2]
      }
      metaresult_rate<- meta_rate_group(raw_rate,sm_rate,a_rate,b_rate,group_rate)  #group_rate$
    }else{
      metaresult_rate<-meta_rate(raw_rate,sm_rate,a_rate,b_rate)
    }
    
    pdf(paste0(td,"/p1_rate.pdf"),
        width = input$plotwidth_rate/72,
        height = input$plotheight_rate/72
    )
    par(oma=c(0,5,1,1), mar=c(0,2,2,2))#c(bottom, left, top, right)
    meta::forest(metaresult_rate,digits=3,col.square.lines='white',col.square=input$col.square_rate,col.study=input$col.study_rate,fontsize=input$fontsize_rate
    )
    dev.off()
    
    png(paste0(td,"/p1_rate.png"),
        width = 5*input$plotwidth_rate,
        height = 5*input$plotheight_rate,
        res=300)
    par(oma=c(2,0,1,0), mar=c(0,5,2,2))#c(bottom, left, top, right)
    meta::forest(metaresult_rate,digits=3,col.square.lines='white',col.square=input$col.square_rate,col.study=input$col.study_rate,fontsize=input$fontsize_rate
    )
    dev.off()
    
    trimfill_rate<-ifelse(input$trimfill_rate=="TRUE","T","F")
    
    pdf(paste0(td,"/p2_rate.pdf"),
        width = input$plotwidth_rate/72,
        height = input$plotheight_rate/72)
    par(mar=c(2,3,1,3))
    par(pty = "s")  # 修改：设置图形区域为正方形
    if(trimfill_rate=="T"){
      meta::funnel(trimfill(metaresult_rate))
    }else{
      meta::funnel(metaresult_rate)
    }
    dev.off()
    
    png(paste0(td,"/p2_rate.png"),
        width = 5*input$plotwidth_rate,
        height =5* input$plotheight_rate,
        res=300)
    par(mar=c(2,3,1,3))
    par(pty = "s")  # 修改：设置图形区域为正方形
    if(trimfill_rate=="T"){
      meta::funnel(trimfill(metaresult_rate))
    }else{
      meta::funnel(metaresult_rate)
    }
    
    dev.off()
    
    metaresult_rate
  })
  trimfill_rate<-eventReactive(input$run_rate, {
    ifelse(input$trimfill_rate=="TRUE","T","F")
  })
  
  metaresult_rate_bias<-eventReactive(input$run_rate, {
    if(input$exam_rate) {
      raw_rate<-read.csv("www/survival rate.csv",sep=",")
      data_rate<-raw_rate
      data_rate$log<-log(data_rate[,2])
      data_rate$loglower<-log(data_rate[,3])
      data_rate$logupper<-log(data_rate[,4])
      data_rate$selog<-(data_rate$logupper-data_rate$loglower)/(2*1.96)
      raw_rate<-data_rate[,c(1,5,8)]
    } else {
      raw_rate<-read.csv(input$file2_rate$datapath,sep=",",fileEncoding="GBK") #row=1,header=T
      data_rate<-raw_rate
      data_rate$log<-log(data_rate[,2])
      data_rate$loglower<-log(data_rate[,3])
      data_rate$logupper<-log(data_rate[,4])
      data_rate$selog<-(data_rate$logupper-data_rate$loglower)/(2*1.96)
      raw_rate<-data_rate[,c(1,5,8)]
      
    }
    b_rate<-ifelse(input$model_rate=="fixed effects model",TRUE,FALSE)
    a_rate<-ifelse(input$model_rate=="random effects model",TRUE,FALSE)
    sm_rate<-input$sm_rate
    metaresult_rate_bias<-meta_rate(raw_rate,sm_rate,a_rate,b_rate)
    metaresult_rate_bias
  })
  
  p1_rate<- eventReactive(input$run_rate, {
    p1_rate<-meta::forest(metaresult_rate(),digits=3,col.square.lines='white',col.square=input$col.square_rate,col.study=input$col.study_rate,fontsize=input$fontsize_rate
    )
    p1_rate
  })
  
  
  Begg_p_rate<-eventReactive(input$run_rate, {
    round(metabias(metaresult_rate_bias(), k.min = 3,method.bias = "Begg")$pval,4)
  })
  
  Egger_p_rate<-eventReactive(input$run_rate, {
    round(metabias(metaresult_rate_bias(), k.min = 3,method.bias = "Egger")$pval,4)
  })
  
  
  
  
  # 生成比较表格的数据
  output$comparison_table <- renderDT({
    meta_results <- metaresult_rate()
    
    req(meta_results)  # 确保数据可用
    group_levels <- unique(meta_results$byvar)  # 获取唯一值
    
    comparison_results <- data.frame(
      Comparison = character(),
      Difference_in_Effect = numeric(),
      Confidence_Interval = character(),
      P_Value = numeric(),
      stringsAsFactors = FALSE
    )
    
    # 根据用户选择的模型提取数据
    if (input$model_rate == "random effects model") {
      effect_field <- "TE.random.w"
      se_field <- "seTE.random.w"
    } else if (input$model_rate == "fixed effects model") {
      effect_field <- "TE.fixed.w"
      se_field <- "seTE.fixed.w"
    }
    
    if (length(group_levels) > 1) {
      for (i in 1:(length(group_levels) - 1)) {
        for (j in (i + 1):length(group_levels)) {
          md1 <- meta_results[[effect_field]][i]
          se1 <- meta_results[[se_field]][i]
          
          md2 <- meta_results[[effect_field]][j]
          se2 <- meta_results[[se_field]][j]
          
          md_diff <- md1 - md2
          se_diff <- sqrt(se1^2 + se2^2)
          
          alpha <- 0.05
          z_value <- qnorm(1 - alpha/2)
          
          ci_lower <- md_diff - z_value * se_diff
          ci_upper <- md_diff + z_value * se_diff
          ci_text <- paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")
          
          z_score <- md_diff / se_diff
          p_value <- 2 * (1 - pnorm(abs(z_score)))
          
          comparison_results <- rbind(comparison_results, data.frame(
            Comparison = paste0(group_levels[i], " vs ", group_levels[j]),
            Difference_in_Effect = round(md_diff, 2),
            Confidence_Interval = ci_text,
            P_Value = round(p_value, 4)
          ))
        }
      }
    }
    rownames(comparison_results) <- NULL
    datatable(comparison_results)
  })  # 新增的比较表格功能实现
  
  
  
  # 生成敏感性分析结果
  sensitivity_result_rate <- eventReactive(input$run_rate, {
    meta_result_surv <- metaresult_rate()
    metainf(meta_result_surv)
  })
  
  
  
  
  
  
  
  
  output$downloadp3_rate <- downloadHandler(
    filename = function() {
      "p3_rate.pdf"
    },
    content = function(file) {
      
      pdf(file,
          width = input$plotwidth_sensitivity_rate / 72,
          height = input$plotheight_sensitivity_rate / 72)
      par(oma = c(0, 5, 1, 1), mar = c(0, 2, 2, 2)) # 设置边距
      print(meta::forest(sensitivity_result_rate(), digits = 3,
                         col.bg = input$col.square_sensitivity_rate,
                         col.border = "white",
                         col = input$col.study_sensitivity_rate,
                         fontsize = input$fontsize_sensitivity_rate,
                         height = input$plotheight_sensitivity_rate / 72,
                         width = input$plotwidth_sensitivity_rate / 72,
                         spacing = 1.2))
      dev.off()
    }
  )
  
  
  output$downloadp3png_rate <- downloadHandler(
    filename = function() {
      "p3_rate.png"
    },
    content = function(file) {
      png(file,
          width = 5 * input$plotwidth_sensitivity_rate,
          height = 5 * input$plotheight_sensitivity_rate,
          res = 300)
      par(oma = c(2, 0, 1, 0), mar = c(0, 5, 2, 2)) # 设置边距
      print(meta::forest(sensitivity_result_rate(), digits = 3,
                         col.bg = input$col.square_sensitivity_rate,
                         col.border = "white",
                         col = input$col.study_sensitivity_rate,
                         fontsize = input$fontsize_sensitivity_rate,
                         height = input$plotheight_sensitivity_rate / 72,
                         width = input$plotwidth_sensitivity_rate / 72,
                         spacing = 1.2))
      dev.off()
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$run_rate,{
    shinyjs::show("h_rate")
    shinyjs::show("f_rate")
    shinyjs::show("s_rate")
    output$distPlot_rate <- renderPlot({
      print(p1_rate())
    })
    
    output$Funnelplot_rate <- renderPlot({
      par(pty = "s")  # 修改：设置图形区域为正方形
      if(trimfill_rate()=="T"){
        meta::funnel(trimfill(metaresult_rate()))
      }else{
        meta::funnel(metaresult_rate())
      }
    })
    output$Egger_p_text_rate<-renderText({print(paste0("P value of Egger's test: ", Egger_p_rate()))})
    output$Begg_p_text_rate<-renderText(print(paste0("P value of Begg's test: ", Begg_p_rate())))
    # 生成敏感性分析森林图
    output$sensitivity_forest_rate <- renderPlot({
      print(meta::forest(sensitivity_result_rate(), digits = 3,
                         col.bg = input$col.square_sensitivity_rate,
                         col.border = "white",
                         col = input$col.study_sensitivity_rate,
                         fontsize = input$fontsize_sensitivity_rate,
                         height = input$plotheight_sensitivity_rate / 72,
                         width = input$plotwidth_sensitivity_rate / 72,
                         spacing = 1.2))
    })
  })
  
  
  
  
  
  
  
  
  #meta_surv2----
  output$file2_rate2 <- renderUI({
    fileInput('file2_rate2', 'Choose Primary File', accept = ".csv")
  })
  observeEvent(input$reset_rate2, {
    output$file2_rate2 <- renderUI({
      fileInput('file2_rate2', 'Choose Primary File', accept = ".csv")
    })
  })
  output$group_rate2 <- renderUI({
    fileInput('group_rate2', 'Choose Group File', accept = ".csv")
  })
  observeEvent(input$reset_rate2, {
    output$group_rate2 <- renderUI({
      fileInput('group_rate2', 'Choose Group File', accept = ".csv")
    })
  })
  output$exam_rate2 <- renderUI({
    checkboxInput("exam_rate2","Example Data",TRUE)
  })
  observeEvent(input$file2_rate2, {
    output$exam_rate2 <- renderUI({
      checkboxInput("exam_rate2","Example Data",FALSE)
    })
  }) 
  observeEvent(input$group_rate2, {
    output$exam_rate2 <- renderUI({
      checkboxInput("exam_rate2","Example Data",FALSE)
    })
  }) 
  
  observeEvent(input$file2_rate2, {
    updateCheckboxInput(session, "exam_rate2", value = FALSE)
  }) 
  observeEvent(input$group_rate2, {
    updateCheckboxInput(session, "exam_rate2", value = FALSE)
  })
  
  
  
  raw_rate2<-reactive({
    req(input$file2_rate2)
    try(read.csv(input$file2_rate2$datapath, header = TRUE,fileEncoding="GBK"))
  })
  d1_rate2<- reactive({
    inFile1_rate2 <- input$file2_rate2
    ext_rate2 <- tools::file_ext(inFile1_rate2$datapath)  
    if(input$exam_rate2){
      data <- read.csv("www/survival rate3.csv",sep=",")
      return(data) }else{
        if (!is.null(input$file2_rate2)&& ext_rate2  == "csv" &&
            all(nms_rate2%in% names(raw_rate2())) ){
          return(raw_rate2())
        }else{
          shiny::showNotification("Primary file: Please upload a valid csv file", type = "error")
          NULL
        }
      }
  })
  output$head_rate2<- renderDT(
    d1_rate2(),
    options = list(pageLength = 5)
  )
  #download 
  output$downloadBtn_rate2<- downloadHandler(
    filename = function() {
      paste("meta_rate2", "zip",sep='.')
    },
    content = function(file) {
      myfile <- srcpath <- 'www/meta_rate2.zip'
      file.copy(myfile, file)
    }
  )
  group_rate2<-reactive({
    req(input$group_rate2)
    try(read.csv(input$group_rate2$datapath, header = TRUE,fileEncoding="GBK"))
  })
  
  d2_rate2<- reactive({
    inFile2_rate2 <- input$group_rate2
    ext_rate2 <- tools::file_ext(inFile2_rate2$datapath)  
    if(input$exam_rate2){
      data <- read.csv("www/group_rate3.csv",sep=",")
      return(data) }else{
        if (!is.null(input$group_rate2)&& ext_rate2  == "csv"&&
            all(nms_group%in% names(group_rate2())) ){
          return(group_rate2())
        }else{
          shiny::showNotification("Group file: Please upload a valid csv file", type = "error")
          NULL
        }
      }
  })
  output$head2_rate2<- renderDT(
    d2_rate2(),
    options = list(pageLength = 5)
  )
  
  
  # 无分组的元分析函数
  meta_rate2 <- function(metadata, sm, a, b, group = NULL) {  
    
    metagen(TE = metadata$OE, 
            seTE = sqrt(metadata$V),
            data = metadata,
            random = a,
            fixed = b,
            studlab = metadata$study,
            sm = sm)
  }
  
  # 有分组的元分析函数
  meta_rate2_group <- function(metadata, sm, a, b, group) {
    
    metagen(TE = metadata$OE,
            seTE = sqrt(metadata$V),
            data = metadata,
            random = a,
            fixed = b,
            studlab = metadata$study,
            sm = sm,
            subgroup = group)
  }
  
  
  
  
  
  metaresult_rate2 <- eventReactive(input$run_rate2, {
    raw_rate2 <- if (input$exam_rate2) {
      read.csv("www/survival rate3.csv", sep = ",")
    } else {
      read.csv(input$file2_rate2$datapath, sep = ",", fileEncoding = "GBK")
    }
    
    b_rate2 <- input$model_rate2 == "fixed effects model"
    a_rate2 <- input$model_rate2 == "random effects model"
    sm_rate2 <- input$sm_rate2
    
    group_rate2 <- if (!is.null(input$group_rate2) || input$exam_rate2) {
      if (input$exam_rate2) {
        read.csv("www/group_rate3.csv", sep = ",")[, "group"]
      } else {
        read.csv(input$group_rate2$datapath, sep = ",", fileEncoding = "GBK")[, "group"]
      }
    }
    
    result <- if (exists("group_rate2")) {
      meta_rate2_group(raw_rate2, sm_rate2, a_rate2, b_rate2, group_rate2)
    } else {
      meta_rate2(raw_rate2, sm_rate2, a_rate2, b_rate2)
    }
    
    draw_forest_plot <- function(filename, format, width, height, res = NULL) {
      if (format == "pdf") {
        pdf(filename, width = width, height = height)
      } else if (format == "png") {
        png(filename, width = width, height = height, res = res)
      }
      on.exit(dev.off(), add = TRUE)
      
      par(oma = c(0, 5, 1, 1), mar = c(0, 2, 2, 2))
      meta::forest(result, digits = 3, col.square.lines = 'white',
                   col.square = input$col.square_rate2,
                   col.study = input$col.study_rate2,
                   fontsize = input$fontsize_rate2)
    }
    
    draw_forest_plot(paste0(td, "/p1_rate.pdf"), "pdf", input$plotwidth_rate2/72, input$plotheight_rate2/72)
    draw_forest_plot(paste0(td, "/p1_rate.png"), "png", 5 * input$plotwidth_rate2, 5 * input$plotheight_rate2, 300)
    
    trimfill_rate2 <- input$trimfill_rate2 == "TRUE"
    
    draw_funnel_plot <- function(filename, format, width, height, res = NULL) {
      if (format == "pdf") {
        pdf(filename, width = width, height = height)
      } else if (format == "png") {
        png(filename, width = width, height = height, res = res)
      }
      on.exit(dev.off(), add = TRUE)
      
      par(mar = c(2, 3, 1, 3))
      par(pty = "s")  # 修改：设置图形区域为正方形
      if (trimfill_rate2) {
        meta::funnel(trimfill(result))
      } else {
        meta::funnel(result)
      }
    }
    
    draw_funnel_plot(paste0(td, "/p2_rate.pdf"), "pdf", input$plotwidth_rate2/72, input$plotheight_rate2/72)
    draw_funnel_plot(paste0(td, "/p2_rate.png"), "png", 5 * input$plotwidth_rate2, 5 * input$plotheight_rate2, 300)
    
    result
  })
  
  
  
  
  
  
  
  
  
  
  trimfill_rate2<-eventReactive(input$run_rate2, {
    ifelse(input$trimfill_rate2=="TRUE","T","F")
  })
  
  metaresult_rate_bias2<-eventReactive(input$run_rate2, {
    if(input$exam_rate2) {
      raw_rate2<-read.csv("www/survival rate3.csv",sep=",")
      
    } else {
      raw_rate2<-read.csv(input$file2_rate2$datapath,sep=",",fileEncoding="GBK") #row=1,header=T
      
    }
    b_rate2<-ifelse(input$model_rate2=="fixed effects model",TRUE,FALSE)
    a_rate2<-ifelse(input$model_rate2=="random effects model",TRUE,FALSE)
    sm_rate2<-input$sm_rate2
    
    metaresult_rate_bias2<-meta_rate2(raw_rate2,sm_rate2,a_rate2,b_rate2)
    metaresult_rate_bias2
  })
  
  p1_rate2<- eventReactive(input$run_rate2, {
    p1_rate2<-meta::forest(metaresult_rate2(),digits=3,col.square.lines='white',col.square=input$col.square_rate2,col.study=input$col.study_rate2,fontsize=input$fontsize_rate2
    )
    p1_rate2
  })
  
  
  Begg_p_rate2<-eventReactive(input$run_rate2, {
    round(metabias(metaresult_rate_bias2(), k.min = 3,method.bias = "Begg")$pval,4)
  })
  
  Egger_p_rate2<-eventReactive(input$run_rate2, {
    round(metabias(metaresult_rate_bias2(), k.min = 3,method.bias = "Egger")$pval,4)
  })
  
  
  
  
  # 生成比较表格的数据
  output$comparison_table2 <- renderDT({
    meta_results <- metaresult_rate2()
    
    req(meta_results)  # 确保数据可用
    group_levels <- unique(meta_results$byvar)  # 获取唯一值
    
    comparison_results <- data.frame(
      Comparison = character(),
      Difference_in_Effect = numeric(),
      Confidence_Interval = character(),
      P_Value = numeric(),
      stringsAsFactors = FALSE
    )
    
    # 根据用户选择的模型提取数据
    if (input$model_rate2 == "random effects model") {
      effect_field <- "TE.random.w"
      se_field <- "seTE.random.w"
    } else if (input$model_rate2 == "fixed effects model") {
      effect_field <- "TE.fixed.w"
      se_field <- "seTE.fixed.w"
    }
    
    if (length(group_levels) > 1) {
      for (i in 1:(length(group_levels) - 1)) {
        for (j in (i + 1):length(group_levels)) {
          md1 <- meta_results[[effect_field]][i]
          se1 <- meta_results[[se_field]][i]
          
          md2 <- meta_results[[effect_field]][j]
          se2 <- meta_results[[se_field]][j]
          
          md_diff <- md1 - md2
          se_diff <- sqrt(se1^2 + se2^2)
          
          alpha <- 0.05
          z_value <- qnorm(1 - alpha/2)
          
          ci_lower <- md_diff - z_value * se_diff
          ci_upper <- md_diff + z_value * se_diff
          ci_text <- paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")
          
          z_score <- md_diff / se_diff
          p_value <- 2 * (1 - pnorm(abs(z_score)))
          
          comparison_results <- rbind(comparison_results, data.frame(
            Comparison = paste0(group_levels[i], " vs ", group_levels[j]),
            Difference_in_Effect = round(md_diff, 2),
            Confidence_Interval = ci_text,
            P_Value = round(p_value, 4)
          ))
        }
      }
    }
    rownames(comparison_results) <- NULL
    datatable(comparison_results)
  })  # 新增的比较表格功能实现
  
  
  
  # 生成敏感性分析结果
  sensitivity_result_rate2 <- eventReactive(input$run_rate2, {
    meta_result_surv2 <- metaresult_rate2()
    metainf(meta_result_surv2)
  })
  
  
  
  
  
  observeEvent(input$run_rate2, {
    shinyjs::show("h_rate2")
    shinyjs::show("f_rate2")
    shinyjs::show("s_rate2")
    
    output$distPlot_rate2 <- renderPlot({
      plot <- p1_rate2()
      if (!is.null(plot)) {
        print(plot)
      }
      
    })
    
    output$Funnelplot_rate2 <- renderPlot({
      par(pty = "s")
      result <- metaresult_rate2()
      if (trimfill_rate() == "T" && !is.null(result)) {
        meta::funnel(trimfill(result))
      } else if (!is.null(result)) {
        meta::funnel(result)
      }
      
    })
    
    output$Egger_p_text_rate2 <- renderText({
      paste0("P value of Egger's test: ", Egger_p_rate2())
    })
    
    output$Begg_p_text_rate2 <- renderText({
      paste0("P value of Begg's test: ", Begg_p_rate2())
    })
    
    output$sensitivity_forest_rate2 <- renderPlot({
      result <- sensitivity_result_rate2()
      if (!is.null(result)) {
        print(meta::forest(result, digits = 3,
                           col.bg = input$col.square_sensitivity_rate2,
                           col.border = "white",
                           col = input$col.study_sensitivity_rate2,
                           fontsize = input$fontsize_sensitivity_rate2,
                           height = input$plotheight_sensitivity_rate2 / 72,
                           width = input$plotwidth_sensitivity_rate2 / 72,
                           spacing = 1.2))
      }
      
    })
  })
  
  
  
  
  output$downloadp3_rate2 <- downloadHandler(
    filename = function() {
      "p3_rate.pdf"
    },
    content = function(file) {
      
      pdf(file,
          width = input$plotwidth_sensitivity_rate2 / 72,
          height = input$plotheight_sensitivity_rate2 / 72)
      par(oma = c(0, 5, 1, 1), mar = c(0, 2, 2, 2)) # 设置边距
      result <- sensitivity_result_rate2()
      if (!is.null(result)) {
        print(meta::forest(result, digits = 3,
                           col.bg = input$col.square_sensitivity_rate2,
                           col.border = "white",
                           col = input$col.study_sensitivity_rate2,
                           fontsize = input$fontsize_sensitivity_rate2,
                           height = input$plotheight_sensitivity_rate2 / 72,
                           width = input$plotwidth_sensitivity_rate2 / 72,
                           spacing = 1.2))
      }
      dev.off()
    }
  )
  
  
  output$downloadp3png_rate2 <- downloadHandler(
    filename = function() {
      "p3_rate.png"
    },
    content = function(file) {
      png(file,
          width = 5 * input$plotwidth_sensitivity_rate2,
          height = 5 * input$plotheight_sensitivity_rate2,
          res = 300)
      par(oma = c(2, 0, 1, 0), mar = c(0, 5, 2, 2)) # 设置边距
      result <- sensitivity_result_rate2()
      if (!is.null(result)) {
        print(meta::forest(result, digits = 3,
                           col.bg = input$col.square_sensitivity_rate2,
                           col.border = "white",
                           col = input$col.study_sensitivity_rate2,
                           fontsize = input$fontsize_sensitivity_rate2,
                           height = input$plotheight_sensitivity_rate2 / 72,
                           width = input$plotwidth_sensitivity_rate2 / 72,
                           spacing = 1.2))
      }
      dev.off()
    }
  )
  
  
  
  
  
 
  
  
  
  
  
  
  
  
  #meta_dig----
  output$file3_dig <- renderUI({
    fileInput('file3_dig', 'Choose Primary File', accept = ".csv")
  })
  observeEvent(input$reset_dig, {
    output$file3_dig <- renderUI({
      fileInput('file3_dig', 'Choose Primary File', accept = ".csv")
    })
  })
  output$exam_dig <- renderUI({
    checkboxInput("exam_dig","Example Data",TRUE)
  })
  observeEvent(input$file3_dig, {
    output$exam_dig <- renderUI({
      checkboxInput("exam_dig","Example Data",FALSE)
    })
  })
  
  observeEvent(input$file3_dig, {
    updateCheckboxInput(session, "exam_dig", value = FALSE)
  }) 

  
  
  
  raw_dig1<-reactive({
    req(input$file3_dig)
    try(read.csv(input$file3_dig$datapath, header = TRUE,fileEncoding="GBK"))
  })
  
  d1_dig<- reactive({
    inFile1_dig <- input$file3_dig
    ext_dig <- tools::file_ext(inFile1_dig$datapath)  
    if(input$exam_dig){
      data <- read.csv("www/meta_dig.csv",sep=",")
      return(data) 
    }else{
      if (!is.null(input$file3_dig)&& ext_dig  == "csv"&&
          all(nms_dig%in% names(raw_dig1())) ){
        return(raw_dig1())
      }else{
        shiny::showNotification("Primary file: Please upload a valid csv file", type = "error")
        NULL
      }
    }
  })
  output$head_dig<- renderDT(
    d1_dig(),
    options = list(pageLength = 5)
  )
  #download 
  output$downloadBtn_dig<- downloadHandler(
    filename = function() {
      paste("meta_dig", "csv",sep='.')
    },
    content = function(file) {
      myfile <- srcpath <- 'www/meta_dig.csv'
      file.copy(myfile, file)
    }
  )
  META_dig<- eventReactive(input$run_dig, {
    if(input$exam_dig) {
      META_dig<-read.csv("www/meta_dig.csv",sep=",",row.names = 1)
    } else {
      META_dig<-read.csv(input$file3_dig$datapath,sep=",",row.names = 1,fileEncoding="GBK")
    }
    pdf(paste0(td,"/p2_dig.pdf"),
        width = input$plotwidth_dig1/72,
        height = input$plotheight_dig1/72
    )
    
    mada::ROCellipse(META_dig,pch = 1,col=input$col1,ellipsecol = input$ellipsecol)#col1
    mada::mslSROC(META_dig, add=T,col = input$color2, lwd = input$lwd, lty = input$lty)
    dev.off() 
    
    png(paste0(td,"/p2_dig.png"),
        width = 5*input$plotwidth_dig1,
        height = 5*input$plotheight_dig1,
        res=300)
    mada::ROCellipse(META_dig,pch = 1,col=input$col1,ellipsecol = input$ellipsecol)#col1
    mada::mslSROC(META_dig, add=T,col = input$color2, lwd = input$lwd, lty = input$lty)
    dev.off()   
    pdf(paste0(td,"/p3_dig.pdf"),
        width = input$plotwidth_dig2/72,
        height = input$plotheight_dig2/72
    )
    mada::crosshair(META_dig,col=1:100000,pch =input$pch,method = input$method)#pch =1
    dev.off() 
    
    png(paste0(td,"/p3_dig.png"),
        width = 5*input$plotwidth_dig2,
        height = 5*input$plotheight_dig2,
        res=300)
    mada::crosshair(META_dig,col=1:100000,pch =input$pch,method = input$method)#pch =1
    dev.off()  
    
    res_dig<-riley(META_dig,optimization = "Nelder-Mead")
    res_dig$slab<-as.character(row.names(META_dig))
    plot(res_dig,study.digits =1,
         col.diamond=input$col.diamond,col.predint=input$col.predint,
         size.study =input$size.study,size.predint = input$size.predint,refline = input$refline,lty.ref = input$lty.ref)
    pdf(paste0(td,"/p1_dig.pdf"),
        width = input$plotwidth_dig/72,
        height = input$plotheight_dig/72
    )
    plot(res_dig,study.digits =1,
         col.diamond=input$col.diamond,col.predint=input$col.predint,
         size.study =input$size.study,size.predint = input$size.predint,refline = input$refline,lty.ref = input$lty.ref)
    
    dev.off() 
    
    png(paste0(td,"/p1_dig.png"),
        width = 5*input$plotwidth_dig,
        height = 5*input$plotheight_dig,
        res=300)
    plot(res_dig,study.digits =1,
         col.diamond=input$col.diamond,col.predint=input$col.predint,
         size.study =input$size.study,size.predint = input$size.predint,refline = input$refline,lty.ref = input$lty.ref)
    dev.off()    
    
    META_dig
    
  })
  h1_dig<-eventReactive(input$run_dig, {
    res_dig<-riley(META_dig(),optimization = "Nelder-Mead")
    res_dig$slab<-as.character(row.names(META_dig()))
    h1_dig<-plot(res_dig,study.digits =1,
                 col.diamond=input$col.diamond,col.predint=input$col.predint,
                 size.study =input$size.study,size.predint = input$size.predint,refline = input$refline,lty.ref = input$lty.ref)
    h1_dig
  })
  h2_dig<-eventReactive(input$run_dig, {
    mada::ROCellipse(META_dig(),pch = 1,col=input$col1,ellipsecol = input$ellipsecol)#col1
    mada::mslSROC(META_dig(), add=T,col = input$color2, lwd = input$lwd, lty = input$lty)
  })
  h3_dig<-eventReactive(input$run_dig, {
    mada::crosshair(META_dig(),col=1:100000,pch =input$pch,method = input$method)#pch =1
  })
  observeEvent(input$run_dig,{  
    shinyjs::show("h1_dig")
    shinyjs::show("h2_dig")
    shinyjs::show("h3_dig")
    output$stp1_dig <- renderPlot({
      print(h1_dig())
      
    })
    output$stp2_dig <- renderPlot({
      print(h2_dig())
      
    })
    output$stp3_dig <- renderPlot({
      print(h3_dig())
    })
  })
  
  
  #network_dic----
  output$file1_net_dic <- renderUI({
    fileInput('file1_net_dic', 'Choose CSV File',accept = ".csv")
  })
  observeEvent(input$reset_net_dic, {
    output$file1_net_dic <- renderUI({
      fileInput('file1_net_dic', 'Choose CSV File',accept = ".csv")
    })
  })
  output$exam_net_dic <- renderUI({
    checkboxInput("exam_net_dic","Example Data",TRUE)
  })
  observeEvent(input$file1_net_dic, {
    output$exam_net_dic <- renderUI({
      checkboxInput("exam_net_dic","Example Data",FALSE)
    })
  })
  
  observeEvent(input$file1_net_dic, {
    updateCheckboxInput(session, "exam_net_dic", value = FALSE)
  }) 
 
  
  raw_net_dic1<-reactive({
    req(input$file1_net_dic)
    try(read.csv(input$file1_net_dic$datapath, header = TRUE,fileEncoding="GBK"))
  })
  
  di_net_dic<- reactive({
    inFile1_net_dic <- input$file1_net_dic
    ext <- tools::file_ext(inFile1_net_dic$datapath)
    
    if(input$exam_net_dic){
      data <- read.csv("www/network_dic.csv",sep=",") 
      return(data)
    }else{
      if (!is.null(input$file1_net_dic)&& ext == "csv"&&
          all(nms_net_dic%in% names(raw_net_dic1()))
      ){
        return(raw_net_dic1())
      }else{
        shiny::showNotification("Primary file: Please upload a valid csv file", type = "error")
        NULL
      }
    }
    
  })
  
  output$head_net_dic <- renderDT(
    di_net_dic(),
    options = list(pageLength = 5)
  )
  
  #download
  output$downloadBtn_net_dic <- downloadHandler(
    filename = function() {
      paste("network_dic", "csv",sep='.')
    },
    content = function(file) {
      myfile <- srcpath <- 'www/network_dic.csv'
      file.copy(myfile, file)
    }
  )
  
  
  
  raw_net_dic<-eventReactive(input$run_net_dic,{
    if(input$exam_net_dic) {
      raw_net_dic<-read.csv("www/network_dic.csv",sep=",")
    } else {
      raw_net_dic<-read.csv(input$file1_net_dic$datapath,sep=",",fileEncoding="GBK") #row=1,header=T
    }
  })
  
  
  
  
  
  
  
  
  
  
  # 构建 rank_probs_melted 数据
  rank_probs_melted <- eventReactive(input$run_net_dic, {
    req(raw_net_dic())
    
    # 构建网络荟萃分析模型
    nma_model <- set_agd_arm(
      raw_net_dic(),
      study = study,
      trt = treatment,
      r = r,
      n = n,
      trt_class = trt_class
    )
    
    # 提取排名概率并清洗
    nma(nma_model, trt_effects = "random", consistency = "consistency") %>%
      posterior_rank_probs(cumulative = FALSE) %>%
      as.matrix() %>%
      reshape2::melt() %>%
      filter(Var2 != ".trt") %>%
      mutate(
        value = as.numeric(value),
        Var1 = as.factor(gsub("d\\[(.*)\\]", "\\1", Var1)),
        Var2 = as.factor(gsub("p_rank\\[(.*)\\]", "\\1", Var2))
      ) %>%
      mutate(Var2 = factor(Var2, levels = sort(as.numeric(levels(Var2)))))
  })
  
  # 生成热图
  heatmap_plot <- eventReactive(input$run_net_dic, {
    req(rank_probs_melted())
    
    ggplot(rank_probs_melted(), aes(x = Var2, y = Var1, fill = value)) +
      geom_tile() +
      geom_text(
        aes(label = round(value, 2)),
        color = input$col.text_net_dic,  # 文字颜色
        size = 4,                        # 文字大小
        
      ) +
      scale_fill_gradient(
        low = input$col.square_net2,
        high = input$col.square_net1,
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        limits = c(0, 1)
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 12),  # 设置全局字体大小
        axis.text = element_text(size = 10),  # 坐标轴文字大小
        axis.title = element_text(size = 14)  # 坐标轴标题大小
      ) +
      labs(
        title = "Heat Map of Rank Probabilities",
        x = "Rank",
        y = "Treatment",
        fill = "Probability"
      )
  })
  
  # 显示热图
  output$stp5_net_dic <- renderPlot({
    req(heatmap_plot())
    heatmap_plot()
  })
  
  # 下载 PDF 文件
  output$downloadp5_net_dic <- downloadHandler(
    filename = function() {
      paste("p5_net_dic", "pdf", sep = ".")
    },
    content = function(file) {
      pdf(file, width = input$plotwidth_net_dic5/72, height = input$plotheight_net_dic5/72)  # 使用用户指定的宽高
      print(heatmap_plot())
      dev.off()
    }
  )
  
  # 下载 PNG 文件
  output$downloadp5png_net_dic <- downloadHandler(
    filename = function() {
      paste("p5_net_dic", "png", sep = ".")
    },
    content = function(file) {
      # 打开 PNG 文件设备
      png(file, 
          width = input$plotwidth_net_dic5 * 5,  # 宽度（像素）
          height = input$plotheight_net_dic5 * 5,  # 高度（像素）
          res = 300)  # 分辨率（DPI）
      
      # 显式调用图像对象
      print(heatmap_plot())
      
      # 关闭文件设备
      dev.off()
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##网络图绘制代码
  
  af_net_dic<-eventReactive(input$run_net_dic,{
    weight_edge_dic<- ifelse(input$weight_edges_dic=="TRUE",TRUE,FALSE)
    show_class_dic<-ifelse(input$show_trt_class_dic=="TRUE",TRUE,FALSE)
    af_net_dic <- if (input$show_trt_class_dic=="TRUE") {
      set_agd_arm(raw_net_dic(), 
                  study = study,
                  trt = treatment,
                  r = r,n = n,
                  trt_ref =raw_net_dic()$treatment[1] ,
                  trt_class = trt_class)
    } else {
      set_agd_arm(raw_net_dic(), 
                  study = study,
                  trt = treatment,
                  r = r,n = n,
                  trt_ref =raw_net_dic()$treatment[1] 
      )
    }
    
    # af_net_dic <-set_agd_arm(raw_net_dic(), 
    #                          study = study,
    #                          trt = treatment,
    #                          y = mean, 
    #                          se= std.dev,
    #                          trt_ref =raw_net_dic()$treatment[1] ,
    #                          sample_size = samplesize,
    #                          trt_class = trt_class)
    
    p1_net_dic<-plot(af_net_dic, weight_nodes =TRUE, weight_edges =weight_edge_dic, show_trt_class =show_class_dic) +
      ggplot2::theme(legend.position ="bottom", legend.box ="vertical")
    pdf(paste0(td,"/p1_net_dic.pdf"),
        width = input$plotwidth_net_dic/72,
        height = input$plotheight_net_dic/72)
    
    print(p1_net_dic)
    dev.off()
    
    png(paste0(td,"/p1_net_dic.png"),
        width =5*input$plotwidth_net_dic,
        height =5*input$plotheight_net_dic,
        res=300)
    print(p1_net_dic)
    dev.off()
    
    af_net_dic
  })
  
  
  
  
  ##森林图绘制
  af_fit_1_dic<-eventReactive(input$run_net_dic,{
    
    model_net_dic<-ifelse(input$model_net_dic=="fixed effects model",c("fixed"),c("random"))
    consistency_dic<-ifelse(input$consistency_dic=="consistency",c("consistency"),
                            ifelse(input$consistency_dic=="ume",c("ume"),c("nodesplit")))
    af_fit_1_dic<- nma(af_net_dic(), 
                       trt_effects = model_net_dic, #选择随机效应模型，若选择固定则输入fixed
                       consistency=consistency_dic #选择一致性模型，不一致模型则输入ume
    )
    
    af_1_releff_dic <-relative_effects(af_fit_1_dic, trt_ref =raw_net_dic()$treatment[1])
    p2_net_dic.pdf<-plot(af_1_releff_dic, ref_line =0)
    pdf(paste0(td,"/p2_net_dic.pdf"),
        width = input$plotwidth_net_dic2/72,
        height = input$plotheight_net_dic2/72)
    print(p2_net_dic.pdf)
    dev.off()
    
    png(paste0(td,"/p2_net_dic.png"),
        width =5*input$plotwidth_net_dic2,
        height =5*input$plotheight_net_dic2,
        res=300)
    #plot(af_1_releff, ref_line =0)
    print(p2_net_dic.pdf)
    dev.off()
    
    
    
    
    ##绘制排名概率图
    af_1_cumrankprobs <-posterior_rank_probs(af_fit_1_dic, cumulative =TRUE)
    p3_net_dic<- plot(af_1_cumrankprobs)
    pdf(paste0(td,"/p3_net_dic.pdf"),
        width = input$plotwidth_net_dic3/72,
        height = input$plotheight_net_dic3/72)
    #plot(af_1_cumrankprobs)
    print(p3_net_dic)
    dev.off()
    
    png(paste0(td,"/p3_net_dic.png"),
        width =5*input$plotwidth_net_dic3,
        height =5*input$plotheight_net_dic3,
        res=300)
    print(p3_net_dic)
    dev.off()
    
    
    
    ##绘制排名图
    af_1_ranks <-posterior_ranks(af_fit_1_dic)
    p4_net_dic<- plot(af_1_ranks)
    pdf(paste0(td,"/p4_net_dic.pdf"),
        width = input$plotwidth_net_dic4/72,
        height = input$plotheight_net_dic4/72)
    print(p4_net_dic)
    dev.off()
    
    png(paste0(td,"/p4_net_dic.png"),
        width =5*input$plotwidth_net_dic4,
        height =5*input$plotheight_net_dic4,
        res=300)
    print(p4_net_dic)
    dev.off()
    
    af_fit_1_dic
  })
  
  # h1_net_dic<-eventReactive(input$run_net_dic, {
  #   weight_edge_dic<- ifelse(input$weight_edges_dic=="TRUE",TRUE,FALSE)
  #   show_class_dic<-ifelse(input$show_trt_class_dic=="TRUE",TRUE,FALSE)
  #   h1_net<-plot(af_net_dic(), weight_nodes =TRUE, weight_edges =weight_edge_dic, show_trt_class =show_class_dic) +
  #     ggplot2::theme(legend.position ="bottom", legend.box ="vertical")
  #   
  #   h1_net_dic
  # })
  # h2_net_dic<-eventReactive(input$run_net_dic, {
  #   af_1_releff_dic <-relative_effects(af_fit_1_dic(), trt_ref =raw_net_dic()$treatment[1])
  #   h2_net_dic<-plot(af_1_releff_dic, ref_line =0)
  #   h2_net_dic
  # })
  
  
  # 准备数据
  heatmap_data <- eventReactive(input$run_net_dic, {
    af_fit <- af_fit_1_dic()
    rel_effects <- relative_effects(af_fit, trt_ref = raw_net_dic()$treatment[1])
    
    # rel_effects转换为矩阵
    matrix_data <- as.matrix(rel_effects$summary)
    
    # 转换为长格式
    melt_data <- reshape2::melt(matrix_data)
    colnames(melt_data) <- c("Treatment1", "Treatment2", "Effect")
    
    melt_data
  })
  
  
  
  
  
  
  observeEvent(input$run_net_dic,{  
    shinyjs::show("h1_net_dic")
    shinyjs::show("h2_net_dic")
    shinyjs::show("h3_net_dic")
    shinyjs::show("h4_net_dic")
    shinyjs::show("h5_net_dic")
    
    
    if(input$exam_net_dic) {
      raw_net_dic<-read.csv("www/network_dic.csv",sep=",")
    } else {
      raw_net_dic<-read.csv(input$file1_net_dic$datapath,sep=",",fileEncoding="GBK") #row=1,header=T
    }
    
    
    nma_model <- set_agd_arm(
      raw_net_dic,
      study = study,
      trt = treatment,
      r = r,
      n = n,
      trt_class = trt_class
    )
    
    
    rank_probs_melted <- nma(nma_model, trt_effects = "random", consistency = "consistency") %>%
      posterior_rank_probs(cumulative = FALSE) %>%
      as.matrix() %>%
      reshape2::melt() %>%
      filter(Var2 != ".trt") %>%
      mutate(
        value = as.numeric(value),
        Var1 = as.factor(gsub("d\\[(.*)\\]", "\\1", Var1)),
        Var2 = as.factor(gsub("p_rank\\[(.*)\\]", "\\1", Var2))
      ) %>%
      mutate(Var2 = factor(Var2, levels = sort(as.numeric(levels(Var2)))))
    
    
    output$stp1_net_dic <- renderPlot({
      #print(h1_net_dic())
      weight_edge_dic<- ifelse(input$weight_edges_dic=="TRUE",TRUE,FALSE)
      show_class_dic<-ifelse(input$show_trt_class_dic=="TRUE",TRUE,FALSE)
      plot(af_net_dic(), weight_nodes =TRUE, weight_edges =weight_edge_dic, show_trt_class =show_class_dic) +
        ggplot2::theme(legend.position ="bottom", legend.box ="vertical")
      
    })
    
    output$stp2_net_dic <- renderPlot({
      af_1_releff_dic <-relative_effects(af_fit_1_dic(), trt_ref =raw_net_dic()$treatment[1])
      plot(af_1_releff_dic, ref_line =0)
      # print(h2_net_dic())
      
    })
    output$stp3_net_dic <- renderPlot({
      p3<-posterior_rank_probs(af_fit_1_dic(), cumulative =TRUE)
      plot(p3)
      # print(h3_net_dic())
    })
    output$stp4_net_dic <- renderPlot({
      p4<-posterior_ranks(af_fit_1_dic())
      plot(p4)
      # print(h4_net_dic())
    })
    
    # 绘制热图
    output$stp5_net_dic <- renderPlot({
      
      
      req(rank_probs_melted)
      p5 <- ggplot(rank_probs_melted, aes(x = Var2, y = Var1, fill = value)) +
        geom_tile() +
        geom_text(aes(label = round(value, 2)), color = input$col.text_net_dic) +  # 使用自定义颜色
        scale_fill_gradient(low = input$col.square_net2, high = input$col.square_net1, breaks = c(0, 0.25, 0.5, 0.75, 1),
                            limits = c(0, 1)) +
        theme_minimal() +
        labs(title = "Heat Map of Rank Probabilities",
             x = "Rank",
             y = "Treatment",
             fill = "Probability")  
      plot(p5)
    })
    
  })
  
  
  
  
  

  
  #network_con--------------------------
  output$file1_net_con <- renderUI({
    fileInput('file1_net_con', 'Choose CSV File',accept = ".csv")
  })
  observeEvent(input$reset_net_con, {
    output$file1_net_con <- renderUI({
      fileInput('file1_net_con', 'Choose CSV File',accept = ".csv")
    })
  })
  output$exam_net_con <- renderUI({
    checkboxInput("exam_net_con","Example Data",TRUE)
  })
  observeEvent(input$file1_net_con, {
    output$exam_net_con <- renderUI({
      checkboxInput("exam_net_con","Example Data",FALSE)
    })
  })
  
  observeEvent(input$file1_net_con, {
    updateCheckboxInput(session, "exam_net_con", value = FALSE)
  })
  
  
  raw_net_con1<-reactive({
    req(input$file1_net_con)
    try(read.csv(input$file1_net_con$datapath, header = TRUE,fileEncoding="GBK"))
  })
  
  di_net_con<- reactive({
    inFile1_net_con <- input$file1_net_con
    ext <- tools::file_ext(inFile1_net_con$datapath)
    
    if(input$exam_net_con){
      data <- read.csv("www/network_con.csv",sep=",") 
      return(data)
    }else{
      if (!is.null(input$file1_net_con)&& ext == "csv"&&
          all(nms_net_con%in% names(raw_net_con1()))
      ){
        return(raw_net_con1())
      }else{
        shiny::showNotification("Primary file: Please upload a valid csv file", type = "error")
        NULL
      }
    }
    
  })
  
  output$head_net_con <- renderDT(
    di_net_con(),
    options = list(pageLength = 5)
  )
  
  #热图相关代码
  ##热图数据生成
  rank_probs_melted_con <- eventReactive(input$run_net_con, {
    nma_model_con <- set_agd_arm(
      raw_net_con(),
      study = study,
      trt = treatment,
      y = mean,
      se = std.dev,
      trt_ref = raw_net_con()$treatment[1],
      sample_size = n
    )
    
    rank_probs_melted <- nma(nma_model_con, trt_effects = "random", consistency = "consistency") %>%
      posterior_rank_probs(cumulative = FALSE) %>%
      as.matrix() %>%
      reshape2::melt() %>%
      filter(Var2 != ".trt") %>%
      mutate(
        value = as.numeric(value),
        Var1 = as.factor(gsub("d\\[(.*)\\]", "\\1", Var1)),
        Var2 = as.factor(gsub("p_rank\\[(.*)\\]", "\\1", Var2))
      ) %>%
      mutate(Var2 = factor(Var2, levels = sort(as.numeric(levels(Var2)))))
    
    rank_probs_melted
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #download
  output$downloadBtn_net_con <- downloadHandler(
    filename = function() {
      paste("network_con", "csv",sep='.')
    },
    content = function(file) {
      myfile <- srcpath <- 'www/network_con.csv'
      file.copy(myfile, file)
    }
  )
  raw_net_con<-eventReactive(input$run_net_con,{
    if(input$exam_net_con) {
      raw_net_con<-read.csv("www/network_con.csv",sep=",")
    } else {
      raw_net_con<-read.csv(input$file1_net_con$datapath,sep=",",fileEncoding="GBK") #row=1,header=T
    }
  })
  
  
  
  
  

  
  # 生成热图
  heatmap_plot_con <- eventReactive(input$run_net_con, {
    req(rank_probs_melted_con())
    
    ggplot(rank_probs_melted_con(), aes(x = Var2, y = Var1, fill = value)) +
      geom_tile() +
      geom_text(
        aes(label = round(value, 2)),
        color = input$col.text_net_con,  # 文字颜色
        size = 4,                        # 文字大小
        
      ) +
      scale_fill_gradient(
        low = input$col.square_net_con2,
        high = input$col.square_net_con1,
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        limits = c(0, 1)
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 12),  # 设置全局字体大小
        axis.text = element_text(size = 10),  # 坐标轴文字大小
        axis.title = element_text(size = 14)  # 坐标轴标题大小
      ) +
      labs(
        title = "Heat Map of Rank Probabilities",
        x = "Rank",
        y = "Treatment",
        fill = "Probability"
      )
  })
  
  # 显示热图
  output$stp5_net_con <- renderPlot({
    req(heatmap_plot_con())
    heatmap_plot_con()
  })
  
  # 下载 PDF 文件
  output$downloadp5_net_con <- downloadHandler(
    filename = function() {
      paste("p5_net_con", "pdf", sep = ".")
    },
    content = function(file) {
      pdf(file, width = input$plotwidth_net_con5/72, height = input$plotheight_net_con5/72)  # 使用用户指定的宽高
      print(heatmap_plot_con())
      dev.off()
    }
  )
  
  # 下载 PNG 文件
  output$downloadp5png_net_con <- downloadHandler(
    filename = function() {
      paste("p5_net_con", "png", sep = ".")
    },
    content = function(file) {
      # 打开 PNG 文件设备
      png(file, 
          width = input$plotwidth_net_con5 * 5,  # 宽度（像素）
          height = input$plotheight_net_con5 * 5,  # 高度（像素）
          res = 300)  # 分辨率（DPI）
      
      # 显式调用图像对象
      print(heatmap_plot_con())
      
      # 关闭文件设备
      dev.off()
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  af_net_con<-eventReactive(input$run_net_con,{
    weight_edge_con<- ifelse(input$weight_edges_con=="TRUE",TRUE,FALSE)
    show_class_con <- input$show_trt_class_con  
    show_class_con<-ifelse(input$show_trt_class_con=="TRUE",TRUE,FALSE)
    # af_net_con<- ifelse(input$show_trt_class_con=="TRUE",
    #        set_agd_arm(raw_net_con(),
    #                          study = study,
    #                          trt = treatment,
    #                          y = mean,
    #                          se= std.dev,
    #                          trt_ref =raw_net_con()$treatment[1] ,
    #                          sample_size = samplesize,
    #                          trt_class = trt_class),
    #        set_agd_arm(raw_net_con(),
    #                                 study = study,
    #                                 trt = treatment,
    #                                 y = mean,
    #                                 se= std.dev,
    #                                 trt_ref =raw_net_con()$treatment[1] ,
    #                                 sample_size = samplesizes)
    #                          )
    
    
    af_net_con <- if (input$show_trt_class_con=="TRUE") {
      set_agd_arm(raw_net_con(),
                  study = study,
                  trt = treatment,
                  y = mean,
                  se = std.dev,
                  trt_ref = raw_net_con()$treatment[1],
                  sample_size = n,
                  trt_class = trt_class)
    } else {
      set_agd_arm(raw_net_con(),
                  study = study,
                  trt = treatment,
                  y = mean,
                  se = std.dev,
                  trt_ref = raw_net_con()$treatment[1],
                  sample_size = n)
    }
    # af_net_con <-set_agd_arm(raw_net_con(),
    #                          study = study,
    #                          trt = treatment,
    #                          y = mean,
    #                          se= std.dev,
    #                          trt_ref =raw_net_con()$treatment[1] ,
    #                          sample_size = samplesize,
    #                          trt_class = trt_class)
    
    p1_net_con<-plot(af_net_con, weight_nodes =TRUE, weight_edges =weight_edge_con, show_trt_class =show_class_con) +
      ggplot2::theme(legend.position ="bottom", legend.box ="vertical")
    pdf(paste0(td,"/p1_net_con.pdf"),
        width = input$plotwidth_net_con/72,
        height = input$plotheight_net_con/72)
    
    print(p1_net_con)
    dev.off()
    
    png(paste0(td,"/p1_net_con.png"),
        width =5*input$plotwidth_net_con,
        height =5*input$plotheight_net_con,
        res=300)
    print(p1_net_con)
    dev.off()
    
    af_net_con
  })
  
  af_fit_1_con<-eventReactive(input$run_net_con,{
    
    model_net_con<-ifelse(input$model_net_con=="fixed effects model",c("fixed"),c("random"))
    consistency_con<-ifelse(input$consistency_con=="consistency",c("consistency"),
                            ifelse(input$consistency_con=="ume",c("ume"),c("nodesplit")))
    af_fit_1_con<- nma(af_net_con(), 
                       trt_effects = model_net_con, #选择随机效应模型，若选择固定则输入fixed
                       consistency=consistency_con #选择一致性模型，不一致模型则输入ume
    )
    
    af_1_releff_con <-relative_effects(af_fit_1_con, trt_ref =raw_net_con()$treatment[1])
    p2_net_con.pdf<-plot(af_1_releff_con, ref_line =0)
    pdf(paste0(td,"/p2_net_con.pdf"),
        width = input$plotwidth_net_con2/72,
        height = input$plotheight_net_con2/72)
    print(p2_net_con.pdf)
    dev.off()
    
    png(paste0(td,"/p2_net_con.png"),
        width =5*input$plotwidth_net_con2,
        height =5*input$plotheight_net_con2,
        res=300)
    #plot(af_1_releff, ref_line =0)
    print(p2_net_con.pdf)
    dev.off()
    
    af_1_cumrankprobs <-posterior_rank_probs(af_fit_1_con, cumulative =TRUE)
    p3_net_con<- plot(af_1_cumrankprobs)
    pdf(paste0(td,"/p3_net_con.pdf"),
        width = input$plotwidth_net_con3/72,
        height = input$plotheight_net_con3/72)
    #plot(af_1_cumrankprobs)
    print(p3_net_con)
    dev.off()
    
    png(paste0(td,"/p3_net_con.png"),
        width =5*input$plotwidth_net_con3,
        height =5*input$plotheight_net_con3,
        res=300)
    print(p3_net_con)
    dev.off()
    
    af_1_ranks <-posterior_ranks(af_fit_1_con)
    p4_net_con<- plot(af_1_ranks)
    pdf(paste0(td,"/p4_net_con.pdf"),
        width = input$plotwidth_net_con4/72,
        height = input$plotheight_net_con4/72)
    print(p4_net_con)
    dev.off()
    
    png(paste0(td,"/p4_net_con.png"),
        width =5*input$plotwidth_net_con4,
        height =5*input$plotheight_net_con4,
        res=300)
    print(p4_net_con)
    dev.off()
    
    af_fit_1_con
  })
  
  # h1_net_con<-eventReactive(input$run_net_con, {
  #   weight_edge_con<- ifelse(input$weight_edges_con=="TRUE",TRUE,FALSE)
  #   show_class_con<-ifelse(input$show_trt_class_con=="TRUE",TRUE,FALSE)
  #   h1_net<-plot(af_net_con(), weight_nodes =TRUE, weight_edges =weight_edge_con, show_trt_class =show_class_con) +
  #     ggplot2::theme(legend.position ="bottom", legend.box ="vertical")
  # 
  #   h1_net_con
  # })
  # h2_net_con<-eventReactive(input$run_net_con, {
  #   af_1_releff_con <-relative_effects(af_fit_1_con(), trt_ref =raw_net_con()$treatment[1])
  #   h2_net_con<-plot(af_1_releff_con, ref_line =0)
  #   h2_net_con
  # })
  
  observeEvent(input$run_net_con,{  
    shinyjs::show("h1_net_con")
    shinyjs::show("h2_net_con")
    shinyjs::show("h3_net_con")
    shinyjs::show("h4_net_con")
    shinyjs::show("h5_net_con")
    output$stp1_net_con <- renderPlot({
      #print(h1_net_con())
      weight_edge_con<- ifelse(input$weight_edges_con=="TRUE",TRUE,FALSE)
      show_class_con<-ifelse(input$show_trt_class_con=="TRUE",TRUE,FALSE)
      plot(af_net_con(), weight_nodes =TRUE, weight_edges =weight_edge_con, show_trt_class =show_class_con) +
        ggplot2::theme(legend.position ="bottom", legend.box ="vertical")
      
    })
    output$stp2_net_con <- renderPlot({
      af_1_releff_con <-relative_effects(af_fit_1_con(), trt_ref =raw_net_con()$treatment[1])
      plot(af_1_releff_con, ref_line =0)
      # print(h2_net_con())
      
    })
    output$stp3_net_con <- renderPlot({
      p3<-posterior_rank_probs(af_fit_1_con(), cumulative =TRUE)
      plot(p3)
      # print(h3_net_con())
    })
    output$stp4_net_con <- renderPlot({
      p4<-posterior_ranks(af_fit_1_con())
      plot(p4)
      # print(h4_net_con())
    })
    
    ##绘制热图
    
    output$stp5_net_con <- renderPlot({
      req(rank_probs_melted_con())
      p5 <- ggplot(rank_probs_melted_con(), aes(x = Var2, y = Var1, fill = value)) +
        geom_tile() +
        geom_text(aes(label = round(value, 2)), color = input$col.text_net_con) +  # 使用自定义颜色
        scale_fill_gradient(low = input$col.square_net_con2, high = input$col.square_net_con1, breaks = c(0, 0.25, 0.5, 0.75, 1),
                            limits = c(0, 1)) +
        theme_minimal() +
        labs(title = "Heat Map of Rank Probabilities",
             x = "Rank",
             y = "Treatment",
             fill = "Probability")  
      plot(p5)
    })
  })
  
  #Download Part----
  #在这里如果没找到对应的代码可以去各个模块后面看看，因为到了后期就直接在模块后面写了，比较方便（debug起来会比较麻烦……
  output$downloadp1 <- downloadHandler(
    filename <- function() {
      paste("p1", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1.pdf"), file)
    })
  
  output$downloadp1png <- downloadHandler(
    filename <- function() {
      paste("p1", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1.png"), file)
    })
  
  output$downloadp2_ris <- downloadHandler(
    filename <- function() {
      paste("p1_ris", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_ris.pdf"), file)
    })
  
  output$downloadp2_rispng <- downloadHandler(
    filename <- function() {
      paste("p1_ris", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_ris.png"), file)
    })
  output$downloadp1_dic <- downloadHandler(
    filename <- function() {
      paste("p1_dic", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_dic.pdf"), file)
    })
  
  output$downloadp1png_dic <- downloadHandler(
    filename <- function() {
      paste("p1_dic", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_dic.png"), file)
    })
  
  output$downloadp2_dic <- downloadHandler(
    filename <- function() {
      paste("p2_dic", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_dic.pdf"), file)
    })
  
  output$downloadp2png_dic <- downloadHandler(
    filename <- function() {
      paste("p2_dic", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_dic.png"), file)
    })
  output$downloadp1_con <- downloadHandler(
    filename <- function() {
      paste("p1_con", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_con.pdf"), file)
    })
  
  output$downloadp1png_con <- downloadHandler(
    filename <- function() {
      paste("p1_con", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_con.png"), file)
    })
  
  output$downloadp2_con <- downloadHandler(
    filename <- function() {
      paste("p2_con", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_con.pdf"), file)
    })
  
  output$downloadp2png_con <- downloadHandler(
    filename <- function() {
      paste("p2_con", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_con.png"), file)
    })
  
  
  download_plot <- function(temp_file, file_name, suffix){
    downloadHandler(
      filename <- function() {
        paste(file_name, suffix, sep=".")
      },
      content <- function(file) {
        file.copy(paste0(temp_file, paste0("/", file_name, ".", suffix)), file)
      })
  }
  
  output$downloadp1_dig <- download_plot(td, "p1_dig", "pdf")
  output$downloadp1png_dig <- download_plot(td, "p1_dig", "png")
  
  output$downloadp2_dig <- download_plot(td, "p2_dig", "pdf")
  output$downloadp2png_dig <- download_plot(td, "p2_dig", "png")
  
  output$downloadp3_dig <- download_plot(td, "p3_dig", "pdf")
  output$downloadp3png_dig <- download_plot(td, "p3_dig", "png")
  
  output$downloadp1_sig <- downloadHandler(
    filename <- function() {
      paste("p1_sig", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_sig.pdf"), file)
    })
  
  output$downloadp1png_sig <- downloadHandler(
    filename <- function() {
      paste("p1_sig", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_sig.png"), file)
    })
  
  output$downloadp2_sig <- downloadHandler(
    filename <- function() {
      paste("p2_sig", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_sig.pdf"), file)
    })
  
  output$downloadp2png_sig <- downloadHandler(
    filename <- function() {
      paste("p2_sig", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_sig.png"), file)
    })
  output$downloadp1_sig_con <- downloadHandler(
    filename <- function() {
      paste("p1_sig_con", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_sig_con.pdf"), file)
    })
  
  output$downloadp1png_sig_con <- downloadHandler(
    filename <- function() {
      paste("p1_sig_con", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_sig_con.png"), file)
    })
  
  output$downloadp2_sig_con <- downloadHandler(
    filename <- function() {
      paste("p2_sig_con", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_sig_con.pdf"), file)
    })
  
  output$downloadp2png_sig_con <- downloadHandler(
    filename <- function() {
      paste("p2_sig_con", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_sig_con.png"), file)
    }) 
  output$downloadp1_rate <- downloadHandler(
    filename <- function() {
      paste("p1_rate", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_rate.pdf"), file)
    })
  
  output$downloadp1png_rate <- downloadHandler(
    filename <- function() {
      paste("p1_rate", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_rate.png"), file)
    })
  
  output$downloadp2_rate <- downloadHandler(
    filename <- function() {
      paste("p2_rate", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_rate.pdf"), file)
    })
  
  output$downloadp2png_rate <- downloadHandler(
    filename <- function() {
      paste("p2_rate", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_rate.png"), file)
    })
  
  
  
  
  
  
  
  
  output$downloadp1_rate2 <- downloadHandler(
    filename <- function() {
      paste("p1_rate", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_rate.pdf"), file)
    })
  
  output$downloadp1png_rate2 <- downloadHandler(
    filename <- function() {
      paste("p1_rate", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_rate.png"), file)
    })
  
  output$downloadp2_rate2 <- downloadHandler(
    filename <- function() {
      paste("p2_rate", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_rate.pdf"), file)
    })
  
  output$downloadp2png_rate2 <- downloadHandler(
    filename <- function() {
      paste("p2_rate", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_rate.png"), file)
    })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$downloadp1_de <- downloadHandler(
    filename <- function() {
      paste("p1_de", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_de.pdf"), file)
    })
  
  output$downloadp1png_de <- downloadHandler(
    filename <- function() {
      paste("p1_de", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_de.png"), file)
    })
  
  output$downloadp2_de <- downloadHandler(
    filename <- function() {
      paste("p2_de", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_de.pdf"), file)
    })
  
  output$downloadp2png_de <- downloadHandler(
    filename <- function() {
      paste("p2_de", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_de.png"), file)
    })
  
  output$downloadp1_net_dic <- downloadHandler(
    filename <- function() {
      paste("p1_net_dic", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_net_dic.pdf"), file)
    })
  
  output$downloadp1png_net_dic <- downloadHandler(
    filename <- function() {
      paste("p1_net_dic", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_net_dic.png"), file)
    })
  
  output$downloadp2_net_dic <- downloadHandler(
    filename <- function() {
      paste("p2_net_dic", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_net_dic.pdf"), file)
    })
  
  output$downloadp2png_net_dic <- downloadHandler(
    filename <- function() {
      paste("p2_net_dic", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_net_dic.png"), file)
    })
  
  output$downloadp3_net_dic <- downloadHandler(
    filename <- function() {
      paste("p3_net_dic", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p3_net_dic.pdf"), file)
    })
  
  output$downloadp3png_net_dic <- downloadHandler(
    filename <- function() {
      paste("p3_net_dic", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p3_net_dic.png"), file)
    })
  output$downloadp4_net_dic <- downloadHandler(
    filename <- function() {
      paste("p4_net_dic", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p4_net_dic.pdf"), file)
    })
  
  output$downloadp4png_net_dic <- downloadHandler(
    filename <- function() {
      paste("p4_net_dic", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p4_net_dic.png"), file)
    })
  
  
  
  
  
  output$downloadp1_net_con <- downloadHandler(
    filename <- function() {
      paste("p1_net_con", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_net_con.pdf"), file)
    })
  
  output$downloadp1png_net_con <- downloadHandler(
    filename <- function() {
      paste("p1_net_con", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p1_net_con.png"), file)
    })
  
  output$downloadp2_net_con <- downloadHandler(
    filename <- function() {
      paste("p2_net_con", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_net_con.pdf"), file)
    })
  
  output$downloadp2png_net_con <- downloadHandler(
    filename <- function() {
      paste("p2_net_con", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p2_net_con.png"), file)
    })
  
  output$downloadp3_net_con <- downloadHandler(
    filename <- function() {
      paste("p3_net_con", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p3_net_con.pdf"), file)
    })
  
  output$downloadp3png_net_con <- downloadHandler(
    filename <- function() {
      paste("p3_net_con", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p3_net_con.png"), file)
    })
  output$downloadp4_net_con <- downloadHandler(
    filename <- function() {
      paste("p4_net_con", "pdf", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p4_net_con.pdf"), file)
    })
  
  output$downloadp4png_net_con <- downloadHandler(
    filename <- function() {
      paste("p4_net_con", "png", sep=".")
    },
    content <- function(file) {
      file.copy(paste0(td,"/p4_net_con.png"), file)
    })
  
  
  
  
  # 服务器逻辑可以在这里定义
}