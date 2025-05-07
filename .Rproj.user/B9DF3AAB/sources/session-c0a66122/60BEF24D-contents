set.seed(1234)
options(future.globals.maxSize = 200 * 1024^3)

server <- function(input,output,session){
  
  # server global var
  # base
  cat(R.Version()$version.string," \033[0m\n");
  cat(getwd()," \033[0m\n");
  
  
  
  #新变量-----------
  brain <<- NULL
  Selectdata <<- NULL
  obj <<- NULL
  obj_DimClu <<- NULL
  brain_DimClu <<- NULL
  
  genes_input <<- NULL
  genes <<- NULL
  obj_nomal <<- NULL
  brain_nomal <<- NULL
  all_markers <<- NULL
  gene_names <<- NULL
  mt_genes <<- NULL
  seurat_obj <<- NULL
  cells <<- NULL
  cortex<<- NULL
  refdata <<- NULL
  hvgs <<- NULL
  ####plot---------
  vlnplot1 <<- NULL
  vlnplot_aft <<- NULL
  bef_filter_Vlnplot <<- NULL
  Spatial_Feature_Plot <<- NULL
  umap_dim <<- NULL
  SpatialDim_Plot <<- NULL
  SpatialFeaturePlot_marker <<- NULL
  vlnplot_marker <<- NULL
  Dimplot_aut_plot <<- NULL
  DotPlot_plot_marker <<- NULL
  combined_plot_annot <<- NULL
  SpatialFeaturePlot_manual <<- NULL
  Vlnplot_plot_manual <<- NULL
  Dimplot_manual_plot <<- NULL
  DotPlot_plot_manual <<- NULL
  sc_image <<- NULL
  sc_spatialfeatureplot<<- NULL
  sc_dimplot<<- NULL
  sig_gene_symbols <<- NULL
  ref_SE <<- NULL
  
  show_next <<- reactiveVal(FALSE)
  ref_data <<- reactiveVal(NULL)
  
  # 菜单控制-------------------------------------------------------
  
  shinyjs::disable(selector = ".sidebar li a[data-value='Visual_data']")
  shinyjs::disable(selector = ".sidebar li a[data-value='Quality_control']")
  shinyjs::disable(selector = ".sidebar li a[data-value='one']")
  shinyjs::disable(selector = ".sidebar li a[data-value='ones']")
  shinyjs::disable(selector = ".sidebar li a[data-value='find_markers']")
  shinyjs::disable(selector = ".sidebar li a[data-value='aut_annot']")
  shinyjs::disable(selector = ".sidebar li a[data-value='manual_annot']")
  shinyjs::disable(selector = ".sidebar li a[data-value='two']")
  shinyjs::disable(selector = ".sidebar li a[data-value='sc']")
  shinyjs::disable(selector = ".sidebar li a[data-value='sc_next']")
  shinyjs::disable(selector = ".sidebar li a[data-value='kegg_analysis']")
  
  js.0 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=Quality_control]").css("color", "#808080"); });'
  shinyjs::runjs(js.0)
  
  js.1.1 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=Visual_data]").css("color", "#808080"); });'
  shinyjs::runjs(js.1.1)
  
  js.1 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=one]").css("color", "#808080"); });'
  shinyjs::runjs(js.1)
  
  js.2 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=ones]").css("color", "#808080");}); '
  shinyjs::runjs(js.2)
  
  
  js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=find_markers]").css("color", "#808080");});'
  shinyjs::runjs(js.8)
  
  
  js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=two]").css("color", "#808080");});'
  shinyjs::runjs(js.3.1)
  
  js.4 <- ' $(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=aut_annot]").css("color", "#808080");});'
  shinyjs::runjs(js.4)
  
  js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=manual_annot]").css("color", "#808080");});'
  shinyjs::runjs(js.41)
  
  js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=sc]").css("color", "#808080");});'
  shinyjs::runjs(js.5)
  
  
  js.6 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=kegg_analysis]").css("color", "#808080");});'
  shinyjs::runjs(js.6)
  
  js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=sc_next]").css("color", "#808080");});'
  shinyjs::runjs(js.7)
  
  
  observe({
    # 每当页面标签切换时，执行滚动到顶部的 JavaScript
    runjs('window.scrollTo(0, 0);')  # 滚动到页面顶部
  })
  
  
  runjs("
    document.getElementById('submit_Start').style.backgroundColor = '#8376a3';
    document.getElementById('submit_Start').style.borderColor = '#8376a3';
  ")
  ###按钮控制---------------
  
  shinyjs::disable("submit_GeneExpression_continue")
  shinyjs::disable("submit_sc_continue")
  shinyjs::disable("run_moransi")
  
  
  ####function-------------------------------------------
  SpatiallyVariableFeatures_workaround <- function(object, assay="SCT", selection.method = "moransi") {
    #' This is work around function to replace SeuratObject::SpatiallyVariableFeatures function.
    #' return ranked list of Spatially Variable Features
    
    # Check if object is a Seurat object
    if (!inherits(object, "Seurat")) {
      stop("object must be a Seurat object")
    }
    
    # Check if assay is a valid assay
    if (!assay %in% names(object@assays)) {
      stop("assay must be a valid assay")
    }
    
    # Extract meta.features from the specified object and assay
    data <- object@assays[[assay]]@meta.features
    
    # Select columns starting with the provided col_prefix
    moransi_cols <- grep(paste0("^", selection.method), colnames(data), value = TRUE)
    
    # Filter rows where "moransi.spatially.variable" is TRUE
    filtered_data <- data[data[[paste0(selection.method, ".spatially.variable")]], moransi_cols]
    
    # Sort filtered data by "moransi.spatially.variable.rank" column in ascending order
    sorted_data <- filtered_data[order(filtered_data[[paste0(selection.method, ".spatially.variable.rank")]]), ]
    
    # Return row names of the sorted data frame
    rownames(sorted_data)
  }
  # 数据加载--------------------------------------------------------
  observe({
    if(input$Selectdata == "Custom Data"){
      
      showNotification("Custom Data selected", type = "message", duration = 5)
      
      output$FileInputs <- renderUI({
        tagList(
          div(
            style = "line-height: 1.5; letter-spacing: normal; border: 2px solid #FFA500; padding: 10px; border-radius: 8px; box-shadow: 4px 4px 12px rgba(0, 0, 0, 0.2); background-color: #f9f9f9;",
            code("Both h5 files and spatial files are output from Space Ranger in the upstream analysis."),
            br(),
            "STshiny file input requires a spatial file in zip format, where the spatial folder must contain files related to tissue sections and spatial coordinates.",
            br(),"including:",
            strong(HTML("tissue_hires_image.png<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;tissue_lowres_image.png<br>
                        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;scalefactors_json.json<br>
                        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;tissue_positions_list.csv")),
            align = "justify"
          ),br(),
          fileInput("fileH5Zip", "h5 Data：", placeholder = "H5 format data",accept = c(".h5")),
          fileInput("fileSpatialZip", "Spatial Zip File：", placeholder = "spatial.zip", accept = c(".zip"))
        )})
    }else{ 
      showNotification("stxBrain selected", type = "message", duration = 2)
      output$FileInputs <- renderUI({
        tagList(
          div(
            style = "line-height: 1.5; letter-spacing: normal; border: 2px solid #FFA500; padding: 10px; border-radius: 8px; box-shadow: 4px 4px 12px rgba(0, 0, 0, 0.2); background-color: #f9f9f9;",
            
            code("The stxBrain dataset is a spatial transcriptomics dataset of sagittal sections of the mouse brain, generated using 10x Genomics' Visium technology."),
            "The dataset contains two consecutive forebrain sections and two matching hindbrain sections. The sample data is the spatial single-cell data of the expression matrix of",
            strong("31,053 genes in 2,696 samples."),
            align = "justify"
          )
        )
      })
    }
  })
  
  observeEvent(input$submit_Start,{
    cat("\033[31m","Create SeuratObject! \033[0m\n");
    id<-showNotification("Load Data Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    clean(TRUE)  # 尝试清理变量
    
    if(input$Selectdata == "stxBrain"){
      print("...............................加载实例数据")
      progressSweetAlert(
        session = session,
        id = "simulationProgress1",
        title = div("Load Data Runing...,",br(),
                    "It will take some time, please be patient and wait..."),
        display_pct = TRUE,
        value = 20
      )
      brain<<-readRDS("brain_data.rds")
      updateProgressBar(
        session = session,
        id = "simulationProgress1",
        title = "Load Data Runing...",
        value = 50
      )
      print("----------已经加载空转数据------")
      print(brain)
      
      updateProgressBar(session = session,id = "simulationProgress1",title = "Load Data Runing...",value = 100)
      closeSweetAlert(session = session)
      sendSweetAlert(  
        title = "Done",  
        text = "Load Data Completed",  
        type = "success",
      )
      showNotification("Load Data End of run！", type = "message",duration = 2)
      
      shinyjs::enable(selector = ".sidebar li a[data-value='Quality_control']")
      js.1 <- '
            $(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=Quality_control]").css("color", "#ded7e1");
            });
          '
      shinyjs::runjs(js.1)
      updateTabItems(session, "inTabset", selected = "Quality_control")
    }
    if(input$Selectdata == "Custom Data"){
      progressSweetAlert(session = session,id = "simulationProgress2",
                         title = div("Load Data Runing...,",br(),"It will take some time, please be patient and wait..."),
                         display_pct = TRUE,value =10)
      
      print("-----------------------------------用户自定义输入数据")
      req(input$fileSpatialZip)
      
      # 定义文件夹路径 input$fileMatrix_h5,
      
      SpatialDataFolder <- file.path(tempdir(), "SpatialDataFolder")
      
      
      # 检查并处理 SpatialDataFolder
      if (dir.exists(SpatialDataFolder)) {
        # 获取文件夹中所有文件和子文件夹的路径
        files <- list.files(SpatialDataFolder, full.names = TRUE)
        # 删除所有文件和子文件夹
        unlink(files, recursive = TRUE)
      } else {
        # 如果文件夹不存在，则创建它
        dir.create(SpatialDataFolder, recursive = TRUE)
      }
      
      updateProgressBar(session = session,id = "simulationProgress2",value = 20)
      # 将上传的文件保存到临时文件夹中
      h5FilePath <- file.path(SpatialDataFolder, input$fileH5Zip$name)
      updateProgressBar(session = session,id = "simulationProgress2",value = 25)
      zipFilePath <- file.path(SpatialDataFolder, input$fileSpatialZip$name)
      updateProgressBar(session = session,id = "simulationProgress2",value = 30)
      # 将文件移动到临时文件夹中
      file.copy(input$fileSpatialZip$datapath, zipFilePath)
      file.copy(input$fileH5Zip$datapath,h5FilePath)
      # 解压 zip 文件到同一目录
      updateProgressBar(session = session,id = "simulationProgress2",value = 40)
      unzip(input$fileSpatialZip$datapath,exdir = SpatialDataFolder)
      
      updateProgressBar(session = session,id = "simulationProgress2",value = 60)
      subdirs_spatial <- list.dirs(SpatialDataFolder, recursive = FALSE)#列出DataFolderPath的子目录 spatial
      if (length(subdirs_spatial) > 1) {
        stop("在解压后的目录中发现了多个顶层文件夹，无法确定正确的文件夹名。")
      } else if (length(subdirs_spatial) == 0) {
        stop("在解压后的目录中没有找到任何文件夹。")
      } else {
        SpatialName <- basename(subdirs_spatial[1]) # 获取文件夹名（不包括路径）
        SpatialPath <- subdirs_spatial[1] # 获取文件夹的完整路径
        cat("解压后的spatial文件夹名是：", SpatialName, "\n")
        cat("解压后的spatial文件夹路径是：",SpatialPath, "\n")
      }
      updateProgressBar(session = session,id = "simulationProgress2",value = 80)
      
      # 打印确认信息
      cat("h5 file and special.zip have been processed and stored in the same directory.\n")
      cat("文件夹里有哪些文件：",list.files(SpatialDataFolder),"\n")
      
      filename_h5 <- input$fileH5Zip$name
      cat("h5文件名为：",filename_h5,"\n")
      obj <<- Load10X_Spatial(data.dir = SpatialDataFolder,
                              filename = filename_h5,
                              assay = "Spatial", 
                              slice = "slice1")
      
      cat("-------------整理数据完成")
      updateProgressBar(session = session,id = "simulationProgress2",value = 100)
      
      showNotification("Data Loaded Successfully", type = "default", duration = 3)
      print("-----------------matrix和spatia文件加载成功")
      print(obj)
      
      closeSweetAlert(session = session)
      
      sendSweetAlert(title = "Done",text = "Load Data Completed",type = "success")
      showNotification("Load Data End of run！", type = "message")
      
      shinyjs::enable(selector = ".sidebar li a[data-value='Quality_control']")
      js.0 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=Quality_control]").css("color", "#ded7e1");});'
      shinyjs::runjs(js.0)
      updateTabItems(session, "inTabset", selected = "Quality_control")
    }
    output$Vlnplot_before <- renderPlot({
      if(input$Selectdata == "stxBrain"){
        vlnplot1 <<- VlnPlot(brain, features = c("nFeature_Spatial", "nCount_Spatial"), pt.size = 0.1) + NoLegend()
      }else{
        vlnplot1 <<- VlnPlot(obj, features = c("nFeature_Spatial", "nCount_Spatial"), pt.size = 0.1) + NoLegend()
      }
      print(vlnplot1)
    })
    
  })
  
  #质控--------------------------------------
  observeEvent(input$submit_QC,{
    id<-showNotification("Quality control Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    updateTabsetPanel(session, "Filter_plots", selected = "Vlnplot_after")
    
    
    if(input$Selectdata=="stxBrain"){
      brain <<- subset(brain, subset = nFeature_Spatial >= input$nFeature_Spatial_min & nFeature_Spatial<=input$nFeature_Spatial_max &nCount_Spatial>= input$nCount_Spatial_min & nCount_Spatial<= input$nCount_Spatial_max )
    }else{
      obj <<- subset(obj, subset = nFeature_Spatial >= input$nFeature_Spatial_min & nFeature_Spatial<=input$nFeature_Spatial_max &nCount_Spatial>= input$nCount_Spatial_min & nCount_Spatial<=input$nCount_Spatial_max )
      
    }
    
    output$Vlnplot_after <- renderPlot({
      
      if(input$Selectdata=="stxBrain"){
        vlnplot_aft <<- VlnPlot(brain, features = c("nFeature_Spatial", "nCount_Spatial"), pt.size = 0.1) + NoLegend()
        
      }else{
        vlnplot_aft <<- VlnPlot(obj, features = c("nFeature_Spatial", "nCount_Spatial"), pt.size = 0.1) + NoLegend()
        
      }
      print(vlnplot_aft)
    })
  })
  
  observeEvent(input$submit_skip,{
    
    closeSweetAlert(session = session)
    
    shinyjs::enable(selector = ".sidebar li a[data-value='Visual_data']")
    js.1 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=Visual_data]").css("color", "#ded7e1");});'
    shinyjs::runjs(js.1)
    updateTabItems(session, "inTabset", selected = "Visual_data")
    
  })
  
  observeEvent(input$submit_next,{
    closeSweetAlert(session = session)
    
    shinyjs::enable(selector = ".sidebar li a[data-value='Visual_data']")
    js.1 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=Visual_data]").css("color", "#ded7e1");});'
    shinyjs::runjs(js.1)
    updateTabItems(session, "inTabset", selected = "Visual_data")
    
    
  })
  
  
  #Visual_data---------------------------
  observeEvent(input$submit_Visual_data,{
    
    output$brain_Vlnplot <- renderPlot({
      
      cat("\033[31m","QC:before filter Vlnplot \033[0m\n");
      if (input$Selectdata == "stxBrain") {
        # 根据选择的特征来决定绘制的特征
        feature_choice <- if (input$brain_Vlnplot_SpatialChoice == "nCount_Spatial") {
          "nCount_Spatial"
        } else {
          "nFeature_Spatial"
        }
        vln.plot <- VlnPlot(brain, features = feature_choice, pt.size = input$pt_size) + NoLegend()
        feature.plot <- SpatialFeaturePlot(brain, features = feature_choice,pt.size.factor =input$pt_size) + theme(legend.position = "right")
        bef_filter_Vlnplot <<- wrap_plots(vln.plot, feature.plot)
        print(bef_filter_Vlnplot)
      }
      if(input$Selectdata == "Custom Data"){
        feature_choice <- if (input$brain_Vlnplot_SpatialChoice == "nCount_Spatial") {
          "nCount_Spatial"
        } else {
          "nFeature_Spatial"
        }
        vln.plot <- VlnPlot(obj, features = feature_choice, pt.size =  input$pt_size) + NoLegend()
        feature.plot <- SpatialFeaturePlot(obj, features = feature_choice,pt.size.factor =input$pt_size) + theme(legend.position = "right")
        
        bef_filter_Vlnplot <<- wrap_plots(vln.plot, feature.plot)
        print(bef_filter_Vlnplot)
      }
    })  
    
    shinyjs::enable(selector = ".sidebar li a[data-value='one']")
    js.1 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=one]").css("color", "#ded7e1");});'
    shinyjs::runjs(js.1)
    
    
  })
  
  
  #归一化和标准化----------------------------------------------
  
  
  observeEvent(input$submit_GeneExpression,{
    # 显示运行中的通知消息
    id<-showNotification("Gene expression analysis Running...", duration = 5,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    # updateActionButton(session, "submit_GeneExpression_continue", label = "Continue analysis", icon = icon("play"))
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing data...", value = 0)
    on.exit(progress$close())  # 确保退出时关闭进度条
    
    
    tryCatch({
      
      progress$set(value = 0.3, message = "Standardization is in progress and may take a long time...")
      
      if(input$Selectdata == "stxBrain"){
        brain_nomal <<- readRDS("brain_SCTransform.rds")
        
        progress$set(value = 0.6, message = "Applying SCTransform...")
        
        DefaultAssay(brain_nomal) <<- "SCT"
        hvgs <<- brain_nomal@assays[["SCT"]]@SCTModel.list[["counts"]]@feature.attributes
        
        progress$set(value = 0.8, message = "Applying SCTransform...")
        
        hvgs <- hvgs[,1:5]
      }else{
        
        progress$set(value = 0.4, message = "Filtering invalid data...")
        
        #计算运行时间
        tic("Filtering step")
        if(any(obj$nCount_Spatial <= 0, na.rm = TRUE)){
          obj <- subset(obj, nCount_Spatial>0)
        }
        toc()
        
        progress$set(value = 0.8, message = "Applying SCTransform and may take a long time...")
        
        tic("SCTransform step")
        obj_nomal <<- SCTransform(obj, method = "glmGamPoi",assay = "Spatial", verbose = FALSE,return.only.var.genes = TRUE)
        toc()
        
        DefaultAssay(obj_nomal) <<- "SCT"
        hvgs <<- obj_nomal@assays[["SCT"]]@SCTModel.list[["counts"]]@feature.attributes
        hvgs <<- hvgs[,1:5]
        
        # 启用下载按钮并恢复样式
        shinyjs::enable("GeneExpression_table_download")
        
      }
      progress$set(value = 0.9, message = "Finalizing table...")
    }, error = function(e) {
      message(paste("Error processing SCTransform:", e$message))
    })
    
    output$hvgs_df <- renderDT({
      
      datatable(hvgs,options = list(scrollX = TRUE))
      
      
    })
    shinyjs::enable("submit_GeneExpression_continue")
  })
  
  observeEvent(input$submit_GeneExpression_continue,{
    
    if (!input$submit_GeneExpression) {
      showNotification("Please click 'Start' first to run the analysis.", type = "error", duration = 4)
    } else {
      
      output$SpatialFeaturePlot <- renderPlot({
        
        cat("\033[31m"," GE:SpatialFeaturePlot \033[0m\n"); 
        genes_input1 <- gsub('[""]', '', input$genes_input1)  # 去除输入中的引号
        
        pt_size_input <- input$pt_size_input
        
        genes1 <- unlist(strsplit(genes_input1, split = ",\\s*")) 
        
        
        # 如果输入框为空，显示提示消息
        if (genes_input1 == "") {
          showNotification("Please enter a gene name！", type = "warning")
          return(NULL)  # 停止执行后续代码
        }
        
        
        # 检查输入的基因是否在seurat_obj中
        if(input$Selectdata == "stxBrain"){
          missing_genes1 <- setdiff(genes1, rownames(brain_nomal))
        }else{
          missing_genes1 <- setdiff(genes1, rownames(obj_nomal))
          
        }
        if (length(missing_genes1) > 0) {
          # 如果有缺失的基因，显示通知消息
          showNotification(paste("The following genes do not exist in the data:", paste(missing_genes1, collapse = ", ")), type = "error")
          
          # 停止执行绘图
          return(NULL)
        }
        
        
        cat("输入的基因为：",genes1,"\n")
        
        if(input$Selectdata == "stxBrain"){
          print("stxBrain")
          
          cat("检查基因是否在数据中：", genes1 %in% rownames(brain_nomal), "\n")
          Spatial_Feature_Plot <<- SpatialFeaturePlot(brain_nomal, features = genes1,pt.size.factor = pt_size_input)
        }
        if(input$Selectdata == "Custom Data"){
          
          Spatial_Feature_Plot <<- SpatialFeaturePlot(obj_nomal, features = genes1,pt.size.factor = pt_size_input)
        }
        print(Spatial_Feature_Plot)
        
        showNotification("Standardization and normalization have been completed！", type = "message",duration = 6)
        
        shinyjs::enable(selector = ".sidebar li a[data-value='ones']")
        js.2 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=ones]").css("color", "#ded7e1");});'
        shinyjs::runjs(js.2)
      })
    }
  })
  #降维聚类-----------------------------------------------------------------
  observeEvent(input$submit_DimClu,{
    id<-showNotification("Dim Reduction & Clustering Running...", duration = 5,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    clean_filter(TRUE)
    cat("\033[31m","  Dim Reduction & Clustering \033[0m\n");
    
    
    
    if(input$Selectdata=="stxBrain"){
      seurat_obj<<- brain_nomal
    }else{
      seurat_obj<<- obj_nomal
    }
    
    
    tryCatch({
      progress <- Progress$new()
      on.exit(progress$close())  # 确保即使发生错误，进度条也会关闭
      progress$set(value = 0, message = "Starting...")
      
      tic("降维聚类 step")
      progress$set(value = 0.2, message = "Running PCA...")
      seurat_obj <<- seurat_obj %>%
        RunPCA(assay = "SCT", verbose = FALSE)
      
      progress$set(value = 0.4, message = "Finding Neighbors...")
      seurat_obj <<- seurat_obj %>%
        FindNeighbors(reduction = "pca", dims = 1:30)
      
      progress$set(value = 0.6, message = "Finding Clusters...")
      seurat_obj <<- seurat_obj %>%
        FindClusters(verbose = FALSE)
      
      progress$set(value = 0.8, message = "Running UMAP...")
      seurat_obj <<- seurat_obj %>%
        RunUMAP(reduction = "pca", dims = 1:30)
      
      toc()
      
      progress$set(value = 1, message = "Completed!")
      
    }, error = function(e) {
      cat("Error in Dim Reduction & Clustering:", e$message, "\n")
    })
    
    output$umap_dim <- renderPlot({
      req(input$submit_DimClu)
      cat("\033[31m"," Dim Reduction:umap_dim \033[0m\n");
      
      label_size <- input$label_size
      
      
      plot_umap <- DimPlot(seurat_obj, reduction = "umap", label = TRUE)
      plot_spatial_dim <- SpatialDimPlot(seurat_obj, label = TRUE, label.size = label_size)
      
      umap_dim <<- plot_umap +plot_spatial_dim
      print(umap_dim)
    })
    
    output$SpatialDimPlot<-renderPlot({
      req(input$submit_DimClu)
      pt_size_input<- input$pt_size_input
      idents <- as.integer(unlist(strsplit(as.character(input$idents_input), ",")))
      cat("输入的idents是",idents,"\n")
      
      SpatialDim_Plot <<- SpatialDimPlot(seurat_obj, cells.highlight = CellsByIdentities(object = seurat_obj, idents = idents), facet.highlight = TRUE, ncol = 3,pt.size.factor =pt_size_input)
      
      print(SpatialDim_Plot)
    })
    
    showNotification("Dim Reduction & Clustering End of run！", type = "message")
    
    shinyjs::enable(selector = ".sidebar li a[data-value='find_markers']")
    js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=find_markers]").css("color", "#ded7e1");});'
    shinyjs::runjs(js.8)
    
    
    
  })
  
  # 可视化marker ----------------------------------------------------------------------
  observe({
    addPopover(session, "q1", title = NULL,
               content = HTML("<strong>detection_rate:</strong>  The frequency with which the gene is detected in the dataset.<br>
                             <strong>gmean:</strong>The geometric mean of the gene, usually used to measure gene expression levels.<br>
                             <strong>variance:</strong>The variance of gene expression data indicates the degree of variation in the expression level of a gene in all samples.<br>
                            <strong>residual_mean:</strong>represents the mean of the residuals. A smaller residual mean may mean a better model fit.<br>
                            <strong>residual_variance: </strong>It is used to measure the error or variation remaining after the model is fitted. A larger value means that the model fits poorly.<br>
                              "),
               placement = "right", trigger = "hover", options = list(container = "body"))
  })
  
  observeEvent(input$submit_FM,{
    
    if (exists("all_markers") && !is.null(all_markers) && nrow(all_markers) > 0) {
      return(datatable(all_markers, options = list(scrollX = TRUE)))
    }
    
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(value = 0.2, message = "Finding Markers...")
    
    all_markers <<- cosg(
      seurat_obj,
      groups='all',
      assay='SCT',
      slot='data',
      mu=1,         #惩罚项参数，值越大
      remove_lowly_expressed=TRUE,   #是否过滤低表达基因
      expressed_pct=0.1,             #设置低表达的阈值
      n_genes_user=50      #每个cluster定义Top-N个marker gene
    )
    progress$set(value = 0.3, message = "Arranging data...")
    
    all_markers <<- as.data.frame(all_markers)
    
    
    
    progress$set(value = 0.4, message = "Arranging data...")
    
    gene_cols <- grep("^names", colnames(all_markers), value = TRUE)
    score_cols <- grep("^scores", colnames(all_markers), value = TRUE)
    
    
    
    progress$set(value = 0.7, message = "Arranging data...")
    
    # 按照编号匹配，确保列是相邻的
    ordered_cols <- as.vector(rbind(gene_cols, score_cols))
    
    # 重新排列数据框列顺序
    all_markers <<- all_markers[, ordered_cols]
    colnames(all_markers) <<- gsub("^names", "cluster", colnames(all_markers))
    
    progress$set(value = 0.9, message = "Arranging data...")
    
    sig_gene_symbols <<- all_markers[, grep("^cluster", colnames(all_markers))]
    
    
    progress$set(value = 1, message = "Arranging data...")
    
    output$de_markers_df <- renderDT({
      
      # if (exists("all_markers") && !is.null(all_markers) && nrow(all_markers) > 0) {
      #   return(datatable(all_markers, options = list(scrollX = TRUE)))
      # }
      # 
      # progress <- Progress$new()
      # on.exit(progress$close())
      # progress$set(value = 0.2, message = "Finding Markers...")
      # 
      # all_markers <- cosg(
      #   seurat_obj,
      #   groups='all',
      #   assay='SCT',
      #   slot='data',
      #   mu=1,         #惩罚项参数，值越大
      #   remove_lowly_expressed=TRUE,   #是否过滤低表达基因
      #   expressed_pct=0.1,             #设置低表达的阈值
      #   n_genes_user=50      #每个cluster定义Top-N个marker gene
      # )
      # progress$set(value = 0.3, message = "Arranging data...")
      # 
      # all_markers <<- as.data.frame(all_markers)
      # 
      # 
      # 
      # progress$set(value = 0.4, message = "Arranging data...")
      # 
      # gene_cols <- grep("^names", colnames(all_markers), value = TRUE)
      # score_cols <- grep("^scores", colnames(all_markers), value = TRUE)
      # 
      # 
      # 
      # progress$set(value = 0.7, message = "Arranging data...")
      # 
      # # 按照编号匹配，确保列是相邻的
      # ordered_cols <- as.vector(rbind(gene_cols, score_cols))
      # 
      # # 重新排列数据框列顺序
      # all_markers <<- all_markers[, ordered_cols]
      # colnames(all_markers) <<- gsub("^names", "cluster", colnames(all_markers))
      # 
      # progress$set(value = 0.9, message = "Arranging data...")
      # 
      # sig_gene_symbols <<- all_markers[, grep("^cluster", colnames(all_markers))]
      # 
      # 
      # progress$set(value = 1, message = "Arranging data...")
      
      datatable(all_markers,options = list(scrollX = TRUE))
      
      
    })
    
    
    shinyjs::enable(selector = ".sidebar li a[data-value='aut_annot']")
    js.3 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=aut_annot]").css("color", "#ded7e1");});'
    shinyjs::runjs(js.3)
    
    shinyjs::enable(selector = ".sidebar li a[data-value='manual_annot']")
    js.4 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=manual_annot]").css("color", "#ded7e1");});'
    shinyjs::runjs(js.4)
    
    shinyjs::enable(selector = ".sidebar li a[data-value='sc']")
    js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sc]").css("color", "#ded7e1");});'
    shinyjs::runjs(js.5)
    
    shinyjs::enable(selector = ".sidebar li a[data-value='kegg_analysis']")
    js.6 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=kegg_analysis]").css("color", "#ded7e1");
            });'
    shinyjs::runjs(js.6)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$submit_de_markers_df,{
    # id<-showNotification("Normalization & Linear Dimensional Reduction Running...", duration = NULL,closeButton = FALSE)
    # on.exit(removeNotification(id), add = TRUE)
    # cat("\033[31m","Subpopulation marker gene analysis! \033[0m\n");
    
    
    output$SpatialFeaturePlot_marker <- renderPlot({
      top_input1 <- input$top_input
      cat("top_input的值为：",top_input1,"\n")
      
      if(input$markers_plots == "SpatialFeaturePlot_marker") {
        
        # 检查全局变量 SpatialFeaturePlot_marker 是否存在且不为空
        if (exists("SpatialFeaturePlot_marker") && !is.null(SpatialFeaturePlot_marker) && nrow(SpatialFeaturePlot_marker) > 0) {
          return(datatable(SpatialFeaturePlot_marker, options = list(scrollX = TRUE)))
        }
        
        progress <- Progress$new()
        on.exit(progress$close())  # 确保即使发生错误，进度条也会关闭
        progress$set(value = 0.4, message = "Finding Variable Features...")
        
        print("-------------1")
        seurat_obj <- FindSpatiallyVariableFeatures(seurat_obj, assay = "SCT", features = VariableFeatures(seurat_obj)[1:500],selection.method = "moransi",verbose = T)        
        
        progress$set(value = 0.8, message = "Finding Variable Features...")
        
        top.features <- head(SpatiallyVariableFeatures_workaround(seurat_obj, selection.method = "moransi"), top_input1)
        SpatialFeaturePlot_marker <<- SpatialFeaturePlot(seurat_obj, features = top.features, ncol = 4, alpha = c(0.1, 1),pt.size.factor = input$pt_size_input3)
        
        print(SpatialFeaturePlot_marker)
        
      }
    })
    
    output$vlnplot_marker <- renderPlot({
      genes_input2 <- gsub('[""]', '', input$genes_input2)  # 去除输入中的引号
      pt_size_input2 <- input$pt_size_input2
      
      # 如果输入框为空，显示提示消息
      if (genes_input2 == "") {
        showNotification("Please enter a gene name！", type = "warning")
        return(NULL)  # 停止执行后续代码
      }
      
      # 将输入的基因名转化为向量
      genes2 <- unlist(strsplit(genes_input2, split = ",\\s*")) 
      
      # 检查输入的基因是否在seurat_obj中
      missing_genes <- setdiff(genes2, rownames(seurat_obj))
      
      if (length(missing_genes) > 0) {
        # 如果有缺失的基因，显示通知消息
        showNotification(paste("The following genes do not exist in the data:", paste(missing_genes, collapse = ", ")), type = "error")
        
        # 停止执行绘图
        return(NULL)
      }
      
      cat("输入的基因为：",genes,"\n")
      if(input$markers_plots == "Vlnplot") {
        
        vlnplot_marker <<- VlnPlot(seurat_obj,features =genes2, group.by = "seurat_clusters",pt.size = pt_size_input2)
        
        print(vlnplot_marker)
      }
    })
    
    output$Dimplot_aut <- renderPlot({
      
      if(input$markers_plots == "Dimplot_aut") {
        
        Dimplot_aut_plot<<-DimPlot(seurat_obj, reduction = "umap", group.by = "seurat_clusters", 
                                   pt.size = 1.2, alpha = 0.7,label = T) 
        
        print(Dimplot_aut_plot)
      }})
    
    output$DotPlot <- renderPlot({
      
      top_genes <- c()
      
      cluster_cols <- grep("^cluster", colnames(all_markers), value = TRUE)
      
      for (cluster_col in cluster_cols) {
        
        score_col <- gsub("cluster", "scores", cluster_col)
        
        if (score_col %in% colnames(all_markers)) {
          top_genes_cluster <- all_markers %>%
            arrange(desc(.data[[score_col]])) %>%  # 按 scores 降序排列
            slice_head(n = input$top_input_DotPlot) %>%  # 取前 5 个基因
            pull(.data[[cluster_col]])  # 提取基因名
          
          # 存入结果
          top_genes <- c(top_genes, top_genes_cluster)
        }
      }
      
      # 确保基因唯一
      top_genes <- unique(top_genes)
      
      print(top_genes[1:10])
      
      
      
      
      
      DotPlot_plot_marker <<- DotPlot(seurat_obj,assay = 'SCT',scale=TRUE,features =  unique(top_genes)) +
        # coord_flip() + #翻转
        theme(panel.grid = element_blank(), 
              axis.text.x=element_text(angle = 45, hjust = 0.5,vjust=0.5))+ #轴标签
        labs(x=NULL,y=NULL) + 
        guides(size = guide_legend("Percent Expression") )+ #legend
        scale_color_gradientn(colours = c('#330066','#336699','#66CC66','#FFCC33')) #颜色
      print(DotPlot_plot_marker)
      
      
    })
    
    # showNotification("Normalization & Linear Dimensional Reduction End of run！", type = "message")
    
    
    
    
    
  })
  
  #自动注释singleR ---------------------------------
  ref_data_reactive <- reactive({
    ref_data <- input$ref_data_choice
    if (ref_data == "BlueprintEncodeData") {
      BlueprintEncodeData()
    } else if (ref_data == "DatabaseImmuneCellExpressionData") {
      DatabaseImmuneCellExpressionData()
    } else if (ref_data == "HumanPrimaryCellAtlasData") {
      HumanPrimaryCellAtlasData()
    } else if (ref_data == "ImmGenData") {
      ImmGenData()
    } else if (ref_data == "MonacoImmuneData") {
      MonacoImmuneData()
    } else if (ref_data == "MouseRNAseqData") {
      MouseRNAseqData()
    } else if (ref_data == "NovershternHematopoieticData") {
      NovershternHematopoieticData()
    } else {
      NULL
    }
  })
  
  ####自建库注释----
  observeEvent(input$custom_ref, {
    req(input$custom_ref)  # 确保文件输入存在
    tryCatch({
      # 读取上传的 RDS 文件
      ref_SE <<- readRDS(input$custom_ref$datapath)
      output$message <- renderText("Data successfully loaded!")
    }, error = function(e) {
      output$message <- renderText("Error in loading data. Please try again.")
    })
  })
  
  observeEvent(input$submit_singleR,{
    id<-showNotification("Cell Annotation Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    #选择参考数据集
    ref_data <- ref_data_reactive()
    
    if (input$ref_data_choice == "CustomData" && !is.null(input$custom_ref)) {
      
      ref_data <- ref_SE
      
      seurat_obj_count=seurat_obj[["SCT"]]@counts
      
      #需要取不同数据集的基因交集
      common_gene <- intersect(rownames(seurat_obj_count), rownames(ref_data))
      ref_data <- ref_data[common_gene,]
      seurat_obj_count <- seurat_obj_count[common_gene,]
      
      #也要创建SummarizedExperiment对象，以及log normalize
      seurat_obj_SE <- SummarizedExperiment(assays=list(counts=seurat_obj_count))
      seurat_obj_SE <- logNormCounts(seurat_obj_SE)
      
      annotations <- SingleR(test = seurat_obj_SE, ref = ref_data, labels = ref_data$ref_label)
      
    } else {
      # 使用默认参考数据集
      req(ref_data)  # 确保选择了参考数据集
      st_counts <- GetAssayData(seurat_obj, layer = "counts")
      if (!is.null(ref_data)) {
        label_column <- input$annotation_type  # 选择使用的标签
        # 运行 SingleR
        annotations <- SingleR(test = st_counts, ref = ref_data, labels = ref_data[[label_column]])
        print(annotations)  # 这里可以检查结果
      }
    }
    
    seurat_obj$SingleR_labels <- annotations$labels  
    seurat_obj$SingleR_labels <- as.factor(seurat_obj$SingleR_labels)
    
    output$SpatialDimPlot_annot <- renderPlot({
      
      DimPlot_annot <- DimPlot(seurat_obj, group.by = "SingleR_labels", label = TRUE, repel = TRUE) +
        ggtitle("SingleR Automated Cell Type Annotation") +
        scale_color_npg() 
      SpatialDimPlot_annot  <- SpatialDimPlot(seurat_obj, group.by = "SingleR_labels", label = TRUE, repel = TRUE,label.box = T,label.size =3,label.color ="black") +
        ggtitle("Spatial Annotation of Cell Types")
      combined_plot_annot <<- SpatialDimPlot_annot | DimPlot_annot
      print(combined_plot_annot)
      
      
    })
    
  })
  
  
  
  
  ####手动注释--------------------
  observeEvent(input$submit_de_markers_manual,{
    id<-showNotification("Cell Annotation Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    cat("\033[31m","Subpopulation marker gene analysis! \033[0m\n");
    
    
    # 可视化marker
    output$de_markers_df_manual <- renderDT({
      
      # 检查全局变量 all_markers 是否存在且不为空
      if (exists("all_markers") && !is.null(all_markers) && nrow(all_markers) > 0) {
        return(datatable(all_markers, options = list(scrollX = TRUE)))
      }
      
      
      
      progress <- Progress$new()
      on.exit(progress$close())  # 确保即使发生错误，进度条也会关闭
      progress$set(value = 0.2, message = "Finding Markers...")
      
      all_markers <<- cosg(
        seurat_obj,
        groups='all', #考虑全部分组
        assay='SCT',
        slot='data',
        mu=1,         #惩罚项参数，值越大
        remove_lowly_expressed=TRUE,   #是否过滤低表达基因
        expressed_pct=0.1,             #设置低表达的阈值
        n_genes_user=100      #每个cluster定义Top-N个marker gene
      )
      progress$set(value = 0.3, message = "Arranging data...")
      
      all_markers <<- as.data.frame(all_markers)
      
      progress$set(value = 0.4, message = "Arranging data...")
      
      gene_cols <- grep("^names", colnames(all_markers), value = TRUE)
      score_cols <- grep("^scores", colnames(all_markers), value = TRUE)
      
      progress$set(value = 0.7, message = "Arranging data...")
      # 按照编号匹配，确保列是相邻的
      ordered_cols <- as.vector(rbind(gene_cols, score_cols))
      # 重新排列数据框列顺序
      all_markers <<- all_markers[, ordered_cols]
      colnames(all_markers) <<- gsub("^names", "cluster", colnames(all_markers))
      
      progress$set(value = 0.9, message = "Arranging data...")
      
      
      datatable(all_markers,options = list(scrollX = TRUE))
    })
    
    
    output$SpatialFeaturePlot_manual <- renderPlot({
      top_input1 <- input$top_input_manual
      cat("top_input的值为：",top_input1,"\n")
      
      if(input$markers_plots_manual == "SpatialFeaturePlot_manual") {
        
        progress <- Progress$new()
        on.exit(progress$close())  # 确保即使发生错误，进度条也会关闭
        progress$set(value = 0.4, message = "Finding Variable Features...")
        
        
        print("-------------1")
        seurat_obj <- FindSpatiallyVariableFeatures(seurat_obj, assay = "SCT", features = VariableFeatures(seurat_obj)[1:500],selection.method = "moransi",verbose = T)        
        
        progress$set(value = 0.8, message = "Finding Variable Features...")
        
        top.features <- head(SpatiallyVariableFeatures_workaround(seurat_obj, selection.method = "moransi"), top_input1)
        SpatialFeaturePlot_manual <<- SpatialFeaturePlot(seurat_obj, features = top.features, ncol = 4, alpha = c(0.1, 1),pt.size.factor = input$pt_size_input3)
        
        
        print(SpatialFeaturePlot_manual)
        
      }
    })
    
    output$Vlnplot_manual <- renderPlot({
      genes_input2 <- gsub('[""]', '', input$genes_input_manual)  # 去除输入中的引号
      pt_size_input2 <- input$pt_size_input_manual
      # 将输入的基因名转化为向量
      genes2 <- unlist(strsplit(genes_input2, split = ",\\s*")) 
      
      # 如果输入框为空，显示提示消息
      if (genes_input2 == "") {
        showNotification("Please enter a gene name！", type = "warning")
        return(NULL)  # 停止执行后续代码
      }
      
      
      # 检查输入的基因是否在seurat_obj中
      missing_genes <- setdiff(genes2, rownames(seurat_obj))
      
      if (length(missing_genes) > 0) {
        # 如果有缺失的基因，显示通知消息
        showNotification(paste("The following genes do not exist in the data:", paste(missing_genes, collapse = ", ")), type = "error")
        
        # 停止执行绘图
        return(NULL)
      }
      
      cat("输入的基因为：",genes,"\n")
      if(input$markers_plots_manual == "Vlnplot_manual") {
        
        Vlnplot_plot_manual <<- VlnPlot(seurat_obj,features =genes2, group.by = "seurat_clusters",pt.size = pt_size_input2)
        
        print(Vlnplot_plot_manual)
      }
    })
    
    output$Dimplot_manual <- renderPlot({
      
      if(input$markers_plots_manual == "Dimplot_manual") {
        if(input$Selectdata=="stxBrain"){
          Dimplot_manual_plot <<-  DimPlot(seurat_obj, reduction = "umap", group.by = "seurat_clusters", 
                                           pt.size = 1.2, alpha = 0.7,label = T) 
          
          
        }
        print(Dimplot_manual_plot)
      }
    })
    
    
    
    output$DotPlot_manual <- renderPlot({
      
      top_genes <- c()
      
      cluster_cols <- grep("^cluster", colnames(all_markers), value = TRUE)
      
      for (cluster_col in cluster_cols) {
        
        score_col <- gsub("cluster", "scores", cluster_col)
        
        if (score_col %in% colnames(all_markers)) {
          top_genes_cluster <- all_markers %>%
            arrange(desc(.data[[score_col]])) %>%  # 按 scores 降序排列
            slice_head(n = input$top_input_DotPlot_manual) %>%  # 取前 5 个基因
            pull(.data[[cluster_col]])  # 提取基因名
          
          # 存入结果
          top_genes <- c(top_genes, top_genes_cluster)
        }
      }
      
      # 确保基因唯一
      top_genes <- unique(top_genes)
      
      print(top_genes[1:10])
      
      
      DotPlot_plot_manual <<- DotPlot(seurat_obj, features = unique(top_genes) ,assay='SCT' ,group.by = "seurat_clusters") + 
        # coord_flip() + #翻转
        theme(panel.grid = element_blank(), 
              axis.text.x=element_text(angle = 45, hjust = 0.5,vjust=0.5))+ #轴标签
        labs(x=NULL,y=NULL) + 
        guides(size = guide_legend("Percent Expression") )+ #legend
        scale_color_gradientn(colours = c('#330066','#336699','#66CC66','#FFCC33')) #颜色
      
      
      print(DotPlot_plot_manual)
      
    })
    
    
    #updateActionButton(session, "submit_NLDR", disabled = FALSE)  
    # showNotification("Normalization & Linear Dimensional Reduction End of run！", type = "message")
  })
  
  ####手动输入--------
  
  output$manual_input <- renderUI({
    
    
    fluidRow(
      column(6, 
             selectizeInput("clusters", "Select the cell population:",
                            choices = sort(unique(Idents(seurat_obj))),
                            multiple = TRUE)
      ),
      column(6, 
             textInput("new_label", "Enter a new cell type:", value = "")
      )
    )
  })
  
  # 监听“更新细胞注释”按钮点击事件
  observeEvent(input$update, {
    req(input$clusters, input$new_label)  # 确保输入框不为空
    
    # 获取当前 Seurat 对象的 Idents
    current_idents <- Idents(seurat_obj)
    levels(current_idents) <- unique(c(levels(current_idents), input$new_label))
    # 遍历用户选择的 cluster 并更新为新标签
    for (clust in input$clusters) {
      current_idents[current_idents == clust] <- input$new_label
    }
    cat("用户输入的细胞类型为",clust,"\n")
    
    Idents(seurat_obj) <- current_idents
    
    
    seurat_obj <<- seurat_obj
    
    
    showNotification("Cell annotation updated successfully！", type = "message", duration = 3)
  })
  
  observeEvent(input$submit_manual, {
    req(seurat_obj)  
    
    
    output$manual_plot <- renderPlot({
      
      manual_dimplot <<-  DimPlot(seurat_obj, reduction = "umap", label = TRUE, pt.size = 1, group.by = "ident") + 
        ggtitle("Updated Cell Annotations")
      print(manual_dimplot)
    })
  })
  
  ####与单细胞集成-------
  
  
  output$image_select_ui <- renderUI({
    
    available_images <- names(seurat_obj@images)
    
    
    selectInput("image", "Select Image:", choices = available_images)
  })
  
  output$ident_select_ui <- renderUI({
    
    selectInput("sc_spatialdimplot_selectInput","Select Clusters:",multiple = T, choices = levels(seurat_obj),selected = c(1, 2, 3, 4, 6, 7))
  })
  
  
  observeEvent(input$submit_sc,{
    
    shinyjs::enable("submit_sc_next")
    
    output$sc_spatialdimplot <- renderPlot({
      
      
      image_data <- seurat_obj@images[[input$image]]$centroids
      brain_coordinates <- data.frame(
        cells = image_data@cells,
        spatial_coord_x = image_data@coords[, 1],
        spatial_coord_y = image_data@coords[, 2]
      )
      
      seurat_obj@meta.data$cells <- rownames(seurat_obj@meta.data)
      
      print(str(seurat_obj@meta.data))
      
      seurat_obj@meta.data <- merge(seurat_obj@meta.data, brain_coordinates, by = "cells")
      
      rownames(seurat_obj@meta.data) <- seurat_obj@meta.data$cells
      
      print(colnames(seurat_obj@meta.data))
      
      cortex <<- subset(seurat_obj, idents = input$sc_spatialdimplot_selectInput)
      
      
      cortex <<- subset(cortex, cells = cells$cells)
      
      
      plot_seurat_obj <- SpatialDimPlot(seurat_obj, label = TRUE, label.size = 3, pt.size.factor = 2)
      
      plot_sc <- SpatialDimPlot(cortex, crop = FALSE, label = TRUE, pt.size.factor = 2, label.size = 3)
      
      sc_image <<- plot_seurat_obj+plot_sc
      
      sc_image
      
    })
    
  })
  
  
  observeEvent(input$submit_sc_next, {
    closeSweetAlert(session = session)
    updateTabItems(session, "inTabset", selected = "sc_next")
    
    shinyjs::enable(selector = ".sidebar li a[data-value='sc_next']")
    js.7 <- '
            $(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=sc_next]").css("color", "#ded7e1");
            });
          '
    shinyjs::runjs(js.7)
    
  })
  
  
  observe({
    if(input$Select_ref_data == "Custom Reference"){
      
      showNotification("Select RDS file", type = "message", duration = 5)
      
      output$RefInputs <- renderUI({
        fileInput("upload_ref_rds", "Select RDS file",accept = c(".rds"))
        
        
        helpText(
          div(
            style = "
            letter-spacing: normal; 
            border: 2px solid #FFA500; 
            padding: 15px; 
            border-radius: 8px; 
            box-shadow: 4px 4px 12px rgba(0, 0, 0, 0.2);
            background-color: #f9f9f9; 
            width: 100%; 
            margin-top: 10px; /* 与sidebarPanel之间增加间隔 */
            margin-bottom: 10px; /* 增加底部间隔 */
            box-sizing: border-box;
            line-height: 1.5; 
            color: #210d2e;
            font-size: 1.5rem; /* 可以根据需要调整字体大小 */
          ",
            # style = "letter-spacing: normal; border: 2px solid #FFA500; 
            #  padding: 10px; border-radius: 8px; box-shadow: 4px 4px 12px rgba(0, 0, 0, 0.2);
            #  background-color: #f9f9f9; width: 300px; line-height: 1.5; color: #210d2e;",
            strong(code("Tips:")),
            br(),
            tags$div(style = "text-indent: 20px;", 
                     "Please load data in rds format, the data type is seurat object, 
                     and the metadata must contain a subclass column storing the cell type, 
                     and the data must have been normalized."),
            
            tags$div(style = "text-indent: 20px;", 
                     "The sample reference data is a reference scRNA-seq dataset of about 
              14,000 adult mouse cortical cells from the Allen Institute, which was 
              generated using the SMART-Seq 2 protocol.")
          )
        )
      })
    }else{ 
      showNotification("Allen Reference", type = "message", duration = 2)
      
      output$RefInputs <- renderUI({
        
        
        helpText(
          div(
            style = "
            letter-spacing: normal; 
            border: 2px solid #FFA500; 
            padding: 15px; 
            border-radius: 8px; 
            box-shadow: 4px 4px 12px rgba(0, 0, 0, 0.2);
            background-color: #f9f9f9; 
            width: 100%; 
            margin-top: 10px; /* 与sidebarPanel之间增加间隔 */
            margin-bottom: 10px; /* 增加底部间隔 */
            box-sizing: border-box;
            line-height: 1.5; 
            color: #210d2e;
            font-size: 1.5rem; /* 可以根据需要调整字体大小 */
          ",
            # style = "letter-spacing: normal; border: 2px solid #FFA500; 
            #    padding: 10px; border-radius: 8px; box-shadow: 4px 4px 12px rgba(0, 0, 0, 0.2);
            #    background-color: #f9f9f9; width: 300px; line-height: 1.5; color: #210d2e;",
            strong(code("Tips:")),
            br(),
            tags$div(style = "text-indent: 20px;", 
                     "Please load data in rds format, the data type is seurat object, 
                   and the metadata must contain a subclass column storing the cell type, 
                   and the data must have been normalized."),
            
            tags$div(style = "text-indent: 20px;", 
                     "The sample reference data is a reference scRNA-seq dataset of about 
              14,000 adult mouse cortical cells from the Allen Institute, which was 
              generated using the SMART-Seq 2 protocol.")
          )
        )
      })
    }
    
  })
  
  
  observeEvent(input$submit_sc_next_start, {
    
    id<-showNotification("Load Data Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    # 尝试读取 RDS 文件
    
    print("------111")
    # tryCatch({
    
    progressSweetAlert(
      session = session,
      id = "simulationProgress3",
      title = div("Load Data Runing...,",br(),
                  "It will take some time, please be patient and wait..."),
      display_pct = TRUE,
      value = 20
    )
    
    if(input$Select_ref_data == "Custom Reference"){
      refdata <<- readRDS(input$upload_ref_rds$datapath)
    }else{
      refdata<<- readRDS("allen_reference.rds")
    }
    
    
    print("-----2222")
    updateProgressBar(session = session,id = "simulationProgress3",title = " Runing PCA...",value = 40)
    
    refdata <<- RunPCA(refdata,verbose = FALSE) 
    
    updateProgressBar(
      session = session,
      id = "simulationProgress3",
      title = " Runing UMAP...",
      value = 60
    )
    
    refdata <<- RunUMAP(refdata,Reduction="pca",dims = 1:30)
    
    updateProgressBar(session = session,id = "simulationProgress3",title = " Normalization using SCTransform...",value = 80)
    # cortex <<- cortex_data()
    cortex <<- SCTransform(cortex, assay = "Spatial", verbose = FALSE) %>%
      RunPCA(verbose = FALSE)
    
    updateProgressBar(session = session,id = "simulationProgress3",title = "Normalization using SCTransform...",value = 100)
    
    closeSweetAlert(session = session)
    
    sendSweetAlert(  
      title = "Done",  
      text = "Data processing completed",  
      type = "success",
    )
    
    
    
    showNotification("Data processing End of run！", type = "message",duration = 2)
    
    
    
    
    
    output$sc_dimplot <- renderPlot({
      
      sc_dimplot<<- DimPlot(refdata, group.by = "subclass", label = TRUE)
      print(sc_dimplot)
      
    })
    
    shinyjs::enable("submit_sc_continue")
  })
  
  
  
  observeEvent(input$submit_sc_continue, {
    
    tryCatch({
      # 计算锚点
      anchors <- FindTransferAnchors(reference = refdata,query = cortex,normalization.method = "SCT")
      
      # 预测数据传输
      predictions.assay <- TransferData(anchorset = anchors, 
                                        refdata = refdata$subclass, 
                                        prediction.assay = TRUE,
                                        weight.reduction = cortex[["pca"]], 
                                        dims = 1:30
      )
      
      # 将预测数据存入 Seurat 对象
      cortex[["predictions"]] <<- predictions.assay
      
      # 设置默认 Assay
      DefaultAssay(cortex) <<- "predictions"
      
      # 输出成功消息
      message("ransferData completed successfully!")
      
    }, error = function(e) {
      # 捕获错误并输出错误信息
      message("Error occurred:", e$message)
    })
    
    
    output$sc_spatialfeatureplot <- renderPlot({
      # 确保 Seurat 对象 cortex 存在
      req(exists("cortex"))
      
      # 解析用户输入的特征
      feature_list <- unlist(strsplit(input$sc_features, ","))  # 解析输入
      feature_list <- trimws(feature_list)  # 去除空格
      
      # print("feature_list:",feature_list)
      
      # 生成 SpatialFeaturePlot
      sc_spatialfeatureplot <<- SpatialFeaturePlot(
        cortex, 
        features = feature_list, 
        pt.size.factor = input$sc_pt_size_factor, 
        ncol = input$sc_ncol, 
        crop = FALSE
      )
      
      print(sc_spatialfeatureplot)
    })
    
    
    # })
    
    shinyjs::enable("run_moransi")
    
  })
  
  observeEvent(input$run_moransi, {
    # 执行分析
    cortex <<- FindSpatiallyVariableFeatures(
      cortex,
      assay = "predictions",
      selection.method = "moransi",
      features = rownames(cortex),
      r.metric = input$r_metric,
      slot = "data"
    )
    
    # 提取 top cluster 名字
    meta_feats <- cortex[["predictions"]]@meta.features
    top_clusters <- rownames(
      dplyr::slice_min(meta_feats, order_by = meta_feats$moransi.spatially.variable.rank, n = input$top_n)
    )
    
    # 保存用于后续输出
    # output$top_clusters_table <- DT::renderDataTable({
    #   data.frame(TopCluster = top_clusters)
    # })
    
    output$moransi_plot <- renderPlot({
      moransi_plot <<-  SpatialPlot(object = cortex, features = top_clusters, ncol = input$annot_ncol, pt.size.factor = input$pt_size_factor)
      print(moransi_plot)
    })
    
    
    
    
  })
  
  kegg_go_species <- reactive({
    if (input$species == "mmu") {
      list(OrgDb = org.Mm.eg.db, organism = "mmu")
    } else {
      list(OrgDb = org.Hs.eg.db, organism = "hsa")
    }
  })
  
  
  ###kegg-----
  
  
  # 动态生成 kegg_clusters 的选择框
  output$kegg_clusters_ui <- renderUI({
    req(sig_gene_symbols)  # 确保 sig_gene_symbols 是有效的
    selectInput("kegg_clusters",  "Select Clusters:", choices = colnames(sig_gene_symbols), selected = c("cluster.0"), multiple = TRUE)
  })
  
  
  observeEvent(input$submit_kegg, {
    
    withProgress(message = 'Running KEGG and GO Enrichment...', value = 0, {
      
      # 设置进度条的初始值
      incProgress(0.1, detail = "Preparing data...")
      
      # 获取物种信息
      species_info <- kegg_go_species()
      
      # 获取用户选择的列
      selected_columns <- input$kegg_clusters
      
      incProgress(0.3, detail = "Preparing data...")
      
      # 根据选择的列筛选数据
      sig_gene_symbols <- all_markers[, selected_columns, drop = FALSE]
      
      # 转换为向量
      sig_gene_symbols <- as.vector(as.matrix(sig_gene_symbols))
      
      # 进行基因符号到EntrezID的转换
      sig_entrez <- bitr(sig_gene_symbols, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = species_info$OrgDb)
      
      # 更新进度条
      incProgress(0.5, detail = "Performing GO enrichment analysis...")
      
      # GO富集分析
      GO <- enrichGO(
        gene = sig_entrez$ENTREZID,
        OrgDb = species_info$OrgDb, 
        ont = "ALL",  # Biological Process, 可改为 "MF" 或 "CC"
        pAdjustMethod = "BH",  # Benjamini-Hochberg 校正
        qvalueCutoff = 0.05
      )
      
      
      
      # 更新进度条
      incProgress(0.7, detail = "Performing KEGG enrichment analysis...")
      
      # KEGG富集分析
      kegg_enrich <- enrichKEGG(
        gene = sig_entrez$ENTREZID,
        organism = species_info$organism,  # Mouse or Human
        pAdjustMethod = "BH",
        qvalueCutoff = 0.05
      )
      incProgress(0.9, detail = "Performing KEGG enrichment analysis...")
      
      # 渲染GO条形图
      output$go_plot <- renderPlot({
        go_plot <<-  barplot(GO, showCategory = 10)
        print(go_plot)
      })
      
      # 渲染KEGG气泡图
      output$kegg_plot <- renderPlot({
        kegg_plot <<- dotplot(kegg_enrich, showCategory = 10)
        print(kegg_plot)
      })
      
      # 最后更新进度条
      incProgress(1, detail = "Analysis complete.")
    })
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  #下载------------------------------------------------------------------ 
  
  
  output$Filter_plots_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")  
      paste0(formatted_time, "-", "Vlnplot",input$Filter_plots_PlotChoices)
    },
    content=function(file) {
      if(input$Filter_plots_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$Filter_plots_PlotWidth, height = input$Filter_plots_PlotHeight)}
      else if(input$Filter_plots_PlotChoices==".png")
      {png(file = file, width = input$Filter_plots_PlotWidth, height = input$Filter_plots_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$Filter_plots_PlotWidth, height = input$Filter_plots_PlotHeight,units="in",res=1000)}
      print(vlnplot1)
      dev.off()
    }
  )
  output$Filter_plots_download_plot2 <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")  
      paste0(formatted_time, "-", "Vlnplot",input$Filter_plots_PlotChoices2)
    },
    content=function(file) {
      if(input$Filter_plots_PlotChoices2==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$Filter_plots_PlotWidth2, height = input$Filter_plots_PlotHeight2)}
      else if(input$Filter_plots_PlotChoices2==".png")
      {png(file = file, width = input$Filter_plots_PlotWidth2, height = input$Filter_plots_PlotHeight2,units="in",res=1000)}
      else
      {tiff(file = file, width = input$Filter_plots_PlotWidth2, height = input$Filter_plots_PlotHeight2,units="in",res=1000)}
      print(vlnplot_aft)
      dev.off()
    }
  )
  
  
  output$brain_Vlnplot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")  
      paste0(formatted_time, "-", "Vlnplot",input$brain_Vlnplot_PlotChoices)
    },
    content=function(file) {
      if(input$brain_Vlnplot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$brain_Vlnplot_PlotWidth, height = input$brain_Vlnplot_PlotHeight)}
      else if(input$brain_Vlnplot_PlotChoices==".png")
      {png(file = file, width = input$brain_Vlnplot_PlotWidth, height = input$brain_Vlnplot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$brain_Vlnplot_PlotWidth, height = input$brain_Vlnplot_PlotHeight,units="in",res=1000)}
      print(bef_filter_Vlnplot)
      dev.off()
    }
  )
  
  output$SpatialFeaturePlot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S") 
      paste0(formatted_time,"-", "FeaturePlot",input$SpatialFeaturePlot_PlotChoices)
    },
    content=function(file) {
      if(input$SpatialFeaturePlot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$SpatialFeaturePlot_PlotWidth, height = input$SpatialFeaturePlot_PlotHeight)}
      else if(input$SpatialFeaturePlot_PlotChoices==".png")
      {png(file = file, width = input$SpatialFeaturePlot_PlotWidth, height = input$SpatialFeaturePlot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$SpatialFeaturePlot_PlotWidth, height = input$SpatialFeaturePlot_PlotHeight,units="in",res=1000)}
      print(Spatial_Feature_Plot)
      dev.off()
    })
  
  output$umap_Dimplot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "umap_Dimplot",input$umap_Dimplot_PlotChoices)
    },
    content=function(file) {
      if(input$umap_Dimplot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$umap_Dimplot_PlotWidth, height = input$umap_Dimplot_PlotHeight)}
      else if(input$umap_Dimplot_PlotChoices==".png")
      {png(file = file, width = input$umap_Dimplot_PlotWidth, height = input$umap_Dimplot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$umap_Dimplot_PlotWidth, height = input$umap_Dimplot_PlotHeight,units="in",res=1000)}
      print(umap_dim)
      dev.off()
    })
  
  output$SpatialDimPlot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "SpatialDimPlot",input$SpatialDimPlot_PlotChoices)
    },
    content=function(file) {
      if(input$SpatialDimPlot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$SpatialDimPlot_PlotWidth, height = input$SpatialDimPlot_PlotHeight)}
      else if(input$SpatialDimPlot_PlotChoices==".png")
      {png(file = file, width = input$SpatialDimPlot_PlotWidth, height = input$SpatialDimPlot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$SpatialDimPlot_PlotWidth, height = input$SpatialDimPlot_PlotHeight,units="in",res=1000)}
      print(SpatialDim_Plot)
      dev.off()
    })
  
  output$vlnplot_marker_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Violin plot",input$vlnplot_marker_PlotChoices)
    },
    content=function(file) {
      if(input$vlnplot_marker_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$vlnplot_marker_PlotWidth, height = input$vlnplot_marker_PlotHeight)}
      else if(input$vlnplot_marker_PlotChoices==".png")
      {png(file = file, width = input$vlnplot_marker_PlotWidth, height = input$vlnplot_marker_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$vlnplot_marker_PlotWidth, height = input$vlnplot_marker_PlotHeight,units="in",res=1000)}
      print(vlnplot_marker)
      dev.off()
    })
  
  output$Dimplot_aut_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "clusters",input$Dimplot_aut_PlotChoices)
    },
    content=function(file) {
      if(input$Dimplot_aut_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$Dimplot_aut_PlotWidth, height = input$Dimplot_aut_PlotHeight)}
      else if(input$Dimplot_aut_PlotChoices==".png")
      {png(file = file, width = input$Dimplot_aut_PlotWidth, height = input$Dimplot_aut_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$Dimplot_aut_PlotWidth, height = input$Dimplot_aut_PlotHeight,units="in",res=1000)}
      print(Dimplot_aut_plot)
      dev.off()
    })
  
  output$DotPlot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "DotPlot",input$DotPlot_PlotChoices)
    },
    content=function(file) {
      if(input$DotPlot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$DotPlot_PlotWidth, height = input$DotPlot_PlotHeight)}
      else if(input$DotPlot_PlotChoices==".png")
      {png(file = file, width = input$DotPlot_PlotWidth, height = input$DotPlot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$DotPlot_PlotWidth, height = input$DotPlot_PlotHeight,units="in",res=1000)}
      
      print(DotPlot_plot_marker)
      dev.off()
    })
  
  output$SpatialFeaturePlot_marker_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "SpatialFeaturePlot",input$SpatialFeaturePlot_marker_PlotChoices)
    },
    content=function(file) {
      if(input$SpatialFeaturePlot_marker_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$SpatialFeaturePlot_marker_PlotWidth, height = input$SpatialFeaturePlot_marker_PlotHeight)}
      else if(input$SpatialFeaturePlot_marker_PlotChoices==".png")
      {png(file = file, width = input$SpatialFeaturePlot_marker_PlotWidth, height = input$SpatialFeaturePlot_marker_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$SpatialFeaturePlot_marker_PlotWidth, height = input$SpatialFeaturePlot_marker_PlotHeight,units="in",res=1000)}
      print(SpatialFeaturePlot_marker)
      dev.off()
    })
  
  output$SpatialDimPlot_annot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "SpatialDimPlot",input$SpatialDimPlot_annot_PlotChoices)
    },
    content=function(file) {
      if(input$SpatialDimPlot_annot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$SpatialDimPlot_annot_PlotWidth, height = input$SpatialDimPlot_annot_PlotHeight)}
      else if(input$SpatialDimPlot_annot_PlotChoices==".png")
      {png(file = file, width = input$SpatialDimPlot_annot_PlotWidth, height = input$SpatialDimPlot_annot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$SpatialDimPlot_annot_PlotWidth, height = input$SpatialDimPlot_annot_PlotHeight,units="in",res=1000)}
      print(combined_plot_annot)
      dev.off()
    })
  
  output$Vlnplot_manual_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Violin plot",input$Vlnplot_manual_PlotChoices)
    },
    content=function(file) {
      if(input$Vlnplot_manual_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$Vlnplot_manual_PlotWidth, height = input$Vlnplot_manual_PlotHeight)}
      else if(input$Vlnplot_manual_PlotChoices==".png")
      {png(file = file, width = input$Vlnplot_manual_PlotWidth, height = input$Vlnplot_manual_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$Vlnplot_manual_PlotWidth, height = input$Vlnplot_manual_PlotHeight,units="in",res=1000)}
      print(Vlnplot_plot_manual)
      dev.off()
    })
  
  output$Dimplot_manual_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Dimplot",input$Dimplot_manual_PlotChoices)
    },
    content=function(file) {
      if(input$Dimplot_manual_PlotChoices==".pdf")
      {pdf(file = file, width = input$Dimplot_manual_PlotWidth, height = input$Dimplot_manual_PlotHeight)}
      else if(input$Dimplot_manual_PlotChoices==".png")
      {png(file = file, width = input$Dimplot_manual_PlotWidth, height = input$Dimplot_manual_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$Dimplot_manual_PlotWidth, height = input$Dimplot_manual_PlotHeight,units="in",res=1000)}
      print(Dimplot_manual_plot)
      dev.off()
    })
  
  
  output$DotPlot_manual_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "DotPlot_manual",input$DotPlot_manual_PlotChoices)
    },
    content=function(file) {
      if(input$DotPlot_manual_PlotChoices==".pdf")
      {pdf(file = file, width = input$DotPlot_manual_PlotWidth, height = input$DotPlot_manual_PlotHeight)}
      else if(input$DotPlot_manual_PlotChoices==".png")
      {png(file = file, width = input$DotPlot_manual_PlotWidth, height = input$DotPlot_manual_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$DotPlot_manual_PlotWidth, height = input$DotPlot_manual_PlotHeight,units="in",res=1000)}
      print(DotPlot_plot_manual)
      dev.off()
    })
  
  output$SpatialFeaturePlot_manual_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "SpatialFeaturePlot_manual",input$SpatialFeaturePlot_manual_PlotChoices)
    },
    content=function(file) {
      if(input$SpatialFeaturePlot_manual_PlotChoices==".pdf")
      {pdf(file = file, width = input$SpatialFeaturePlot_manual_PlotWidth, height = input$SpatialFeaturePlot_manual_PlotHeight)}
      else if(input$SpatialFeaturePlot_manual_PlotChoices==".png")
      {png(file = file, width = input$SpatialFeaturePlot_manual_PlotWidth, height = input$SpatialFeaturePlot_manual_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$SpatialFeaturePlot_manual_PlotWidth, height = input$SpatialFeaturePlot_manual_PlotHeight,units="in",res=1000)}
      print(SpatialFeaturePlot_manual)
      dev.off()
    })
  
  output$manual_plot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Annotation",input$manual_plot_PlotChoices)
    },
    content=function(file) {
      if(input$manual_plot_PlotChoices==".pdf")
      {pdf(file = file, width = input$manual_plot_PlotWidth, height = input$manual_plot_PlotHeights)}
      else if(input$manual_plot_PlotChoices==".png")
      {png(file = file, width = input$manual_plot_PlotWidth, height = input$manual_plot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$manual_plot_PlotWidth, height = input$manual_plot_PlotHeight,units="in",res=1000)}
      print(manual_dimplot)
      dev.off()
    })
  
  
  output$sc_spatialdimplot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Integration_single-cell",input$sc_spatialdimplot_PlotChoices)
    },
    content=function(file) {
      if(input$sc_spatialdimplot_PlotChoices==".pdf")
      {pdf(file = file, width = input$sc_spatialdimplot_PlotWidth, height = input$sc_spatialdimplot_PlotHeight)}
      else if(input$sc_spatialdimplot_PlotChoices==".png")
      {png(file = file, width = input$sc_spatialdimplot_PlotWidth, height = input$sc_spatialdimplot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$sc_spatialdimplot_PlotWidth, height = input$sc_spatialdimplot_PlotHeight,units="in",res=1000)}
      print(sc_image)
      dev.off()
    })
  
  ###222----
  output$sc_dimplot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "sc_dimplot",input$sc_dimplot_PlotChoices)
    },
    content=function(file) {
      if(input$sc_dimplot_PlotChoices==".pdf")
      {pdf(file = file, width = input$sc_dimplot_PlotWidth, height = input$sc_dimplot_PlotHeight)}
      else if(input$sc_dimplot_PlotChoices==".png")
      {png(file = file, width = input$sc_dimplot_PlotWidth, height = input$sc_dimplot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$sc_dimplot_PlotWidth, height = input$sc_dimplot_PlotHeight,units="in",res=1000)}
      print(sc_dimplot)
      dev.off()
    })
  
  
  
  output$sc_spatialfeatureplot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Visualization of cell types",input$sc_spatialfeatureplot_PlotChoices)
    },
    content=function(file) {
      if(input$sc_spatialfeatureplot_PlotChoices==".pdf")
      {pdf(file = file, width = input$sc_spatialfeatureplot_PlotWidth, height = input$sc_spatialfeatureplot_PlotHeight)}
      else if(input$sc_spatialfeatureplot_PlotChoices==".png")
      {png(file = file, width = input$sc_spatialfeatureplot_PlotWidth, height = input$sc_spatialfeatureplot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$sc_spatialfeatureplot_PlotWidth, height = input$sc_spatialfeatureplot_PlotHeight,units="in",res=1000)}
      print(sc_spatialfeatureplot)
      dev.off()
    })
  
  
  
  
  
  output$moransi_plot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Visualization of cell types",input$moransi_plot_PlotChoices)
    },
    content=function(file) {
      if(input$moransi_plot_PlotChoices==".pdf")
      {pdf(file = file, width = input$moransi_plot_PlotWidth, height = input$moransi_plot_PlotHeight)}
      else if(input$moransi_plot_PlotChoices==".png")
      {png(file = file, width = input$moransi_plot_PlotWidth, height = input$moransi_plot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$moransi_plot_PlotWidth, height = input$moransi_plot_PlotHeight,units="in",res=1000)}
      print(moransi_plot)
      dev.off()
    })
  
  ###111----
  output$go_plot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "GO enrichment analysis",input$go_plot_PlotChoices)
    },
    content=function(file) {
      if(input$go_plot_PlotChoices==".pdf")
      {pdf(file = file, width = input$go_plot_PlotWidth, height = input$go_plot_PlotHeight)}
      else if(input$go_plot_PlotChoices==".png")
      {png(file = file, width = input$go_plot_PlotWidth, height = input$go_plot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$go_plot_PlotWidth, height = input$go_plot_PlotHeight,units="in",res=1000)}
      print(go_plot)
      dev.off()
    })
  
  output$kegg_plot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "KEGG enrichment analysis",input$kegg_plot_PlotChoices)
    },
    content=function(file) {
      if(input$kegg_plot_PlotChoices==".pdf")
      {pdf(file = file, width = input$kegg_plot_PlotWidth, height = input$kegg_plot_PlotHeight)}
      else if(input$kegg_plot_PlotChoices==".png")
      {png(file = file, width = input$kegg_plot_PlotWidth, height = input$kegg_plot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$kegg_plot_PlotWidth, height = input$kegg_plot_PlotHeight,units="in",res=1000)}
      print(kegg_plot)
      dev.off()
    })
  
  #### 下载表格-----
  ###11-----
  
  output$GeneExpression_table_download <- downloadHandler(
    filename = function() {
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "GeneExpression_table", ".", input$GeneExpression_table_format)
    },
    content = function(file) {
      req(hvgs)  # 确保 hvgs 已生成
      # req(input$GeneExpression_table_download != "")
      
      if (input$GeneExpression_table_format == "csv") {
        write.csv(hvgs, file,row.names = T,quote = F)
      } else {
        write.table(hvgs,file,sep = "\t",col.names = T,row.names = T,quote = F
        )}})
  
  
  ###222----
  output$markers_table_download <- downloadHandler(
    filename = function() {
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "markers_table", ".", input$markers_table_format)
    },
    content = function(file) {
      req(all_markers)  # 确保 hvgs 已生成
      # req(input$GeneExpression_table_download != "")
      
      if (input$markers_table_format == "csv") {
        write.csv(all_markers, file,row.names = T,quote = F)
      } else {
        write.table(all_markers,file,sep = "\t",col.names = T,row.names = T,quote = F
        )}})
  # output$annotation_download_data <- downloadHandler(
  #   filename = function() {
  #     formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
  #     paste0(formatted_time, "-", "annotation_all.csv")
  #   },
  #   content = function(file) {
  #     write.csv(cells, file)
  #   })
  # output$Motif_download_data1 <- downloadHandler(
  #   filename = function() {
  #     formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
  #     paste0(formatted_time, "-", "Allmarkers_Findmotif.csv")
  #   },
  #   content = function(file) {
  #     write.csv(enriched.motifs1, file)
  #   })
  # 
  # output$ClusterMarkers_download_data <- downloadHandler(
  #   filename = function() {
  #     formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
  #     paste0(formatted_time, "-", "Deg_Findallmarkers_",input$Ident13,".csv")
  #   },
  #   content = function(file) {
  #     write.csv(ClusterMarkers_result1, file)
  #   })
  # output$CompareMarkers_download_data <- downloadHandler(
  #   filename = function() {
  #     formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
  #     paste0(formatted_time, "-", "Deg_Findmarkers_",input$Ident11,"_vs_",input$Ident12,"_all.csv")
  #   },
  #   content = function(file) {
  #     write.csv(FindMarkers_result1, file)
  #   })
  # output$Motif_download_data2 <- downloadHandler(
  #   filename = function() {  
  #     formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
  #     paste0(formatted_time, "-", "Top",input$AllMarkersTopPeaks3,"markers_findmotif_",input$Ident41,"_vs_",input$Ident42,".csv")
  #   },
  #   content = function(file) {
  #     write.csv(enriched.motifs2, file)
  #   })
  
  #清除---------------------------------------------------------------------
  clean<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      
      brain <<- NULL
      Selectdata <<- NULL
      obj <<- NULL
      obj_DimClu <<- NULL
      brain_DimClu <<- NULL
      
      genes_input <<- NULL
      genes <<- NULL
      obj_nomal <<- NULL
      brain_nomal <<- NULL
      all_markers <<- NULL
      gene_names <<- NULL
      mt_genes <<- NULL
      seurat_obj <<- NULL
      cells <<- NULL
      cortex<<- NULL
      refdata <<- NULL
      ####plot---------
      vlnplot1 <<- NULL
      vlnplot_aft <<- NULL
      bef_filter_Vlnplot <<- NULL
      Spatial_Feature_Plot <<- NULL
      umap_dim <<- NULL
      SpatialDim_Plot <<- NULL
      SpatialFeaturePlot_marker <<- NULL
      vlnplot_marker <<- NULL
      Dimplot_aut_plot <<- NULL
      DotPlot_plot_marker <<- NULL
      combined_plot_annot <<- NULL
      SpatialFeaturePlot_manual <<- NULL
      Vlnplot_plot_manual <<- NULL
      Dimplot_manual_plot <<- NULL
      DotPlot_plot_manual <<- NULL
      sc_image <<- NULL
      sc_spatialfeatureplot<<- NULL
      sc_dimplot<<- NULL
      sig_gene_symbols <<- NULL
      #----
      show_next <<- reactiveVal(FALSE)
      ref_data <<- reactiveVal(NULL)
      
      shinyjs::disable(selector = ".sidebar li a[data-value='Visual_data']")
      shinyjs::disable(selector = ".sidebar li a[data-value='Quality_control']")
      shinyjs::disable(selector = ".sidebar li a[data-value='one']")
      shinyjs::disable(selector = ".sidebar li a[data-value='ones']")
      shinyjs::disable(selector = ".sidebar li a[data-value='aut_annot']")
      shinyjs::disable(selector = ".sidebar li a[data-value='manual_annot']")
      shinyjs::disable(selector = ".sidebar li a[data-value='two']")
      
      js.0 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=Quality_control]").css("color", "#808080"); });'
      shinyjs::runjs(js.0)
      
      js.1.1 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=Visual_data]").css("color", "#808080"); });'
      shinyjs::runjs(js.1.1)
      
      js.1 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=one]").css("color", "#808080"); });'
      shinyjs::runjs(js.1)
      
      js.2 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=ones]").css("color", "#808080");}); '
      shinyjs::runjs(js.2)
      
      js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=two]").css("color", "#808080");});'
      shinyjs::runjs(js.3.1)
      
      js.4 <- ' $(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=aut_annot]").css("color", "#808080");});'
      shinyjs::runjs(js.4)
      
      js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=manual_annot]").css("color", "#808080");});'
      shinyjs::runjs(js.41)
    }
  }
  
  clean_filter<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      brains <<- NULL
      Ident11<<-NULL
      Ident12<<-NULL
      Ident41<<-NULL
      Ident13<<-NULL
      Ident42<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      cell_counts_after<<-NULL
      cell_counts<<-NULL
      # gene.activities<<-NULL
      unique_clusters<<-NULL
      
      gene_names_overlapping<<-NULL
      # Dynamic variables(动态变量)
      assaysList <<- NULL
      identsList <<- NULL
      # plots
      qcVlnPlots<<-NULL
      dDimplot<<-NULL
      dbFeaturePlot <<- NULL
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      FindAllMarkers_result <<- NULL
      FindMarkers_result1 <<- NULL
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      ClusterMarkers_result1<<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      #output---------------------------------------------
      output$QCVlnPlots<<- NULL
      output$text.cellsremain2<<- NULL
      output$DepthCorPlot<<- NULL
      output$ClusterDimPlot<<- NULL
      # output$Chr_num
      # output$Feature3
      output$FindAllMarkers_data<<- NULL
      output$FeaturePlot<<- NULL
      output$ClusterMarkers_data<<- NULL
      output$CompareMarkers_data<<- NULL
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      # reactive--------------------------------------------------------
      # values$deDepthCorPlot<<-NULL
      # values$dpDimPlot<<-NULL
      # values$submit_Clustering<<-NULL
      # #values<-reactiveValues(submit_Clustering=NULL)
      # values<-reactiveValues(submit_ClusterMarkers=FALSE)
      # shinyjs::hide("featurePlotBox")
      # shinyjs::disable("submit_NLDR")
      # shinyjs::disable("submit_Clustering")
      # shinyjs::disable("submit_GeneActivity")
      shinyjs::disable(selector = ".sidebar li a[data-value='aut_annot']")
      shinyjs::disable(selector = ".sidebar li a[data-value='twos']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threeone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      shinyjs::disable(selector = ".sidebar li a[data-value='fiveone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='fivetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='six']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#808080");});'
      shinyjs::runjs(js.3.1)
      js.4 <- ' $(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threeone]").css("color", "#637584");});'
      shinyjs::runjs(js.4)
      js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.41)
      js.511 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#637584");});'
      shinyjs::runjs(js.511)
      js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=fiveone]").css("color", "#637584");});'
      shinyjs::runjs(js.5)
      js.51 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=fivetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.51)
      js.6 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#808080");});'
      shinyjs::runjs(js.6)
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
      js.3 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=tow]").css("color", "#808080");});'
      shinyjs::runjs(js.3)
    }
  }
  
  clean_NLDR<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      Ident11<<-NULL
      Ident12<<-NULL
      Ident41<<-NULL
      Ident13<<-NULL
      Ident42<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      # gene.activities<<-NULL
      unique_clusters<<-NULL
      # Dynamic variables(动态变量)
      assaysList <<- NULL
      identsList <<- NULL
      # plots
      dbFeaturePlot <<- NULL
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      FindAllMarkers_result <<- NULL
      FindMarkers_result1 <<- NULL
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      ClusterMarkers_result1<<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      
      gene_names_overlapping<<-NULL
      #output-------------------------------------------
      output$ClusterDimPlot<<- NULL
      # output$Chr_num
      # output$Feature3
      output$FindAllMarkers_data<<- NULL
      output$FeaturePlot<<- NULL
      output$ClusterMarkers_data<<- NULL
      output$CompareMarkers_data<<- NULL
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      # reactive--------------------------------------------------------
      # values$dpDimPlot<<-NULL
      # values$submit_Clustering<<-NULL
      # #values<-reactiveValues(submit_Clustering=NULL)
      # values<-reactiveValues(submit_ClusterMarkers=FALSE)
      # shinyjs::hide("featurePlotBox")
      # shinyjs::disable("submit_Clustering")
      # shinyjs::disable("submit_GeneActivity")
      shinyjs::disable(selector = ".sidebar li a[data-value='twos']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threeone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      shinyjs::disable(selector = ".sidebar li a[data-value='fiveone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='fivetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='six']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#808080");});'
      shinyjs::runjs(js.3.1)
      js.4 <- ' $(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threeone]").css("color", "#637584");});'
      shinyjs::runjs(js.4)
      js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.41)
      js.511 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#637584");});'
      shinyjs::runjs(js.511)
      js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=fiveone]").css("color", "#637584");});'
      shinyjs::runjs(js.5)
      js.51 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=fivetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.51)
      js.6 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#808080");});'
      shinyjs::runjs(js.6)
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
    }
  }
  
  clean_Clustering<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      Ident11<<-NULL
      Ident12<<-NULL
      Ident41<<-NULL
      Ident13<<-NULL
      Ident42<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      # gene.activities<<-NULL
      unique_clusters<<-NULL
      # Dynamic variables(动态变量)
      assaysList <<- NULL
      identsList <<- NULL
      # plots
      dbFeaturePlot <<- NULL
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      FindAllMarkers_result <<- NULL
      FindMarkers_result1 <<- NULL
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      ClusterMarkers_result1<<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      
      gene_names_overlapping<<-NULL
      #output--------------------------------------------------------
      output$ClusterDimPlot<<- NULL
      # output$Chr_num
      # output$Feature3
      output$FindAllMarkers_data<<- NULL
      output$FeaturePlot<<- NULL
      output$ClusterMarkers_data<<- NULL
      output$CompareMarkers_data<<- NULL
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      # update--------------------------------------------------------
      brains@assays$RNA<<-NULL
      if(!is.null(brains)){
        assaysList <<- as.list(names(brains@assays)); names(assaysList) <- names(brains@assays)
        identsList <<- as.list(colnames(brains@meta.data)[unlist(purrr::map(colnames(brains@meta.data),function(x){is.factor(brains@meta.data[[x]])}))]);
        names(identsList) <- colnames(brains@meta.data)[unlist(purrr::map(colnames(brains@meta.data),function(x){is.factor(brains@meta.data[[x]])}))];
        updateRadioButtons(inputId="AssayRadio1",label="Assay:",choices=assaysList,selected = "peaks")
        updateRadioButtons(inputId="AssayRadio2",label="Assay:",choices=assaysList,selected = "peaks")
        updateRadioButtons(inputId="AssayRadio3",label="Assay:",choices=assaysList,selected = "peaks")
        
      }
      # reactive--------------------------------------------------------
      values<-reactiveValues(submit_ClusterMarkers=FALSE)
      shinyjs::hide("featurePlotBox")
      shinyjs::disable("submit_GeneActivity")
      shinyjs::disable(selector = ".sidebar li a[data-value='twos']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threeone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      shinyjs::disable(selector = ".sidebar li a[data-value='fiveone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='fivetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='six']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#808080");});'
      shinyjs::runjs(js.3.1)
      js.4 <- ' $(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threeone]").css("color", "#637584");});'
      shinyjs::runjs(js.4)
      js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.41)
      js.511 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#637584");});'
      shinyjs::runjs(js.511)
      js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=fiveone]").css("color", "#637584");});'
      shinyjs::runjs(js.5)
      js.51 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=fivetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.51)
      js.6 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#808080");});'
      shinyjs::runjs(js.6)
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
    }
  }
  clean_Findallmarker<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      Ident11<<-NULL
      Ident12<<-NULL
      Ident41<<-NULL
      Ident13<<-NULL
      Ident42<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      unique_clusters<<-NULL
      # plots
      dbFeaturePlot <<- NULL
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      FindAllMarkers_result <<- NULL
      FindMarkers_result1 <<- NULL
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      ClusterMarkers_result1<<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      #output--------------------------------------------------------
      # output$Chr_num
      # output$Feature3
      output$FindAllMarkers_data<<- NULL
      output$FeaturePlot<<- NULL
      output$ClusterMarkers_data<<- NULL
      output$CompareMarkers_data<<- NULL
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      # reactive--------------------------------------------------------
      values<-reactiveValues(submit_ClusterMarkers=FALSE)
      shinyjs::disable(selector = ".sidebar li a[data-value='twos']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      shinyjs::disable(selector = ".sidebar li a[data-value='fiveone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='fivetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='six']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#808080");});'
      shinyjs::runjs(js.3.1)
      js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.41)
      js.511 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#637584");});'
      shinyjs::runjs(js.511)
      js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=fiveone]").css("color", "#637584");});'
      shinyjs::runjs(js.5)
      js.51 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=fivetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.51)
      js.6 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#808080");});'
      shinyjs::runjs(js.6)
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
    }
  }
  clean_Findmarker<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      # Ident11<<-NULL
      # Ident12<<-NULL
      Ident41<<-NULL
      Ident42<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      # plots
      mtMotifPlot <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      FindMarkers_result1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs2 <<- NULL
      Motif_data2<<-NULL
      # output---------------------------------------------------------
      output$CompareMarkers_data<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      # reactive--------------------------------------------------------
      shinyjs::disable(selector = ".sidebar li a[data-value='fivetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='six']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.51 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=fivetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.51)
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
    }
  }
  
  clean_annotation<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      # plots
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      # output--------------------------------------------------------
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      # # reactive--------------------------------------------------------
      
      
    }
  }
  
  clean_motif_all_table<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      motifs1<<-NULL
      # plots
      mtMotifPlot <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      # output------------------------------------------------------
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      # reactive--------------------------------------------------------
      shinyjs::disable(selector = ".sidebar li a[data-value='six']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
    }
  }
  
  clean_motif_table<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      motifs1<<-NULL
      # plots
      mtMotifPlot <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      MotifTopMarkers2 <<- NULL
      enriched.motifs2 <<- NULL
      Motif_data2<<-NULL
      # output---------------------------------------------------------
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      
    }
  }
  
  
}

