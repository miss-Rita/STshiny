suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinydashboardPlus))
suppressPackageStartupMessages(library(MAST))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(GenomicRanges))

js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #8376a3;
}'

ui<-dashboardPage(
  
  
  
  
  # skin = "purple",
  title = "STshiny",
  dashboardHeader(
    
    title = shiny::tags$a(img(
      src = "STshiny_logo1.png", height = 50
    )),titleWidth = "240"),
  #目录--------------------------
  sidebar = shinydashboard::dashboardSidebar(
    
    sidebarMenu(
      id = "inTabset",
      menuItem("Home", tabName = "strat", icon = icon("home")), 
      menuItem(
        "Quality Control",
        tabName = "Quality_control",
        icon = icon("flask-vial", lib = "font-awesome")
      ), 
      #Visualize overall data   
      
      menuItem(
        "Visualize Overall Data",
        tabName = "Visual_data",
        icon = icon("dna", lib = "font-awesome")
      ),
      
      
      menuItem(
        "Data Preprocessing",
        tabName = "one",
        icon = icon("dna", lib = "font-awesome")
      ),
      menuItem(
        "Dim Reduction & Clustering",
        tabName = "ones",
        icon = icon("chart-column", lib = "font-awesome")
      ),
      
      menuItem(
        "Find Markers",
        tabName = "find_markers",
        icon = icon("chart-column", lib = "font-awesome")
      ),
      
      sidebarMenu(
        menuItem(
          "Cell Annotation",
          tabName = "two",
          icon = icon("tags", lib = "font-awesome"),
          menuSubItem("Automatic Annotation", tabName = "aut_annot"),
          menuSubItem("Manual Annotation", tabName = "manual_annot")
        )
      ),
      menuItem(
        "Regional Enrichment Analysis",
        tabName = "kegg_analysis",
        icon = icon("chart-column", lib = "font-awesome")
      ),
      ###1111---------
      menuItem(
        "Subset Out Anatomical Regions",
        tabName = "sc",
        icon = icon("chart-column", lib = "font-awesome")
      ),
      
      menuItem(
        "Integration With Single-cell",
        tabName = "sc_next",
        icon = icon("chart-column", lib = "font-awesome")
      ),
      
      
      # menuItem(
      #   "Regional Enrichment Analysis",
      #   tabName = "kegg_analysis",
      #   icon = icon("chart-column", lib = "font-awesome")
      # ),
      
      menuItem(
        "Help",
        tabName = "eight",
        icon = icon("glyphicon glyphicon-question-sign", lib = "glyphicon")
      ),
      menuItem(
        "About",
        tabName = "nine",
        icon = icon("glyphicon glyphicon-info-sign", lib = "glyphicon")
      )
    ),
    width="240px",
    
    # tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    # tags$script(src = "custom.js"),
    
    shiny::tags$head(
      shiny::tags$style(
        HTML('
      .main-content {
          height: 100%;
          padding: 20px;
          overflow: auto;
        }
      .main-sidebar {
        position: fixed;
        width: 300px;
        top: 0;
        left: 0;
        bottom: 0;
      }
      
      .sidebar {
        position: fixed;
        top: 50px;  /* 让侧边栏在标题下方 */
        bottom: 0;
        width: 250px;
      }
      
      /* 固定标题 */
      .main-header .logo{
        position: fixed;
        height: 50px;
        width: 100%;
        top: 0;
      }
      
      .navbar-static-top {
        position: fixed;
        width: 100%;
        top: 0;
      }
      
      html, body {
        margin: 0;
        padding: 0;
        height: 100%;
      }
      
      .content-wrapper {
        min-height: calc(100vh - 50px);  /* 减去标题栏的高度 */
        margin-left: 300px; /* 留出侧边栏的空间 */
        padding-top: 10px; /* 给顶部预留空间 */
      }
      
      .footer {
        position: fixed;
        bottom: 0;
        left: 0;
        width: 100%;
        height: 150px;  /* 页脚的高度 */
        background-color: #f1f1f1;  /* 背景颜色 */
        text-align: center;
        padding: 10px;
        z-index: 1000;  /* 确保页脚在最上层 */
      }
    ')
      )
    )
    
    
  ),
  
  body=dashboardBody(
    fresh::use_theme(mytheme),
    useShinyjs(),
    div(
      id="content-container",
      class="content-wrapper",
      style="width:auto;margin:0;padding:0;line-height:1;",
      useShinyjs(),
      tags$style(js),
      tags$head(
        tags$style(
          HTML(" .my-custom-div { font-size:17.5px; font-style:calibri; color:black; text-align:justify;line-height:1.5;}"))),
      
      tabItems(
        tabItem(
          tabName = "strat",
          useShinyjs(),
          
          # fluidRow(
          #   shinydashboard::box(
          #     title = tagList(icon("readme"),"Introduction"), width =12, solidHeader = T,  status = "primary", collapsible = TRUE,
          # 
          #     helpText(
          #       div(style = "font-size:17.5px; font-style:calibri; color:black; text-align:justify;line-height:1.5;",
          # 
          #           HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
          #           strong(code("STshiny is a user-friendly, interactive and open source web tool designed for spatial transcriptomics data analysis.")),
          # 
          #           "It supportsand",
          #           strong("Seurat objects"),
          #           "and can efficiently analyze and visualize spatially resolved RNA sequencing data.",
          # 
          #           strong(code("The core advantage of spatial transcriptomics is that it can preserve the spatial information of samples")),
          #           "which allows gene expression to be not only accurately measured, but also clearly defines the specific location
          #           of these expressions in the tissue. In addition, spatial transcriptomics can also provide information in the spatiotemporal
          #           dimension, revealing the dynamic changes of gene expression over time and space, and can be combined with other omics data to
          #           reveal the heterogeneity of cells in the same tissue.",
          #           br(),
          #           HTML("&nbsp;&nbsp;&nbsp;&nbsp;With STshiny, users can easily upload spatial transcriptomics data, including"),
          # 
          #           strong(code("filtered_feature_bc_matrix.h5 and spatial.zip files.")),
          #           "After uploading, users will unlock a series of powerful analysis modules covering key links such as",
          #           strong("quality control, data preprocessing, dimensionality reduction clustering, cell annotation, Subset out anatomical regions,
          #                  Integration with single-cell data, and regional functional annotation."),
          #           "Users can also easily perform data analysis, browse and download various visual analysis results by clicking buttons, greatly
          #           improving the processing efficiency and operability of spatial transcriptomics data.",
          #           br(),
          #            HTML("&nbsp;&nbsp;&nbsp;&nbsp;Looking ahead, we will continue to update STshiny, optimize existing functions,
          #           and add more downstream analysis modules to meet the growing scientific research needs and promote the in-depth development of
          #           spatial transcriptomics research.",
          #           )
          #     )
          #     )
          #   )),
          
          
          
          
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("folder-open"),"InputData"), width =NULL, status = "primary", collapsible = TRUE,
                     selectInput("Selectdata", label = ("Data source："), choices = list("stxBrain","Custom Data"),selected = "stxBrain"),
                     uiOutput("FileInputs"),
                     
                     br(),
                     do.call(actionBttn, c(
                       list(
                         inputId = "submit_Start",
                         label = "Start",
                         icon = icon("play")
                       ),
                       actionBttnParams
                     )),
                     shiny::tags$style(
                       HTML("
                           #submit_Start { 
                            box-shadow: 0px 2px 5px #888888;
                              border-radius: 10px;
                          }
                            #submit_Start:hover { 
                              color: #8376a3; 
                            }
                          "))
                     # shiny::tags$style(
                     #   "#submit {box-shadow: 0px 2px 5px #888888;}"
                     # ),
                     # br(),div(actionButton("submit_Start","Start", icon = icon("play-circle")), align = "center")
                     
                   ))),
          # fluidRow(
          # column(12,
          #        shinydashboard::box(
          #          title = tagList(icon("puzzle-piece"),"Workflow"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
          #          align = "center",
          #          shiny::tags$img(src = "workflow.png", width = "50%", height = "auto"),
          #              helpText(
          #                div(style = "font-size:17.5px; font-style:calibri; color:black; text-align:justify;line-height:1.5;",
          # 
          #                    HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
          #                    strong(code("STshiny is a user-friendly, interactive and open source web tool designed for spatial transcriptomics data analysis.")),
          # 
          #                    "It supportsand",
          #                    strong("Seurat objects"),
          #                    "and can efficiently analyze and visualize spatially resolved RNA sequencing data.",
          # 
          #                    strong(code("The core advantage of spatial transcriptomics is that it can preserve the spatial information of samples")),
          #                    "which allows gene expression to be not only accurately measured, but also clearly defines the specific location
          #                    of these expressions in the tissue. In addition, spatial transcriptomics can also provide information in the spatiotemporal
          #                    dimension, revealing the dynamic changes of gene expression over time and space, and can be combined with other omics data to
          #                    reveal the heterogeneity of cells in the same tissue.",
          #                    br(),
          #                    HTML("&nbsp;&nbsp;&nbsp;&nbsp;With STshiny, users can easily upload spatial transcriptomics data, including"),
          # 
          #                    strong(code("filtered_feature_bc_matrix.h5 and spatial.zip files.")),
          #                    "After uploading, users will unlock a series of powerful analysis modules covering key links such as",
          #                    strong("quality control, data preprocessing, dimensionality reduction clustering, cell annotation, Subset out anatomical regions,
          #                           Integration with single-cell data, and regional functional annotation."),
          #                    "Users can also easily perform data analysis, browse and download various visual analysis results by clicking buttons, greatly
          #                    improving the processing efficiency and operability of spatial transcriptomics data.",
          #                    br(),
          #                     HTML("&nbsp;&nbsp;&nbsp;&nbsp;Looking ahead, we will continue to update STshiny, optimize existing functions,
          #                    and add more downstream analysis modules to meet the growing scientific research needs and promote the in-depth development of
          #                    spatial transcriptomics research.",
          #                    )
          #              )
          #              )
          #        )))
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("puzzle-piece"), "Workflow"), 
                     width = NULL, 
                     solidHeader = FALSE, 
                     status = "primary", 
                     collapsible = TRUE,
                     align = "center",
                     
                     fluidRow(  # 使用 fluidRow 来分割内容
                       column(5,  # 文字部分占 50%
                              helpText(
                                div(style = "font-size:17.5px; font-style:calibri; color:black; text-align:justify;line-height:1.5;",
                                    HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
                                    strong(code("STshiny is a user-friendly, interactive and open source web tool designed for spatial transcriptomics data analysis.")),
                                    "It supports and",
                                    strong("Seurat objects"),
                                    "and can efficiently analyze and visualize spatially resolved RNA sequencing data.",
                                    strong(code("The core advantage of spatial transcriptomics is that it can preserve the spatial information of samples")),
                                    "which allows gene expression to be not only accurately measured, but also clearly defines the specific location of these expressions in the tissue. In addition, spatial transcriptomics can also provide information in the spatiotemporal dimension, revealing the dynamic changes of gene expression over time and space, and can be combined with other omics data to reveal the heterogeneity of cells in the same tissue.",
                                    br(),
                                    HTML("&nbsp;&nbsp;&nbsp;&nbsp;With STshiny, users can easily upload spatial transcriptomics data, including"),
                                    strong(code("filtered_feature_bc_matrix.h5 and spatial.zip files.")),
                                    "After uploading, users will unlock a series of powerful analysis modules covering key links such as",
                                    strong("quality control, data preprocessing, dimensionality reduction clustering, cell annotation, Subset out anatomical regions, Integration with single-cell data, and regional functional annotation."),
                                    "Users can also easily perform data analysis, browse and download various visual analysis results by clicking buttons, greatly improving the processing efficiency and operability of spatial transcriptomics data.",
                                    br(),
                                    HTML("&nbsp;&nbsp;&nbsp;&nbsp;Looking ahead, we will continue to update STshiny, optimize existing functions, and add more downstream analysis modules to meet the growing scientific research needs and promote the in-depth development of spatial transcriptomics research.")
                                )
                              )
                       ),
                       column(7,  # 图片部分占 50%
                              tags$img(src = "workflow7.png", width = "100%", height = "auto", style = "margin: auto;")
                       )
                     )
                   )
            )
          )
        ),
        ####质控------      
        tabItem(
          tabName = "Quality_control",
          
          useShinyjs(), 
          fluidRow(
            shinydashboard::box(
              title = tagList(icon("readme"),"Introduction"), width =12, solidHeader = T, status = "primary", collapsible = TRUE,
              
              helpText(
                div(class="my-custom-div",
                    strong(code("Quality control modules:")),
                    " The quality control module is a key preprocessing step designed to help users",
                    strong(code("remove low-quality cells and technical noise")),
                    "to ensure the accuracy of subsequent analysis. STshiny will use",
                    strong("violin plots"),
                    "for visualization based on the data entered by the user. 
                   The violin plots can be analyzed to decide whether to filter. Users can use two indicators,",
                    strong("nCount and nFeature,"),
                    "to filter cells with low expression (possibly vacuoles) and high expression (possibly double cells) to improve the",
                    strong("accuracy and usability "),
                    "of the data."
                )), 
              div(style = "text-align: center; width: 100%;",
                  div(style = "display: inline-block;",actionButton("submit_skip", "Skip", icon = icon("play-circle"))
                  ))
              
            )
          ),
          
          
          fluidRow(column(3,
                          shinydashboard::box(
                            title =tagList(icon("wrench"),"Filter"),width = NULL,status = "primary",collapsed = T,
                            helpText(
                              div(class="my-custom-div",
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
          ",                                  strong(code("Tips:")),
                                  "Filtering the data can reduce the number of points in the cluster and reduce 
                                  the impact on the subsequent clustering results. Not filtering the data can keep 
                                  the tissue image intact and make the drawing look better. The quality control module 
                                  can filter based on UMI, gene number, etc. The filtering criteria of BGI are: nFeature_Spatial>=20 & 
                                  nCount_Spatial>=3"
                              )), 
                            splitLayout(cellWidths=c("50%","50%"),
                                        numericInput("nFeature_Spatial_min", "nFeature Min:", value = 0),
                                        numericInput("nFeature_Spatial_max", "Max:", value = 7500)
                            ),
                            
                            splitLayout(cellWidths=c("50%","50%"),
                                        numericInput("nCount_Spatial_min", "nCount Min:", value = 0),
                                        numericInput("nCount_Spatial_max", "Max:", value = 60000)
                            ),
                            
                            div(style = "text-align: center; width: 100%;",
                                div(style = "display: inline-block;",actionButton("submit_QC", "Start", icon = icon("play-circle"))
                                ))
                          )),
                   column(9,
                          shinydashboard::tabBox(
                            id="Filter_plots",width =NULL,
                            tabPanel(
                              title = "Before Filter",value = "Vlnplot_before",
                              plotOutput("Vlnplot_before",width="100%")%>%withSpinner(color = "#8e81aa"),
                              splitLayout(cellWidths = c("30%","30%","40%"),
                                          numericInput("Filter_plots_PlotWidth", "Width", min = 0, max = 250, value = 10),
                                          numericInput("Filter_plots_PlotHeight", "Height", min = 0, max = 250, value = 7),
                                          selectizeInput('Filter_plots_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png"),
                                          tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                              
                              div(downloadButton(outputId = "Filter_plots_download_plot", label = "Download"), align = "center")
                            ),
                            tabPanel(
                              title = "After Filter",value = "Vlnplot_after",
                              conditionalPanel(
                                condition = "input.submit_QC > 0",  # 只有在按钮按下后，才显示
                                plotOutput("Vlnplot_after",width="100%")%>%withSpinner(color = "#8e81aa")
                              ),
                              
                              
                              
                              
                              splitLayout(cellWidths = c("30%","30%","40%"),
                                          numericInput("Filter_plots_PlotWidth2", "Width", min = 0, max = 250, value = 10),
                                          numericInput("Filter_plots_PlotHeight2", "Height", min = 0, max = 250, value = 7),
                                          selectizeInput('Filter_plots_PlotChoices2',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png"),
                                          tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                              tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}"))),
                              div(downloadButton(outputId = "Filter_plots_download_plot2", label = "Download"), align = "center"),
                              div(actionButton("submit_next", label = "Next",icon = icon("arrow-right")), style = "text-align: right;"),
                              
                            ),
                          ))
          )),
        
        ####Visual_data-----------
        tabItem(
          tabName = "Visual_data",
          useShinyjs(),
          fluidRow(
            shinydashboard::box(
              title = tagList(icon("readme"), "Introduction"),
              width = 12,
              solidHeader = T,
              status = "primary",
              collapsible = TRUE,
              helpText(
                div(
                  class = "my-custom-div",
                  strong(code("Visualize Overall Data module:")),
                  "This module uses violin plots and spatial feature plots to visualize the two indicators",
                  strong("nCount and nFeature"),
                  "based on the quality control or original data. Through",
                  strong(code("spatial feature plots")),
                  ", users can clearly see the location-specific expression of genes 
            in tissues, helping them understand the role of genes in tissue structure, 
            identify potential regional expression differences, and explore the relationship between genes and spatial regions.",
                  strong(code("This is of great significance for studying heterogeneity and spatial structure in tissues.")),
                  
                  
                  
                )),
              div(
                actionButton(
                  "submit_Visual_data",
                  "Start",
                  icon = icon("play-circle")
                ),
                align = "center"
              )
            )
          ), 
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("chart-column"),"Violin Plot and Spatial Feature Plots"), width =NULL,status = "primary", collapsible = TRUE,
                     
                     conditionalPanel(
                       condition = "input.submit_Visual_data > 0",  # 只有在按钮按下后，才显示
                       plotOutput("brain_Vlnplot",width="100%")%>%withSpinner(color = "#8e81aa")
                     ),
                     splitLayout(cellWidths = c("65%","35%"),
                                 selectInput('brain_Vlnplot_SpatialChoice', 
                                             label = "Select Features", 
                                             choices = c("nCount_Spatial", "nFeature_Spatial"), 
                                             selected = "nCount_Spatial"),
                                 numericInput("pt_size", "Point Size:", value = 2, min = 0.1, max = 2, step = 0.1)
                     ),
                     splitLayout(cellWidths = c("30%","30%","40%"),
                                 numericInput("brain_Vlnplot_PlotWidth", "Width", min = 0, max = 250, value = 10),
                                 numericInput("brain_Vlnplot_PlotHeight", "Height", min = 0, max = 250, value = 7),
                                 selectizeInput('brain_Vlnplot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png"),
                                 tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                     
                     div(downloadButton(outputId = "brain_Vlnplot_download_plot", label = "Download"), align = "center")
                   )),
            
          )),
        
        
        
        
        
        
        ####数据预处理-----------------------
        
        tabItem(
          tabName = "one",
          fluidRow(
            shinydashboard::box(
              title = tagList(icon("readme"), "Introduction"),
              width = 12,
              solidHeader = T,
              status = "primary",
              collapsible = TRUE,
              helpText(
                div(
                  class = "my-custom-div",
                  strong(code("Data preprocessing module: including standardization and normalization.")),
                  "Through",
                  strong(code("SCTransform")),
                  ", STshiny can standardize the data after removing technical noise unrelated to gene expression, 
            so as to more accurately reflect the true expression level of the gene.STshiny also visualizes two key indicators,",
                  strong(code("nCount and nFeature")),
                  ", through",
                  strong("violin plots and spatial feature plots,"),
                  "to help users intuitively display the expression patterns of genes in different cells, tissues or spatial locations.
            These visualizations not only help to evaluate the quality of the data, but also reveal the distribution and expression 
            changes of genes in spatial organizations, providing important information for subsequent analysis.",
                  
                  
                )),
              div(
                actionButton(
                  "submit_GeneExpression",
                  "Start",
                  icon = icon("play-circle")
                ),
                align = "center"
              )
            )
          ), 
          fluidRow(
            
            shinydashboard::box(
              title = tagList(icon("table"),"Gene Expression", 
                              bsButton('q1',label = '',icon = icon(name = 'question'),size = 'extra-small'),
                              bsPopover(id = 'q1',title=NULL,
                                        content ='detection_rate: The frequency with which the gene is detected in the dataset.
                                        gmean: The geometric mean of the gene, usually used to measure gene expression levels.
                                        variance: The variance of gene expression data indicates the degree of variation in the expression level of a gene in all samples.
                                        residual_mean: represents the mean of the residuals. A smaller residual mean may mean a better model fit.
                                        residual_variance: It is used to measure the error or variation remaining after the model is fitted. A larger value means that the model fits poorly.',
                                        placement = 'right',trigger = 'hover',options = list(container = 'body')
                              )),width =12,status = "primary", collapsible = TRUE,
              # 设置溢出控制
              tags$style(HTML("
        .box-body {
          overflow-x: auto;
          max-width: 100%;
        }
      ")), 
              
              conditionalPanel(
                condition = "input.submit_GeneExpression > 0",  # 只有在按钮按下后，才显示
                DTOutput("hvgs_df", width = "100%", height = "400px")
              ),
              fluidRow(
                column(
                  3,
                  radioButtons(inputId = "GeneExpression_table_format",label = helpText("Output Format"),choices = c("CSV" = "csv", "TXT" = "txt"), inline = T),
                  
                  div(
                    downloadButton("GeneExpression_table_download", "Download"),
                    shiny::tags$style(
                      "#tableDown {background-color: white; color: black; margin-left: 0%; margin-top:25px; box-shadow: inset 0px 1px 2.5px #888888;}"
                    )
                  )
                )),
              
              
              
              div(actionButton("submit_GeneExpression_continue","Continue",
                               icon = icon("play-circle")),align = "center"))
            
          ),
          fluidRow(
            
            column(12,
                   shinydashboard::box(
                     
                     title = tagList(icon("chart-column"),"Spatial Feature Plot"), width =NULL,status = "primary", collapsible = TRUE,
                     
                     conditionalPanel(
                       condition = "input.submit_GeneExpression > 0 && input.submit_GeneExpression_continue > 0",  # 只有在两个按钮都点击后才显示
                       plotOutput("SpatialFeaturePlot", width = "100%") %>% withSpinner(color = "#8e81aa")
                     ),
                     
                     splitLayout(cellWidths = c("50%","50%"),
                                 textInput("genes_input1", "Gene Names (comma-separated):", "Hpca,Ttr"),
                                 numericInput("pt_size_input", "Point Size:", value = 2, min = 1, max = 10, step = 0.1)
                     ),
                     splitLayout(cellWidths = c("30%","30%","40%"),
                                 numericInput("SpatialFeaturePlot_PlotWidth", "Width", min = 0, max = 250, value = 10),
                                 numericInput("SpatialFeaturePlot_PlotHeight", "Height", min = 0, max = 250, value = 7),
                                 selectizeInput('SpatialFeaturePlot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png"),
                                 tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                     div(downloadButton(outputId = "SpatialFeaturePlot_download_plot", label = "Download"), align = "center")
                     
                   ))
          )),
        ####降维聚类-------
        tabItem(
          tabName = "ones",
          fluidRow(
            shinydashboard::box(
              title = tagList(icon("readme"),"Introduction"), width =12, solidHeader = T, status = "primary", collapsible = TRUE,
              helpText(div(class="my-custom-div",
                           strong(code("Dim Reduction & Clustering Module:")),
                           strong("It maps high-dimensional gene expression data to low-dimensional space, 
                                  and groups cells or spatial locations with similar gene expression patterns through clustering analysis"),
                           
                           ", thereby identifying different cell populations or functional regions. 
                           This module supports visualization of clustering results in ",
                           strong(code("UMAP space")),
                           ", and users can also ",
                           strong(code("select specific clusters")),
                           " for spatial location visualization. This process helps to reveal the spatial distribution of cells 
                           in tissues and their relationships, thus providing important information for understanding tissue structure and 
                           cell function.",
                           
              )), div(actionButton("submit_DimClu","Start", icon = icon("play-circle")), align = "center")
            )),
          fluidRow(
            column(width = 6,
                   shinydashboard::box(
                     title = tagList(icon("chart-column"),"Dimplot and Spatial Dimplot"), width =NULL, status = "primary", collapsible = TRUE,
                     
                     conditionalPanel(
                       condition = "input.submit_DimClu > 0",  # 只有在按钮按下后，才显示
                       plotOutput("umap_dim",width="100%")%>%withSpinner(color = "#8e81aa")
                     ),
                     
                     
                     splitLayout(cellWidths = c("100%"),
                                 # numericInput("dims_input",label = "number of dimensions:",value = 30,min = 1, max = 50),
                                 numericInput("label_size", label = "Label Size:",value = 3, min = 1,max = 10), 
                                 tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                     splitLayout(cellWidths = c("30%","30%","40%"),
                                 numericInput("umap_Dimplot_PlotWidth", "Width", min = 0, max = 250, value = 10),
                                 numericInput("umap_Dimplot_PlotHeight", "Height", min = 0, max = 250, value = 7),
                                 selectizeInput('umap_Dimplot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png"),
                                 tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                     div(downloadButton(outputId = "umap_Dimplot_download_plot", label = "Download"), align = "center")
                   )),
            column(width = 6,
                   shinydashboard::box(
                     title = tagList(icon("chart-column"),"Spatial DimPlot"), width =NULL,side="right",status = "primary", collapsible = TRUE,
                     
                     conditionalPanel(
                       condition = "input.submit_DimClu > 0",  # 只有在按钮按下后，才显示
                       plotOutput("SpatialDimPlot",width="100%")%>%withSpinner(color = "#8e81aa"),
                     ),
                     
                     splitLayout(cellWidths=c("50%","50%"),
                                 textInput("idents_input",label = "Cluster Identities :",value = "2,1,4,3,5,8"),
                                 numericInput("pt_size_input", "Point Sizes:", value = 2, min = 1, max = 10, step = 0.1)
                     ),
                     
                     splitLayout(cellWidths = c("30%","30%","40%"),
                                 numericInput("SpatialDimPlot_PlotWidth", "Width", min = 0, max = 250, value = 10),
                                 numericInput("SpatialDimPlot_PlotHeight", "Height", min = 0, max = 250, value = 7),
                                 selectizeInput('SpatialDimPlot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png"),
                                 tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                     div(downloadButton(outputId = "SpatialDimPlot_download_plot", label = "Download"), align = "center")
                     
                   )))
          
          
        ),
        
        #### findmarker----
        tabItem(
          tabName = "find_markers",
          fluidRow(
            shinydashboard::box(
              title = tagList(icon("readme"),"Introduction"), width =12, solidHeader = T, status = "primary", collapsible = TRUE,
              helpText(div(class="my-custom-div",
                           strong(code("Find Markers Module:")),
                           "This module uses the",
                           strong(code("COSG method")),
                           "to identify the",
                           strong(code("top 100")),
                           "significantly differentially expressed genes between different cell populations in the dataset.
                     Its main function is to help users determine which genes have significant differences in expression 
                     levels between different cell populations or clusters, and then ",
                           strong("reveal the marker genes for each cluster."),
                           
              )), div(actionButton("submit_FM","Start", icon = icon("play-circle")), align = "center")
            )),
          
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("table"),"Markers"), width =NULL,status = "primary", collapsible = TRUE,
                     
                     
                     
                     conditionalPanel(
                       condition = "input.submit_FM > 0",  # 只有在按钮按下后，才显示
                       DTOutput("de_markers_df", width = "100%", height = "400px")%>%withSpinner(color = "#8e81aa")
                     ),
                     br(),
                     fluidRow(
                       column(
                         3,
                         radioButtons(inputId = "markers_table_format",label = helpText("Output Format"),choices = c("CSV" = "csv", "TXT" = "txt"), inline = T),
                         
                         div(
                           downloadButton("markers_table_download", "Download"),
                           shiny::tags$style(
                             "#tableDown {background-color: white; color: black; margin-left: 0%; margin-top:25px; box-shadow: inset 0px 1px 2.5px #888888;}"
                           )
                         )
                       ))
                     
                   ))),
          
          
        ),
        
        
        
        
        
        
        
        
        
        ####注释-------  
        tabItem(
          tabName = "aut_annot",
          
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("readme"),"Introduction"), width =NULL,  solidHeader = T, status = "primary", collapsible = TRUE,
                     helpText(div(class="my-custom-div",
                                  code(strong("Automatic Annotation Module:")),
                                  "This module will use Violin Plot, DimPlot, DotPlot and Spatial FeaturePlot to visualize the marker genes selected by the user. And",
                                  strong("use"),
                                  strong(code("SingleR")),
                                  strong("to automatically annotate cell types."),
                                  
                                  "This tool supports reference datasets of two species,",
                                  strong(code("human and mouse")),
                                  ", especially the immune genes of these two species. Among them, the mouse reference 
                                  dataset ImmGenData mainly focuses on immune cells, while MouseRNAseqData covers a wider range 
                                  of cells. In addition, the annotation section of this module also",
                                  strong(code("allows users to input reference datasets in rds format by themselves")),
                                  ", so as to flexibly adapt to the analysis needs of different users."
                                  
                     )),div(actionButton("submit_de_markers_df","Start", icon = icon("play-circle")), align = "center")
                   ))),
          
          fluidRow(
            column(12,
                   shinydashboard::tabBox(
                     id="markers_plots",width =NULL,
                     tabPanel(
                       title = "Violin Plot",value = "Vlnplot", collapsible = TRUE,
                       conditionalPanel(
                         condition = "input.submit_de_markers_df > 0",  # 只有在按钮按下后，才显示
                         plotOutput("vlnplot_marker",width="100%")%>%withSpinner(color = "#8e81aa")
                       ),
                       
                       
                       splitLayout(cellWidths = c("60%","40%"),
                                   textInput("genes_input2", "Enter Gene Names (comma-separated):", "Hpca",width = "100%"),
                                   numericInput("pt_size_input2", "Enter Point Size:", value = 1, min = 1, max = 10, step = 0.1,width = "100%"),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))
                                   
                       ),
                       splitLayout(cellWidths = c("30%","30%","40%"),
                                   numericInput("vlnplot_marker_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                   numericInput("vlnplot_marker_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                   selectizeInput('vlnplot_marker_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                       div(downloadButton(outputId = "vlnplot_marker_download_plot", label = "Download"), align = "center")
                       
                     ),
                     
                     
                     tabPanel(
                       title = "DimPlot",value = "Dimplot_aut", collapsible = TRUE,
                       conditionalPanel(
                         condition = "input.submit_de_markers_df > 0",  # 只有在按钮按下后，才显示
                         plotOutput("Dimplot_aut",width="100%")%>%withSpinner(color = "#8e81aa")
                       ),
                       splitLayout(cellWidths = c("30%","30%","40%"),
                                   numericInput("Dimplot_aut_PlotWidth", "Width", min = 0, max = 250, value = 10,,width = "100%"),
                                   numericInput("Dimplot_aut_PlotHeight", "Height", min = 0, max = 250, value = 7,,width = "100%"),
                                   selectizeInput('Dimplot_aut_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",,width = "100%"),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                       div(downloadButton(outputId = "Dimplot_aut_download_plot", label = "Download"), align = "center")
                     ),
                     
                     
                     
                     
                     
                     tabPanel(
                       title = "DotPlot",value = "DotPlot", collapsible = TRUE,
                       conditionalPanel(
                         condition = "input.submit_de_markers_df > 0",  # 只有在按钮按下后，才显示
                         plotOutput("DotPlot",width="100%")%>%withSpinner(color = "#8e81aa")
                       ),
                       
                       splitLayout(cellWidths = c("100%"),
                                   sliderInput(
                                     inputId = "top_input_DotPlot",label = "Select number of top markers to display:",min = 1, max = 100, value =3, step = 1,width = "100%"
                                   ),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))
                       ),
                       splitLayout(cellWidths = c("30%","30%","40%"),
                                   numericInput("DotPlot_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                   numericInput("DotPlot_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                   selectizeInput('DotPlot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                       div(downloadButton(outputId = "DotPlot_download_plot", label = "Download"), align = "center")
                     ),
                     
                     
                     
                     tabPanel(
                       title = "Spatial Feature Plot",value = "SpatialFeaturePlot_marker", collapsible = TRUE,
                       
                       conditionalPanel(
                         condition = "input.submit_de_markers_df > 0",  # 只有在按钮按下后，才显示
                         plotOutput("SpatialFeaturePlot_marker",width="100%")%>%withSpinner(color = "#8e81aa")
                       ),
                       
                       splitLayout(cellWidths = c("80%","20%"),
                                   sliderInput(
                                     inputId = "top_input",label = "Select number of top markers to display:",min = 1, max = 100, value =2, step = 1,width = "100%"
                                   ),
                                   numericInput("pt_size_input3", "Enter Point Size:", value = 2, min = 1, max = 10, step = 0.1,width = "100%"),
                       ),
                       splitLayout(cellWidths = c("30%","30%","40%"),
                                   numericInput("SpatialFeaturePlot_marker_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                   numericInput("SpatialFeaturePlot_marker_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                   selectizeInput('SpatialFeaturePlot_marker_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                       div(downloadButton(outputId = "SpatialFeaturePlot_marker_download_plot", label = "Download"), align = "center")
                     ),
                   )
            )),
          tags$head(tags$style(HTML("
    .shiny-split-layout > div { overflow: visible !important; }
    .selectize-dropdown { z-index: 9999 !important; } 
  "))),
          ####自动注释-----        
          fluidRow(
            column(12,
                   shinydashboard::box(
                     id="singleR_annot",width =NULL,
                     # div(actionButton("submit_singleR","Continue", icon = icon("play-circle")), align = "center"),
                     title = tagList(icon("area-chart", lib = "font-awesome"), "DimPlot and SpatialDimPlot"),
                     value = "annot_dim", status = "primary",collapsible = TRUE,
                     
                     sidebarLayout(
                       sidebarPanel(
                         width = 3,
                         
                         title = tagList(icon("area-chart", lib = "font-awesome"), "DimPlot and SpatialDimPlot"),
                         
                         selectInput("ref_data_choice", "Choose a Dataset:", 
                                     choices = c("BlueprintEncodeData", 
                                                 "DatabaseImmuneCellExpressionData",
                                                 "HumanPrimaryCellAtlasData",
                                                 "ImmGenData",
                                                 "MonacoImmuneData",
                                                 "MouseRNAseqData",
                                                 "NovershternHematopoieticData",
                                                 "CustomData"),  # 增加这个选项
                                     selected = "MouseRNAseqData"),
                         ###1111----         
                         conditionalPanel(
                           condition = "input.ref_data_choice == 'CustomData'",
                           fileInput("custom_ref", "Upload Custom Data (.rds):", 
                                     accept = c(".rds")),
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
                               strong(code("Tips:")),
                               br(),
                               tags$div(style = "text-indent: 20px;",
                                        "1. The count matrix must have index as the row name and contain a ref_label column, as well as other additional information."),
                               
                               tags$div(style = "text-indent: 20px;", 
                                        "2. The data needs to be standardized using logNormCounts and converted to a SummarizedExperiment object."),
                               tags$div(style = "text-indent: 20px;", 
                                        "3. The input data format must be in rds format.")
                             )
                           )
                           
                           
                           
                         ),
                         
                         conditionalPanel(
                           condition = "input.ref_data_choice != 'CustomData'",  # 只有在选择非 CustomData 时才显示
                           selectInput("annotation_type", "Choose Annotation Type:", 
                                       choices = c("Coarse Annotation" = "label.main", 
                                                   "Fine Annotation" = "label.fine"),
                                       selected = "label.fine")
                         ),
                         div(actionButton("submit_singleR", "Continue", icon = icon("play-circle")), align = "center"),
                         
                       ),
                       
                       mainPanel(
                         conditionalPanel(
                           condition = "input.submit_singleR > 0",  # 只有在按钮按下后，才显示
                           plotOutput("SpatialDimPlot_annot", width = "100%") %>% withSpinner(color = "#8e81aa"),
                           splitLayout(cellWidths = c("30%","30%","40%"),
                                       numericInput("SpatialDimPlot_annot_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                       numericInput("SpatialDimPlot_annot_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                       selectizeInput('SpatialDimPlot_annot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                       tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                           div(downloadButton(outputId = "SpatialDimPlot_annot_download_plot", label = "Download"), align = "center")
                           
                         )
                       )
                     )
                     
                     
                   )
            )),
          
        ),
        
        ####手动注释----         
        tabItem(
          tabName = "manual_annot",
          
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("readme"),"Introduction"), width =NULL,  solidHeader = T, status = "primary", collapsible = TRUE,
                     helpText(div(class="my-custom-div",
                                  strong(code("Manual Annotation Module:")),
                                  "This module helps users display the expression patterns of selected marker genes through 
                          visualization methods such as Violin Plot, DimPlot, DotPlot and Spatial FeaturePlot. 
                          Users can use these visualization charts,",
                                  strong("combined with the analysis results generated in the previous modules, and their own prior knowledge to manually annotate cell types."),
                                  "Specifically, STshiny allows users to",
                                  strong(code("label them as specific cell types based on the selected cluster (clustering results)")),
                                  ", thereby completing the manual labeling and annotation of cell types.",
                                  
                     )),div(actionButton("submit_de_markers_manual","Start", icon = icon("play-circle")), align = "center")
                   ))),
          
          fluidRow(
            column(12,
                   shinydashboard::tabBox(
                     id="markers_plots_manual",width =NULL,
                     tabPanel(
                       title = "Violin Plot",value = "Vlnplot_manual",
                       
                       conditionalPanel(
                         condition = "input.submit_de_markers_manual > 0",  # 只有在按钮按下后，才显示
                         plotOutput("Vlnplot_manual",width="100%")%>%withSpinner(color = "#8e81aa")
                       ),
                       
                       splitLayout(cellWidths = c("60%","40%"),
                                   textInput("genes_input_manual", "Enter Gene Names (comma-separated):", "Hpca",width = "100%"),
                                   numericInput("pt_size_input_manual", "Enter Point Size:", value = 1, min = 1, max = 10, step = 0.1,width = "100%"),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))
                                   
                       ),
                       splitLayout(cellWidths = c("30%","30%","40%"),
                                   numericInput("Vlnplot_manual_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                   numericInput("Vlnplot_manual_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                   selectizeInput('Vlnplot_manual_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                       div(downloadButton(outputId = "Vlnplot_manual_download_plot", label = "Download"), align = "center")
                     ),
                     tabPanel(
                       title = "DimPlot",value = "Dimplot_manual",
                       
                       conditionalPanel(
                         condition = "input.submit_de_markers_manual > 0",  # 只有在按钮按下后，才显示
                         plotOutput("Dimplot_manual",width="100%")%>%withSpinner(color = "#8e81aa")
                       ),
                       splitLayout(cellWidths = c("30%","30%","40%"),
                                   numericInput("Dimplot_manual_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                   numericInput("Dimplot_manual_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                   selectizeInput('Dimplot_manual_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                       div(downloadButton(outputId = "Dimplot_manual_download_plot", label = "Download"), align = "center")
                       
                     ),
                     
                     tabPanel(
                       title = "DotPlot",value = "DotPlot_manual",
                       
                       conditionalPanel(
                         condition = "input.submit_de_markers_manual > 0",  # 只有在按钮按下后，才显示
                         plotOutput("DotPlot_manual",width="100%")%>%withSpinner(color = "#8e81aa")
                       ),
                       
                       splitLayout(cellWidths = c("100%"),
                                   sliderInput(
                                     inputId = "top_input_DotPlot_manual",label = "Select number of top markers to display:",min = 1, max = 100, value =3, step = 1,width = "100%"
                                   ),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))
                       ),
                       splitLayout(cellWidths = c("30%","30%","40%"),
                                   numericInput("DotPlot_manual_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                   numericInput("DotPlot_manual_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                   selectizeInput('DotPlot_manual_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                       div(downloadButton(outputId = "DotPlot_manual_download_plot", label = "Download"), align = "center")
                     ),
                     
                     tabPanel(
                       title = "Spatial Feature Plot",value = "SpatialFeaturePlot_manual",status = "primary", collapsible = TRUE,
                       
                       conditionalPanel(
                         condition = "input.submit_de_markers_manual > 0",  # 只有在按钮按下后，才显示
                         plotOutput("SpatialFeaturePlot_manual",width="100%")%>%withSpinner(color = "#8e81aa")
                       ),
                       
                       splitLayout(cellWidths = c("80%","20%"),
                                   sliderInput(
                                     inputId = "top_input_manual",label = "Select number of top markers to display:",min = 1, max = 100, value =4, step = 1,width = "100%"
                                   ),
                                   numericInput("pt_size_input_manual", "Enter Point Size:", value = 2, min = 1, max = 10, step = 0.1),
                       ),
                       splitLayout(cellWidths = c("30%","30%","40%"),
                                   numericInput("SpatialFeaturePlot_manual_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                   numericInput("SpatialFeaturePlot_manual_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                   selectizeInput('SpatialFeaturePlot_manual_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                       div(downloadButton(outputId = "SpatialFeaturePlot_manual_download_plot", label = "Download"), align = "center")
                     ),
                   )
            )),
          
          fluidRow(
            column(12,
                   shinydashboard::box(
                     id="manual_annot",width =NULL,
                     title = tagList(icon("area-chart", lib = "font-awesome"), "Manual Annotation"),  
                     status = "primary", 
                     collapsible = TRUE, 
                     ####手动输入注释啊啊啊-----
                     tags$div(style = "margin-top: 20px;",uiOutput("manual_input")),
                     actionButton("update", "Update",class = "btn-primary"),
                     div(
                       actionButton("submit_manual", "Continue", icon = icon("play-circle")),
                       style = "display: flex; justify-content: center; align-items: center; margin-left: 10px;"  # 设置按钮和标题之间的间距
                     ),
                     
                     conditionalPanel(
                       condition = "input.submit_manual > 0",  # 只有在按钮按下后，才显示
                       plotOutput("manual_plot", height = "500px")%>%withSpinner(color = "#8e81aa")
                     ),
                     
                     
                     
                     splitLayout(cellWidths = c("30%","30%","40%"),
                                 numericInput("manual_plot_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                 numericInput("manual_plot_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                 selectizeInput('manual_plot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                 tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                     div(downloadButton(outputId = "manual_plot_download_plot", label = "Download"), align = "center")
                     
                   ))),
          
        ),
        
        
        ###kegg_analysis----
        tabItem(
          tabName = "kegg_analysis",
          fluidRow(
            shinydashboard::box(
              title = tagList(icon("readme"),"Introduction"), width = 12,solidHeader = T, status = "primary", collapsible = TRUE,
              helpText(div(class="my-custom-div",
                           strong(code("Regional Enrichment Analysis Module:")),
                           "This module allows users to perform",
                           strong("enrichment analysis on differentially expressed genes in a specific cluster,"),
                           "helping users to explore the",
                           strong(code("functional annotations and biological significance of differentially expressed genes in each cluster.")),
                           "Through enrichment analysis, users can obtain an overall overview of biological processes, cellular components, 
                   or molecular functions related to the cluster, thus providing users with a preliminary exploratory analysis."
                           
              ))
            )
            
          ),
          fluidRow(column(12,
                          shinydashboard::box(
                            title = tagList(icon("puzzle-piece"), "Selection"),width = 12,status = "primary",
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("species", "Select Organism:", choices = c("Mouse" = "mmu", "Human" = "hsa"), selected = "mmu"),  # 默认选择小鼠
                                uiOutput("kegg_clusters_ui"), 
                                # selectInput("kegg_clusters",  "Select Clusters:", choices = colnames(sig_gene_symbols), selected = c("cluster.0"), multiple = TRUE),
                                numericInput("kegg_logfc", "Log Fold Change Threshold", value = 0.25, min = 0),
                                numericInput("kegg_pval", "p-value Threshold", value = 0.05, min = 0, max = 1),
                                
                                div(actionButton("submit_kegg", "Run Analysis", icon = icon("play-circle")), align = "center")
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabsetPanel(
                                    tabPanel("GO Enrichment", 
                                             
                                             conditionalPanel(
                                               condition = "input.submit_kegg > 0",  # 只有在按钮按下后，才显示
                                               plotOutput("go_plot")%>%withSpinner(color = "#8e81aa")
                                             ),
                                             
                                             splitLayout(cellWidths = c("30%","30%","40%"),
                                                         numericInput("go_plot_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                                         numericInput("go_plot_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                                         selectizeInput('go_plot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                                         tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                                             div(downloadButton(outputId = "go_plot_download_plot", label = "Download"), align = "center")
                                             
                                    ),
                                    tabPanel("KEGG Enrichment", 
                                             conditionalPanel(
                                               condition = "input.submit_kegg > 0",  # 只有在按钮按下后，才显示
                                               plotOutput("kegg_plot")%>%withSpinner(color = "#8e81aa")
                                             ),
                                             splitLayout(cellWidths = c("30%","30%","40%"),
                                                         numericInput("kegg_plot_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                                         numericInput("kegg_plot_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                                         selectizeInput('kegg_plot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                                         tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                                             div(downloadButton(outputId = "kegg_plot_download_plot", label = "Download"), align = "center")
                                             
                                    )
                                  )
                                  
                                )
                              ),
                              
                              
                              
                            )),
                          
                          
          )
          )
        ),
        ####取子集----
        
        tabItem(
          tabName = "sc",
          fluidRow(
            shinydashboard::box(
              title = tagList(icon("readme"),"Introduction"), width = 12,solidHeader = T, status = "primary", collapsible = TRUE,
              helpText(div(class="my-custom-div",
                           strong(code("Subset Out Anatomical Regions Module:")),
                           strong("This module allows users to subset data in order to focus on specific regions or subpopulations,"),
                           "further supporting in-depth research on these regions or subpopulations. STshiny provides spatial visualization through",
                           strong(code("Spatial DimPlot")),
                           ", helping users to more intuitively view and analyze the data distribution and characteristics of the selected region. Through this module, users can",
                           strong("flexibly filter and explore anatomical regions of interest to optimize the accuracy and effectiveness of subsequent analysis.")
              ))
            )
            
          ),
          fluidRow(column(3,
                          shinydashboard::box(
                            title = tagList(icon("puzzle-piece"), "Selection"),width = 12,status = "primary",
                            
                            uiOutput("image_select_ui"),
                            uiOutput("ident_select_ui"),
                            # selectInput("sc_spatialdimplot","Select Clusters (Identifiers):",multiple = T, choices = levels(seurat_obj),selected = c(1, 2, 3, 4, 6, 7)),
                            
                            helpText(
                              div(class="my-custom-div",
                                  style = "letter-spacing: normal; border: 2px solid #FFA500; padding: 10px; border-radius: 8px; box-shadow: 4px 4px 12px rgba(0, 0, 0, 0.2); background-color: #f9f9f9;",
                                  strong(code("Tips:")),
                                  "Please select the cluster you are interested in, this process also helps 
            to integrate this data with the scRNA-seq dataset. We extract subsets of 
            clusters and then further split them based on exact location. After subsetting, 
            we can visualize specific cells on the full image."
                              )), 
                            
                            div(actionButton("submit_sc", "Start", icon = icon("play-circle")), align = "center")
                            
                          )),
                   
                   column(9,
                          shinydashboard::box(
                            title = tagList(icon("area-chart"),"Spatial DimPlot"),width = 12,status = "primary",collapsible = T,
                            
                            
                            conditionalPanel(
                              condition = "input.submit_sc > 0",  # 只有在按钮按下后，才显示
                              plotOutput("sc_spatialdimplot",width="100%")%>%withSpinner(color = "#8e81aa")
                            ),
                            
                            splitLayout(cellWidths = c("30%","30%","40%"),
                                        numericInput("sc_spatialdimplot_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                        numericInput("sc_spatialdimplot_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                        selectizeInput('sc_spatialdimplot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                        tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                            div(downloadButton(outputId = "sc_spatialdimplot_download_plot", label = "Download"), align = "center"),
                            
                            div(actionButton("submit_sc_next", label = "Next", icon = icon("arrow-right")), 
                                style = "text-align: right;"
                                , class = "disabled")
                            
                            
                          ))
          )
          
        ),
        ##与单细胞集成----
        tabItem(
          tabName = "sc_next",
          fluidRow(
            shinydashboard::box(
              title = tagList(icon("readme"),"Introduction"), width = 12,solidHeader = T, status = "primary", collapsible = TRUE,
              helpText(div(class="my-custom-div",
                           strong(code("Integration With Single-cell Module:")),
                           "As scRNA-seq data continues to increase, users may want to",
                           strong(code("'deconvolve' each spatial voxel to predict its cell type composition.")),
                           "This module allows users to use",
                           strong(code("custom reference datasets")),
                           "and perform label transfer operations. This process outputs the classification probability of 
                   each cell type from the scRNA-seq data for each spatial voxel. STshiny will",
                           strong("add these prediction results as new detection information"),
                           "to the Seurat object being analyzed, thereby providing more cell type composition information for subsequent spatial transcriptomics analysis."
                           
                           
              ))
            )
            
          ),
          
          
          fluidRow(column(12,
                          shinydashboard::box(
                            title = tagList(icon("table"), "Visualization reference dataset"),width = 12,status = "primary",
                            
                            sidebarLayout(
                              sidebarPanel(
                                
                                selectInput("Select_ref_data", label = ("Data source:"), choices = list("Allen Reference","Custom Reference"),selected = "Allen Reference"),
                                uiOutput("RefInputs"),
                                
                                div(actionButton("submit_sc_next_start","Start", icon = icon("play-circle")), align = "center"),
                                
                                
                              ),
                              mainPanel(
                                conditionalPanel(
                                  condition = "input.submit_sc_next_start > 0",  # 只有在按钮按下后，才显示
                                  plotOutput("sc_dimplot",width="100%")%>%withSpinner(color = "#8e81aa")
                                ),
                                
                                
                                splitLayout(cellWidths = c("30%","30%","40%"),
                                            numericInput("sc_dimplot_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                            numericInput("sc_dimplot_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                            selectizeInput('sc_dimplot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                            tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                                div(downloadButton(outputId = "sc_dimplot_download_plot", label = "Download"), align = "center")
                                
                              )
                            ),
                            
                            
                            
                          )
          )),
          
          fluidRow(column(12,
                          shinydashboard::box(
                            title = tagList(icon("table"), "Visualize specific cell types in subsets"),width = 12,status = "primary",
                            
                            sidebarLayout(
                              sidebarPanel(
                                textInput("sc_features", "Enter Features (comma-separated):", value = "L2/3 IT, L4"),
                                numericInput("sc_pt_size_factor", "Point Size Factor:", value = 2, min = 1, max = 10),
                                numericInput("sc_ncol", "Number of Columns:", value = 2, min = 1, max = 5),
                                div(actionButton("submit_sc_continue","Continue", icon = icon("play-circle")), align = "center")
                              ),
                              
                              mainPanel(
                                conditionalPanel(
                                  condition="input.submit_sc_continue > 0",
                                  plotOutput("sc_spatialfeatureplot") %>% withSpinner(color = "#8e81aa"),
                                  
                                ),
                                
                                
                                splitLayout(cellWidths = c("30%","30%","40%"),
                                            numericInput("sc_spatialfeatureplot_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                            numericInput("sc_spatialfeatureplot_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                            selectizeInput('sc_spatialfeatureplot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                            tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                                div(downloadButton(outputId = "sc_spatialfeatureplot_download_plot", label = "Download"), align = "center")
                                
                              )
                            ), 
                            
                            
                          )
          )),
          
          fluidRow(column(12,
                          shinydashboard::box(
                            title = tagList(icon("table"), "Visualization of cell types based on prediction scores"),width = 12,status = "primary",
                            
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("r_metric", "r.metric:", value = 5, min = 1),
                                numericInput("top_n", "Number of top clusters to show:", value = 4, min = 1),
                                numericInput("annot_ncol", "Number of columns in plot:", value = 2, min = 1),
                                numericInput("pt_size_factor", "Point size factor:", value = 2, min = 1),
                                # div(actionButton("run_moransi","Continue", icon = icon("play-circle")), align = "center"),
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
                                    strong(code("How to choose the right r.metric:")),
                                    br(),
                                    tags$div(style = "text-indent: 20px;", 
                                             "Large-scale spatial patterns: If you are interested in spatial 
                                     patterns in large areas, such as expression differences between 
                                     different tissue regions, you can choose a larger r.metric value. 
                                     This will identify spatial correlations across larger areas."),
                                    
                                    tags$div(style = "text-indent: 20px;", 
                                             "Local spatial patterns: If you are interested in subtle differences 
                                     between cell types or small-scale spatial structures, you can choose 
                                     a smaller r.metric value to focus on more detailed local areas.")
                                  )
                                ),div(actionButton("run_moransi","Continue", icon = icon("play-circle")), align = "center"),
                                
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel(
                                    title = "Spatial Plot",value = "moransi_plot", collapsible = TRUE,
                                    conditionalPanel(
                                      condition = "input.run_moransi > 0",  # 只有在按钮按下后，才显示
                                      plotOutput("moransi_plot" ,height = "500px", width = "100%")%>%withSpinner(color = "#8e81aa"),
                                    ),
                                    splitLayout(cellWidths = c("30%","30%","40%"),
                                                numericInput("moransi_plot_PlotWidth", "Width", min = 0, max = 250, value = 10,width = "100%"),
                                                numericInput("moransi_plot_PlotHeight", "Height", min = 0, max = 250, value = 7,width = "100%"),
                                                selectizeInput('moransi_plot_PlotChoices',label = "Format",choices = c(".pdf",".png",".tiff"),selected = ".png",width = "100%"),
                                                tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible;}")))),
                                    div(downloadButton(outputId = "moransi_plot_download_plot", label = "Download"), align = "center")
                                  ),
                                )
                                
                                
                              )
                            ), 
                            
                            
                          )
          )),
          
        ),
        
        ####eight-----------      
        tabItem(
          tabName = "eight",
          fluidRow(
            
            ###1111------
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("puzzle-piece"),"Home"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
                     align = "center",
                     shiny::tags$img(src = "1.jpeg",width="100%")
                   ))),
          
          
          
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("puzzle-piece"),"Quality Control"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
                     align = "center",
                     shiny::tags$img(src = "2.jpeg",width="100%")
                   ))),
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("puzzle-piece"),"Visualize Overall Data"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
                     align = "center",
                     shiny::tags$img(src = "3.jpeg",width="100%")
                   ))),
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("puzzle-piece"),"Data Preprocessing"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
                     align = "center",
                     shiny::tags$img(src = "4.jpeg",width="100%")
                   ))),
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("puzzle-piece"),"Dim Reduction & Clustering"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
                     align = "center",
                     shiny::tags$img(src = "5.jpeg",width="100%")
                   ))),
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("puzzle-piece"),"Find Markers"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
                     align = "center",
                     shiny::tags$img(src = "6.jpeg",width="100%")
                   ))),
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("puzzle-piece"),"Automatic Annotation"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
                     align = "center",
                     shiny::tags$img(src = "7.jpeg",width="100%")
                   ))),
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("puzzle-piece"),"Manual Annotation"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
                     align = "center",
                     shiny::tags$img(src = "8.jpeg",width="100%")
                   ))),
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("puzzle-piece"),"Regional Enrichment Analysis"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
                     align = "center",
                     shiny::tags$img(src = "9.jpeg",width="100%")
                   ))),
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("puzzle-piece"),"Subset Out Anatomical Regions"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
                     align = "center",
                     shiny::tags$img(src = "10.jpeg",width="100%")
                   ))),
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title = tagList(icon("puzzle-piece"),"Integration With Single-cell"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
                     align = "center",
                     shiny::tags$img(src = "11.jpeg",width="100%")
                   ))),
          
          
        ),
        
        
        
        
        
        
        tabItem(
          tabName = "nine",
          
          fluidRow(
            column(12,
                   shinydashboard::box(
                     title =tagList(icon("readme"),"About"), width =NULL, solidHeader = F, status = "primary", collapsible = TRUE,
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
                         strong("Contact"),
                         br(),
                         tags$div(style = "text-indent: 20px;", 
                                  "If you have any technical or collaboration needs, please contact:"),
                         
                         tags$div(style = "text-indent: 20px;", 
                                  "Siwen Xu (siwxu@gdpu.edu.cn)"),
                         tags$div(style = "text-indent: 20px;", 
                                  "Jieru Huang (b14771500308@163.com)")
                       ))))),
        )))),
  
  
  footer=dashboardFooter(
    div(
      # class = "footer",
      # img(src = "STshiny_logo.png", style = "position:absolute;left:250px;bottom:0px;height:120px"),
      img(src = "gdpu_logo.png", style = "position:absolute;bottom:0px;height:50px;right:0px"),
      HTML(paste0(  "</br><p style = ' text-align: center;font-size:1.0em; color: black; line-height: 10%;'>",
                    "<b>Created by</b>: XuLabGDPU | ",
                    "<b>Last update</b>: 29/01/2025",
                    "</p>",
                    "</br><p style = 'text-align: center; font-size:1.0em; color: black; line-height: 10%;'>",
                    "<b>Address</b>: No. 160, Outer Ring East Road, Panyu District, Guangzhou City, Guangdong Province, China | ",
                    "<b>Postcode</b>: 511400",
                    "</p>",
                    "</br><p style = 'text-align: center; font-size:1.0em; line-height: 10%;'> ",
                    # "<a  href = 'https://github.com/lthevenard/dt_exercises'target='_blank'>Github</a> | ",
                    "<a  href = 'https://www.xulabgdpu.org.cn'target='_blank'>XuLabGDPU</a> | ",
                    "<a  href = 'https://xulabgdpu.cpolar.top/STshiny/'target='_blank'>STshiny</a> |",
                    "<a  href = 'https://www.gdpu.edu.cn/'target='_blank'>Guangdong Pharmaceutical University</a> |",
                    "<a href='https://beian.miit.gov.cn/' target='_blank'>黑ICP备2024016624</a>",
                    "</p>"))
    )
  )
)
