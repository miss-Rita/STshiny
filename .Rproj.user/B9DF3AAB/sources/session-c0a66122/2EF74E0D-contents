suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(Matrix))

suppressPackageStartupMessages(library(glmGamPoi))
suppressPackageStartupMessages(library(tidyr))

suppressPackageStartupMessages(library(tictoc))
# remotes::install_github(repo = 'genecell/COSGR')
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(COSG))

# suppressPackageStartupMessages(library(hdf5r))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(future))
suppressPackageStartupMessages(library(doFuture))
suppressPackageStartupMessages(library(ape))
suppressPackageStartupMessages(library(Rfast2))
# 查看当前的并行计划
# nbrOfWorkers()
# # 查看可用的核心数
# future::availableCores()
# 
# detectCores()
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(SingleR))
suppressPackageStartupMessages(library(celldex))# 提供参考数据库
suppressPackageStartupMessages(library(SummarizedExperiment))
suppressPackageStartupMessages(library(ggsci))

suppressPackageStartupMessages(library(BiocParallel))

suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(shinybusy))
suppressPackageStartupMessages(library(shinyWidgets))

suppressPackageStartupMessages(library(plyr))

suppressPackageStartupMessages(library(Seurat)) #报错
suppressPackageStartupMessages(library(irlba)) #报错

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))

suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(ggplot2))


suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(patchwork))

suppressPackageStartupMessages(library(shinyalert))

suppressPackageStartupMessages(library(EnhancedVolcano))


suppressPackageStartupMessages(library(scater))


#富集分析
suppressPackageStartupMessages(library(ggprism))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(org.Mm.eg.db))
suppressPackageStartupMessages(library(org.Hs.eg.db))
suppressPackageStartupMessages(library(clusterProfiler))


options(shiny.maxRequestSize = 100000000000 * 1024^2)
# options(future.globals.maxSize = 200 * 1024^3)
# 设置并行后端（使用 4 个核心）
bp <- MulticoreParam(workers = 20)  # 使用 20 个核心
options(BioC_parallel = bp)



mytheme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#8376a3"
  ),
  fresh::adminlte_sidebar(
    width = "250px",
    dark_bg = "#2b2c34",
    dark_hover_color = "#FFF",
    # dark_color = "#edebf1"
    dark_color = "#FFF"
  ),
  fresh::adminlte_global(
    info_box_bg = "#a89fc4"
  )
)


Purples <- c("#6f6190","#78619b","#8376a3","#8e81aa","#988db2","#a399ba","#ada5c2","#b8b0ca",
             "#c3bcd2","#cdc8da","#d8d3e2","#e2dfea","#edebf1","#f7f7f9")

#将锁定文件更新到当前安装的版本
# renv::snapshot()

#降级到锁定文件中的版本
# renv::restore()

actionBttnParams <- list(
  size = "md",
  color = "primary",
  style = "fill",
  block = TRUE
)
# library(devtools)
# install_version("future", version = "1.34.0")
