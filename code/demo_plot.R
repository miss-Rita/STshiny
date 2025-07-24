library(ggplot2)
library(dplyr)
library(tidyr)

# 假设你有一个数据框df，其中包含了细胞类型（cellType）、基因名称（gene）和表达水平（expressionLevel）
# 例如：df <- data.frame(cellType = ..., gene = ..., expressionLevel = ...)


obj_demo111 = Load10X_Spatial(data.dir = "/users/jierhuang/jierhuang/shiny/spatial",
                      filename = "filtered_feature_bc_matrix_008.h5",
                      assay = "Spatial", 
                      slice = "slice1")
grep(pattern = "^MT-", rownames(obj), value = TRUE)











# 将数据转换为长格式
df_long <- df %>%
  pivot_longer(cols = starts_with("LYZ"), names_to = "gene", values_to = "expressionLevel")

# 绘制小提琴图
ggplot(df_long, aes(x = cellType, y = expressionLevel, fill = cellType)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  facet_wrap(~gene, scales = "free_y") + # 为每个基因创建一个分面
  theme_minimal() +
  labs(x = "Cell Type", y = "Expression Level", fill = "Cell Type", title = "Gene Expression by Cell Type")






SpatialFeaturePlot(brain_nomal, features = genes,pt.size.factor = pt_size_input)
SpatialFeaturePlot(object = brain_nomal, features = rownames(de_markers)[1:3], alpha = c(0.1, 1), ncol = 3)
