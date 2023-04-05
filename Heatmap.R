library(ggplot2)
library(ggrepel)
library(dplyr)
library("readxl")
library("gplots")
library("hrbrthemes")
library("viridis")


d_j <- read_excel('juvenilechanged.xlsx')
d_a <- read_excel('adultchanged.xlsx')

d_j <- subset(d_j, select = c(4,6,7,10) )
d_j <- subset(d_j, d_j$Genes != "NaN")

d_a = subset(d_a, select = c(4,6,7,10) )
d_a <- subset(d_a, d_a$Genes != "NaN")

data <- merge(d_j,d_a,by="Genes")
data[12,1] <- "fthl30"
data[34,1] <- "timm17a"

comp <- read.csv("ComparisonNew_IB_Heatmap.csv")
comp <- subset(comp, select=c(2,3,4))


df <- merge(data,comp)

df1 <- subset(df, select=c(1,2,3,4,8,9))
df1$Age <- "Juvenile"
colnames(df1) <- c("Genes", "Log2", "Pvalue", "Qvalue", "Type","Function","Age")

df2 <- subset(df, select=c(1,5,6,7,8,9))
df2$Age <- "Adult"
colnames(df2) <- c("Genes", "Log2", "Pvalue", "Qvalue", "Type","Function","Age")

df <- rbind(df1,df2)


#to use heatmap(), convert to a numeric matrix


matrix <- matrix(1:72, nrow = 36, ncol = 2)
rownames(matrix) <- data$Genes
colnames(matrix) <- c("Juvenile","Adult")

matrix[,1] <- df1$Log2
matrix[,2] <- df2$Log2

library("ComplexHeatmap")
split <- factor(df1$Function, levels = c("RNA DNA Processing","Potassium Channel", "Calcium Binding Protein", "ECM Component","Cell Homeostasis","Cell Cycle","Mitochondrial","Metabolism/Catabolism/Stress","Transport","Other"))

ht <- Heatmap(matrix, show_column_dend =F, show_row_dend = F,
              split=split,
              cluster_row_slices = F,
              name = "Fold Change", #title of legend
              row_names_gp = gpar(fontsize = 10), 
              row_title_gp = gpar(fontsize = 10),
              row_title_rot = 0,
              column_names_rot = 0, column_names_gp = gpar(fontsize=20), column_names_centered = T
)
ht



















