library("readxl")

d_j <- read_excel('juvenilechanged.xlsx')
d_a <- read_excel('adultchanged.xlsx')

d_j <- subset(d_j, select = c(4,6,7,10) )
d_j <- subset(d_j, d_j$Genes != "NaN")

d_a = subset(d_a, select = c(4,6,7,10) )
d_a <- subset(d_a, d_a$Genes != "NaN")

data <- merge(d_j,d_a,by="Genes")

#juvenilefirst, adultsecond

uu <- filter(data, data[2]>= 0.58 & data[5]>= 0.58)
dd <- filter(data, data[2] <= -0.58 & data[5] <= -0.58)
ud <- filter(data, data[2]>= 0.58 & data[5] <= -0.58)
du <- filter(data, data[2]<= -0.58 & data[5]>= 0.58)



uu$Type <- "uu"
dd$Type <- "dd"
du$Type <- "du"
ud$Type <- "ud"

data <- rbind(uu,dd,ud,du)

Comparison <- subset(data, select=c(1,8))

write.csv(Comparison, "Comparison.csv")


x <- list(
  Juvenile = d_j$Genes, 
  Adults = d_a$Genes
)


library(ggvenn)
ggvenn(
  x, 
  fill_color = c( "#33FF99","#6666FF"),
  stroke_size = 0.5, set_name_size = 8, text_size = 8
)

print(dd$Genes)
print(uu$Genes)
print(ud$Genes)
print(du$Genes)


