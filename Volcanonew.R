#Required libraries 
library(ggplot2)
library(ggrepel)
library(dplyr)
library("readxl")

#Read in data
d1 <- read_excel('juvenilefinal.xlsx')
d2 <- read_excel('adultfull.xlsx')


#Rename the columns 

colnames(d1)[4] = "log2FoldChange"
colnames(d1)[6] = "p-value"
d1$"p-value" <- -log10(d1$"p-value")
colnames(d1)[6] = "-log10p"


colnames(d2)[4] = "log2FoldChange"
colnames(d2)[6] = "p-value"
d2$"p-value" <- -log10(d2$"p-value")
colnames(d2)[6] = "-log10p"


d1$Expression <- "Unchanged"
d1$Expression[d1$log2FoldChange>=.58 & d1$Qvalue <= 0.05] <- "Juvenile Up Regulated"
d1$Expression[d1$log2FoldChange<= -.58 & d1$Qvalue <= .05] <- "Juvenile Down Regulated"

d2$Expression <- "Unchanged"
d2$Expression[d2$log2FoldChange>=.58 & d2$Qvalue <= 0.05] <- "Adult Up Regulated"
d2$Expression[d2$log2FoldChange<= -.58 & d2$Qvalue <= .05] <- "Adult Down Regulated"



#Create a size scheme
d1$Size <- 0

d1$Size[d1$log2FoldChange>=.58 & d1$Qvalue <= 0.05 ] <- d1$`Absolute AVG Log2 Ratio`[d1$log2FoldChange>=.58 & d1$Qvalue <= 0.05]
d1$Size[d1$log2FoldChange<= -.58 & d1$Qvalue <= .05] <- d1$`Absolute AVG Log2 Ratio`[d1$log2FoldChange<= -.58 & d1$Qvalue <= .05]


d2$Size <- 0

d2$Size[d2$log2FoldChange>=.58 & d2$Qvalue <= 0.05 ] <- d2$`Absolute AVG Log2 Ratio`[d2$log2FoldChange>=.58 & d2$Qvalue <= 0.05]
d2$Size[d2$log2FoldChange<= -.58 & d2$Qvalue <= .05] <- d2$`Absolute AVG Log2 Ratio`[d2$log2FoldChange<= -.58 & d2$Qvalue <= .05]



#Choose genes to label

d1up <- filter(d1,Expression=="Juvenile Up Regulated")
d1down <- filter(d1, Expression =="Juvenile Down Regulated")

d1up <- d1up[order(-d1up$log2FoldChange),]
d1up10 <- d1up[1:10,]

d1down <- d1down[order(d1down$log2FoldChange),]
d1down10 <- d1down[1:10,]


d2up <- filter(d2,Expression=="Adult Up Regulated")
d2down <- filter(d2, Expression =="Adult Down Regulated")

d2up <- d2up[order(-d2up$log2FoldChange),]
d2up10 <- d2up[1:10,]

d2down <- d2down[order(d2down$log2FoldChange),]
d2down10 <- d2down[1:10,]




d1$label_d1 <- NA

for (i in 1:6408){
  for (j in 1:10){
     if (d1$Genes[i] == d1up10$Genes[j]){
       d1$label_d1[i] <- d1$Genes[i]
       
     }}}

for (i in 1:6408){
  for (j in 1:10){
    if (d1$Genes[i] == d1down10$Genes[j]){
      d1$label_d1[i] <- d1$Genes[i]
      
    }}}


d2$label_d2 <- NA

for (i in 1:6408){
  for (j in 1:10){
    if (d2$Genes[i] == d2up10$Genes[j]){
      d2$label_d2[i] <- d2$Genes[i]
      
    }}}

for (i in 1:6408){
  for (j in 1:10){
    if (d2$Genes[i] == d2down10$Genes[j]){
      d2$label_d2[i] <- d2$Genes[i]
      
    }}}
        
colnames(d1)[23] <- 'lab'
colnames(d2)[23] <- 'lab'



#merge data for plotting

d2$`-log10p` <- -d2$`-log10p`

df <- rbind(d1, d2)

df$Color <- 'gray'
df$Color[df$log2FoldChange>=.58 & df$Qvalue <= 0.05] <- 'black'
df$Color[df$log2FoldChange<= -.58 & df$Qvalue <= .05] <- 'black'

df$Size2 <- df$Size
for (i in 1:13008){
if (df$Size[i] ==0){
  df$Size2[i] == 0
} else {df$Size2[i] <- df$Size[i] +2}}

#Remove/change names
colnames(df)[21] <- "ProteinExpression"
df$lab <- NA

#Create initial plot
v <- ggplot(data=df, aes(x=log2FoldChange, y =`-log10p`, col=ProteinExpression, label=lab))+
  geom_point(aes(size = Size),show.legend = F)+
  geom_point(pch=21, colour=df$Color, size=df$Size2 )+
  theme_minimal()+
  geom_text_repel(max.overlaps = 1000, key_glyph = 'rect')+
  geom_vline(xintercept = c(-.58,.58))+
  geom_hline(yintercept = c(-1.64, 1.44))+
  scale_color_manual(values=c( 'aquamarine2', 'aquamarine4', '#CCCCFF', '#6600CC','gray'),name="Protein Expression", breaks=c('Juvenile Down Regulated', 'Juvenile Up Regulated', 'Adult Down Regulated','Adult Up Regulated', 'Unchanged'))+ #1st is adult down, 2nd is adult up, 3rd is juvenile down, 4th is juvenile up
  theme(text=element_text(size=15))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
     legend.text = element_text(colour="black", size=15), legend.position = c(.95, .95),
    legend.justification = c("right", "top"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


v


