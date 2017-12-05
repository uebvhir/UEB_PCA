#segment.size: Grosor de las líneas de las etiquetas (segment.size)
#size: se controla el tamaño de las etiquetas (size)
#colores: vector con el color de cada factor de forma ordenada
#datos: matriz con los datos
#labels: etiquetas de cada muestra
#factor: columna del targets con los grupos
#scale: si es necesario o no escalar los datos (TRUE/FALSE)

library(ggplot2)
library(ggrepel)

plotPCA3 <- function (datos, labels, factor,title,scale,colores,size = 1.5, segment.size =0.25) {
  data <- prcomp(t(datos),scale=scale)
  #ajustos del gràfic
  dataDf <- data.frame(data$x)
  Grupo <- factor
  loads <- round(data$sdev^2/sum(data$sdev^2)*100,1)
  # the graphic
  p1 <- ggplot(dataDf,aes(x=PC1, y=PC2)) +
    theme_classic() +
    geom_hline(yintercept = 0, color = "gray70") +
    geom_vline(xintercept = 0, color = "gray70") +
    geom_point(aes(color = Grupo), alpha = 0.55, size = 3) +
    coord_cartesian(xlim = c(min(data$x[,1])-5,max(data$x[,1])+5))
  # the graphic with ggrepel
  p1 + geom_text_repel(aes(y = PC2 + 0.25, label = labels),segment.size = 0.25, size = size) + 
    labs(x = c(paste("PC1",loads[1],"%")),y=c(paste("PC2",loads[2],"%"))) +  
    ggtitle(paste("Principal Component Analysis for:",title,sep=" "))+ 
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values=colores)
  }
