#datos = no se resta la primera columna
#factor = factor por el que se colorearán las muestras
#title = pequeño título que se añade a "Principal Component Analysis for: "
#colores <- vector con el color de cada factor de forma ordenada
#size = se controla el tamaño de las etiquetas (size=1.5)
#glineas = se controla el grosor de las líneas de las etiquetas (segment.size)
#labels = nom de les mostres. En cas de no escriure res, només es veuran els punts, no els noms
#scale = si quieres escalar los datos o no
#ellipse = si vols que dibuixi l'ellipse del grup de les mostres

require(ggplot2)
require(ggrepel)
require(vegan)

plotPCA5 <- function (datos, factor, scale, title, colores, size = 1.5, glineas = 0.25, labels = NULL, ellipse = TRUE) {
  data <- prcomp(t(datos), scale = scale)
  #ajustos del gràfic
  dataDf <- data.frame(data$x)
  Group <- factor
  loads <- round(data$sdev^2/sum(data$sdev^2)*100, 1)
  
  # the graphic
  p1 <- ggplot(dataDf, aes(x = PC1, y = PC2, group = Group, color = Group)) +
    theme_classic() +
    geom_hline(yintercept = 0, color = "gray70") +
    geom_vline(xintercept = 0, color = "gray70") +
    geom_point(aes(color = Group), alpha = 0.55, size = 3) +
    coord_cartesian(xlim = c(min(data$x[, 1]) - 5, max(data$x[, 1]) + 5)) +
    scale_fill_discrete(name = "Group")
  
  # the graphic with ggrepel, depending on if we gave labels or not
  if(!is.null(labels)){
    if(ellipse){
    p1 + labs(x = c(paste("PC1", loads[1], "%")), y = c(paste("PC2", loads[2], "%"))) +
      ggtitle(paste("Principal Component Analysis for: ", title, sep = " ")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_color_manual(values = colores) +
      geom_text_repel(aes(y = PC2 + 0.25, label = labels), segment.size = 0.25, size = size) +
        stat_ellipse()
    } else {
      p1 + labs(x = c(paste("PC1", loads[1], "%")), y = c(paste("PC2", loads[2], "%"))) +
      ggtitle(paste("Principal Component Analysis for: ", title, sep = " ")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_color_manual(values = colores) +
      geom_text_repel(aes(y = PC2 + 0.25, label = labels), segment.size = 0.25, size = size) 
    }
  } else {
     if(ellipse){
    p1 + labs(x = c(paste("PC1", loads[1], "%")), y = c(paste("PC2", loads[2], "%"))) +
      ggtitle(paste("Principal Component Analysis for: ", title, sep = " ")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_color_manual(values = colores) +
         stat_ellipse()
     } else {
        p1 + labs(x = c(paste("PC1", loads[1], "%")), y = c(paste("PC2", loads[2], "%"))) +
      ggtitle(paste("Principal Component Analysis for: ", title, sep = " ")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_color_manual(values = colores)
  }
  
  }
}
