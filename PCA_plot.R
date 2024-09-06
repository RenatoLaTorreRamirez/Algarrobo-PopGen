## Packages
library(ggplot2)
library(viridis)

## Read PCAngsd output
cov <- as.matrix(read.table("output.cov"))

## Read metadata
sampleInfo <- read.csv("metadata.csv", sep =",")

sampleInfo$Sample.Code

sampleInfo[2,]

row.names(cov) <- sampleInfo$Sample.Code
colnames(cov) <- sampleInfo$Sample.Code


cov.pca <- prcomp(cov)

v <- cov.pca$sdev^2/sum(cov.pca$sdev^2)*100

plot(v, xlab ="PC", ylab ="variance explained [%]", pch=16, col="blue")

s <- cbind(sampleInfo, cov.pca$x)

#si quiero guardar el pdf con los resultados
#pdf("2023-11-27_PCA_Osinensis.pdf")

ggplot(data=s, aes(x=PC1, y=PC2, color=Haplotype)) +
  geom_point() + 
  # use viridis colors
  #scale_color_viridis_d() +
  # use manual colors (need to define a color vector before then)
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  xlab(paste("PC1 (", round(v[1], 2), " %)")) +
  ylab(paste("PC2 (", round(v[2], 2), " %)")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour= "black"),
        legend.justification = c(0, 1))

ggplot(data=s, aes(x=PC1, y=PC2, color=Locality)) +
  geom_point() + 
  # use viridis colors
  scale_color_viridis_d() +
  # use manual colors (need to define a color vector before then)
  #scale_color_manual(values = c("red", "blue", "green", "orange")) +
  xlab(paste("PC1 (", round(v[1], 2), " %)")) +
  ylab(paste("PC2 (", round(v[2], 2), " %)")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour= "black"),
        legend.justification = c(0, 1))

ggplot(data=s, aes(x=PC1, y=PC2, color=Locality, shape=Haplotype)) +
  geom_point() + 
  # use viridis colors
  #scale_color_viridis_d() +
  # use manual colors (need to define a color vector before then)
  scale_color_manual(values = c("red", "blue", "green", "orange", "black")) +
  xlab(paste("PC1 (", round(v[1], 2), " %)")) +
  ylab(paste("PC2 (", round(v[2], 2), " %)")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour= "black"),
        legend.justification = c(0, 1))
#cerrar el pdf que ya se creo
dev.off()
