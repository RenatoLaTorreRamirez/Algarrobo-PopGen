## Packages
library(ggplot2)
library(viridis)

## Read PCAngsd output
cov <- as.matrix(read.table("output.cov"))

## Read metadata
sampleInfo <- read.csv("metadata.csv", sep =",")
remove <- c("Alg3", "Alg62", "Alg63", "Alg64",
            "Alg65", "Alg71", "Alg73", "Alg80",
            "Alg88", "Alg82", "Alg94", "Alg95", "Alg96")
filt_sampleInfo <- sampleInfo[-which(sampleInfo$Sample %in% remove),]
row.names(cov) <- filt_sampleInfo$Sample
colnames(cov) <- filt_sampleInfo$Sample
cov.pca <- prcomp(cov)
v <- cov.pca$sdev^2/sum(cov.pca$sdev^2)*100
s <- cbind(filt_sampleInfo, cov.pca$x)

pdf("2024-01_PopulationGenomics/2024-09-09_NewResults/2024-09-09_PCA_Pomac.pdf")
plot(v, xlab ="PC", ylab ="variance explained [%]", pch=16, col="blue")
ggplot(data=s, aes(x=PC1, y=PC2)) +
  geom_point() + 
  # use viridis colors
  scale_color_viridis_d() +
  # use manual colors (need to define a color vector before then)
  xlab(paste("PC1 (", round(v[1], 2), " %)")) +
  ylab(paste("PC2 (", round(v[2], 2), " %)")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour= "black"),
        legend.justification = c(0, 1))
dev.off()
