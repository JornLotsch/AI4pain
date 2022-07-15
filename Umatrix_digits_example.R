# Umatrix digits eample
######################### Get data #####################################################################
pfad_o <- "/home/joern/Aktuell/AI4pain/"
pfad_u <- "09Originale/"
pfad_u1 <- "08AnalyseProgramme/"
setwd("~/Aktuell/AI4pain/08AnalyseProgramme")
# Fucntions

toRange <- function(data, lower, upper) {
  data <- as.matrix(data)
  if (lower == upper) {
    error("interval width can not be 0!")
  }
  if (lower > upper) {
    temp <- upper
    upper <- lower
    lower <- upper
  }
  range <- upper - lower
  n <- dim(data)[1]
  d <- dim(data)[2]
  if ((n == 1) & (d > 1)) {
    data <- t(data)
    wasRowVector <- 1
  } else {
    wasRowVector <- 0
  }
  nRow <- dim(data)[1]
  nCol <- dim(data)[2]
  min <- apply(data, 2, min, na.rm = TRUE)
  min <- matrix(min, nRow, nCol, byrow = TRUE)
  max <- apply(data, 2, max, na.rm = TRUE)
  max <- matrix(max, nRow, nCol, byrow = TRUE)
  range <- max - min
  range[range == 0] <- 1
  scaleData <- (data - min) / range
  scaleData <- lower + scaleData * (upper - lower)
  if (wasRowVector == 1) {
    scaleData <- t(scaleData)
  }
  return(scaleData)
}

toPercent <- function(data) {
  scaled <- toRange(data, 0, 100)
  return(scaled)
}

# data
DigitsExample <- read.csv(paste0(pfad_o, pfad_u, "DigitsExample.csv"))
Cls <- DigitsExample$digits
DigitsExampleData <- subset(DigitsExample, select = -c(digits))

library(Umatrix)
# digits_Umx <- Umatrix::iEsomTrain(Data = as.matrix(toPercent(DigitsExampleData)), Cls = Cls)
# WriteBM(FileName = "digits_Umx.bm", BestMatches = digits_Umx$BestMatches)
# WriteUMX(FileName = "digits_Umx.umx", UMatrix = digits_Umx$Umatrix)
# WriteWTS(FileName = "digits_Umx.wts", wts = digits_Umx$Weights, Lines = 75, Columns = 120)

BestMatches <- ReadBM(FileName = "digits_Umx.bm", InDirectory = paste0(pfad_o, pfad_u1))
UMatrix <- ReadUMX(FileName = "digits_Umx.umx", InDirectory = paste0(pfad_o, pfad_u1))
Weigths <- ReadWTS(FileName = "digits_Umx.wts", InDirectory = paste0(pfad_o, pfad_u1))

# digits_Imx <- Umatrix::iUmapIsland(Umatrix = UMatrix, BestMatches = BestMatches$BestMatches, Cls = Cls)
# WriteIMX("digits_Imx.imx", digits_Imx$Imx)
Imx <- ReadIMX("digits_Imx.imx", InDirectory = paste0(pfad_o, pfad_u1))

# digits_Cls <- Umatrix::iClassification(Umatrix = UMatrix, BestMatches = BestMatches$BestMatches, Cls = Cls+1, Imx = Imx)
# WriteCLS("digits_Cls.cls", digits_Cls$Cls)
# digits_ClsUmx <- ReadCLS("digits_Cls.cls", InDirectory = paste0(pfad_o, pfad_umx))
# digits_ClsUmx$Cls[digits_ClsUmx$Cls > 1] <- 2
# table(digits_ClsUmx$Cls)

ClsColors <- c(
  rainbow(10)[1],
  rainbow(10)[2],
  rainbow(10)[3],
  rainbow(10)[4],
  rainbow(10)[5],
  rainbow(10)[6],
  rainbow(10)[7],
  rainbow(10)[8],
  rainbow(10)[9],
  "black"
)

library(ggplot2)
pUmatrix <- Umatrix::plotMatrix(
  Matrix = UMatrix, BestMatches = BestMatches$BestMatches, Cls = Cls + 1, Imx = Imx,
  TransparentContours = T, BmSize = 3, RemoveOcean = T, ClsColors = ClsColors
) + theme_bw() + guides(fill = "none", color = "none") + ylim(0, 100)

Umatrix::showMatrix3D(
  Matrix = UMatrix, BestMatches = BestMatches$BestMatches, Cls = Cls + 1, ClsColors = ClsColors, Imx = Imx,
  BmSize = 1, RemoveOcean = T
)
library(rgl)
snapshot3d("digits_Umx_3d.png", "png")


#################### Plost
library(ggplot2)

Digit10 <- DigitsExampleData[1:10, ]
names(Digit10)
nDigits <- nrow(Digit10)
Digit10$Digit <- c(0:9)
Digit10_long <- reshape2::melt(Digit10, id.vars = "Digit")
Digit10_long$y <- rep(1:8, each = nDigits * 8)
Digit10_long$x <- rep(1:8, times = 8, each = nDigits)

pDigits09 <- ggplot(data = Digit10_long, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  facet_wrap(~Digit) +
  scale_y_continuous(trans = "reverse") +
  theme_bw() +
  theme(legend.position = "right", legend.title = element_text(angle = 90), legend.title.align = 1, strip.background = element_rect(fill = "cornsilk"), strip.text = element_text(colour = "black")) +
  scale_fill_distiller(type = "seq", direction = 1, palette = "Greys") +
  labs(x = "Pixel", y = "Letter example #", fill = "Grey value")


DigitsExampleDataHM <- DigitsExampleData
dim(DigitsExampleDataHM)
DigitsExampleDataHM$Number <- as.numeric(rownames(DigitsExampleDataHM))
DigitsExampleDataHM_long <- reshape2::melt(DigitsExampleDataHM, id.vars = c("Number"))

pDigitsAll <- ggplot(data = DigitsExampleDataHM_long, aes(y = Number, x = variable, fill = value)) +
  geom_tile() +
  theme_bw() +
  scale_y_continuous(trans = "reverse") +
  scale_fill_distiller(type = "seq", direction = 1, palette = "Greys") +
  theme(legend.position = "right", legend.title = element_text(angle = 90), legend.title.align = 1, axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Pixel", y = "Letter example #", fill = "Grey value")


DigitsExampleData_PCAproj <- read.csv(paste0(pfad_o, pfad_u, "digits_PCA_projected.csv"))
DigitsExampleData_PCAproj <- DigitsExampleData_PCAproj[, 1:2]
DigitsExampleData_PCAproj$Cls <- Cls
pDigitsAll_PCA <- ggplot(data = DigitsExampleData_PCAproj, aes(x = X0, y = X1, color = factor(Cls))) +
  geom_point() +
  theme_bw() +
  ylim(-31, 31) +
  scale_color_manual(values = ClsColors) +
  theme(legend.position = "right") +
  labs(x = "PC1", y = "PC2", color = "Digits")

cowplot::plot_grid(cowplot::plot_grid(
  plotlist = list(
    pDigits09 + ggtitle("First 9 sample digits, each scanned as 8 x 8 pixels"),
    pDigitsAll + ggtitle("Complete digits data (1797 digits, each scanned as 8 x 8 pixels)")
  ),
  nrow = 1, labels = LETTERS[1:2]
),
cowplot::plot_grid(plotlist = list(
  pDigitsAll_PCA + ggtitle("PCA projection"),
  pUmatrix + ggtitle("SOM projection")
), labels = LETTERS[3:4], align = "hv", axis = "bt"),
nrow = 2
)
