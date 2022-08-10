#Umatrix digits eample
######################### Get data #####################################################################
pfad_o <- "/home/joern/Aktuell/AI4pain/"
pfad_u <- "09Originale/"
pfad_u1 <- "08AnalyseProgramme/"
setwd("~/Aktuell/AI4pain/08AnalyseProgramme")
# Fucntions

toRange <- function (data, lower, upper) 
{
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
  }
  else {
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
  scaleData <- (data - min)/range
  scaleData <- lower + scaleData * (upper - lower)
  if (wasRowVector == 1) {
    scaleData = t(scaleData)
  }
  return(scaleData)
}

toPercent <- function (data) 
{
  scaled = toRange(data, 0, 100)
  return(scaled)
}

#data
DigitsExample <- read.csv(paste0(pfad_o, pfad_u, "DigitsExample.csv"))
Cls <- DigitsExample$digits
table(Cls)
DigitsExampleData <- subset(DigitsExample, select = -c(digits))
# DigitsExampleData <- read.csv(paste0(pfad_o, pfad_u, "digits_PCA_minmax.csv"))
# DigitsExampleData <- read.csv(paste0(pfad_o, pfad_u, "digits_PCA_projected.csv"))
# DigitsExampleData <- DigitsExampleData[,2:30]
library(Umatrix)
# digits_Umx <- Umatrix::iEsomTrain(Data = as.matrix(toPercent(DigitsExampleData)), Cls = Cls)
# WriteBM(FileName = "digits_Umx.bm", BestMatches = digits_Umx$BestMatches)
# WriteUMX(FileName = "digits_Umx.umx", UMatrix = digits_Umx$Umatrix)
# WriteWTS(FileName = "digits_Umx.wts", wts = digits_Umx$Weights, Lines = 75, Columns = 120)

BestMatches = ReadBM(FileName = "digits_Umx.bm", InDirectory = paste0(pfad_o, pfad_u1))
UMatrix = ReadUMX(FileName = "digits_Umx.umx", InDirectory = paste0(pfad_o, pfad_u1))
Weigths <- ReadWTS(FileName = "digits_Umx.wts", InDirectory = paste0(pfad_o, pfad_u1))

digits_Imx <- Umatrix::iUmapIsland(Umatrix = UMatrix, BestMatches = BestMatches$BestMatches, Cls = Cls)
# WriteIMX("digits_Imx.imx", digits_Imx$Imx)
Imx <- ReadIMX("digits_Imx.imx", InDirectory = paste0(pfad_o, pfad_u1))

digits_Cls <- Umatrix::iClassification(Umatrix = UMatrix, BestMatches = BestMatches$BestMatches, Cls = Cls, Imx = Imx)
table(digits_Cls$Cls)
WriteCLS("digits_Cls.cls", digits_Cls$Cls)
digits_ClsUmx <- ReadCLS("digits_Cls.cls", InDirectory = paste0(pfad_o, pfad_u1))
# digits_ClsUmx$Cls[digits_ClsUmx$Cls > 1] <- 2
table(digits_ClsUmx$Cls)

Umatrix::plotMatrix( Matrix = UMatrix, BestMatches = BestMatches$BestMatches, Cls = digits_ClsUmx$Cls, Imx = Imx,
                     TransparentContours = T, BmSize = 3, RemoveOcean = T, ClsColors = rainbow(12))
barplot(1:10, col=rainbow(10))

Umatrix::showMatrix3D( Matrix = UMatrix, BestMatches = BestMatches$BestMatches, Cls = Cls, ClsColors = rainbow(10), Imx = Imx, 
                       BmSize = 1, RemoveOcean = T)
library(rgl)
snapshot3d("digits_Umx_3d.png","png")


fisher.test(table(digits_ClsUmx$Cls, paclitaxel_uct_imputed$Probe.1.oder.2))
fisher.test(table(digits_ClsUmx$Cls, paclitaxel_uct_imputed$Neuropathie))
fisher.test(table(digits_ClsUmx$Cls, silclust))

# write.csv(cbind.data.frame(Probe12 = paclitaxel_uct_imputed_log$Probe.1.oder.2, 
#                            Neuropaty = paclitaxel_uct_imputed_log$Neuropathie,
#                            Clusters = silclust), "dfdigits_Classes.csv")
# write.csv(rownames(paclitaxel_uct_imputed_log), "Index.csv")
