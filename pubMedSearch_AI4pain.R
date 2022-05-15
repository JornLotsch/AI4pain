# Paths
pfad_o <- "/home/joern/Aktuell/AI4pain/"
pfad_u <- "09Originale/"
pfad_u1 <- "08AnalyseProgramme/"

setwd(paste0(pfad_o, pfad_u1))

# https://www.r-bloggers.com/2015/12/how-to-search-pubmed-with-rismed-package-in-r/

my_query <-
  "((machine-learning) OR (machine learning) OR (machine-learned) OR (machine learned) OR (artificial intelligence) OR (explainable AI) OR (explainable artificial intelligence) OR (XAI) OR (knowledge discovery) OR (deep learning)) AND ((chronic) OR (persisting) OR (persistent) OR (lasting) OR (neuropathic) OR (nociceptive) OR (nociplastic) OR (mixed) OR (neurogenic) OR (back) OR (neck) OR (migraine) OR (arthritis) OR (osteoart*) OR (joint) OR (rheumatic) OR (orchialgia) OR (inflammatory) OR (musculoskeletal) OR (muscle) OR (visceral) OR (widespread) OR (somatoform) OR (fibromyalgia) OR (cancer) OR (postoperative) OR (perioperative)) AND ((pain) OR (painful) OR (analgesi*)) NOT (review[Publication Type])"

######################### PubMed query ###################################
# https://davetang.org/muse/2013/10/31/querying-pubmed-using-r/
library(RISmed)

AI4painPubYear <- array()
AI4painAbstracts_0 <- list()
AI4painAbstracts <- list()
AI4painPMIDs <- list()
x <- 1
for (i in 1945:2022) {
  Sys.sleep(1)
  r <-
    EUtilsSummary(
      my_query,
      type = "esearch",
      db = "pubmed",
      mindate = i,
      maxdate = i
    )
  # AI4painPubYear[x] <- QueryCount(r)
  r1 <- EUtilsGet(r)
  AI4painAbstracts_0 <- AbstractText(r1)
  whichIDs_AI <-
    grep(
      "machine-learning|machine learning|machine-learned|machine learned|artificial intelligence|explainable ai|explainable artificial intelligence|xai|knowledge discovery|deep learning",
      tolower(AI4painAbstracts_0)
    )
  whichIDs_paintypes <-
    grep(
      "chronic|persisting|persistent|lasting|neuropathic|nociceptive|nociplastic|mixed|neurogenic|back|neck|migraine|arthritis|osteoart|joint|rheumatic|orchialgia|inflammatory|musculoskeletal|muscle|visceral|widespread|somatoform|fibromyalgia|cancer|postoperative|perioperative",
      tolower(AI4painAbstracts_0)
    )
  whichIDs_pain <-
    grep("pain|analgesi", tolower(AI4painAbstracts_0))
  whichIDs <- intersect(whichIDs_AI, whichIDs_paintypes)
  whichIDs <- intersect(whichIDs, whichIDs_pain)
  AI4painPubYear[x] <- length(whichIDs)
  AI4painAbstracts <-
    append(AI4painAbstracts, AbstractText(r1)[whichIDs])
  resFetch <- EUtilsGet(r, type = "efetch", db = "pubmed")
  PubPMID <- resFetch@PMID
  AI4painPMIDs <- append(AI4painPMIDs, PubPMID[whichIDs])
  x <- x + 1
}
names(AI4painPubYear) <- 1945:2022
max(AI4painPubYear)
sum(AI4painPubYear)
YearsWithAI4painPublications1 <-
  as.integer(names(AI4painPubYear)[AI4painPubYear > 0])

barplot(AI4painPubYear, las = 2, main = "Number of PubMed articles on AI and machine learning in pain-related data")

r2 <- EUtilsSummary(
  my_query,
  type = "esearch",
  db = "pubmed",
  mindate = as.integer(names(AI4painPubYear[min(which(AI4painPubYear > 0))])),
  maxdate = as.integer(names(AI4painPubYear[min(which(AI4painPubYear > 0))]))
)
resFetch <- EUtilsGet(r2, type = "efetch", db = "pubmed")
PubYear <- resFetch@YearPubDate
length(PubYear)
PubPMID <- resFetch@PMID
length(PubPMID)



library(ggthemes)
library(ggpubr)
library(ggplot2)
library(viridis)


dfAI4painPubYear <- data.frame(AI4painPubYear)
dfAI4painPubYear$Year <- row.names(dfAI4painPubYear)
dfAI4painPubYear$Year <- row.names(dfAI4painPubYear)

sum(dfAI4painPubYear$AI4painPubYear[dfAI4painPubYear$Year >= 2019]) / sum(dfAI4painPubYear$AI4painPubYear) * 100
sum(dfAI4painPubYear$AI4painPubYear[dfAI4painPubYear$Year >= 2020]) / sum(dfAI4painPubYear$AI4painPubYear) * 100

AI4painPubYear_barplot <-
  ggplot(data = dfAI4painPubYear, aes(x = Year, y = AI4painPubYear)) +
  geom_bar(stat = "identity", fill = viridis(1)) +
  labs(x = "Year", y = "Number of hits", title = "Number of PubMed articles on AI and machine learning in pain-related data") +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    panel.background = element_rect(fill = "aliceblue", colour = "white")
  )

# Workaround for country queres gets only NAs
library(countrycode)
library(dplyr)

CountriesList <- codelist
dfAi4painPubsearchCountry <-
  subset(CountriesList, select = "country.name.en")
dfAi4painPubsearchCountry$Country.Code <-
  countrycode(
    dfAi4painPubsearchCountry$country.name.en,
    destination = "iso3c",
    origin = "country.name"
  )

dfAi4painPubsearchCountry <- na.omit(dfAi4painPubsearchCountry)
dfAi4painPubsearchCountry$PublicationsAI4pain <- 0
for (i in 1:nrow(dfAi4painPubsearchCountry)) {
  my_query1 <-
    paste(
      my_query,
      " AND (",
      paste0(dfAi4painPubsearchCountry[i, 1][!is.na(dfAi4painPubsearchCountry[i, 1])], "[PL]", collapse = " OR "),
      ")",
      sep = ""
    )
  resYearCountry <-
    EUtilsSummary(my_query1,
      type = "esearch",
      db = "pubmed",
      retmax = 9999999
    )
  r1 <- EUtilsGet(resYearCountry)
  AI4painAbstracts_0_C <- AbstractText(r1)
  whichIDs_AI <-
    grep(
      "machine-learning|machine learning|machine-learned|machine learned|artificial intelligence|explainable ai|explainable artificial intelligence|xai|knowledge discovery|deep learning",
      tolower(AI4painAbstracts_0_C)
    )
  whichIDs_paintypes <-
    grep(
      "chronic|persisting|persistent|lasting|neuropathic|nociceptive|nociplastic|mixed|neurogenic|back|neck|migraine|arthritis|osteoart|joint|rheumatic|orchialgia|inflammatory|musculoskeletal|muscle|visceral|widespread|somatoform|fibromyalgia|cancer|postoperative|perioperative",
      tolower(AI4painAbstracts_0_C)
    )
  whichIDs_pain <-
    grep("pain|analgesi", tolower(AI4painAbstracts_0_C))
  whichIDs <- intersect(whichIDs_AI, whichIDs_paintypes)
  whichIDs <- intersect(whichIDs, whichIDs_pain)
  PubsYearCountry <- length(whichIDs)
  if (!is.na(dfAi4painPubsearchCountry$Country.Code[i])) {
    if (dfAi4painPubsearchCountry$Country.Code[i] == "GBR") {
      my_query1 <-
        paste(
          my_query,
          " AND (United Kingdom[PL] OR England[PL] OR Scotland[PL] OR  Wales[PL])",
          sep = ""
        )
      resYearCountry <-
        EUtilsSummary(my_query1,
          type = "esearch",
          db = "pubmed",
          retmax = 9999999
        )
      r1 <- EUtilsGet(resYearCountry)
      AI4painAbstracts_0_C <- AbstractText(r1)
      whichIDs_AI <-
        grep(
          "machine-learning|machine learning|machine-learned|machine learned|artificial intelligence|explainable ai|explainable artificial intelligence|xai|knowledge discovery|deep learning",
          tolower(AI4painAbstracts_0_C)
        )
      whichIDs_paintypes <-
        grep(
          "chronic|persisting|persistent|lasting|neuropathic|nociceptive|nociplastic|mixed|neurogenic|back|neck|migraine|arthritis|osteoart|joint|rheumatic|orchialgia|inflammatory|musculoskeletal|muscle|visceral|widespread|somatoform|fibromyalgia|cancer|postoperative|perioperative",
          tolower(AI4painAbstracts_0_C)
        )
      whichIDs_pain <-
        grep("pain|analgesi", tolower(AI4painAbstracts_0_C))
      whichIDs <- intersect(whichIDs_AI, whichIDs_paintypes)
      whichIDs <- intersect(whichIDs, whichIDs_pain)
      PubsYearCountry <- PubsYearCountry + length(whichIDs)
    }
  }
  dfAi4painPubsearchCountry$PublicationsAI4pain[i] <-
    PubsYearCountry
}

length(dfAi4painPubsearchCountry$Country.Code[dfAi4painPubsearchCountry$PublicationsAI4pain > 0])

######################### Cartogram  ###################################
# Normalze citations by population size

library(foreign)
library(gdata)
WorldPopulationCountries <-
  read.xls(
    "/home/joern/Aktuell/Biomedinformatics/Editorial/WorldPopulationCountries.xlsx"
  )

library(readr)
# names(WorldPopulationCountries)
names(WorldPopulationCountries)[grep("X", names(WorldPopulationCountries))] <-
  parse_number(names(WorldPopulationCountries)[grep("X", names(WorldPopulationCountries))])
# names(WorldPopulationCountries)

WorldPopulationCountries1 <-
  WorldPopulationCountries[, which(as.integer(names(WorldPopulationCountries)) >=
    as.integer(names(AI4painPubYear[min(which(AI4painPubYear > 0))])))]
# names(WorldPopulationCountries1)
WorldPopulationCountries$MeanPop <-
  apply(WorldPopulationCountries1, 1, mean)
WorldPopulationCountries$Country.Code <-
  countrycode(
    WorldPopulationCountries$Region..subregion..country.or.area..,
    destination = "iso3c",
    origin = "country.name"
  )

dfAi4painPubsearchCountry$MeanPop <-
  WorldPopulationCountries$MeanPop[match(
    dfAi4painPubsearchCountry$Country.Code,
    WorldPopulationCountries$Country.Code
  )]
dfAi4painPubsearchCountry$PublicationsAI4pain_per_meanPop <-
  slog(dfAi4painPubsearchCountry$PublicationsAI4pain) / slog(dfAi4painPubsearchCountry$MeanPop)
dfAi4painPubsearchCountry$PublicationsAI4pain_log <-
  slog(dfAi4painPubsearchCountry$PublicationsAI4pain, m = 10)

# Create cartngram

library(Rcartogram)
library(getcartr)
library(ggplot2)
library(maptools)
library(data.table)
library(viridis)
library(ggthemes)

# smaller.data_pub <-
#   data.frame(subset(
#     dfAi4painPubsearchCountry,
#     select = c("Country.Code", "PublicationsAI4pain_log")
#   ))
# names(smaller.data_pub) <- c("Country.Code", "Population")
# smaller.data_pub <-
#   smaller.data_pub[smaller.data_pub$Population > 0, ]
# # smaller.data_pub$Country.Code <- factor(smaller.data_pub$Country.Code)
# dim(smaller.data_pub)
# # str(smaller.data_pub)
# # smaller.data_pub$Population[smaller.data_pub$Population == 0] <- .0001
# world <-
#   readShapePoly(paste0(pfad_o, pfad_u1, "TM_WORLD_BORDERS-0.3.shp"))
# matched.indices <-
#   match(world@data[, "ISO3"], smaller.data_pub[, "Country.Code"])
# world@data <-
#   cbind.data.frame(world@data, smaller.data_pub[matched.indices, ])
# world@data[is.na(world@data$Population), ] <- 0.0001
# world.carto <- quick.carto(world, world@data$Population, blur = 0.5)
# world.f <- fortify(world.carto, region = "Country.Code")
# world.f <-
#   merge(world.f, world@data, by.x = "id", by.y = "Country.Code")
# Cartogram_Ai4pain <-
#   ggplot(
#     world.f,
#     aes(
#       long,
#       lat,
#       group = group,
#       colour = factor(1),
#       fill = Population
#     )
#   ) +
#   geom_polygon(size = .1) +
#   scale_fill_gradientn(colours = rev(inferno(256)), na.value = "grey90") +
#   scale_color_manual(values = "dodgerblue") +
#   guides(color = "none") +
#   theme_bw() +
#   theme(
#     legend.position = c(.1, .2),
#     panel.background = element_rect(fill = "aliceblue", colour = "white")
#   ) +
#   labs(
#     fill = "log Publications",
#     title = "Cartogram of PubMed articles on AI and machine learning in pain-related data"
#   )

smaller.data_pub <-
  data.frame(subset(
    dfAi4painPubsearchCountry,
    select = c("Country.Code", "PublicationsAI4pain_per_meanPop")
  ))
names(smaller.data_pub) <- c("Country.Code", "Population")
smaller.data_pub <-
  smaller.data_pub[smaller.data_pub$Population > 0, ]
# smaller.data_pub$Country.Code <- factor(smaller.data_pub$Country.Code)
dim(smaller.data_pub)
# str(smaller.data_pub)
# smaller.data_pub$Population[smaller.data_pub$Population == 0] <- .0001
world1 <-
  readShapePoly(paste0(pfad_o, pfad_u1, "TM_WORLD_BORDERS-0.3.shp"))
matched.indices <-
  match(world1@data[, "ISO3"], smaller.data_pub[, "Country.Code"])
world1@data <-
  cbind.data.frame(world1@data, smaller.data_pub[matched.indices, ])
world1@data[is.na(world1@data$Population), ] <- 0.0001
world1.carto <-
  quick.carto(world1, world1@data$Population, blur = 0.5)
world1.f <- fortify(world1.carto, region = "Country.Code")
world1.f <-
  merge(world1.f, world1@data, by.x = "id", by.y = "Country.Code")
Cartogram_Ai4pain_per_MeanPop <-
  ggplot(
    world1.f,
    aes(
      long,
      lat,
      group = group,
      colour = factor(1),
      fill = Population
    )
  ) +
  geom_polygon(size = .1) +
  scale_fill_gradientn(colours = rev(inferno(256)), na.value = "grey90") +
  scale_color_manual(values = "dodgerblue") +
  guides(color = "none") +
  theme_bw() +
  theme(
    legend.position = c(.1, .23),
    panel.background = element_rect(fill = "azure", colour = "white")
  ) +
  labs(
    fill = "log Publications\nper log population",
    title = "Cartogram of PubMed articles on AI and machine learning in pain-related data per population size"
  )

ggarrange(
  AI4painPubYear_barplot,
  Cartogram_Ai4pain_per_MeanPop,
  labels = LETTERS[1:4],
  align = "hv",
  ncol = 1,
  heights = c(1, 3)
)

######################### Save query ###################################

# save.image(file = paste0(pfad_o, "01Transformierte/", "AI4pain.RData"))
# load(paste0(pfad_o, "01Transformierte/", "AI4pain.RData"))

######################### Abstract text mining ###################################

# Search for pain types
paintypes <-
  c("nociceptiv", "neuropath", "nociplasti", "mixed pain")
dfPaintypes <- data.frame(paintypes)
for (i in 1:length(paintypes)) {
  whichIDs_paintypes <- grep(paintypes[i], tolower(AI4painAbstracts))
  whichIDs_pain <- grep("pain|analgesi", tolower(AI4painAbstracts))
  dfPaintypes$paintypes[i] <-
    length(intersect(whichIDs_paintypes, whichIDs_pain))
}


# Search for pain types
painduractions <- c("acute", "chronic|persist")
dfPaindurations <- data.frame(painduractions)
for (i in 1:length(painduractions)) {
  whichIDs_paindurations <-
    grep(painduractions[i], tolower(AI4painAbstracts))
  whichIDs_pain <- grep("pain|analgesi", tolower(AI4painAbstracts))
  dfPaindurations$painduractions[i] <-
    length(intersect(whichIDs_paindurations, whichIDs_pain))
}

# Search for pain settings
painsettings <-
  c(
    "musculoskeletal|muscle|skelet",
    "visceral",
    "idiopath",
    "widespread",
    "neuropathic",
    "back pain",
    "inflammatory",
    "fibromy",
    "osteoart"
  )
dfPainsettings <- data.frame(painsettings)
for (i in 1:length(painsettings)) {
  whichIDs_painsettings <-
    grep(painsettings[i], tolower(AI4painAbstracts))
  whichIDs_pain <- grep("pain|analgesi", tolower(AI4painAbstracts))
  dfPainsettings$painsettings[i] <-
    length(intersect(whichIDs_painsettings, whichIDs_pain))
}

# Search for ML methods
MLmethods <- c(
  "autoencoder",
  "support vector",
  "regression",
  "convolutional neu",
  "perceptron",
  "forest",
  "nearest nei|k near|k-near",
  "generative adver",
  "reinforcement learn",
  "k-means|kmeans|k means",
  "dbscan|spatial clus",
  "pca|principal comp",
  "independent comp",
  "t-sne|tsne|t-distrib|t distrib",
  "self organi|self-organi|esom",
  "natural lang",
  "knowledge dicover"
)
dfMLmethods <- data.frame(MLmethods)
for (i in 1:length(MLmethods)) {
  whichIDs_MLmethods <- grep(MLmethods[i], tolower(AI4painAbstracts))
  whichIDs_pain <- grep("pain|analgesi", tolower(AI4painAbstracts))
  dfMLmethods$MLmethods[i] <-
    length(intersect(whichIDs_MLmethods, whichIDs_pain))
}

# Search for XAI
whichIDs_XAI <-
  grep("xai|explainable art", tolower(AI4painAbstracts))
whichIDs_pain <- grep("pain|analgesi", tolower(AI4painAbstracts))
length(intersect(whichIDs_XAI, whichIDs_pain))

# Search for laboratory animals
labanimals <-
  c(
    "rats",
    "mice|mouse"
  )
dfLabanimals <- data.frame(labanimals)
for (i in 1:length(labanimals)) {
  whichIDs_labanimals <-
    grep(labanimals[i], tolower(AI4painAbstracts))
  whichIDs_pain <- grep("pain|analgesi", tolower(AI4painAbstracts))
  dfLabanimals$labanimals[i] <-
    length(intersect(whichIDs_labanimals, whichIDs_pain))
}


# Wordcloud of abstracts
library(PubMedWordcloud)
library(ggthemes)
library(ggwordcloud)

cleanAbs <- cleanAbstracts(AI4painAbstracts)
head(cleanAbs)
Kafka <-
  read.csv(
    "~/.Datenplatte/Joerns Dateien/Aktuell/AI4pain/08AnalyseProgramme/AI4pain/Kafka.txt",
    sep = ""
  )
PNAS <-
  read.csv(
    "~/.Datenplatte/Joerns Dateien/Aktuell/AI4pain/08AnalyseProgramme/AI4pain/PNAS.txt",
    sep = ""
  )
Words_unique <- unique(c(Kafka$WordsKafka, PNAS$X))
cleanAbs <- cleanAbs[!cleanAbs$word %in% Words_unique, ]
head(cleanAbs)
# set.seed(7)
# plotWordCloud(
#   cleanAbs,
#   min.freq = 2,
#   scale = c(8, 1),
#   colors = colorblind_pal()(8)[2:8]
# )

set.seed(7)
cleanAbs["angle"] <-
  45 * sample(-2:2,
    length(cleanAbs[, 2] >= 50),
    replace = TRUE,
    prob = c(1, 1, 4, 1, 1)
  )
ggplot(data = cleanAbs[cleanAbs[, 2] >= 50, ], aes(
  label = word,
  size = freq,
  color = freq,
  angle = angle
)) +
  geom_text_wordcloud_area(shape = "square") +
  scale_size_area(max_size = 40) +
  theme_minimal()



######################### Anaylzsed sample sizes ###################################

# Prepare for manual extraction from abstracts
# capture.output(AI4painAbstracts, file = "AI4painAbstracts.txt")

# sample sizes manually extracted from abstracts
library(readxl)
AI4painAbstracts2_BM <- data.frame(
  read_excel(
    "~/.Datenplatte/Joerns Dateien/Aktuell/AI4pain/09Originale/AI4painAbstracts2_BM_korr.xlsx",
    col_names = FALSE,
    col_types = c(
      "text",
      "numeric",
      "text"
    )
  )
)

AI4painAbstracts_DK <- data.frame(
  read_excel(
    "~/.Datenplatte/Joerns Dateien/Aktuell/AI4pain/09Originale/AI4painAbstracts_DK_korr.xlsx",
    col_names = FALSE,
    col_types = c(
      "text",
      "numeric",
      "text",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "text"
    )
  )
)


sample.sizes_2 <-
  data.frame(na.omit(c(
    AI4painAbstracts2_BM[, 2], AI4painAbstracts_DK[, 2]
  )))
names(sample.sizes_2) <- "SampleSizes"
range(sample.sizes_2$SampleSizes)
table(sample.sizes_2$SampleSizes)

ggplot(data = sample.sizes_2, aes(x = log10(SampleSizes))) +
  geom_histogram(
    aes(y = ..count..),
    binwidth = 1,
    colour = "grey50",
    fill = "grey80"
  ) +
  geom_density(aes(y = ..count..), fill = "dodgerblue", color = "dodgerblue2", size = 1, alpha = .2) +
  labs( # title = "Sample sizes in pain-related studies analyzed with machine-learning",
    y = "Count of reports", x = "Log10 sample sizes from abstracts"
  ) +
  theme_linedraw()