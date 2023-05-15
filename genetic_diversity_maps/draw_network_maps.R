library(phangorn)
library(tanggle)
## read the data = nexus file recorded with SplitsTree4 (!)
Nnet <- read.nexus.networx("BOLD_COI_both_trim.aln.nex")

Nnet$edge.length <- Nnet$edge.length*519

pn <- ggsplitnet(Nnet)

tips <- pn$data[pn$data$isTip, ]
tips$id <- sapply(tips$label, function(x) strsplit(x, split = "\\|")[[1]][4])
tips$species <- sapply(tips$label, function(x) strsplit(x, split = "\\|")[[1]][2])


coord.info <- read.csv("bold_data_both.csv")
coord.info$latlon <- paste0(coord.info$lat, "_", coord.info$lon)

tips$place <- sapply(tips$id, function(x) coord.info[which(coord.info$sampleid == x), "latlon"][1])
tips$BIN <- sapply(tips$id, function(x) coord.info[which(coord.info$sampleid == x), "bin_uri"][1])

library(dplyr)
tips %>% count(x, y, place, BIN, species) -> tips.occur
tips.occur <- tips.occur[(!(tips.occur$place %in% c("NA_NA", ""))), ]
tips.occur <- tips.occur[complete.cases(tips.occur$BIN), ]


unique(tips.occur$BIN)

ggsplitnet(Nnet) +
  geom_point(data = tips.occur, aes(x=x, y=y, fill = BIN, shape = species), 
             size=3, alpha=1, color = "lightgrey") + 
  #scale_color_brewer(palette = "greys")
  scale_fill_manual(values = c("BOLD:ADC7414"="#F0E442", 
                                "BOLD:AEB1648" = "#D81B60", "BOLD:AAY3465" = "#D81B60", 
                                "BOLD:AEB1649" = "#4477AA", 
                                "BOLD:ABW2101" = "purple",
                                "BOLD:ACB4278" = "#134e13",
                                "BOLD:ACE5392" = "#186218",
                                "BOLD:ACF1897" = "#1d771d",
                                "BOLD:ACB5508" = "#228b22",
                                "BOLD:ACE2909" = "#279f27",
                                "BOLD:AAY1020" = "#2cb42c",
                                "BOLD:ACB4275" = "#31c831"), name = "BIN",
                    guide = guide_legend(override.aes = list(shape=21,
                                                             alpha = 1))) + 
  scale_shape_manual(values = c("Gmelinoides_fasciatus" = 24,
                                "Eulimnogammarus_verrucosus" =22))
  #scale_x_continuous(breaks = 1:2) + 
  #theme(axis.line.x = element_line(), axis.ticks.x = element_line())




library(ggmap)
#library(scales)

bbox <- c(left=102, right=112, bottom=51, top=56.2)
BaikalMap <- get_map(bbox, zoom=10, maptype = "terrain-background")

#sampling_points <- data.frame(lat = c(53.374878, 51.870608, 51.870684), 
#                              lon = c(108.975189, 104.828101, 104.811648), 
#                              col = c("#D81B60", "#F0E442", "#4477AA"),
#                              label = c("E", "W", "S"))

pmap <- 
ggmap(BaikalMap) + 
  xlab("Долгота") + ylab("Широта") + 
  #xlab("Longitude") + ylab("Latitude") +
  geom_point(data = coord.info, aes(x = lon, y = lat, fill = bin_uri, shape = species_name), 
             alpha = .5, size = 5, col = "white") + 
  scale_fill_manual(values = c("BOLD:ADC7414"="#F0E442", 
                               "BOLD:AEB1648" = "#D81B60", "BOLD:AAY3465" = "#D81B60", 
                               "BOLD:AEB1649" = "#4477AA", 
                               "BOLD:ABW2101" = "purple",
                               "BOLD:ACB4278" = "#134e13",
                               "BOLD:ACE5392" = "#186218",
                               "BOLD:ACF1897" = "#1d771d",
                               "BOLD:ACB5508" = "#228b22",
                               "BOLD:ACE2909" = "#279f27",
                               "BOLD:AAY1020" = "#2cb42c",
                               "BOLD:ACB4275" = "#31c831"), name = "BIN",
                    guide = guide_legend(override.aes = list(shape=21,
                                                             alpha = 1))) + 
      scale_shape_manual(values = c("Gmelinoides fasciatus" = 24, 
                                "Eulimnogammarus verrucosus" =22), name = "Вид",
                     guide = guide_legend(override.aes = list(color="black", alpha = 1)))

pmap




ggmap(BaikalMap) + 
  xlab("Долгота") + ylab("Широта") + 
  #xlab("Longitude") + ylab("Latitude") +
  geom_point(data = coord.info[coord.info$species_name == "Gmelinoides fasciatus", ], 
             aes(x = lon, y = lat, fill = bin_uri, shape = species_name), 
             alpha = .5, size = 5, col = "white") + 
  scale_fill_manual(values = c("BOLD:ADC7414"="#F0E442", 
                               "BOLD:AEB1648" = "#D81B60", 
                               "BOLD:AAY3465" = "#C11856", 
                               "BOLD:AEB1649" = "#4477AA", 
                               "BOLD:ABW2101" = "purple",
                               "BOLD:ACB4278" = "#134e13",
                               "BOLD:ACE5392" = "#186218",
                               "BOLD:ACF1897" = "#1d771d",
                               "BOLD:ACB5508" = "#228b22",
                               "BOLD:ACE2909" = "#279f27",
                               "BOLD:AAY1020" = "#2cb42c",
                               "BOLD:ACB4275" = "#31c831"), name = "BIN",
                    guide = guide_legend(override.aes = list(shape=21,
                                                             alpha = 1))) + 
  scale_shape_manual(values = c("Gmelinoides fasciatus" = 24, 
                                "Eulimnogammarus verrucosus" =22), name = "Вид",
                     guide = guide_legend(override.aes = list(color="black", alpha = 1)))


ggmap(BaikalMap) + 
  xlab("Долгота") + ylab("Широта") + 
  #xlab("Longitude") + ylab("Latitude") +
  geom_point(data = coord.info[coord.info$species_name == "Eulimnogammarus verrucosus", ], 
             aes(x = lon, y = lat, fill = bin_uri, shape = species_name), 
             alpha = 1, size = 5, col = "white") + 
  scale_fill_manual(values = c("BOLD:ADC7414"="#F0E442", 
                               "BOLD:AEB1648" = "#D81B60", 
                               "BOLD:AAY3465" = "#C11856", 
                               "BOLD:AEB1649" = "#4477AA", 
                               "BOLD:ABW2101" = "purple",
                               "BOLD:ACB4278" = "#134e13",
                               "BOLD:ACE5392" = "#186218",
                               "BOLD:ACF1897" = "#1d771d",
                               "BOLD:ACB5508" = "#228b22",
                               "BOLD:ACE2909" = "#279f27",
                               "BOLD:AAY1020" = "#2cb42c",
                               "BOLD:ACB4275" = "#31c831"), name = "BIN",
                    guide = guide_legend(override.aes = list(shape=21,
                                                             alpha = 1))) + 
  scale_shape_manual(values = c("Gmelinoides fasciatus" = 24, 
                                "Eulimnogammarus verrucosus" =22), name = "Вид",
                     guide = guide_legend(override.aes = list(color="black", alpha = 1)))



### particular places where these guys come very close to one another
bbox <- c(left=103, right=105, bottom=51, top=53)
BaikalMap <- get_map(bbox, zoom=10, maptype = "terrain-background")

ggmap(BaikalMap) + 
  xlab("Долгота") + ylab("Широта") + 
  #xlab("Longitude") + ylab("Latitude") +
  geom_point(data = coord.info, aes(x = lon, y = lat, fill = bin_uri, shape = species_name), 
             alpha = .5, size = 5, col = "white") + 
  scale_fill_manual(values = c("BOLD:ADC7414"="#F0E442", 
                               "BOLD:AEB1648" = "#D81B60", "BOLD:AAY3465" = "#D81B60", 
                               "BOLD:AEB1649" = "#4477AA", 
                               "BOLD:ABW2101" = "purple",
                               "BOLD:ACB4278" = "#134e13",
                               "BOLD:ACE5392" = "#186218",
                               "BOLD:ACF1897" = "#1d771d",
                               "BOLD:ACB5508" = "#228b22",
                               "BOLD:ACE2909" = "#279f27",
                               "BOLD:AAY1020" = "#2cb42c",
                               "BOLD:ACB4275" = "#31c831"), name = "BIN",
                    guide = guide_legend(override.aes = list(shape=21,
                                                             alpha = 1))) + 
  scale_shape_manual(values = c("Gmelinoides fasciatus" = 24, 
                                "Eulimnogammarus verrucosus" =22), name = "Вид",
                     guide = guide_legend(override.aes = list(color="black", alpha = 1)))
