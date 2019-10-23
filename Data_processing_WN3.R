#### import libraries ####
# data wrangling

library(tidyverse)
library(reshape)
library(data.table)
library(xlsx)
library(gridExtra)
library(grid)
library(chron)
library(devtools)
library(rscopus)
library(rlist)
library(rgeos)
library(future)
library(parallel)
library(doParallel)

# data visualization 

library(GGally)
library(RColorBrewer)
library(proj4)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(mapview)
library(htmlwidgets)
library(corrplot)
library(mice)
library(VIM)
library(ggmosaic)
library(esquisse)
library(bibliometrix)
library(ggwordcloud)
library(colorspace)
library(rworldmap)
library(countrycode)
#### import dataset ####

WN3 <- read_csv("SDG6.csv",  locale = readr::locale(encoding = "latin1"))
WN3 <- WN3[,-c(1, 7:12,17,18,21:29,33:37,41,43,44)]

# put the affiliation into the dataset ####
WN3$Affi <- NA
for (j in 1: nrow(WN3)){
    WN3$Affi[j] <- list(as.list(str_split_fixed(WN3$Affiliations[j], "; ", 
                                                      n =1+ str_count(WN3$Affiliations[j], "; "))))
    
}

# split into list of different affiliation
WN3$Country <- WN3$Affi

for (i in 1:nrow(WN3)){
    for (j in 1:length(WN3$Affi[[i]])){
        WN3$Country[[i]][[j]] <- as.list(str_split_fixed(WN3$Affi[[i]][[j]], ", ", n = 1 + str_count(WN3$Affi[[i]][[j]], ", ")))
        k <- length(WN3$Country[[i]][[j]])
        WN3$Country[[i]][[j]] <-  WN3$Country[[i]][[j]][[k]]
    }
}


# Add lat long dataset

lat_long <- as.data.frame(gCentroid(getMap(resolution="high"), byid=TRUE))
lat_long$Country <- rownames(lat_long)
rownames(lat_long) <- c(1:nrow(lat_long))

# some different countries --> match them

country <- vector()

for (i in 1:nrow(WN3)){
    country <- append(country, unlist(WN3$Country[[i]]))
}

setdiff(unique(country), lat_long$Country)
# change in lat long 
lat_long <- bind_rows(lat_long, data.frame(x = 31.9522, y = 35.2332, Country = 'Palestine'))
lat_long <- bind_rows(lat_long, data.frame(x = 16.2650, y = 61.5510, Country = 'Guadeloupe'))

# change in WN3
old_1 <- c("United States", "Serbia", "Viet Nam", "Russian Federation", 
           "Tanzania", "Syrian Arab Republic", "Congo", "Brunei Darussalam", "Macau", "Cote d'Ivoire",
           "Chinese Academy of Agricultural Sciences", "The Netherlands",
           "USA", "Bern Switzerland", "Côte d’Ivoire", "UK", "Ghent University", "KU Leuven",
           "Republic of South Sudan", "Brussels", "Southampton", "National Belge de la Recherche Scientifique FRS - FNRS", "Democratic Republic Republic of the Congo", "Territory Protection and Sea",
           "Schlumberger", "Mayotte", "INP", "Fond de la Recherche Scientifique FNRS","Universiteandacute", "Martinique", "University of Rennes I",
           "Institute for Research in Biomedicine", "Falkland Islands (Malvinas)","Universite de Toulouse", 
           "Université de Toulouse",
           "Libyan Arab Jamahiriya", "IUEM", 
           "Royal Netherlands Institute for Sea Research \\(NIOZ",
           "Procter &amp",
           "Grenoble INP - Université de Savoie - Université J. Fourier", 
           "CNRS",
           "LMOPS", "Isotope Bioscience Laboratory Andndash", "SCRiPTS", 
           "Hong Kong", "VU University Medical Center", "Timor Leste", "Bahamas", "Reunion", "INPT",
           "IBMC", "Tunisia E-mail: hdbenmansour@gmail.com", "L'Oreal Research Andamp", "Universidad Autónoma de Madrid","Bahir Dar University Bahir Dar Institute of Technology Bahir Dar Ethiopia",
           "UMR 7574 75005 Paris France", "China \\(e-mail: song.liu3000@hotmail.com\\).","Leuven Food Science and Nutrition", "Earth and Life Institute -Agronomy \\(ELI-A",
           "Deltares", "Ministry of Agriculture and Rural Affairs", "Satlantic","Vlaamse Milieumaatschappij", "Democratic Republic Congo",
           "KU Leuven \\(University of Leuven\\) &amp","Universite",
           "Royal Netherlands Institute for Sea Research \\(NIOZ-Yerseke",
           "Building Physics Section", "Procter Andamp", "University of Leuven &amp","China (e-mail: song.liu3000@hotmail.com).",
           "Bio Base Europe Pilot Plant Rodenhuizekaai 1 9042 Ghent Belgium",
           "AbbVie;", "Earth and Environment", "Ministry of Agriculture", "Campus International de Baillarguet", "Earth and Life Institute -Agronomy (ELI-A",
           "Leuven", "Ministry of Education" 
           )

new_1 <- c("United States of America", "Republic of Serbia", "Vietnam", "Russia",
           "United Republic of Tanzania", "Syria", "Republic of the Congo","Brunei", "Macau S.A.R", 
           "Ivory Coast", "China", "Netherlands", "United States of America",
           "Switzerland", "Ivory Coast","United Kingdom", "Belgium", "Belgium", "South Sudan", "Belgium",
           "United Kingdom", "France", "Democratic Republic of the Congo", "Belgium", "United States of America", "France", "Belgium", "France",
           "Belgium", "France", "France", "Spain", "Falkland Islands","France", 
           "France",
           "Libya", "Mexico", 
           "Netherlands",
           "Belgium", "France",
           "France", "France", "Belgium", "Belgium", 
           "Hong Kong S.A.R.", "Netherlands", "East Timor", "The Bahamas", "France", "France", "France",
           "Tunisia", "France", "Spain", "Ethiopia", "France", "China", "Belgium", "Belgium", "Netherlands", "China", "Canada", "Belgium", "Democratic Republic of the Congo",
           "Belgium", "Belgium", "Netherlands", "Belgium", "Belgium", "Belgium", "China", "Belgium", "Belgium","United Kingdom", "Belgium", "France", "Belgium", "Belgium", "Belgium"
           # , "Netherlands", "Netherlands", "Belgium", "Netherlands" 
           )

names_wrong <- data.frame(Country = old_1, new_name = new_1)

names_wrong$x <- NA 
names_wrong$y <- NA

for(i in 1:nrow(names_wrong)){
    for(j in 1:nrow(lat_long)){
        if(names_wrong$new_name[i] == lat_long$Country[j]){
            names_wrong$x[i] <- lat_long$x[j]
            names_wrong$y[i] <- lat_long$y[j]
        }
    }
}

lat_long <- bind_rows(lat_long, names_wrong[,-2])



# Country is still wrong

country[!country %in% lat_long$Country]
old_1 <- c(old_1, country[!country %in% lat_long$Country])
# match latitude and longitude

WN3$long <- as.list(NA)
WN3$lat <- as.list(NA)

for (i in 1:nrow(WN3)){
    for (j in 1:length(WN3$Country[[i]])){
        for (k in 1:nrow(lat_long)){
            if(str_to_lower(as.character(WN3$Country[[i]][[j]])) == str_to_lower(lat_long$Country[k])){
                WN3$long[[i]][[j]] <- lat_long$x[k]
                WN3$lat[[i]][[j]] <- lat_long$y[k]
            }
        }
    }
}


# unlist the country and author_list ####

for(i in 1:nrow(WN3)){
    WN3$Country[[i]] <- unlist(WN3$Country[[i]])
}

write.csv(x = lat_long, "lat_long.csv")

# change name of country in WN3

for(i in 1:nrow(WN3)){
    for(j in 1:length(WN3$Country[[i]])){
        for(k in 1:length(old_1)){
            if (WN3$Country[[i]][[j]] == old_1[k]){
                WN3$Country[[i]][[j]] <- new_1[k]
            }
        }
    }
}

for(i in 1:nrow(WN3)){
    for(j in 1:length(WN3$Country[[i]])){
        if (WN3$Country[[i]][[j]] == "KU Leuven (University of Leuven) &amp"){
            WN3$Country[[i]][[j]] <- "Belgium"
        }
    }
}

b <- as.data.frame(matrix(data = NA, nrow = nrow(WN3), ncol = length(levels(as.factor(unlist(WN3$Country))))))
colnames(b) <- levels(as.factor(unlist(WN3$Country)))
WN4 <- bind_cols(WN3, b)

for (i in 1:nrow(WN4)){
    for(j in 1:length(WN4$Country[[i]])){
        for(k in (ncol(WN3)+1):ncol(WN4)){
            if (WN4$Country[[i]][[j]] == colnames(WN4[,k])){
                WN4[i,k] <- colnames(WN4[,k])
            }
        }
    }
}

WN5 <- WN4[, -c(19:22)]
write.csv(WN5, "WN3.csv", row.names = FALSE)

colnames(b) <- paste("lat",levels(as.factor(unlist(WN3$Country))), sep = "_")
WN6 <- bind_cols(WN5, b)
colnames(b) <- paste("long",levels(as.factor(unlist(WN3$Country))), sep = "_")
WN6 <- bind_cols(WN6, b)
c <- ncol(WN5)-ncol(WN3)+4

lat_long2 <- lat_long[lat_long$Country %in% colnames(WN5[,19:204]),]
lat_long3 <- lat_long2 %>%  arrange(match(Country, colnames(WN5[,19:204])))
View(cbind(lat_long3[,], colnames(WN5[,19:204]))) ### --> correct

for (i in 19:204){
    WN6[which(!is.na(WN6[,i])),i + 186] <- lat_long3$x[i-18]
    WN6[which(!is.na(WN6[,i])),i + 186*2] <- lat_long3$y[i-18]
}

# test
a <- vector(mode = "character", 0)
for (i in 19:204){
    if(sum(which(is.na(WN6[,i])) != which(is.na(WN6[,i + 186]))) >0){
        a <- c(a, i)
    }
}
b <- vector(mode = "character", 0)
for (i in 19:204){
    if(sum(which(is.na(WN6[,i])) != which(is.na(WN6[,i + 186*2]))) >0){
        b <- c(b, i)
    }
} 
c1 <- vector(mode = "character", 0)
c2 <- vector(mode = "character", 0)

for (i in 19:204){
    if(WN6[which(!is.na(WN6[,i])),i+186] != lat_long3$x[i-18]){
        c1 <- c(c1,i)
    }
    if(WN6[which(!is.na(WN6[,i])),i+186*2] != lat_long3$y[i-18]){
        c2 <- c(c2,i)
    }
}

write.csv(WN6,"WN3_v2.csv", row.names = FALSE)
