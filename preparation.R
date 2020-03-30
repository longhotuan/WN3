library(feather)
library(tidyverse)

water_nexus3 <- read_feather("wn3_dashboard.feather") %>% select(-`PubMed ID`)
gs <- read_csv("GS.csv")

first_country <- which(colnames(water_nexus3) == "Albania")
last_country <- which (colnames(water_nexus3) == "Zimbabwe")

# Global South

z1 <- water_nexus3[,first_country:last_country][,colnames(water_nexus3[,first_country:last_country]) %in% gs$Country]
z1$`Global South` <- NA
for (i in 1:nrow(z1)){
    if(sum(is.na(z1[i,])) != 82){
        z1$`Global South`[i] <- "Global South"
    }
}
z2 <- as.data.frame(z1$`Global South`)
colnames(z2)<-"Global South"
water_nexus3 <- bind_cols(water_nexus3, z2)


# Years

water_nexus3$`From 2010 to 2019` <- NA
water_nexus3$`From 2000 to 2009` <- NA
water_nexus3$`Before 2000` <- NA

for (i in 1:nrow(water_nexus3)){
    if (between(water_nexus3$Year[i], 2010, 2019)){
        water_nexus3$`From 2010 to 2019`[i] <-"From 2010 to 2019"
    } else if(between(water_nexus3$Year[i], 2000, 2009)){
        water_nexus3$`From 2000 to 2009`[i] <-"From 2000 to 2009"
    } else {
        water_nexus3$`Before 2000`[i] <-"Before 2000"
    }
}

write_feather(water_nexus3, "water_nexus.feather")
