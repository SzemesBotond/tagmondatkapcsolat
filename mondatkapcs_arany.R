# az osszes regexet tartalmazo valtozo a mondatkapcs_relfreq.R-ben

#az egyes kapcsolatfajtak aranyat megallapito fuggveny
kotoszavak_darab <- function(kapcsolat, korpusz) {
  ossz <- list ()
  darab <- list ()
  for (i in 1:length(korpusz)) {
    ossz[[i]] <- gregexpr(kapcsolat, korpusz[[i]], perl = TRUE, ignore.case =TRUE)
    ossz [[i]] <- unlist(ossz[[i]])
    ossz [[i]] <- ossz[[i]][which(ossz[[i]] != -1)]
    darab [[i]] <- length(ossz[[i]])
  }
  return(darab)
}

#az osszes darab kiszamolasa az egyes regexekkel az "osszetetelek" nevu valtozoban
osszetetel_db <- list ()
for (i in 1:length(kapcsolatok)) {
  osszetetel_db[[i]] <- kotoszavak_darab(kapcsolatok[[i]], token_sent2)
}
#a hasonlító, helyhatározó, időhatározó és vonatkozói alárenelések szűrt mondatai a mondatkapcs_relfreq.R-ben
has_db <- kotoszavak_darab(hasonlito, hasmin)
hely_db <- kotoszavak_darab(helyhatarozo, helymin)
ido_db <- kotoszavak_darab(idobeli, idomin)
von_db <- kotoszavak_darab(vonatkozo, vonmin)

#dataframe letrehozasa
osszetetel_db[9] <- list(has_db)
osszetetel_db[10] <- list(hely_db)
osszetetel_db[11] <- list(ido_db)
osszetetel_db[12] <- list(von_db)
osszetetel_db  <-  as.data.frame(matrix(unlist(osszetetel_db), nrow=length(unlist(osszetetel_db[1]))))
names <- c("Kapcsolatos", "Ellentetes","Választó","Magyarázó","Következtető","Megengedő","Ok/Célhatarozó","Feltételes","Hasonlító","Helyhatározó","Időbeli", "Vonatkozó")
colnames(osszetetel_db) <- names
library(stringr)
for (i in 1:length(files.v)) {
  files.v[[i]] <- str_remove_all(files.v[[i]], ".txt")
  files.v[[i]] <- str_remove_all(files.v[[i]], ".*?\\_")
}
row.names(osszetetel_db) <- files.v
# a dataframe oszlopainak és sorainak felcserélése
ossz_db <- t(osszetetel_db) 
#ellent+valaszt,magyarazo+kovetkezteto, hely+ido közös kategória, -kapcsolatos kiszedése
magykov <- c()
ellentval <- c()
helyido <- c()
for (i in 1:100) {
  magykov[[i]] <-  sum(ossz_db[4,i], ossz_db[5,i])
  ellentval[[i]] <-  sum(ossz_db[2,i], ossz_db[3,i])
  helyido[[i]] <-  sum(ossz_db[10,i], ossz_db[11,i])
} 
magykov <- unlist(magykov)
ellentval <- unlist(ellentval)
helyido <- unlist(helyido)
ossz_db<- rbind(ossz_db,magykov, ellentval, helyido)
rownames(ossz_db)[rownames(ossz_db) == "magykov"] <- "Követ/Magyar"
rownames(ossz_db)[rownames(ossz_db) == "ellentval"] <- "Ellent/Választ"
rownames(ossz_db)[rownames(ossz_db) == "helyido"] <- "Hely/Idő"
ossz_db <- ossz_db[-c(1,2,3,4,5,10,11),]
arany_db <- as.data.frame(ossz_db)

#aranyok kiszamitasa
for (i in 1:ncol(ossz_db)) {
  for (j in 1:nrow(ossz_db)){
    arany_db[j,i] <- ossz_db[j,i]/sum(ossz_db[,i])*100 
  }
} 
#kapcsolat-regény-darab oszlopokká alakítás a vizualizációhoz
ossz_arany <- data.matrix(arany_db)
ossz_arany <- as.table(ossz_arany)
ossz_arany <- as.data.frame(ossz_arany)
colnames(ossz_arany) <- c("Kapcsolat", "Regény", "Arány")

#csak pár regény
library(tidyverse)
szukit <- ossz_arany %>%
  filter(Regény %in% c("AzÉrsekL", "Sinistra","Magyarorszag1514","AFaluJegyzője",  "Kaddis", "Sortalansag", "ATrafik", "Atleta",  "Saulus", "Aranysárkány", "EgyCsaladregeny","Emlekiratok") )
library(ggplot2)
ggplot(szukit, aes("", Arány, fill = Kapcsolat)) +
  geom_bar(stat = "identity", position = "fill", width = .5) +
  ggtitle("4.1. Kapcsolattípusok egymáshoz viszonyított aránya\n12 regény") +
  xlab ("Regények") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap (~ Regény, ncol=6)+
  theme(strip.text.x = element_text(size = 7, colour = "black"))




