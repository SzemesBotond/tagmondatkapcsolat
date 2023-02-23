#korpusz betoltese (vagy az egyes regények, vagy az évcsoportok) - elotte regex kiszedi a romai szamokat es a "-jeleket
input.dir <- "C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/regenyek"
files.v <- dir(input.dir, "\\.txt$")
make.file.l <- function(files.v, input.dir){
  text.l <- list()
  for(i in 1:length(files.v)){
    text.v <- scan(paste(input.dir, files.v[i], sep="/"),
                   what="character", sep="\f", quote = "", encoding = "UTF-8")
    text.l[[files.v[i]]] <- text.v
  }
  return(text.l)
}
my.corpus.l <- make.file.l(files.v, input.dir)

#korpusz elokeszitese: nem listazni; oldalszamok kivetel, 
#gondolatjel utani nagybetu, rovidites, idezojel,
#zarojelen beluli irasjel
regenyek <- sapply(my.corpus.l, unlist,recursive = TRUE, use.names = TRUE)
regenyek <-  sapply(regenyek,function(x) gsub("([0-9]+)([A-zöüóőúéáűí])", "\\2",as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("(– )([A-ZÖÜÓŐÚÉÁŰÍ])", "\\2",as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("(- )([A-ZÖÜÓŐÚÉÁŰÍ])", "\\2",as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("(\\.\\.\\.)( [A-ZÖÜÓŐÚÉÁŰÍ])", "\\.\\2",as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("([A-zzöüóőúéáűí])(-)", "\\1",as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("([[:punct:]])([A-zzöüóőúéáűí])", "\\2",as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("Dr\\. ", "Dr ",as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("stb\\. ", "stb ",as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("Özv\\. ", "Özv ",as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("ifj\\. ", "ifj ",as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("ún\\. ", "ún ",as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("St\\. ", "st ",as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("( [A-zzöüóőúéáűí])(\\.)", "\\1", as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("([.?!])( [a-zöüóőúéáűí])", "\\2", as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("([.?!])( [a-zöüóőúéáűí])", "\\2", as.character(x)))
regenyek <-  sapply(regenyek,function(x) gsub("([.?!])([\\)] [a-zöüóőúéáűí])", "\\2", as.character(x)))

#tokenizer csomag - mondatokra, szavakra, beture szegentalas - a token_sent2 lesz, amin a kapcsolatokat vizsgáljuk
library(tokenizers)
token_sent <- sapply(regenyek, tokenize_sentences)
token_sent2 <- sapply(token_sent, unlist, recursive = TRUE, use.names = TRUE)
sentence_words <- sapply(token_sent2, tokenize_words)

# fejezet es fejezet-szam kivetele
sw <- list()
for (i in 1:length(sentence_words)) {
  sw[[i]] <- sentence_words[[i]][which(sentence_words[[i]] != "fejezet")]
  sw[[i]] <- sw[[i]][which(!grepl("^[0-9][0-9]", sw[[i]]))]
  sw[[i]] <- sw[[i]][which(!grepl("^[0-9]", sw[[i]]))]
}
#mondathossz és a 0 értékek (pl. dubla sorköz)  kiszedése
sentence_length <- list ()
for (i in 1:length(sw)) {
  sentence_length [[i]] <- sapply(sw[[i]], length)
  sentence_length [[i]] <- sentence_length[[i]][which(sentence_length[[i]] !=0)]
}
#szavak szama
szavak_szama <- lapply(sentence_length, sum)


#MONDATKAPCSOLATok
# az összes regexet tartalmazó valtozó, kivéve azok, amelyek nem a mondatokra botott szövegre ("token_sent2" változó) vonatkoznak
kapcsolatok <- c("([,;:\\-\\–](( (és |sőt |s |valamint|majd |aztán|azután))|( (([a-zíéáűúőóüö]+ ){0,2})(ráadásul))))|(((egyrészt )(.*[,;] )(másrészt))|((hol )(.*[,;] )(hol )))",
                 "[,;:\\-\\–] ((([a-zíéáűúőóüö ]+|)(azonban|ellenben|viszont |ámde |csakhogy |ellenkezőleg|mindazonáltal|ugyananakkor |márpedig |mégis |mégse|csak hát))|(de |pedig|ám ))",
                 "[,;:\\-\\–] (vagy |avagy )",
                 "[,;:\\-\\–]( (([a-zíéáűúőóüö ]+|)(azaz |ugyanis |hiszen |azazhogy |vagyishogy |tudniillik |mármint|mégpedig|elvégre |egyszóval|utóvégre|illetve))|((( mert | de | és | s )| )(hisz |pontosabban)))",
                 "[,;:\\-\\–]( (([a-zíéáűúőóüö ]+|)(tehát |ennélfogva |eszerint |úgyhogy|következésképpen))|(( és | s | )(ezért|szóval)))|([,;:]( és | s | )így )",
                 "(((^(?=.*[,;:]))|([,;:\\-\\–]([a-zíéáűúőóüö ]+|) ))(bár |(ha )(([a-zíéáűúőóüö]+ )+)(is )|habár |igaz, hogy |jóllehet |még ha |mégha |noha |ugyan |ámbár))|([,;:\\-\\–] (holott))",
                 "([,;:\\-\\–] ((([a-zíéáűúőóüö]+ ){0,1}((éppen|)mert |merthogy |minthogy |mivel |nehogy))|különben))|((amiatt |avégett |avégre |azért)(([a-zíéáűúőóüö ]+|)(([a-zíéáűúőóüö]+)|))[,;]( hogy))|((^(?=.*[,;:]))(([a-zíéáűúőóüö]+ ){0,1})(minthogy |mivel ))",
                 "((^(?=.*[,;:]))|([,;:\\-\\–]([a-zíéáűúőóüö ]+|) ))(ha |hacsak |amennyiben |hogyha|a mennyiben)")

hasonlito <- "(^|([,;:\\-\\–] ))(([a-zíéáűúőóüö]+ ){0,1})(mint |mintha |akár |akárha |akárcsak |minthogyha)"
helyhatarozo <- "(((^(?=.*[,;:]))|([,;:\\-\\–]([a-zíéáűúőóüö ]+|) ))(ahol |ahonnan |ahov|ameddig |amerr))|([,;:\\-\\–] (a |)(hol |honnan |hová |hova |meddig |merről |merre))"
idobeli <- "(((^(?=.*[,;:]))|([,;:\\-\\–]([a-zíéáűúőóüö ]+|) ))(alighogy |ameddig |amíg |amikor|amióta |attól (fogva|kezdve), hogy |amint |mígnem |amidőn |midőn |mielőtt |míg |mialatt |mi alatt |mihelyt |mikor|mi kor|mióta|mi óta |miután|mi után|miközben|mi közben))|([,;:\\-\\–] (közben|mire))"
vonatkozo <- "(((^(?=.*[,;:]))|([,;:\\-\\–]([a-zíéáűúőóüö ]+|) ))((ami|aki|amely|mely)))|[,;\\-\\–]((( és | s | de | a | )(ki |a mit |kit |mik |kik |miben |kiben |mibe |kibe |a mire |kire |a miről |kiről |mitől |kitől |miből |kiből |kinél  |kivel |a mivel |minek |kinek |min |kin |mihez |kihez |miket |kiket |mikben |kikben |mikbe |kikbe |mikre |kikre |mikről |kikről |miktől |kiktől |mikből |kikből |miknél |kiknél |mikkel |kikkel |miknek |kiknek |miken |kiken |mikhez |kikhez))|( mi ))"

#az egyes kapcsolatfajtak aranyat megallapito fuggveny
kotoszavak_aranya <- function(kapcsolat, korpusz) {
  ossz <- list ()
  darab <- list ()
  arany <- list()
  for (i in 1:length(korpusz)) {
    ossz[[i]] <- gregexpr(kapcsolat, korpusz[[i]], perl = TRUE, ignore.case =TRUE)
    ossz [[i]] <- unlist(ossz[[i]])
    ossz [[i]] <- ossz[[i]][which(ossz[[i]] != -1)]
    darab [[i]] <- length(ossz[[i]])
    arany[[i]] <- darab[[i]]/szavak_szama[[i]]*1000 
  }
  return(arany)
}

#az osszes arany kiszamolasa az egyes regexekkel az "osszetetelek" nevu valtozoban
osszetetel <- list ()
for (i in 1:length(kapcsolatok)) {
  osszetetel[[i]] <- kotoszavak_aranya(kapcsolatok[[i]], token_sent2)
}

#a mondatokra bontott szöveg előkészítése az egyes típusokhoz
#ehhez meghatározni a mentális igéket
# a mentális igék és más alakok kiszűrése a mondatokból
ment_ig <- "((^| )(elárul|eldönt|elképzel|ért|érdekel|érdekl|tud|kérde|kérdi|megkérd|képzel|kitalál|megállapít|megért|megmutat|sejt))|(ni akar)"
ment_ig2 <- "(ni akar)|((^| )(elárul|eldönt|elképzel|érdekel|érdekl|tud|kérde|kérdi|megkréd|képzel|kitalál|megállapít|megért|megmutat|sejt))"
library(stringr)
hasmin <- lapply(token_sent2, str_remove_all,"((akár|Akár) (.*?)akár )|( a mint | nem mint )")
vonmin <- lapply(token_sent2, str_remove_all, "amint|(A|a)mikor|amiként|amiképp|amiatt|amidőn|amióta|amialatt|amielőtt|amiután|amíg ")
vonmin <- lapply(vonmin, function(x) gsub("(^| )a melyik", " amelyik", ignore.case=T, as.character(x)))
vonmin <- lapply(vonmin, str_remove_all, "( mint (aki|ami|ki))|( melyik)")
vonmin <- lapply(vonmin, function(x) gsub(paste(ment_ig,"([a-zíéáűúőóüö ]+|)(\\, )(mi|mely)", sep=""), "\\1\\2\\3\\4", ignore.case=T, as.character(x)))
vonmin <- lapply(vonmin, function(x) gsub(paste(ment_ig, "([a-zíéáűúőóüö]+|)(\\, )(ki)", sep=""), "\\1\\2\\3", as.character(x)))
idomin <- lapply(token_sent2, function(x) gsub(paste(ment_ig2,"([a-zíéáűúőóüö]+|)(\\, )(mikor|mióta)", sep=""),"\\1\\2\\3", ignore.case=T, as.character(x)))
idomin <- lapply(token_sent2, function(x) gsub("(\\, hogy)([a-zíűáéúőóüö ]+|) (mikor|mi kor|mióta|mi óta )", "\\1\\2 ", ignore.case=T, as.character(x)))
idomin  <- lapply(idomin, str_remove_all, " mint (amikor|amióta |mióta |amidőn |midőn |mikor|mi kor)")
helymin <- lapply(token_sent2, function(x) gsub(paste(ment_ig2,"([a-zíéáűúőóüö]+|)(\\, )(hol |hon|hov|merr|meddig)", sep=""),"\\1\\2\\3", ignore.case=T, as.character(x)))
helymin <- lapply(token_sent2, function(x) gsub("(\\, hogy)([a-zíűáéúőóüö ]+|) (hol |honnan |hová |hova)", "\\1\\2 ", ignore.case=T, as.character(x)))

#hasonlító, helyhatározói, időhatározói és vonatkozói kapcsolatok száma
has <- kotoszavak_aranya(hasonlito, hasmin)
hely <- kotoszavak_aranya(helyhatarozo, helymin)
ido <- kotoszavak_aranya(idobeli, idomin)
von <- kotoszavak_aranya(vonatkozo, vonmin)


#az összes összetétel kombinalalsa es a listakat tartalmazo lista atalakitasa dataframe-be
osszetetel[9] <- list(has)
osszetetel[10] <- list(hely)
osszetetel[11] <- list(ido)
osszetetel[12] <- list(von)
osszetetel  <-  as.data.frame(matrix(unlist(osszetetel), nrow=length(unlist(osszetetel[1]))))
names <- c("Kapcsolatos", "Ellentetes","Választó","Magyarázó","Következtető","Megengedő","Ok/Célhatarozó","Feltételes","Hasonlító","Helyhatározó","Időbeli", "Vonatkozó")
colnames(osszetetel) <- names
row.names(osszetetel) <- files.v

#eredmények kiírása .xls fájlba
library("writexl")
write_xlsx(arany_db,"C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/összetetelek_belsoarany_legujabb_evtized.xlsx")
