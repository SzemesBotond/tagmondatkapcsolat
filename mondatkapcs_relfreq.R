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
regenyek <- c()
for (i in 1:length(my.corpus.l)) {
  regenyek[[i]] <- unlist(my.corpus.l[[i]], recursive = TRUE, use.names = TRUE)
  regenyek [[i]] <- gsub("([0-9]+)([A-zöüóőúéáűí])", "\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("(– )([A-ZÖÜÓŐÚÉÁŰÍ])", "\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("(- )([A-ZÖÜÓŐÚÉÁŰÍ])", "\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("(\\.\\.\\.)( [A-ZÖÜÓŐÚÉÁŰÍ])", "\\.\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("([A-zzöüóőúéáűí])(-)", "\\1", regenyek[[i]])
  regenyek [[i]] <- gsub("([[:punct:]])([A-zzöüóőúéáűí])", "\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("Dr\\. ", "Dr ", regenyek[[i]], ignore.case = TRUE)
  regenyek [[i]] <- gsub("stb\\. ", "stb ", regenyek[[i]])
  regenyek [[i]] <- gsub("Özv\\. ", "Özv ", regenyek[[i]], ignore.case = TRUE)
  regenyek [[i]] <- gsub("ifj\\. ", "ifj ", regenyek[[i]])
  regenyek [[i]] <- gsub("ún\\. ", "ún ", regenyek[[i]])
  regenyek [[i]] <- gsub("St\\. ", "st ", regenyek[[i]])
  regenyek [[i]] <- gsub("( [A-zzöüóőúéáűí])(\\.)", "\\1", regenyek[[i]])
  regenyek [[i]] <- gsub("([.?!])( [a-zöüóőúéáűí])", "\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("([.?!])( [a-zöüóőúéáűí])", "\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("([.?!])([\\)] [a-zöüóőúéáűí])", "\\2", regenyek[[i]])
}
#tokenizer csomag - mondatokra, szavakra, beture szegentalas - a token_sent2 lesz, amin a kapcsolatokat vizsgáljuk
library(tokenizers)
token_sent <- sapply(regenyek, tokenize_sentences)
token_sent2 <- list ()
for (i in 1:length(token_sent)) {
  token_sent2[[i]] <- unlist(token_sent[[i]], recursive = TRUE, use.names = TRUE)
}  
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
#szavak szama - ez lesz az osztó a relatív gyakoriság kiszámolásakor
szavak_szama <- list()
for (i in 1:length(sentence_length)) {
  szavak_szama [[i]] <- sum (sentence_length[[i]])
}

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
vonmin <- list ()
idomin <- list()
hasmin <- list()
helymin <- list ()
for (i in 1:length(token_sent2)) {
  hasmin[[i]] <- str_remove_all(token_sent2[[i]], "((akár|Akár) (.*?)akár )|( a mint | nem mint )")
  vonmin [[i]] <- str_remove_all(token_sent2[[i]], "amint|(A|a)mikor|amiként|amiképp|amiatt|amidőn|amióta|amialatt|amielőtt|amiután|amíg ")
  vonmin [[i]] <- gsub("(^| )a melyik", " amelyik", vonmin[[i]], ignore.case =  T)
  vonmin [[i]] <- str_remove_all(vonmin [[i]], "( mint (aki|ami|ki))|( melyik)")
  vonmin [[i]] <- gsub(paste(ment_ig,"([a-zíéáűúőóüö ]+|)([a-zíéáűúőóüö ]+|)(\\, )(mi|mely)", sep=""), "\\1\\2\\3\\4", vonmin [[i]])
  vonmin [[i]] <- gsub(paste(ment_ig, "([a-zíéáűúőóüö]+|)(\\, )(ki)", sep=""), "\\1\\2\\3", vonmin [[i]],ignore.case = T)
  idomin [[i]] <- gsub(paste(ment_ig2,"([a-zíéáűúőóüö]+|)(\\, )(mikor|mióta)", sep=""),"\\1\\2\\3", token_sent2 [[i]], ignore.case = T)
  idomin [[i]] <- gsub("(\\, hogy)([a-zíűáéúőóüö ]+|) (mikor|mi kor|mióta|mi óta )", "\\1\\2 ", idomin[[i]],ignore.case = T)
  idomin [[i]] <- str_remove_all(idomin[[i]], " mint (amikor|amióta |mióta |amidőn |midőn |mikor|mi kor)")
  helymin [[i]] <- gsub(paste(ment_ig2,"([a-zíéáűúőóüö]+|)(\\, )(hol |hon|hov|merr|meddig)", sep=""),"\\1\\2\\3", token_sent2[[i]], ignore.case = T)
  helymin [[i]] <- gsub("(\\, hogy)([a-zíűáéúőóüö ]+|) (hol |honnan |hová |hova)", "\\1\\2 ", helymin[[i]], ignore.case = T)
}

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
