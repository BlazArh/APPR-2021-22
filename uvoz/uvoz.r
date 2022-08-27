source("lib/libraries.r", encoding="UTF-8")
source("lib/gradnja_modelov.r")
source("lib/kolena.r")
source("lib/obrisi.r")

# Uvoz, obdelava in čiščenje podatkov
# uvozvarnost2012 <- function(Varnost2012){
#   linkVarnost2012 <-read_html("https://www.numbeo.com/crime/rankings_by_country.jsp?title=2012-Q1&region=150")
#   tabelavarnost2012 <- linkVarnost2012 %>% html_table(fill = TRUE)
#   Varnost2012 <- tabelavarnost2012[[2]] %>% select(-Rank) # -Rank spusti stolpec z imenom Rang
#   return(Varnost2012)
# }

uvozvarnost <- function(){
  # uvoz za leto 2013
  leto <- 2013
  link = paste0("https://www.numbeo.com/crime/rankings_by_country.jsp?title=", leto, "-Q1&region=150")
  linkVarnost <-read_html(link)
  tabelavarnost <- linkVarnost %>% html_table(fill = TRUE)
  Varnost <- tabelavarnost[[2]] %>% select(-Rank) # -Rank spusti stolpec z imenom Rang
  colnames(Varnost) <- c("Drzava", "kriminal", "varnost")
  Varnost$Leto <- leto
  Skupna <- Varnost
  # uvoz za vsa naslednja leta
  for(leto in 2014:2019){
    link = paste0("https://www.numbeo.com/crime/rankings_by_country.jsp?title=", leto, "&region=150")
    linkVarnost <-read_html(link)
    tabelavarnost <- linkVarnost %>% html_table(fill = TRUE)
    Varnost <- tabelavarnost[[2]] %>% select(-Rank) # -Rank spusti stolpec z imenom Rang
    colnames(Varnost) <- c("Drzava", "kriminal", "varnost")
    Varnost$Leto <- leto
    Skupna <- rbind(Skupna, Varnost)
  }
  Skupna
  
}

Varnost <- uvozvarnost()





Izobrazba <- read.csv("podatki/Izobrazba.csv", skip = 5, sep = ",")
obdrzistolpec <- c("Country","X2013","X2014", "X2015", "X2016","X2017","X2018","X2019")
Izobrazba <- Izobrazba[ , obdrzistolpec]
Izobrazba <- head(Izobrazba, -17) #spustim zadnjih 17 stolpcev, ker je na koncu za vsako celino posebej
Izobrazba <- Izobrazba %>% mutate(Country = str_trim(Country)) #znebim se presledka, ki je bil pred vsakim imenom države
Izobrazba <- na.omit(Izobrazba)
Izobrazba[,-1] <- as.data.frame(sapply(Izobrazba[,-1], as.numeric)) #spremenim tip stolpcev iz charachter v numeric
Izobrazba[,-1] <- Izobrazba[,-1] * 100 #pomnožim * 100, da so enaki index safety in criminal
#colnames(Izobrazba) <- c("Drzava", 2013:2019) 
colnames(Izobrazba)[1] <- "Drzava"
Izobrazba  <- Izobrazba %>% pivot_longer(-Drzava, names_to = "Leto", values_to = "izobrazba")
Izobrazba$Leto <- str_extract(Izobrazba$Leto, "\\d\\d\\d\\d")



Izobrazba[Izobrazba$Drzava == "Czechia",]$Drzava <- "Czech Republic"


Population <- read.csv("podatki/Population.csv", skip = 4, sep = ",")
obdrzistolpec <- c("Country.Name","X2013","X2014", "X2015", "X2016","X2017","X2018","X2019")
Population <- Population[ , obdrzistolpec]
Population <- na.omit(Population)
colnames(Population) <- c("Drzava", 2013:2019)
Population  <- Population %>% pivot_longer(-Drzava, names_to = "Leto", values_to = "populacija")




# dohodek je mišljen povprečni LETNI dohodek !!!

linkdohodek <-read_html("https://en.wikipedia.org/wiki/List_of_countries_by_average_wage")
Dohodek <- linkdohodek %>% html_table(fill = TRUE, dec = ",") #zelo pomemben tale dec = ,
Dohodek <- Dohodek[[1]] 
stolpci <- c("Country","2013","2014","2015","2016", "2017","2018","2019")
Dohodek <- Dohodek[ ,stolpci]
Dohodek <- Dohodek %>% mutate(Country = substr(Country, 1, nchar(Country) - 2)) #znebim se * in enega " ", ki je bil na koncu vsakega imena drzave
colnames(Dohodek)[1] <- "Drzava"
Dohodek[,-1] <- Dohodek[,-1]*1000
Dohodek  <- Dohodek %>% pivot_longer(-Drzava, names_to = "Leto", values_to = "dohodek")

Population[Population$Drzava == "Slovak Republic",]$Drzava <- "Slovakia"


skupna <- left_join(Dohodek, Population, by = c("Drzava", "Leto"))
skupna <- left_join(skupna, Izobrazba, by = c("Drzava", "Leto"))
skupna <- skupna %>% mutate(Leto = parse_number(Leto))

skupna <- left_join(skupna, Varnost, by = c("Drzava", "Leto"))
skupna2 <- skupna
skupna  <- skupna %>% pivot_longer(-c(1:2), names_to = "tip", values_to = "vrednost")
drzave <- unique(Varnost$Drzava)
skupna <- skupna %>% filter(Drzava %in% drzave)



skupna2
skupna2 <- skupna2 %>% filter(Drzava %in% drzave)


