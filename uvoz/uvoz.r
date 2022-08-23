source("lib/libraries.r", encoding="UTF-8")

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
  colnames(Varnost) <- c("Drzava", "Kriminal", "Varnost")
  Varnost <- Varnost %>% pivot_longer(-Drzava, values_to = "vrednost" , names_to = "indeks" )
  Varnost$Leto <- leto
  Skupna <- Varnost
  # uvoz za vsa naslednja leta
  for(leto in 2014:2019){
    link = paste0("https://www.numbeo.com/crime/rankings_by_country.jsp?title=", leto, "&region=150")
    linkVarnost <-read_html(link)
    tabelavarnost <- linkVarnost %>% html_table(fill = TRUE)
    Varnost <- tabelavarnost[[2]] %>% select(-Rank) # -Rank spusti stolpec z imenom Rang
    colnames(Varnost) <- c("Drzava", "Kriminal", "Varnost")
    Varnost <- Varnost %>% pivot_longer(-Drzava, values_to = "vrednost" , names_to = "indeks" )
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
colnames(Izobrazba) <- c("Drzava", 2013:2019) # uporabi raje regularni izraz da odstraniš X iz let!!!!!!!!!!!!!!!!!!!!!
Izobrazba  <- Izobrazba %>% pivot_longer(-Drzava, names_to = "Leto", values_to = "izobrazba")



Population <- read.csv("podatki/Population.csv", skip = 4, sep = ",")
obdrzistolpec <- c("Country.Name","X2013","X2014", "X2015", "X2016","X2017","X2018","X2019")
Population <- Population[ , obdrzistolpec]
Population <- na.omit(Population)
colnames(Population) <- c("Drzava", 2013:2019)
Population  <- Population %>% pivot_longer(-Drzava, names_to = "Leto", values_to = "populacija")




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


drzave <- unique(Varnost$Drzava)

skupna <- skupna %>% filter(Drzava %in% drzave)



# uvoz2012 <- function(Tabela2012){
#    obdrzi_1 <- c("Country", "Dohodek.2012")
#    obdrzi_2 <- c("Country", "Izobrazba.2012")
#    Tabela2012 <- uvozplace()[ , obdrzi_1] %>% inner_join(uvozizobrazba()[, obdrzi_2], "Country" = "Country") %>% inner_join(uvozvarnost2012(), "Country" = "Country")
#    return(Tabela2012)
#  }

 uvoz2013 <- function(Tabela2013){
   obdrzi_1 <- c("Country", "Dohodek.2013")
   obdrzi_2 <- c("Country", "Izobrazba.2013")
   obdrzi_3 <- c("Country", "Population.2013")
   Tabela2013 <- uvozplace()[ , obdrzi_1] %>% inner_join(uvozizobrazba()[, obdrzi_2], "Country" = "Country") %>% inner_join(uvozvarnost2013(), "Country" = "Country") %>% inner_join(uvozipopulation()[, obdrzi_3], "Country" = "Country")
   return(Tabela2013)
 }

 uvoz2014 <- function(Tabela2014){
   obdrzi_1 <- c("Country", "Dohodek.2014")
   obdrzi_2 <- c("Country", "Izobrazba.2014")
   obdrzi_3 <- c("Country", "Population.2014")
   Tabela2014 <- uvozplace()[ , obdrzi_1] %>% inner_join(uvozizobrazba()[, obdrzi_2], "Country" = "Country") %>% inner_join(uvozvarnost2014(), "Country" = "Country") %>% inner_join(uvozipopulation()[, obdrzi_3], "Country" = "Country")
   return(Tabela2014)
 }
 uvoz2015 <- function(Tabela2015){
   obdrzi_1 <- c("Country", "Dohodek.2015")
   obdrzi_2 <- c("Country", "Izobrazba.2015")
   obdrzi_3 <- c("Country", "Population.2015")
   Tabela2015 <- uvozplace()[ , obdrzi_1] %>% inner_join(uvozizobrazba()[, obdrzi_2], "Country" = "Country") %>% inner_join(uvozvarnost2015(), "Country" = "Country") %>% inner_join(uvozipopulation()[, obdrzi_3], "Country" = "Country")
   return(Tabela2015)
 }

 uvoz2016 <- function(Tabela2016){
   obdrzi_1 <- c("Country", "Dohodek.2016")
   obdrzi_2 <- c("Country", "Izobrazba.2016")
   obdrzi_3 <- c("Country", "Population.2016")
   Tabela2016 <- uvozplace()[ , obdrzi_1] %>% inner_join(uvozizobrazba()[, obdrzi_2], "Country" = "Country") %>% inner_join(uvozvarnost2016(), "Country" = "Country") %>% inner_join(uvozipopulation()[, obdrzi_3], "Country" = "Country")
   return(Tabela2016)
 }

 uvoz2017 <- function(Tabela2017){
   obdrzi_1 <- c("Country", "Dohodek.2017")
   obdrzi_2 <- c("Country", "Izobrazba.2017")
   obdrzi_3 <- c("Country", "Population.2017")
   Tabela2017 <- uvozplace()[ , obdrzi_1] %>% inner_join(uvozizobrazba()[, obdrzi_2], "Country" = "Country") %>% inner_join(uvozvarnost2017(), "Country" = "Country") %>% inner_join(uvozipopulation()[, obdrzi_3], "Country" = "Country")
   return(Tabela2017)
 }

 uvoz2018 <- function(Tabela2018){
   obdrzi_1 <- c("Country", "Dohodek.2018")
   obdrzi_2 <- c("Country", "Izobrazba.2018")
   obdrzi_3 <- c("Country", "Population.2018")
   Tabela2018 <- uvozplace()[ , obdrzi_1] %>% inner_join(uvozizobrazba()[, obdrzi_2], "Country" = "Country") %>% inner_join(uvozvarnost2018(), "Country" = "Country") %>% inner_join(uvozipopulation()[, obdrzi_3], "Country" = "Country")
   return(Tabela2018)
 }

 uvoz2019 <- function(Tabela2019){
   obdrzi_1 <- c("Country", "Dohodek.2019")
   obdrzi_2 <- c("Country", "Izobrazba.2019")
   obdrzi_3 <- c("Country", "Population.2019")
   Tabela2019 <- uvozplace()[ , obdrzi_1] %>% inner_join(uvozizobrazba()[, obdrzi_2], "Country" = "Country") %>% inner_join(uvozvarnost2019(), "Country" = "Country") %>% inner_join(uvozipopulation()[, obdrzi_3], "Country" = "Country")
   return(Tabela2019)
 }
 
uvoz <- uvoz2019()
 
skupna_tabela <- function(SkupnaTabela){
   SkupnaTabela <- uvoz2013() %>% inner_join(uvoz2014(), "Country" = "Country") %>% inner_join(uvoz2015(), "Country" = "Country") %>% inner_join(uvoz2016(), "Country" = "Country")%>% inner_join(uvoz2017(), "Country" = "Country")%>% inner_join(uvoz2018(), "Country" = "Country")%>% inner_join(uvoz2019(), "Country" = "Country")
   return(SkupnaTabela)
 }


#Tabela2012 <- uvoz2012()
Izobrazba <- uvozizobrazba()
Population <- uvozipopulation()
Dohodek <- uvozplace()
Tabela2013 <- uvoz2013()
Tabela2014 <- uvoz2014()
Tabela2015 <- uvoz2015()
Tabela2016 <- uvoz2016()
Tabela2017 <- uvoz2017()
Tabela2018 <- uvoz2018()
Tabela2019 <- uvoz2019()
SkupnaTabela <- skupna_tabela()
