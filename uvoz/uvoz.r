source("lib/libraries.r", encoding="UTF-8")

# Uvoz, obdelava in čiščenje podatkov
# uvozvarnost2012 <- function(Varnost2012){
#   linkVarnost2012 <-read_html("https://www.numbeo.com/crime/rankings_by_country.jsp?title=2012-Q1&region=150")
#   tabelavarnost2012 <- linkVarnost2012 %>% html_table(fill = TRUE)
#   Varnost2012 <- tabelavarnost2012[[2]] %>% select(-Rank) # -Rank spusti stolpec z imenom Rang
#   return(Varnost2012)
# }

uvozvarnost2013 <- function(Varnost2013){
  linkVarnost2013 <-read_html("https://www.numbeo.com/crime/rankings_by_country.jsp?title=2013-Q1&region=150")
  tabelavarnost2013 <- linkVarnost2013 %>% html_table(fill = TRUE)
  Varnost2013 <- tabelavarnost2013[[2]] %>% select(-Rank) # -Rank spusti stolpec z imenom Rang
  names(Varnost2013) <- c("Country", "Kriminal.2013", "Varnost.2013")
  return(Varnost2013)
}

uvozvarnost2014 <- function(Varnost2014){
  linkVarnost2014 <-read_html("https://www.numbeo.com/crime/rankings_by_country.jsp?title=2014&region=150")
  tabelavarnost2014 <- linkVarnost2014 %>% html_table(fill = TRUE)
  Varnost2014 <- tabelavarnost2014[[2]] %>% select(-Rank) # -Rank spusti stolpec z imenom Rang
  names(Varnost2014) <- c("Country", "Kriminal.2014", "Varnost.2014")
  return(Varnost2014)
}

uvozvarnost2015 <- function(Varnost2015){
  linkVarnost2015 <-read_html("https://www.numbeo.com/crime/rankings_by_country.jsp?title=2015&region=150")
  tabelavarnost2015 <- linkVarnost2015 %>% html_table(fill = TRUE)
  Varnost2015 <- tabelavarnost2015[[2]] %>% select(-Rank) # -Rank spusti stolpec z imenom Rang
  names(Varnost2015) <- c("Country", "Kriminal.2015", "Varnost.2015")
  return(Varnost2015)
}

uvozvarnost2016 <- function(Varnost2016){
linkVarnost2016 <-read_html("https://www.numbeo.com/crime/rankings_by_country.jsp?title=2016&region=150&fbclid=IwAR18q8KnRFu9mFaSipSE6DJV1JLjOxZEcuuDDD2doy-v6UfjLPWl2I0avR8")
tabelavarnost2016 <- linkVarnost2016 %>% html_table(fill = TRUE)
Varnost2016 <- tabelavarnost2016[[2]]%>% select(-Rank)
names(Varnost2016) <- c("Country", "Kriminal.2016", "Varnost.2016")
return(Varnost2016)
}

uvozvarnost2017 <- function(Varnost2017){
  linkVarnost2017 <-read_html("https://www.numbeo.com/crime/rankings_by_country.jsp?title=2017&region=150")
  tabelavarnost2017 <- linkVarnost2017 %>% html_table(fill = TRUE)
  Varnost2017 <- tabelavarnost2017[[2]] %>% select(-Rank)
  names(Varnost2017) <- c("Country", "Kriminal.2017", "Varnost.2017")
  return(Varnost2017)
}

uvozvarnost2018 <- function(Varnost2018){
  linkVarnost2018 <-read_html("https://www.numbeo.com/crime/rankings_by_country.jsp?title=2018&region=150")
  tabelavarnost2018 <- linkVarnost2018 %>% html_table(fill = TRUE)
  Varnost2018 <- tabelavarnost2018[[2]] %>% select(-Rank)
  names(Varnost2018) <- c("Country", "Kriminal.2018", "Varnost.2018")
  return(Varnost2018)
}

uvozvarnost2019 <- function(Varnost2019){
  linkVarnost2019 <-read_html("https://www.numbeo.com/crime/rankings_by_country.jsp?title=2018&region=150")
  tabelavarnost2019 <- linkVarnost2019 %>% html_table(fill = TRUE)
  Varnost2019 <- tabelavarnost2019[[2]] %>% select(-Rank)
  names(Varnost2019) <- c("Country", "Kriminal.2019", "Varnost.2019")
  return(Varnost2019)
}

uvozizobrazba <- function(Izobrazba){
  Izobrazba <- read.csv("podatki/Izobrazba.csv", skip = 5, sep = ",")
  obdrzistolpec <- c("Country","X2013","X2014", "X2015", "X2016","X2017","X2018","X2019")
  Izobrazba <- Izobrazba[ , obdrzistolpec]
  names(Izobrazba) <- c("Country","Izobrazba.2013","Izobrazba.2014","Izobrazba.2015","Izobrazba.2016", "Izobrazba.2017","Izobrazba.2018","Izobrazba.2019")
  Izobrazba <- head(Izobrazba, -17) #spustim zadnjih 17 stolpcev, ker je na koncu za vsako celino posebej
  Izobrazba <- Izobrazba %>% mutate(Country = str_trim(Country)) #znebim se presledka, ki je bil pred vsakim imenom države
  Izobrazba <- na.omit(Izobrazba)
  Izobrazba[,-1] <- as.data.frame(sapply(Izobrazba[,-1], as.numeric)) #spremenim tip stolpcev iz charachter v numeric
  Izobrazba[,-1] <- Izobrazba[,-1] * 100 #pomnožim * 100, da so enaki index safety in criminal 
  return(Izobrazba)
}

uvozipopulation <- function(Population){
  Population <- read.csv("podatki/Population.csv", skip = 4, sep = ",")
  obdrzistolpec <- c("Country.Name","X2013","X2014", "X2015", "X2016","X2017","X2018","X2019")
  Population <- Population[ , obdrzistolpec]
  names(Population) <- c("Country","Population.2013","Population.2014","Population.2015","Population.2016", "Population.2017","Population.2018","Population.2019")
  Population <- na.omit(Population)
  return(Population)
}

uvozplace <- function(Dohodek){
  linkdohodek <-read_html("https://en.wikipedia.org/wiki/List_of_countries_by_average_wage")
  Dohodek <- linkdohodek %>% html_table(fill = TRUE, dec = ",") #zelo pomemben tale dec = ,
  Dohodek <- Dohodek[[1]] 
  stolpci <- c("Country","2013","2014","2015","2016", "2017","2018","2019")
  Dohodek <- Dohodek[ ,stolpci]
  names(Dohodek) <- c("Country","Dohodek.2013","Dohodek.2014","Dohodek.2015","Dohodek.2016","Dohodek.2017","Dohodek.2018","Dohodek.2019")
  Dohodek <- Dohodek %>% mutate(Country = substr(Country, 1, nchar(Country) - 2)) #znebim se * in enega " ", ki je bil na koncu vsakega imena drzave
  Dohodek[,-1] <- Dohodek[,-1]*1000
  return(Dohodek)
}



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
