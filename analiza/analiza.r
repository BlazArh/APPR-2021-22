# 4. faza: Analiza podatkov
library(ggalt)
library(ranger)


#Razvrščanje v skupine

#Hierarhično razvrščanje v skupine s funkcijo hclust
skupna_dendrogram <- skupna2 %>% filter(Leto == "2019")
skupna_dendrogram$Leto <- NULL
dendrogram <- skupna_dendrogram[,-1] %>% dist() %>% hclust(method= "ward.D")
plot(dendrogram, labels = skupna_dendrogram$Drzava, ylab = "višina", main = NULL)
rect.hclust(dendrogram, k = 2, border = "red")

# izračunamo tabelo s koleni za dendrogram
r = hc.kolena(dendrogram)

# narišemo diagram višin združevanja
diagram.kolena(r)

#Za število skupin se na podlagi kolen odločim za k = 2
skupine1 = dendrogram %>% cutree(k = 2) %>% as.ordered()


#Uporabimo metodo k-tih voditeljev
#Ugotavljamo optimalno število skupin
skupna_skupine <- skupna2 %>% filter(Leto == "2019")
skupna_skupine$Leto <- NULL
r.hc = skupna_skupine[, -1] %>% obrisi(hc = TRUE)
diagram.obrisi(r.hc)


# FALSE; 

r.km = skupna_skupine[, -1] %>% obrisi(hc = FALSE)
diagram.obrisi(r.km)
#Na obeh diagramih je optimalno število skupin k = 2


set.seed(42)
skupine2 = skupna_skupine[, -1] %>%
  kmeans(centers = 2) %>%
  getElement("cluster") %>%
  as.ordered()


#MEJE!!!!!

skupna_skupine["Skupina"] <- skupine2
skupna_skupine <- skupna_skupine[c("Drzava", "Skupina")]
zdruzitev_skupine <- left_join(zemljevid, skupna_skupine, by=c("ADMIN"="Drzava"))
slikazemljevid_skupine <- ggplot(zdruzitev_skupine) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Skupina )) + xlab("") + ylab("") + ggtitle("Skupine") + coord_cartesian(xlim=c(-30, 30), ylim=c(30, 70))
slikazemljevid_skupine <- slikazemljevid_skupine + guides(fill=guide_legend(title="Skupina"))
slikazemljevid_skupine


#NAPOVEDI

#Korelacija med spremenljivkami
slovenija <- skupna2 %>% filter(Drzava == "Slovenia")

kriminal_izobrazba <- ggplot(data=slovenija, aes(x= izobrazba, y= kriminal)) + geom_point() + geom_smooth(method="lm", formula = y ~x)
print(kriminal_izobrazba)

kriminal_dohodek <- ggplot(data=slovenija, aes(x= dohodek, y= kriminal)) + geom_point() + geom_smooth(method="lm", formula = y ~x)
print(kriminal_dohodek)


#Z uporabo linearne regresije bom napovedal prihodnji kriminal Slovenije glede na:



# predavanja, lin regresija, naključni gozdovi

#dohodek in izobrazba
lin.reg.varnost_izobrazba = lm(kriminal ~ dohodek + izobrazba, data = slovenija)
print(lin.reg.varnost_izobrazba)

#populacija in dohodek
lin.reg.populacija_dohodek = lm(kriminal ~ populacija + dohodek, data = slovenija)
print(lin.reg.populacija_dohodek)

#kao bolj kompleksno
lin.model1 = slovenija %>% ucenje(kriminal ~ dohodek + izobrazba, "lin.reg")
print(skupna2 %>% napaka_regresije(lin.model1, "lin.reg"))

ng.reg.model1 = slovenija %>% ucenje(kriminal ~ dohodek + izobrazba, "ng")
print(skupna2 %>% napaka_regresije(ng.reg.model1, "ng"))


lin.model2 = slovenija %>% ucenje(kriminal ~ populacija + dohodek, "lin.reg")
print(skupna2 %>% napaka_regresije(lin.model2, "lin.reg"))

ng.reg.model2 = slovenija %>% ucenje(kriminal ~ populacija + dohodek, "ng")
print(skupna2 %>% napaka_regresije(ng.reg.model2, "ng"))


#Najboljsi model je kriminal ~ dohodek + izobrazba z metodo nakljucnih gozdov
lm.napovedi <- predict(ng.reg.model1, data = slovenija)$predictions
print(lm.napovedi)
#ggplot(lm.napovedi)


#isto kot zgoraj. malo drugače
#Kot na vajah preizkusimo modele do vključno stopnje pet in na podlagi petkratnega prečnega 
# preverjanja izberemo tistega z najmanjšo kvadratno napako. 

set.seed(123)
podatki <- slovenija
k <- 5
formula <- kriminal ~ izobrazba


napaka.cv <- function(podatki, k, formula) {
  set.seed(123)
  n <- nrow(podatki)
  r <- sample(1:n)
  
  razrez <- cut(1:n, k, labels=FALSE)
  razbitje <- split(r, razrez)
  razbitje
  
  
  #pripravimo vektor za napovedi
  pp.napovedi  <- rep(0, n)
  
  for (i in 1:length(razbitje)) {
    #ucni podatki
    train.data <- podatki[-razbitje[[i]], ]
    #testni podatki
    test.data <- podatki[razbitje[[i]], ]
    
    #naucimo model na ucnih podatkih
    model <- lm(data= train.data, formula = formula)
    
    #napovedi za testne pdoatke
    napovedi <- predict(model, newdata = test.data)
    pp.napovedi[razbitje[[i]]] <- napovedi
  }
  
  napaka <- mean((pp.napovedi - podatki$kriminal)^2)
  return(napaka)
}
napaka.cv(slovenija, 5, kriminal ~ izobrazba)

formule <- c(kriminal ~ izobrazba,
             kriminal ~ izobrazba + I(izobrazba^2),
             kriminal ~ izobrazba + I(izobrazba^2) + I(izobrazba^3),
             kriminal ~ izobrazba + I(izobrazba^2) + I(izobrazba^3), 
             kriminal ~ izobrazba + I(izobrazba^2) + I(izobrazba^3),
             kriminal ~ izobrazba + I(izobrazba^2) + I(izobrazba^3) + I(izobrazba^4),
             kriminal ~ izobrazba + I(izobrazba^2) + I(izobrazba^3) + I(izobrazba^4) + I(izobrazba^5))

napake <- rep(0,5)
for (i in 1:5){
  formula <- formule[[i]]
  napaka <- napaka.cv(slovenija, 5, formula)
  napake[i] <- napaka
}

which.min(napake)
napake
model <- lm(data=slovenija, formula = kriminal ~ izobrazba)
