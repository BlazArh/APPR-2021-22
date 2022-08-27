# 4. faza: Analiza podatkov
library(ggalt)
library(ranger)


#Razvrščanje v skupine

#Hierarhično razvrščanje v skupine s funkcijo hclust
skupna_mean <- skupna2 %>% group_by(Drzava) %>% summarise(dohodek = mean(dohodek),
                                                              populacija = mean(populacija),
                                                              izobrazba = mean(izobrazba),
                                                              kriminal = mean(varnost),
                                                              varnost = mean(varnost))

dendrogram <- skupna_mean[,-1] %>% scale()
rownames(dendrogram) <- skupna_mean$Drzava
dendrogram <- dendrogram %>% dist() %>% hclust(method= "ward.D")

#plot(dendrogram)

# izračunamo tabelo s koleni za dendrogram
r = hc.kolena(dendrogram)

# narišemo diagram višin združevanja
diagram.kolena(r)


#Za število skupin se na podlagi kolen odločim za k = 3
skupine1 = dendrogram %>% cutree(k = 3) %>% as.ordered()


#Uporabimo metodo k-tih voditeljev
#Ugotavljamo optimalno število skupin

r.km = skupna_mean[, -1] %>% obrisi(hc = FALSE)
diagram.obrisi(r.km)



set.seed(42)
skupine2 = skupna_mean[, -1] %>%
  kmeans(centers = 2) %>%
  getElement("cluster") %>%
  as.ordered()


#MEJE!!!!!

skupna_mean["Skupina"] <- skupine1
skupna_mean1 <- skupna_mean[c("Drzava", "Skupina")]
zdruzitev_skupine1 <- left_join(zemljevid, skupna_mean1, by=c("ADMIN"="Drzava"))
slikazemljevid_skupine1 <- ggplot(zdruzitev_skupine1) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Skupina )) + xlab("") + ylab("") + ggtitle("Skupine") + coord_cartesian(xlim=c(-30, 30), ylim=c(30, 70))
slikazemljevid_skupine1 <- slikazemljevid_skupine1 + guides(fill=guide_legend(title="Skupina"))
slikazemljevid_skupine1


skupna_mean["Skupina"] <- skupine2
skupna_mean2 <- skupna_mean[c("Drzava", "Skupina")]
zdruzitev_skupine2 <- left_join(zemljevid, skupna_mean2, by=c("ADMIN"="Drzava"))
slikazemljevid_skupine2 <- ggplot(zdruzitev_skupine2) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Skupina )) + xlab("") + ylab("") + ggtitle("Skupine") + coord_cartesian(xlim=c(-30, 30), ylim=c(30, 70))
slikazemljevid_skupine2 <- slikazemljevid_skupine2 + guides(fill=guide_legend(title="Skupina"))
slikazemljevid_skupine2

zlepek <- plot_grid(slikazemljevid_skupine1, slikazemljevid_skupine2)
zlepek
#NAPOVEDI

#Korelacija med spremenljivkami
slovenija <- skupna2 %>% filter(Drzava == "Slovenia")

# prvi model
m1 <- lm(kriminal ~ izobrazba, data = slovenija)
# drug model
m2 <- lm(kriminal ~ izobrazba + dohodek, data = slovenija)

# kateri model je boljši?
napaka.cv <- function(podatki, k, formula) {
  set.seed(123)
  n <- nrow(podatki)
  r <- sample(1:n)
  
  razrez <- cut(1:n, k, labels=FALSE)
  razbitje <- split(r, razrez)
  
  
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

napaka.cv(slovenija, 2, kriminal ~ izobrazba)
napaka.cv(slovenija, 2, kriminal ~ izobrazba + I(izobrazba^2))
napaka.cv(slovenija, 2,  kriminal ~ izobrazba + dohodek)
# srednji model je boljši


m3 = lm(kriminal ~ izobrazba + I(izobrazba^2), data = slovenija)
param <- m3$coefficients
beta0 <- param[1]
beta1 <- param[2]
beta2 <- param[3]

summary(m3)
data_napoved = slovenija[,c(5:6)]
data_napoved$napoved <- beta0 + beta1 * data_napoved$izobrazba + beta2 * data_napoved$izobrazba^2

iz <- seq(88, 92, by = 0.5)
nap <- beta0 + beta1 * iz + beta2 * iz^2
n <- length(iz)
df = data.frame(izobrazba = iz, kriminal = rep(NA, n), napoved = nap)

data_napoved = rbind(data_napoved, df)
napoved <- ggplot(data=data_napoved, aes(x= izobrazba, y= kriminal)) + geom_point() +
  geom_line(aes(y = napoved), color = "red")+
  xlab("Stopnja izobrazbe") + ylab("Indeks kriminala")

napoved









