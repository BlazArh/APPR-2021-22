# Analiza podatkov s programom R - 2021/22

## Tematika
Za temo projekta sem si izbral analizo plač v EU. Zraven plač bomo analizirali tudi stopnjo varnosti, indeks izobrazbe in bdp na prebivalca.
Cilj naše projektne naloge bo, da ugotovimo npr. koliko visoka izobrazba in višina BDP-ja vplivata na višino plač in ali se ljudje, zaradi višje plače, posledično počutijo bolj varne.


## Začetne tabele


### tabela 1: bruto plače za leta 2016,2017,2018

Stolpci: države EU,leto,plače.

### tabela 2: izobrazba("education index" za leta 2016, 2017, 2018)

Stolpci: države EU, leto, E.I.

### tabela 3: stopnja varnosti(2016)

Stolpci: države EU, 2016, stopnja kriminala

### tabela 4: stopnja varnost(2017)

Stolpci: države EU, 2017, stopnja kriminala

### tabela 5: stopnja varnost(2018)

Stolpci: države EU, 2018, stopnja kriminala

Ko bom uvozil in počistil tabele 3,4,5, jih bom združil v eno tabelo.

### tabela 6: BDP

stolpci: evropske države, leta, bdp


## Končne tabele:


Naredil bom 3 tabele(2016,2017,2018)

stolpci: evropske države, leto, bruto plače, bdp, indeks izobrazbe, stopnja kriminala

## Viri

Tabela plač: https://en.wikipedia.org/wiki/List_of_European_countries_by_average_wage

Tabela izobrazbe: http://hdr.undp.org/en/indicators/137506

Tabela kriminal:
  - 2016: https://www.numbeo.com/crime/rankings_by_country.jsp?title=2016&region=150
  - 2017: https://www.numbeo.com/crime/rankings_by_country.jsp?title=2017&region=150
  - 2018: https://www.numbeo.com/crime/rankings_by_country.jsp?title=2018&region=150
  
Tabela BDP: https://ec.europa.eu/eurostat/databrowser/view/nama_10_pc/default/table?lang=en


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta)
aaaaaa
