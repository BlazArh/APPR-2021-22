# Analiza podatkov s programom R - 2021/22

## Tematika
Za temo projekta sem si izbral analizo plač v Evropi. Zraven plač bomo analizirali tudi stopnjo kriminala, indeks izobrazbe in bdp.
Cilj naše projektne naloge bo, da analiziramo vpliv teh 3 spremenljivk na bruto plače po Evropi.


## Začetne tabele

### tabela 1: bruto plače za leta 2016,2017,2018
Stolpci: evropske države,leto,plače.

### tabela 2: izobrazba("education index" za leta 2016, 2017, 2018)
Stolpci: evropske države, leto, E.I.

### tabela 3: stopnja kriminala(2016)
Stolpci: evropske države, 2016, stopnja kriminala

### tabela 4: stopnja kriminala(2017)
Stolpci: evropske države, 2017, stopnja kriminala

### tabela 5: stopnja kriminala(2018)
Stolpci: evropske države, 2018, stopnja kriminala

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
  - 2016:
  - 2017:
  - 2018:
Tabela BDP: https://ec.europa.eu/eurostat/databrowser/view/sdg_08_10/default/table?lang=en


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
