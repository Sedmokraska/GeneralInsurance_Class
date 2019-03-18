# nacitanie potrebnych kniznic
library(dplyr)
library(ggplot2) 
library(ChainLadder)

#nacitanie dat
dt_PaidCase <- read.csv("./Data/lesson4_PaidCase.csv")

#.........................................................................................#
#.........................................................................................#
#.........................................................................................#

#pozrieme sa, co je v tych datach
summary(dt_PaidCase)

#case-rezervy; paid-realne, co sa vyplatilo
#nema velmi zmysel porovnavat velke skody (nap. hurikan) a male skody (napr. kradez), neodhadli by sme potom spodny trojuholnik dobre, preto jednotlive pripady budeme filtrovat aj na zaklade velkosti 

#.........................................................................................#
#................ 1. pripad: paid data for House business and Small claim size ...........#
#.........................................................................................#

#prefiltrujeme data
Paid_HH_sml <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Small" & dataset_type == "PAID" ) 
#zobrazime prvych par dat
head(Paid_HH_sml)

#spravime trojuholnik, pricom ay predstavuje cas, kedy sa stala nehoda a dy je cas, kedy su s tym spojene naklady; Sumofamount su nascitane naklady 
Paid_HH_sml_triangle <- Paid_HH_sml %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")

#vypiseme trojuholnik
Paid_HH_sml_triangle 
# vidime, ze prve udaje nam pre rok 2007 a 2008 chybaju,z coho vyplyva, ze pri vykresleni udajov nam v tychto rokov na zaciatku nastane ,,skos" sposobeny medzerou 

#vykreslime, aby sme lepsie vedeli citat infosky
plot(Paid_HH_sml_triangle)
#pozorujeme, ze v pripade domacnosti s malymi ,,udalostami" nam hodnota naakumulovanych nakladov velmi rychlo zkonverguje , viacmanej po tej 4-5 casovej jednotke sa to uz v ziadnom roku nemeni, mozeme hovorit o ,,short tail". Zaujimavym faktom je, ze v pripade 3. a 4. roku naklady na zaciatku velmi rychlo narastli, ocakavame, ze pri predikovani predbehnu hodnotami ostatne roky
plot(predict(chainladder(Paid_HH_sml_triangle))) 
#v predikcii vychadzaju najnakladovejsie roky 3 a 4; roky 5 a 6 sa za zaciatku troska lisili, no po uplynuti 5 tej casovej jednotky su uz v predikcii viacmenej totozne

#urcime aging factors 
ata(Paid_HH_sml_triangle) 
#pozrieme sa hlavne na vwtd hodnotu, nakolko je pouzietlnejsia z dovodu, ze pri vazenych hodnotach nepredpokladame, ze vsetko je ,,rovnake""
#tento ukazovatel nam hovori, akym cislom to mam dany bod  prenasobit,aby som sa dostal na koniec riadku ... vidime, ze s postupom casu nam hodnota klesa, co dava zmysel, kedze s postupom casu sa hodnota nakladov skoro uz nemeni



#.........................................................................................#
#................ 2. pripad: paid data for House business and Large claim size ...........#
#.........................................................................................#

#prefiltrujeme data
Paid_HH_lrg <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Large" & dataset_type == "PAID" ) 
#zobrazime prvych par dat
head(Paid_HH_lrg)

#spravime trojuholnik
Paid_HH_lrg_triangle <- Paid_HH_lrg %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")

#vypis trojuholnika
Paid_HH_lrg_triangle
#opat nam chybaju prve data v roku 2007 a 2008
# v porovnani so small nehodami, sa nam nascitane naklady z dvoj az ztrojnasobili... ak si vezmeme uz hore uvedeny pripad, ze small nehody mozu byt kradeze a large nejake pohromy, dava to zmysel, nakolko tie sposobuju rozsiahle skody u velke mnozstva domacnosti

#vykreslime, aby sme lepsie vedeli citat infosky
plot(Paid_HH_lrg_triangle)
#najprudsie na zaciatku rastu hodnoty nakladov pre 5ty rok, pravdepodobne sa tam stalo nieco zavazne skoda sa hromadne zacala vyplacat po 2 casovych jednotkach, no obdobne ako v predoslom pripade po tej 4 tej casovej jednotke to uz cca skonvergovalo a mozeme hovorit o ,,short tail".

#vykreslime predikovanie
plot(predict(chainladder(Paid_HH_lrg_triangle))) 

#urcime aging factors 
ata(Paid_HH_lrg_triangle) 
#oproti small velkosti kde prva hodnota vwtd bola 3.295 sa nam hodnota faktora znizila na 2.903. Tento vysledok je podporeny aj vykreslenim jednotlivych hodnot nakladov... kedze v tomto pripade dochadza k rychlejsiemu skonvergovaniu.



#.........................................................................................#
#............ 3. pripad: paid data for 3rd party business and Small claim size ...........#
#.........................................................................................#

#prefiltrujeme data
Paid_3rd_sml <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Small" & dataset_type == "PAID" ) 
#zobrazime prvych par dat, prisom 3rd party predstavuje auta
head(Paid_3rd_sml)

#spravime trojuholnik
Paid_3rd_sml_triangle <- Paid_3rd_sml %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")

#vypiseme trojuholnik
Paid_3rd_sml_triangle
#opat nam prve udaju pre rok 2007 a 2008 chybaju, no jendotlive hodnoty v tabulke nie su velmi vzdialene od hodnot pri domacnostiach v pripade small size

#vykreslime, aby sme lepsie vedeli citat infosky
plot(Paid_3rd_sml_triangle)
# avsak narozdiel od domacnosti, v pripade aut a nehod sa nam z obrazka ani v pripade 1 nezda, aby to uz dokonvergovalo, hovorime o ,,long tail". Ako sme si spominali na hodine, suvisi to s tym, ze v pripade nehod, sa postupne naklady suvisiace s opravou auta, popripade liecbou zranenych ,,objavuju" a nasledne pozaduju az casom.
plot(predict(chainladder(Paid_3rd_sml_triangle))) 
# z predikcie vidime, ze najnizsie hodnoty su pre rok 0 a najvyssie z predikcie pre 7. a 8. rok... co moze suvisiet aj s tym, ze s postupom casu je v spolocnosti ovela viac aut a teda aj automobilovych nehod

#urcime aging factors 
ata(Paid_3rd_sml_triangle) 
# narozdiel od domacnosti, kde vwtd v stlpci 9-10 dosahovalo hodnotu tesne pod 1, tu stale ostava nad 1, co tiez potvrdzuje ,,long tail"


#.........................................................................................#
#............ 4. pripad: paid data for 3rd Party business and Large claim size ...........#
#.........................................................................................#

#prefiltrujeme data
Paid_3rd_lrg <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Large" & dataset_type == "PAID" ) 
#zobrazime prvych par dat
head(Paid_3rd_lrg)

#spravime trojuholnik
Paid_3rd_lrg_triangle <- Paid_3rd_lrg %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")

#vykreslime, aby sme lepsie vedeli citat infosky
Paid_3rd_lrg_triangle
plot(Paid_3rd_lrg_triangle)
# po vykresleni jednotlive krivky od seba velmi odskakuju, ocividne v tomto pripade bude volatilita vacsia nez v predoslych pripadoch, zaujimave je, ze v pripade 5teho roku nastal po 5tej casovej jednotke opat prudky narast.

plot(predict(chainladder(Paid_3rd_lrg_triangle))) 
#v predikcii nam nulty rok od ostatnych velmi odskakuje

#urcime aging factors 
ata(Paid_3rd_sml_triangle) 




#_If you are now comforatble with what this does, try doing the same, but using additional information: The Case data!_
#Hint: Sum Paid and Case together to come up with the final claims estimates (the Incurred claims)


#.........................................................................................#
#.......... 4. pripad: paid and case data for House business and Small claim size ........#
#.........................................................................................#

#prefiltrujeme data
Paca_HH_sml <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Small" ) 
#zobrazime prvych par dat
head(Paca_HH_sml)

#spravime trojuholnik 
Paca_HH_sml_triangle <- Paca_HH_sml %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")

#vykreslime, aby sme lepsie vedeli citat infosky
Paca_HH_sml_triangle
plot(Paca_HH_sml_triangle)
#narozdiel od uvazovania iba vyplatenych nakladov, v tomto pripade pozrujeme klesajuci trend, suvisi to s tym, ze uvazujeme aj rezervy a tie sa nam casom zmensuju
plot(predict(chainladder(Paca_HH_sml_triangle))) 
#no opat ako v pripade paid mozeme hovorit o ,,short tail"

#urcime aging factors 
ata(Paca_HH_sml_triangle) 
#klesajuci trend vyvoja nakladov potvrdzuje aj fakt, ze vacsina hodnot vwtd je mensia nez 1


#.........................................................................................#
#.......... 2. pripad: paid and case data for House business and Large claim size ........#
#.........................................................................................#

#prefiltrujeme data
Paca_HH_lrg <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Large") 
#zobrazime prvych par dat
head(Paca_HH_lrg)

#spravime trojuholnik
Paca_HH_lrg_triangle <- Paca_HH_lrg %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")

#vykreslime, aby sme lepsie vedeli citat infosky
Paca_HH_lrg_triangle
plot(Paca_HH_lrg_triangle)
plot(predict(chainladder(Paca_HH_lrg_triangle))) 
#podobne ako pri paid, nastava v pripade large velkosti konvergencia rychlejsie

#urcime aging factors 
ata(Paca_HH_lrg_triangle) 
#vsetky hodnoty vwtd osciluju okolo hodnoty 1

#.........................................................................................#
#....... 3. pripad: paid and case data for 3rd party business and Small claim size .......#
#.........................................................................................#

#prefiltrujeme data
Paca_3rd_sml <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Small") 
#zobrazime prvych par dat
head(Paca_3rd_sml)

#spravime trojuholnik
Paca_3rd_sml_triangle <- Paca_3rd_sml %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")

#vykreslime, aby sme lepsie vedeli citat infosky
Paca_3rd_sml_triangle
plot(Paca_3rd_sml_triangle)
plot(predict(chainladder(Paca_3rd_sml_triangle))) 
#podobne ako v pripade paid, konvergencia ani po 10tich casovych jednotkach nenastava

#urcime aging factors 
ata(Paca_3rd_sml_triangle) 


#.........................................................................................#
#....... 4. pripad: paid and case data for 3rd Party business and Large claim size .......#
#.........................................................................................#

#prefiltrujeme data
Paca_3rd_lrg <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Large") 
#zobrazime prvych par dat
head(Paca_3rd_lrg)

#spravime trojuholnik
Paca_3rd_lrg_triangle <- Paca_3rd_lrg %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")

#vykreslime, aby sme lepsie vedeli citat infosky
Paca_3rd_lrg_triangle
plot(Paca_3rd_lrg_triangle)
plot(predict(chainladder(Paca_3rd_lrg_triangle))) 
#ani v tomto pripade konvergencia nenastava, avsak narozdiel od small velkosti, vidime na konci v kazdom roku mensi pokles hodnoty naakumulovanych nakladov

#urcime aging factors 
ata(Paca_3rd_sml_triangle) 
