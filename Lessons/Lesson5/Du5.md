#nacitanie kniznic
library(dplyr)
library(ggplot2)


#nacitanie Policy data
dt_Policy <- read.csv("C:/Users/Simi/Documents/GeneralInsurance_Class/Data/lesson5_PolicyHistory.csv") %>% distinct(NrPolicy, NrObject, .keep_all = TRUE) 
dt_Policy %>% nrow() #pocet riadkov
dt_Policy %>% distinct(NrPolicy, NrObject) %>% nrow() 
#distinc - pocet riadkov, ktore maju unikatnu kombinaciu
# nastala rovnst cisel - nemame tam ziadne duplikaty


#nacitanie Claims data
dt_Claims <- read.csv("C:/Users/Simi/Documents/GeneralInsurance_Class/Data/lesson5_Claims.csv") %>% distinct(NrClaim, .keep_all = TRUE)
dt_Claims %>% nrow()
dt_Claims %>% distinct(NrClaim) %>% nrow()


#To properly bring the claims info on policy + object level we need to ensure claims data are also on that level. Lets check it and if they are not, roll them up to that level.
dt_Claims %>% distinct(NrPolicy, NrObject) %>% nrow()
## they are on required level as no. of unique rows at level equals to no. of rows for raw dataset


# Lets finally join them together.
dt_pol_w_claims <- left_join(dt_Policy, 
                             dt_Claims, 
                             by = c("NrPolicy", "NrObject")
)
#vypisanie prvych par riadkov
head(dt_pol_w_claims) 
# v du sa pozriem na zavislost Burning cost od Construct_year a D_age_banded


# how much claims we joined?
dt_pol_w_claims %>% group_by(is.na(Paid)) %>% summarise(cnt = n())
#2100 skod nastalo na 28 217 poistiek (ti nemali vyplatene claimy)
#kazdy cca 15ty clovek nabural
  

# definovanie burning cost
dt_pol_w_claims <- 
  dt_pol_w_claims %>% 
  mutate(Ult_Loss = Paid + Reserves,
         Burning_Cost = ifelse(is.na(Ult_Loss), 0,  Ult_Loss / as.integer(Time_Exposure))
  )


#For the first one-way analysis we will try to explore feature about vehicle type of client: Veh_type2
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Veh_type2)) + 
  geom_jitter()
#na y-ovej osi su burning cost
#pri car vidime ze ta disperzia je velka, vela outlierov...auta vyplacaju vacsie skody
#skody pri motorbikoch su viacmenej konstantne
# z other nemozeme vyvodit nic, lebo tam mame len 5 pozorovani...museli by sme napr. spojit tractory, other a motorbike
#tento faktor by sme do modelu asi zamietli, ani jedno nie je take vyrazne, ze by sme mohli usudit,,ze to sposobuje nehody"


#ideme modelovat
dt_pol_w_claims %>% 
  group_by(Veh_type2) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
#vacsina poistenych nemala ziadne burning cost...median je 0


dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Veh_type2)) + 
  geom_boxplot() +
  ylim(0, 100)
#Two things happend here:
# - outliers screw it up so much, that this feature is definitelly not good predictor alone, we are not able to explain those outliers using only one feature describing vehicle type.
# - but...there is definitelly some trend, which might be usefull in next stages of modelling. (saying that there is definitelly some trend might be too strong and we should use also other methods to confirm this. e.g. ANOVA)


# na GLM netreba ziaden speci package
knitr::opts_chunk$set(echo = TRUE)


### First Model #nulove burning cost nas nezaujimaju a velke su outliere(klienti, ktori boli s nami kratko a sposibili velku skodu)
#ak chceme specifikovat vsetky premenne v modeli okrem burning costov..tak namiesto Veh_type2 dame bodku
#GLM model je prepojeny s gama rozdelenim...to pouzivame najcastie na modelovanie spojitych(napr. burning costy)...na skody, diskretne rozdelene, najcastejsie pouzivame poisonovo rozdelenie
model1 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ Veh_type2,
              family = Gamma())
summary(model1)









#.................................................................................
#.................................... U1 .........................................
#.................................................................................

# Use One-way analysis to find out 2 more features like Veh_type2, which can be usefull in   the GLM model and might have potiantial influence on our target.

# ako prve sa pozrieme na zavislost Burning cost od Construct_year
# moj osobny predpoklad: novsie auta jazdia rychlejsie a maju preto castejsie nehody

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Construct_year)) + 
  geom_jitter()
#vidime, ze najviac zaznamov (aut) je okolo roku vyroby 2008-2014, co pravdepodobne znacne ovplyvnuje fakt, ze disperzia v tychto rokoch je velka a tiez sa tam vyskytuju outlieri, avsak mame viacero zaznamov aj okolo roku 2000, no tam vidime, ze burning cost su viacmenej konstantne  

dt_pol_w_claims %>% 
  group_by(Construct_year) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))

# median je vsade nulovy, vacsina aut nemala ziadnu nehodu, avsak z priemernych hodnot burning cost sa nam nas tusak potvrdzuje.... na druhom a tretom mieste s najvacsou priemernou hodnotou su roky 2011 a 2009
#zauimavoustou je, ze na prvom mieste skoncil rok 1997, avsak tam mame len 30 udajov, co je prilis malo na vyvodzovanie nejakych zaverov


dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Construct_year)) + 
  geom_boxplot() +
  ylim(0, 100)
# to nam vyhadzuje info len o roku 1997, ktory ma malo udajov


# ...................................................................................


# teraz sa pozrime ako suvisi Burning cost a D_age_banded
# moj osobny predpoklad: mladsie vekove skupiny jazdia rychlejsie a maju preto castejsie nehody, no na druhej strane starsi ludia su velmi nepozorni vodici a tiez tam bude nehodovost stupat

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age_banded)) + 
  geom_jitter()
#jednotlive vekove skupiny su v rozmedzi 5tich rokov, pricom najviac zaznamov sa javi pre skupiny 40-45 a 45-50, avsak ako sme si spominali na hodine, to moze suvisiet s tym, ze deti takto starych ludi zacinaju jazdit na autach rodicov
# skupiny 30-35 a 35-40 sa az na par outlierov javia byt v celku konstanstne s malou disperziou
# skupiny v dochodku su viacmenej konstantne az na par outlierov, avsak oproti skupinam 30-40 tu mame menej zaznamov, co je sposobene pravdepodobne tym, ze starsi ludia skor uprednostnia v takomto veku hromadnu dopravu 


dt_pol_w_claims %>% 
  group_by(D_age_banded) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
# median je vsade nulovy, vacsina aut nemala ziadnu nehodu, avsak z priemernych hodnot vyplyva, ze najvacsie priemerne BC dosahuju ludia v dochodkovom veku, na prvom mieste su skupiny 60-65 (ta je aj vcelku dost poctovo zastupena) a potom s mensim poctovym zastupeni je skupina 75-80
# mladsi vodici resp. strednovekovci sa na prvych miestach neumiestnuju


dt_pol_w_claims_nove %>% 
  ggplot(aes(y = Burning_Cost, x = D_age_banded)) + 
  geom_boxplot() +
  ylim(0, 100)
#vidime, ze najvyssie hodnoty sa dosahuju v skupinach 40-45 a 45-50... co moze nasvedcovat nerozumnym, casto riskantnym jazdam deti rodicov




#.................................................................................
#.................................... U2 .........................................
#.................................................................................
#Create simple GLM model with features you have found in previous exercise. Is the model 
#predictive? Are you satisfied with your model? Use your knowledge about GLM theory to 
#evaluate model, make a suggestion what could be improved and what would be your next steps. 


# pre pripad hladania suvislosti s Construct_year
model2 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ Construct_year,
              family = Gamma())
summary(model2)
#ani jeden z parametrov nam nevychadza vyznamny, tymto modelom by sme nevedeli nic predikovat, vobec s nim nie som spokojna


# pre pripad hladania suvislosti s D_age_banded
model3 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ D_age_banded,
              family = Gamma())
summary(model3)
# z parametrov sa tu ponuka iba D_age_banded 35 <=  x < 40, ktoreho odhadovana hodnota je 0.055514, z toho usudzujem, ze pri urcovani vysky poistneho by sa mohlo zohladnit to, ci vodic nahodou nespada do tejto skupiny. (avsak najprv by to chcelo hlpsiu analyzu pre jednotlivy vek vodica)
# no na predikovanie burning cost to podla mna nestaci ... aj ked nam aspon z casti vyssiel v tomto pripade jeden parameter signifikantny, aj to nie s uplnym poctom hviezdiciek



# mozeme sa este na to pozriet, ako by to vyzeralo, ak by sme skumali zavislost od oboch naraz
model4 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ D_age_banded + Construct_year,
              family = Gamma())
summary(model4)
# v tomto pripade nam zcasti vyznamne vychadzaju vekove skupiny 35-40 a 40-45, no aj to nie s vela hviezdickami...ani tuto kombinaciu nepovazujem za dobru na predikovanie burning cost.


# v dalsom kroku by som skusila hladat suvis medzi BC a nejakymi inymi skupinami hodnot, mozno Veh_type 1 , ucel pouzivania aut by nam mohol priniest rozumnejsie vysledky... popripade by som skusala hladat suvislost medzi BC a viacerymi inymi faktormi naraz, avsak to by mohlo byt znacne neprehliadnejsie





