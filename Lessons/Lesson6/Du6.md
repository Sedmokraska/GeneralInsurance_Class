#nacitanie kniznic
library(dplyr)
library(ggplot2)

knitr::opts_chunk$set(echo = TRUE)


#nacitanie dat
dt_pol_w_claims <- readRDS("C:/Users/Simi/Documents/GeneralInsurance_Class/Data/lesson6_dt_pol_w_claims.rds")


#nastavime seed, aby sa nam pri randomize generovalo to iste
set.seed(58742)
ind <- sample(2, nrow(dt_pol_w_claims), replace=TRUE, prob=c(0.80, 0.20)) 
# rozdelime data, aby sme mali training vzorku a aj vzorku na otestovanie kvality -validation
dt_pol_w_claims <- mutate(dt_pol_w_claims,
                data_status = ifelse(ind == 1, 
                                     "Training",
                                     ifelse(ind == 2, 
                                            "Validation", 
                                            "Unseen")
                )
)
train <- dt_pol_w_claims %>% filter(data_status == "Training")
val <- dt_pol_w_claims %>% filter(data_status == "Validation")


#definovanie mse
mse <- function(prediction, actual){
  return(sum((prediction-actual)^2, na.rm = TRUE)/length(prediction))
}


#model z predoslej du: zavislost burning cost od D_age_banded a Construct_year
model <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ D_age_banded + Construct_year,
              family = Gamma())
summary(model)
# v tomto pripade nam zcasti vychadzali vyznamne vekove skupiny 35-40 a 40-45, no aj to nie s vela hviezdickami...tuto kombinaciu sme nepovazovali za dobru na predikovanie burning cost.



#teraz sa pokusime nas model vylepsit, no predtym, nez nieco v nom zmenime, spravime model z predoslej du na training datach, aby sme na validation vzorke zistili, aku to ma hodnotu MSE
model1a <- glm(data = train,
              formula = Burning_Cost ~ D_age_banded + Construct_year,
              family = Gamma())
summary(model1a) 
#vidime, ze AIC nam oproti modelu na celej datovej vzorke kleslo, taktiez nam viac parametrov vyslo signifikantnych

# spravime predikciu na validation dataset a nasledne urcime mse
prediction <- predict(model1a, val, type = "response")
mse(prediction, val$Burning_Cost) 
#hadze nam to pri predikovani error, faktor D_age_banded nema v oboch vzorkach pokryte vsetky nadobudane hodnoty, navyse aj pri samotnom modelovani v predoslej du nam nevychadzal velmi sgnifikantne

#........................................................................................

#FEATURE ENGINEERING - skusime do nasho modelu pridat namiesto faktora D_age_banded iny faktor
#pozrieme sa, ako faktory su obsiahnute v datach - vypiseme par prvych zaznamov
head(dt_pol_w_claims) 
#skusme do nasho modelu zamenit faktor Veh_type1, ktory hovori o ucele pouzivania vozidla
model2a <- glm(data = train,
              formula = Burning_Cost ~ Construct_year + Veh_type1,
              family = Gamma())
summary(model2a) 
#oproti model1a sa nam tu vyskytuje parametre so signifikantnostou az 3 hviezdiciek, 
#AIC nam kleslo o 41 - to je dobre

# spravime predikciu na validation dataset
prediction <- predict(model2a, val, type = "response")
#vypocitame mse
mse(prediction, val$Burning_Cost) 
#288.67


# skusme do modelu pridat este dalsi faktor Customer_Type
#podme sa pozriet na to, ci nam zvazenie tohto aspektu v modeli pomoze
model3a <- glm(data = train,
              formula = Burning_Cost ~ Construct_year + Veh_type1 + Customer_Type,
              family = Gamma())
summary(model3a) 
# spravime predikciu na validation dataset
prediction <- predict(model3a, val, type = "response")
#vypocitame mse
mse(prediction, val$Burning_Cost) 
#288.45 ... vidime, ze mse sa nam trocha znizilo - nastalo teda zlepsenie


#pozrime sa este na faktor Construct_year
source("Support/emb_chart.R")
emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(model3a, train, type = "response"))),
  x_var =  "Construct_year",
  target = "Burning_Cost",
  prediction =  "pred"
  )
# vidime, ze pred rokom 2002 je tam velky vykyv, no tiez tam spada male mnozstvo udajov, co ma negativny dopad na trend


#aby sme zvacsili pocet pozorovani .... zlucme tieto male skupiny dovedna, spravime tzv. capping
train <- train %>% mutate(Construct_year = ifelse(Construct_year <= 2002, 2002, Construct_year))


#pozrime sa na model, ako bude vyzerat v tomto pripade
model4a <- glm(data = train,
              formula = Burning_Cost ~ Construct_year + Veh_type1 + Customer_Type,
              family = Gamma())
summary(model4a)
#AIC nam kleslo o hodnotu 1

# spravime predikciu na validation dataset
prediction <- predict(model4a, val, type = "response")
#vypocitame mse
mse(prediction, val$Burning_Cost) 
#288.4086 ... vidime, ze aj mse nam kleslo, co je dobre :)



#pozrime sa este na faktor Customer_Type
emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(model4a, train, type = "response"))),
  x_var =  "Customer_Type",
  target = "Burning_Cost",
  prediction =  "pred"
  )
# tento faktor nadobuda len 2 hodnoty, no oba su viditelne zastupene, nema smysel nieco zlucovat


#skonstatovanie:
#model4a je lepsim nez nas povodny model, avsak aj tak by sa dal este vylepsovat, urcite by sme mohli skusit zvazit dodat dalsie faktory... a podla toho, ci by hodnota mse klesala alebo nie by sme usudzovali, ci to bol rozumny tah, tiez by sme v pripade Veh_type mohli skusit napr. zlucit vsetky trucky-dokopy, vsetky taxiky dokopy..., cim by nam vzniklo viac zaznamov v danych skupinach, alebo skusit spravit transformaciu na time_on_book, mozno nejaky logartmus