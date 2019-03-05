# Find out, which __year__ was the __most terrific__ for portfolio you have identified as __most profitable__ during the lesson and 
# show it on the chart using `ggplot2` package. Write an explanation about your findings into the code as comment.
# __Commit__ it to your repository into `Lessons/Lesson2/Homework`.

## Code
# #instalujeme balicek
install.packages("dplyr")
library(dplyr)

#nacitanie dat
setwd("H:/Aktuarstvo/GeneralInsurance_Class/Data")
dt_KPI <- read.csv("lesson2_KPI.csv")

#zobrazenie prvych par dat
dt_KPI %>% head

#zgrupneme data podla unit, pre kazdu unit vyratame profit, nascitame profity a nasledne to zoradime od najvacsieho profitu
dt_KPI %>% group_by(Unit) %>% mutate(profit=Premium-Expenses-Losses) %>% summarize(profit = sum(profit, na.rm = TRUE)) %>% arrange(desc(profit))

#zistili sme, ze najviac profitovy je Unit7, preto s nim dalej budeme pracovat
#vyberieme data prinaleziace unit7, pre kazde spravime profit, zgrupneme to podla rokov, zosumarizujeme profity pre jednotlive roky a zoradime od najmensieho profitu
dt_KPI %>% filter(Unit == "Unit7")  %>% mutate(profit=Premium-Expenses-Losses) %>% group_by(Year) %>% summarize(profit=sum(profit, na.rm = TRUE)) %>% arrange(profit)

#vyjde nam rok 2014

#instalujeme druhy balicek
install.packages("ggplot2")
library(ggplot2)

#vykreslenie rocnych profitov unit7
dt_KPI %>% filter(Unit == "Unit7")  %>% mutate(profit=Premium-Expenses-Losses) %>% group_by(Year) %>% summarize(profit=sum(profit, na.rm = TRUE)) %>% arrange(profit) %>% ggplot(aes(x = Year, y = profit)) + geom_col()

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 




# Your Explanation about analysis:
# nainstalovali sme balicek dplyr
# nacitali sme data
# zobrazili sme prvych par dat
#zgrupli sme data podla unit, pre kazdu unit vyratali profit, nascitali profity a nasledne to zoradili od najvacsieho profitu
#zistili sme, ze najviac profitovy je Unit7, preto sme s nim dalej pracovali
#vybrali sme si data prinaleziace unit7, pre kazde spravili profit, zgrupli to podla rokov, zosumarizovali profity pre jednotlive roky a zoradimli od najmensieho profitu
#zistili sme ze najmenej profitovym bol rok 2014
#nainstalovali sme balicek ggplot2
#graficky znazornili profity pre unit7 za kazdy rok
