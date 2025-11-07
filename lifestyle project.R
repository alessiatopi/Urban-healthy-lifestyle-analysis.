rm(list=ls())
my_path=getwd()
setwd(my_path)

library(corrplot)
library(vcd)
library(olsrr)
library(car)
library(stats)
library(Matrix) 
library(pvclust)
library(mclust)
library(rstudioapi)
library(ggplot2)
library(factoextra)
library(cluster)
library(NbClust)
library(dplyr)
library(GGally)
library(plotly)
library(scatterplot3d)

############################################################################################################################################
### importo il dataset e gli assegno il nome  tab
tab=read.table(file="healthy_lifestyle_city_2021.csv", header=TRUE, sep=",")
View(tab)

#verifico che siano tutte variabili numeriche 
str(tab)

# elimino il simbolo "%" e "£" su alcune variabili
tab$Cost.of.a.bottle.of.water.City. <- gsub("£", "", tab$Cost.of.a.bottle.of.water.City.)
tab$Obesity.levels.Country. <- gsub("%", "", tab$Obesity.levels.Country.)
tab$Cost.of.a.monthly.gym.membership.City. <- gsub("£", "", tab$Cost.of.a.monthly.gym.membership.City. )

# Elimino le righe che hanno valori mancanti o vuoti nelle colonne di interesse
tab=tab[!(grepl("-", tab$Sunshine.hours.City.)), ]
tab=tab[!(grepl("-", tab$Pollution.Index.score...City.)), ]
View(tab)

#trasformo variabili chr in num
tab$Sunshine.hours.City.<- as.numeric(tab$Sunshine.hours.City.)
tab$Cost.of.a.bottle.of.water.City.<- as.numeric(tab$Cost.of.a.bottle.of.water.City.)
tab$Obesity.levels.Country.<- as.numeric(tab$Obesity.levels.Country.)
tab$Pollution.Index.score...City.<- as.numeric(tab$Pollution.Index.score...City.)
tab$Annual.avg..hours.worked<- as.numeric(tab$Annual.avg..hours.worked)
tab$Cost.of.a.monthly.gym.membership.City.<- as.numeric(tab$Cost.of.a.monthly.gym.membership.City.)

View(tab)
summary(tab)

#rinomino le colonne
names(tab)=c("City","Rank","Sunshine.hours", "Cost.of.a.bottle.of.water", "Obesity.levels", "Life.expectancy.years", "Pollution.index.score","Annual.avg.hours.worked", "Happiness.levels", "Outdoor.activities", "Number.of.take.out.places", "Cost.of.a.monthly.gym.membership" )

#elimino colonna 1 e 2 che sono categoriali, la 8 che ha troppi NA e le altre di poco interesse
tab1= tab [, -c(1, 2, 8, 11, 12)]
View(tab1)
summary(tab1)

#RIORDINO I DATI DEL DATASET IN MANIERA CASUALE (sono ordinati in base al ranking)
set.seed(200)
indici_casuali=sample(nrow(tab1))
tab.new=tab1[indici_casuali, , drop = FALSE]
View(tab.new) 


# Calcola l'IQR per la variabile life expectancy
Q1.2=quantile(tab.new$Life.expectancy.years, 0.25)
Q3.2=quantile(tab.new$Life.expectancy.years, 0.75)
IQR_value2= Q3.2 - Q1.2
# Identifica gli outlier
lower_bound2=Q1.2 - 1.5 * IQR_value2
upper_bound2=Q3.2 + 1.5 * IQR_value2
# Filtra il dataset rimuovendo gli outlier
tab.new1= subset(tab.new, Life.expectancy.years> lower_bound2 & Life.expectancy.years < upper_bound2)


#######################################################################################################################################################################################################################################################################################
#STATISTICA DESCRITTIVA
par(mfrow=c(1,7))
boxplot(tab.new1$Obesity.levels, col="violet", main="Obesity level")
boxplot(tab.new1$Life.expectancy.years, col="lightblue", main="Life expectancy")
boxplot(tab.new1$Pollution.index.score, col="lightgreen", main="Pollution index")
boxplot(tab.new1$Sunshine.hours, col="yellow", main="Sunshine hours")
boxplot(tab.new1$Happiness.levels, col="orange", main="Happiness level")
boxplot(tab.new1$Outdoor.activities, col="red", main="Outdoor activities")
boxplot(tab.new1$Cost.of.a.bottle.of.water, col="pink", main="Cost of a bottle of water")


# Calcola la matrice di correlazione
par(mfrow=c(1,1))
cor_matrix = cor(tab1)
View(cor_matrix)
# Visualizza la matrice di correlazione con corrplot
corrplot(cor_matrix, method = "color")

##############################################################################################################################################################################################################à
# REGRESSIONE LINEARE MULTIPLA
# creo il modello standardizzato

life_st=scale(tab.new$Life.expectancy.years)
sun_st=scale(tab.new$Sunshine.hours)
obesity_st=scale(tab.new$Obesity.levels)
pollution_st=scale(tab.new$Pollution.index.score)
happy_st=scale(tab.new$Happiness.levels)
outa_st=scale(tab.new$Outdoor.activities)
cost_st=scale(tab.new$Cost.of.a.bottle.of.water)

result.st=lm(life_st ~ sun_st + obesity_st + pollution_st + happy_st + outa_st , data=tab.new)
summary(result.st)

######################################################################################################################################################################
#proseguo con il modello non standardizzato perchè il dataset è già pesato

result=lm(Life.expectancy.years ~ Sunshine.hours + Obesity.levels + Pollution.index.score + Happiness.levels + Outdoor.activities + Cost.of.a.bottle.of.water, data=tab.new1)
summary(result)

#provo a migliorare il modello togliendo le variabili poco significative
#nel nostro caso sunshine hours era la meno significativa
result2=lm(Life.expectancy.years ~ Obesity.levels + Pollution.index.score + Happiness.levels + Outdoor.activities + Cost.of.a.bottle.of.water, data=tab.new1)
summary(result2)

#tolgo anche la pollution
result3=lm(Life.expectancy.years ~ Obesity.levels + Happiness.levels + Outdoor.activities + Cost.of.a.bottle.of.water, data=tab.new1)
summary(result3)

#verifica delle condizioni sugli errori: 
# normalità
ols_plot_resid_qq(result3)
ols_test_normality(result3)
# almeno due di essi devono avere p-value > 0.05
ols_plot_resid_hist(result3) 

#omoschedasticità
ncvTest(result3)
#Quello che dobbiamo osservare è il valore p: 
#se esso è maggiore di 0.05 possiamo accettare l’ipotesi nulla che la varianza degli errori sia costante. 


#assenza di autocorrelazione
durbinWatsonTest(result3)    
#Il test da utilizzare è il Durbin-Watson, che serve a verificare che gli errori non siano autocorrelati
#Il risultato di questo si considera accettabile per valori tra 1.5 e 2.5, 
# dove il valore 2 (ideale) indica assenza di autocorrelazione.


#collinearità 
vif(result3) 
#Quando il test ha un valore fino a 2 il risultato risulta ottimale, 
#da 2.5 a 4 invece è accettabile ma si potrebbe togliere qualche variabile. 

# Visualizzazione del grafico 3D
dev.new()
result4=lm(Life.expectancy.years ~ Happiness.levels + Outdoor.activities, data=tab.new1)
summary(result4)

s3d=scatterplot3d(tab.new1$Happiness.levels, tab.new1$Outdoor.activities, tab.new1$Life.expectancy.years, main="Regressione Lineare 3D",
                  pch=16, color="violet",
                  xlab="Happiness.levels", ylab="Outdoor.activities", zlab="Life.expectancy.years")

s3d$plane3d(result4, alpa=0.5, draw_polygon = TRUE, draw_lines=FALSE, col.grid="lightblue")




###################################################################################################################################################################################à
#CLUSTER ANALYSIS CON IL METODO DI WARD E LA DISTANZA DI CANBERRA NON SCALATA
rownames(tab1)=tab[,1]
View(tab1)

#calcolo la matrice distanze con il metodo CANBERRA
d2=dist(tab1, method="canberra")
summary(d2) #le distanze variano da circa 0.1 a circa 4 (da 1 a 1)

#uso la distanza di canberra non scalata
dendro_canberra=hclust(d2,method='ward.D2') #dendogramma con metodo di ward
plot(dendro_canberra,cex=0.5)
gruppi=rect.hclust(dendro_canberra, k=4) #disegno i tagli
# 4 sembrano andare bene

#metodo del gomito
#dove inizia ad esserci il primo salto più alto è la divisione migliore
matplot((nrow(tab1)-1):1, round(dendro_canberra$height,2),type="p",pch=21)
#sembrano 5 i gruppi ideali

#riguardo il dendro con i gruppi che mi ha suggerito il matplot
dendro_canberra.new=hclust(d2,method='ward.D2') 
plot(dendro_canberra.new,cex=0.5)
gruppi.2=rect.hclust(dendro_canberra.new, k=5)

# catturo i singoli gruppi rispetto ai risultati del matplot: bastano 5 gruppi
gruppi.finali=cutree(dendro_canberra.new,k=5)
View(gruppi.finali)

# GRUPPO 1
gruppo1=tab1[which(gruppi.finali==1),]
View(gruppo1)
summary(gruppo1)

# GRUPPO 2
gruppo2=tab1[which(gruppi.finali==2),]
View(gruppo2)
summary(gruppo2)

# GRUPPO 3
gruppo3=tab1[which(gruppi.finali==3),]
View(gruppo3)
summary(gruppo3)

#GRUPPO 4
gruppo4=tab1[which(gruppi.finali==4),]
View(gruppo4)
summary(gruppo4)

#GRUPPO 5
gruppo5=tab1[which(gruppi.finali==5),]
View(gruppo5)
summary(gruppo5)

#########################################################################################################################################################

# CLUSTER ANALYSIS CON LA DISTANZA DI CAMBERRA SCALATA
#cacolo la distanza scalata
d2_st=dist(scale(tab1,center=TRUE,scale=TRUE),method="canberra")
summary(d2_st)
# le distanze variano da 0.5 a circa 7, sono più grandi .

#uso la distanza di canberra scalata
dendro_canberra_st=hclust(d2_st,method='ward.D2') #dendogramma con metodo di ward
plot(dendro_canberra_st,cex=0.5)
gruppi_st=rect.hclust(dendro_canberra_st, k=4) #disegno i tagli
# ricorda: 6 e 4 sembrano andare bene

#metodo del gomito
#dove inizia ad esserci il primo salto più alto è la divisione migliore
matplot((nrow(tab1)-1):1, round(dendro_canberra_st$height,2),type="p",pch=21)
#noto che il primo salto più alto è dopo 3 gruppi, scelgo quelli

#riguardo il dendro con i gruppi che mi ha suggerito il matplot
dendro_canberra_st2=hclust(d2_st,method='ward.D2') 
plot(dendro_canberra_st2,cex=0.5)
gruppi.finali=rect.hclust(dendro_canberra_st2, k=3)

# catturo i singoli gruppi rispetto ai risultati del matplot: bastano 3 gruppi
gruppi.finali2<-cutree(dendro_canberra_st2,k=3)
View(gruppi.finali2)

# GRUPPO 1
gruppo1.st=tab1[which(gruppi.finali2==1),]
View(gruppo1.st)
summary(gruppo1.st)

# GRUPPO 2
gruppo2.st=tab1[which(gruppi.finali2==2),]
View(gruppo2.st)
summary(gruppo2.st)

# GRUPPO 3
gruppo3.st=tab1[which(gruppi.finali2==3),]
View(gruppo3.st)
summary(gruppo3.st)

#############################################################################################################à
#CLUSTER ANALYSIS CON IL METODO DI WARD E LA DISTANZA EUCLIDEA SCALATA

#cacolo la distanza euclidea scalata
d1_st=dist(scale(tab1,center=TRUE,scale=TRUE),method="euclidean")
summary(d1_st)
# le distanze variano da 0.4 a circa 8, sono più corte.

#uso la distanza di euclidea scalata
dendro_eucli=hclust(d1_st,method='ward.D2') 
plot(dendro_eucli,cex=0.5)
gruppi.e=rect.hclust(dendro_eucli, k=6) #disegno i tagli, idealmente 6

#metodo del gomito
#dove inizia ad esserci il primo salto più alto è la divisione migliore
matplot((nrow(tab1)-1):1, round(dendro_eucli$height,2),type="p",pch=21)
#noto che il primo salto più alto è dopo 7 gruppi, scelgo quelli

#rifaccio il dendro con i gruppi del grafico
dendro_eucli2=hclust(d1_st,method='ward.D2') 
plot(dendro_eucli2,cex=0.5)
gruppi.e2=rect.hclust(dendro_eucli2, k=7) 

# catturo i singoli gruppi
gruppi.finali.e<-cutree(dendro_eucli2,k=7)
View(gruppi.finali.e)

# GRUPPO 1
gruppo1.e=tab1[which(gruppi.finali.e==1),]
View(gruppo1.e)
summary(gruppo1.e)

# GRUPPO 2
gruppo2.e=tab1[which(gruppi.finali.e==2),]
View(gruppo2.e)
summary(gruppo2.e)

# GRUPPO 3
gruppo3.e=tab1[which(gruppi.finali.e==3),]
View(gruppo3.e)
summary(gruppo3.e)

# GRUPPO 4
gruppo4.e=tab1[which(gruppi.finali.e==4),]
View(gruppo4.e)
summary(gruppo4.e)

# GRUPPO 5
gruppo5.e=tab1[which(gruppi.finali.e==5),]
View(gruppo5.e)
summary(gruppo5.e)

# GRUPPO 6
gruppo6.e=tab1[which(gruppi.finali.e==6),]
View(gruppo6.e)
summary(gruppo6.e)

# GRUPPO 7
gruppo7.e=tab1[which(gruppi.finali.e==7),]
View(gruppo7.e)
summary(gruppo7.e)

##############################################################################################################
#Rappresentazione grafica
#Euclidea
# creo un nuovo dataset partendo da quello iniziale, a cui aggiungo la variabile gruppo di appartenenza
new_set.e=data.frame(tab1,as.factor(gruppi.finali.e))
names(new_set.e)=c(names(tab1),"Gruppo")
rownames(new_set.e)=tab[,1]
View(new_set.e)

#grafico 2d con cerchi shade=ombre
clusplot(new_set.e, new_set.e$Gruppo, main='Cluster solution - Euclidean method',
         color=TRUE, shade=FALSE,
         labels=2, cex.lab=0.5, lines=0) #NON CI PIACE
####################################################################################################################
#provo con gli altri metodi: canberra normale
new_set.c=data.frame(tab1,as.factor(gruppi.finali))
names(new_set.c)=c(names(tab1),"Gruppo")
rownames(new_set.c)=tab[,1]
View(new_set.c)

clusplot(new_set.c, new_set.c$Gruppo, main='Cluster solution - Canberra method',
         color=TRUE, shade=FALSE,
         labels=2, cex.lab=0.5, lines=0) #MIGLIORE SECONDO NOI
###########################################################################################################################
#provo con altri metodi:canberra scalata
new_set.c.st=data.frame(tab1,as.factor(gruppi.finali2))
names(new_set.c.st)=c(names(tab1),"Gruppo")
rownames(new_set.c.st)=tab[,1]
View(new_set.c.st)

clusplot(new_set.c.st, new_set.c.st$Gruppo, main='Cluster solution - Scaled Canberra method',
         color=TRUE, shade=FALSE,
         labels=2, cex.lab=0.5, lines=0) #BUONO MA POCHI GRUPPI

##############################################################################################################################################
#ANOVA SU CANBERRA-WARD
#H0 non ci sono differenze significative tra le medie dei gruppi
#H1= le medie sono uguali 
#pvalue piccolo: RIFIUTO H0, ci sono almeno due gruppi con medie significativamente diverse

anova_one_way=aov(new_set.c$Obesity.levels~Gruppo, data = new_set.c)
summary(anova_one_way) #OK

anova_one_way1=aov(new_set.c$Life.expectancy.years~Gruppo, data = new_set.c)
summary(anova_one_way1) #OK

anova_one_way2=aov(new_set.c$Pollution.index.score~Gruppo, data = new_set.c)
summary(anova_one_way2) #OK

anova_one_way3=aov(new_set.c$Sunshine.hours~Gruppo, data = new_set.c)
summary(anova_one_way3) #OK
# Il p-value non è bassissimo ma sulle ore di sole abbiamo il vincolo delle 24h.
#le medie non possono essere abissalmente diverse

anova_one_way4=aov(new_set.c$Cost.of.a.bottle.of.water~Gruppo, data = new_set.c)
summary(anova_one_way4)  #OK

anova_one_way5=aov(new_set.c$Happiness.levels~Gruppo, data = new_set.c)
summary(anova_one_way5) #OK

anova_one_way6=aov(new_set.c$Outdoor.activities~Gruppo, data = new_set.c)
summary(anova_one_way6)  #OK 

#ho fatto l'anova anche sulla canberra st: 
# i p-value andavano tutti bene tranne l'ultimo > 0.05
# ho lasciato la canberra non standardizzata perchè qui i p-value sono tutti piccoli

#BOXPLOT OBESITY
boxplot(new_set.c$Obesity.level[new_set.c$Gruppo==1],ylim=c(0,55))
par(new=TRUE)
boxplot(new_set.c$Obesity.level[new_set.c$Gruppo==2],ylim=c(0,55))
par(new=TRUE)
boxplot(new_set.c$Obesity.level[new_set.c$Gruppo==3],ylim=c(0,55))
par(new=TRUE)
boxplot(new_set.c$Obesity.level[new_set.c$Gruppo==4],ylim=c(0,55))
par(new=TRUE)
boxplot(new_set.c$Obesity.level[new_set.c$Gruppo==5],ylim=c(0,55))

#BOXPLOT SUNSHINE HOURS
boxplot(new_set.c$Sunshine.hours[new_set.c$Gruppo==1],ylim=c(0,4000))
par(new=TRUE)
boxplot(new_set.c$Sunshine.hours[new_set.c$Gruppo==2],ylim=c(0,4000))
par(new=TRUE)
boxplot(new_set.c$Sunshine.hours[new_set.c$Gruppo==3],ylim=c(0,4000))
par(new=TRUE)
boxplot(new_set.c$Sunshine.hours[new_set.c$Gruppo==4],ylim=c(0,4000))
par(new=TRUE)
boxplot(new_set.c$Sunshine.hours[new_set.c$Gruppo==4],ylim=c(0,4000))

# BOXPLOT COST OF A BOTTLE OF WATER
boxplot(new_set.c$Cost.of.a.bottle.of.water[new_set.c$Gruppo==1],ylim=c(0,3))
par(new=TRUE)
boxplot(new_set.c$Cost.of.a.bottle.of.water[new_set.c$Gruppo==2],ylim=c(0,3))
par(new=TRUE)
boxplot(new_set.c$Cost.of.a.bottle.of.water[new_set.c$Gruppo==3],ylim=c(0,3))
par(new=TRUE)
boxplot(new_set.c$Cost.of.a.bottle.of.water[new_set.c$Gruppo==4],ylim=c(0,3))
par(new=TRUE)
boxplot(new_set.c$Cost.of.a.bottle.of.water[new_set.c$Gruppo==5],ylim=c(0,3))

# BOXPLOT LIFE EXPECTANCY YEARS
boxplot(new_set.c$Life.expectancy.years[new_set.c$Gruppo==1],ylim=c(65,90))
par(new=TRUE)
boxplot(new_set.c$Life.expectancy.years[new_set.c$Gruppo==2],ylim=c(65,90))
par(new=TRUE)
boxplot(new_set.c$Life.expectancy.years[new_set.c$Gruppo==3],ylim=c(65,90))
par(new=TRUE)
boxplot(new_set.c$Life.expectancy.years[new_set.c$Gruppo==4],ylim=c(65,90))
par(new=TRUE)
boxplot(new_set.c$Life.expectancy.years[new_set.c$Gruppo==5],ylim=c(65,90))

# BOXPLOT PPLLUTION INDEX SCORE
boxplot(new_set.c$Pollution.index.score[new_set.c$Gruppo==1],ylim=c(0,100))
par(new=TRUE)
boxplot(new_set.c$Pollution.index.score[new_set.c$Gruppo==2],ylim=c(0,100))
par(new=TRUE)
boxplot(new_set.c$Pollution.index.score[new_set.c$Gruppo==3],ylim=c(0,100))
par(new=TRUE)
boxplot(new_set.c$Pollution.index.score[new_set.c$Gruppo==4],ylim=c(0,100))
par(new=TRUE)
boxplot(new_set.c$Pollution.index.score[new_set.c$Gruppo==5],ylim=c(0,100))

# BOXPLOT HAPPINESS LEVEL
boxplot(new_set.c$Happiness.levels[new_set.c$Gruppo==1],ylim=c(5,10))
par(new=TRUE)
boxplot(new_set.c$Happiness.levels[new_set.c$Gruppo==2],ylim=c(5,10))
par(new=TRUE)
boxplot(new_set.c$Happiness.levels[new_set.c$Gruppo==3],ylim=c(5,10))
par(new=TRUE)
boxplot(new_set.c$Happiness.levels[new_set.c$Gruppo==4],ylim=c(5,10))
par(new=TRUE)
boxplot(new_set.c$Happiness.levels[new_set.c$Gruppo==5],ylim=c(5,10))

# BOXPLOT OUTDOOR ACTIVITIES
boxplot(new_set.c$Outdoor.activities[new_set.c$Gruppo==1],ylim=c(0,1000))
par(new=TRUE)
boxplot(new_set.c$Outdoor.activities[new_set.c$Gruppo==2],ylim=c(0,1000))
par(new=TRUE)
boxplot(new_set.c$Outdoor.activities[new_set.c$Gruppo==3],ylim=c(0,1000))
par(new=TRUE)
boxplot(new_set.c$Outdoor.activities[new_set.c$Gruppo==4],ylim=c(0,1000))
par(new=TRUE)
boxplot(new_set.c$Outdoor.activities[new_set.c$Gruppo==5],ylim=c(0,1000))


#il test di Tukey è un test che aiuta a identificare quali gruppi hanno medie che differiscono in modo significativo.
#test di Tukey esegue tutte le possibili coppie di confronti tra le medie dei gruppi
#Il risultato del test di Tukey mostrerà gli intervalli di confidenza e i valori p corretti per tutte le coppie di gruppi.
#Se l'intervallo di confidenza non include zero e il valore p è inferiore al livello di significatività scelto 
#(solitamente 0.05), puoi concludere che le medie dei gruppi sono significativamente diverse.

#OBESITY
Comp=TukeyHSD(anova_one_way,ordered=TRUE,conf.level=0.95) 
View(Comp$Gruppo) 
Diverse=subset(Comp$Gruppo, Comp$Gruppo[,4]<0.025) 
View(Diverse) 

#LIFE EXP.
Comp1=TukeyHSD(anova_one_way1,ordered=TRUE,conf.level=0.95) 
View(Comp1$Gruppo) 
Diverse1=subset(Comp1$Gruppo, Comp1$Gruppo[,4]<0.025) 
View(Diverse1) 

#POLLUTION
Comp2=TukeyHSD(anova_one_way2,ordered=TRUE,conf.level=0.95) 
View(Comp2$Gruppo) 
Diverse2=subset(Comp2$Gruppo, Comp2$Gruppo[,4]<0.025) 
View(Diverse2) 

#SUNSHINE HOURS
Comp3=TukeyHSD(anova_one_way3,ordered=TRUE,conf.level=0.95) 
View(Comp3$Gruppo) 
Diverse3=subset(Comp3$Gruppo, Comp3$Gruppo[,4]<0.025) 
View(Diverse3) 
# i gruppi con medie significativamente diverse sono solo il 2 ed il quarto gruppo

#COST OF A BOTTLE OF WATER
Comp4=TukeyHSD(anova_one_way4,ordered=TRUE,conf.level=0.95) 
View(Comp4$Gruppo) 
Diverse4=subset(Comp4$Gruppo, Comp4$Gruppo[,4]<0.025) 
View(Diverse4) 

#HAPPINESS
Comp5=TukeyHSD(anova_one_way5,ordered=TRUE,conf.level=0.95) 
View(Comp5$Gruppo) 
Diverse5=subset(Comp5$Gruppo, Comp5$Gruppo[,4]<0.025) 
View(Diverse5) 

#OUTDOOR
Comp6=TukeyHSD(anova_one_way6,ordered=TRUE,conf.level=0.95) 
View(Comp6$Gruppo) 
Diverse6=subset(Comp6$Gruppo, Comp6$Gruppo[,4]<0.025) 
View(Diverse6) 
#I gruppi non hanno medie significatamente diverse rispetto a questa variabile


#creiamo un nuovo dataset senza le variabili in cui pochi gruppi avevano medie differenti:
#tolgo le outdoor activities e le sunshine hours
tab2= tab1 [, -c(1,7)]
View(tab2)

#faccio la silhuette dei gruppi senza le variabili
# avevo scelto la canberra classica
d.finale=dist(tab2, method="canberra")
dendro_canberra.finale=hclust(d.finale,method='ward.D2') 
sil=silhouette(cutree(dendro_canberra.finale, k=5), daisy(tab2))
View(sil)
plot(sil, nmax =45, cex.names=0.5)

#aumentando e diminuendo il numero di gruppi, mantenerne 5 sembra la soluzione migliore
# nel secondo, terzo e quinto gruppo la maggior parte delle città si avvicinando ad 1, dunque sono ben piazzate
# i gruppi in cui le città risultano non ben piazzate non si avvicinano proprio a -1 ma massimo a -0.5

#validazione dei cluster
validation=NbClust(tab2, distance="canberra",min.nc=2, max.nc=5, 
             method = "ward.D", index = "all")
View(validation$Best.partition) #migliore partizione
#consiglia due gruppi, ma 2 gruppi non andavano bene in tutte le altre analisi

#CONCLUSIONI
#abbiamo provato varie distanze con il metodo di ward e scelto i gruppi con il metodo del gomito
#la rappresentazione grafica dei gruppi ci hanno fatto scegliere la canberra normale
#abbiamo proseguito con l'anova che ci porta p-value tutti minori di 0.05
#con il test turkey abbiamo tolto le variabili sunshine hours e outdoor activities
#poichè non vi era una differenza significativa tra le medie dei gruppi
#infine la silhuette ci ha evidenziato che la maggior parte delle unità sono ben collocate

#sintesi grafica
d.finale2=dist(tab2, method="canberra")
dendro_canberra.finale2=hclust(d.finale,method='ward.D2') 
grp=cutree(dendro_canberra.finale2, k=5) 
View(grp)

new_data=data.frame(tab2, as.factor(grp))
names(new_data)=c(names(tab2),"Gruppo")
View(new_data)

#creo il grafico
dev.new()
grafico=group_by(new_data, new_data$Gruppo) %>% summarise (Cost.of.a.bottle.of.water=mean(Cost.of.a.bottle.of.water),
                                                           Obesity.levels=mean(Obesity.levels),
                                                           Life.expectancy.years=mean(Life.expectancy.years),
                                                           Pollution.index.score=mean(Pollution.index.score),
                                                           Happiness.levels=mean(Happiness.levels))



palette(c("blue","lightblue", "purple", "violet", "pink"))
to.draw=apply(grafico[,-1],2,function(x) x/max(x))
stars(to.draw, draw.segments = TRUE, scale=FALSE, key.loc = c(7.2,2.2),
      labels=c("Gruppo 1", "Gruppo 2", "Gruppo 3", "Gruppo 4", "Gruppo 5"), main="Profili dei gruppi", cex=0.75, flip.labels = TRUE, ncol = 3)



################################################################################################################################
#provo con il metodo del k-means
fit <- kmeans(tab2, 5, nstart=200)
fviz_cluster(fit,tab2)
View(fit)
View(fit$cluster)
print(fit)

str(fit)

#la within/devtot (varianza intra i gruppi) è il 15,5%
#la between/devtot (varianza tra i gruppi) è l'84,5%
#il k-means ci conferma che 5 gruppi sono ottimali, 
#seppur la suddivisione da lui fatta sia diversa

#silhuette sul k-means
sil2=silhouette(fit$cluster, dist(tab2))
View(sil2)
dev.new()
plot(sil2, nmax =45, cex.names=0.5)

# la silhuette ci conferma che la partizioni dei gruppi è ottimale: ogni città all'interno del gruppo
# è ben collocata ad eccezione di due città

#######################################################################################################################################
#PRINCIPAL COMPONENT ANALYSIS
rownames(tab2)=tab[,1]
View(tab2)
cov(tab2)
A=cor(tab2)
ggscatmat(tab2, columns = 1:5)
View(A)
#l'aspettativa di vita è correlata (+) principalmente al livello di felicità
#il livello di felicità è correlato (+) principalemnte con il costo dell'acqua
#la felicità è correlata (-) al livello di inquinamento

pca_health<-princomp(tab2, cor=TRUE, scores=TRUE)
View(pca_health)
print(summary(pca_health),loading=TRUE) 
fviz_eig(pca_health) 
#serve a vedere la varianza spiegata dalle componenti
#la prima componente e la seconda c. mi spiegano l'83% della variabilità
#la prima componente sembra rappresentare il benessere economico
#la seconda componente sembra rappresentare un'alimentazione salutare

#visualizzo le correlazioni con un grafico tra le componenti e le variabili principali
ggcorr(cbind(tab2, pca_health$scores), label = TRUE, cex = 2.5)

pca_health$scores 

################## GRAFICI #############################
#######################################################

pca_sc <- data.frame(pca_health$scores, country = tab[, 1])
grafico.col=ggplot(pca_sc, aes(x = Comp.1, y = Comp.2, color = country)) +
  geom_point() +
  geom_text(mapping = aes(label = country), vjust=1) +
  theme(legend.position = "none") +
  ggtitle("Punteggi Comp.1 Vs Comp.2")

grafico.col=grafico.col+labs(x="Benessere economico", y="Alimentazione salutare")
print(grafico.col) 

# altra rappresentazione grafica dei risultati di una PCA 
dev.new()
biplot(pca_health, choices = c(1,2), cex = c(.7, .7), col = c("gray", "orange"))
abline(h=0)
abline(v=0)

######## CLUSTER CON PCA ##############################
#######################################################à

#creo un dataframe con le città ed i relativi scores sulle 2 componenti
pca_group=data.frame(pca_health$scores[,1:2])
View(pca_group)

#guardo i gruppi con il metodo del k-means
fit.k=kmeans(pca_group, 5, nstart=200)
print(fit.k)
fviz_cluster(fit.k, pca_group, xlab = "Benessere economico", ylab = "Alimentazione salutare")

sil3=silhouette(fit.k$cluster, dist(pca_group))
plot(sil3, main="Silhouette plot of pca clusters", nmax =45, cex.names=0.5)

############################################################################################################################

