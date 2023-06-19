# Modele cu date de tip panel

# Fisiere: 
#   happy_rank1.xls

# Install packages
PackageNames <- c("tidyverse","gplots","plm","readxl","foreign","lmtest")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Exemplu: Analiza gradului de fericire al oamenilor din Europa
# la nivelul tarilor din Europa in perioada 2015-2022
data <- read_excel("C:/Users/Ada/Desktop/Proiect Econometrie/Partea2/Pro2/happy_rank1.xls")

summary(data)

# Declararea setului de date de tip panel
pd.df <- pdata.frame(data, index = c("Country","Year"), drop.index = TRUE)


# Corelatia dintre Happiness_Rank/spatiu/timp
coplot(Happiness_Rank ~ Year|Country, type="l", data=data) 

# Heterogeneitatea presupune ca exista diferente intre unitatile studiate

# Explorarea heterogeneitatii in sectiunea transversala
# Graficul traseaza un interval de incredere de 95% in jurul mediilor
# Tari cu rata foarte mare si tari cu rata foarte mica => avem heterogeneitate transversala
plotmeans(Happiness_Rank ~ Country, main = 'Heterogeneitate in randul tarilor', data = data)

# Explorarea heterogeneitatii in sectiunea temporala
# Ani cu rata mare si ani cu rata mica => avem heterogeneitate temporala, 
# dar mai mica decat in cazul heterogeneitatii transversale
plotmeans(Happiness_Rank ~ Year, main = 'Heterogeneitate in timp', data = data)


# Model OLS - model clasic de regresie liniara
# Nu ia in calcul heterogeneitatea intre spatiu si timp


ols <- lm( Happiness_Rank  ~ Freedom+ Economy_GDP_per_Capita + Health_Life_Expectancy + Generosity  , data)
summary(ols) #output
yhat <- ols$fitted # valori estimate
ggplot(data, aes(x=Freedom, y=Happiness_Rank))+ #Happiness_Rank~Freedom
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  theme_bw()


# Model FE (cu efecte fixe) 

fe <- plm(Happiness_Rank  ~Freedom+  Economy_GDP_per_Capita + Health_Life_Expectancy+ Generosity  , 
          data, index = c('Country','Year'),
          model = 'within')
summary(fe)


# freedom este cea mai semnificativa, vom estima din nou modelul tinand
# cont doar de freedom
fe <- plm(Happiness_Rank  ~   Freedom,
          data, index = c('Country','Year'),
          model = 'within')
summary(fe)

#p-value < 0,05, => modelul este ok, toți coeficienții din model sunt
#diferiti de zero


# Alegerea celei mai adecvate variante de model prin testarea intre regresie 
# OLS vs fixed effects panel model
# H0: FE 
# H1: OLS
pFtest(fe, ols) # p-value < 0.05 => se recomanda model de panel data FE

# Model cu efecte aleatorii RE (random effects)
re <- plm(Happiness_Rank  ~   Freedom , 
          data, index = c('Country','Year'),
          model = 'between')
summary(re)

# Testul Hausmann il utilizam pentru a decide intre FE si RE
# H0: model cu efecte random 
# H1: model cu efecte fixe
phtest(fe,re) # p-value <0.05 => model cu efecte fixe se recomanda

# Testare ipoteze model
# In urma testului Hausmann am decis sa utilizam modelul FE

# Testarea efectelor fixe in timp
fixed.time <- plm(Happiness_Rank  ~ Freedom + factor(Year), data=data, index=c("Country",
                                                                               "Year"), model="within")

# H0:  nu sunt necesare efectele fixe in timp
# H1:  sunt necesare efectele fixe in timp
pFtest(fixed.time, fe) # p-value < 0.05 => se recomanda folosirea efectelor fixe in timp
plmtest(fe, c('time'), type = 'bp') # p-value=0.0019 <0.05  => se recomanda folosirea efectelor fixe in timp


# Testarea efectelor aleatorii cu Breusch-Pagan Lagrange Multiplier
# Testul ne ajuta sa decidem intre RE si OLS 
pool <- plm(Happiness_Rank  ~ Freedom, data=data, index=c("Country", "Year"), model="pooling")
summary(pool)

# H0: variatiile in timp sunt 0
# H1: variatiile in timp sunt diferite de 0
plmtest(pool, type=c("bp")) # p-value < 0.05 => respingem ipoteza nula
# variatiile in timp sunt diferite de 0 => efectele aleatorii sunt adecvate a.i.
# exista diferente semnificative intre tari


# Testarea dependentei transversale folosind testul Breusch-Pagan LM si 
# testul Parasan CD

# Ipoteze teste
# H0: reziduurile intre entitati nu sunt corelate
# H1: reziduurile intre entitati sunt corelate

pcdtest(fe, test = 'lm') # p-value <0.05 => dependenta transversala
pcdtest(fe, test = 'cd') # pvalue<0.05 => dependenta transversala
# Nu corectam pt ca avem panel mic. daca aveam serie de timp 40 perioade +
# trebuia sa corectam


# Testarea autocorelarii - Breusch-Godfrey/Wooldridge test 
# Testul se aplica doar cand seriile de timp sunt lungi. In acest caz nu 
# pune probleme setul de date deoarece avem date pe 8 ani

# H0: Nu exista autocorelate
# H1: autocorelarea este prezenta
pbgtest(fe) # p-value <0.05 => avem autocorelarem dar fiind panelul mic
# o vom ignora


# Testarea heteroschedasticitatii cu testul Breusch-Pagan
# H0: homoschedasticitate
# H1: heteroschedasticitate
bptest(Happiness_Rank  ~ Freedom + factor(Country), data = data, studentize=F)
# deoarece p-value <0.05 => avem heteroschedasticitate

# Testarea efectelor random 
pFtest(re, ols) # p-value=0.9 > 0.05 => nu se recomanda efecte random.
plmtest(re, c('time'), type = 'bp') # p-value=0.0019 < 0.05 =>  recomanda efecte random
#Situatia este incerta dar o sa incercam si corectarea 
pbgtest(re) # p-value=0.8931 > 0.05 => nu exista autocorelare
bptest(Happiness_Rank  ~ Freedom + factor(Year), data = data, studentize=F) # p-value=0.6344 > 0.05 => nu exista hetero

