#Proiect

# Cuprins:
#   Regresia simpla
#   Regresia multipla

# Fisiere:
#   wine_quality1.csv

# setup
rm(list = ls()) 
directory <- "C:/Users/Ada/Desktop/Proiect Econometrie/"

# Instalarea pachetelor
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", 
                  "olsrr", "moments","whitestrap","ggplot2","DataCombine","car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Regresia simpla --------------------------------------------------------------

wine <- read.csv(paste0(directory,"wine_quality1.csv"))
wine %<>% select(quality, alcohol)
str(wine)
stargazer(wine, type = "text")
head(wine, 10)

# Explorarea datelor - covarianta
cor(wine)

# Regresia simpla: quality = beta0 + beta1*alcohol + u
model_wine <- lm(formula = quality ~ alcohol, data = wine)
summary(model_wine)
model_wine$coefficients['alcohol']

# Graficul observatiilor cu dreapta estimata
plot(x = wine$alcohol, y = wine$quality)
abline(a = model_wine$coefficients['(Intercept)'], 
       b = model_wine$coefficients['alcohol'],
       col = 'red')

ggplot(data = wine, mapping = aes(x = alcohol, y = quality)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)



# Testul T pentru semnificatia coeficientilor -------------------------

# Model de regresie
model_wine <- lm(formula = quality ~ alcohol, data = wine)
summary(model_wine)

# Testare de ipoteze metoda 1: compararea testului t calculat cu cel critic

# Afisarea coeficientilor
(coefficient <- coef(model_wine)["alcohol"])

# Afisarea erorii standard
(se <- vcov(model_wine) %>% # matricea de varianta-covarianta
    diag %>% # extragerea diagonale
    sqrt %>% # radical
    .["alcohol"]) # obtinerea erirolor stadarde (se) ale regresorului "alcohol"

# Calcularea testului t = coeficient/se
(tstat <- coefficient/se)

# Gradele de libertate (n-k-1)
(df_r <- model_wine$df.residual)

# t-critic la nivel de semnificatie de 5%  
qt(p = 0.975, df = df_r, lower.tail = TRUE)

# tsrat > tcritic =>acceptăm ipoteza alternativă conform căreia coeficientul de corelație liniară Pearson 
#este semnificativ din punct de vedere statistic


# Testare de ipoteze metoda 2: compararea p-value cu nivelul de semnificatie (10%-5%-1%)
#P-value pentru test bilateral al semnificatiei coeficientilor. 
#Daca p-value < nivelul de semnificatie se respinge nula => coeficient semnificativ
# H0: beta[alcohol] = 0;  
# H1: beta[alcohol] != 0          

2 * pt(q = abs(tstat), df = df_r, lower.tail = FALSE)

# P-value pentru test unilateral dreapta pentru a testa un coeficient pozitiv
#H0: beta[alcohol] <=0;  
#H1: beta[alcohol] > 0
pt(q = tstat, df = df_r, lower.tail = FALSE)

# P-value pentru test unilateral stanga pentru a testa un coeficient negativ
#H0: beta[alcohol]  >=0;   
#H1: beta[alcohol] <0  
pt(q = tstat, df = df_r, lower.tail = TRUE)



# Testare de ipoteze metoda 3: calcularea intervalelor de incredere

# Valoarea critica pentru nivel de semnificatie de 5%
qt(p = 0.975, df = df_r)

# Valoarea minima a intervalului pentru 95% nivel de incredere
coefficient - 1.96 * se

# Valoarea maxima a intervalului pentru 95% nivel de incredere
coefficient + 1.96 * se


# Valoarea critica pentru nivel de semnificatie de 10%
qt(p = 0.95, df = df_r)

# Valoarea minima a intervalului pentru 90% nivel de incredere
coefficient - 1.65 * se

# Valoarea maxima a intervalului pentru 90% nivel de incredere
coefficient + 1.65 * se

# Cele trei metode: de comparare a testului t cu valorile critice,
# p-value cu nivel de semnificatie, si intervalele de incredere duc la aceleasi 
# rezultate si concluzii



# Bonitatea modelului (R-squared) ----------------------------------

str(model_wine)
(summary(model_wine))

# 'r.squared' din summary este R-squared
summary(model_wine)$r.squared

# Folosim nobs pentru a afisa numarul de observatii pe care le contine modelul
nobs(model_wine)



# Ipotezele modelului de regresie simpla -----------------

wine %<>% mutate(uhat1 = resid(model_wine)) # extragem reziduurile din model 

# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara 

# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(model_wine) > (model_wine$rank - 1)

# Ipoteza 3 - Modelul de regresie este corect specificat

# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru

# Ipoteza 4 - Variabilitatea in x este pozitiva
var(wine$alcohol)# toate valorile=1 > 0 => ipoteza acceptata

# Ipoteza 5 - Media reziduurilor este 0
mean(model_wine$residuals) # medie=1.02 aproape de 0 => ipoteza acceptata

# Ipoteza 6 - Testare multicoliniaritate
vif(model_wine) # nu avem valori pt VIF > 10 => ipoteza acceptata


# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(wine$alcohol, model_wine$residuals) # p-value=1 > 0.1 => nu sunt corelate
# => ipoteza acceptata


# Ipoteza 8 - Testare Heteroschedasticitate

#Metode predefinite

# Testul Breusch-Pagan 
bptest(model_wine)  #0.0003>0.05 => reziduri hetero

# Testul White
white_test(model_wine)  #0.0002>0.05 => reziduri hetero 

# Testare Heteroschedasticitate----------------------------------------------------------

wine %<>% mutate(alcohols = alcohol^2)
summary(model_wine)

wine %<>% mutate(uhat = resid(model_wine)) # extragem reziduurile din model 
wine %<>% mutate(yhat = fitted(model_wine))


# Obtinem residuals(uhat) si valorile estimate(yhat), si le ridicam la patrat
wine %<>% mutate(uhatsq = uhat^2,
                yhatsq = yhat^2)


# Testul Breusch-Pagan

# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)


# Regresia pentru testul Breusch-Pagan 
model_BP <- lm(uhatsq ~ alcohol, wine)
summary(model_BP)


# Metoda matematizata a testului
# Nr de variabile independente k1 
(k1 <- model_BP$rank - 1)

# Testul F si Testul LM pentru detectarea heteroscedasticitatii
(r2 <- summary(model_BP)$r.squared) # R-squared
(n <- nobs(model_BP)) # nr obs


( F_stat <- (r2/k1) / ((1-r2)/(n-k1-1)) ) # F-statistic
( F_pval <- pf(F_stat, k1, n-k1-1, lower.tail = FALSE) ) # p-value=0.0003< 0.05 => rezidurile sunt hetero


( LM_stat <- n * r2 ) # LM-statistic
( LM_pval <- pchisq(q = LM_stat, df = k1, lower.tail = FALSE)) # p-value=0.0003 < 0.05 =>reziduurile sunt hetero


# reziduurile sunt hetersocedastice => ipoteza este incalcata

# Testul White

# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)

# Regresia pentru testul White 
model_white <- lm(uhatsq ~ alcohol+alcohols, wine)
summary(model_white)
# Nr de variabile independente k2
(k2 <- model_white$rank - 1)


# Testul F si Testul LM pentru detectarea heteroscedasticitatii
(r2 <- summary(model_white)$r.squared) # R-squared
(n <- nobs(model_white)) # nr observatii

( F_stat <- (r2/k2) / ((1-r2)/(n-k2-1)) ) # F-statistic
( F_pval <- pf(F_stat, k2, n-k2-1, lower.tail = FALSE) ) # p-value=0.0002 < 0.05 =>
# reziduuri heteroscedastice

( LM_stat <- n * r2 ) # LM-statistic
( LM_pval <- pchisq(q = LM_stat, df = k2, lower.tail = FALSE)) # p-value < 0.05 =>
# reziduuri heteroscedastice



# Ambele teste arata prezenta heteroscedasticitatii pentru variabila pret
# Vom testa daca schimband forma functionala vom elimina heteroscedasticitatea


# Testam heteroscedasticitatea pentru variabila  log quality 

# Model de regresie log quality (log-lin)

wine1 <- read.csv(paste0(directory,"wine_quality1.csv"))

wine1 %<>% select(quality, alcohol)


wine1 <- wine1 %>% mutate(lquality = log(quality))


wine1 %>% select( lquality, alcohol) %>% head(10)

model_1 <- lm(lquality ~alcohol, wine1)
summary(model_1)


# Reziduurile si valorile estimate, ridicarea lor la patrat
wine1 %<>% mutate(uhat1 = resid(model_1),
                  yhat1 = fitted(model_1), 
                  uhat1sq = uhat1^2, 
                  yhat1sq = yhat1^2)

# Testul Breusch-Pagan 
bptest(model_1)  #0.32>0.05 => reziduri homoschedastice

# Testul White
white_test(model_1)  #0.11>0.05 => reziduri homoschedastice



# Testare autocorelare-------------------------

# Inspectarea autocorelarii cu ajutorul graficului ACF (autocorelare)
acf(model_1$residuals)


# Testul Durbin-Watson (ordinul 1)

# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate

dwtest(model_1) # p-value < 0.1 => reziduurile sunt autocorelate

# Testul Breusch-Godfrey (order superior)

# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate

bgtest(model_1) # p-value < 0.1 
bgtest(model_1, order = 2)  
bgtest(model_1, order = 3) # => reziduurile sunt autocorelate si la lag superior


# Cream un nou set de date  pt cel corectat
econ_data <- data.frame(wine1, resid_mod1=model_1$residuals)
# Cream variabila lag1 
econ_data_1 <- slide(econ_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
wine2 <- na.omit(econ_data_1) # eliminam valorile NA
# Reimplementam modelul cu noua variabila lag1 adaugata in model
model_2 <- lm(lquality ~ alcohol + lag1, data=wine2)
summary(model_2)
par(mar = c(1, 1, 1, 1))
acf(model_2$residuals) 

# Durbin Watson ok 
dwtest(model_2) # p-value=0.4 > 0.1 => reziduuri nonautocorelate 

# Breusch-Godfrey 
bgtest(model_2) # p-value=0.4 > 0.1
bgtest(model_2, order = 2)  #0.6>0.1


# Ipoteza 10 -  Reziduurile sunt normal distribuite-----------------------

if(!require(tseries)){install.packages('tseries')}

jarque.bera.test(model_2$residuals) # reziduurile nu sunt normale distribuite
# => corectie


# Normalitate --------------

# Testul Shapiro Wilk pentru normalitate
# H0: distributie normala, Ha: distributie nenormala


wine2 %>% 
  select(lquality,alcohol ) %>% 
  head(10)


wine2 %<>% mutate(uhat = resid(model_2)) # extragem reziduurile din model 

# Histograma reziduri
ggplot(data = wine2) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat), col = 'grey') +
  xlab('quality') + 
  ylab('Count') +
  ggtitle('Histograma rezidurilor') + 
  theme(plot.title = element_text(hjust = 0.5))


# Testul Shapiro Wilk pentru normalitate
# H0: distributie normala, Ha: distributie nenormala
shapiro.test(model_2$residuals) # deoarece p-value < 0.05 => reziduurile nu sunt normal distribuite

jarque.bera.test(model_2$residuals)

ols_plot_cooksd_chart(model_2) # ambele modalitati de calculare si afisare
# a distantei Cook ne indica faptul ca a 77-a observatie din setul nostru de date
# este un punct influent care ne afecteaza modelul. Il vom elimina si retesta

# Eliminam al 77 lea rand 
wine_cook <- wine2[-c(3,20,26,55,58,57,61,74,90,91,103,104,133,135,148,142,143,162,196,218,217,222,254,253,265,
                      301,302,314,376,378,402,430,481,480,502,525,554,569,595,594,697,718,719,
                      747,766,767,781,793,820,931,944,960,961,994), ]

# Reestimam modelul
model_3 <- lm(lquality ~ alcohol, wine_cook) 
summary(model_3)
wine_cook %<>% mutate(uhat_cook = resid(model_3)) # extragem reziduurile din model 

# Testam cu Jarque-Bera
jarque.bera.test(model_3$residuals) # p-value > 0.1 => reziduurile sunt normal
shapiro.test(model_3$residuals)

ols_plot_cooksd_chart(model_3) # in continuare identificam valorile outlier care


wine_cook1 <- wine_cook[-c(34,35,108,123,152,214,374,411,443,447,592,660,650,734,733,790), ]
model_4 <- lm(lquality ~alcohol, wine_cook1) 
jarque.bera.test(model_4$residuals) # 0.002
ols_plot_cooksd_chart(model_4) 


wine_cook2 <- wine_cook1[-c(207,231,234,278,574,645,152,191,217,249,340,509,507,573,651,657,713769,787,918
                           ), ]

model_5 <- lm(lquality ~alcohol, wine_cook2) 
jarque.bera.test(model_5$residuals) # 0.5171>0.1 => normal distribuite
summary(model_5)


# Regresie multipla -----------------------------
 
wine_multiple <- read.csv(paste0(directory,"wine_quality1.csv"))

wine_multiple %<>% select(quality, alcohol,sulphates,volatile_acidity)

wine_multiple <- wine_multiple %>% mutate(lquality = log(quality))

wine_multiple %>% 
  select( lquality,alcohol,sulphates,volatile_acidity) %>% 
  head(10)
stargazer(wine_multiple, type = "text")



model_multiple <- lm(lquality ~ alcohol+sulphates+volatile_acidity, wine_multiple)
summary(model_multiple)

# Valorile previzionate si reziduurile 
wine_multiple %<>% mutate(qhat = fitted(model_multiple),
                  uhat = residuals(model_multiple))


wine_multiple %>% 
  select(lquality, qhat, uhat) %>% 
  head(10)


# Testul T pentru semnificatia coeficientilor----------

# Afisarea coeficientilor
(coefficient1 <- coef(model_multiple)["alcohol"])
(coefficient2 <- coef(model_multiple)["sulphates"])
(coefficient3 <- coef(model_multiple)["volatile_acidity"])


# Afisarea erorii standard
(se1 <- vcov(model_multiple) %>%
    diag %>% 
    sqrt %>% 
    .["alcohol"]) # obtinerea erirolor stadarde (se) ale regresorului "alcohol"

# Calcularea testului t = coeficient/se
(tstat1 <- coefficient1/se1)


# Afisarea erorii standard
(se2 <- vcov(model_multiple) %>%
    diag %>% 
    sqrt %>% 
    .["sulphates"]) # obtinerea erirolor stadarde (se) ale regresorului "sulphates"

# Calcularea testului t = coeficient/se
(tstat2 <- coefficient2/se2)

# Afisarea erorii standard
(se3 <- vcov(model_multiple) %>%
    diag %>% 
    sqrt %>% 
    .["volatile_acidity"]) # obtinerea erirolor stadarde (se) ale regresorului "volatile_acidity"

# Calcularea testului t = coeficient/se
(tstat3 <- coefficient3/se3)



# Gradele de libertate (n-k-1)
(df_r <- model_multiple$df.residual)

# t-critic la nivel de semnificatie de 5%  
qt(p = 0.975, df = df_r, lower.tail = TRUE)

# tsrat > tcritic =>  semnificativ statistic



# Bonitatea modelului (R-squared si R-squared ajustat)------------

# Bonitatea modelului este data de R-squared si R-squared ajuutate 
# care arata ce procent din variatie este explaicat de model


# Regresie multipla cu 3 regresori
summary(model_multiple)
summary(model_multiple)$r.squared
summary(model_multiple)$adj.r.squared

# ! Diferenta dintre R-patrat si R-patrat ajustat este ca R-patrat
# presupune ca fiecare variabila ind din model explica variatia var
# dep, in timp ce R-patrat ajustat da procentul de variatie explicat
# doar de acele variabile ind care in realitate afecteaza var dep.



# Testul F pentru semnificatia coeficientilor---------------

####pt coef  volatile_acidity
# H0: beta[volatile_acidity]=0 
# Model restrictionat: 
#   lquality = alpha0 + alpha1*alcohol + alpha3*sulphates + alpha4*volatile_acidity + e
model_r1 <- lm(lquality ~ alcohol + sulphates, wine_multiple)
summary(model_r1)

# SSR pentru modeului restictionat ssr_r
(ssr_r1 <- sum(resid(model_r1)^2))

# Model nerestrictionat: 
# wage = beta0 + beta1*educ + beta2*exper + beta3*tenure + beta4*female + u
# La fel ca modelul de deasupra
model_ur <- model_multiple
summary(model_ur)

# SSR pt modelul nerestrictionat = ssr_ur, q = nr de restrictii si df_denom=n-k-1
(ssr_ur <- sum(resid(model_ur)^2))
(df_ur  <- model_ur$df.residual) # df_ur = n - k - 1
q <- 1

# Calcularea F-stat folosind ssr_r and ssr_ur 
# F-stat = ((ssr_r-ssr_ur)/q) / (ssr_ur/(n-k-1))
(F_stat <- ((ssr_r1 - ssr_ur)/q) / (ssr_ur/df_ur))

# Calcularea valorilor critice F-critical 
qf(p = 0.95, df1 = 1, df2 = df_ur)
# Daca F-stat > F-critical => respingem nula, coeficientul  volatile_acidity este semnificativ

# p-value pentru F-test
(F_pvalue <- pf(q = F_stat, df1 = 1, df2 = df_ur, lower.tail = F))
# Daca F p-value<0.1 => respingem nula => coeficientul este semnificativ

####pt coef sulphates
model_r2 <- lm(lquality ~ alcohol + volatile_acidity, wine_multiple)
summary(model_r2)

# SSR pentru modeului restictionat ssr_r
(ssr_r2 <- sum(resid(model_r2)^2))

# Model nerestrictionat: 
# La fel ca modelul de deasupra
model_ur2 <- model_multiple
summary(model_ur2)

# SSR pt modelul nerestrictionat = ssr_ur, q = nr de restrictii si df_denom=n-k-1
(ssr_ur2 <- sum(resid(model_ur2)^2))
(df_ur2  <- model_ur2$df.residual) # df_ur = n - k - 1
q2 <- 1

# Calcularea F-stat folosind ssr_r and ssr_ur 
# F-stat = ((ssr_r-ssr_ur)/q) / (ssr_ur/(n-k-1))
(F_stat2 <- ((ssr_r2 - ssr_ur2)/q2) / (ssr_ur2/df_ur2))


# Calcularea valorilor critice F-critical 
qf(p = 0.95, df1 = 1, df2 = df_ur2)
# Daca F-stat > F-critical => respingem nula, coeficientul  volatile_acidity este semnificativ

# p-value pentru F-test
(F_pvalue2 <- pf(q = F_stat, df1 = 1, df2 = df_ur2, lower.tail = F))
# Daca F p-value<0.1 => respingem nula => coeficientul este semnificativ

####pt coef ALCOHOL
model_r3 <- lm(lquality ~volatile_acidity+sulphates, wine_multiple)
summary(model_r3)

# SSR pentru modeului restictionat ssr_r
(ssr_r3 <- sum(resid(model_r3)^2))

# Model nerestrictionat: 
# wage = beta0 + beta1*educ + beta2*exper + beta3*tenure + beta4*female + u
# La fel ca modelul de deasupra
model_ur3 <- model_multiple
summary(model_ur3)

# SSR pt modelul nerestrictionat = ssr_ur, q = nr de restrictii si df_denom=n-k-1
(ssr_ur3 <- sum(resid(model_ur3)^2))
(df_ur3  <- model_ur3$df.residual) # df_ur = n - k - 1
q3 <- 1

# Calcularea F-stat folosind ssr_r and ssr_ur 
# F-stat = ((ssr_r-ssr_ur)/q) / (ssr_ur/(n-k-1))
(F_stat3 <- ((ssr_r3 - ssr_ur3)/q3) / (ssr_ur3/df_ur3))


# Calcularea valorilor critice F-critical 
qf(p = 0.95, df1 = 1, df2 = df_ur3)
# Daca F-stat > F-critical => respingem nula, coeficientul  volatile_acidity este semnificativ

# p-value pentru F-test
(F_pvalue2 <- pf(q = F_stat, df1 = 1, df2 = df_ur2, lower.tail = F))
# Daca F p-value<0.1 => respingem nula => coeficientul este semnificativ



# F-test pentru testarea simultana a coeficientilor diferiti de 0 
###alcohol
model_r4 <- lm(lquality ~ alcohol, wine_multiple)
summary(model_r4)

# SSR pentru modelul restrictionat ssr_r
ssr_r4 <- sum(resid(model_r4)^2)

# modelul nerestrictionat: 
summary(model_ur)

# SSR pentru modelul nerestrictionat = ssr_ur, q = nr de restrictii si
# df_denom = n-k-1
ssr_ur # SSR pentru modelul nerestrictionat
df_ur # gradele de libertate pentru modelul nerestrictionat
q <- 2

# Calcularea F_stat folosind ssr_r si ssr_ur 
# F-stat=((ssr_r-ssr_ur)/q) / (ssr_ur/(n-k-1))
(F_stat4 <- ((ssr_r4 - ssr_ur)/q) / (ssr_ur/df_ur))

# F-critical 
qf(p = 0.05, df1 = q, df2 = df_ur, lower.tail = F)
# Daca F-stat > F-critical => respingem ipoteza nula => coef semnificativi simultan

# F-test folosind R commands
linearHypothesis(model_ur, c("volatile_acidity = 0", "sulphates = 0"))

####sulphates
model_r5 <- lm(lquality ~ sulphates, wine_multiple)
summary(model_r5)

# SSR pentru modelul restrictionat ssr_r
ssr_r5 <- sum(resid(model_r5)^2)

# modelul nerestrictionat: 
summary(model_ur)

# SSR pentru modelul nerestrictionat = ssr_ur, q = nr de restrictii si
# df_denom = n-k-1
ssr_ur # SSR pentru modelul nerestrictionat
df_ur # gradele de libertate pentru modelul nerestrictionat
q <- 2

# Calcularea F_stat folosind ssr_r si ssr_ur 
# F-stat=((ssr_r-ssr_ur)/q) / (ssr_ur/(n-k-1))
(F_stat5 <- ((ssr_r5 - ssr_ur)/q) / (ssr_ur/df_ur))

####volatile
model_r6 <- lm(lquality ~ volatile_acidity, wine_multiple)
summary(model_r6)

# SSR pentru modelul restrictionat ssr_r
ssr_r6 <- sum(resid(model_r6)^2)

# modelul nerestrictionat: 
# wage = beta0 + beta1*educ + beta2*exper + beta3*tenure + beta4*female + u
summary(model_ur)

# SSR pentru modelul nerestrictionat = ssr_ur, q = nr de restrictii si
# df_denom = n-k-1
ssr_ur # SSR pentru modelul nerestrictionat
df_ur # gradele de libertate pentru modelul nerestrictionat
q <- 2

# Calcularea F_stat folosind ssr_r si ssr_ur 
# F-stat=((ssr_r-ssr_ur)/q) / (ssr_ur/(n-k-1))
(F_stat6 <- ((ssr_r6 - ssr_ur)/q) / (ssr_ur/df_ur))


##########
# Ipotezele modelului de regresie ----------------- 

# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara 

# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(model_multiple) > (model_multiple$rank - 1)

# Ipoteza 3 - Modelul de regresie este corect specificat

# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru

# Ipoteza 4 - Variabilitatea in x este pozitiva
var(wine_multiple$alcohol)
var(wine_multiple$volatile_acidity)
var(wine_multiple$sulphates) # toate valorile > 0 => ipoteza acceptata

# Ipoteza 5 - Media reziduurilor este 0
mean(model_multiple$residuals) # medie aproape de 0 => ipoteza acceptata

# Ipoteza 6 - Testare multicoliniaritate
vif(model_multiple) # nu avem valori pt VIF > 10 => ipoteza acceptata

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(wine_multiple$alcohol, model_multiple$residuals) # p-value =1> 0.1 => nu sunt corelate
cor.test(wine_multiple$volatile_acidity, model_multiple$residuals) # p-value=1 > 0.1 => nu sunt corelate
cor.test(wine_multiple$sulphates, model_multiple$residuals) # p-value=1 > 0.1 => nu sunt corelate
# => ipoteza acceptata



# Ipoteza 8 - Reziduurile sunt homoscedastice

bptest(model_multiple)  # 0.2>0.05
white_test(model_multiple)  #0.66>0.05

# Ipoteza 9 - Reziduurile nu sunt autocorelate---------

# Testare autocorelare-------------------------

# Inspectarea autocorelarii cu ajutorul graficului ACF (autocorelare)
acf(model_multiple$residuals)


# Testul Durbin-Watson (ordinul 1)

# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate

dwtest(model_multiple) # p-value < 0.1 => reziduurile sunt autocorelate

# Testul Breusch-Godfrey (order superior)

# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate

bgtest(model_multiple) # p-value < 0.1 
bgtest(model_multiple, order = 2)  
bgtest(model_multiple, order = 3) # => reziduurile sunt autocorelate si la lag superior


# Cream un nou set de date  pt cel corectat
econ_data <- data.frame(wine_multiple, resid_mod1=model_multiple$residuals)
# Cream variabila lag1 
econ_data_1 <- slide(econ_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
wine_multiple1 <- na.omit(econ_data_1) # eliminam valorile NA
# Reimplementam modelul cu noua variabila lag1 adaugata in model
model_multiple1 <- lm(lquality ~ alcohol +sulphates+volatile_acidity+ lag1, data=wine_multiple1)
summary(model_multiple1)
par(mar = c(1, 1, 1, 1))
acf(model_multiple1$residuals) 

# Durbin Watson ok 
dwtest(model_multiple1) # p-value=0.38 > 0.1 => reziduuri nonautocorelate 

# Breusch-Godfrey 
bgtest(model_multiple1) # p-value=0.13 > 0.1   => reziduuri nonautocorelate 
bgtest(model_multiple1, order = 2)  #0.33>0.1  => reziduuri nonautocorelate 


# Ipoteza 10 -  Reziduurile sunt normal distribuite-----------
# Normalitate--------------

if(!require(tseries)){install.packages('tseries')}

plot(model_multiple1) 

ols_plot_resid_qq(model_multiple1)

ols_plot_resid_hist(model_multiple1)
skewness(model_multiple1$uhat)
kurtosis(model_multiple1$uhat) 

ols_plot_cooksd_chart(model_multiple1) 
ols_plot_cooksd_chart(model_multiple1) 

jarque.bera.test(model_multiple1$residuals) #nu sunt normal distribuite

wine_multiple_cook <- wine_multiple1[-c(3,55,74,91,120,217,56,192,254,236,103,104,253,301,302,314,378,402,430,475,481,480,
                                        502,509,525,553,554,595,594,715,718,719,820,833,820,781,766,820,833,931,945,960,961,994,959), ]

# Reestimam modelul
model_multiple2 <- lm(lquality ~alcohol + volatile_acidity + sulphates, wine_multiple_cook) 
jarque.bera.test(model_multiple2$residuals) #nu sunt normal distribuite
ols_plot_cooksd_chart(model_multiple2) 

wine_multiple_cook1 <- wine_multiple_cook[-c(12,134,125,154,206,486), ]
model_multiple3 <- lm(lquality ~alcohol + volatile_acidity + sulphates, wine_multiple_cook1) 
jarque.bera.test(model_multiple3$residuals) # nu sunt normal distribuite
ols_plot_cooksd_chart(model_multiple3) 

wine_multiple_cook2 <- wine_multiple_cook1[-c(158,246,217,291,353,355,349,624,588,728,708,753,901,666,741,183,419,163,57,272,219,218,208,125), ]
model_multiple4 <- lm(lquality ~alcohol + volatile_acidity + sulphates, wine_multiple_cook2) 
jarque.bera.test(model_multiple4$residuals) # 0.32 >0.1 sunt normal distribuite

#####Model final corectat

model_multiple4 <- lm(lquality ~alcohol + volatile_acidity + sulphates, wine_multiple_cook2) 
summary(model_multiple4)


##Regresia multipla cu dummy + schimbare forma 

wine_dummy <- read.csv(paste0(directory,"wine_quality1.csv"))


wine_dummy %<>% select(quality, alcohol ,volatile_acidity,sulphates,white) 


wine_dummy %>% select( lquality, lalcohol,lvolatile_acidity,lsulphates,white) %>% 
  head(10)

model_dummy <- lm(lquality ~ lalcohol + lvolatile_acidity + lsulphates + white , wine_dummy)
summary(model_dummy)

wine_dummy %<>% mutate(lqualityhat = fitted(model_dummy),
                       whiteXlalcohol=white*lalcohol)

model_dummy <- lm(lquality ~ lalcohol +lsulphates+ lvolatile_acidity + white + whiteXlalcohol, wine_dummy)
summary(model_dummy)


wine_dummy %<>% mutate(red = 1 - white)
wine_dummy %<>%
  mutate(type = factor(white, levels = 1:0, labels = c('white', 'red')))


ggplot(data = wine_dummy, mapping = aes(x = lalcohol, col = type)) + 
  theme_bw() + 
  geom_point(aes(y = lquality)) + 
  geom_line(aes(y = lqualityhat)) +
  guides(color = guide_legend(title = 'type'))

ggplot(wine_dummy, aes(x = lalcohol, y = lquality, col = type, group = type)) + 
  theme_bw() + 
  geom_point() + 
  geom_smooth(method = lm, se = F, linewidth = .5) + 
  guides(color = guide_legend("type")) # legend title




# Bonitatea modelului------------------------


# Bonitatea modelului este data de R-squared si R-squared ajuutate 
# care arata ce procent din variatie este explaicat de model


# Regresie multipla cu 3 regresori
summary(model_dummy)
summary(model_dummy)$r.squared
summary(model_dummy)$adj.r.squared

# ! Diferenta dintre R-patrat si R-patrat ajustat este ca R-patrat
# presupune ca fiecare variabila ind din model explica variatia var
# dep, in timp ce R-patrat ajustat da procentul de variatie explicat
# doar de acele variabile ind care in realitate afecteaza var dep.



# Testul T pentru semnificatia coeficientilor----------

# Afisarea coeficientilor
(coefficient1 <- coef(model_dummy)["lalcohol"])
(coefficient2 <- coef(model_dummy)["lsulphates"])
(coefficient3 <- coef(model_dummy)["lvolatile_acidity"])
(coefficient4 <- coef(model_dummy)["white"])
(coefficient5 <- coef(model_dummy)["whiteXlalcohol"])



# Afisarea erorii standard
(se1 <- vcov(model_dummy) %>%
    diag %>% 
    sqrt %>% 
    .["lalcohol"]) # obtinerea erirolor stadarde (se) ale regresorului "lalcohol"

# Calcularea testului t = coeficient/se
(tstat1 <- coefficient1/se1)


# Afisarea erorii standard
(se2 <- vcov(model_dummy) %>%
    diag %>% 
    sqrt %>% 
    .["lsulphates"]) # obtinerea erirolor stadarde (se) ale regresorului "lsulphates"

# Calcularea testului t = coeficient/se
(tstat2 <- coefficient2/se2)

# Afisarea erorii standard
(se3 <- vcov(model_dummy) %>%
    diag %>% 
    sqrt %>% 
    .["lvolatile_acidity"]) # obtinerea erirolor stadarde (se) ale regresorului "lvolatile_acidity"

# Calcularea testului t = coeficient/se
(tstat3 <- coefficient3/se3)

# Afisarea erorii standard
(se4 <- vcov(model_dummy) %>%
    diag %>% 
    sqrt %>% 
    .["white"]) # obtinerea erirolor stadarde (se) ale regresorului "white"

# Calcularea testului t = coeficient/se
(tstat4 <- coefficient4/se4)

# Afisarea erorii standard
(se5 <- vcov(model_dummy) %>%
    diag %>% 
    sqrt %>% 
    .["whiteXlalcohol"]) # obtinerea erirolor stadarde (se) ale regresorului "whiteXlalcohol"

# Calcularea testului t = coeficient/se
(tstat5 <- coefficient5/se5)


# Gradele de libertate (n-k-1)
(df_r <- model_dummy$df.residual)

# t-critic la nivel de semnificatie de 5%  
qt(p = 0.975, df = df_r, lower.tail = TRUE)

# tsrat > tcritic =>  coef semnificativi statistic



#-------------Impartirea setului de date----------

# Incarcarea librariilor
library(tidyverse)
library(caret)
library(mltools)
library(MLmetrics)

# Instalare si activare pachete


#Impartirea setului de date si prognoza pt model multiplu-------------------------------------

training.samples <- wine_dummy$lquality %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- wine_dummy[training.samples, ]
test.data <- wine_dummy[-training.samples, ]

# Modelul REGRESIE MULTIPLA  de tip LOG-LOG 
model <- lm(lquality ~ lalcohol +lsulphates + lvolatile_acidity  , wine_dummy)
summary(model)

# Model de regresie de tip log-log pentru setul de antrenare
model_antrenare <- lm(lquality ~ lalcohol + lvolatile_acidity + lsulphates, data = train.data) 
summary(model_antrenare)


# Predictia modelului pe setul de testare
y_pred <- predict(model_antrenare, newdata = test.data)
y_pred

# RMSE - Root Mean Squared Error =>  RMSE=0.1  < 1 => predictie buna 
RMSE(y_pred, test.data$lquality)

# MAE - Mean Absolute Error => MAE=0.09 < 1 => predictie buna 
MAE(y_pred, test.data$lquality)


# MSE - Mean Squared Error => MSE=0.01 < 1 =>
# predictie buna
MSE(y_pred, test.data$lquality)

# MAPE - Mean Absolute Percentage Error => MAPE=0.05 < 1 => predictie buna 
MAPE(y_pred, test.data$lquality)

# Out of sample forecasting 
out_of_sample <- data.frame(alcohol = c(12,12.5,14.9),
                            sulphates = c(0.89,0.82,2),
                            volatile_acidity=c(0.8,0.9,1.5))


# Logaritmam
out_of_sample_log <- out_of_sample %>%
  mutate(lalcohol = log(alcohol),
         lvolatile_acidity=log(volatile_acidity),
         lsulphates=log(sulphates))

# Deselectam variabilele de care nu avem nevoie
out_of_sample_log <- out_of_sample_log %>%
  select(-alcohol,-sulphates,-volatile_acidity)

# Prognoza
y_pred_outsample <- predict(model, newdata = out_of_sample_log)
y_pred_outsample
exp(y_pred_outsample) 





# Regresie Neliniara ----------------------------------

# Ramsey RESET-----------------------------------------

neliniar <- read.csv(paste0(directory, "wine_quality1.csv"))

# Modelul de regresie pentru salariu
model_neliniar <- lm(quality ~ alcohol + sulphates + volatile_acidity + white, neliniar)
summary(model_neliniar)

neliniar %<>% mutate(yhat = fitted(model_neliniar), # reziduuri
                     yhatsq = yhat^2, # patratul reziduurilor
                     yhatcube = yhat^3) # cubul reziduurilor



# RESET, testarea semnificatiei comune a coef yhatsq si yhatcube
model1_RESET <- update(model_neliniar, ~ . + yhatsq + yhatcube) # adaugam in modelul initial
# patratul si cubul reziduurilor
summary(model1_RESET)

# H0: modelul este bine specificat 
# H1: modelul nu este bine specificat

linearHypothesis(model1_RESET, c("yhatsq = 0", "yhatcube = 0")) #0.8>1 =>model bine specificat


# Testul Chow -------------------------------------------------------------------


sctest(neliniar$quality ~ neliniar$alcohol, type = "Chow", point = 14) # p-value > 0.1 => 
#  nu exista rupturi in serie si este suficient un singur model de regresie
sctest(neliniar$quality ~ neliniar$volatile_acidity, type = "Chow", point = 14) # p-value > 0.1 => 
#  nu exista rupturi in serie si este suficient un singur model de regresie
sctest(neliniar$quality ~ neliniar$sulphates, type = "Chow", point = 14) # p-value > 0.1 => 
sctest(neliniar$quality ~ neliniar$white, type = "Chow", point = 14) # p-value > 0.1 => 



training.samples <- neliniar$quality %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- neliniar[training.samples, ]
test.data <- neliniar[-training.samples, ]

# Inspectam grafic 
ggplot(train.data, aes(alcohol,quality ) ) +
  geom_point() +
  stat_smooth() # din grafic se vede forma neliniara a unui posibil model de regresie

model <- lm(quality ~ alcohol, data = train.data)
summary(model)
# Prognoze pe test data
predictions <- model %>% predict(test.data)
# Performanta modelului
data.frame(
  RMSE = RMSE(predictions, test.data$quality),
  R2 = R2(predictions, test.data$quality)
)


# Inspectam grafic modelul de regresie liniara
ggplot(train.data, aes(quality,alcohol ) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)


# Implementam modelul de regresie polinomiala de ord superior pana cand coef nu vor
# mai fi semnificativi
model2 <- lm(quality ~ alcohol + I(alcohol^2), data = train.data)
summary(model2)
model3 <- lm(quality ~ alcohol + I(alcohol^2) + I(alcohol^3), data = train.data)
summary(model3)
model4 <- lm(quality ~ alcohol + I(alcohol^2) + I(alcohol^3) +
               I(alcohol^4), data = train.data)
summary(model4)

predictions2 <- model4 %>% predict(test.data)
# Performanta modelului
data.frame(
  RMSE = RMSE(predictions2, test.data$quality),
  R2 = R2(predictions2, test.data$quality)
) # modeulul polinomial are o acuratete mai mare decat cel liniara 


# Inspectam grafic modelul de regresie polinomiala de ord 5
ggplot(train.data, aes(alcohol, quality) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 4, raw = TRUE))

# Spline--------------------------------------------

# Trebuie specificate gradul polinomului si numarul de noduri
knots <- quantile(train.data$alcohol, p = c(0.25, 0.5, 0.75)) # vom pozitiona
# nodurile in dreptul quartilelor
model_spline <- lm (quality ~ bs(alcohol, knots = knots), data = train.data)
summary(model_spline)
#Prognoza pe test
predictions_spline <- model_spline %>% predict(test.data)
# Performanta modelului
data.frame(
  RMSE = RMSE(predictions_spline, test.data$quality),
  R2 = R2(predictions_spline, test.data$quality)
)

# Reprezentare grafica
ggplot(train.data, aes(alcohol, quality) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))

# Modelul aditiv generalizat (GAM) 
model_gam <- gam(quality ~ s(alcohol), data = train.data)
summary(model_gam)
# Prognoze
predictions_gam <- model_gam %>% predict(test.data)
# Performanta modelului
data.frame(
  RMSE = RMSE(predictions_gam, test.data$quality),
  R2 = R2(predictions_gam, test.data$quality)
)

# Comparam acuratetea si bonitatea modelelor
# RMSE: 
# liniar = 0.73
# polinomial = 0.745
# spline = 0.74
# GAM = 0.736
# R2:
# liniar = 19.74%
# polinomial = 17.43%
# spline = 17.77%
# GAM = 19.72%



#--------------------- Metode de penalitate----------------------------------------------------------
PackageNames <- c("tidyverse", "stargazer", "magrittr", "car", "strucchange",
                  "ggplot2","caret", "splines","mgcv","glmnet","psych")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Regresia Ridge 
wine_pen <- read.csv(paste0(directory,"wine_quality1.csv"))



wine_pen %<>% select(quality, alcohol ,volatile_acidity,sulphates,white) 


wine_pen <- wine_pen %>% mutate(lquality = log(quality))

model_penalitate <- lm(lquality ~ alcohol +sulphates + volatile_acidity + white , wine_pen)
summary(model_penalitate)
prognoza <- data.frame(alcohol = c(14),
                       sulphates = c(0.8),
                       volatile_acidity = c(0.9),
                       white = c(1))
y_pred_scenariu <- predict(model_penalitate, newdata = prognoza)
y_pred_scenariu

# Definim variabila raspuns
y <- wine_pen$lquality

# Definim predictorii
x <- data.matrix(wine_pen[, c('alcohol', 'sulphates', 'volatile_acidity', 'white' )])

# Estimam modelul ridge (alpha = 0)
model <- glmnet(x, y, alpha = 0)
summary(model)

# In continuare vom identifica valoarea lui lambda pt care avem MSE minimizat
# utilizand validarea incrucisata (cross validation)
cv_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model$lambda.min
best_lambda # 0.0068

# testarea valorii lamda 
plot(cv_model) 

# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 

# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:4, legend = colnames(x), cex = .7)

# Prognoze 
y_predicted <- predict(model, s = best_lambda, newx = x)

# Progoza out-of-sample
new <- matrix(c(14, 0.8, 09, 1), nrow=1, ncol=4) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 30,1608%

# Regresia LASSO --------------------------------------
model <- glmnet(x, y, alpha = 1)

# Din punct de vedere tehnic, vom seta valoarea alpha = 1 pentru 
# regresia LASSO. 
cv_model <- cv.glmnet(x, y, alpha = 1)

# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 0.0019

# testarea valorii lamda
plot(cv_model) 


# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor  # coeficientii variabilelor 
# daca unul din coeficienti este 0 inseamna ca acea variabila nu este importanta
# si de aceea nu o estimeaza modelul

# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model, xvar = "lambda",label=T)
legend("bottomright", lwd = 1, col = 1:4, legend = colnames(x), cex = .7)

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Prognoza out-of-sample
new <- matrix(c(14, 0.8, 0.9, 1), nrow=1, ncol=4) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 30.19%


# Elastic net regression -------------------------------
model <- cv.glmnet(x, y, alpha = 0.5)
cv_model <- cv.glmnet(x, y, alpha = 0.5)

# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 0.002


# testarea valorii lamda
plot(cv_model) 

# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Prognoza out-of-sample
new <- matrix(c(14, 0.8, 0.9, 1), nrow=1, ncol=4) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 30.18%

# Vom compara valorile lui rsq si in functie de acestea vom alege modelul cu cea
# mai mare bonitate drept modelul optim =>Lasso cea mai buna

#-------------------------

# Algoritmul Boruta 

# Vom converti variabilele categoricale in variabile factor 
convert <- c(1:5)
wine_pen[,convert] <- data.frame(apply(wine_pen[convert], 2, as.factor))
if(!require(Boruta)){install.packages('Boruta')}
library(Boruta)

set.seed(500)
boruta.bank_train <- Boruta(lquality~., data = wine_pen, doTrace = 2)
print(boruta.bank_train)

# Vom selecta atributele importante 
getSelectedAttributes(boruta.bank_train, withTentative = T)

# Vom reimplementa un model de regresie cu aceste atribute
model_boruta <- lm(lquality ~ alcohol + volatile_acidity+ sulphates + white , wine_dummy)
summary(model_boruta) 


#---------------------------------------------------------------------------------------------

# Regresia cu variabila dummy---
model_dummy1 <- lm(lquality ~ lalcohol + lvolatile_acidity + lsulphates + white, wine_dummy)
summary(model_dummy1)

# Heteroschedasticitatea
bptest(model_dummy)  #asta nu se testeaza
white_test(model_dummy) # 0.3> 0.05 erorile sunt homoschedastice, ipoteza se valideaza

# Non autocorelarea
acf(model_dummy$residuals) 
dwtest(model_dummy) # p-value > 0.1 => reziduuri autocorelate 
bgtest(model_dummy, order=2) # p-value > 0.1 => reziduuri autocorelate

# Cream un nou set de date  pt cel corectat
econ_data <- data.frame(wine_dummy, resid_mod1=model_dummy$residuals)
# Cream variabila lag1 
econ_data_1 <- slide(econ_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
wine_dummy1 <- na.omit(econ_data_1) # eliminam valorile NA
# Reimplementam modelul cu noua variabila lag1 adaugata in model
model_dummy2 <- lm(lquality ~ lalcohol +lsulphates+lvolatile_acidity+white+whiteXlalcohol+ lag1, data=wine_dummy1)
summary(model_dummy2)
par(mar = c(1, 1, 1, 1))
acf(model_dummy2$residuals) 

# Durbin Watson ok 
dwtest(model_dummy2) # p-value=0.38 > 0.1 => reziduuri nonautocorelate 

# Breusch-Godfrey 
bgtest(model_dummy2) # p-value=0.13 > 0.1   => reziduuri nonautocorelate 
bgtest(model_dummy2, order = 2)  #0.33>0.1  => reziduuri nonautocorelate 

econ_data <- data.frame(wine_dummy, resid_mod1=model_dummy$residuals)
# Cream variabila lag1 
econ_data_1 <- slide(econ_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
wine_dummy1 <- na.omit(econ_data_1) # eliminam valorile NA
# Reimplementam modelul cu noua variabila lag1 adaugata in model
model_dummy2 <- lm(lquality ~ lalcohol +lsulphates+lvolatile_acidity+white+whiteXlalcohol+ lag1, data=wine_dummy1)
summary(model_dummy2)
par(mar = c(1, 1, 1, 1))
acf(model_dummy2$residuals) 

# Durbin Watson ok 
dwtest(model_dummy2) # p-value=0.38 > 0.1 => reziduuri nonautocorelate 

# Breusch-Godfrey 
bgtest(model_dummy2) # p-value=0.13 > 0.1   => reziduuri nonautocorelate 
bgtest(model_dummy2, order = 2)  #0.33>0.1  => reziduuri nonautocorelate 


# Normalitate
jarque.bera.test(model_dummy$residuals)

