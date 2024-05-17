setwd("C:/R scripts/Project")
dataset <- read.csv("hrs_15.csv")
summary(dataset)
View(dataset)

## PRELIMINAR PHASE: 
# ---------- Create a dummy for the waves (for ages) --------------
dataset$y_2006=dataset$wave==8
dataset$y_2008=dataset$wave==9
dataset$y_2010=dataset$wave==10
dataset$y_2012=dataset$wave==11
dataset$y_2014=dataset$wave==12
dataset$y_2016=dataset$wave==13
dataset$y_2018=dataset$wave==14
dataset$y_2020=dataset$wave==15

summary(dataset)

# ------------------- OLS regression ----------------------------
reg1 = lm(socladder ~ age + female + hi_school + college + empl + selfempl + log(income) + log(tot_finw) + y_2008 + y_2010 + y_2012 + y_2014 + y_2016 + y_2018 + y_2020, data = dataset)
summary(reg1)

# Estrai il coefficiente associato a log(income)
coeff_income <- coef(reg1)["log(income)"]

# Calcola l'effetto dell'aumento del reddito del 10%
effect_10_percent <- coeff_income * 0.1 
effect_final = effect_10_percent*100
cat("L'effetto di aumentare il reddito del 10% è:", effect_final, "%\n")

# performo il T-test:
coef_log_income <- coef(reg1)["log(income)"]
stderr_log_income <- summary(reg1)$coefficients["log(income)", "Std. Error"]

# Esegui il test t
t_value <- coef_log_income / stderr_log_income

# Calcola il p-value
p_value <- 2 * pt(abs(t_value), df = length(reg1$residuals) - length(reg1$coefficients))

# Stampa i risultati
cat("Test t-value:", t_value, "\n")
cat("P-value:", p_value, "\n")


# punto B:
library(car)
lht(reg1, c("hi_school = college"))

#POINT C:
lht(reg1,c("y_2008TRUE=0","y_2010TRUE=0","y_2012TRUE=0","y_2014TRUE=0","y_2016TRUE=0","y_2018TRUE=0","y_2020TRUE=0"), test="Chisq")

# POINT D:
reg2 = lm(log(socladder) ~ age + female + hi_school + college + empl + selfempl + log(income) + log(tot_finw) + y_2008 + y_2010 + y_2012 + y_2014 + y_2016 + y_2018 + y_2020, data = dataset)
summary(reg2)



## ------------- EXERCISE 2 -------------------------
#Point A:
library(lmtest)
bptest(reg2)

#Point B:
install.packages("sandwich")
library(sandwich)
coeftest(reg2, vcov=vcovHC(reg1, type="HC1"))#coeff test aggiunge robust st. errors
summary(reg2)

robust_se <- coeftest(reg2, vcov. = vcovHC(reg1, type = "HC1"))[, "Std. Error"]

# Stampa l'errore standard robusto per il coefficiente su female
print("Robust st. Error:"+robust_se["female"])


#Punto C
resettest(reg2, power=2:3, type = "fitted")

#Point D:
reg3 = lm(log(socladder) ~ age + female + hi_school + college + empl + selfempl + log(income) + log(tot_finw) + if_child + married + y_2008 + y_2010 + y_2012 + y_2014 + y_2016 + y_2018 + y_2020, data = dataset)
summary(reg3)
# joint: tutte variabili uguali a zero, se hanno stesso effetto:var1=var2
lht(reg3, c("married = 0", "if_child = 0"), test="Chisq")

## ---------------- EXERCISE 3 ---------------------
install.packages("plm")
library(plm)
#PE:
regPE= plm(log(socladder) ~ age+female+hi_school+college+empl+selfempl+log(tot_finw)+log(income)+y_2008+y_2010+y_2012+y_2014+y_2016+y_2018+y_2020, data=dataset, index=c("hhidpn","wave"), model="pooling")
summary(regPE)

#RE
regRE= plm(log(socladder) ~ age+female+hi_school+college+empl+selfempl+log(tot_finw)+log(income)+y_2008+y_2010+y_2012+y_2014+y_2016+y_2018+y_2020, data=dataset, index=c("hhidpn","wave"), model="random")
summary(regRE)
#FE
regFE= plm(log(socladder) ~ age+female+hi_school+college+empl+selfempl+log(tot_finw)+log(income)+y_2008+y_2010+y_2012+y_2014+y_2016+y_2018+y_2020, data=dataset, index=c("hhidpn","wave"), model="within")
summary(regFE)

#POINT A:
plmtest(regPE, type = c("bp"))

#Point B: 
pFtest(regFE, regPE)

#Point C:
phtest(regFE, regRE)
phtest(regRE, regFE)


#POINT D: eseguo un t-test e runno un T-Test
regBestModel = plm(log(socladder) ~ age+female+hi_school+college+empl+selfempl+log(tot_finw)+log(income)+y_2008+y_2010+y_2012+y_2014+y_2016+y_2018+y_2020, data=dataset, index=c("hhidpn","wave"), model="within")
summary(regBestModel)

# --------------- Esercizio 4 --------------

#punto A:
dataset$socladderAdd = dataset$socladder>5
View(dataset)
summary(dataset)

reg7= lm(socladderAdd~age+female+hi_school+college+empl+selfempl+log(tot_finw)+log(income)+y_2008+y_2010+y_2012+y_2014+y_2016+y_2018+y_2020, data=dataset)
summary(reg7)
fit1 = coef(reg7)["(Intercept)"] + coef(reg7)["age"]*55 +coef(reg7)["female"]*1 +coef(reg7)["hi_school"]*1 +coef(reg7)["log(income)"]*log(30000) +coef(reg7)["log(tot_finw)"]*log(80000) +coef(reg7)["y_2012TRUE"]*1
fit1 = pnorm(fit1, mean=0, sd=1)
fit1

#reg7= lm(socladderAdd ~ age+female+hi_school+college+empl+selfempl+log(tot_finw)+log(income)+y_2008+y_2010+y_2012+y_2014+y_2016+y_2018+y_2020, data=dataset)
#summary(reg7)

#Set up the dummy variables environment:
#female aged55
#earning 30k
#whealth 80k
#dataset$aged55 = dataset$age==55
#dataset$earning30kusd = dataset$income>30000
#dataset$whealth80kusd = dataset$tot_finw>80000
#summary(dataset)

#Point B
lht(reg7,c("empl=selfempl"), test="Chisq")

#Point c
reg8=glm(socladderAdd~age+female+hi_school+college+empl+selfempl+log(tot_finw)+log(income)+y_2008+y_2010+y_2012+y_2014+y_2016+y_2018+y_2020, data=dataset, family = binomial(link="probit"))
summary(reg8)
lht(reg8,c("empl=selfempl"), test="Chisq")


#point D)
CountR2 =function(m) mean(m$y==round(m$fitted.values))
CountR2(reg8)
