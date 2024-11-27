### Importing data
install.packages("readxl")
library(readxl)
file.choose()
ruta_excel<-"/Users/dianaco/Desktop/ENOE/BaseCOVID.xlsx"

### Loading packages
library(readxl)
library(dplyr)
positiv <- read_excel(ruta_excel)

### Create PANEL DATA DATASET

Data_Panel <- positiv %>% 
  group_by(anio) %>% 
  arrange(ent)

#### PLM packages
installed.packages("plm")
library(plm)
pdata = pdata.frame(Data_Panel, index = c("ent", "anio"))

### Methods of Data Panel POOLED
pooledmethod=plm(positivos ~ rps + dns + ssegs + sm, data = pdata, model = "pooling")
summary(pooledmethod)

pooledmethod1=plm(positivos ~ rps+ ssegs + sm, data = pdata, model = "pooling")
summary(pooledmethod1)

#### Methods of Data Panel FIXED METHOD
femethod =plm(positivos ~  dns + ssegs + sm, data = pdata, model = "within")
summary(femethod)

#### Methods od Data Panel RANDOM METHOD
remethod = plm(positivos ~ rps + dns + ssegs + sm, data = pdata, model = "random")
summary(remethod)


### Test de Pooled Method or Poolability
pooltest(positivos ~ rps + dns + ssegs + sm, data = pdata, model = "within")

##### Pooled OLS or Fixed Effecet Model
pFtest(femethod,pooledmethod)

# Se rechazo la hipotesis nula y nos quedamos con efectos fijos. 


### Hausmman Test
phtest(femethod,remethod)

## Se rechaza la Ho, por lo tanto nos quedamos con efectos fijos porque es consistente.

#### PRUEBAS ECONOMETRICAS


###AUTOCORRELACION
pdwtest(positivos ~ rps + dns + ssegs + sm, data = pdata, model = "within")

# Ho: there is no autocorrelation in error term (NO HAY).
# p no menor a 0.05, no se rechaza. NO HAY AUTOCORRELACIO4N.


#### HOMOCEDASTICIDAD
library(lmtest)
bptest(positivos ~ rps + dns + ssegs + sm, data = pdata, studentize = F)


### Si hay heterocedasticidad, hay que corregir el modelo. <0.05
coeftest(femethod,vcovHC)

### Corregiendo los coeficientes heterocedasticidad. 
coeftest(femethod,vcovHC(femethod,method = "arellano"))


#### Box plots

Grafica <- df %>% 
  group_by(anio,ent) %>% 
  summarize(mediana_def = median(def), mediana_rps = median(rps), mediana_dns = median(dns)) 

Medianas_2020 <- Grafica %>% 
  filter(anio == 2020)

Medianas_2021 <- Grafica %>% 
  filter(anio == 2021)

Medianas_2022 <- Grafica %>% 
  filter(anio == 2022)

#ags_rps <- df %>% 
#           filter(ent== "AGUASCALIENTES") %>% 
#           select(rps) %>% 
#           summarize(median(rps))


#### plotting
library(ggplot2)

defunciones <- ggplot(Medianas_2020, aes(x=ent, y= mediana_def, color = anio)) + 
  geom_boxplot() + 
  labs(x="Entidades Federativas", y="Defunciones") +
  theme(axis.text.x = element_text(angle=90, hjust=1))


defunciones


defunciones <- ggplot(a, aes(x=ent, y= mediana_def, color = 2021)) + 
  geom_boxplot() + 
  labs(x="Entidades Federativas", y="Defunciones") +
  theme(axis.text.x = element_text(angle=90, hjust=1))


rps <- ggplot(positiv, aes(x=ent, y= rps, color = anio)) + 
  geom_boxplot() + 
  labs(x="Groups", y="RPS") +
  theme(axis.text.x = element_text(angle=90, hjust=1))
rps

positivos <- ggplot(positiv, aes(x=ent, y= positivos, color = anio)) + 
  geom_boxplot() + 
  labs(x="Entidades Federativas", y="positivos") +
  theme(axis.text.x = element_text(angle=90, hjust=1))
positivos

ssegs <- ggplot(positiv, aes(x=ent, y= ssegs, color = anio)) + 
  geom_boxplot() + 
  labs(x="Entidades Federativas", y="ssegs") +
  theme(axis.text.x = element_text(angle=90, hjust=1))
ssegs

dns <- ggplot(positiv, aes(x=ent, y= dns, color = anio)) + 
  geom_boxplot() + 
  labs(x="Entidades Federativas", y="DNS") +
  theme(axis.text.x = element_text(angle=90, hjust=1))
dns

sm  <-  ggplot(df, aes(x=ent, y= sm, color = anio)) + 
  geom_boxplot() + 
  labs(x="Entidades Federativas", y="SM") +
  theme(axis.text.x = element_text(angle=90, hjust=1))



positiv %>%
  group_by("anio") %>%
  summarise_all(funs(mean,sd))

FGLS <- pggls(positivos~rps+dns+ssegs+sm, data=pdata, model = "within")
summary(FGLS)

###Modelo de regresión ponderada de mínimos cuadrados
install.packages("nlme")
library(nlme)

modelo_pcce <- pcce(positivos ~ rps+dns+ssegs+sm, data =pdata, panelVar = "ent", lag=1)
summary(modelo_pcce)




####Verificar que no hay heterocedasticidad
bptest(FGLS, data = pdata, studentize = F)

####(Ho): homocedasticidad  
