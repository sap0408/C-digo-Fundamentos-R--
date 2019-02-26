# Codigo-Fundamentos-R--
Analizando datos de economía naranja con R. Jugando con mtcars antes de procesar orange economy.

#EDA Scatter plot mtcars
plot(mtcars$mpg ~ mtcars$cyl,
     xlab="cilindros", ylab = "millas por galón",
     main="Relación cilindros y millas por galón")

plot(mtcars$mpg ~ mtcars$hp,
     xlab="caballos de fuerza", ylab = "millas por galón",
     main="Relación caballos de fuerza y millas por galón")

#EDA orangeec

plot(orangeec$Unemployment ~ orangeec$Education.invest...GDP,
     xlab="Inversión educación (%PIB)",
     ylab = "Desempleo",
     main="Relación inversión en educación y desempleo")

#Otro plot
plot(orangeec$GDP.PC ~ orangeec$Creat.Ind...GDP,
     xlab="Aporte economia naranja al PIB(%)",
     ylab = "PIB per cápita",
     main="Relación economía naranja y pib per cápita")

#histograma mtcars qplot
qplot(mtcars$hp,
      geom="histogram",
      xlab="caballos de fuerza",
      main="Carros según caballos de fuerza")

#ahora con ggplot
ggplot(mtcars, aes(x=hp))+
  geom_histogram()+
  labs(x="Caballos de fuerza", y="Cantidad de carros",
       title="Caballos de fuerza en carros seleccionados")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
#ggplot y ajustando tamaño de barras con binwidth
ggplot(mtcars, aes(x=hp))+
  geom_histogram(binwidth = 30)+
  labs(x="Caballos de fuerza", y="Cantidad de carros",
       title="Caballos de fuerza en carros seleccionados")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#hist mtcars
ggplot()+geom_histogram(data=mtcars,
                        aes(x=hp),fill="blue",color="red",
                        binwidth = 20)+
  labs(x="Caballos de fuerza", y="Cantidad de carros",
       title="Caballos de fuerza en carros seleccionados")+
  xlim(c(80,280))+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#hist orangeec
ggplot()+geom_histogram(data=orangeec,
                        aes(x=GDP.PC),fill="blue",color="red",
                        binwidth = 2000)+
  labs(x="pib per cápita", y="Cantidad de paises",
       title="PIB per cápita en paises latam")+
   theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#
ggplot()+geom_histogram(data=orangeec,
                        aes(x=Creat.Ind...GDP),fill="blue",color="red",
                        binwidth = 1)+
  labs(x="Aporte economia naranja al pib(%)", y="Cantidad de paises",
       title="Contribución economia naranja al pib en paises latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#
ggplot()+geom_histogram(data=orangeec,
                        aes(x=Internet.penetration...population),fill="red",color="yellow",
                        binwidth = 5)+
  labs(x="Penetración internet(%)población", y="Cantidad de paises",
       title="Penetración de internet en paises latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#boxplot
boxplot(mtcars$hp,
        ylab="caballos de fuerza",
        main="Caballos de fuerza en carros mtcars")

#box plot con ggplot
ggplot(mtcars,aes(x=as.factor(cyl),y=hp,fill=cyl))+
  geom_boxplot(alpha=0.6)+
  labs(x="cilindros", y="caballos de fuerza",
       title="Caballos de fuerza según cilindros en mtcars")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#
ggplot(mtcars,aes(x=am, y=mpg, fill=am))+
  geom_boxplot()+
  labs(x="Tipo de caja", y="millas por galón",
       title="Millas por galón según tipo de caja-mtcars")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Cambiar etiquetas en variable. No true y false sino Manual y Automático.
mtcars$am <- factor(mtcars$am, levels=c(TRUE,FALSE),
                    labels=c("Manual","Automático"))


#Sacando promedios de variables
economy <- mean(orangeec$GDP.PC)
economy

#Creación de nueva variable (nueva columna)
orangeec <- orangeec %>%
  mutate(Strong_economy = ifelse(GDP.PC < economy,
                    "Por debajo promedio pib per cápita",
                    "Sobre-Arriba promedio pib per cápita"))

#box plot con ggplot y colores mas bonitos usando alpha.
ggplot(orangeec, aes(x=Strong_economy, y=Creat.Ind...GDP,
                     fill=Strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de país", y="Aporte economia naranja al pib",
       title="Aporte economia naranja en pib paises latam con
alto y bajo pib per cápita")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Otro box plot
ggplot(orangeec, aes(x=Strong_economy, y=Internet.penetration...population,
                     fill=Strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de país", y="Penetración de internet(%)",
       title="Penetración de internet en paises latam con
alto y bajo pib per cápita")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Relacionar variables para identificar visualmente posibles correlaciones: Usar scatter plot con geom_point de ggplot.
#scatter plot con ggplot en mtcars - dos variables
ggplot(mtcars, aes(hp,mpg))+
  geom_point()+
  labs(x="caballos fuerza", y="millas por galón",
       title="Relación caballos de fuerza y millas por galón")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#scatter plot
ggplot(mtcars, aes(wt,hp))+
  geom_point()+
  labs(x="peso", y="potencia",
       title="Relación peso-potencia")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#otro scatter plot que relaciona otras dos variables de mtcars.
ggplot(mtcars, aes(hp,qsec))+
  geom_point(aes(color=am, size=cyl))+
  labs(s="caballos de fuerza", y="tiempo en 1/4 milla",
       title="caballos-velocidad según cilindraje y
       tipo de caja")

#scatter plot con dataset economia naranja relacionando CUATRO variables: X, y, tamaño del punto y color. 
ggplot(orangeec, aes(Internet.penetration...population,Creat.Ind...GDP))+
  geom_point(aes(color=factor(Strong_economy), size=GDP.Growth..))+
  labs(x="Penetración Internet", y="Aporte economia naranja PIB",
       title="Internet y aporte economia naranja según economia y 
       crecimiento pib")

#Promedios en variable de economia naranja.
education <- mean(orangeec$Education.invest...GDP)
education

#Creando una nueva variable (columna) en data set economia naranja. 
orangeec <- orangeec %>%
  mutate(Inversión_Educación = ifelse(Education.invest...GDP < education,
                                 "Por debajo del promedio de inversión",
                                 "Igual-sobre promedio de inversión"))
                                 
#Scatter plot con cuatro variables. Una es la nueva clasificación según promedio de educación en educación. 
ggplot(orangeec, aes(Internet.penetration...population,Creat.Ind...GDP))+
  geom_point(aes(color=factor(Inversión_Educación), size=GDP.Growth..))+
  labs(x="Penetración Internet", y="Aporte economia naranja PIB",
       title="Internet y aporte economia naranja según crecimiento pib
       e inversión en educación")

#Preparando el uso de ggplotly
my_graph <- ggplot(orangeec, aes(Internet.penetration...population,
                                 Creat.Ind...GDP,label=row.names(orangeec)))+
  geom_point()+
  labs(x="Penetración internet", y="Aporte economia naranja",
       title="Penetración Internet y aporte economia naranja")
my_graph

p = ggplotly(my_graph)
p

#Ahora varios scatter plot a la vez con pairs.
pairs(mtcars)

#Selección de variables a relacionar usando subset
newdata <- subset(mtcars, select=c(2,7:8,11,12))
pairs(newdata)

#La misma selección pero eliminando las variables que no se quieren.
pairs(mtcars[,-c(1,3,4,5,6,9,10)])

#Filtrado de observaciones según una variable millas por galón.
Eficientes <- filter(mtcars, mpg >=30)
Eficientes

#
pairs(Eficientes[,2:6])

#Ubicando observaciones que empiezan con determinadas letras. En este caso, nombres de carros que empiecen por "merc".
merc <- mtcars %>%
  filter(str_detect(model,"Merc"))
merc

#varios scatter plot de solo ese tipo de carros (merc) relacionando las variables 2 a la 6
pairs(merc[,2:6])

#Relacion de variables de todas las observaciones de mtcars, de las columnas 2 a la 6.
pairs(mtcars[,2:6])

#Correlación para confirmar con número (entre 1 y -1) si efectivamente existe correlación.
cor(mtcars[,2:6])

#Sacando las correlaciones de las graficas que ya teníamos con pairs. Lo unico que cambia es pairs, por "cor".
cor(newdata)

cor(merc[,2:6])

#Ahora con economia naranja.
pairs(orangeec[,2:6])

#
pairs(orangeec[,5:10])

#
newdata <- subset(orangeec,select=c(5,6,10,11,12,13))
newdata

pairs(newdata)

#En esta correlación veremos "NA" porque hay espacios vacios...
cor(orangeec[,2:6])

#Entonces anulamos los espacios vacios pidiendo a R que solo correlacione las observaciones completas: "complete.obs"
cor(orangeec[,2:6],use="complete.obs")

#
cor(orangeec[,5:10],use="complete.obs")
#
cor(newdata, use="complete.obs")

#Resúmen:valores claves en estadística.
summary(mtcars)

#desviación estandar: standard deviation (sd)
sd(mtcars$mpg)
desv <- sd(mtcars$mpg)
mean(mtcars$mpg)

#
prom <- mean(mtcars$mpg)
prom

#Coeficiente de variación de la desviación para saber qué tan desviados estan los datos del promedio.
CoefVar <- (desv/prom)*100
CoefVar

#Ahora lo mismo para economia naranja. 
summary(orangeec)

#
sd(orangeec$Internet.penetration...population)
desv <- sd(orangeec$Internet.penetration...population)
desv

mean(orangeec$Internet.penetration...population)
prom <- mean(orangeec$Internet.penetration...population)
prom
#
CoefVar <- (desv/prom)*100
CoefVar

summary(orangeec)

#
mean(orangeec$Creat.Ind...GDP)
mean(orangeec$Creat.Ind...GDP, na.rm=TRUE)
prom <- mean(orangeec$Creat.Ind...GDP, na.rm=TRUE)

#
sd(orangeec$Creat.Ind...GDP)
sd(orangeec$Creat.Ind...GDP, na.rm=TRUE)
desv <- sd(orangeec$Creat.Ind...GDP, na.rm=TRUE)

#
CoefVar <- (desv/prom)*100
CoefVar

#Ajustando datos para mejorar visualizaciones
eficientes <- mean(mtcars$mpg)
eficientes

mtcars <- mtcars %>%
  mutate(Mas_eficientes=ifelse(mpg<eficientes,
                    "bajo promedio","en ó sobre promedio"))

Mas_veloces <- mtcars[mtcars$qsec<16,]
Mas_veloces

mtcars <- mtcars %>%
  mutate(Velocidad_Cuarto_milla=ifelse(qsec < 16,
                      "Menos 16 segs",
                      "Mas 16 segs"))

#Creando nueva variable con mutate (nueva columna)
mtcars <- mtcars %>%
  mutate(Peso_kilos=(wt/2)*1000)

#Y rebautizando datos de variables según selección.
mtcars <- mtcars %>%
  mutate(Peso=ifelse(Peso_kilos <= 1500,
                    "Livianos", "Pesados"))

#
orangeec <- orangeec %>%
  mutate(Crecimiento_GDP = ifelse(GDP.Growth.. >= 2.5,
                          "2.5% ó más","Menos 2.5%"))
#
orangeec <- orangeec %>%
  mutate(Anaranjados=ifelse(Creat.Ind...GDP >= 2.5,
                    "Mas anaranjados", "Menos anaranjados"))

#Para ver ranking, pedimos un orden en sentido descendente con arrange y desc 
orangeec %>%
  arrange(desc(Creat.Ind...GDP))

#Buscando paises con nombres especificos con %in%
TopNaranjas <- orangeec %>%
  filter(Country %in% c("Mexico", "Panama", "Argentina",
                        "Colombia", "Brazil","Paraguay"))

TopNaranjas

#
TopNaranjas %>%
  arrange(desc(Creat.Ind...GDP))

#
mtcars %>%
  arrange(desc(Peso_kilos))

Mas_pesados <- mtcars %>%
  filter(model %in% c("Lincoln Continental", "Chrysler Imperial",
                      "Cadillac Fleetwood", "Merc 450SE"))

Mas_pesados

#viendo varios scatter plot en una misma imagen, usando facet_wrap
ggplot(Mas_pesados, aes(x=hp, y=mpg))+
  geom_point()+
  facet_wrap(~model)

#
ggplot(mtcars, aes(x=cyl, y=mpg, size=Peso))+
  geom_point()+
  facet_wrap(~ am)

#
ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                        y=Services...GDP, size=GDP.PC))+
  geom_point()+
  facet_wrap(~Country)

#
ggplot(TopNaranjas, aes(x=Education.invest...GDP,
                        y=Creat.Ind...GDP, size=Unemployment))+
  geom_point()+
  facet_wrap(~Country)

#A divertirse con R. Me cuentan!


