#install.packages("foreign")
#install.packages("httr")
#install.packages("plyr")

library(foreign) #para leer archivos de Stata, SPSS, etc
library(httr) #permite descargar desde la web
library(plyr) #comandos útiles para bases de datos

# para los gráficos
library(ggplot2)
library(ggthemes)

###########################
# Diccionario de variables
###########################

# Lo primero es consultar el diccionario de variables donde aparece la explicación de los códigos y la naturaleza de las variables
# http://observatorio.ministeriodesarrollosocial.gob.cl/documentos/Libro_de_Codigos_Casen_2013_Base_Principal_Metodologia_Nueva.pdf

####################################
# Descargar y cargar base de datos
####################################

url <- "http://pachamaltese.github.io/analisis-de-datos-unab/laboratorio4/casen2013.dta.zip"
zip <- "casen2013.dta.zip"
dta <- "casen2013.dta"

if(!file.exists(zip)) {
  print("descargando")
  download.file(url, zip, method="curl")
}

if(!file.exists(dta) & file.exists(zip)) {
  unzip(zip)
  casen <- read.dta(dta)
}

if(file.exists(dta)) {
  casen <- read.dta(dta)
}

####################################
# Limpiar datos y generar variables
####################################

# creo una copia que ahorrará tiempo si quiero retroceder algunos pasos
# o sea, si me equivoco vuelvo a ejecutar esta línea y la variable casen sigue intacta (ahorra tiempo)
data <- casen

# conservo en memoria únicamente las variables relevantes para la regresión
# para esto hay que consultar el diccionario de variables
keep <- c("expr", "yoprcor", "edad", "esc", "sexo", "region", "rama1", "o10")
data <- data[keep]

# veo el tipo de variables en memoria
str(data) #hay variables enteras, factores y numéricas 

# saco las filas que tengan celdas vacías (o sino habrá que hacer pasos adicionales al final)
data <- na.omit(data)

# hay que crear la variable "logaritmo del salario por hora"
# se consideran los datos de los trabajadores con jornada > 30 horas/semana
# para el cálculo hay que calcular 
# salario ($/mes) * 12 (meses/año) / jornada (horas/semana) * 52 (semanas/año)
data <- data[data$o10 > 30,]
data$WHP <- (data$yoprcor*12)/(data$o10*52)
data$logWHP <- log(data$WHP)

# conservo solo los datos que corresponden al tramo de edad entre 35 y 45 años
#data <- data[(data$edad >= 35 & data$edad <= 45),]

# conservo solo los datos que corresponden al nivel de escolaridad pedido
# profesional en Chile = 5 años (o más) de educación superior
# entonces Experiencia Laboral = Edad - Años de Escolaridad ("esc") - 5 (duración mínima de estudios profesionales)
# la ecuación de arriba puede tomar valores negativos ya que hay personas en la encuesta que se encuentran estudiando 
# y trabajan part-time, realizan continuidad de estudios, tienen post-grado, etc. entonces hay que descartar esos casos
data$exp <- data$edad - data$esc - 5
data <- data[data$exp >= 0,]
data$exp2 <- (data$exp)^2

# veo como están asignados los niveles del factor región
levels(data$region)

# los nombres de los niveles contienen espacios, entonces asigno nombres simples
# (ver el excel que usé para hacer que el proceso sea eficiente)
data$region <- revalue(data$region, c("i. tarapaca"="r1",
                                      "ii. antofagasta"="r2",
                                      "iii. atacama"="r3",
                                      "iv. coquimbo"="r4",
                                      "v. valpara\xedso"="r5",
                                      "vi. o higgins"="r6",
                                      "vii. maule"="r7",
                                      "viii. biob\xedo"="r8",
                                      "ix. la araucan\xeda"="r9",
                                      "x. los lagos"="r10",
                                      "xi. ays\xe9n"="r11",
                                      "xii. magallanes"="r12",
                                      "metropolitana"="r13",
                                      "xiv. los r\xedos"="r14",
                                      "xv. arica y parinacota"="r15"))

data$region2 <- ifelse(data$region == "r1", "Tarapaca",
                       ifelse(data$region == "r2", "Antofagasta",
                              ifelse(data$region == "r3", "Atacama",
                                     ifelse(data$region == "r4", "Coquimbo",
                                            ifelse(data$region == "r5", "Valparaiso",
                                                   ifelse(data$region == "r6", "OHiggins",
                                                          ifelse(data$region == "r7", "Maule",
                                                                 ifelse(data$region == "r8", "Biobio",
                                                                        ifelse(data$region == "r9", "La Araucania",
                                                                               ifelse(data$region == "r10", "Los Lagos",
                                                                                      ifelse(data$region == "r11", "Aysen",
                                                                                             ifelse(data$region == "r12", "Magallanes",
                                                                                                    ifelse(data$region == "r13", "Metropolitana",
                                                                                                           ifelse(data$region == "r14", "Los Rios", "Arica y Parinacota"))))))))))))))

data$region2 <- as.factor(data$region2)

# verifico que los nombres asignados a las regiones
levels(data$region)
levels(data$region2)

# veo como están asignados los niveles del factor región
levels(data$rama1)

# aparece un nivel que corresponde a "sin clasificar" (nivel X), entonces debo sacar ese nivel y volver a definir los niveles
data <- data[data$rama1 != "x. no bien especificado",]
data$rama1 <- factor(data$rama1) 

# verifico los cambios
levels(data$rama1)

# los nombres de los niveles contienen espacios, entonces asigno nombres simples
data$rama1 <- revalue(data$rama1, c("a. agricultura, ganader\xeda, caza y silvicultura"="sa",
                                    "b. pesca"="sb",
                                    "c. explotaci\xf3n de minas y canteras"="sc",
                                    "d. industrias manufactureras"="sd",
                                    "e. suministro de electricidad, gas y agua"="se",
                                    "f.construcci\xf3n"="sf",
                                    "g. comercio al por mayor y al por menor"="sg",
                                    "h. hoteles y restaurantes"="sh",
                                    "i. transporte, almacenamiento y comunicaciones"="si",
                                    "j. intermediaci\xf3n financiera"="sj",
                                    "k. actividades inmobiliarias, empresariales y de alquiler"="sk",
                                    "l.administrasci\xf3n p\xfablica y defensa"="sl",
                                    "m. ense\xf1anza"="sm",
                                    "n. servicios sociales y de salud"="sn",
                                    "o. otras actividades de servicios comunitarios, sociales y personales"="so",
                                    "p. hogares privados con servicio dom\xe9stico"="sp",
                                    "q.organizaciones y organos extraterritoriales"="sq"))

data$rama1_2 <- ifelse(data$rama1 == "sa", "Silvoagropecuario",
                       ifelse(data$rama1 == "sb", "Pesca",
                              ifelse(data$rama1 == "sc", "Minería",
                                     ifelse(data$rama1 == "sd", "Industria\nmanufacturera",
                                            ifelse(data$rama1 == "se", "Servicios\nbásicos",
                                                   ifelse(data$rama1 == "sf", "Construcción",
                                                          ifelse(data$rama1 == "sg", "Comercio",
                                                                 ifelse(data$rama1 == "sh", "Hoteles\ny restaurantes",
                                                                        ifelse(data$rama1 == "si", "Transporte\ny comunicaciones",
                                                                               ifelse(data$rama1 == "sj", "Banca",
                                                                                      ifelse(data$rama1 == "sk", "Inmobiliario",
                                                                                             ifelse(data$rama1 == "sl", "Administración\npública",
                                                                                                    ifelse(data$rama1 == "sm", "Enseñanza",
                                                                                                           ifelse(data$rama1 == "sn", "Salud",
                                                                                                                  ifelse(data$rama1 == "so", "Otros",
                                                                                                                         ifelse(data$rama1 == "sp", "Servicio\ndoméstico", "ONGs"))))))))))))))))
data$rama1_2 <- as.factor(data$rama1_2)

data$rama1_2 <- ordered(data$rama1_2, levels = c("Silvoagropecuario", "Pesca", "Minería", "Industria\nmanufacturera", "Servicios\nbásicos", "Construcción",
                                                 "Comercio", "Hoteles\ny restaurantes", "Transporte\ny comunicaciones", "Banca", "Inmobiliario", "Administración\npública",
                                                 "Enseñanza", "Salud", "Otros", "Servicio\ndoméstico", "ONGs"))


# verifico los cambios
levels(data$rama1)
levels(data$rama1_2)

# creo una variable que agrupe el salario por hora por cuartiles

summary(data$WHP)

data$WHP2 <- ifelse(data$WHP > 0 & data$WHP <= 1035.00, "Primer cuartil",
                    ifelse(data$WHP > 1035.00 & data$WHP <= 2102.00, "Segundo cuartil",
                           ifelse(data$WHP > 2102.00 & data$WHP <= 2308.00, "Tercer cuartil", "Cuarto cuartil")))

data$WHP2 <- ordered(data$WHP2, levels = c("Primer cuartil", "Segundo cuartil", "Tercer cuartil", "Cuarto cuartil"))

# pongo mayusculas a la variable sexo

data$sexo <- ifelse(data$sexo == "mujer", "Mujer", "Hombre")

# creo una variable que agrupe la escolaridad por niveles

data$esc2 <- ifelse((data$esc > 0) & (data$esc < 8), "Basica incompleta",
                         ifelse((data$esc == 8), "Basica completa",
                                ifelse((data$esc > 8 ) & (data$esc < 12), "Media incompleta",
                                       ifelse((data$esc == 12), "Media completa",
                                              ifelse((data$esc > 12) & (data$esc < 14), "Tecnica superior incompleta",
                                                     ifelse((data$esc == 14), "Tecnica superior completa",
                                                            ifelse((data$esc > 14) & (data$esc < 17), "Universitaria incompleta",
                                                                   ifelse((data$esc == 17), "Universitaria completa",
                                                                          ifelse((data$esc >= 17), "Posgrado", "NA")))))))))

quantile(data$yoprcor, probs = seq(0, 1, 0.1), na.rm = TRUE)

data$yoprcor2 = ifelse(data$yoprcor <= 157000, "($0,\n$157.000]",
                       ifelse(data$yoprcor > 157000 & data$yoprcor <= 200000, "($157.000,\n$200.000]",
                              ifelse(data$yoprcor > 200000 & data$yoprcor <= 210000, "($200.000,\n$210.000]",
                                     ifelse(data$yoprcor > 210000 & data$yoprcor <= 230000, "($210.000,\n$230.000]",
                                            ifelse(data$yoprcor > 230000 & data$yoprcor <= 270000, "($230.000,\n$270.000]",
                                                   ifelse(data$yoprcor > 270000 & data$yoprcor <= 300000, "($270.000,\n$300.000]",
                                                          ifelse(data$yoprcor > 300000 & data$yoprcor <= 400000, "($300.000,\n$400.000]",
                                                                 ifelse(data$yoprcor > 400000 & data$yoprcor <= 500000, "($400.000,\n$500.000]",
                                                                        ifelse(data$yoprcor > 500000 & data$yoprcor <= 750000, "($500.000,\n$750.000]", "($750.000,\n$25.000.000]")))))))))

data$yoprcor2 <- ordered(data$yoprcor2, levels = c("[$0,\n$157.000]", "($157.000,\n$200.000]", "($200.000,\n$210.000]", "($210.000,\n$230.000]", "($230.000,\n$270.000]", 
                                               "($270.000,\n$300.000]", "($300.000,\n$400.000]", "($400.000,\n$500.000]", "($500.000,\n$750.000]", "($750.000,\n$25.000.000]"))

quantile(data$yoprcor, probs = seq(0, 1, 0.25), na.rm = TRUE)

data$yoprcor3 <- ifelse(data$yoprcor <= 210000, "($0,\n$210.000]",
                        ifelse(data$yoprcor <= 270000 & data$yoprcor > 210000, "($210.000,\n$270.000]",
                               ifelse(data$yoprcor <= 420000 & data$yoprcor > 270000, "($270.000,\n$420.000]", "($420.000,\n$25.002.000]")))

data$yoprcor3 <- ordered(data$yoprcor3, levels = c("($0,\n$210.000]", "($210.000,\n$270.000]", "($270.000,\n$420.000]", "($420.000,\n$25.002.000]"))

save(data, file = "casen_datos_limpios.RData")

#############
# Estadística descriptiva
#############

# tablas resumen
tabla1 <- with(data, addmargins(table(sexo, yoprcor2, useNA = "ifany")))
tabla2 <- with(data, addmargins(table(region2, yoprcor2, useNA = "ifany")))
tabla3 <- with(data, addmargins(table(rama1_2, yoprcor2, useNA = "ifany")))
tabla4 <- with(data, addmargins(table(esc2, yoprcor2, useNA = "ifany")))

write.csv(tabla1, "tabla1.csv")
write.csv(tabla2, "tabla2.csv")
write.csv(tabla3, "tabla3.csv")
write.csv(tabla4, "tabla4.csv")

# graficos 7ma region

data_r7 <- data[data$region == "r7",]
qplot(factor(sexo), data=data_r7, geom="bar", fill=factor(esc2)) + coord_flip() + theme(legend.position="bottom")

# graficos pais

g1 <- ggplot(data, aes(x = yoprcor2, fill = sexo))
g1 + geom_bar(width = 1) + coord_polar(start = 0) + 
  theme_fivethirtyeight() + scale_fill_fivethirtyeight() + 
  theme(axis.title = element_text(family="Atlas Grotesk Regular", size=15),
        legend.position="bottom", legend.direction="horizontal", 
        legend.title=element_blank(),
        plot.title=element_text(family="Atlas Grotesk Medium", size=32), 
        legend.text=element_text(family="Atlas Grotesk Regular", size=15),
        axis.text.x=element_text(family="DecimaMonoPro", size=15)) +
  labs(x="Rama Ocupacional", y="Número de personas") +
  ggtitle("Distribución de la Población\npor Ingreso de la Ocupación Principal")

g2 <- ggplot(data, aes(x = rama1_2, fill = sexo))
g2 + geom_bar(width = 1) + coord_polar(start = 5*pi/4) + 
  theme_fivethirtyeight() + scale_fill_fivethirtyeight() + 
  theme(axis.title = element_text(family="Atlas Grotesk Regular", size=15),
        legend.position="bottom", legend.direction="horizontal", 
        legend.title=element_blank(),
        plot.title=element_text(family="Atlas Grotesk Medium", size=32), 
        legend.text=element_text(family="Atlas Grotesk Regular", size=15),
        axis.text.x=element_text(family="DecimaMonoPro", size=15)) +
  labs(x="Rama Ocupacional", y="Número de personas") +
  ggtitle("Distribución de la Población\npor Rama Ocupacional y Sexo")

#############
# Regresiones
#############

data$r13 = ifelse(data$region == "r13", 1, 0)
data$sl = ifelse(data$rama1 == "sl", 1, 0)

# estimadores para la muestra
summary(lm(logWHP ~ sexo + esc + exp + exp2 + r13 + sl, data = data))

# estimadores para la población (población = país)
summary(lm(logWHP ~ sexo + esc + exp + exp2 + r13 + sl, data = data, weights = expr))
