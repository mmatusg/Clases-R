# Primera tarea

## Parte 1

setwd("/Users/pacha/clases_profesor_mario_matus/clase2/")
exports_data <- read.csv("copper_data.csv")

library(ggplot2)
ggplot() + geom_line(aes(y = export, x = year, colour = product),
                     data = exports_data, stat="identity")

## Parte 2

ggplot() + geom_bar(aes(y = export, x = year, colour = product),
                     data = exports_data, stat="identity")

ggplot() + geom_point(aes(y = export, x = year, colour = product),
                    data = exports_data, stat="identity")

# Segunda tarea

ggplot() + geom_point(aes(y = Fertility, x = Education),
                     data = swiss, stat="identity")

ggplot(swiss, aes(x=Education, y=Fertility)) + geom_point(shape=1) + geom_smooth(method=lm)

# Tercera tarea

cor(swiss, method="pearson")

#install.packages("GGally")
library(GGally)
ggpairs(swiss)

### truco ###
source("ggally_mod.R")

ggpairs(swiss,
        lower=list(continuous=wrap("smooth", colour="blue")),
        diag=list(continuous=wrap("barDiag", fill="blue")),
        upper=list(continuous=cor_fun))

# Cuarta tarea

library(XLConnect)

gini <- readWorksheetFromFile("gini_por_regiones_modificado.xlsx", sheet = "Sheet1", region = "A1:K14", header = TRUE)
gini_stgo <- subset(gini, region == "Metropolitana")

gini_grafico <- readWorksheetFromFile("gini_por_regiones_modificado.xlsx", sheet = "Sheet1", region = "A18:B28", header = TRUE)
ggplot() + geom_line(aes(y = gini, x = anio), data = gini_grafico, stat="identity")

t.test(gini$X2008, gini$X1990, alternative = "two.sided", paired = TRUE)
t.test(gini$X2008 - gini$X1990, alternative = "two.sided") #alternativamente
t.test(gini$X2008, gini$X1990, alternative = "less", paired = TRUE) #no es lo mismo

