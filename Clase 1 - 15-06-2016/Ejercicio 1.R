data(swiss) #cargar base
str(swiss) #informacion de cada columna
#install.packages("ggplot2") #instalar extension (1era vez)
library(ggplot2) #cargar extension
ggplot() + geom_line(aes(y = Fertility, x = Education), 
                     data = swiss, stat="identity") #graficar

#guardar: ctrl+s

#ejecutar: ctrl+enter
