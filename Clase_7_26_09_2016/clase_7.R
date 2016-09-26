# 1. leer datos

library(data.table)
tabla_normalizada <- read.csv("tabulacion_cruzada.csv")
setnames(tabla_normalizada, colnames(tabla_normalizada), c("categoria","titulo","nota","grupo"))
tabla_normalizada$exito <- ifelse(tabla_normalizada$grupo == "Éxito", 1, 0)
tabla_normalizada$fracaso <- ifelse(tabla_normalizada$grupo == "Fracaso", 1, 0)

# 2. transformar datos

library(dplyr)
tabla_normalizada_2 <- tabla_normalizada[,c("categoria","exito","fracaso")]
tabulacion_cruzada <- as.data.frame(tabla_normalizada_2 %>% group_by(categoria) %>% 
                                       summarise_each(funs(sum)))
colnames(tabulacion_cruzada)

# 3. graficar

library(ggplot2)
grafico <- ggplot() + 
  geom_bar(aes(y = exito, x = categoria, fill = "Éxito"), data = tabulacion_cruzada, stat="identity") +
  geom_bar(aes(y = -1*fracaso, x = categoria, fill = "Fracaso"), data = tabulacion_cruzada, stat="identity") +
  geom_hline(yintercept = 0, color = "blue", size=1.0, linetype = "dashed")

grafico <- grafico + labs(x="Categoría", y="Éxitos y fracasos") + 
  ggtitle("Éxitos y fracasos de sagas de películas")
  theme(legend.position="bottom", legend.direction="horizontal")

grafico

# hasta acá está ok, lo que viene ahora es el resultado de mucho ocio / buscar la perfección
# esto se basa en The Hitchhiker's Guide to Ggplot2 in R

grafico <- grafico +
  scale_fill_manual(values=c("#40b8d0", "#b2d183")) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5), 
        axis.text.x=element_text(colour="black", size = 10), 
        axis.text.y=element_text(colour="black", size = 10),
        legend.key=element_rect(fill="white", colour="white"),
        legend.position="bottom", legend.direction="horizontal", 
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"), 
        text=element_text(family="Tahoma")) +
  scale_y_continuous(breaks=seq(min(-1*tabulacion_cruzada$fracaso),max(tabulacion_cruzada$exito),1))
  
grafico

