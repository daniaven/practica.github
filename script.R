# librerias ####
library(tidyverse)

# cargar datos ####
# es un set de dagos de pinguinos que se usa de ejemplo para exploracion y visualización de datos
# https://github.com/allisonhorst/palmerpenguins
datos=read.csv("data/penguins.csv", row.names = 1)

# resumen de lo datos ####

# 1
str(datos)

# 2
datos %>% 
  group_by(species) %>% 
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

# graficar cosas ####

# asociación entre el largo de la aleta y la masa  del pinguino

grafico1 <- ggplot(data = datos, 
                       aes(x = flipper_length_mm,
                           y = body_mass_g)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 3,
             alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Gráfico 1",
       subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.2, 0.7),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

grafico1

ggsave( plot = grafico1, filename = "grafico.aletas.png", device = "png", path = "figuras")


# graficar un boxplot de masa por sexo del pinguino (una persona hace este)

p<-ggplot(datos, aes(x=sex, y=body_mass_g, fill=sex)) +
  scale_fill_manual(values=c("#FE4A49", "#FED766", "#009FB7"))+
  geom_boxplot()
  theme_minimal()
p


# graficar la correlacion entre bill_length y bill_depth (una persona hace esto)

cor <- cor.test(datos$bill_length_mm, datos$bill_depth_mm, method = "spearman")
cor

p <- datos %>%
  ggplot() +
  aes(x=bill_depth_mm,y=bill_length_mm)+
  geom_point(color="#E31A1C") +
  labs(y="bill_length", x = "bill_depth",
       title="bill_length vs bill_depth",
       caption=paste0("rho(Spearman)= -0.2217492, p-val= 3.512e-05")) +
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank())

p + scale_fill_brewer(palette = "Paired")+theme(plot.caption=element_text(color="#1F78B4"))
p + geom_smooth(method=lm)

# cual es la distribucion de especies segun la isla??

distribucion <- table(datos$species, datos$island)
print(distribucion)
