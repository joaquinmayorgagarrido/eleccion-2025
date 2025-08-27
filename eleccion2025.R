
rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggrepel) # TO add labels to scatterplot bubbles in ggplot
library(readxl)

eleccion20_df <- read_excel("Datos OEP/EG2020_20201026_234234_3811128304235370806.xlsx")
eleccion20_df <- eleccion20_df %>% filter(ID_PAIS==32 & CANDIDATURA=="PRESIDENTE")
eleccion20_df <- eleccion20_df %>% 
  group_by(ID_PAIS, PAIS, ID_DEPARTAMENTO, DEPARTAMENTO, MUNICIPIO) %>%
  summarise(across(c("CREEMOS", "ADN", "MAS_IPSP", "FPV", "PAN_BOL", "LIBRE_21", "CC", "JUNTOS", 
                     "VOTO_VALIDO", "VOTO_BLANCO", "VOTO_NULO", "VOTO_EMITIDO"), sum, na.rm=TRUE)) 
eleccion20_df <- eleccion20_df %>% rename(
    id_departamento = ID_DEPARTAMENTO, 
    municipio = MUNICIPIO,
    mas20 = MAS_IPSP, 
    emitidos20 = VOTO_EMITIDO,
    nulo20 = VOTO_NULO
)
eleccion20_df <- eleccion20_df %>% mutate(derecha20 = CREEMOS + CC)
eleccion20_df <- eleccion20_df %>% mutate(derecha20_pc = 100*derecha20/emitidos20)
eleccion20_df <- eleccion20_df %>% mutate(mas20_pc = 100*(mas20+nulo20)/emitidos20)
eleccion20_df <- eleccion20_df %>% select(id_departamento, municipio, derecha20_pc, mas20_pc, emitidos20)


eleccion25_df <- read_excel("Datos OEP/EG2025_20250824_235612_7503428591324372789.xlsx")
eleccion25_df <- eleccion25_df %>% filter(CodigoPais==32 & Descripcion=="PRESIDENTE")
sum(eleccion25_df$VotoEmitido, na.rm = TRUE)
eleccion25_df <- eleccion25_df %>% 
                    group_by(CodigoPais, CodigoDepartamento, NombreDepartamento, NombreMunicipio) %>%
                    summarise(across(starts_with("voto"), sum, na.rm=TRUE)) 
eleccion25_df <- eleccion25_df %>% rename(
    id_departamento = CodigoDepartamento, 
    municipio = NombreMunicipio, 
    ap = Voto1, 
    lyp_adn = Voto2, 
    apb_sumate = Voto3, 
    libre = Voto5, 
    fp = Voto6, 
    mas_ipsp = Voto7, 
    morena = Voto8, 
    unidad = Voto9, 
    pdc = Voto10,
    emitidos25 = VotoEmitido 
  )

eleccion25_df <- eleccion25_df %>% mutate(derecha25 = (libre + unidad))
eleccion25_df <- eleccion25_df %>% mutate(derechaamplia25 = (libre + unidad + apb_sumate))
eleccion25_df <- eleccion25_df %>% mutate(popular25 = (pdc + ap + mas_ipsp + VotoNuloDirecto + VotoNuloDeclinacion))
eleccion25_df <- eleccion25_df %>% mutate(pdc25_pc = (pdc/emitidos25)*100)
eleccion25_df <- eleccion25_df %>% mutate(derecha25_pc = (derecha25/emitidos25)*100)
eleccion25_df <- eleccion25_df %>% mutate(derechaamplia25_pc = (derechaamplia25/emitidos25)*100)
eleccion25_df <- eleccion25_df %>% mutate(popular25_pc = (popular25/emitidos25)*100)
eleccion25_df <- eleccion25_df %>%
  ungroup() %>%
  select(id_departamento, municipio, pdc25_pc, derecha25_pc, derechaamplia25_pc, popular25_pc, emitidos25)
eleccion25_df <- eleccion25_df %>% select(id_departamento, municipio, pdc25_pc, derecha25_pc, derechaamplia25_pc, popular25_pc, emitidos25)

df_matched <- inner_join(eleccion20_df, eleccion25_df, by = c("id_departamento", "municipio"))
df_matched$popular_abajo <- ifelse(df_matched$popular25_pc<=df_matched$mas20_pc, 1, 0)
df_matched$derecha_abajo <- ifelse(df_matched$derecha25_pc<=df_matched$derecha20_pc, 1, 0)
df_matched$derechaamplia_abajo <- ifelse(df_matched$derechaamplia25_pc<=df_matched$derecha20_pc, 1, 0)
mean(df_matched$popular_abajo)
mean(df_matched$derecha_abajo)
mean(df_matched$derechaamplia_abajo)

sum(eleccion20_df$emitidos20, na.rm = TRUE)
sum(eleccion25_df$emitidos25, na.rm = TRUE)

#Nulos 2020: 3.5% 

plot_pdc <- ggplot(df_matched, aes(x = pdc25_pc, y = mas20_pc, size = emitidos25)) +
  geom_point(aes(color = DEPARTAMENTO == "La Paz"), alpha = 0.6) +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "blue"), 
                     name = "Departamento",
                     labels = c("Otros", "La Paz")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_text_repel(data = df_matched %>% filter(emitidos25 > 150000),
                  aes(label = municipio), 
                  size = 3,
                  max.overlaps = Inf,
                  box.padding = 0.3) +
  scale_size_continuous(name = "Votos emitidos\n2025", 
                        range = c(1, 10)) +
  labs(
    title = "Relación entre voto popular 2025 y MAS 2020 por municipio",
    x = "PDC + MAS + AP + NULO 2025 (%)",
    y = "MAS 2020 (%)",
    caption = "Notas: Tamaño de burbuja = votos emitidos 2025."
  ) +
  theme_minimal() +
  theme(legend.position = "right")
print(plot_pdc)

plot_popular <- ggplot(df_matched, aes(x = popular25_pc, y = mas20_pc, size = emitidos25)) +
  geom_point(aes(color = DEPARTAMENTO == "La Paz"), alpha = 0.6) +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "blue"), 
                     name = "Departamento",
                     labels = c("Otros", "La Paz")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_text_repel(data = df_matched %>% filter(emitidos25 > 150000),
                  aes(label = municipio), 
                  size = 3,
                  max.overlaps = Inf,
                  box.padding = 0.3) +
  scale_size_continuous(name = "Votos emitidos\n2025", 
                        range = c(1, 10)) +
  labs(
    title = "Relación entre voto MAS 2020 y popular 2025 por municipio",
    x = "PDC + MAS + AP + NULO 2025 (%)",
    y = "MAS + Nulo 2020 (%)",
    caption = "Notas: Tamaño de burbuja = votos emitidos 2025.\n% = porcentaje de votos emitidos."
  ) +
  theme_minimal() +
  theme(legend.position = "right", plot.caption = element_text(hjust = 0.5))
print(plot_popular)
ggsave("/Users/joaquinmayorga/Downloads/popular.png", width = 4, height = 4)

plot_derecha <- ggplot(df_matched, aes(x = derecha25_pc, y = derecha20_pc, size = emitidos25)) +
  geom_point(aes(color = DEPARTAMENTO == "La Paz"), alpha = 0.6) +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "blue"), 
                     name = "Departamento",
                     labels = c("Otros", "La Paz")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_text_repel(data = df_matched %>% filter(emitidos25 > 135000),
                  aes(label = municipio), 
                  size = 3,
                  max.overlaps = Inf,
                  box.padding = 0.3) +
  scale_size_continuous(name = "Votos emitidos\n2025", 
                        range = c(1, 10)) +
  labs(
    title = "Relación entre voto derecha 2020 y 2025 por municipio",
    x = "Libre + Unidad 2025 (%)",
    y = "Creemos + CC 2020 (%)",
    caption = "Nota: Tamaño de burbuja = votos emitidos 2025.\n % = porcentaje de votos emitidos."
  ) +
  theme_minimal() +
  theme(legend.position = "right", plot.caption = element_text(hjust = 0.5))
print(plot_derecha)

plot_derechaamplia <- ggplot(df_matched, aes(x = derechaamplia25_pc, y = derecha20_pc, size = emitidos25)) +
  geom_point(aes(color = DEPARTAMENTO == "La Paz"), alpha = 0.6) +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "blue"), 
                     name = "Departamento",
                     labels = c("Otros", "Cochabamba")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_text_repel(data = df_matched %>% filter(emitidos25 > 135000),
                  aes(label = municipio), 
                  size = 3,
                  max.overlaps = Inf,
                  box.padding = 0.3) +
  scale_size_continuous(name = "Votos emitidos\n2025", 
                        range = c(1, 10)) +
  labs(
    title = "Relación entre voto derecha 2020 y derecha amplia 2025",
    x = "Libre + Unidad + APB Súmate 2025 (%)",
    y = "Creemos + CC 2020 (%)",
    caption = "Nota: Tamaño de burbuja = votos emitidos 2025.\n % = porcentaje de votos emitidos."
  ) +
  theme_minimal() +
  theme(legend.position = "right", plot.caption = element_text(hjust = 0.5))
print(plot_derechaamplia)


print(plot_popular)
print(plot_derecha)
print(plot_derechaamplia)

