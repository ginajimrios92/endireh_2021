#
# Author: Georgina Jimenez
# -----------------------------------------------------------
# endireh2021/output/graficas

#### Paquetería ####
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, ggplot2, ggalt)
options(scipen=999)

files <- list(endireh11 = here("import-clean/output/endireh2011.rds"),
              endireh16 = here("import-clean/output/endireh2016.rds"),
              endireh21 = here("import-clean/output/endireh2021.rds"),
              pob = here("import-clean/output/pob.rds"))

#### Tema y funciones ####
source(here("descriptives/src/theme.R"))

suma <- function(x) {
  x <- ifelse(x>0, 1, 2)
}

sino <- function(x){
  x <- case_when(x==1 ~ "Sí",
                 x==2 ~ "No")
}


#### Gráfica de familia de origen ####
endireh16 <- readRDS(files$endireh16)
endireh21 <- readRDS(files$endireh21)%>%
             filter(veinteomas==1)

data <- bind_rows(endireh16, endireh21)%>%
        mutate(viosexinf=rowSums(.[,41:46], na.rm=T),
               viosexinf=sino(suma(viosexinf)))


vars <- c("viopsiinf", "viofisinf", "viosexinf")
tempo <- data.frame()

for (i in 1:length(vars)) {
minitempo <- data%>%
             group_by(data[vars[i]], year)%>%
             summarize(total=sum(fac_muj))%>%
             ungroup()%>%
             group_by(year)%>%
             mutate(den=sum(total, na.rm=T), 
             per=total/den*100)
minitempo <- minitempo[c(3:4),c(2,5)]
minitempo <- pivot_wider(minitempo, names_from="year", values_from="per")
minitempo$vars <- vars[i]
names(minitempo) <- c("y2016", "y2021", "vars")
tempo <- bind_rows(tempo, minitempo)
}

tempo%>%
mutate(vars=gsub("viopsiinf", "Violencia psicológica", vars),
       vars=gsub("viofisinf", "Violencia física", vars),
       vars=gsub("viosexinf", "Violencia sexual", vars))%>%
  ggplot(aes(x=y2016, xend=y2021, y=vars))+
  geom_dumbbell(color=pal[1], size=3, colour_x = pal[2],
                colour_xend = pal[3])+
  tema+
  labs(title="Porcentaje de mujeres entrevistadas que dijeron haber vivido \n violencia durante la infancia...",
       subtitle="Según el tipo de violencia",
       x="", y="", fill="",
       caption=caption2)
save("violencia-infancia")

tempo <- data.frame()
for (i in 1:length(vars)) {
minitempo <- data%>%
    group_by(data[vars[i]], year, grupo_edad)%>%
    summarize(total=sum(fac_muj))%>%
    ungroup()%>%
    group_by(year, grupo_edad)%>%
    mutate(den=sum(total, na.rm=T), 
           per=total/den*100)
  minitempo <- minitempo[c(13:24),c(2,3,6)]
  minitempo$vars <- vars[i]
  tempo <- bind_rows(tempo, minitempo)
}

tempo%>%
  mutate(vars=gsub("viopsiinf", "Violencia psicológica", vars),
         vars=gsub("viofisinf", "Violencia física", vars),
         vars=gsub("viosexinf", "Violencia sexual", vars),
         order=as.integer(grupo_edad),
         grupo_edad=gsub("15 a 25 años", "25 años o menos", grupo_edad))%>%
  ggplot()+
  geom_bar(aes(x=reorder(grupo_edad, -order), y=per, fill=as.factor(year)),
           stat="identity", position="dodge")+
  tema+
  labs(title="Porcentaje de mujeres que dicen haber vivido violencia antes de los 15 años",
       subtitle="Según su edad y el tipo de violencia",
       y="", x="", fill="Año de la encuesta", caption=caption2)+
  coord_flip()+
  facet_wrap(~vars)+
  scale_fill_manual(values=pal)
save("violencia-infancia-edades")


#### Gráfica sobre pedir ayuda ####
endireh16 <- readRDS(files$endireh16)
endireh21 <- readRDS(files$endireh21)

data <- bind_rows(endireh16, endireh21)

data%>%
group_by(year,grupo_edad, habloalg)%>%
summarize(total=sum(fac_muj, na.rm=T))%>%
ungroup()%>%
filter(!is.na(habloalg))%>%
filter(!is.na(grupo_edad))%>%
group_by(year, grupo_edad)%>%
mutate(den=sum(total, na.rm=T), 
       per=total/den*100)%>%
ungroup()%>%
filter(habloalg=="Sí")%>%
mutate(order=as.numeric(grupo_edad),
       grupo_edad=gsub("15 a 25 años", "25 años o menos", grupo_edad))%>%
ggplot()+
geom_bar(aes(x=reorder(grupo_edad, -order), y=per, fill=as.factor(year)),
           stat="identity", position="dodge")+
  tema+
  labs(title="Porcentaje de mujeres que dicen haber hablado con \n con alguien de la violencia en pareja",
       subtitle="Según su edad",
       y="", x="", fill="Año de la encuesta", caption=caption2)+
  coord_flip()+
  scale_fill_manual(values=pal)
save("hablar-alguien-edad")

