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

#### Gráficas de libertad ####
solo_ella <- function(x){
  x <- ifelse(x=="Sólo la entrevistada", "Sólo la entrevistada", "Otro")
}


endireh16 <- readRDS(files$endireh16)
endireh21 <- readRDS(files$endireh21)

data <- bind_rows(endireh16, endireh21)

tempo <- data%>%
         mutate(help=substr(t_instrum, 0, 1))%>%
         filter(help!="C")%>%
         select(-help)%>%
         select(fac_muj, si_trabaja:vida_social, ropausa, year, grupo_edad)%>%
         mutate_at(vars(si_trabaja:ropausa), solo_ella)
         
vars <- names(tempo)[2:7]

decisiones <- data.frame()


for (i in 1:length(vars)) {
tempo2 <- group_by(tempo, year, tempo[vars[1]], grupo_edad)%>%
        summarize(total=sum(fac_muj, na.rm=T))
tempo2 <- group_by(tempo2, year, grupo_edad)%>%
          mutate(den=sum(total),
                 per=total/den*100)
tempo2 <- tempo2[c(7:12, 20:25),]
tempo2$decision <- vars[i]
decisiones <- bind_rows(decisiones, tempo2)
}
rm(tempo, tempo2)

decisiones%>%
mutate(decision=gsub("si_trabaja", "Si trabaja o estudia", decision),
       decision=gsub("si_salir", "Si puede salir de su casa", decision),
       decision=gsub("quehacer_dinero", "Qué puede hacer con el \n dinero del que dispone", decision),
       decision=gsub("comprar_cosas", "Si comprar cosas para ella", decision),
       decision=gsub("vida_social", "Si participa en la vida social", decision),
       decision=gsub("ropausa", "El tipo de ropa que usa", decision))%>%
ggplot()+
geom_bar(aes(x=reorder(decision, per), y=per, fill=as.factor(year)),
         stat="identity", position="dodge")+
coord_flip()+
scale_fill_manual(values=pal)+
labs(title="Porcentaje de mujeres que respondieron que deciden solas...",
     y="", x="", caption=caption2, fill="Año de la encuesta")+
  tema
save("libertad-personal")

tempo <- data%>%
         mutate(help=substr(t_instrum, 0, 1))%>%
         filter(help!="C")%>%
         select(-help)%>%
         select(fac_muj, dinero_el, permisos_hijes:cuantos_hijos, year)
        
vars <- names(tempo)[2:9]

decisiones <- data.frame()
for (i in 1:length(vars)) {
  tempo2 <- group_by(tempo, year, tempo[vars[i]])%>%
            summarize(total=sum(fac_muj, na.rm=T))%>%
            ungroup()%>%
            group_by(year)%>%
            mutate(den=sum(total),
                   per=total/den*100)%>%
            ungroup()%>%
            select(-c(total, den))
  names(tempo2) <- c("year", "respuesta", "per")
  tempo2 <- filter(tempo2, respuesta!="Más él")%>%
            filter(respuesta!="Más ella")%>%
            filter(respuesta!="No aplica")%>%
            filter(respuesta!="Otras personas")
  tempo2$decision <- vars[i]
  decisiones <- bind_rows(decisiones, tempo2)
}

decisiones%>%
  mutate(decision=gsub("anticonceptivos", "Si usar anticonceptivos", decision),
         decision=gsub("cuando_sexo", "Cuando tener sexo", decision),
         decision=gsub("cuantos_hijos", "Cuántos hijos tener", decision),
         decision=gsub("dinero_el", "Qué hacer con el dinero \n que gana él", decision),
         decision=gsub("mudarse_casa", "Si mudarse de casa", decision),
         decision=gsub("permisos_hijes", "Si darles permisos a los hijos", decision),
         decision=gsub("quien_anti", "Quien debe usar anticonceptivos", decision),
         decision=gsub("tener_hijos", "Si tener hijos", decision),)%>%
  ggplot()+
  geom_bar(aes(x=reorder(decision, per), y=per, fill=respuesta),
           stat="identity", position="dodge")+
  coord_flip()+
  scale_fill_manual(values=pal)+
  labs(title="¿Quién decide si...?",
       y="", x="", caption=caption2, fill="")+
  tema+
  facet_wrap(~year)
save("libertad-personal-quien")


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
  labs(title="Porcentaje de mujeres entrevistadas que dijeron haber vivido violencia durante la infancia...",
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
         order=as.numeric(grupo_edad))%>%
  ggplot()+
  geom_bar(aes(x=reorder(grupo_edad, -order), y=per, fill=as.factor(year)),
           stat="identity", position="dodge")+
  tema+
  labs(title="Porcentaje de mujeres que dicen haber vivido violencia antes de los 15 años",
       subtitle="Según su edad y el tipo de violencia",
       y="", x="", fill="Año")+
  coord_flip()+
  facet_wrap(~vars)+
  scale_fill_manual(values=pal)
save("violencia-infancia-edades")


tempo <- data.frame()
for (i in 1:length(vars)) {
  minitempo <- data%>%
    group_by(data[vars[i]], year, escol)%>%
    summarize(total=sum(fac_muj))%>%
    ungroup()%>%
    group_by(year, escol)%>%
    mutate(den=sum(total, na.rm=T), 
           per=total/den*100)
  minitempo <- minitempo[c(11:20),c(2,3,6)]
  minitempo$vars <- vars[i]
  tempo <- bind_rows(tempo, minitempo)
}

tempo%>%
  mutate(vars=gsub("viopsiinf", "Violencia psicológica", vars),
         vars=gsub("viofisinf", "Violencia física", vars),
         vars=gsub("viosexinf", "Violencia sexual", vars))%>%
  ggplot()+
  geom_bar(aes(x=escol, y=per, fill=as.factor(year)),
           stat="identity", position="dodge")+
  tema+
  labs(title="Porcentaje de mujeres que dicen haber vivido violencia antes de los 15 años",
       subtitle="Según su escolaridad y el tipo de violencia",
       y="", x="", fill="Año")+
  coord_flip()+
  facet_wrap(~vars)+
  scale_fill_manual(values=pal)
save("violencia-infancia-escolaridad")





