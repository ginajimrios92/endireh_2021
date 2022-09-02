#
# Author: Georgina Jimenez
# -----------------------------------------------------------
# endireh2021/output/graficas

#### Paquetería ####
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, ggplot2)
options(scipen=999)

files <- list(endireh11 = here("import-clean/output/endireh2011.rds"),
              endireh16 = here("import-clean/output/endireh2016.rds"),
              endireh21 = here("import-clean/output/endireh2021.rds"),
              pob = here("import-clean/output/pob.rds"))

#### Tema y funciones ####
source(here("descriptives/src/theme.R"))


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

data <- bind_rows(endireh16, endireh21)

tempo <- data%>%
  mutate(help=substr(t_instrum, 0, 1))%>%
  filter(help!="C")%>%
  select(-help)%>%
  select(fac_muj, manoseos:vio_din, escol, grupo_edad, year)

vars <- names(tempo)[2:7]

decisiones <- data.frame()

for (i in 1:length(vars)) {
  tempo2 <- group_by(tempo, year, tempo[vars[i]], escol)%>%
            summarize(total=sum(fac_muj, na.rm=T))
  tempo2 <- group_by(tempo2, year, escol)%>%
            mutate(den=sum(total),
                   per=total/den*100)
  tempo2 <- tempo2[c(6:10, 21:25),c(1,3,6)]
  tempo2$decision <- vars[i]
  decisiones <- bind_rows(decisiones, tempo2)
}
rm(tempo, tempo2)


