#
# Author: Gina Jiménez
# ------------------------------------------------------------------------------------
# blog_endireh_2022
#
rm(list=ls())
require(pacman)
p_load(argparse, foreign, tidyverse, janitor, here, data.table)

direc <-  "/Users/georginajimenez92/Documents/inputs-datos/0922_endireh/"

##### ENDIREH 2011 ####
files <- list(lib1=paste0(direc, "endireh_2011/TUnidas3.dbf"),
              socio=paste0(direc, "endireh_2011/TSDEM.DBF"),
              endireh11=here("import-clean/output/endireh2011.rds"))

### Las decisiones sólo se las preguntan a las mujeres casadas en 2011

#Funciones para 2011
personas <- function(x){
  x <- case_when(x==1 ~ "Sólo la entrevistada",
                 x==2 ~ "Sólo el esposo o pareja",
                 x==3 ~ "Ambos",
                 x==4 ~ "Otras personas")
}

# Importar y seleccionar
data <- read.dbf(files$lib1)%>%
  clean_names()%>%
  select(control:r_sel_m, 
         starts_with("ap7_1_"),
         fac_per:upm_dis)

tempo <- read.dbf(files$socio)%>%
  clean_names()%>%
  rename(r_sel_m=n_ren)

data <- left_join(data, tempo)


data <- data%>%
  mutate(sexo=case_when(sexo==2 ~ "Mujer",
                        sexo==1 ~ "Hombre"),
         niv=as.numeric(as.character(niv)),
         gra=as.numeric(as.character(gra)),
         escol=case_when(niv<2 ~ 1,
                         niv==2 & gra<6 ~ 1,
                         niv==2 & gra>=6 ~ 2,
                         niv==3 & gra<3 ~ 2,
                         niv==3 & gra>=3 ~ 3,
                         niv==4 ~ 3,
                         niv==5 & gra<4 ~ 3,
                         niv==5 & gra>=4 ~ 4,
                         niv==6 ~ 4,
                         niv==7 ~ 4,
                         niv>7 & niv<10 ~ 5),
         escol=factor(escol, levels=c(1:5), labels=c("Sin escolaridad",
                                                     "Primaria",
                                                     "Secundaria \n o equivalente",
                                                     "Preparatoria \n o equivalente",
                                                     "Licenciatura \n o más")),
         edad=as.numeric(as.character(edad)))%>%
  mutate_at(vars(ap7_1_1:ap7_1_13), personas)

data$edad[data$edad>97] <- NA

saveRDS(data, files$endireh11)


##### ENDIREH 2016 ####
files <- list(lib=paste0(direc, "endireh_2016/conjunto_de_datos_tb_sec_xiv_endireh_2016.csv"),
              fam_ori=paste0(direc, "endireh_2016/conjunto_de_datos_tb_sec_xi_endireh_2016.csv"),
              socio=paste0(direc, "endireh_2016/conjunto_de_datos_tsdem_endireh_2016.csv"),
              pareja=paste0(direc, "endireh_2016/conjunto_de_datos_tb_sec_xiii_endireh_2016.csv"),
              endireh16=here("import-clean/output/endireh2016.rds"))

#### Funciones para 2016 y 2021
personas <- function(x){
  x <- case_when(x==1 ~ "Sólo la entrevistada",
                 x==2 ~ "Sólo el esposo o pareja",
                 x==3 ~ "Más él",
                 x==4 ~ "Más ella",
                 x==5 ~ "Ambos",
                 x==6 ~ "Otras personas",
                 x==7 ~ "No aplica")
}

sino <- function(x){
  x <- case_when(x==1 ~ "Sí",
                 x==2 ~ "No")
}

change <- function(x) {
  x <- ifelse(x<4, 1, 0)
}

suma <- function(x) {
  x <- ifelse(x>0, 1, 0)
}


#Limpiamos
data <- read.csv(files$lib)%>%
        clean_names()%>%
        select(id_viv:t_instrum, 
        starts_with("p14_1ab"),
          fac_viv:est_dis)
         
tempo <- read.csv(files$socio)%>%
         clean_names()%>%
         select(id_viv:cod_res, sexo, edad, niv, gra,
                fac_viv:upm_dis)

data <- left_join(data, tempo)

tempo <- read.csv(files$fam_ori)%>%
         clean_names()%>%
         select(id_viv:t_instrum,
                     starts_with("p11_12_"), p11_6, p11_7)

data <- left_join(data, tempo)

tempo <- read.csv(files$pareja)%>%
         clean_names()%>%
         select(id_viv:t_instrum,
                starts_with("p13_1_"),
                p13_4, p13_7_1, p13_7_2,
                fac_viv:est_dis)

data <- left_join(data, tempo)
rm(tempo)

data <- mutate_at(data, vars(p14_1ab_1:p14_1ab_15), personas)%>%
        mutate_at(vars(edad, niv), as.character)%>%
        mutate_at(vars(edad, niv), as.numeric)%>%
        mutate_at(vars(starts_with("p11_12_")), sino)%>%
        mutate(escol=case_when(niv<2 ~ 1,
                               niv==2 & gra<6 ~ 1,
                               niv==2 & gra>=6 ~ 2,
                               niv==3 & gra<3 ~ 2,
                               niv==3 & gra>=3 ~ 3,
                               niv==4 & gra<3 ~ 4,
                               niv==4 & gra>=3 ~ 4,
                               niv==5 ~ 2,
                               niv==6 ~ 3,
                               niv==7 ~ 4,
                               niv==8 ~ 3,
                               niv>8 ~ 5),
                p11_6=ifelse(p11_6<3, 1, 2),
                p11_7=ifelse(p11_7<3, 1, 2),
         escol=factor(escol, levels=c(1:5), 
                      labels=c("Sin escolaridad",
                               "Primaria",
                               "Secundaria \n o equivalente",
                               "Preparatoria \n o equivalente",
                               "Licenciatura \n o más")),
         grupo_edad = case_when(edad %in% 15:25 ~ 1,
                                edad %in% 26:35 ~ 2,
                                edad %in% 36:45 ~ 3,
                                edad %in% 46:55 ~ 4,
                                edad %in% 56:65 ~ 5,
                                edad > 65 ~ 6),
         grupo_edad = factor(grupo_edad, levels=c(1:6),
                             labels=c("15 a 25 años",
                                      "26 a 35 años",
                                      "36 a 45 años",
                                      "46 a 55 años",
                                      "56 a 65 años",
                                      "Más de 65 años")),
         veinteomas = ifelse(edad>19, 1, 0), 
         year=2016)%>%
         mutate_at(vars(p11_6, p11_7, p13_4:p13_7_2), sino)%>%
         mutate_at(vars(starts_with("p13_1_")), change)%>%
         mutate(vio_fis=rowSums(.[,49:57], na.rm=T),
                vio_psi=rowSums(.[,58:72], na.rm=T),
                vio_sex=rowSums(.[,73:77], na.rm=T),
                vio_eco=rowSums(.[,78:84], na.rm=T))%>%
         mutate_at(vars(starts_with("vio")), suma)%>%
         mutate_at(vars(starts_with("vio")), sino)%>%
         select(-c(starts_with("p13_1_")))%>%
         rename(si_trabaja=p14_1ab_1,
                     si_salir=p14_1ab_2,
                     quehacer_dinero=p14_1ab_3,
                     comprar_cosas=p14_1ab_4,
                     vida_social=p14_1ab_5,
                     como_gasta=p14_1ab_6,
                     dinero_el=p14_1ab_7,
                     ropausa=p14_1ab_8,
                     permisos_hijes=p14_1ab_9,
                     mudarse_casa=p14_1ab_10,
                     cuando_sexo=p14_1ab_11,
                     anticonceptivos=p14_1ab_12,
                     quien_anti=p14_1ab_13,
                     tener_hijos=p14_1ab_14,
                     cuantos_hijos=p14_1ab_15,
                     manoseos=p11_12_1,
                     mostrar_gen=p11_12_2,
                     ver_porno=p11_12_3,
                     int_vio=p11_12_4,
                     viola=p11_12_5,
                     vio_din=p11_12_6,
                     viofisinf=p11_6,
                     viopsiinf=p11_7, 
                     pidioayuda=p13_7_1,
                     denuncio=p13_7_2,
                     habloalg=p13_4)
saveRDS(data, files$endireh16)


##### ENDIREH 2021 ####
files <- list(lib=paste0(direc, "endireh_2021/TB_SEC_XV.csv"),
              fam_ori=paste0(direc, "endireh_2021/TB_SEC_XII.csv"),
              socio=paste0(direc, "endireh_2021/TSDem.csv"),
              pareja=paste0(direc, "endireh_2021/TB_SEC_XIV.csv"),
              endireh21=here("import-clean/output/endireh2021.rds"))


data <- read.csv(files$lib)%>%
        clean_names()%>%
        select(id_viv:t_instrum, 
               starts_with("p15_1ab"),
              fac_viv:est_dis)

tempo <- read.csv(files$socio)%>%
         clean_names()%>%
         select(id_viv:n_ren, sexo, edad, niv, gra,
         fac_viv:upm_dis)

data <- left_join(data, tempo)

tempo <- read.csv(files$fam_ori)%>%
         clean_names()%>%
         select(id_viv:t_instrum,
                starts_with("p12_14_"), p12_6, p12_7)

data <- left_join(data, tempo)

tempo <- read.csv(files$pareja)%>%
         clean_names()%>%
         select(id_viv:t_instrum,
         starts_with("p14_1_"),
         p14_4, p14_7_1, p14_7_2,
         fac_viv:est_dis)

data <- left_join(data, tempo)
rm(tempo)


data <- mutate_at(data, vars(p15_1ab_1:p15_1ab_17), personas)%>%
        mutate_at(vars(edad, niv), as.character)%>%
        mutate_at(vars(edad, niv), as.numeric)%>%
        mutate_at(vars(starts_with("p12_14_")), sino)%>%
        mutate(escol=case_when(niv<2 ~ 1,
                               niv==2 & gra<6 ~ 1,
                               niv==2 & gra>=6 ~ 2,
                               niv==3 & gra<3 ~ 2,
                               niv==3 & gra>=3 ~ 3,
                               niv==4 & gra<3 ~ 4,
                               niv==4 & gra>=3 ~ 4,
                               niv==5 ~ 2,
                               niv==6 ~ 3,
                               niv==7 ~ 4,
                               niv==8 ~ 3,
                               niv>8 ~ 5),
         escol=factor(escol, levels=c(1:5), 
                      labels=c("Sin escolaridad",
                               "Primaria",
                               "Secundaria \n o equivalente",
                               "Preparatoria \n o equivalente",
                               "Licenciatura \n o más")),
         grupo_edad = case_when(edad %in% 15:25 ~ 1,
                                edad %in% 26:35 ~ 2,
                                edad %in% 36:45 ~ 3,
                                edad %in% 46:55 ~ 4,
                                edad %in% 56:65 ~ 5,
                                edad > 65 ~ 6),
         grupo_edad = factor(grupo_edad, levels=c(1:6),
                             labels=c("15 a 25 años",
                                      "26 a 35 años",
                                      "36 a 45 años",
                                      "46 a 55 años",
                                      "56 a 65 años",
                                      "Más de 65 años")),
         veinteomas = ifelse(edad>19, 1, 0),
         p12_6=ifelse(p12_6<3, 1, 2),
         p12_7=ifelse(p12_7<3, 1, 2),
         year=2021)%>%
         mutate_at(vars(p12_6, p12_7, p14_4:p14_7_2), sino)%>%
         mutate_at(vars(starts_with("p14_1_")), change)%>%
         mutate(vio_fis=rowSums(.[,47:55], na.rm=T),
                vio_psi=rowSums(.[,56:70], na.rm=T),
                vio_sex=rowSums(.[,71:77], na.rm=T),
                vio_eco=rowSums(.[,78:84], na.rm=T))%>%
  mutate_at(vars(starts_with("vio")), suma)%>%
  mutate_at(vars(starts_with("vio")), sino)%>%
  select(-c(starts_with("p14_1_")))%>%
  rename(si_trabaja=p15_1ab_1,
         si_salir=p15_1ab_2,
         quehacer_dinero=p15_1ab_3,
         comprar_cosas=p15_1ab_4,
         vida_social=p15_1ab_5,
               vida_politica=p15_1ab_6,
               como_gasta=p15_1ab_7,
               dinero_el=p15_1ab_8,
               ropausa=p15_1ab_9,
               permisos_hijes=p15_1ab_10,
               mudarse_casa=p15_1ab_11,
               cuando_sexo=p15_1ab_12,
               anticonceptivos=p15_1ab_13,
               cui_sexual=p15_1ab_14,
               quien_anti=p15_1ab_15,
               tener_hijos=p15_1ab_16,
               cuantos_hijos=p15_1ab_17,
               manoseos=p12_14_1,
               mostrar_gen=p12_14_2,
               ver_porno=p12_14_3,
               int_vio=p12_14_4,
               viola=p12_14_5,
               vio_din=p12_14_6,
               viofisinf=p12_6,
               viopsiinf=p12_7,
               pidioayuda=p14_7_1,
               denuncio=p14_7_2,
               habloalg=p14_4)
saveRDS(data, files$endireh21)
         
# done.


