#
# Author: Gina Jiménez
#
# ------------------------------------------------------------------------------------
# blog_endireh_2022
#
rm(list=ls())
require(pacman)
p_load(argparse, foreign, tidyverse, janitor, here, data.table)

direc <-  "/Users/georginajimenez92/Documents/inputs-datos/0922_endireh/"


##### ENDIREH 2011 ###
files <- list(lib1=paste0(direc, "endireh_2011/TDunida3.DBF"),
              lib2=paste0(direc, "endireh_2011/TUnidas3.DBF"),
              socio=paste0(direc, "endireh_2011/TSDEM.DBF"),
              endireh11=here("import/output/endireh2011.rds"))


# Importar y seleccionar
data <- read.dbf(files$base1)%>%
        clean_names()%>%
        select(id_viv:t_instrum, 
               starts_with("p13_1_"),
               p13_4:p13_7_2,
               fac_viv:est_dis)

tempo <- read.csv(files$base2)%>%
         clean_names()

data <- left_join(data, tempo)

tempo <- read.csv(files$base3)%>%
         clean_names()%>%
         mutate(hijos=ifelse(paren==3, 1, 0),
                mujer_jefa=ifelse(paren==1 & sexo==2, 1, 0),
                esposa_jefe=ifelse(paren==2 & sexo==2, 1, 0),
                elegidas=ifelse((ren_muj_el==n_ren & paren==1) | (ren_muj_el==n_ren & paren==2), 1, 0))%>%
         select(id_viv:cod_res, sexo, edad, niv,
                fac_viv:upm_dis, hijos:elegidas)

prueba <- group_by(tempo, id_viv, hogar, fac_viv, upm,
                   cve_ent, cve_mun)%>%
         summarize(mujer_jefa=sum(mujer_jefa, na.rm=T),
                   esposa_jefe=sum(esposa_jefe, na.rm=T),
                   elegidas=sum(elegidas, na.rm=T))%>%
         ungroup()%>%
         mutate(hogar_les=ifelse(mujer_jefa==1 & esposa_jefe==1 & elegidas, 1, 0))
        

data <- left_join(data, prueba)

saveRDS(data, files$endireh)
         
# done.


