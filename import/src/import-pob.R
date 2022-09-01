#!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Adrián Lara
# Maintainer(s): Oscar Elton, Mariana Solano, Gina Jimenez, Adrián Lara, Alicia Franco
# License: (c) Data Cívica 2020, GPL v2 or newer
#
# ------------------------------------------------------------------------------------
# informe-claves/import/src/import-pob.R


if(!require(pacman))install.packages("pacman")
pacman::p_load(argparse, tidyverse, data.table, janitor, here)

# Lista de inputs/outputs

files <- list(input_estatal = here("import/input/pob_mit_proyecciones.csv"),
              output_estatal = here("import/output/pob-estatal-0050.rds"),
              input_municipal = here("import/input/pobmun-sex-2000-2030.csv"),
              output_municipal = here("import/output/pob-municipal-0030.rds")
              )

# Función para abrir y guardar base de población estatal
getdataest <- function(){
  df_pobestatal <- fread(files$input_estatal, encoding = "UTF-8", stringsAsFactors = F) %>% 
    mutate(cve_geo = str_pad(cve_geo, width = 2, side = "left", pad = "0")) %>% 
    filter(ano >= 2000)
}
 
# Función para abrir y guardar base de población municipal
getdatamun <- function(){
  df_pobmunicipal <- fread(files$input_municipal, encoding = "UTF-8", stringsAsFactors = F) %>% 
    mutate(inegi = str_pad(clave, width = 5, side = "left", pad = "0")) %>% 
    select(-clave) 
}

main <- function(){
  est <- getdataest()
  saveRDS(est, files$output_estatal)
  mun <- getdatamun()
  saveRDS(mun, files$output_municipal)
}

main()

# done.










