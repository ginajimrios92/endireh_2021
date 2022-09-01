#!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Oscar Elton
# Maintainer(s): Oscar Elton, Mariana Solano, Gina Jimenez, Adrián Lara, Alicia Franco
# License: (c) Data Cívica 2020, GPL v2 or newer
#
# ------------------------------------------------------------------------------------
# informe-claves/import/src/import-nommun.R

require(pacman)
p_load(argparse, tidyverse, readxl, janitor, here)

files <- list(input = here("import/input/Nombres_Municipios.xlsx"),
              output = here("import/output/nom-mun.rds"))

getdata <- function(){
  nommun <- read_excel(files$input) %>%
    clean_names() %>% 
    mutate(inegi = paste0(cve_ent, cve_mun)) %>% 
    select(inegi, cve_ent, nom_ent, cve_mun, nom_mun)
}

main <- function(){
  nommun <- getdata()
  saveRDS(nommun, files$output)
}

main()

# done.