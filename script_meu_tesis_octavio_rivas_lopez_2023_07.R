# El Colegio de Mexico A.C.
# Centro de Estudios Demograficos, Urbanos y Ambientales
# Maestria en Estudios Urbanos 2021-2023
# Tesis: El vecindario como diferenciador en el acceso de los jovenes al mercado 
# de trabajo informal de la Zona Metropolitana del Valle de Mexico en 2020
# Autor: Octavio Rivas Lopez
# Directora de tesis: Dra. Landy Lizbeth Sanchez Peña
# ------------------------------------------------------------------------------
  # Directorio de trabajo
setwd('C://....')
  # Librerias
library(tidyverse)
library(survey)
library(Matrix)
library(margins)
library(mfx)
library(mice)
library(aod)
library(ResourceSelection)
library(pROC)
options(survey.lonely.psu='adjust')
  # Tratamiento primario de datos 2005
    # Llave vivienda
viv05 <- read.csv('vivt105.csv', header=T, sep=',', fileEncoding='latin1')
viv05$cd_a <- as.character(viv05$cd_a)
viv05$cd_a <- str_pad(viv05$cd_a, width=2, pad='0')
viv05$ent <- as.character(viv05$ent)
viv05$ent <- str_pad(viv05$ent, width=2, pad='0')
viv05$con <- as.character(viv05$con)
viv05$v_sel <- as.character(viv05$v_sel)
viv05$v_sel <- str_pad(viv05$v_sel, width=2, pad='0')
viv05$llave <- paste0(viv05$cd_a, viv05$ent, viv05$con, viv05$v_sel)
    # Llave hogares
hog05 <- read.csv('hogt105.csv', header=T, sep=',', fileEncoding='latin1')
hog05$cd_a <- as.character(hog05$cd_a)
hog05$cd_a <- str_pad(hog05$cd_a, width=2, pad='0')
hog05$ent <- as.character(hog05$ent)
hog05$ent <- str_pad(hog05$ent, width=2, pad='0')
hog05$con <- as.character(hog05$con)
hog05$v_sel <- as.character(hog05$v_sel)
hog05$v_sel <- str_pad(hog05$v_sel, width=2, pad='0')
hog05$n_hog <- as.character(hog05$n_hog)
hog05$h_mud <- as.character(hog05$h_mud)
hog05$llave <- paste0(hog05$cd_a, hog05$ent, hog05$con, hog05$v_sel, 
                      hog05$n_hog, hog05$h_mud)
    # Llave sociodemograficos
dem05 <- read.csv('sdemt105.csv', header=T, sep=',', fileEncoding='latin1')
dem05$cd_a <- as.character(dem05$cd_a)
dem05$cd_a <- str_pad(dem05$cd_a, width=2, pad='0')
dem05$ent <- as.character(dem05$ent)
dem05$ent <- str_pad(dem05$ent, width=2, pad='0')
dem05$con <- as.character(dem05$con)
dem05$v_sel <- as.character(dem05$v_sel)
dem05$v_sel <- str_pad(dem05$v_sel, width=2, pad='0')
dem05$n_hog <- as.character(dem05$n_hog)
dem05$h_mud <- as.character(dem05$h_mud)
dem05$n_ren <- as.character(dem05$n_ren)
dem05$n_ren <- str_pad(dem05$n_ren, width=2, pad='0')
dem05$llave <- paste0(dem05$cd_a, dem05$ent, dem05$con, dem05$v_sel, 
                      dem05$n_hog, dem05$h_mud, dem05$n_ren)
    # Llave COE1
coe105 <- read.csv('coe1t105.csv', header=T, sep=',', fileEncoding='latin1')
coe105$cd_a <- as.character(coe105$cd_a)
coe105$cd_a <- str_pad(coe105$cd_a, width=2, pad='0')
coe105$ent <- as.character(coe105$ent)
coe105$ent <- str_pad(coe105$ent, width=2, pad='0')
coe105$con <- as.character(coe105$con)
coe105$v_sel <- as.character(coe105$v_sel)
coe105$v_sel <- str_pad(coe105$v_sel, width=2, pad='0')
coe105$n_hog <- as.character(coe105$n_hog)
coe105$h_mud <- as.character(coe105$h_mud)
coe105$n_ren <- as.character(coe105$n_ren)
coe105$n_ren <- str_pad(coe105$n_ren, width=2, pad='0')
coe105$llave <- paste0(coe105$cd_a, coe105$ent, coe105$con, coe105$v_sel, 
                       coe105$n_hog, coe105$h_mud, coe105$n_ren)
    # Llave COE2
coe205 <- read.csv('coe2t105.csv', header=T, sep=',', fileEncoding='latin1')
coe205$cd_a <- as.character(coe205$cd_a)
coe205$cd_a <- str_pad(coe205$cd_a, width=2, pad='0')
coe205$ent <- as.character(coe205$ent)
coe205$ent <- str_pad(coe205$ent, width=2, pad='0')
coe205$con <- as.character(coe205$con)
coe205$v_sel <- as.character(coe205$v_sel)
coe205$v_sel <- str_pad(coe205$v_sel, width=2, pad='0')
coe205$n_hog <- as.character(coe205$n_hog)
coe205$h_mud <- as.character(coe205$h_mud)
coe205$n_ren <- as.character(coe205$n_ren)
coe205$n_ren <- str_pad(coe205$n_ren, width=2, pad='0')
coe205$llave <- paste0(coe205$cd_a, coe205$ent, coe205$con, coe205$v_sel, 
                       coe205$n_hog, coe205$h_mud, coe205$n_ren)
    # Total de hogares y viviendas y validacion de llaves unicas del COE
length(unique(viv05$llave))
length(unique(hog05$llave))
length(unique(dem05$llave))
length(unique(coe105$llave))
length(unique(coe205$llave))
    # Union entre COE y atributos sociodemograficos
coe05 <- merge(coe105, coe205, by='llave')
dem_coe05 <- merge(dem05, coe05, by='llave')
head(dem_coe05)
length(unique(dem_coe05$llave))
  # Tratamiento primario de datos 2020
    # Llave vivienda
viv20 <- read.csv('ENOEN_VIVT120.csv', header=T, sep=',', fileEncoding='latin1')
viv20$cd_a <- as.character(viv20$cd_a)
viv20$cd_a <- str_pad(viv20$cd_a, width=2, pad='0')
viv20$ent <- as.character(viv20$ent)
viv20$ent <- str_pad(viv20$ent, width=2, pad='0')
viv20$con <- as.character(viv20$con)
viv20$v_sel <- as.character(viv20$v_sel)
viv20$v_sel <- str_pad(viv20$v_sel, width=2, pad='0')
viv20$llave <- paste0(viv20$cd_a, viv20$ent, viv20$con, viv20$v_sel)
    # Llave hogares
hog20 <- read.csv('ENOEN_HOGT120.csv', header=T, sep=',', fileEncoding='latin1')
hog20$cd_a <- as.character(hog20$cd_a)
hog20$cd_a <- str_pad(hog20$cd_a, width=2, pad='0')
hog20$ent <- as.character(hog20$ent)
hog20$ent <- str_pad(hog20$ent, width=2, pad='0')
hog20$con <- as.character(hog20$con)
hog20$v_sel <- as.character(hog20$v_sel)
hog20$v_sel <- str_pad(hog20$v_sel, width=2, pad='0')
hog20$n_hog <- as.character(hog20$n_hog)
hog20$h_mud <- as.character(hog20$h_mud)
hog20$llave <- paste0(hog20$cd_a, hog20$ent, hog20$con, hog20$v_sel, 
                      hog20$n_hog, hog20$h_mud)
    # Llave sociodemograficos
dem20 <- read.csv('ENOEN_SDEMT120.csv', header=T, sep=',', 
                   fileEncoding='latin1')
dem20$cd_a <- as.character(dem20$cd_a)
dem20$cd_a <- str_pad(dem20$cd_a, width=2, pad='0')
dem20$ent <- as.character(dem20$ent)
dem20$ent <- str_pad(dem20$ent, width=2, pad='0')
dem20$con <- as.character(dem20$con)
dem20$v_sel <- as.character(dem20$v_sel)
dem20$v_sel <- str_pad(dem20$v_sel, width=2, pad='0')
dem20$n_hog <- as.character(dem20$n_hog)
dem20$h_mud <- as.character(dem20$h_mud)
dem20$n_ren <- as.character(dem20$n_ren)
dem20$n_ren <- str_pad(dem20$n_ren, width=2, pad='0')
dem20$llave <- paste0(dem20$cd_a, dem20$ent, dem20$con, dem20$v_sel, 
                      dem20$n_hog, dem20$h_mud, dem20$n_ren)
    # Llave COE1
coe120 <- read.csv('ENOEN_COE1T120.csv', header=T, sep=',', 
                    fileEncoding='latin1')
coe120$cd_a <- as.character(coe120$cd_a)
coe120$cd_a <- str_pad(coe120$cd_a, width=2, pad='0')
coe120$ent <- as.character(coe120$ent)
coe120$ent <- str_pad(coe120$ent, width=2, pad='0')
coe120$con <- as.character(coe120$con)
coe120$v_sel <- as.character(coe120$v_sel)
coe120$v_sel <- str_pad(coe120$v_sel, width=2, pad='0')
coe120$n_hog <- as.character(coe120$n_hog)
coe120$h_mud <- as.character(coe120$h_mud)
coe120$n_ren <- as.character(coe120$n_ren)
coe120$n_ren <- str_pad(coe120$n_ren, width=2, pad='0')
coe120$llave <- paste0(coe120$cd_a, coe120$ent, coe120$con, coe120$v_sel, 
                       coe120$n_hog, coe120$h_mud, coe120$n_ren)
    # Llave COE2
coe220 <- read.csv('ENOEN_COE2T120.csv', header=T, sep=',', 
                   fileEncoding='latin1')
coe220$cd_a <- as.character(coe220$cd_a)
coe220$cd_a <- str_pad(coe220$cd_a, width=2, pad='0')
coe220$ent <- as.character(coe220$ent)
coe220$ent <- str_pad(coe220$ent, width=2, pad='0')
coe220$con <- as.character(coe220$con)
coe220$v_sel <- as.character(coe220$v_sel)
coe220$v_sel <- str_pad(coe220$v_sel, width=2, pad='0')
coe220$n_hog <- as.character(coe220$n_hog)
coe220$h_mud <- as.character(coe220$h_mud)
coe220$n_ren <- as.character(coe220$n_ren)
coe220$n_ren <- str_pad(coe220$n_ren, width=2, pad='0')
coe220$llave <- paste0(coe220$cd_a, coe220$ent, coe220$con, coe220$v_sel, 
                       coe220$n_hog, coe220$h_mud, coe220$n_ren)
    # Total de hogares y viviendas y validacion de llaves unicas del COE
length(unique(viv20$llave))
length(unique(hog20$llave))
length(unique(dem20$llave))
length(unique(coe120$llave))
length(unique(coe220$llave))
    # Union entre COE y atributos sociodemograficos
coe20 <- merge(coe120, coe220, by='llave')
dem_coe20 <- merge(dem20, coe20, by='llave')
head(dem_coe20)
length(unique(dem_coe20$llave))
# ------------------------------------------------------------------------------
  # Validacion de datos mediante reconstruccion de variables de la ENOE
    # Calculos a escala nacional urbana/no rural 2005
cpea05_tot.nal <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                         (eda>=15&eda<=98), clase1==1, t_loc!=4) 
pea05_tot.nal <- sum(cpea05_tot.nal$fac) 
      # PEA nacional urbana/no rural (34,576,961 [INEGI, 1T-2005])
cpot05_tot.nal <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                         (eda>=15&eda<=98), clase2==1, t_loc!=4)
pot05_tot.nal <- sum(cpot05_tot.nal$fac) 
      # POT nacional urbana/no rural (33,100,296 [INEGI, 1T-2005])
tdes05_tot.nal <- 1-(pot05_tot.nal/pea05_tot.nal) 
      # Tasa de desempleo nacional urbana/no rural
cpotm05_tot.nal <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                          (eda>=15&eda<=98), clase2==1, t_loc!=4, sex==2)
potm05_tot.nal <- sum(cpotm05_tot.nal$fac) 
      # Total nacional (urbano/no rural) de mujeres ocupadas 
      # (12,559,326 [INEGI, 1T-2005])
cpoth05_tot.nal <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                          (eda>=15&eda<=98), clase2==1, t_loc!=4, sex==1)
poth05_tot.nal <- sum(cpoth05_tot.nal$fac) 
      # Total nacional (urbano/no rural) de hombres ocupados 
      # (20,540,970 [INEGI, 1T-2005])
    # Calculos a escala ZMVM 2005
cpea05_tot.zmvm <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                          (eda>=15&eda<=98), clase1==1, cd_a=='01') 
pea05_tot.zmvm <- sum(cpea05_tot.zmvm$fac) 
      # PEA ZMVM (8,041,962 [INEGI, 1T-2005])
cpot05_tot.zmvm <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                          (eda>=15&eda<=98), clase2==1, cd_a=='01')
pot05_tot.zmvm <- sum(cpot05_tot.zmvm$fac) 
      # POT ZMVM (7,564,221 [INEGI, 1T-2005])
tdes05_tot.zmvm <- 1-(pot05_tot.zmvm/pea05_tot.zmvm) 
      # Tasa de desempleo ZMVM 
cpotm05_tot.zmvm <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                           (eda>=15&eda<=98), clase2==1, cd_a=='01', sex==2)
potm05_tot.zmvm <- sum(cpotm05_tot.zmvm$fac) 
      # Total ZMVM de mujeres ocupadas (2,885,956 [INEGI, 1T-2005])
cpoth05_tot.zmvm <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                           (eda>=15&eda<=98), clase2==1, cd_a=='01', sex==1)
poth05_tot.zmvm <- sum(cpoth05_tot.zmvm$fac) 
      # Total ZMVM de hombres ocupados (4,678,265 [INEGI, 1T-2005])
    # Calculos a escala nacional urbana/no rural 2020
cpea20_tot.nal <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                     (eda>=15&eda<=98), clase1==1, t_loc!=4) 
pea20_tot.nal <- sum(cpea20_tot.nal$fac) 
      # PEA nacional urbana/no rural (45,500,693 [INEGI, 1T-2020])
cpot20_tot.nal <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                     (eda>=15&eda<=98), clase2==1, t_loc!=4)
pot20_tot.nal <- sum(cpot20_tot.nal$fac) 
     # POT nacional urbana/no rural (43,801,143 [INEGI, 1T-2020])
tdes20_tot.nal <- 1-(pot20_tot.nal/pea20_tot.nal) 
      # Tasa de desempleo nacional urbana/no rural
cpotm20_tot.nal <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                      (eda>=15&eda<=98), clase2==1, t_loc!=4, sex==2)
potm20_tot.nal <- sum(cpotm20_tot.nal$fac) 
      # Total nacional (urbano/no rural) de mujeres ocupadas 
      # (18,173,321 [INEGI, 1T-2020])
cpoth20_tot.nal <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                      (eda>=15&eda<=98), clase2==1, t_loc!=4, sex==1)
poth20_tot.nal <- sum(cpoth20_tot.nal$fac) 
      # Total nacional (urbano/no rural) de hombres ocupados 
      # (25,627,822 [INEGI, 1T-2020])
    # Calculos a escala ZMVM 2020
cpea20_tot.zmvm <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                          (eda>=15&eda<=98), clase1==1, cd_a=='01') 
pea20_tot.zmvm <- sum(cpea20_tot.zmvm$fac) 
      # PEA ZMVM (9,459,547 [INEGI, 1T-2020])
cpot20_tot.zmvm <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                          (eda>=15&eda<=98), clase2==1, cd_a=='01')
pot20_tot.zmvm <- sum(cpot20_tot.zmvm$fac) 
      # POT ZMVM (8,933,080 [INEGI, 1T-2020])
tdes20_tot.zmvm <- 1-(pot20_tot.zmvm/pea20_tot.zmvm) 
      # Tasa de desempleo ZMVM 
cpotm20_tot.zmvm <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                           (eda>=15&eda<=98), clase2==1, cd_a=='01', sex==2)
potm20_tot.zmvm <- sum(cpotm20_tot.zmvm$fac) 
      # Total ZMVM de mujeres ocupadas (3,811,356 [INEGI, 1T-2020])
cpoth20_tot.zmvm <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                           (eda>=15&eda<=98), clase2==1, cd_a=='01', sex==1)
poth20_tot.zmvm <- sum(cpoth20_tot.zmvm$fac) 
      # Total ZMVM de hombres ocupados (5,121,724 [INEGI, 1T-2020])
  # Nota: Los datos obtenidos por medio de la reconstruccion de variables de la
  # ENOE coinciden con los publicados en el sitio web InfoLaboral. Se concluye
  # que la union de tablas de datos es correcta
# ------------------------------------------------------------------------------
  # Indicadores basicos de empleo
    # Calculos a escala nacional urbana, total de poblacion
      # Jovenes 2005
cpt05_jov.nal <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3),
                        (eda>=15&eda<=29), t_loc!=4) 
pt05_jov.nal <- sum(cpt05_jov.nal$fac)
cpea05_jov.nal <- filter(cpt05_jov.nal, clase1==1)
pea05_jov.nal <- sum(cpea05_jov.nal$fac)
cpeaoc05_jov.nal <- filter(cpt05_jov.nal, clase2==1)
peaoc05_jov.nal <- sum(cpeaoc05_jov.nal$fac)
cpeanoc05_jov.nal <- filter(cpt05_jov.nal, clase2==2)
peanoc05_jov.nal <- sum(cpeanoc05_jov.nal$fac)
cpnea05_jov.nal <- filter(cpt05_jov.nal, clase1==2)
pnea05_jov.nal <- sum(cpnea05_jov.nal$fac)
cpnini05_jov.nal <- filter(cpt05_jov.nal, clase1==2, p2e!=3)
pnini05_jov.nal <- sum(cpnini05_jov.nal$fac)
motivonotrab05_jov.nal <- aggregate(fac~p2e, cpnini05_jov.nal, sum)
motivonotrab05_jov.nal
otromotivonotrab05_jov.nal <- aggregate(fac~p2g2, cpnini05_jov.nal, sum)
otromotivonotrab05_jov.nal
      # No jovenes 2005
cpt05_nojov.nal <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                          (eda>=30&eda<=64), t_loc!=4) 
pt05_nojov.nal <- sum(cpt05_nojov.nal$fac)
cpea05_nojov.nal <- filter(cpt05_nojov.nal, clase1==1) 
pea05_nojov.nal <- sum(cpea05_nojov.nal$fac)
cpeaoc05_nojov.nal <- filter(cpt05_nojov.nal, clase2==1)
peaoc05_nojov.nal <- sum(cpeaoc05_nojov.nal$fac)
cpeanoc05_nojov.nal <- filter(cpt05_nojov.nal, clase2==2)
peanoc05_nojov.nal <- sum(cpeanoc05_nojov.nal$fac)
cpnea05_nojov.nal <- filter(cpt05_nojov.nal, clase1==2)
pnea05_nojov.nal <- sum(cpnea05_nojov.nal$fac)
      # Jovenes 2020
cpt20_jov.nal <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3),
                        (eda>=15&eda<=29), t_loc!=4) 
pt20_jov.nal <- sum(cpt20_jov.nal$fac)
cpea20_jov.nal <- filter(cpt20_jov.nal, clase1==1)
pea20_jov.nal <- sum(cpea20_jov.nal$fac)
cpeaoc20_jov.nal <- filter(cpt20_jov.nal, clase2==1)
peaoc20_jov.nal <- sum(cpeaoc20_jov.nal$fac)
cpeanoc20_jov.nal <- filter(cpt20_jov.nal, clase2==2)
peanoc20_jov.nal <- sum(cpeanoc20_jov.nal$fac)
cpnea20_jov.nal <- filter(cpt20_jov.nal, clase1==2)
pnea20_jov.nal <- sum(cpnea20_jov.nal$fac)
cpnini20_jov.nal <- filter(cpt20_jov.nal, clase1==2, p2e!=3)
pnini20_jov.nal <- sum(cpnini20_jov.nal$fac)
motivonotrab20_jov.nal <- aggregate(fac~p2e, cpnini20_jov.nal, sum)
motivonotrab20_jov.nal
otromotivonotrab20_jov.nal <- aggregate(fac~p2g2, cpnini20_jov.nal, sum)
otromotivonotrab20_jov.nal
      # No jovenes 2020
cpt20_nojov.nal <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                          (eda>=30&eda<=64), t_loc!=4) 
pt20_nojov.nal <- sum(cpt20_nojov.nal$fac)
cpea20_nojov.nal <- filter(cpt20_nojov.nal, clase1==1) 
pea20_nojov.nal <- sum(cpea20_nojov.nal$fac)
cpeaoc20_nojov.nal <- filter(cpt20_nojov.nal, clase2==1)
peaoc20_nojov.nal <- sum(cpeaoc20_nojov.nal$fac)
cpeanoc20_nojov.nal <- filter(cpt20_nojov.nal, clase2==2)
peanoc20_nojov.nal <- sum(cpeanoc20_nojov.nal$fac)
cpnea20_nojov.nal <- filter(cpt20_nojov.nal, clase1==2)
pnea20_nojov.nal <- sum(cpnea20_nojov.nal$fac)
    # Calculos a escala nacional urbana, hombres
      # Jovenes 2005
cpt05_jvh.nal <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3),
                        (eda>=15&eda<=29), sex==1, t_loc!=4) 
pt05_jvh.nal <- sum(cpt05_jvh.nal$fac)
cpea05_jvh.nal <- filter(cpt05_jvh.nal, clase1==1)
pea05_jvh.nal <- sum(cpea05_jvh.nal$fac)
cpeaoc05_jvh.nal <- filter(cpt05_jvh.nal, clase2==1)
peaoc05_jvh.nal <- sum(cpeaoc05_jvh.nal$fac)
cpeanoc05_jvh.nal <- filter(cpt05_jvh.nal, clase2==2)
peanoc05_jvh.nal <- sum(cpeanoc05_jvh.nal$fac)
cpnea05_jvh.nal <- filter(cpt05_jvh.nal, clase1==2)
pnea05_jvh.nal <- sum(cpnea05_jvh.nal$fac)
cpnini05_jvh.nal <- filter(cpt05_jvh.nal, clase1==2, p2e!=3)
pnini05_jvh.nal <- sum(cpnini05_jvh.nal$fac)
motivonotrab05_jvh.nal <- aggregate(fac~p2e, cpnini05_jvh.nal, sum)
motivonotrab05_jvh.nal
otromotivonotrab05_jvh.nal <- aggregate(fac~p2g2, cpnini05_jvh.nal, sum)
otromotivonotrab05_jvh.nal
      # No jovenes 2005
cpt05_nojvh.nal <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                          (eda>=30&eda<=64), sex==1, t_loc!=4) 
pt05_nojvh.nal <- sum(cpt05_nojvh.nal$fac)
cpea05_nojvh.nal <- filter(cpt05_nojvh.nal, clase1==1) 
pea05_nojvh.nal <- sum(cpea05_nojvh.nal$fac)
cpeaoc05_nojvh.nal <- filter(cpt05_nojvh.nal, clase2==1)
peaoc05_nojvh.nal <- sum(cpeaoc05_nojvh.nal$fac)
cpeanoc05_nojvh.nal <- filter(cpt05_nojvh.nal, clase2==2)
peanoc05_nojvh.nal <- sum(cpeanoc05_nojvh.nal$fac)
cpnea05_nojvh.nal <- filter(cpt05_nojvh.nal, clase1==2)
pnea05_nojvh.nal <- sum(cpnea05_nojvh.nal$fac)
      # Jovenes 2020
cpt20_jvh.nal <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3),
                        (eda>=15&eda<=29), sex==1, t_loc!=4) 
pt20_jvh.nal <- sum(cpt20_jvh.nal$fac)
cpea20_jvh.nal <- filter(cpt20_jvh.nal, clase1==1)
pea20_jvh.nal <- sum(cpea20_jvh.nal$fac)
cpeaoc20_jvh.nal <- filter(cpt20_jvh.nal, clase2==1)
peaoc20_jvh.nal <- sum(cpeaoc20_jvh.nal$fac)
cpeanoc20_jvh.nal <- filter(cpt20_jvh.nal, clase2==2)
peanoc20_jvh.nal <- sum(cpeanoc20_jvh.nal$fac)
cpnea20_jvh.nal <- filter(cpt20_jvh.nal, clase1==2)
pnea20_jvh.nal <- sum(cpnea20_jvh.nal$fac)
cpnini20_jvh.nal <- filter(cpt20_jvh.nal, clase1==2, p2e!=3)
pnini20_jvh.nal <- sum(cpnini20_jvh.nal$fac)
motivonotrab20_jvh.nal <- aggregate(fac~p2e, cpnini20_jvh.nal, sum)
motivonotrab20_jvh.nal
otromotivonotrab20_jvh.nal <- aggregate(fac~p2g2, cpnini20_jvh.nal, sum)
otromotivonotrab20_jvh.nal
      # No jovenes 2020
cpt20_nojvh.nal <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                          (eda>=30&eda<=64), sex==1, t_loc!=4) 
pt20_nojvh.nal <- sum(cpt20_nojvh.nal$fac)
cpea20_nojvh.nal <- filter(cpt20_nojvh.nal, clase1==1) 
pea20_nojvh.nal <- sum(cpea20_nojvh.nal$fac)
cpeaoc20_nojvh.nal <- filter(cpt20_nojvh.nal, clase2==1)
peaoc20_nojvh.nal <- sum(cpeaoc20_nojvh.nal$fac)
cpeanoc20_nojvh.nal <- filter(cpt20_nojvh.nal, clase2==2)
peanoc20_nojvh.nal <- sum(cpeanoc20_nojvh.nal$fac)
cpnea20_nojvh.nal <- filter(cpt20_nojvh.nal, clase1==2)
pnea20_nojvh.nal <- sum(cpnea20_nojvh.nal$fac)
    # Calculos a escala nacional urbana, mujeres
      # Jovenes 2005
cpt05_jvm.nal <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3),
                        (eda>=15&eda<=29), sex==2, t_loc!=4) 
pt05_jvm.nal <- sum(cpt05_jvm.nal$fac)
cpea05_jvm.nal <- filter(cpt05_jvm.nal, clase1==1)
pea05_jvm.nal <- sum(cpea05_jvm.nal$fac)
cpeaoc05_jvm.nal <- filter(cpt05_jvm.nal, clase2==1)
peaoc05_jvm.nal <- sum(cpeaoc05_jvm.nal$fac)
cpeanoc05_jvm.nal <- filter(cpt05_jvm.nal, clase2==2)
peanoc05_jvm.nal <- sum(cpeanoc05_jvm.nal$fac)
cpnea05_jvm.nal <- filter(cpt05_jvm.nal, clase1==2)
pnea05_jvm.nal <- sum(cpnea05_jvm.nal$fac)
cpnini05_jvm.nal <- filter(cpt05_jvm.nal, clase1==2, p2e!=3)
pnini05_jvm.nal <- sum(cpnini05_jvm.nal$fac)
motivonotrab05_jvm.nal <- aggregate(fac~p2e, cpnini05_jvm.nal, sum)
motivonotrab05_jvm.nal
otromotivonotrab05_jvm.nal <- aggregate(fac~p2g2, cpnini05_jvm.nal, sum)
otromotivonotrab05_jvm.nal
      # No jovenes 2005
cpt05_nojvm.nal <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                          (eda>=30&eda<=64), sex==2, t_loc!=4) 
pt05_nojvm.nal <- sum(cpt05_nojvm.nal$fac)
cpea05_nojvm.nal <- filter(cpt05_nojvm.nal, clase1==1) 
pea05_nojvm.nal <- sum(cpea05_nojvm.nal$fac)
cpeaoc05_nojvm.nal <- filter(cpt05_nojvm.nal, clase2==1)
peaoc05_nojvm.nal <- sum(cpeaoc05_nojvm.nal$fac)
cpeanoc05_nojvm.nal <- filter(cpt05_nojvm.nal, clase2==2)
peanoc05_nojvm.nal <- sum(cpeanoc05_nojvm.nal$fac)
cpnea05_nojvm.nal <- filter(cpt05_nojvm.nal, clase1==2)
pnea05_nojvm.nal <- sum(cpnea05_nojvm.nal$fac)
      # Jovenes 2020
cpt20_jvm.nal <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3),
                        (eda>=15&eda<=29), sex==2, t_loc!=4) 
pt20_jvm.nal <- sum(cpt20_jvm.nal$fac)
cpea20_jvm.nal <- filter(cpt20_jvm.nal, clase1==1)
pea20_jvm.nal <- sum(cpea20_jvm.nal$fac)
cpeaoc20_jvm.nal <- filter(cpt20_jvm.nal, clase2==1)
peaoc20_jvm.nal <- sum(cpeaoc20_jvm.nal$fac)
cpeanoc20_jvm.nal <- filter(cpt20_jvm.nal, clase2==2)
peanoc20_jvm.nal <- sum(cpeanoc20_jvm.nal$fac)
cpnea20_jvm.nal <- filter(cpt20_jvm.nal, clase1==2)
pnea20_jvm.nal <- sum(cpnea20_jvm.nal$fac)
cpnini20_jvm.nal <- filter(cpt20_jvm.nal, clase1==2, p2e!=3)
pnini20_jvm.nal <- sum(cpnini20_jvm.nal$fac)
motivonotrab20_jvm.nal <- aggregate(fac~p2e, cpnini20_jvm.nal, sum)
motivonotrab20_jvm.nal
otromotivonotrab20_jvm.nal <- aggregate(fac~p2g2, cpnini20_jvm.nal, sum)
otromotivonotrab20_jvm.nal
      # No jovenes 2020
cpt20_nojvm.nal <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                          (eda>=30&eda<=64), sex==2, t_loc!=4) 
pt20_nojvm.nal <- sum(cpt20_nojvm.nal$fac)
cpea20_nojvm.nal <- filter(cpt20_nojvm.nal, clase1==1) 
pea20_nojvm.nal <- sum(cpea20_nojvm.nal$fac)
cpeaoc20_nojvm.nal <- filter(cpt20_nojvm.nal, clase2==1)
peaoc20_nojvm.nal <- sum(cpeaoc20_nojvm.nal$fac)
cpeanoc20_nojvm.nal <- filter(cpt20_nojvm.nal, clase2==2)
peanoc20_nojvm.nal <- sum(cpeanoc20_nojvm.nal$fac)
cpnea20_nojvm.nal <- filter(cpt20_nojvm.nal, clase1==2)
pnea20_nojvm.nal <- sum(cpnea20_nojvm.nal$fac)
    # Calculos a escala ZMVM, total de poblacion
      # Jovenes 2005
cpt05_jov.zmvm <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3),
                         (eda>=15&eda<=29), cd_a=='01') 
pt05_jov.zmvm <- sum(cpt05_jov.zmvm$fac)
cpea05_jov.zmvm <- filter(cpt05_jov.zmvm, clase1==1)
pea05_jov.zmvm <- sum(cpea05_jov.zmvm$fac)
cpeaoc05_jov.zmvm <- filter(cpt05_jov.zmvm, clase2==1)
peaoc05_jov.zmvm <- sum(cpeaoc05_jov.zmvm$fac)
cpeanoc05_jov.zmvm <- filter(cpt05_jov.zmvm, clase2==2)
peanoc05_jov.zmvm <- sum(cpeanoc05_jov.zmvm$fac)
cpnea05_jov.zmvm <- filter(cpt05_jov.zmvm, clase1==2)
pnea05_jov.zmvm <- sum(cpnea05_jov.zmvm$fac)
cpnini05_jov.zmvm <- filter(cpt05_jov.zmvm, clase1==2, p2e!=3)
pnini05_jov.zmvm <- sum(cpnini05_jov.zmvm$fac)
motivonotrab05_jov.zmvm <- aggregate(fac~p2e, cpnini05_jov.zmvm, sum)
motivonotrab05_jov.zmvm
otromotivonotrab05_jov.zmvm <- aggregate(fac~p2g2, cpnini05_jov.zmvm, sum)
otromotivonotrab05_jov.zmvm
      # No jovenes 2005
cpt05_nojov.zmvm <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                           (eda>=30&eda<=64), cd_a=='01') 
pt05_nojov.zmvm <- sum(cpt05_nojov.zmvm$fac)
cpea05_nojov.zmvm <- filter(cpt05_nojov.zmvm, clase1==1) 
pea05_nojov.zmvm <- sum(cpea05_nojov.zmvm$fac)
cpeaoc05_nojov.zmvm <- filter(cpt05_nojov.zmvm, clase2==1)
peaoc05_nojov.zmvm <- sum(cpeaoc05_nojov.zmvm$fac)
cpeanoc05_nojov.zmvm <- filter(cpt05_nojov.zmvm, clase2==2)
peanoc05_nojov.zmvm <- sum(cpeanoc05_nojov.zmvm$fac)
cpnea05_nojov.zmvm <- filter(cpt05_nojov.zmvm, clase1==2)
pnea05_nojov.zmvm <- sum(cpnea05_nojov.zmvm$fac)
      # Jovenes 2020
cpt20_jov.zmvm <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3),
                         (eda>=15&eda<=29), cd_a=='01') 
pt20_jov.zmvm <- sum(cpt20_jov.zmvm$fac)
cpea20_jov.zmvm <- filter(cpt20_jov.zmvm, clase1==1)
pea20_jov.zmvm <- sum(cpea20_jov.zmvm$fac)
cpeaoc20_jov.zmvm <- filter(cpt20_jov.zmvm, clase2==1)
peaoc20_jov.zmvm <- sum(cpeaoc20_jov.zmvm$fac)
cpeanoc20_jov.zmvm <- filter(cpt20_jov.zmvm, clase2==2)
peanoc20_jov.zmvm <- sum(cpeanoc20_jov.zmvm$fac)
cpnea20_jov.zmvm <- filter(cpt20_jov.zmvm, clase1==2)
pnea20_jov.zmvm <- sum(cpnea20_jov.zmvm$fac)
cpnini20_jov.zmvm <- filter(cpt20_jov.zmvm, clase1==2, p2e!=3)
pnini20_jov.zmvm <- sum(cpnini20_jov.zmvm$fac)
motivonotrab20_jov.zmvm <- aggregate(fac~p2e, cpnini20_jov.zmvm, sum)
motivonotrab20_jov.zmvm
otromotivonotrab20_jov.zmvm <- aggregate(fac~p2g2, cpnini20_jov.zmvm, sum)
otromotivonotrab20_jov.zmvm
      # No jovenes 2020
cpt20_nojov.zmvm <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                           (eda>=30&eda<=64), cd_a=='01') 
pt20_nojov.zmvm <- sum(cpt20_nojov.zmvm$fac)
cpea20_nojov.zmvm <- filter(cpt20_nojov.zmvm, clase1==1) 
pea20_nojov.zmvm <- sum(cpea20_nojov.zmvm$fac)
cpeaoc20_nojov.zmvm <- filter(cpt20_nojov.zmvm, clase2==1)
peaoc20_nojov.zmvm <- sum(cpeaoc20_nojov.zmvm$fac)
cpeanoc20_nojov.zmvm <- filter(cpt20_nojov.zmvm, clase2==2)
peanoc20_nojov.zmvm <- sum(cpeanoc20_nojov.zmvm$fac)
cpnea20_nojov.zmvm <- filter(cpt20_nojov.zmvm, clase1==2)
pnea20_nojov.zmvm <- sum(cpnea20_nojov.zmvm$fac)
    # Calculos a escala ZMVM, hombres
      # Jovenes 2005
cpt05_jvh.zmvm <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3),
                         (eda>=15&eda<=29), sex==1, cd_a=='01') 
pt05_jvh.zmvm <- sum(cpt05_jvh.zmvm$fac)
cpea05_jvh.zmvm <- filter(cpt05_jvh.zmvm, clase1==1)
pea05_jvh.zmvm <- sum(cpea05_jvh.zmvm$fac)
cpeaoc05_jvh.zmvm <- filter(cpt05_jvh.zmvm, clase2==1)
peaoc05_jvh.zmvm <- sum(cpeaoc05_jvh.zmvm$fac)
cpeanoc05_jvh.zmvm <- filter(cpt05_jvh.zmvm, clase2==2)
peanoc05_jvh.zmvm <- sum(cpeanoc05_jvh.zmvm$fac)
cpnea05_jvh.zmvm <- filter(cpt05_jvh.zmvm, clase1==2)
pnea05_jvh.zmvm <- sum(cpnea05_jvh.zmvm$fac)
cpnini05_jvh.zmvm <- filter(cpt05_jvh.zmvm, clase1==2, p2e!=3)
pnini05_jvh.zmvm <- sum(cpnini05_jvh.zmvm$fac)
motivonotrab05_jvh.zmvm <- aggregate(fac~p2e, cpnini05_jvh.zmvm, sum)
motivonotrab05_jvh.zmvm
otromotivonotrab05_jvh.zmvm <- aggregate(fac~p2g2, cpnini05_jvh.zmvm, sum)
otromotivonotrab05_jvh.zmvm
      # No jovenes 2005
cpt05_nojvh.zmvm <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                           (eda>=30&eda<=64), sex==1, cd_a=='01') 
pt05_nojvh.zmvm <- sum(cpt05_nojvh.zmvm$fac)
cpea05_nojvh.zmvm <- filter(cpt05_nojvh.zmvm, clase1==1) 
pea05_nojvh.zmvm <- sum(cpea05_nojvh.zmvm$fac)
cpeaoc05_nojvh.zmvm <- filter(cpt05_nojvh.zmvm, clase2==1)
peaoc05_nojvh.zmvm <- sum(cpeaoc05_nojvh.zmvm$fac)
cpeanoc05_nojvh.zmvm <- filter(cpt05_nojvh.zmvm, clase2==2)
peanoc05_nojvh.zmvm <- sum(cpeanoc05_nojvh.zmvm$fac)
cpnea05_nojvh.zmvm <- filter(cpt05_nojvh.zmvm, clase1==2)
pnea05_nojvh.zmvm <- sum(cpnea05_nojvh.zmvm$fac)
      # Jovenes 2020
cpt20_jvh.zmvm <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3),
                         (eda>=15&eda<=29), sex==1, cd_a=='01') 
pt20_jvh.zmvm <- sum(cpt20_jvh.zmvm$fac)
cpea20_jvh.zmvm <- filter(cpt20_jvh.zmvm, clase1==1)
pea20_jvh.zmvm <- sum(cpea20_jvh.zmvm$fac)
cpeaoc20_jvh.zmvm <- filter(cpt20_jvh.zmvm, clase2==1)
peaoc20_jvh.zmvm <- sum(cpeaoc20_jvh.zmvm$fac)
cpeanoc20_jvh.zmvm <- filter(cpt20_jvh.zmvm, clase2==2)
peanoc20_jvh.zmvm <- sum(cpeanoc20_jvh.zmvm$fac)
cpnea20_jvh.zmvm <- filter(cpt20_jvh.zmvm, clase1==2)
pnea20_jvh.zmvm <- sum(cpnea20_jvh.zmvm$fac)
cpnini20_jvh.zmvm <- filter(cpt20_jvh.zmvm, clase1==2, p2e!=3)
pnini20_jvh.zmvm <- sum(cpnini20_jvh.zmvm$fac)
motivonotrab20_jvh.zmvm <- aggregate(fac~p2e, cpnini20_jvh.zmvm, sum)
motivonotrab20_jvh.zmvm
otromotivonotrab20_jvh.zmvm <- aggregate(fac~p2g2, cpnini20_jvh.zmvm, sum)
otromotivonotrab20_jvh.zmvm
      # No jovenes 2020
cpt20_nojvh.zmvm <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                           (eda>=30&eda<=64), sex==1, cd_a=='01') 
pt20_nojvh.zmvm <- sum(cpt20_nojvh.zmvm$fac)
cpea20_nojvh.zmvm <- filter(cpt20_nojvh.zmvm, clase1==1) 
pea20_nojvh.zmvm <- sum(cpea20_nojvh.zmvm$fac)
cpeaoc20_nojvh.zmvm <- filter(cpt20_nojvh.zmvm, clase2==1)
peaoc20_nojvh.zmvm <- sum(cpeaoc20_nojvh.zmvm$fac)
cpeanoc20_nojvh.zmvm <- filter(cpt20_nojvh.zmvm, clase2==2)
peanoc20_nojvh.zmvm <- sum(cpeanoc20_nojvh.zmvm$fac)
cpnea20_nojvh.zmvm <- filter(cpt20_nojvh.zmvm, clase1==2)
pnea20_nojvh.zmvm <- sum(cpnea20_nojvh.zmvm$fac)
    # Calculos a escala ZMVM, mujeres
      # Jovenes 2005
cpt05_jvm.zmvm <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3),
                         (eda>=15&eda<=29), sex==2, cd_a=='01') 
pt05_jvm.zmvm <- sum(cpt05_jvm.zmvm$fac)
cpea05_jvm.zmvm <- filter(cpt05_jvm.zmvm, clase1==1)
pea05_jvm.zmvm <- sum(cpea05_jvm.zmvm$fac)
cpeaoc05_jvm.zmvm <- filter(cpt05_jvm.zmvm, clase2==1)
peaoc05_jvm.zmvm <- sum(cpeaoc05_jvm.zmvm$fac)
cpeanoc05_jvm.zmvm <- filter(cpt05_jvm.zmvm, clase2==2)
peanoc05_jvm.zmvm <- sum(cpeanoc05_jvm.zmvm$fac)
cpnea05_jvm.zmvm <- filter(cpt05_jvm.zmvm, clase1==2)
pnea05_jvm.zmvm <- sum(cpnea05_jvm.zmvm$fac)
cpnini05_jvm.zmvm <- filter(cpt05_jvm.zmvm, clase1==2, p2e!=3)
pnini05_jvm.zmvm <- sum(cpnini05_jvm.zmvm$fac)
motivonotrab05_jvm.zmvm <- aggregate(fac~p2e, cpnini05_jvm.zmvm, sum)
motivonotrab05_jvm.zmvm
otromotivonotrab05_jvm.zmvm <- aggregate(fac~p2g2, cpnini05_jvm.zmvm, sum)
otromotivonotrab05_jvm.zmvm
      # No jovenes 2005
cpt05_nojvm.zmvm <- filter(dem_coe05, r_def.x==0, (c_res==1|c_res==3), 
                           (eda>=30&eda<=64), sex==2, cd_a=='01') 
pt05_nojvm.zmvm <- sum(cpt05_nojvm.zmvm$fac)
cpea05_nojvm.zmvm <- filter(cpt05_nojvm.zmvm, clase1==1) 
pea05_nojvm.zmvm <- sum(cpea05_nojvm.zmvm$fac)
cpeaoc05_nojvm.zmvm <- filter(cpt05_nojvm.zmvm, clase2==1)
peaoc05_nojvm.zmvm <- sum(cpeaoc05_nojvm.zmvm$fac)
cpeanoc05_nojvm.zmvm <- filter(cpt05_nojvm.zmvm, clase2==2)
peanoc05_nojvm.zmvm <- sum(cpeanoc05_nojvm.zmvm$fac)
cpnea05_nojvm.zmvm <- filter(cpt05_nojvm.zmvm, clase1==2)
pnea05_nojvm.zmvm <- sum(cpnea05_nojvm.zmvm$fac)
      # Jovenes 2020
cpt20_jvm.zmvm <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3),
                         (eda>=15&eda<=29), sex==2, cd_a=='01') 
pt20_jvm.zmvm <- sum(cpt20_jvm.zmvm$fac)
cpea20_jvm.zmvm <- filter(cpt20_jvm.zmvm, clase1==1)
pea20_jvm.zmvm <- sum(cpea20_jvm.zmvm$fac)
cpeaoc20_jvm.zmvm <- filter(cpt20_jvm.zmvm, clase2==1)
peaoc20_jvm.zmvm <- sum(cpeaoc20_jvm.zmvm$fac)
cpeanoc20_jvm.zmvm <- filter(cpt20_jvm.zmvm, clase2==2)
peanoc20_jvm.zmvm <- sum(cpeanoc20_jvm.zmvm$fac)
cpnea20_jvm.zmvm <- filter(cpt20_jvm.zmvm, clase1==2)
pnea20_jvm.zmvm <- sum(cpnea20_jvm.zmvm$fac)
cpnini20_jvm.zmvm <- filter(cpt20_jvm.zmvm, clase1==2, p2e!=3)
pnini20_jvm.zmvm <- sum(cpnini20_jvm.zmvm$fac)
motivonotrab20_jvm.zmvm <- aggregate(fac~p2e, cpnini20_jvm.zmvm, sum)
motivonotrab20_jvm.zmvm
otromotivonotrab20_jvm.zmvm <- aggregate(fac~p2g2, cpnini20_jvm.zmvm, sum)
otromotivonotrab20_jvm.zmvm
      # No jovenes 2020
cpt20_nojvm.zmvm <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                           (eda>=30&eda<=64), sex==2, cd_a=='01') 
pt20_nojvm.zmvm <- sum(cpt20_nojvm.zmvm$fac)
cpea20_nojvm.zmvm <- filter(cpt20_nojvm.zmvm, clase1==1) 
pea20_nojvm.zmvm <- sum(cpea20_nojvm.zmvm$fac)
cpeaoc20_nojvm.zmvm <- filter(cpt20_nojvm.zmvm, clase2==1)
peaoc20_nojvm.zmvm <- sum(cpeaoc20_nojvm.zmvm$fac)
cpeanoc20_nojvm.zmvm <- filter(cpt20_nojvm.zmvm, clase2==2)
peanoc20_nojvm.zmvm <- sum(cpeanoc20_nojvm.zmvm$fac)
cpnea20_nojvm.zmvm <- filter(cpt20_nojvm.zmvm, clase1==2)
pnea20_nojvm.zmvm <- sum(cpnea20_nojvm.zmvm$fac)
      # Vector de extraccion de datos
extrac_bas.nal <- c(pt05_jov.nal, pea05_jov.nal, peaoc05_jov.nal, 
                    peanoc05_jov.nal, pnea05_jov.nal, pnini05_jov.nal, 
                    pt05_nojov.nal, pea05_nojov.nal, peaoc05_nojov.nal, 
                    peanoc05_nojov.nal, pnea05_nojov.nal, pt20_jov.nal, 
                    pea20_jov.nal, peaoc20_jov.nal, peanoc20_jov.nal, 
                    pnea20_jov.nal, pnini20_jov.nal, pt20_nojov.nal, 
                    pea20_nojov.nal, peaoc20_nojov.nal, peanoc20_nojov.nal, 
                    pnea20_nojov.nal,  pt05_jvh.nal, pea05_jvh.nal, 
                    peaoc05_jvh.nal, peanoc05_jvh.nal, pnea05_jvh.nal, 
                    pnini05_jvh.nal, pt05_nojvh.nal, pea05_nojvh.nal, 
                    peaoc05_nojvh.nal, peanoc05_nojvh.nal, pnea05_nojvh.nal, 
                    pt20_jvh.nal, pea20_jvh.nal, peaoc20_jvh.nal, 
                    peanoc20_jvh.nal, pnea20_jvh.nal, pnini20_jvh.nal, 
                    pt20_nojvh.nal, pea20_nojvh.nal, peaoc20_nojvh.nal, 
                    peanoc20_nojvh.nal, pnea20_nojvh.nal, pt05_jvm.nal, 
                    pea05_jvm.nal, peaoc05_jvm.nal, peanoc05_jvm.nal, 
                    pnea05_jvm.nal, pnini05_jvm.nal, pt05_nojvm.nal, 
                    pea05_nojvm.nal, peaoc05_nojvm.nal, peanoc05_nojvm.nal, 
                    pnea05_nojvm.nal, pt20_jvm.nal, pea20_jvm.nal, 
                    peaoc20_jvm.nal, peanoc20_jvm.nal, pnea20_jvm.nal, 
                    pnini20_jvm.nal, pt20_nojvm.nal, pea20_nojvm.nal, 
                    peaoc20_nojvm.nal, peanoc20_nojvm.nal, pnea20_nojvm.nal)
extrac_bas.zmvm <- c(pt05_jov.zmvm, pea05_jov.zmvm, peaoc05_jov.zmvm, 
                     peanoc05_jov.zmvm, pnea05_jov.zmvm, pnini05_jov.zmvm, 
                     pt05_nojov.zmvm, pea05_nojov.zmvm, peaoc05_nojov.zmvm, 
                     peanoc05_nojov.zmvm, pnea05_nojov.zmvm, pt20_jov.zmvm, 
                     pea20_jov.zmvm, peaoc20_jov.zmvm, peanoc20_jov.zmvm, 
                     pnea20_jov.zmvm, pnini20_jov.zmvm, pt20_nojov.zmvm, 
                     pea20_nojov.zmvm, peaoc20_nojov.zmvm, peanoc20_nojov.zmvm, 
                     pnea20_nojov.zmvm,  pt05_jvh.zmvm, pea05_jvh.zmvm, 
                     peaoc05_jvh.zmvm, peanoc05_jvh.zmvm, pnea05_jvh.zmvm, 
                     pnini05_jvh.zmvm, pt05_nojvh.zmvm, pea05_nojvh.zmvm, 
                     peaoc05_nojvh.zmvm, peanoc05_nojvh.zmvm, pnea05_nojvh.zmvm, 
                     pt20_jvh.zmvm, pea20_jvh.zmvm, peaoc20_jvh.zmvm, 
                     peanoc20_jvh.zmvm, pnea20_jvh.zmvm, pnini20_jvh.zmvm, 
                     pt20_nojvh.zmvm, pea20_nojvh.zmvm, peaoc20_nojvh.zmvm, 
                     peanoc20_nojvh.zmvm, pnea20_nojvh.zmvm, pt05_jvm.zmvm, 
                     pea05_jvm.zmvm, peaoc05_jvm.zmvm, peanoc05_jvm.zmvm, 
                     pnea05_jvm.zmvm, pnini05_jvm.zmvm, pt05_nojvm.zmvm, 
                     pea05_nojvm.zmvm, peaoc05_nojvm.zmvm, peanoc05_nojvm.zmvm, 
                     pnea05_nojvm.zmvm, pt20_jvm.zmvm, pea20_jvm.zmvm, 
                     peaoc20_jvm.zmvm, peanoc20_jvm.zmvm, pnea20_jvm.zmvm, 
                     pnini20_jvm.zmvm, pt20_nojvm.zmvm, pea20_nojvm.zmvm, 
                     peaoc20_nojvm.zmvm, peanoc20_nojvm.zmvm, pnea20_nojvm.zmvm)
# ------------------------------------------------------------------------------ 
  # Clasificacion del empleo en formal e informal segun definicion ampliada
    # Escala nacional urbana, 2005
      # Primer tratamiento
cpot05_tot.nal$emp_ppal.amp <- 0
cpotsub05_tot.nal <- filter(cpot05_tot.nal, mh_col<=4&p6d!=9)
cpotsub05_tot.nal$emp_ppal.amp <- 2
cpotnosub05_tot.nal <- filter(cpot05_tot.nal, mh_col>=5)
cpot05corr_tot.nal <- rbind(cpotsub05_tot.nal, cpotnosub05_tot.nal)
pot05corr_tot.nal <- sum(cpot05corr_tot.nal$fac) # POT corregida
pot05corr_tot.nal==sum(cpotsub05_tot.nal$fac)+sum(cpotnosub05_tot.nal$fac)
pot05corr_tot.nal/pot05_tot.nal # 99.4 % del total
      # Calculo de celdas de la Matriz Hussmanns (POT corregida)                  
mh05_tot.nal.11 <- filter(cpot05corr_tot.nal, mh_fil2==1&mh_col==1)
mh05_tot.nal.13 <- filter(cpot05corr_tot.nal, mh_fil2==1&mh_col==3)
mh05_tot.nal.5  <- filter(cpot05corr_tot.nal, mh_col==5)
mh05_tot.nal.17 <- filter(cpot05corr_tot.nal, mh_fil2==1&mh_col==7)                      
mh05_tot.nal.19 <- filter(cpot05corr_tot.nal, mh_fil2==1&mh_col==9)
mh05_tot.nal.21 <- filter(cpot05corr_tot.nal, mh_fil2==2&mh_col==1)
mh05_tot.nal.22 <- filter(cpot05corr_tot.nal, mh_fil2==2&mh_col==2)
mh05_tot.nal.23 <- filter(cpot05corr_tot.nal, mh_fil2==2&mh_col==3)
mh05_tot.nal.24 <- filter(cpot05corr_tot.nal, mh_fil2==2&mh_col==4)
mh05_tot.nal.31 <- filter(cpot05corr_tot.nal, mh_fil2==3&mh_col==1)
mh05_tot.nal.32 <- filter(cpot05corr_tot.nal, mh_fil2==3&mh_col==2)
mh05_tot.nal.33 <- filter(cpot05corr_tot.nal, mh_fil2==3&mh_col==3)
mh05_tot.nal.34 <- filter(cpot05corr_tot.nal, mh_fil2==3&mh_col==4)
mh05_tot.nal.36 <- filter(cpot05corr_tot.nal, mh_fil2==3&mh_col==6)
mh05_tot.nal.38 <- filter(cpot05corr_tot.nal, mh_fil2==3&mh_col==8)
mh05_tot.nal.39 <- filter(cpot05corr_tot.nal, mh_fil2==3&mh_col==9)
mh05_tot.nal.41 <- filter(cpot05corr_tot.nal, mh_fil2==4&mh_col==1)
mh05_tot.nal.42 <- filter(cpot05corr_tot.nal, mh_fil2==4&mh_col==2)
mh05_tot.nal.43 <- filter(cpot05corr_tot.nal, mh_fil2==4&mh_col==3)
mh05_tot.nal.44 <- filter(cpot05corr_tot.nal, mh_fil2==4&mh_col==4)
mh05_tot.nal.46 <- filter(cpot05corr_tot.nal, mh_fil2==4&mh_col==6)
mh05_tot.nal.47 <- filter(cpot05corr_tot.nal, mh_fil2==4&mh_col==7)
mh05_tot.nal.49 <- filter(cpot05corr_tot.nal, mh_fil2==4&mh_col==9)
pot05mh_tot.nal <- sum(mh05_tot.nal.11$fac)+sum(mh05_tot.nal.13$fac)+
                   sum(mh05_tot.nal.5$fac)+sum(mh05_tot.nal.17$fac)+
                   sum(mh05_tot.nal.19$fac)+sum(mh05_tot.nal.21$fac)+
                   sum(mh05_tot.nal.22$fac)+sum(mh05_tot.nal.23$fac)+
                   sum(mh05_tot.nal.24$fac)+sum(mh05_tot.nal.31$fac)+
                   sum(mh05_tot.nal.32$fac)+sum(mh05_tot.nal.33$fac)+
                   sum(mh05_tot.nal.34$fac)+sum(mh05_tot.nal.36$fac)+
                   sum(mh05_tot.nal.38$fac)+sum(mh05_tot.nal.39$fac)+
                   sum(mh05_tot.nal.41$fac)+sum(mh05_tot.nal.42$fac)+
                   sum(mh05_tot.nal.43$fac)+sum(mh05_tot.nal.44$fac)+
                   sum(mh05_tot.nal.46$fac)+sum(mh05_tot.nal.47$fac)+
                   sum(mh05_tot.nal.49$fac)
pot05mh_tot.nal==pot05corr_tot.nal 
      # Calculo de trabajadores por categoria (formal/informal)
cpotsub05_tot.nal <- mutate_at(cpotsub05_tot.nal, c('p3m4'), 
                               ~replace(., is.na(.), 0))
cpotsub05_tot.nal$emp_ppal.amp <- ifelse(cpotsub05_tot.nal$p3m4==0|
                                           cpotsub05_tot.nal$p6d==6, 1, 
                                         cpotsub05_tot.nal$emp_ppal.amp)
cpotsubinf05_tot.nal <- filter(cpotsub05_tot.nal, emp_ppal.amp==1)
cpotsubfor05_tot.nal <- filter(cpotsub05_tot.nal, emp_ppal.amp==2)
cpotinf05_tot.nal <- rbind(cpotsubinf05_tot.nal, mh05_tot.nal.5, 
                           mh05_tot.nal.17,mh05_tot.nal.19, mh05_tot.nal.39, 
                           mh05_tot.nal.47, mh05_tot.nal.49)
potinf05_tot.nal <- sum(cpotinf05_tot.nal$fac)
cpotfor05_tot.nal <- rbind(cpotsubfor05_tot.nal, mh05_tot.nal.36, 
                           mh05_tot.nal.38, mh05_tot.nal.46)
potfor05_tot.nal <- sum(cpotfor05_tot.nal$fac)
pot05corr_tot.nal==potinf05_tot.nal+potfor05_tot.nal
potinf05_tot.nal/pot05corr_tot.nal
potfor05_tot.nal/pot05corr_tot.nal
      # Calculo de celdas de la Matriz Hussmanns con base en la 
      # definicion ampliada
mh05.amp_tot.nal.11 <- filter(cpotsubinf05_tot.nal, mh_fil2==1&mh_col==1)
mh05.amp_tot.nal.12 <- filter(cpotsubfor05_tot.nal, mh_fil2==1&mh_col==1)
mh05.amp_tot.nal.13 <- filter(cpotsubinf05_tot.nal, mh_fil2==1&mh_col==3)
mh05.amp_tot.nal.14 <- filter(cpotsubfor05_tot.nal, mh_fil2==1&mh_col==3)
mh05.amp_tot.nal.21 <- filter(cpotsubinf05_tot.nal, mh_fil2==2&(mh_col==1|
                                                                mh_col==2))
mh05.amp_tot.nal.22 <- filter(cpotsubfor05_tot.nal, mh_fil2==2&(mh_col==1|
                                                                mh_col==2))
mh05.amp_tot.nal.23 <- filter(cpotsubinf05_tot.nal, mh_fil2==2&(mh_col==3|
                                                                mh_col==4))
mh05.amp_tot.nal.24 <- filter(cpotsubfor05_tot.nal, mh_fil2==2&(mh_col==3|
                                                                mh_col==4))
mh05.amp_tot.nal.31 <- filter(cpotsubinf05_tot.nal, mh_fil2==3&(mh_col==1|
                                                                mh_col==2))
mh05.amp_tot.nal.32 <- filter(cpotsubfor05_tot.nal, mh_fil2==3&(mh_col==1|
                                                                mh_col==2))
mh05.amp_tot.nal.33 <- filter(cpotsubinf05_tot.nal, mh_fil2==3&(mh_col==3|
                                                                mh_col==4))
mh05.amp_tot.nal.34 <- filter(cpotsubfor05_tot.nal, mh_fil2==3&(mh_col==3|
                                                                mh_col==4))
mh05.amp_tot.nal.41 <- filter(cpotsubinf05_tot.nal, mh_fil2==4&(mh_col==1|
                                                                mh_col==2))
mh05.amp_tot.nal.42 <- filter(cpotsubfor05_tot.nal, mh_fil2==4&(mh_col==1|
                                                                mh_col==2))
mh05.amp_tot.nal.43 <- filter(cpotsubinf05_tot.nal, mh_fil2==4&(mh_col==3|
                                                                mh_col==4))
mh05.amp_tot.nal.44 <- filter(cpotsubfor05_tot.nal, mh_fil2==4&(mh_col==3|
                                                                mh_col==4))
sum(mh05.amp_tot.nal.11$fac)  # Asalariados informales del sector informal
sum(mh05.amp_tot.nal.12$fac)  # Asalariados formales del sector informal
sum(mh05.amp_tot.nal.13$fac)  # No asalariados informales del sector informal
sum(mh05.amp_tot.nal.14$fac)  # No asalariados formales del sector informal
sum(mh05_tot.nal.5$fac) # Empleadores informales
sum(mh05_tot.nal.17$fac)  # Cuentapropistas del sector informal
sum(mh05_tot.nal.19$fac)  # No remunerados del sector informal
sum(mh05.amp_tot.nal.21$fac)  # Asalariados informales en trabajo del hogar
sum(mh05.amp_tot.nal.22$fac)  # Asalariados formales en trabajo del hogar
sum(mh05.amp_tot.nal.23$fac)  # No asalariados informales en trabajo del hogar
sum(mh05.amp_tot.nal.24$fac)  # No asalariados formales en trabajo del hogar
sum(mh05.amp_tot.nal.31$fac)  # Asalariados informales fuera del sector informal
sum(mh05.amp_tot.nal.32$fac)  
       # Asalariados formales fuera del sector informal
sum(mh05.amp_tot.nal.33$fac)  
       # No asalariados informales fuera del sector informal
sum(mh05.amp_tot.nal.34$fac)  
       # No asalariados formales fuera del sector informal
sum(mh05_tot.nal.36$fac)  # Empleadores fuera del sector informal
sum(mh05_tot.nal.38$fac)  # Cuentapropistas formales
sum(mh05_tot.nal.39$fac)  # No remunerados fuera del sector informal
sum(mh05.amp_tot.nal.41$fac)  # Asalariados informales del ambito agropecuario
sum(mh05.amp_tot.nal.42$fac)  # Asalariados formales del ambito agropecuario
sum(mh05.amp_tot.nal.43$fac)  
       # No asalariados informales del ambito agropecuario
sum(mh05.amp_tot.nal.44$fac)  # No asalariados formales del ambito agropecuario
sum(mh05_tot.nal.46$fac)  # Empleadores del ambito agropecuario
sum(mh05_tot.nal.47$fac)  # Cuentapropistas del ambito agropecuario
sum(mh05_tot.nal.49$fac)  # No remunerados del ambito agropecuario
pot05mh.amp_tot.nal <- sum(mh05.amp_tot.nal.11$fac)+
                       sum(mh05.amp_tot.nal.12$fac)+
                       sum(mh05.amp_tot.nal.13$fac)+
                       sum(mh05.amp_tot.nal.14$fac)+sum(mh05_tot.nal.5$fac)+
                       sum(mh05_tot.nal.17$fac)+sum(mh05_tot.nal.19$fac)+
                       sum(mh05.amp_tot.nal.21$fac)+
                       sum(mh05.amp_tot.nal.22$fac)+
                       sum(mh05.amp_tot.nal.23$fac)+
                       sum(mh05.amp_tot.nal.24$fac)+
                       sum(mh05.amp_tot.nal.31$fac)+
                       sum(mh05.amp_tot.nal.32$fac)+
                       sum(mh05.amp_tot.nal.33$fac)+
                       sum(mh05.amp_tot.nal.34$fac)+
                       sum(mh05_tot.nal.36$fac)+
                       sum(mh05_tot.nal.38$fac)+sum(mh05_tot.nal.39$fac)+
                       sum(mh05.amp_tot.nal.41$fac)+
                       sum(mh05.amp_tot.nal.42$fac)+
                       sum(mh05.amp_tot.nal.43$fac)+
                       sum(mh05.amp_tot.nal.44$fac)+
                       sum(mh05_tot.nal.46$fac)+sum(mh05_tot.nal.47$fac)+
                       sum(mh05_tot.nal.49$fac)
pot05mh.amp_tot.nal==pot05corr_tot.nal
    # Escala nacional urbana, 2020
      # Primer tratamiento  
cpot20_tot.nal$emp_ppal.amp <- 0
cpotsub20_tot.nal <- filter(cpot20_tot.nal, mh_col<=4&p6d!=9)
cpotsub20_tot.nal$emp_ppal.amp <- 2
cpotnosub20_tot.nal <- filter(cpot20_tot.nal, mh_col>=5)
cpot20corr_tot.nal <- rbind(cpotsub20_tot.nal, cpotnosub20_tot.nal)
pot20corr_tot.nal <- sum(cpot20corr_tot.nal$fac) # POT corregida
pot20corr_tot.nal==sum(cpotsub20_tot.nal$fac)+sum(cpotnosub20_tot.nal$fac)
pot20corr_tot.nal/pot20_tot.nal # 99.5 % del total
      # Calculo de celdas de la Matriz Hussmanns (POT corregida)                    
mh20_tot.nal.11 <- filter(cpot20corr_tot.nal, mh_fil2==1&mh_col==1)
mh20_tot.nal.13 <- filter(cpot20corr_tot.nal, mh_fil2==1&mh_col==3)
mh20_tot.nal.5  <- filter(cpot20corr_tot.nal, mh_col==5)
mh20_tot.nal.17 <- filter(cpot20corr_tot.nal, mh_fil2==1&mh_col==7)                      
mh20_tot.nal.19 <- filter(cpot20corr_tot.nal, mh_fil2==1&mh_col==9)
mh20_tot.nal.21 <- filter(cpot20corr_tot.nal, mh_fil2==2&mh_col==1)
mh20_tot.nal.22 <- filter(cpot20corr_tot.nal, mh_fil2==2&mh_col==2)
mh20_tot.nal.23 <- filter(cpot20corr_tot.nal, mh_fil2==2&mh_col==3)
mh20_tot.nal.24 <- filter(cpot20corr_tot.nal, mh_fil2==2&mh_col==4)
mh20_tot.nal.31 <- filter(cpot20corr_tot.nal, mh_fil2==3&mh_col==1)
mh20_tot.nal.32 <- filter(cpot20corr_tot.nal, mh_fil2==3&mh_col==2)
mh20_tot.nal.33 <- filter(cpot20corr_tot.nal, mh_fil2==3&mh_col==3)
mh20_tot.nal.34 <- filter(cpot20corr_tot.nal, mh_fil2==3&mh_col==4)
mh20_tot.nal.36 <- filter(cpot20corr_tot.nal, mh_fil2==3&mh_col==6)
mh20_tot.nal.38 <- filter(cpot20corr_tot.nal, mh_fil2==3&mh_col==8)
mh20_tot.nal.39 <- filter(cpot20corr_tot.nal, mh_fil2==3&mh_col==9)
mh20_tot.nal.41 <- filter(cpot20corr_tot.nal, mh_fil2==4&mh_col==1)
mh20_tot.nal.42 <- filter(cpot20corr_tot.nal, mh_fil2==4&mh_col==2)
mh20_tot.nal.43 <- filter(cpot20corr_tot.nal, mh_fil2==4&mh_col==3)
mh20_tot.nal.44 <- filter(cpot20corr_tot.nal, mh_fil2==4&mh_col==4)
mh20_tot.nal.46 <- filter(cpot20corr_tot.nal, mh_fil2==4&mh_col==6)
mh20_tot.nal.47 <- filter(cpot20corr_tot.nal, mh_fil2==4&mh_col==7)
mh20_tot.nal.49 <- filter(cpot20corr_tot.nal, mh_fil2==4&mh_col==9)
pot20mh_tot.nal <- sum(mh20_tot.nal.11$fac)+sum(mh20_tot.nal.13$fac)+
                   sum(mh20_tot.nal.5$fac)+sum(mh20_tot.nal.17$fac)+
                   sum(mh20_tot.nal.19$fac)+sum(mh20_tot.nal.21$fac)+
                   sum(mh20_tot.nal.22$fac)+sum(mh20_tot.nal.23$fac)+
                   sum(mh20_tot.nal.24$fac)+sum(mh20_tot.nal.31$fac)+
                   sum(mh20_tot.nal.32$fac)+sum(mh20_tot.nal.33$fac)+
                   sum(mh20_tot.nal.34$fac)+sum(mh20_tot.nal.36$fac)+
                   sum(mh20_tot.nal.38$fac)+sum(mh20_tot.nal.39$fac)+
                   sum(mh20_tot.nal.41$fac)+sum(mh20_tot.nal.42$fac)+
                   sum(mh20_tot.nal.43$fac)+sum(mh20_tot.nal.44$fac)+
                   sum(mh20_tot.nal.46$fac)+sum(mh20_tot.nal.47$fac)+
                   sum(mh20_tot.nal.49$fac)
pot20mh_tot.nal==pot20corr_tot.nal 
      # Calculo de trabajadores por categoria (formal/informal)
cpotsub20_tot.nal <- mutate_at(cpotsub20_tot.nal, c('p3m4'), 
                               ~replace(., is.na(.), 0))
cpotsub20_tot.nal$emp_ppal.amp <- ifelse(cpotsub20_tot.nal$p3m4==0|
                                         cpotsub20_tot.nal$p6d==6, 1, 
                                         cpotsub20_tot.nal$emp_ppal.amp)
cpotsubinf20_tot.nal <- filter(cpotsub20_tot.nal, emp_ppal.amp==1)
cpotsubfor20_tot.nal <- filter(cpotsub20_tot.nal, emp_ppal.amp==2)
cpotinf20_tot.nal <- rbind(cpotsubinf20_tot.nal, mh20_tot.nal.5, 
                           mh20_tot.nal.17,mh20_tot.nal.19, mh20_tot.nal.39, 
                           mh20_tot.nal.47, mh20_tot.nal.49)
potinf20_tot.nal <- sum(cpotinf20_tot.nal$fac)
cpotfor20_tot.nal <- rbind(cpotsubfor20_tot.nal, mh20_tot.nal.36, 
                           mh20_tot.nal.38, mh20_tot.nal.46)
potfor20_tot.nal <- sum(cpotfor20_tot.nal$fac)
pot20corr_tot.nal==potinf20_tot.nal+potfor20_tot.nal
potinf20_tot.nal/pot20corr_tot.nal
potfor20_tot.nal/pot20corr_tot.nal
      # Calculo de celdas de la Matriz Hussmanns con base en la 
      # definicion ampliada
mh20.amp_tot.nal.11 <- filter(cpotsubinf20_tot.nal, mh_fil2==1&mh_col==1)
mh20.amp_tot.nal.12 <- filter(cpotsubfor20_tot.nal, mh_fil2==1&mh_col==1)
mh20.amp_tot.nal.13 <- filter(cpotsubinf20_tot.nal, mh_fil2==1&mh_col==3)
mh20.amp_tot.nal.14 <- filter(cpotsubfor20_tot.nal, mh_fil2==1&mh_col==3)
mh20.amp_tot.nal.21 <- filter(cpotsubinf20_tot.nal, mh_fil2==2&(mh_col==1|
                                                                mh_col==2))
mh20.amp_tot.nal.22 <- filter(cpotsubfor20_tot.nal, mh_fil2==2&(mh_col==1|
                                                                mh_col==2))
mh20.amp_tot.nal.23 <- filter(cpotsubinf20_tot.nal, mh_fil2==2&(mh_col==3|
                                                                mh_col==4))
mh20.amp_tot.nal.24 <- filter(cpotsubfor20_tot.nal, mh_fil2==2&(mh_col==3|
                                                                mh_col==4))
mh20.amp_tot.nal.31 <- filter(cpotsubinf20_tot.nal, mh_fil2==3&(mh_col==1|
                                                                mh_col==2))
mh20.amp_tot.nal.32 <- filter(cpotsubfor20_tot.nal, mh_fil2==3&(mh_col==1|
                                                                mh_col==2))
mh20.amp_tot.nal.33 <- filter(cpotsubinf20_tot.nal, mh_fil2==3&(mh_col==3|
                                                                mh_col==4))
mh20.amp_tot.nal.34 <- filter(cpotsubfor20_tot.nal, mh_fil2==3&(mh_col==3|
                                                                mh_col==4))
mh20.amp_tot.nal.41 <- filter(cpotsubinf20_tot.nal, mh_fil2==4&(mh_col==1|
                                                                mh_col==2))
mh20.amp_tot.nal.42 <- filter(cpotsubfor20_tot.nal, mh_fil2==4&(mh_col==1|
                                                                mh_col==2))
mh20.amp_tot.nal.43 <- filter(cpotsubinf20_tot.nal, mh_fil2==4&(mh_col==3|
                                                                mh_col==4))
mh20.amp_tot.nal.44 <- filter(cpotsubfor20_tot.nal, mh_fil2==4&(mh_col==3|
                                                                mh_col==4))
sum(mh20.amp_tot.nal.11$fac)  # Asalariados informales del sector informal
sum(mh20.amp_tot.nal.12$fac)  # Asalariados formales del sector informal
sum(mh20.amp_tot.nal.13$fac)  # No asalariados informales del sector informal
sum(mh20.amp_tot.nal.14$fac)  # No asalariados formales del sector informal
sum(mh20_tot.nal.5$fac) # Empleadores informales
sum(mh20_tot.nal.17$fac)  # Cuentapropistas del sector informal
sum(mh20_tot.nal.19$fac)  # No remunerados del sector informal
sum(mh20.amp_tot.nal.21$fac)  # Asalariados informales en trabajo del hogar
sum(mh20.amp_tot.nal.22$fac)  # Asalariados formales en trabajo del hogar
sum(mh20.amp_tot.nal.23$fac)  # No asalariados informales en trabajo del hogar
sum(mh20.amp_tot.nal.24$fac)  # No asalariados formales en trabajo del hogar
sum(mh20.amp_tot.nal.31$fac)  # Asalariados informales fuera del sector informal
sum(mh20.amp_tot.nal.32$fac)  
        # Asalariados formales fuera del sector informal
sum(mh20.amp_tot.nal.33$fac)  
        # No asalariados informales fuera del sector informal
sum(mh20.amp_tot.nal.34$fac)  
        # No asalariados formales fuera del sector informal
sum(mh20_tot.nal.36$fac)  # Empleadores fuera del sector informal
sum(mh20_tot.nal.38$fac)  # Cuentapropistas formales
sum(mh20_tot.nal.39$fac)  # No remunerados fuera del sector informal
sum(mh20.amp_tot.nal.41$fac)  # Asalariados informales del ambito agropecuario
sum(mh20.amp_tot.nal.42$fac)  # Asalariados formales del ambito agropecuario
sum(mh20.amp_tot.nal.43$fac)  
        # No asalariados informales del ambito agropecuario
sum(mh20.amp_tot.nal.44$fac)  # No asalariados formales del ambito agropecuario
sum(mh20_tot.nal.46$fac)  # Empleadores del ambito agropecuario
sum(mh20_tot.nal.47$fac)  # Cuentapropistas del ambito agropecuario
sum(mh20_tot.nal.49$fac)  # No remunerados del ambito agropecuario
pot20mh.amp_tot.nal <- sum(mh20.amp_tot.nal.11$fac)+
                       sum(mh20.amp_tot.nal.12$fac)+
                       sum(mh20.amp_tot.nal.13$fac)+
                       sum(mh20.amp_tot.nal.14$fac)+sum(mh20_tot.nal.5$fac)+
                       sum(mh20_tot.nal.17$fac)+sum(mh20_tot.nal.19$fac)+
                       sum(mh20.amp_tot.nal.21$fac)+
                       sum(mh20.amp_tot.nal.22$fac)+
                       sum(mh20.amp_tot.nal.23$fac)+
                       sum(mh20.amp_tot.nal.24$fac)+
                       sum(mh20.amp_tot.nal.31$fac)+
                       sum(mh20.amp_tot.nal.32$fac)+
                       sum(mh20.amp_tot.nal.33$fac)+
                       sum(mh20.amp_tot.nal.34$fac)+
                       sum(mh20_tot.nal.36$fac)+
                       sum(mh20_tot.nal.38$fac)+sum(mh20_tot.nal.39$fac)+
                       sum(mh20.amp_tot.nal.41$fac)+
                       sum(mh20.amp_tot.nal.42$fac)+
                       sum(mh20.amp_tot.nal.43$fac)+
                       sum(mh20.amp_tot.nal.44$fac)+
                       sum(mh20_tot.nal.46$fac)+sum(mh20_tot.nal.47$fac)+
                       sum(mh20_tot.nal.49$fac)
pot20mh.amp_tot.nal==pot20corr_tot.nal
    # Escala ZMVM, 2005
      # Primer tratamiento
cpot05_tot.zmvm$emp_ppal.amp <- 0
cpotsub05_tot.zmvm <- filter(cpot05_tot.zmvm, mh_col<=4&p6d!=9)
cpotsub05_tot.zmvm$emp_ppal.amp <- 2
cpotnosub05_tot.zmvm <- filter(cpot05_tot.zmvm, mh_col>=5)
cpot05corr_tot.zmvm <- rbind(cpotsub05_tot.zmvm, cpotnosub05_tot.zmvm)
pot05corr_tot.zmvm <- sum(cpot05corr_tot.zmvm$fac) # POT corregida
pot05corr_tot.zmvm==sum(cpotsub05_tot.zmvm$fac)+sum(cpotnosub05_tot.zmvm$fac)
pot05corr_tot.zmvm/pot05_tot.zmvm # 99.7 % del total
      # Calculo de celdas de la Matriz Hussmanns (POT corregida)                  
mh05_tot.zmvm.11 <- filter(cpot05corr_tot.zmvm, mh_fil2==1&mh_col==1)
mh05_tot.zmvm.13 <- filter(cpot05corr_tot.zmvm, mh_fil2==1&mh_col==3)
mh05_tot.zmvm.5  <- filter(cpot05corr_tot.zmvm, mh_col==5)
mh05_tot.zmvm.17 <- filter(cpot05corr_tot.zmvm, mh_fil2==1&mh_col==7)                      
mh05_tot.zmvm.19 <- filter(cpot05corr_tot.zmvm, mh_fil2==1&mh_col==9)
mh05_tot.zmvm.21 <- filter(cpot05corr_tot.zmvm, mh_fil2==2&mh_col==1)
mh05_tot.zmvm.22 <- filter(cpot05corr_tot.zmvm, mh_fil2==2&mh_col==2)
mh05_tot.zmvm.23 <- filter(cpot05corr_tot.zmvm, mh_fil2==2&mh_col==3)
mh05_tot.zmvm.24 <- filter(cpot05corr_tot.zmvm, mh_fil2==2&mh_col==4)
mh05_tot.zmvm.31 <- filter(cpot05corr_tot.zmvm, mh_fil2==3&mh_col==1)
mh05_tot.zmvm.32 <- filter(cpot05corr_tot.zmvm, mh_fil2==3&mh_col==2)
mh05_tot.zmvm.33 <- filter(cpot05corr_tot.zmvm, mh_fil2==3&mh_col==3)
mh05_tot.zmvm.34 <- filter(cpot05corr_tot.zmvm, mh_fil2==3&mh_col==4)
mh05_tot.zmvm.36 <- filter(cpot05corr_tot.zmvm, mh_fil2==3&mh_col==6)
mh05_tot.zmvm.38 <- filter(cpot05corr_tot.zmvm, mh_fil2==3&mh_col==8)
mh05_tot.zmvm.39 <- filter(cpot05corr_tot.zmvm, mh_fil2==3&mh_col==9)
mh05_tot.zmvm.41 <- filter(cpot05corr_tot.zmvm, mh_fil2==4&mh_col==1)
mh05_tot.zmvm.42 <- filter(cpot05corr_tot.zmvm, mh_fil2==4&mh_col==2)
mh05_tot.zmvm.43 <- filter(cpot05corr_tot.zmvm, mh_fil2==4&mh_col==3)
mh05_tot.zmvm.44 <- filter(cpot05corr_tot.zmvm, mh_fil2==4&mh_col==4)
mh05_tot.zmvm.46 <- filter(cpot05corr_tot.zmvm, mh_fil2==4&mh_col==6)
mh05_tot.zmvm.47 <- filter(cpot05corr_tot.zmvm, mh_fil2==4&mh_col==7)
mh05_tot.zmvm.49 <- filter(cpot05corr_tot.zmvm, mh_fil2==4&mh_col==9)
pot05mh_tot.zmvm <- sum(mh05_tot.zmvm.11$fac)+sum(mh05_tot.zmvm.13$fac)+
                    sum(mh05_tot.zmvm.5$fac)+sum(mh05_tot.zmvm.17$fac)+
                    sum(mh05_tot.zmvm.19$fac)+sum(mh05_tot.zmvm.21$fac)+
                    sum(mh05_tot.zmvm.22$fac)+sum(mh05_tot.zmvm.23$fac)+
                    sum(mh05_tot.zmvm.24$fac)+sum(mh05_tot.zmvm.31$fac)+
                    sum(mh05_tot.zmvm.32$fac)+sum(mh05_tot.zmvm.33$fac)+
                    sum(mh05_tot.zmvm.34$fac)+sum(mh05_tot.zmvm.36$fac)+
                    sum(mh05_tot.zmvm.38$fac)+sum(mh05_tot.zmvm.39$fac)+
                    sum(mh05_tot.zmvm.41$fac)+sum(mh05_tot.zmvm.42$fac)+
                    sum(mh05_tot.zmvm.43$fac)+sum(mh05_tot.zmvm.44$fac)+
                    sum(mh05_tot.zmvm.46$fac)+sum(mh05_tot.zmvm.47$fac)+
                    sum(mh05_tot.zmvm.49$fac)
pot05mh_tot.zmvm==pot05corr_tot.zmvm 
      # Calculo de trabajadores por categoria (formal/informal)
cpotsub05_tot.zmvm <- mutate_at(cpotsub05_tot.zmvm, c('p3m4'), 
                                ~replace(., is.na(.), 0))
cpotsub05_tot.zmvm$emp_ppal.amp <- ifelse(cpotsub05_tot.zmvm$p3m4==0|
                                          cpotsub05_tot.zmvm$p6d==6, 1, 
                                          cpotsub05_tot.zmvm$emp_ppal.amp)
cpotsubinf05_tot.zmvm <- filter(cpotsub05_tot.zmvm, emp_ppal.amp==1)
cpotsubfor05_tot.zmvm <- filter(cpotsub05_tot.zmvm, emp_ppal.amp==2)
cpotinf05_tot.zmvm <- rbind(cpotsubinf05_tot.zmvm, mh05_tot.zmvm.5, 
                            mh05_tot.zmvm.17,mh05_tot.zmvm.19, mh05_tot.zmvm.39, 
                            mh05_tot.zmvm.47, mh05_tot.zmvm.49)
potinf05_tot.zmvm <- sum(cpotinf05_tot.zmvm$fac)
cpotfor05_tot.zmvm <- rbind(cpotsubfor05_tot.zmvm, mh05_tot.zmvm.36, 
                            mh05_tot.zmvm.38, mh05_tot.zmvm.46)
potfor05_tot.zmvm <- sum(cpotfor05_tot.zmvm$fac)
pot05corr_tot.zmvm==potinf05_tot.zmvm+potfor05_tot.zmvm
potinf05_tot.zmvm/pot05corr_tot.zmvm
potfor05_tot.zmvm/pot05corr_tot.zmvm
      # Calculo de celdas de la Matriz Hussmanns con base en la 
      # definicion ampliada
mh05.amp_tot.zmvm.11 <- filter(cpotsubinf05_tot.zmvm, mh_fil2==1&mh_col==1)
mh05.amp_tot.zmvm.12 <- filter(cpotsubfor05_tot.zmvm, mh_fil2==1&mh_col==1)
mh05.amp_tot.zmvm.13 <- filter(cpotsubinf05_tot.zmvm, mh_fil2==1&mh_col==3)
mh05.amp_tot.zmvm.14 <- filter(cpotsubfor05_tot.zmvm, mh_fil2==1&mh_col==3)
mh05.amp_tot.zmvm.21 <- filter(cpotsubinf05_tot.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_tot.zmvm.22 <- filter(cpotsubfor05_tot.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_tot.zmvm.23 <- filter(cpotsubinf05_tot.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_tot.zmvm.24 <- filter(cpotsubfor05_tot.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_tot.zmvm.31 <- filter(cpotsubinf05_tot.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_tot.zmvm.32 <- filter(cpotsubfor05_tot.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_tot.zmvm.33 <- filter(cpotsubinf05_tot.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_tot.zmvm.34 <- filter(cpotsubfor05_tot.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_tot.zmvm.41 <- filter(cpotsubinf05_tot.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_tot.zmvm.42 <- filter(cpotsubfor05_tot.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_tot.zmvm.43 <- filter(cpotsubinf05_tot.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_tot.zmvm.44 <- filter(cpotsubfor05_tot.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
sum(mh05.amp_tot.zmvm.11$fac)  # Asalariados informales del sector informal
sum(mh05.amp_tot.zmvm.12$fac)  # Asalariados formales del sector informal
sum(mh05.amp_tot.zmvm.13$fac)  # No asalariados informales del sector informal
sum(mh05.amp_tot.zmvm.14$fac)  # No asalariados formales del sector informal
sum(mh05_tot.zmvm.5$fac) # Empleadores informales
sum(mh05_tot.zmvm.17$fac)  # Cuentapropistas del sector informal
sum(mh05_tot.zmvm.19$fac)  # No remunerados del sector informal
sum(mh05.amp_tot.zmvm.21$fac)  # Asalariados informales en trabajo del hogar
sum(mh05.amp_tot.zmvm.22$fac)  # Asalariados formales en trabajo del hogar
sum(mh05.amp_tot.zmvm.23$fac)  # No asalariados informales en trabajo del hogar
sum(mh05.amp_tot.zmvm.24$fac)  # No asalariados formales en trabajo del hogar
sum(mh05.amp_tot.zmvm.31$fac)  # Asalariados informales fuera del sector informal
sum(mh05.amp_tot.zmvm.32$fac)  
        # Asalariados formales fuera del sector informal
sum(mh05.amp_tot.zmvm.33$fac)  
        # No asalariados informales fuera del sector informal
sum(mh05.amp_tot.zmvm.34$fac)  
        # No asalariados formales fuera del sector informal
sum(mh05_tot.zmvm.36$fac)  # Empleadores fuera del sector informal
sum(mh05_tot.zmvm.38$fac)  # Cuentapropistas formales
sum(mh05_tot.zmvm.39$fac)  # No remunerados fuera del sector informal
sum(mh05.amp_tot.zmvm.41$fac)  # Asalariados informales del ambito agropecuario
sum(mh05.amp_tot.zmvm.42$fac)  # Asalariados formales del ambito agropecuario
sum(mh05.amp_tot.zmvm.43$fac)  
        # No asalariados informales del ambito agropecuario
sum(mh05.amp_tot.zmvm.44$fac)  # No asalariados formales del ambito agropecuario
sum(mh05_tot.zmvm.46$fac)  # Empleadores del ambito agropecuario
sum(mh05_tot.zmvm.47$fac)  # Cuentapropistas del ambito agropecuario
sum(mh05_tot.zmvm.49$fac)  # No remunerados del ambito agropecuario
pot05mh.amp_tot.zmvm <- sum(mh05.amp_tot.zmvm.11$fac)+
                        sum(mh05.amp_tot.zmvm.12$fac)+
                        sum(mh05.amp_tot.zmvm.13$fac)+
                        sum(mh05.amp_tot.zmvm.14$fac)+sum(mh05_tot.zmvm.5$fac)+
                        sum(mh05_tot.zmvm.17$fac)+sum(mh05_tot.zmvm.19$fac)+
                        sum(mh05.amp_tot.zmvm.21$fac)+
                        sum(mh05.amp_tot.zmvm.22$fac)+
                        sum(mh05.amp_tot.zmvm.23$fac)+
                        sum(mh05.amp_tot.zmvm.24$fac)+
                        sum(mh05.amp_tot.zmvm.31$fac)+
                        sum(mh05.amp_tot.zmvm.32$fac)+
                        sum(mh05.amp_tot.zmvm.33$fac)+
                        sum(mh05.amp_tot.zmvm.34$fac)+
                        sum(mh05_tot.zmvm.36$fac)+
                        sum(mh05_tot.zmvm.38$fac)+sum(mh05_tot.zmvm.39$fac)+
                        sum(mh05.amp_tot.zmvm.41$fac)+
                        sum(mh05.amp_tot.zmvm.42$fac)+
                        sum(mh05.amp_tot.zmvm.43$fac)+
                        sum(mh05.amp_tot.zmvm.44$fac)+
                        sum(mh05_tot.zmvm.46$fac)+sum(mh05_tot.zmvm.47$fac)+
                        sum(mh05_tot.zmvm.49$fac)
pot05mh.amp_tot.zmvm==pot05corr_tot.zmvm
    # Escala ZMVM, 2020
      # Primer tratamiento  
cpot20_tot.zmvm$emp_ppal.amp <- 0
cpotsub20_tot.zmvm <- filter(cpot20_tot.zmvm, mh_col<=4&p6d!=9)
cpotsub20_tot.zmvm$emp_ppal.amp <- 2
cpotnosub20_tot.zmvm <- filter(cpot20_tot.zmvm, mh_col>=5)
cpot20corr_tot.zmvm <- rbind(cpotsub20_tot.zmvm, cpotnosub20_tot.zmvm)
pot20corr_tot.zmvm <- sum(cpot20corr_tot.zmvm$fac) # POT corregida
pot20corr_tot.zmvm==sum(cpotsub20_tot.zmvm$fac)+sum(cpotnosub20_tot.zmvm$fac)
pot20corr_tot.zmvm/pot20_tot.zmvm # 99.9 % del total
      # Calculo de celdas de la Matriz Hussmanns (POT corregida)                    
mh20_tot.zmvm.11 <- filter(cpot20corr_tot.zmvm, mh_fil2==1&mh_col==1)
mh20_tot.zmvm.13 <- filter(cpot20corr_tot.zmvm, mh_fil2==1&mh_col==3)
mh20_tot.zmvm.5  <- filter(cpot20corr_tot.zmvm, mh_col==5)
mh20_tot.zmvm.17 <- filter(cpot20corr_tot.zmvm, mh_fil2==1&mh_col==7)                      
mh20_tot.zmvm.19 <- filter(cpot20corr_tot.zmvm, mh_fil2==1&mh_col==9)
mh20_tot.zmvm.21 <- filter(cpot20corr_tot.zmvm, mh_fil2==2&mh_col==1)
mh20_tot.zmvm.22 <- filter(cpot20corr_tot.zmvm, mh_fil2==2&mh_col==2)
mh20_tot.zmvm.23 <- filter(cpot20corr_tot.zmvm, mh_fil2==2&mh_col==3)
mh20_tot.zmvm.24 <- filter(cpot20corr_tot.zmvm, mh_fil2==2&mh_col==4)
mh20_tot.zmvm.31 <- filter(cpot20corr_tot.zmvm, mh_fil2==3&mh_col==1)
mh20_tot.zmvm.32 <- filter(cpot20corr_tot.zmvm, mh_fil2==3&mh_col==2)
mh20_tot.zmvm.33 <- filter(cpot20corr_tot.zmvm, mh_fil2==3&mh_col==3)
mh20_tot.zmvm.34 <- filter(cpot20corr_tot.zmvm, mh_fil2==3&mh_col==4)
mh20_tot.zmvm.36 <- filter(cpot20corr_tot.zmvm, mh_fil2==3&mh_col==6)
mh20_tot.zmvm.38 <- filter(cpot20corr_tot.zmvm, mh_fil2==3&mh_col==8)
mh20_tot.zmvm.39 <- filter(cpot20corr_tot.zmvm, mh_fil2==3&mh_col==9)
mh20_tot.zmvm.41 <- filter(cpot20corr_tot.zmvm, mh_fil2==4&mh_col==1)
mh20_tot.zmvm.42 <- filter(cpot20corr_tot.zmvm, mh_fil2==4&mh_col==2)
mh20_tot.zmvm.43 <- filter(cpot20corr_tot.zmvm, mh_fil2==4&mh_col==3)
mh20_tot.zmvm.44 <- filter(cpot20corr_tot.zmvm, mh_fil2==4&mh_col==4)
mh20_tot.zmvm.46 <- filter(cpot20corr_tot.zmvm, mh_fil2==4&mh_col==6)
mh20_tot.zmvm.47 <- filter(cpot20corr_tot.zmvm, mh_fil2==4&mh_col==7)
mh20_tot.zmvm.49 <- filter(cpot20corr_tot.zmvm, mh_fil2==4&mh_col==9)
pot20mh_tot.zmvm <- sum(mh20_tot.zmvm.11$fac)+sum(mh20_tot.zmvm.13$fac)+
                    sum(mh20_tot.zmvm.5$fac)+sum(mh20_tot.zmvm.17$fac)+
                    sum(mh20_tot.zmvm.19$fac)+sum(mh20_tot.zmvm.21$fac)+
                    sum(mh20_tot.zmvm.22$fac)+sum(mh20_tot.zmvm.23$fac)+
                    sum(mh20_tot.zmvm.24$fac)+sum(mh20_tot.zmvm.31$fac)+
                    sum(mh20_tot.zmvm.32$fac)+sum(mh20_tot.zmvm.33$fac)+
                    sum(mh20_tot.zmvm.34$fac)+sum(mh20_tot.zmvm.36$fac)+
                    sum(mh20_tot.zmvm.38$fac)+sum(mh20_tot.zmvm.39$fac)+
                    sum(mh20_tot.zmvm.41$fac)+sum(mh20_tot.zmvm.42$fac)+
                    sum(mh20_tot.zmvm.43$fac)+sum(mh20_tot.zmvm.44$fac)+
                    sum(mh20_tot.zmvm.46$fac)+sum(mh20_tot.zmvm.47$fac)+
                    sum(mh20_tot.zmvm.49$fac)
pot20mh_tot.zmvm==pot20corr_tot.zmvm 
      # Calculo de trabajadores por categoria (formal/informal)
cpotsub20_tot.zmvm <- mutate_at(cpotsub20_tot.zmvm, c('p3m4'), 
                                ~replace(., is.na(.), 0))
cpotsub20_tot.zmvm$emp_ppal.amp <- ifelse(cpotsub20_tot.zmvm$p3m4==0|
                                          cpotsub20_tot.zmvm$p6d==6, 1, 
                                          cpotsub20_tot.zmvm$emp_ppal.amp)
cpotsubinf20_tot.zmvm <- filter(cpotsub20_tot.zmvm, emp_ppal.amp==1)
cpotsubfor20_tot.zmvm <- filter(cpotsub20_tot.zmvm, emp_ppal.amp==2)
cpotinf20_tot.zmvm <- rbind(cpotsubinf20_tot.zmvm, mh20_tot.zmvm.5, 
                            mh20_tot.zmvm.17,mh20_tot.zmvm.19, mh20_tot.zmvm.39, 
                            mh20_tot.zmvm.47, mh20_tot.zmvm.49)
potinf20_tot.zmvm <- sum(cpotinf20_tot.zmvm$fac)
cpotfor20_tot.zmvm <- rbind(cpotsubfor20_tot.zmvm, mh20_tot.zmvm.36, 
                            mh20_tot.zmvm.38, mh20_tot.zmvm.46)
potfor20_tot.zmvm <- sum(cpotfor20_tot.zmvm$fac)
pot20corr_tot.zmvm==potinf20_tot.zmvm+potfor20_tot.zmvm
potinf20_tot.zmvm/pot20corr_tot.zmvm
potfor20_tot.zmvm/pot20corr_tot.zmvm
      # Calculo de celdas de la Matriz Hussmanns con base en la 
      # definicion ampliada
mh20.amp_tot.zmvm.11 <- filter(cpotsubinf20_tot.zmvm, mh_fil2==1&mh_col==1)
mh20.amp_tot.zmvm.12 <- filter(cpotsubfor20_tot.zmvm, mh_fil2==1&mh_col==1)
mh20.amp_tot.zmvm.13 <- filter(cpotsubinf20_tot.zmvm, mh_fil2==1&mh_col==3)
mh20.amp_tot.zmvm.14 <- filter(cpotsubfor20_tot.zmvm, mh_fil2==1&mh_col==3)
mh20.amp_tot.zmvm.21 <- filter(cpotsubinf20_tot.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_tot.zmvm.22 <- filter(cpotsubfor20_tot.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_tot.zmvm.23 <- filter(cpotsubinf20_tot.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_tot.zmvm.24 <- filter(cpotsubfor20_tot.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_tot.zmvm.31 <- filter(cpotsubinf20_tot.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_tot.zmvm.32 <- filter(cpotsubfor20_tot.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_tot.zmvm.33 <- filter(cpotsubinf20_tot.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_tot.zmvm.34 <- filter(cpotsubfor20_tot.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_tot.zmvm.41 <- filter(cpotsubinf20_tot.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_tot.zmvm.42 <- filter(cpotsubfor20_tot.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_tot.zmvm.43 <- filter(cpotsubinf20_tot.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_tot.zmvm.44 <- filter(cpotsubfor20_tot.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
sum(mh20.amp_tot.zmvm.11$fac)  # Asalariados informales del sector informal
sum(mh20.amp_tot.zmvm.12$fac)  # Asalariados formales del sector informal
sum(mh20.amp_tot.zmvm.13$fac)  # No asalariados informales del sector informal
sum(mh20.amp_tot.zmvm.14$fac)  # No asalariados formales del sector informal
sum(mh20_tot.zmvm.5$fac) # Empleadores informales
sum(mh20_tot.zmvm.17$fac)  # Cuentapropistas del sector informal
sum(mh20_tot.zmvm.19$fac)  # No remunerados del sector informal
sum(mh20.amp_tot.zmvm.21$fac)  # Asalariados informales en trabajo del hogar
sum(mh20.amp_tot.zmvm.22$fac)  # Asalariados formales en trabajo del hogar
sum(mh20.amp_tot.zmvm.23$fac)  # No asalariados informales en trabajo del hogar
sum(mh20.amp_tot.zmvm.24$fac)  # No asalariados formales en trabajo del hogar
sum(mh20.amp_tot.zmvm.31$fac)  # Asalariados informales fuera del sector informal
sum(mh20.amp_tot.zmvm.32$fac)  
        # Asalariados formales fuera del sector informal
sum(mh20.amp_tot.zmvm.33$fac)  
        # No asalariados informales fuera del sector informal
sum(mh20.amp_tot.zmvm.34$fac)  
        # No asalariados formales fuera del sector informal
sum(mh20_tot.zmvm.36$fac)  # Empleadores fuera del sector informal
sum(mh20_tot.zmvm.38$fac)  # Cuentapropistas formales
sum(mh20_tot.zmvm.39$fac)  # No remunerados fuera del sector informal
sum(mh20.amp_tot.zmvm.41$fac)  # Asalariados informales del ambito agropecuario
sum(mh20.amp_tot.zmvm.42$fac)  # Asalariados formales del ambito agropecuario
sum(mh20.amp_tot.zmvm.43$fac)  
        # No asalariados informales del ambito agropecuario
sum(mh20.amp_tot.zmvm.44$fac)  # No asalariados formales del ambito agropecuario
sum(mh20_tot.zmvm.46$fac)  # Empleadores del ambito agropecuario
sum(mh20_tot.zmvm.47$fac)  # Cuentapropistas del ambito agropecuario
sum(mh20_tot.zmvm.49$fac)  # No remunerados del ambito agropecuario
pot20mh.amp_tot.zmvm <- sum(mh20.amp_tot.zmvm.11$fac)+
                        sum(mh20.amp_tot.zmvm.12$fac)+
                        sum(mh20.amp_tot.zmvm.13$fac)+
                        sum(mh20.amp_tot.zmvm.14$fac)+sum(mh20_tot.zmvm.5$fac)+
                        sum(mh20_tot.zmvm.17$fac)+sum(mh20_tot.zmvm.19$fac)+
                        sum(mh20.amp_tot.zmvm.21$fac)+
                        sum(mh20.amp_tot.zmvm.22$fac)+
                        sum(mh20.amp_tot.zmvm.23$fac)+
                        sum(mh20.amp_tot.zmvm.24$fac)+
                        sum(mh20.amp_tot.zmvm.31$fac)+
                        sum(mh20.amp_tot.zmvm.32$fac)+
                        sum(mh20.amp_tot.zmvm.33$fac)+
                        sum(mh20.amp_tot.zmvm.34$fac)+
                        sum(mh20_tot.zmvm.36$fac)+
                        sum(mh20_tot.zmvm.38$fac)+sum(mh20_tot.zmvm.39$fac)+
                        sum(mh20.amp_tot.zmvm.41$fac)+
                        sum(mh20.amp_tot.zmvm.42$fac)+
                        sum(mh20.amp_tot.zmvm.43$fac)+
                        sum(mh20.amp_tot.zmvm.44$fac)+
                        sum(mh20_tot.zmvm.46$fac)+sum(mh20_tot.zmvm.47$fac)+
                        sum(mh20_tot.zmvm.49$fac)
pot20mh.amp_tot.zmvm==pot20corr_tot.zmvm
# ------------------------------------------------------------------------------
  # Clasificacion del empleo en formal e informal segun definicion ampliada 
    # Escala ZMVM, total de jovenes 2005
      # Primer tratamiento
cpot05_jov.zmvm <- cpeaoc05_jov.zmvm
cpot05_jov.zmvm$emp_ppal.amp <- 0
cpotsub05_jov.zmvm <- filter(cpot05_jov.zmvm, mh_col<=4&p6d!=9)
cpotsub05_jov.zmvm$emp_ppal.amp <- 2
cpotnosub05_jov.zmvm <- filter(cpot05_jov.zmvm, mh_col>=5)
cpot05corr_jov.zmvm <- rbind(cpotsub05_jov.zmvm, cpotnosub05_jov.zmvm)
pot05corr_jov.zmvm <- sum(cpot05corr_jov.zmvm$fac) # POT corregida
pot05corr_jov.zmvm==sum(cpotsub05_jov.zmvm$fac)+sum(cpotnosub05_jov.zmvm$fac)
pot05_jov.zmvm <- sum(cpot05_jov.zmvm$fac)
pot05corr_jov.zmvm/pot05_jov.zmvm # 99.6 % del total
      # Calculo de celdas de la Matriz Hussmanns (POT corregida)                 
mh05_jov.zmvm.11 <- filter(cpot05corr_jov.zmvm, mh_fil2==1&mh_col==1)
mh05_jov.zmvm.13 <- filter(cpot05corr_jov.zmvm, mh_fil2==1&mh_col==3)
mh05_jov.zmvm.5  <- filter(cpot05corr_jov.zmvm, mh_col==5)
mh05_jov.zmvm.17 <- filter(cpot05corr_jov.zmvm, mh_fil2==1&mh_col==7)                      
mh05_jov.zmvm.19 <- filter(cpot05corr_jov.zmvm, mh_fil2==1&mh_col==9)
mh05_jov.zmvm.21 <- filter(cpot05corr_jov.zmvm, mh_fil2==2&mh_col==1)
mh05_jov.zmvm.22 <- filter(cpot05corr_jov.zmvm, mh_fil2==2&mh_col==2)
mh05_jov.zmvm.23 <- filter(cpot05corr_jov.zmvm, mh_fil2==2&mh_col==3)
mh05_jov.zmvm.24 <- filter(cpot05corr_jov.zmvm, mh_fil2==2&mh_col==4)
mh05_jov.zmvm.31 <- filter(cpot05corr_jov.zmvm, mh_fil2==3&mh_col==1)
mh05_jov.zmvm.32 <- filter(cpot05corr_jov.zmvm, mh_fil2==3&mh_col==2)
mh05_jov.zmvm.33 <- filter(cpot05corr_jov.zmvm, mh_fil2==3&mh_col==3)
mh05_jov.zmvm.34 <- filter(cpot05corr_jov.zmvm, mh_fil2==3&mh_col==4)
mh05_jov.zmvm.36 <- filter(cpot05corr_jov.zmvm, mh_fil2==3&mh_col==6)
mh05_jov.zmvm.38 <- filter(cpot05corr_jov.zmvm, mh_fil2==3&mh_col==8)
mh05_jov.zmvm.39 <- filter(cpot05corr_jov.zmvm, mh_fil2==3&mh_col==9)
mh05_jov.zmvm.41 <- filter(cpot05corr_jov.zmvm, mh_fil2==4&mh_col==1)
mh05_jov.zmvm.42 <- filter(cpot05corr_jov.zmvm, mh_fil2==4&mh_col==2)
mh05_jov.zmvm.43 <- filter(cpot05corr_jov.zmvm, mh_fil2==4&mh_col==3)
mh05_jov.zmvm.44 <- filter(cpot05corr_jov.zmvm, mh_fil2==4&mh_col==4)
mh05_jov.zmvm.46 <- filter(cpot05corr_jov.zmvm, mh_fil2==4&mh_col==6)
mh05_jov.zmvm.47 <- filter(cpot05corr_jov.zmvm, mh_fil2==4&mh_col==7)
mh05_jov.zmvm.49 <- filter(cpot05corr_jov.zmvm, mh_fil2==4&mh_col==9)
pot05mh_jov.zmvm <- sum(mh05_jov.zmvm.11$fac)+sum(mh05_jov.zmvm.13$fac)+
                    sum(mh05_jov.zmvm.5$fac)+sum(mh05_jov.zmvm.17$fac)+
                    sum(mh05_jov.zmvm.19$fac)+sum(mh05_jov.zmvm.21$fac)+
                    sum(mh05_jov.zmvm.22$fac)+sum(mh05_jov.zmvm.23$fac)+
                    sum(mh05_jov.zmvm.24$fac)+sum(mh05_jov.zmvm.31$fac)+
                    sum(mh05_jov.zmvm.32$fac)+sum(mh05_jov.zmvm.33$fac)+
                    sum(mh05_jov.zmvm.34$fac)+sum(mh05_jov.zmvm.36$fac)+
                    sum(mh05_jov.zmvm.38$fac)+sum(mh05_jov.zmvm.39$fac)+
                    sum(mh05_jov.zmvm.41$fac)+sum(mh05_jov.zmvm.42$fac)+
                    sum(mh05_jov.zmvm.43$fac)+sum(mh05_jov.zmvm.44$fac)+
                    sum(mh05_jov.zmvm.46$fac)+sum(mh05_jov.zmvm.47$fac)+
                    sum(mh05_jov.zmvm.49$fac)
pot05mh_jov.zmvm==pot05corr_jov.zmvm 
      # Calculo de trabajadores por categoria (formal/informal)
cpotsub05_jov.zmvm <- mutate_at(cpotsub05_jov.zmvm, c('p3m4'), 
                                ~replace(., is.na(.), 0))
cpotsub05_jov.zmvm$emp_ppal.amp <- ifelse(cpotsub05_jov.zmvm$p3m4==0|
                                          cpotsub05_jov.zmvm$p6d==6, 1, 
                                          cpotsub05_jov.zmvm$emp_ppal.amp)
cpotsubinf05_jov.zmvm <- filter(cpotsub05_jov.zmvm, emp_ppal.amp==1)
cpotsubfor05_jov.zmvm <- filter(cpotsub05_jov.zmvm, emp_ppal.amp==2)
cpotinf05_jov.zmvm <- rbind(cpotsubinf05_jov.zmvm, mh05_jov.zmvm.5, 
                            mh05_jov.zmvm.17,mh05_jov.zmvm.19, mh05_jov.zmvm.39, 
                            mh05_jov.zmvm.47, mh05_jov.zmvm.49)
potinf05_jov.zmvm <- sum(cpotinf05_jov.zmvm$fac)
cpotfor05_jov.zmvm <- rbind(cpotsubfor05_jov.zmvm, mh05_jov.zmvm.36, 
                            mh05_jov.zmvm.38, mh05_jov.zmvm.46)
potfor05_jov.zmvm <- sum(cpotfor05_jov.zmvm$fac)
pot05corr_jov.zmvm==potinf05_jov.zmvm+potfor05_jov.zmvm
cpotpt05_jov.zmvm <- rbind(cpotinf05_jov.zmvm, cpotfor05_jov.zmvm)
cpotpt05_jov.zmvm$emp_ppal.amp <- ifelse(cpotpt05_jov.zmvm$emp_ppal.amp==0, 
                                         cpotpt05_jov.zmvm$emp_ppal, 
                                         cpotpt05_jov.zmvm$emp_ppal.amp)
potpt05_jov.zmvm <- sum(cpotpt05_jov.zmvm$fac)
potinf05_jov.zmvm/potpt05_jov.zmvm
potfor05_jov.zmvm/potpt05_jov.zmvm
      # Calculo de celdas de la Matriz Hussmanns con base en la 
      # definicion ampliada
mh05.amp_jov.zmvm.11 <- filter(cpotsubinf05_jov.zmvm, mh_fil2==1&mh_col==1)
mh05.amp_jov.zmvm.12 <- filter(cpotsubfor05_jov.zmvm, mh_fil2==1&mh_col==1)
mh05.amp_jov.zmvm.13 <- filter(cpotsubinf05_jov.zmvm, mh_fil2==1&mh_col==3)
mh05.amp_jov.zmvm.14 <- filter(cpotsubfor05_jov.zmvm, mh_fil2==1&mh_col==3)
mh05.amp_jov.zmvm.21 <- filter(cpotsubinf05_jov.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jov.zmvm.22 <- filter(cpotsubfor05_jov.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jov.zmvm.23 <- filter(cpotsubinf05_jov.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jov.zmvm.24 <- filter(cpotsubfor05_jov.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jov.zmvm.31 <- filter(cpotsubinf05_jov.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jov.zmvm.32 <- filter(cpotsubfor05_jov.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jov.zmvm.33 <- filter(cpotsubinf05_jov.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jov.zmvm.34 <- filter(cpotsubfor05_jov.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jov.zmvm.41 <- filter(cpotsubinf05_jov.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jov.zmvm.42 <- filter(cpotsubfor05_jov.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jov.zmvm.43 <- filter(cpotsubinf05_jov.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jov.zmvm.44 <- filter(cpotsubfor05_jov.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
sum(mh05.amp_jov.zmvm.11$fac)  # Asalariados informales del sector informal
sum(mh05.amp_jov.zmvm.12$fac)  # Asalariados formales del sector informal
sum(mh05.amp_jov.zmvm.13$fac)  # No asalariados informales del sector informal
sum(mh05.amp_jov.zmvm.14$fac)  # No asalariados formales del sector informal
sum(mh05_jov.zmvm.5$fac) # Empleadores informales
sum(mh05_jov.zmvm.17$fac)  # Cuentapropistas del sector informal
sum(mh05_jov.zmvm.19$fac)  # No remunerados del sector informal
sum(mh05.amp_jov.zmvm.21$fac)  # Asalariados informales en trabajo del hogar
sum(mh05.amp_jov.zmvm.22$fac)  # Asalariados formales en trabajo del hogar
sum(mh05.amp_jov.zmvm.23$fac)  # No asalariados informales en trabajo del hogar
sum(mh05.amp_jov.zmvm.24$fac)  # No asalariados formales en trabajo del hogar
sum(mh05.amp_jov.zmvm.31$fac)  
        # Asalariados informales fuera del sector informal
sum(mh05.amp_jov.zmvm.32$fac)  
        # Asalariados formales fuera del sector informal
sum(mh05.amp_jov.zmvm.33$fac)  
        # No asalariados informales fuera del sector informal
sum(mh05.amp_jov.zmvm.34$fac)  
        # No asalariados formales fuera del sector informal
sum(mh05_jov.zmvm.36$fac)  # Empleadores fuera del sector informal
sum(mh05_jov.zmvm.38$fac)  # Cuentapropistas formales
sum(mh05_jov.zmvm.39$fac)  # No remunerados fuera del sector informal
sum(mh05.amp_jov.zmvm.41$fac)  # Asalariados informales del ambito agropecuario
sum(mh05.amp_jov.zmvm.42$fac)  # Asalariados formales del ambito agropecuario
sum(mh05.amp_jov.zmvm.43$fac)  
        # No asalariados informales del ambito agropecuario
sum(mh05.amp_jov.zmvm.44$fac)  # No asalariados formales del ambito agropecuario
sum(mh05_jov.zmvm.46$fac)  # Empleadores del ambito agropecuario
sum(mh05_jov.zmvm.47$fac)  # Cuentapropistas del ambito agropecuario
sum(mh05_jov.zmvm.49$fac)  # No remunerados del ambito agropecuario
pot05mh.amp_jov.zmvm <- sum(mh05.amp_jov.zmvm.11$fac)+
                        sum(mh05.amp_jov.zmvm.12$fac)+
                        sum(mh05.amp_jov.zmvm.13$fac)+
                        sum(mh05.amp_jov.zmvm.14$fac)+sum(mh05_jov.zmvm.5$fac)+
                        sum(mh05_jov.zmvm.17$fac)+sum(mh05_jov.zmvm.19$fac)+
                        sum(mh05.amp_jov.zmvm.21$fac)+
                        sum(mh05.amp_jov.zmvm.22$fac)+
                        sum(mh05.amp_jov.zmvm.23$fac)+
                        sum(mh05.amp_jov.zmvm.24$fac)+
                        sum(mh05.amp_jov.zmvm.31$fac)+
                        sum(mh05.amp_jov.zmvm.32$fac)+
                        sum(mh05.amp_jov.zmvm.33$fac)+
                        sum(mh05.amp_jov.zmvm.34$fac)+
                        sum(mh05_jov.zmvm.36$fac)+
                        sum(mh05_jov.zmvm.38$fac)+sum(mh05_jov.zmvm.39$fac)+
                        sum(mh05.amp_jov.zmvm.41$fac)+
                        sum(mh05.amp_jov.zmvm.42$fac)+
                        sum(mh05.amp_jov.zmvm.43$fac)+
                        sum(mh05.amp_jov.zmvm.44$fac)+
                        sum(mh05_jov.zmvm.46$fac)+sum(mh05_jov.zmvm.47$fac)+
                        sum(mh05_jov.zmvm.49$fac)
pot05mh.amp_jov.zmvm==pot05corr_jov.zmvm
    # Escala ZMVM, total de jovenes 2020
      # Primer tratamiento
cpot20_jov.zmvm <- cpeaoc20_jov.zmvm
cpot20_jov.zmvm$emp_ppal.amp <- 0
cpotsub20_jov.zmvm <- filter(cpot20_jov.zmvm, mh_col<=4&p6d!=9)
cpotsub20_jov.zmvm$emp_ppal.amp <- 2
cpotnosub20_jov.zmvm <- filter(cpot20_jov.zmvm, mh_col>=5)
cpot20corr_jov.zmvm <- rbind(cpotsub20_jov.zmvm, cpotnosub20_jov.zmvm)
pot20corr_jov.zmvm <- sum(cpot20corr_jov.zmvm$fac) # POT corregida
pot20corr_jov.zmvm==sum(cpotsub20_jov.zmvm$fac)+sum(cpotnosub20_jov.zmvm$fac)
pot20_jov.zmvm <- sum(cpot20_jov.zmvm$fac)
pot20corr_jov.zmvm/pot20_jov.zmvm # 99.9 % del total
      # Calculo de celdas de la Matriz Hussmanns (POT corregida)                 
mh20_jov.zmvm.11 <- filter(cpot20corr_jov.zmvm, mh_fil2==1&mh_col==1)
mh20_jov.zmvm.13 <- filter(cpot20corr_jov.zmvm, mh_fil2==1&mh_col==3)
mh20_jov.zmvm.5  <- filter(cpot20corr_jov.zmvm, mh_col==5)
mh20_jov.zmvm.17 <- filter(cpot20corr_jov.zmvm, mh_fil2==1&mh_col==7)                      
mh20_jov.zmvm.19 <- filter(cpot20corr_jov.zmvm, mh_fil2==1&mh_col==9)
mh20_jov.zmvm.21 <- filter(cpot20corr_jov.zmvm, mh_fil2==2&mh_col==1)
mh20_jov.zmvm.22 <- filter(cpot20corr_jov.zmvm, mh_fil2==2&mh_col==2)
mh20_jov.zmvm.23 <- filter(cpot20corr_jov.zmvm, mh_fil2==2&mh_col==3)
mh20_jov.zmvm.24 <- filter(cpot20corr_jov.zmvm, mh_fil2==2&mh_col==4)
mh20_jov.zmvm.31 <- filter(cpot20corr_jov.zmvm, mh_fil2==3&mh_col==1)
mh20_jov.zmvm.32 <- filter(cpot20corr_jov.zmvm, mh_fil2==3&mh_col==2)
mh20_jov.zmvm.33 <- filter(cpot20corr_jov.zmvm, mh_fil2==3&mh_col==3)
mh20_jov.zmvm.34 <- filter(cpot20corr_jov.zmvm, mh_fil2==3&mh_col==4)
mh20_jov.zmvm.36 <- filter(cpot20corr_jov.zmvm, mh_fil2==3&mh_col==6)
mh20_jov.zmvm.38 <- filter(cpot20corr_jov.zmvm, mh_fil2==3&mh_col==8)
mh20_jov.zmvm.39 <- filter(cpot20corr_jov.zmvm, mh_fil2==3&mh_col==9)
mh20_jov.zmvm.41 <- filter(cpot20corr_jov.zmvm, mh_fil2==4&mh_col==1)
mh20_jov.zmvm.42 <- filter(cpot20corr_jov.zmvm, mh_fil2==4&mh_col==2)
mh20_jov.zmvm.43 <- filter(cpot20corr_jov.zmvm, mh_fil2==4&mh_col==3)
mh20_jov.zmvm.44 <- filter(cpot20corr_jov.zmvm, mh_fil2==4&mh_col==4)
mh20_jov.zmvm.46 <- filter(cpot20corr_jov.zmvm, mh_fil2==4&mh_col==6)
mh20_jov.zmvm.47 <- filter(cpot20corr_jov.zmvm, mh_fil2==4&mh_col==7)
mh20_jov.zmvm.49 <- filter(cpot20corr_jov.zmvm, mh_fil2==4&mh_col==9)
pot20mh_jov.zmvm <- sum(mh20_jov.zmvm.11$fac)+sum(mh20_jov.zmvm.13$fac)+
                    sum(mh20_jov.zmvm.5$fac)+sum(mh20_jov.zmvm.17$fac)+
                    sum(mh20_jov.zmvm.19$fac)+sum(mh20_jov.zmvm.21$fac)+
                    sum(mh20_jov.zmvm.22$fac)+sum(mh20_jov.zmvm.23$fac)+
                    sum(mh20_jov.zmvm.24$fac)+sum(mh20_jov.zmvm.31$fac)+
                    sum(mh20_jov.zmvm.32$fac)+sum(mh20_jov.zmvm.33$fac)+
                    sum(mh20_jov.zmvm.34$fac)+sum(mh20_jov.zmvm.36$fac)+
                    sum(mh20_jov.zmvm.38$fac)+sum(mh20_jov.zmvm.39$fac)+
                    sum(mh20_jov.zmvm.41$fac)+sum(mh20_jov.zmvm.42$fac)+
                    sum(mh20_jov.zmvm.43$fac)+sum(mh20_jov.zmvm.44$fac)+
                    sum(mh20_jov.zmvm.46$fac)+sum(mh20_jov.zmvm.47$fac)+
                    sum(mh20_jov.zmvm.49$fac)
pot20mh_jov.zmvm==pot20corr_jov.zmvm 
      # Calculo de trabajadores por categoria (formal/informal)
cpotsub20_jov.zmvm <- mutate_at(cpotsub20_jov.zmvm, c('p3m4'), 
                                ~replace(., is.na(.), 0))
cpotsub20_jov.zmvm$emp_ppal.amp <- ifelse(cpotsub20_jov.zmvm$p3m4==0|
                                          cpotsub20_jov.zmvm$p6d==6, 1, 
                                          cpotsub20_jov.zmvm$emp_ppal.amp)
cpotsubinf20_jov.zmvm <- filter(cpotsub20_jov.zmvm, emp_ppal.amp==1)
cpotsubfor20_jov.zmvm <- filter(cpotsub20_jov.zmvm, emp_ppal.amp==2)
cpotinf20_jov.zmvm <- rbind(cpotsubinf20_jov.zmvm, mh20_jov.zmvm.5, 
                            mh20_jov.zmvm.17,mh20_jov.zmvm.19, mh20_jov.zmvm.39, 
                            mh20_jov.zmvm.47, mh20_jov.zmvm.49)
potinf20_jov.zmvm <- sum(cpotinf20_jov.zmvm$fac)
cpotfor20_jov.zmvm <- rbind(cpotsubfor20_jov.zmvm, mh20_jov.zmvm.36, 
                            mh20_jov.zmvm.38, mh20_jov.zmvm.46)
potfor20_jov.zmvm <- sum(cpotfor20_jov.zmvm$fac)
pot20corr_jov.zmvm==potinf20_jov.zmvm+potfor20_jov.zmvm
cpotpt20_jov.zmvm <- rbind(cpotinf20_jov.zmvm, cpotfor20_jov.zmvm)
cpotpt20_jov.zmvm$emp_ppal.amp <- ifelse(cpotpt20_jov.zmvm$emp_ppal.amp==0, 
                                         cpotpt20_jov.zmvm$emp_ppal, 
                                         cpotpt20_jov.zmvm$emp_ppal.amp)
potpt20_jov.zmvm <- sum(cpotpt20_jov.zmvm$fac)
potinf20_jov.zmvm/potpt20_jov.zmvm
potfor20_jov.zmvm/potpt20_jov.zmvm
      # Calculo de celdas de la Matriz Hussmanns con base en la 
      # definicion ampliada
mh20.amp_jov.zmvm.11 <- filter(cpotsubinf20_jov.zmvm, mh_fil2==1&mh_col==1)
mh20.amp_jov.zmvm.12 <- filter(cpotsubfor20_jov.zmvm, mh_fil2==1&mh_col==1)
mh20.amp_jov.zmvm.13 <- filter(cpotsubinf20_jov.zmvm, mh_fil2==1&mh_col==3)
mh20.amp_jov.zmvm.14 <- filter(cpotsubfor20_jov.zmvm, mh_fil2==1&mh_col==3)
mh20.amp_jov.zmvm.21 <- filter(cpotsubinf20_jov.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jov.zmvm.22 <- filter(cpotsubfor20_jov.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jov.zmvm.23 <- filter(cpotsubinf20_jov.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jov.zmvm.24 <- filter(cpotsubfor20_jov.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jov.zmvm.31 <- filter(cpotsubinf20_jov.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jov.zmvm.32 <- filter(cpotsubfor20_jov.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jov.zmvm.33 <- filter(cpotsubinf20_jov.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jov.zmvm.34 <- filter(cpotsubfor20_jov.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jov.zmvm.41 <- filter(cpotsubinf20_jov.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jov.zmvm.42 <- filter(cpotsubfor20_jov.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jov.zmvm.43 <- filter(cpotsubinf20_jov.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jov.zmvm.44 <- filter(cpotsubfor20_jov.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
sum(mh20.amp_jov.zmvm.11$fac)  # Asalariados informales del sector informal
sum(mh20.amp_jov.zmvm.12$fac)  # Asalariados formales del sector informal
sum(mh20.amp_jov.zmvm.13$fac)  # No asalariados informales del sector informal
sum(mh20.amp_jov.zmvm.14$fac)  # No asalariados formales del sector informal
sum(mh20_jov.zmvm.5$fac) # Empleadores informales
sum(mh20_jov.zmvm.17$fac)  # Cuentapropistas del sector informal
sum(mh20_jov.zmvm.19$fac)  # No remunerados del sector informal
sum(mh20.amp_jov.zmvm.21$fac)  # Asalariados informales en trabajo del hogar
sum(mh20.amp_jov.zmvm.22$fac)  # Asalariados formales en trabajo del hogar
sum(mh20.amp_jov.zmvm.23$fac)  # No asalariados informales en trabajo del hogar
sum(mh20.amp_jov.zmvm.24$fac)  # No asalariados formales en trabajo del hogar
sum(mh20.amp_jov.zmvm.31$fac)  
        # Asalariados informales fuera del sector informal
sum(mh20.amp_jov.zmvm.32$fac)  
        # Asalariados formales fuera del sector informal
sum(mh20.amp_jov.zmvm.33$fac)  
        # No asalariados informales fuera del sector informal
sum(mh20.amp_jov.zmvm.34$fac)  
        # No asalariados formales fuera del sector informal
sum(mh20_jov.zmvm.36$fac)  # Empleadores fuera del sector informal
sum(mh20_jov.zmvm.38$fac)  # Cuentapropistas formales
sum(mh20_jov.zmvm.39$fac)  # No remunerados fuera del sector informal
sum(mh20.amp_jov.zmvm.41$fac)  # Asalariados informales del ambito agropecuario
sum(mh20.amp_jov.zmvm.42$fac)  # Asalariados formales del ambito agropecuario
sum(mh20.amp_jov.zmvm.43$fac)  
        # No asalariados informales del ambito agropecuario
sum(mh20.amp_jov.zmvm.44$fac)  # No asalariados formales del ambito agropecuario
sum(mh20_jov.zmvm.46$fac)  # Empleadores del ambito agropecuario
sum(mh20_jov.zmvm.47$fac)  # Cuentapropistas del ambito agropecuario
sum(mh20_jov.zmvm.49$fac)  # No remunerados del ambito agropecuario
pot20mh.amp_jov.zmvm <- sum(mh20.amp_jov.zmvm.11$fac)+
                        sum(mh20.amp_jov.zmvm.12$fac)+
                        sum(mh20.amp_jov.zmvm.13$fac)+
                        sum(mh20.amp_jov.zmvm.14$fac)+sum(mh20_jov.zmvm.5$fac)+
                        sum(mh20_jov.zmvm.17$fac)+sum(mh20_jov.zmvm.19$fac)+
                        sum(mh20.amp_jov.zmvm.21$fac)+
                        sum(mh20.amp_jov.zmvm.22$fac)+
                        sum(mh20.amp_jov.zmvm.23$fac)+
                        sum(mh20.amp_jov.zmvm.24$fac)+
                        sum(mh20.amp_jov.zmvm.31$fac)+
                        sum(mh20.amp_jov.zmvm.32$fac)+
                        sum(mh20.amp_jov.zmvm.33$fac)+
                        sum(mh20.amp_jov.zmvm.34$fac)+
                        sum(mh20_jov.zmvm.36$fac)+
                        sum(mh20_jov.zmvm.38$fac)+sum(mh20_jov.zmvm.39$fac)+
                        sum(mh20.amp_jov.zmvm.41$fac)+
                        sum(mh20.amp_jov.zmvm.42$fac)+
                        sum(mh20.amp_jov.zmvm.43$fac)+
                        sum(mh20.amp_jov.zmvm.44$fac)+
                        sum(mh20_jov.zmvm.46$fac)+sum(mh20_jov.zmvm.47$fac)+
                        sum(mh20_jov.zmvm.49$fac)
pot20mh.amp_jov.zmvm==pot20corr_jov.zmvm
    # Escala ZMVM, total de jovenes mujeres 2005
      # Primer tratamiento  
cpot05_jvm.zmvm <- filter(cpeaoc05_jov.zmvm, sex==2)
cpot05_jvm.zmvm$emp_ppal.amp <- 0
cpotsub05_jvm.zmvm <- filter(cpot05_jvm.zmvm, mh_col<=4&p6d!=9)
cpotsub05_jvm.zmvm$emp_ppal.amp <- 2
cpotnosub05_jvm.zmvm <- filter(cpot05_jvm.zmvm, mh_col>=5)
cpot05corr_jvm.zmvm <- rbind(cpotsub05_jvm.zmvm, cpotnosub05_jvm.zmvm)
pot05corr_jvm.zmvm <- sum(cpot05corr_jvm.zmvm$fac) # POT corregida
pot05corr_jvm.zmvm==sum(cpotsub05_jvm.zmvm$fac)+sum(cpotnosub05_jvm.zmvm$fac)
pot05_jvm.zmvm <- sum(cpot05_jvm.zmvm$fac)
pot05corr_jvm.zmvm/pot05_jvm.zmvm # 99.5 % del total
      # Calculo de celdas de la Matriz Hussmanns (POT corregida)                
mh05_jvm.zmvm.11 <- filter(cpot05corr_jvm.zmvm, mh_fil2==1&mh_col==1)
mh05_jvm.zmvm.13 <- filter(cpot05corr_jvm.zmvm, mh_fil2==1&mh_col==3)
mh05_jvm.zmvm.5  <- filter(cpot05corr_jvm.zmvm, mh_col==5)
mh05_jvm.zmvm.17 <- filter(cpot05corr_jvm.zmvm, mh_fil2==1&mh_col==7)                      
mh05_jvm.zmvm.19 <- filter(cpot05corr_jvm.zmvm, mh_fil2==1&mh_col==9)
mh05_jvm.zmvm.21 <- filter(cpot05corr_jvm.zmvm, mh_fil2==2&mh_col==1)
mh05_jvm.zmvm.22 <- filter(cpot05corr_jvm.zmvm, mh_fil2==2&mh_col==2)
mh05_jvm.zmvm.23 <- filter(cpot05corr_jvm.zmvm, mh_fil2==2&mh_col==3)
mh05_jvm.zmvm.24 <- filter(cpot05corr_jvm.zmvm, mh_fil2==2&mh_col==4)
mh05_jvm.zmvm.31 <- filter(cpot05corr_jvm.zmvm, mh_fil2==3&mh_col==1)
mh05_jvm.zmvm.32 <- filter(cpot05corr_jvm.zmvm, mh_fil2==3&mh_col==2)
mh05_jvm.zmvm.33 <- filter(cpot05corr_jvm.zmvm, mh_fil2==3&mh_col==3)
mh05_jvm.zmvm.34 <- filter(cpot05corr_jvm.zmvm, mh_fil2==3&mh_col==4)
mh05_jvm.zmvm.36 <- filter(cpot05corr_jvm.zmvm, mh_fil2==3&mh_col==6)
mh05_jvm.zmvm.38 <- filter(cpot05corr_jvm.zmvm, mh_fil2==3&mh_col==8)
mh05_jvm.zmvm.39 <- filter(cpot05corr_jvm.zmvm, mh_fil2==3&mh_col==9)
mh05_jvm.zmvm.41 <- filter(cpot05corr_jvm.zmvm, mh_fil2==4&mh_col==1)
mh05_jvm.zmvm.42 <- filter(cpot05corr_jvm.zmvm, mh_fil2==4&mh_col==2)
mh05_jvm.zmvm.43 <- filter(cpot05corr_jvm.zmvm, mh_fil2==4&mh_col==3)
mh05_jvm.zmvm.44 <- filter(cpot05corr_jvm.zmvm, mh_fil2==4&mh_col==4)
mh05_jvm.zmvm.46 <- filter(cpot05corr_jvm.zmvm, mh_fil2==4&mh_col==6)
mh05_jvm.zmvm.47 <- filter(cpot05corr_jvm.zmvm, mh_fil2==4&mh_col==7)
mh05_jvm.zmvm.49 <- filter(cpot05corr_jvm.zmvm, mh_fil2==4&mh_col==9)
pot05mh_jvm.zmvm <- sum(mh05_jvm.zmvm.11$fac)+sum(mh05_jvm.zmvm.13$fac)+
                    sum(mh05_jvm.zmvm.5$fac)+sum(mh05_jvm.zmvm.17$fac)+
                    sum(mh05_jvm.zmvm.19$fac)+sum(mh05_jvm.zmvm.21$fac)+
                    sum(mh05_jvm.zmvm.22$fac)+sum(mh05_jvm.zmvm.23$fac)+
                    sum(mh05_jvm.zmvm.24$fac)+sum(mh05_jvm.zmvm.31$fac)+
                    sum(mh05_jvm.zmvm.32$fac)+sum(mh05_jvm.zmvm.33$fac)+
                    sum(mh05_jvm.zmvm.34$fac)+sum(mh05_jvm.zmvm.36$fac)+
                    sum(mh05_jvm.zmvm.38$fac)+sum(mh05_jvm.zmvm.39$fac)+
                    sum(mh05_jvm.zmvm.41$fac)+sum(mh05_jvm.zmvm.42$fac)+
                    sum(mh05_jvm.zmvm.43$fac)+sum(mh05_jvm.zmvm.44$fac)+
                    sum(mh05_jvm.zmvm.46$fac)+sum(mh05_jvm.zmvm.47$fac)+
                    sum(mh05_jvm.zmvm.49$fac)
pot05mh_jvm.zmvm==pot05corr_jvm.zmvm 
      # Calculo de trabajadores por categoria (formal/informal)
cpotsub05_jvm.zmvm <- mutate_at(cpotsub05_jvm.zmvm, c('p3m4'), 
                                ~replace(., is.na(.), 0))
cpotsub05_jvm.zmvm$emp_ppal.amp <- ifelse(cpotsub05_jvm.zmvm$p3m4==0|
                                          cpotsub05_jvm.zmvm$p6d==6, 1, 
                                          cpotsub05_jvm.zmvm$emp_ppal.amp)
cpotsubinf05_jvm.zmvm <- filter(cpotsub05_jvm.zmvm, emp_ppal.amp==1)
cpotsubfor05_jvm.zmvm <- filter(cpotsub05_jvm.zmvm, emp_ppal.amp==2)
cpotinf05_jvm.zmvm <- rbind(cpotsubinf05_jvm.zmvm, mh05_jvm.zmvm.5, 
                            mh05_jvm.zmvm.17,mh05_jvm.zmvm.19, mh05_jvm.zmvm.39, 
                            mh05_jvm.zmvm.47, mh05_jvm.zmvm.49)
potinf05_jvm.zmvm <- sum(cpotinf05_jvm.zmvm$fac)
cpotfor05_jvm.zmvm <- rbind(cpotsubfor05_jvm.zmvm, mh05_jvm.zmvm.36, 
                            mh05_jvm.zmvm.38, mh05_jvm.zmvm.46)
potfor05_jvm.zmvm <- sum(cpotfor05_jvm.zmvm$fac)
pot05corr_jvm.zmvm==potinf05_jvm.zmvm+potfor05_jvm.zmvm
potinf05_jvm.zmvm/pot05corr_jvm.zmvm
potfor05_jvm.zmvm/pot05corr_jvm.zmvm
      # Calculo de celdas de la Matriz Hussmanns con base en la 
      # definicion ampliada
mh05.amp_jvm.zmvm.11 <- filter(cpotsubinf05_jvm.zmvm, mh_fil2==1&mh_col==1)
mh05.amp_jvm.zmvm.12 <- filter(cpotsubfor05_jvm.zmvm, mh_fil2==1&mh_col==1)
mh05.amp_jvm.zmvm.13 <- filter(cpotsubinf05_jvm.zmvm, mh_fil2==1&mh_col==3)
mh05.amp_jvm.zmvm.14 <- filter(cpotsubfor05_jvm.zmvm, mh_fil2==1&mh_col==3)
mh05.amp_jvm.zmvm.21 <- filter(cpotsubinf05_jvm.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jvm.zmvm.22 <- filter(cpotsubfor05_jvm.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jvm.zmvm.23 <- filter(cpotsubinf05_jvm.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jvm.zmvm.24 <- filter(cpotsubfor05_jvm.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jvm.zmvm.31 <- filter(cpotsubinf05_jvm.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jvm.zmvm.32 <- filter(cpotsubfor05_jvm.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jvm.zmvm.33 <- filter(cpotsubinf05_jvm.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jvm.zmvm.34 <- filter(cpotsubfor05_jvm.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jvm.zmvm.41 <- filter(cpotsubinf05_jvm.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jvm.zmvm.42 <- filter(cpotsubfor05_jvm.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jvm.zmvm.43 <- filter(cpotsubinf05_jvm.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jvm.zmvm.44 <- filter(cpotsubfor05_jvm.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
sum(mh05.amp_jvm.zmvm.11$fac)  # Asalariados informales del sector informal
sum(mh05.amp_jvm.zmvm.12$fac)  # Asalariados formales del sector informal
sum(mh05.amp_jvm.zmvm.13$fac)  # No asalariados informales del sector informal
sum(mh05.amp_jvm.zmvm.14$fac)  # No asalariados formales del sector informal
sum(mh05_jvm.zmvm.5$fac) # Empleadores informales
sum(mh05_jvm.zmvm.17$fac)  # Cuentapropistas del sector informal
sum(mh05_jvm.zmvm.19$fac)  # No remunerados del sector informal
sum(mh05.amp_jvm.zmvm.21$fac)  # Asalariados informales en trabajo del hogar
sum(mh05.amp_jvm.zmvm.22$fac)  # Asalariados formales en trabajo del hogar
sum(mh05.amp_jvm.zmvm.23$fac)  # No asalariados informales en trabajo del hogar
sum(mh05.amp_jvm.zmvm.24$fac)  # No asalariados formales en trabajo del hogar
sum(mh05.amp_jvm.zmvm.31$fac)  
        # Asalariados informales fuera del sector informal
sum(mh05.amp_jvm.zmvm.32$fac)  
        # Asalariados formales fuera del sector informal
sum(mh05.amp_jvm.zmvm.33$fac)  
        # No asalariados informales fuera del sector informal
sum(mh05.amp_jvm.zmvm.34$fac)  
        # No asalariados formales fuera del sector informal
sum(mh05_jvm.zmvm.36$fac)  # Empleadores fuera del sector informal
sum(mh05_jvm.zmvm.38$fac)  # Cuentapropistas formales
sum(mh05_jvm.zmvm.39$fac)  # No remunerados fuera del sector informal
sum(mh05.amp_jvm.zmvm.41$fac)  # Asalariados informales del ambito agropecuario
sum(mh05.amp_jvm.zmvm.42$fac)  # Asalariados formales del ambito agropecuario
sum(mh05.amp_jvm.zmvm.43$fac)  
        # No asalariados informales del ambito agropecuario
sum(mh05.amp_jvm.zmvm.44$fac)  # No asalariados formales del ambito agropecuario
sum(mh05_jvm.zmvm.46$fac)  # Empleadores del ambito agropecuario
sum(mh05_jvm.zmvm.47$fac)  # Cuentapropistas del ambito agropecuario
sum(mh05_jvm.zmvm.49$fac)  # No remunerados del ambito agropecuario
pot05mh.amp_jvm.zmvm <- sum(mh05.amp_jvm.zmvm.11$fac)+
                        sum(mh05.amp_jvm.zmvm.12$fac)+
                        sum(mh05.amp_jvm.zmvm.13$fac)+
                        sum(mh05.amp_jvm.zmvm.14$fac)+sum(mh05_jvm.zmvm.5$fac)+
                        sum(mh05_jvm.zmvm.17$fac)+sum(mh05_jvm.zmvm.19$fac)+
                        sum(mh05.amp_jvm.zmvm.21$fac)+
                        sum(mh05.amp_jvm.zmvm.22$fac)+
                        sum(mh05.amp_jvm.zmvm.23$fac)+
                        sum(mh05.amp_jvm.zmvm.24$fac)+
                        sum(mh05.amp_jvm.zmvm.31$fac)+
                        sum(mh05.amp_jvm.zmvm.32$fac)+
                        sum(mh05.amp_jvm.zmvm.33$fac)+
                        sum(mh05.amp_jvm.zmvm.34$fac)+
                        sum(mh05_jvm.zmvm.36$fac)+
                        sum(mh05_jvm.zmvm.38$fac)+sum(mh05_jvm.zmvm.39$fac)+
                        sum(mh05.amp_jvm.zmvm.41$fac)+
                        sum(mh05.amp_jvm.zmvm.42$fac)+
                        sum(mh05.amp_jvm.zmvm.43$fac)+
                        sum(mh05.amp_jvm.zmvm.44$fac)+
                        sum(mh05_jvm.zmvm.46$fac)+sum(mh05_jvm.zmvm.47$fac)+
                        sum(mh05_jvm.zmvm.49$fac)
pot05mh.amp_jvm.zmvm==pot05corr_jvm.zmvm
    # Escala ZMVM, total de jovenes mujeres 2020
      # Primer tratamiento
cpot20_jvm.zmvm <- filter(cpeaoc20_jov.zmvm, sex==2)
cpot20_jvm.zmvm$emp_ppal.amp <- 0
cpotsub20_jvm.zmvm <- filter(cpot20_jvm.zmvm, mh_col<=4&p6d!=9)
cpotsub20_jvm.zmvm$emp_ppal.amp <- 2
cpotnosub20_jvm.zmvm <- filter(cpot20_jvm.zmvm, mh_col>=5)
cpot20corr_jvm.zmvm <- rbind(cpotsub20_jvm.zmvm, cpotnosub20_jvm.zmvm)
pot20corr_jvm.zmvm <- sum(cpot20corr_jvm.zmvm$fac) # POT corregida
pot20corr_jvm.zmvm==sum(cpotsub20_jvm.zmvm$fac)+sum(cpotnosub20_jvm.zmvm$fac)
pot20_jvm.zmvm <- sum(cpot20_jvm.zmvm$fac)
pot20corr_jvm.zmvm/pot20_jvm.zmvm # 100 % del total 
      # Calculo de celdas de la Matriz Hussmanns (POT corregida)                  
mh20_jvm.zmvm.11 <- filter(cpot20corr_jvm.zmvm, mh_fil2==1&mh_col==1)
mh20_jvm.zmvm.13 <- filter(cpot20corr_jvm.zmvm, mh_fil2==1&mh_col==3)
mh20_jvm.zmvm.5  <- filter(cpot20corr_jvm.zmvm, mh_col==5)
mh20_jvm.zmvm.17 <- filter(cpot20corr_jvm.zmvm, mh_fil2==1&mh_col==7)                      
mh20_jvm.zmvm.19 <- filter(cpot20corr_jvm.zmvm, mh_fil2==1&mh_col==9)
mh20_jvm.zmvm.21 <- filter(cpot20corr_jvm.zmvm, mh_fil2==2&mh_col==1)
mh20_jvm.zmvm.22 <- filter(cpot20corr_jvm.zmvm, mh_fil2==2&mh_col==2)
mh20_jvm.zmvm.23 <- filter(cpot20corr_jvm.zmvm, mh_fil2==2&mh_col==3)
mh20_jvm.zmvm.24 <- filter(cpot20corr_jvm.zmvm, mh_fil2==2&mh_col==4)
mh20_jvm.zmvm.31 <- filter(cpot20corr_jvm.zmvm, mh_fil2==3&mh_col==1)
mh20_jvm.zmvm.32 <- filter(cpot20corr_jvm.zmvm, mh_fil2==3&mh_col==2)
mh20_jvm.zmvm.33 <- filter(cpot20corr_jvm.zmvm, mh_fil2==3&mh_col==3)
mh20_jvm.zmvm.34 <- filter(cpot20corr_jvm.zmvm, mh_fil2==3&mh_col==4)
mh20_jvm.zmvm.36 <- filter(cpot20corr_jvm.zmvm, mh_fil2==3&mh_col==6)
mh20_jvm.zmvm.38 <- filter(cpot20corr_jvm.zmvm, mh_fil2==3&mh_col==8)
mh20_jvm.zmvm.39 <- filter(cpot20corr_jvm.zmvm, mh_fil2==3&mh_col==9)
mh20_jvm.zmvm.41 <- filter(cpot20corr_jvm.zmvm, mh_fil2==4&mh_col==1)
mh20_jvm.zmvm.42 <- filter(cpot20corr_jvm.zmvm, mh_fil2==4&mh_col==2)
mh20_jvm.zmvm.43 <- filter(cpot20corr_jvm.zmvm, mh_fil2==4&mh_col==3)
mh20_jvm.zmvm.44 <- filter(cpot20corr_jvm.zmvm, mh_fil2==4&mh_col==4)
mh20_jvm.zmvm.46 <- filter(cpot20corr_jvm.zmvm, mh_fil2==4&mh_col==6)
mh20_jvm.zmvm.47 <- filter(cpot20corr_jvm.zmvm, mh_fil2==4&mh_col==7)
mh20_jvm.zmvm.49 <- filter(cpot20corr_jvm.zmvm, mh_fil2==4&mh_col==9)
pot20mh_jvm.zmvm <- sum(mh20_jvm.zmvm.11$fac)+sum(mh20_jvm.zmvm.13$fac)+
                    sum(mh20_jvm.zmvm.5$fac)+sum(mh20_jvm.zmvm.17$fac)+
                    sum(mh20_jvm.zmvm.19$fac)+sum(mh20_jvm.zmvm.21$fac)+
                    sum(mh20_jvm.zmvm.22$fac)+sum(mh20_jvm.zmvm.23$fac)+
                    sum(mh20_jvm.zmvm.24$fac)+sum(mh20_jvm.zmvm.31$fac)+
                    sum(mh20_jvm.zmvm.32$fac)+sum(mh20_jvm.zmvm.33$fac)+
                    sum(mh20_jvm.zmvm.34$fac)+sum(mh20_jvm.zmvm.36$fac)+
                    sum(mh20_jvm.zmvm.38$fac)+sum(mh20_jvm.zmvm.39$fac)+
                    sum(mh20_jvm.zmvm.41$fac)+sum(mh20_jvm.zmvm.42$fac)+
                    sum(mh20_jvm.zmvm.43$fac)+sum(mh20_jvm.zmvm.44$fac)+
                    sum(mh20_jvm.zmvm.46$fac)+sum(mh20_jvm.zmvm.47$fac)+
                    sum(mh20_jvm.zmvm.49$fac)
pot20mh_jvm.zmvm==pot20corr_jvm.zmvm 
      # Calculo de trabajadores por categoria (formal/informal)
cpotsub20_jvm.zmvm <- mutate_at(cpotsub20_jvm.zmvm, c('p3m4'), 
                                ~replace(., is.na(.), 0))
cpotsub20_jvm.zmvm$emp_ppal.amp <- ifelse(cpotsub20_jvm.zmvm$p3m4==0|
                                          cpotsub20_jvm.zmvm$p6d==6, 1, 
                                          cpotsub20_jvm.zmvm$emp_ppal.amp)
cpotsubinf20_jvm.zmvm <- filter(cpotsub20_jvm.zmvm, emp_ppal.amp==1)
cpotsubfor20_jvm.zmvm <- filter(cpotsub20_jvm.zmvm, emp_ppal.amp==2)
cpotinf20_jvm.zmvm <- rbind(cpotsubinf20_jvm.zmvm, mh20_jvm.zmvm.5, 
                            mh20_jvm.zmvm.17,mh20_jvm.zmvm.19, mh20_jvm.zmvm.39, 
                            mh20_jvm.zmvm.47, mh20_jvm.zmvm.49)
potinf20_jvm.zmvm <- sum(cpotinf20_jvm.zmvm$fac)
cpotfor20_jvm.zmvm <- rbind(cpotsubfor20_jvm.zmvm, mh20_jvm.zmvm.36, 
                            mh20_jvm.zmvm.38, mh20_jvm.zmvm.46)
potfor20_jvm.zmvm <- sum(cpotfor20_jvm.zmvm$fac)
pot20corr_jvm.zmvm==potinf20_jvm.zmvm+potfor20_jvm.zmvm
potinf20_jvm.zmvm/pot20corr_jvm.zmvm
potfor20_jvm.zmvm/pot20corr_jvm.zmvm
      # Calculo de celdas de la Matriz Hussmanns con base en la 
      # definicion ampliada
mh20.amp_jvm.zmvm.11 <- filter(cpotsubinf20_jvm.zmvm, mh_fil2==1&mh_col==1)
mh20.amp_jvm.zmvm.12 <- filter(cpotsubfor20_jvm.zmvm, mh_fil2==1&mh_col==1)
mh20.amp_jvm.zmvm.13 <- filter(cpotsubinf20_jvm.zmvm, mh_fil2==1&mh_col==3)
mh20.amp_jvm.zmvm.14 <- filter(cpotsubfor20_jvm.zmvm, mh_fil2==1&mh_col==3)
mh20.amp_jvm.zmvm.21 <- filter(cpotsubinf20_jvm.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jvm.zmvm.22 <- filter(cpotsubfor20_jvm.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jvm.zmvm.23 <- filter(cpotsubinf20_jvm.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jvm.zmvm.24 <- filter(cpotsubfor20_jvm.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jvm.zmvm.31 <- filter(cpotsubinf20_jvm.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jvm.zmvm.32 <- filter(cpotsubfor20_jvm.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jvm.zmvm.33 <- filter(cpotsubinf20_jvm.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jvm.zmvm.34 <- filter(cpotsubfor20_jvm.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jvm.zmvm.41 <- filter(cpotsubinf20_jvm.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jvm.zmvm.42 <- filter(cpotsubfor20_jvm.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jvm.zmvm.43 <- filter(cpotsubinf20_jvm.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jvm.zmvm.44 <- filter(cpotsubfor20_jvm.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
sum(mh20.amp_jvm.zmvm.11$fac)  # Asalariados informales del sector informal
sum(mh20.amp_jvm.zmvm.12$fac)  # Asalariados formales del sector informal
sum(mh20.amp_jvm.zmvm.13$fac)  # No asalariados informales del sector informal
sum(mh20.amp_jvm.zmvm.14$fac)  # No asalariados formales del sector informal
sum(mh20_jvm.zmvm.5$fac) # Empleadores informales
sum(mh20_jvm.zmvm.17$fac)  # Cuentapropistas del sector informal
sum(mh20_jvm.zmvm.19$fac)  # No remunerados del sector informal
sum(mh20.amp_jvm.zmvm.21$fac)  # Asalariados informales en trabajo del hogar
sum(mh20.amp_jvm.zmvm.22$fac)  # Asalariados formales en trabajo del hogar
sum(mh20.amp_jvm.zmvm.23$fac)  # No asalariados informales en trabajo del hogar
sum(mh20.amp_jvm.zmvm.24$fac)  # No asalariados formales en trabajo del hogar
sum(mh20.amp_jvm.zmvm.31$fac)  
        # Asalariados informales fuera del sector informal
sum(mh20.amp_jvm.zmvm.32$fac)  
        # Asalariados formales fuera del sector informal
sum(mh20.amp_jvm.zmvm.33$fac)  
        # No asalariados informales fuera del sector informal
sum(mh20.amp_jvm.zmvm.34$fac)  
        # No asalariados formales fuera del sector informal
sum(mh20_jvm.zmvm.36$fac)  # Empleadores fuera del sector informal
sum(mh20_jvm.zmvm.38$fac)  # Cuentapropistas formales
sum(mh20_jvm.zmvm.39$fac)  # No remunerados fuera del sector informal
sum(mh20.amp_jvm.zmvm.41$fac)  # Asalariados informales del ambito agropecuario
sum(mh20.amp_jvm.zmvm.42$fac)  # Asalariados formales del ambito agropecuario
sum(mh20.amp_jvm.zmvm.43$fac)  
      # No asalariados informales del ambito agropecuario
sum(mh20.amp_jvm.zmvm.44$fac)  # No asalariados formales del ambito agropecuario
sum(mh20_jvm.zmvm.46$fac)  # Empleadores del ambito agropecuario
sum(mh20_jvm.zmvm.47$fac)  # Cuentapropistas del ambito agropecuario
sum(mh20_jvm.zmvm.49$fac)  # No remunerados del ambito agropecuario
pot20mh.amp_jvm.zmvm <- sum(mh20.amp_jvm.zmvm.11$fac)+
                        sum(mh20.amp_jvm.zmvm.12$fac)+
                        sum(mh20.amp_jvm.zmvm.13$fac)+
                        sum(mh20.amp_jvm.zmvm.14$fac)+sum(mh20_jvm.zmvm.5$fac)+
                        sum(mh20_jvm.zmvm.17$fac)+sum(mh20_jvm.zmvm.19$fac)+
                        sum(mh20.amp_jvm.zmvm.21$fac)+
                        sum(mh20.amp_jvm.zmvm.22$fac)+
                        sum(mh20.amp_jvm.zmvm.23$fac)+
                        sum(mh20.amp_jvm.zmvm.24$fac)+
                        sum(mh20.amp_jvm.zmvm.31$fac)+
                        sum(mh20.amp_jvm.zmvm.32$fac)+
                        sum(mh20.amp_jvm.zmvm.33$fac)+
                        sum(mh20.amp_jvm.zmvm.34$fac)+
                        sum(mh20_jvm.zmvm.36$fac)+
                        sum(mh20_jvm.zmvm.38$fac)+sum(mh20_jvm.zmvm.39$fac)+
                        sum(mh20.amp_jvm.zmvm.41$fac)+
                        sum(mh20.amp_jvm.zmvm.42$fac)+
                        sum(mh20.amp_jvm.zmvm.43$fac)+
                        sum(mh20.amp_jvm.zmvm.44$fac)+
                        sum(mh20_jvm.zmvm.46$fac)+sum(mh20_jvm.zmvm.47$fac)+
                        sum(mh20_jvm.zmvm.49$fac)
pot20mh.amp_jvm.zmvm==pot20corr_jvm.zmvm
    # Escala ZMVM, total de jovenes hombres 2005
      # Primer tratamiento
cpot05_jvh.zmvm <- filter(cpeaoc05_jov.zmvm, sex==1)
cpot05_jvh.zmvm$emp_ppal.amp <- 0
cpotsub05_jvh.zmvm <- filter(cpot05_jvh.zmvm, mh_col<=4&p6d!=9)
cpotsub05_jvh.zmvm$emp_ppal.amp <- 2
cpotnosub05_jvh.zmvm <- filter(cpot05_jvh.zmvm, mh_col>=5)
cpot05corr_jvh.zmvm <- rbind(cpotsub05_jvh.zmvm, cpotnosub05_jvh.zmvm)
pot05corr_jvh.zmvm <- sum(cpot05corr_jvh.zmvm$fac) # POT corregida
pot05corr_jvh.zmvm==sum(cpotsub05_jvh.zmvm$fac)+sum(cpotnosub05_jvh.zmvm$fac)
pot05_jvh.zmvm <- sum(cpot05_jvh.zmvm$fac)
pot05corr_jvh.zmvm/pot05_jvh.zmvm # 99.7 % del total
      # Calculo de celdas de la Matriz Hussmanns (POT corregida)                  
mh05_jvh.zmvm.11 <- filter(cpot05corr_jvh.zmvm, mh_fil2==1&mh_col==1)
mh05_jvh.zmvm.13 <- filter(cpot05corr_jvh.zmvm, mh_fil2==1&mh_col==3)
mh05_jvh.zmvm.5  <- filter(cpot05corr_jvh.zmvm, mh_col==5)
mh05_jvh.zmvm.17 <- filter(cpot05corr_jvh.zmvm, mh_fil2==1&mh_col==7)                      
mh05_jvh.zmvm.19 <- filter(cpot05corr_jvh.zmvm, mh_fil2==1&mh_col==9)
mh05_jvh.zmvm.21 <- filter(cpot05corr_jvh.zmvm, mh_fil2==2&mh_col==1)
mh05_jvh.zmvm.22 <- filter(cpot05corr_jvh.zmvm, mh_fil2==2&mh_col==2)
mh05_jvh.zmvm.23 <- filter(cpot05corr_jvh.zmvm, mh_fil2==2&mh_col==3)
mh05_jvh.zmvm.24 <- filter(cpot05corr_jvh.zmvm, mh_fil2==2&mh_col==4)
mh05_jvh.zmvm.31 <- filter(cpot05corr_jvh.zmvm, mh_fil2==3&mh_col==1)
mh05_jvh.zmvm.32 <- filter(cpot05corr_jvh.zmvm, mh_fil2==3&mh_col==2)
mh05_jvh.zmvm.33 <- filter(cpot05corr_jvh.zmvm, mh_fil2==3&mh_col==3)
mh05_jvh.zmvm.34 <- filter(cpot05corr_jvh.zmvm, mh_fil2==3&mh_col==4)
mh05_jvh.zmvm.36 <- filter(cpot05corr_jvh.zmvm, mh_fil2==3&mh_col==6)
mh05_jvh.zmvm.38 <- filter(cpot05corr_jvh.zmvm, mh_fil2==3&mh_col==8)
mh05_jvh.zmvm.39 <- filter(cpot05corr_jvh.zmvm, mh_fil2==3&mh_col==9)
mh05_jvh.zmvm.41 <- filter(cpot05corr_jvh.zmvm, mh_fil2==4&mh_col==1)
mh05_jvh.zmvm.42 <- filter(cpot05corr_jvh.zmvm, mh_fil2==4&mh_col==2)
mh05_jvh.zmvm.43 <- filter(cpot05corr_jvh.zmvm, mh_fil2==4&mh_col==3)
mh05_jvh.zmvm.44 <- filter(cpot05corr_jvh.zmvm, mh_fil2==4&mh_col==4)
mh05_jvh.zmvm.46 <- filter(cpot05corr_jvh.zmvm, mh_fil2==4&mh_col==6)
mh05_jvh.zmvm.47 <- filter(cpot05corr_jvh.zmvm, mh_fil2==4&mh_col==7)
mh05_jvh.zmvm.49 <- filter(cpot05corr_jvh.zmvm, mh_fil2==4&mh_col==9)
pot05mh_jvh.zmvm <- sum(mh05_jvh.zmvm.11$fac)+sum(mh05_jvh.zmvm.13$fac)+
                    sum(mh05_jvh.zmvm.5$fac)+sum(mh05_jvh.zmvm.17$fac)+
                    sum(mh05_jvh.zmvm.19$fac)+sum(mh05_jvh.zmvm.21$fac)+
                    sum(mh05_jvh.zmvm.22$fac)+sum(mh05_jvh.zmvm.23$fac)+
                    sum(mh05_jvh.zmvm.24$fac)+sum(mh05_jvh.zmvm.31$fac)+
                    sum(mh05_jvh.zmvm.32$fac)+sum(mh05_jvh.zmvm.33$fac)+
                    sum(mh05_jvh.zmvm.34$fac)+sum(mh05_jvh.zmvm.36$fac)+
                    sum(mh05_jvh.zmvm.38$fac)+sum(mh05_jvh.zmvm.39$fac)+
                    sum(mh05_jvh.zmvm.41$fac)+sum(mh05_jvh.zmvm.42$fac)+
                    sum(mh05_jvh.zmvm.43$fac)+sum(mh05_jvh.zmvm.44$fac)+
                    sum(mh05_jvh.zmvm.46$fac)+sum(mh05_jvh.zmvm.47$fac)+
                    sum(mh05_jvh.zmvm.49$fac)
pot05mh_jvh.zmvm==pot05corr_jvh.zmvm 
      # Calculo de trabajadores por categoria (formal/informal)
cpotsub05_jvh.zmvm <- mutate_at(cpotsub05_jvh.zmvm, c('p3m4'), 
                                ~replace(., is.na(.), 0))
cpotsub05_jvh.zmvm$emp_ppal.amp <- ifelse(cpotsub05_jvh.zmvm$p3m4==0|
                                          cpotsub05_jvh.zmvm$p6d==6, 1, 
                                          cpotsub05_jvh.zmvm$emp_ppal.amp)
cpotsubinf05_jvh.zmvm <- filter(cpotsub05_jvh.zmvm, emp_ppal.amp==1)
cpotsubfor05_jvh.zmvm <- filter(cpotsub05_jvh.zmvm, emp_ppal.amp==2)
cpotinf05_jvh.zmvm <- rbind(cpotsubinf05_jvh.zmvm, mh05_jvh.zmvm.5, 
                            mh05_jvh.zmvm.17,mh05_jvh.zmvm.19, mh05_jvh.zmvm.39, 
                            mh05_jvh.zmvm.47, mh05_jvh.zmvm.49)
potinf05_jvh.zmvm <- sum(cpotinf05_jvh.zmvm$fac)
cpotfor05_jvh.zmvm <- rbind(cpotsubfor05_jvh.zmvm, mh05_jvh.zmvm.36, 
                            mh05_jvh.zmvm.38, mh05_jvh.zmvm.46)
potfor05_jvh.zmvm <- sum(cpotfor05_jvh.zmvm$fac)
pot05corr_jvh.zmvm==potinf05_jvh.zmvm+potfor05_jvh.zmvm
potinf05_jvh.zmvm/pot05corr_jvh.zmvm
potfor05_jvh.zmvm/pot05corr_jvh.zmvm
      # Calculo de celdas de la Matriz Hussmanns con base en la 
      # definicion ampliada
mh05.amp_jvh.zmvm.11 <- filter(cpotsubinf05_jvh.zmvm, mh_fil2==1&mh_col==1)
mh05.amp_jvh.zmvm.12 <- filter(cpotsubfor05_jvh.zmvm, mh_fil2==1&mh_col==1)
mh05.amp_jvh.zmvm.13 <- filter(cpotsubinf05_jvh.zmvm, mh_fil2==1&mh_col==3)
mh05.amp_jvh.zmvm.14 <- filter(cpotsubfor05_jvh.zmvm, mh_fil2==1&mh_col==3)
mh05.amp_jvh.zmvm.21 <- filter(cpotsubinf05_jvh.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jvh.zmvm.22 <- filter(cpotsubfor05_jvh.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jvh.zmvm.23 <- filter(cpotsubinf05_jvh.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jvh.zmvm.24 <- filter(cpotsubfor05_jvh.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jvh.zmvm.31 <- filter(cpotsubinf05_jvh.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jvh.zmvm.32 <- filter(cpotsubfor05_jvh.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jvh.zmvm.33 <- filter(cpotsubinf05_jvh.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jvh.zmvm.34 <- filter(cpotsubfor05_jvh.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jvh.zmvm.41 <- filter(cpotsubinf05_jvh.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jvh.zmvm.42 <- filter(cpotsubfor05_jvh.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh05.amp_jvh.zmvm.43 <- filter(cpotsubinf05_jvh.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
mh05.amp_jvh.zmvm.44 <- filter(cpotsubfor05_jvh.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
sum(mh05.amp_jvh.zmvm.11$fac)  # Asalariados informales del sector informal
sum(mh05.amp_jvh.zmvm.12$fac)  # Asalariados formales del sector informal
sum(mh05.amp_jvh.zmvm.13$fac)  # No asalariados informales del sector informal
sum(mh05.amp_jvh.zmvm.14$fac)  # No asalariados formales del sector informal
sum(mh05_jvh.zmvm.5$fac)  # Empleadores informales
sum(mh05_jvh.zmvm.17$fac)  # Cuentapropistas del sector informal
sum(mh05_jvh.zmvm.19$fac)  # No remunerados del sector informal
sum(mh05.amp_jvh.zmvm.21$fac)  # Asalariados informales en trabajo del hogar
sum(mh05.amp_jvh.zmvm.22$fac)  # Asalariados formales en trabajo del hogar
sum(mh05.amp_jvh.zmvm.23$fac)  # No asalariados informales en trabajo del hogar
sum(mh05.amp_jvh.zmvm.24$fac)  # No asalariados formales en trabajo del hogar
sum(mh05.amp_jvh.zmvm.31$fac)  
        # Asalariados informales fuera del sector informal
sum(mh05.amp_jvh.zmvm.32$fac)  
        # Asalariados formales fuera del sector informal
sum(mh05.amp_jvh.zmvm.33$fac)  
        # No asalariados informales fuera del sector informal
sum(mh05.amp_jvh.zmvm.34$fac)  
        # No asalariados formales fuera del sector informal
sum(mh05_jvh.zmvm.36$fac)  # Empleadores fuera del sector informal
sum(mh05_jvh.zmvm.38$fac)  # Cuentapropistas formales
sum(mh05_jvh.zmvm.39$fac)  # No remunerados fuera del sector informal
sum(mh05.amp_jvh.zmvm.41$fac)  # Asalariados informales del ambito agropecuario
sum(mh05.amp_jvh.zmvm.42$fac)  # Asalariados formales del ambito agropecuario
sum(mh05.amp_jvh.zmvm.43$fac)  
        # No asalariados informales del ambito agropecuario
sum(mh05.amp_jvh.zmvm.44$fac)  # No asalariados formales del ambito agropecuario
sum(mh05_jvh.zmvm.46$fac)   # Empleadores del ambito agropecuario
sum(mh05_jvh.zmvm.47$fac)  # Cuentapropistas del ambito agropecuario
sum(mh05_jvh.zmvm.49$fac)  # No remunerados del ambito agropecuario
pot05mh.amp_jvh.zmvm <- sum(mh05.amp_jvh.zmvm.11$fac)+
                        sum(mh05.amp_jvh.zmvm.12$fac)+
                        sum(mh05.amp_jvh.zmvm.13$fac)+
                        sum(mh05.amp_jvh.zmvm.14$fac)+sum(mh05_jvh.zmvm.5$fac)+
                        sum(mh05_jvh.zmvm.17$fac)+sum(mh05_jvh.zmvm.19$fac)+
                        sum(mh05.amp_jvh.zmvm.21$fac)+
                        sum(mh05.amp_jvh.zmvm.22$fac)+
                        sum(mh05.amp_jvh.zmvm.23$fac)+
                        sum(mh05.amp_jvh.zmvm.24$fac)+
                        sum(mh05.amp_jvh.zmvm.31$fac)+
                        sum(mh05.amp_jvh.zmvm.32$fac)+
                        sum(mh05.amp_jvh.zmvm.33$fac)+
                        sum(mh05.amp_jvh.zmvm.34$fac)+
                        sum(mh05_jvh.zmvm.36$fac)+
                        sum(mh05_jvh.zmvm.38$fac)+sum(mh05_jvh.zmvm.39$fac)+
                        sum(mh05.amp_jvh.zmvm.41$fac)+
                        sum(mh05.amp_jvh.zmvm.42$fac)+
                        sum(mh05.amp_jvh.zmvm.43$fac)+
                        sum(mh05.amp_jvh.zmvm.44$fac)+
                        sum(mh05_jvh.zmvm.46$fac)+sum(mh05_jvh.zmvm.47$fac)+
                        sum(mh05_jvh.zmvm.49$fac)
pot05mh.amp_jvh.zmvm==pot05corr_jvh.zmvm
    # Escala ZMVM, total de jovenes hombres 2020
      # Primer tratamiento
cpot20_jvh.zmvm <- filter(cpeaoc20_jov.zmvm, sex==1)
cpot20_jvh.zmvm$emp_ppal.amp <- 0
cpotsub20_jvh.zmvm <- filter(cpot20_jvh.zmvm, mh_col<=4&p6d!=9)
cpotsub20_jvh.zmvm$emp_ppal.amp <- 2
cpotnosub20_jvh.zmvm <- filter(cpot20_jvh.zmvm, mh_col>=5)
cpot20corr_jvh.zmvm <- rbind(cpotsub20_jvh.zmvm, cpotnosub20_jvh.zmvm)
pot20corr_jvh.zmvm <- sum(cpot20corr_jvh.zmvm$fac) # POT corregida
pot20corr_jvh.zmvm==sum(cpotsub20_jvh.zmvm$fac)+sum(cpotnosub20_jvh.zmvm$fac)
pot20_jvh.zmvm <- sum(cpot20_jvh.zmvm$fac)
pot20corr_jvh.zmvm/pot20_jvh.zmvm # 99.9 % del total
      # Calculo de celdas de la Matriz Hussmanns (POT corregida)                
mh20_jvh.zmvm.11 <- filter(cpot20corr_jvh.zmvm, mh_fil2==1&mh_col==1)
mh20_jvh.zmvm.13 <- filter(cpot20corr_jvh.zmvm, mh_fil2==1&mh_col==3)
mh20_jvh.zmvm.5  <- filter(cpot20corr_jvh.zmvm, mh_col==5)
mh20_jvh.zmvm.17 <- filter(cpot20corr_jvh.zmvm, mh_fil2==1&mh_col==7)                      
mh20_jvh.zmvm.19 <- filter(cpot20corr_jvh.zmvm, mh_fil2==1&mh_col==9)
mh20_jvh.zmvm.21 <- filter(cpot20corr_jvh.zmvm, mh_fil2==2&mh_col==1)
mh20_jvh.zmvm.22 <- filter(cpot20corr_jvh.zmvm, mh_fil2==2&mh_col==2)
mh20_jvh.zmvm.23 <- filter(cpot20corr_jvh.zmvm, mh_fil2==2&mh_col==3)
mh20_jvh.zmvm.24 <- filter(cpot20corr_jvh.zmvm, mh_fil2==2&mh_col==4)
mh20_jvh.zmvm.31 <- filter(cpot20corr_jvh.zmvm, mh_fil2==3&mh_col==1)
mh20_jvh.zmvm.32 <- filter(cpot20corr_jvh.zmvm, mh_fil2==3&mh_col==2)
mh20_jvh.zmvm.33 <- filter(cpot20corr_jvh.zmvm, mh_fil2==3&mh_col==3)
mh20_jvh.zmvm.34 <- filter(cpot20corr_jvh.zmvm, mh_fil2==3&mh_col==4)
mh20_jvh.zmvm.36 <- filter(cpot20corr_jvh.zmvm, mh_fil2==3&mh_col==6)
mh20_jvh.zmvm.38 <- filter(cpot20corr_jvh.zmvm, mh_fil2==3&mh_col==8)
mh20_jvh.zmvm.39 <- filter(cpot20corr_jvh.zmvm, mh_fil2==3&mh_col==9)
mh20_jvh.zmvm.41 <- filter(cpot20corr_jvh.zmvm, mh_fil2==4&mh_col==1)
mh20_jvh.zmvm.42 <- filter(cpot20corr_jvh.zmvm, mh_fil2==4&mh_col==2)
mh20_jvh.zmvm.43 <- filter(cpot20corr_jvh.zmvm, mh_fil2==4&mh_col==3)
mh20_jvh.zmvm.44 <- filter(cpot20corr_jvh.zmvm, mh_fil2==4&mh_col==4)
mh20_jvh.zmvm.46 <- filter(cpot20corr_jvh.zmvm, mh_fil2==4&mh_col==6)
mh20_jvh.zmvm.47 <- filter(cpot20corr_jvh.zmvm, mh_fil2==4&mh_col==7)
mh20_jvh.zmvm.49 <- filter(cpot20corr_jvh.zmvm, mh_fil2==4&mh_col==9)
pot20mh_jvh.zmvm <- sum(mh20_jvh.zmvm.11$fac)+sum(mh20_jvh.zmvm.13$fac)+
                    sum(mh20_jvh.zmvm.5$fac)+sum(mh20_jvh.zmvm.17$fac)+
                    sum(mh20_jvh.zmvm.19$fac)+sum(mh20_jvh.zmvm.21$fac)+
                    sum(mh20_jvh.zmvm.22$fac)+sum(mh20_jvh.zmvm.23$fac)+
                    sum(mh20_jvh.zmvm.24$fac)+sum(mh20_jvh.zmvm.31$fac)+
                    sum(mh20_jvh.zmvm.32$fac)+sum(mh20_jvh.zmvm.33$fac)+
                    sum(mh20_jvh.zmvm.34$fac)+sum(mh20_jvh.zmvm.36$fac)+
                    sum(mh20_jvh.zmvm.38$fac)+sum(mh20_jvh.zmvm.39$fac)+
                    sum(mh20_jvh.zmvm.41$fac)+sum(mh20_jvh.zmvm.42$fac)+
                    sum(mh20_jvh.zmvm.43$fac)+sum(mh20_jvh.zmvm.44$fac)+
                    sum(mh20_jvh.zmvm.46$fac)+sum(mh20_jvh.zmvm.47$fac)+
                    sum(mh20_jvh.zmvm.49$fac)
pot20mh_jvh.zmvm==pot20corr_jvh.zmvm 
      # Calculo de trabajadores por categoria (formal/informal)
cpotsub20_jvh.zmvm <- mutate_at(cpotsub20_jvh.zmvm, c('p3m4'), 
                                ~replace(., is.na(.), 0))
cpotsub20_jvh.zmvm$emp_ppal.amp <- ifelse(cpotsub20_jvh.zmvm$p3m4==0|
                                          cpotsub20_jvh.zmvm$p6d==6, 1, 
                                          cpotsub20_jvh.zmvm$emp_ppal.amp)
cpotsubinf20_jvh.zmvm <- filter(cpotsub20_jvh.zmvm, emp_ppal.amp==1)
cpotsubfor20_jvh.zmvm <- filter(cpotsub20_jvh.zmvm, emp_ppal.amp==2)
cpotinf20_jvh.zmvm <- rbind(cpotsubinf20_jvh.zmvm, mh20_jvh.zmvm.5, 
                            mh20_jvh.zmvm.17,mh20_jvh.zmvm.19, mh20_jvh.zmvm.39, 
                            mh20_jvh.zmvm.47, mh20_jvh.zmvm.49)
potinf20_jvh.zmvm <- sum(cpotinf20_jvh.zmvm$fac)
cpotfor20_jvh.zmvm <- rbind(cpotsubfor20_jvh.zmvm, mh20_jvh.zmvm.36, 
                            mh20_jvh.zmvm.38, mh20_jvh.zmvm.46)
potfor20_jvh.zmvm <- sum(cpotfor20_jvh.zmvm$fac)
pot20corr_jvh.zmvm==potinf20_jvh.zmvm+potfor20_jvh.zmvm
potinf20_jvh.zmvm/pot20corr_jvh.zmvm
potfor20_jvh.zmvm/pot20corr_jvh.zmvm
      # Calculo de celdas de la Matriz Hussmanns con base en la 
      # definicion ampliada
mh20.amp_jvh.zmvm.11 <- filter(cpotsubinf20_jvh.zmvm, mh_fil2==1&mh_col==1)
mh20.amp_jvh.zmvm.12 <- filter(cpotsubfor20_jvh.zmvm, mh_fil2==1&mh_col==1)
mh20.amp_jvh.zmvm.13 <- filter(cpotsubinf20_jvh.zmvm, mh_fil2==1&mh_col==3)
mh20.amp_jvh.zmvm.14 <- filter(cpotsubfor20_jvh.zmvm, mh_fil2==1&mh_col==3)
mh20.amp_jvh.zmvm.21 <- filter(cpotsubinf20_jvh.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jvh.zmvm.22 <- filter(cpotsubfor20_jvh.zmvm, mh_fil2==2&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jvh.zmvm.23 <- filter(cpotsubinf20_jvh.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jvh.zmvm.24 <- filter(cpotsubfor20_jvh.zmvm, mh_fil2==2&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jvh.zmvm.31 <- filter(cpotsubinf20_jvh.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jvh.zmvm.32 <- filter(cpotsubfor20_jvh.zmvm, mh_fil2==3&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jvh.zmvm.33 <- filter(cpotsubinf20_jvh.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jvh.zmvm.34 <- filter(cpotsubfor20_jvh.zmvm, mh_fil2==3&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jvh.zmvm.41 <- filter(cpotsubinf20_jvh.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jvh.zmvm.42 <- filter(cpotsubfor20_jvh.zmvm, mh_fil2==4&(mh_col==1|
                                                                  mh_col==2))
mh20.amp_jvh.zmvm.43 <- filter(cpotsubinf20_jvh.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
mh20.amp_jvh.zmvm.44 <- filter(cpotsubfor20_jvh.zmvm, mh_fil2==4&(mh_col==3|
                                                                  mh_col==4))
sum(mh20.amp_jvh.zmvm.11$fac)  # Asalariados informales del sector informal
sum(mh20.amp_jvh.zmvm.12$fac)  # Asalariados formales del sector informal
sum(mh20.amp_jvh.zmvm.13$fac)  # No asalariados informales del sector informal
sum(mh20.amp_jvh.zmvm.14$fac)  # No asalariados formales del sector informal
sum(mh20_jvh.zmvm.5$fac) # Empleadores informales
sum(mh20_jvh.zmvm.17$fac)  # Cuentapropistas del sector informal
sum(mh20_jvh.zmvm.19$fac)  # No remunerados del sector informal
sum(mh20.amp_jvh.zmvm.21$fac)  # Asalariados informales en trabajo del hogar
sum(mh20.amp_jvh.zmvm.22$fac)  # Asalariados formales en trabajo del hogar
sum(mh20.amp_jvh.zmvm.23$fac)  # No asalariados informales en trabajo del hogar
sum(mh20.amp_jvh.zmvm.24$fac)  # No asalariados formales en trabajo del hogar
sum(mh20.amp_jvh.zmvm.31$fac)  # Asalariados informales fuera del sector informal
sum(mh20.amp_jvh.zmvm.32$fac)  
        # Asalariados formales fuera del sector informal
sum(mh20.amp_jvh.zmvm.33$fac)  
        # No asalariados informales fuera del sector informal
sum(mh20.amp_jvh.zmvm.34$fac)  
        # No asalariados formales fuera del sector informal
sum(mh20_jvh.zmvm.36$fac)  # Empleadores fuera del sector informal
sum(mh20_jvh.zmvm.38$fac)  # Cuentapropistas formales
sum(mh20_jvh.zmvm.39$fac)  # No remunerados fuera del sector informal
sum(mh20.amp_jvh.zmvm.41$fac)  # Asalariados informales del ambito agropecuario
sum(mh20.amp_jvh.zmvm.42$fac)  # Asalariados formales del ambito agropecuario
sum(mh20.amp_jvh.zmvm.43$fac)  
        # No asalariados informales del ambito agropecuario
sum(mh20.amp_jvh.zmvm.44$fac)  # No asalariados formales del ambito agropecuario
sum(mh20_jvh.zmvm.46$fac)  # Empleadores del ambito agropecuario
sum(mh20_jvh.zmvm.47$fac)  # Cuentapropistas del ambito agropecuario
sum(mh20_jvh.zmvm.49$fac)  # No remunerados del ambito agropecuario
pot20mh.amp_jvh.zmvm <- sum(mh20.amp_jvh.zmvm.11$fac)+
                        sum(mh20.amp_jvh.zmvm.12$fac)+
                        sum(mh20.amp_jvh.zmvm.13$fac)+
                        sum(mh20.amp_jvh.zmvm.14$fac)+sum(mh20_jvh.zmvm.5$fac)+
                        sum(mh20_jvh.zmvm.17$fac)+sum(mh20_jvh.zmvm.19$fac)+
                        sum(mh20.amp_jvh.zmvm.21$fac)+
                        sum(mh20.amp_jvh.zmvm.22$fac)+
                        sum(mh20.amp_jvh.zmvm.23$fac)+
                        sum(mh20.amp_jvh.zmvm.24$fac)+
                        sum(mh20.amp_jvh.zmvm.31$fac)+
                        sum(mh20.amp_jvh.zmvm.32$fac)+
                        sum(mh20.amp_jvh.zmvm.33$fac)+
                        sum(mh20.amp_jvh.zmvm.34$fac)+
                        sum(mh20_jvh.zmvm.36$fac)+
                        sum(mh20_jvh.zmvm.38$fac)+sum(mh20_jvh.zmvm.39$fac)+
                        sum(mh20.amp_jvh.zmvm.41$fac)+
                        sum(mh20.amp_jvh.zmvm.42$fac)+
                        sum(mh20.amp_jvh.zmvm.43$fac)+
                        sum(mh20.amp_jvh.zmvm.44$fac)+
                        sum(mh20_jvh.zmvm.46$fac)+sum(mh20_jvh.zmvm.47$fac)+
                        sum(mh20_jvh.zmvm.49$fac)
pot20mh.amp_jvh.zmvm==pot20corr_jvh.zmvm
# ------------------------------------------------------------------------------
  # Caracteristicas del mercado de trabajo formal e informal de los jovenes
    # Principales indicadores, escala nacional urbana 2005
cpotpt05_jov.nal<- filter(cpot05corr_tot.nal, eda>=15&eda<=29)
cpotinf05_jov.nal <- filter(cpotinf05_tot.nal, eda>=15&eda<=29)
cpotfor05_jov.nal <- filter(cpotfor05_tot.nal, eda>=15&eda<=29)
sum(cpotinf05_jov.nal$fac)/sum(cpotpt05_jov.nal$fac)
sum(cpotfor05_jov.nal$fac)/sum(cpotpt05_jov.nal$fac)
cpotpt05_nojov.nal<- filter(cpot05corr_tot.nal, eda>=30&eda<=64)
cpotinf05_nojov.nal <- filter(cpotinf05_tot.nal, eda>=30&eda<=64)
cpotfor05_nojov.nal <- filter(cpotfor05_tot.nal, eda>=30&eda<=64)
sum(cpotinf05_nojov.nal$fac)/sum(cpotpt05_nojov.nal$fac)
sum(cpotfor05_nojov.nal$fac)/sum(cpotpt05_nojov.nal$fac)
    # Estadisticas de jovenes a escala nacional urbana y por grupo etario, 2005
      # 15 a 19 anios
        # Escolaridad acumulada
cpotpt05esc_1519.nal <- filter(cpotpt05_jov.nal, eda<=19, anios_esc!=99)
d05esc_1519nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05esc_1519.nal)
mediapt05esc_1519.nal <- svymean(~anios_esc, d05esc_1519nal.pt)
svyttest(anios_esc~0, d05esc_1519nal.pt, na = T)
cpotinf05esc_1519.nal <- filter(cpotinf05_jov.nal, eda<=19, anios_esc!=99)
d05esc_1519nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf05esc_1519.nal)
mediainf05esc_1519.nal <- svymean(~anios_esc, d05esc_1519nal.inf)
svyttest(anios_esc~0, d05esc_1519nal.inf, na = T)
cpotfor05esc_1519.nal <- filter(cpotfor05_jov.nal, eda<=19, anios_esc!=99)
d05esc_1519nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor05esc_1519.nal)
mediafor05esc_1519.nal <- svymean(~anios_esc, d05esc_1519nal.for)
svyttest(anios_esc~0, d05esc_1519nal.for, na = T)
       # Jornada de trabajo
cpotpt05jor_1519.nal <- filter(cpotpt05_jov.nal, eda<=19, hrsocup!=0)
d05jor_1519nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05jor_1519.nal)
mediapt05jor_1519.nal <- svymean(~hrsocup, d05jor_1519nal.pt)
svyttest(hrsocup~0, d05jor_1519nal.pt, na = T)
cpotinf05jor_1519.nal <- filter(cpotinf05_jov.nal, eda<=19, hrsocup!=0)
d05jor_1519nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf05jor_1519.nal)
mediainf05jor_1519.nal <- svymean(~hrsocup, d05jor_1519nal.inf)
svyttest(hrsocup~0, d05jor_1519nal.inf, na = T)
cpotfor05jor_1519.nal <- filter(cpotfor05_jov.nal, eda<=19, hrsocup!=0)
d05jor_1519nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor05jor_1519.nal)
mediafor05jor_1519.nal <- svymean(~hrsocup, d05jor_1519nal.for)
svyttest(hrsocup~0, d05jor_1519nal.for, na = T)
        # Trabajo de reproduccion social
cpotpt05basetrs_1519.nal <- filter(cpotpt05_jov.nal, eda<=19)
cpotpt05basetrs_1519.nal[is.na(cpotpt05basetrs_1519.nal)] <- 0
cpotpt05trs_1519.nal <- filter(cpotpt05basetrs_1519.nal, p11_h2!=98, p11_h2!=99, 
                               p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                               p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                               p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrs_1519.nal <- cpotpt05trs_1519.nal$p11_m2+cpotpt05trs_1519.nal$p11_m3+
                   cpotpt05trs_1519.nal$p11_m4+cpotpt05trs_1519.nal$p11_m5+
                   cpotpt05trs_1519.nal$p11_m6+cpotpt05trs_1519.nal$p11_m7+
                   cpotpt05trs_1519.nal$p11_m8
cpotpt05trs_1519.nal$hrs_trs <- cpotpt05trs_1519.nal$p11_h2+
                                cpotpt05trs_1519.nal$p11_h3+
                                cpotpt05trs_1519.nal$p11_h4+
                                cpotpt05trs_1519.nal$p11_h5+
                                cpotpt05trs_1519.nal$p11_h6+
                                cpotpt05trs_1519.nal$p11_h7+
                                cpotpt05trs_1519.nal$p11_h8+(mintrs_1519.nal/60)
d05trs_1519nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotpt05trs_1519.nal)
mediapt05trs_1519.nal <- svymean(~hrs_trs, d05trs_1519nal.pt)
svyttest(hrs_trs~0, d05trs_1519nal.pt, na = T)
cpotinf05basetrs_1519.nal <- filter(cpotinf05_jov.nal, eda<=19)
cpotinf05basetrs_1519.nal[is.na(cpotinf05basetrs_1519.nal)] <- 0
cpotinf05trs_1519.nal <- filter(cpotinf05basetrs_1519.nal, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_1519.nal <- cpotinf05trs_1519.nal$p11_m2+cpotinf05trs_1519.nal$p11_m3+
                   cpotinf05trs_1519.nal$p11_m4+cpotinf05trs_1519.nal$p11_m5+
                   cpotinf05trs_1519.nal$p11_m6+cpotinf05trs_1519.nal$p11_m7+
                   cpotinf05trs_1519.nal$p11_m8
cpotinf05trs_1519.nal$hrs_trs <- cpotinf05trs_1519.nal$p11_h2+
                                 cpotinf05trs_1519.nal$p11_h3+
                                 cpotinf05trs_1519.nal$p11_h4+
                                 cpotinf05trs_1519.nal$p11_h5+
                                 cpotinf05trs_1519.nal$p11_h6+
                                 cpotinf05trs_1519.nal$p11_h7+
                                 cpotinf05trs_1519.nal$p11_h8+
                                 (mintrs_1519.nal/60)
d05trs_1519nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf05trs_1519.nal)
mediainf05trs_1519.nal <- svymean(~hrs_trs, d05trs_1519nal.inf)
svyttest(hrs_trs~0, d05trs_1519nal.inf, na = T)
cpotfor05basetrs_1519.nal <- filter(cpotfor05_jov.nal, eda<=19)
cpotfor05basetrs_1519.nal[is.na(cpotfor05basetrs_1519.nal)] <- 0
cpotfor05trs_1519.nal <- filter(cpotfor05basetrs_1519.nal, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_1519.nal <- cpotfor05trs_1519.nal$p11_m2+cpotfor05trs_1519.nal$p11_m3+
                   cpotfor05trs_1519.nal$p11_m4+cpotfor05trs_1519.nal$p11_m5+
                   cpotfor05trs_1519.nal$p11_m6+cpotfor05trs_1519.nal$p11_m7+
                   cpotfor05trs_1519.nal$p11_m8
cpotfor05trs_1519.nal$hrs_trs <- cpotfor05trs_1519.nal$p11_h2+
                                 cpotfor05trs_1519.nal$p11_h3+
                                 cpotfor05trs_1519.nal$p11_h4+
                                 cpotfor05trs_1519.nal$p11_h5+
                                 cpotfor05trs_1519.nal$p11_h6+
                                 cpotfor05trs_1519.nal$p11_h7+
                                 cpotfor05trs_1519.nal$p11_h8+
                                 (mintrs_1519.nal/60)
d05trs_1519nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor05trs_1519.nal)
mediafor05trs_1519.nal <- svymean(~hrs_trs, d05trs_1519nal.for)
svyttest(hrs_trs~0, d05trs_1519nal.for, na = T)
        # Principal actividad
cpotpt05act_1519.nal <- filter(cpotpt05_jov.nal, eda<=19)
cact05_1519nal.pt <- aggregate(fac~p3, cpotpt05act_1519.nal, sum, na.rm=T)
cact05_1519nal.pt$prop <- cact05_1519nal.pt$fac/sum(cact05_1519nal.pt$fac)
act05_1519nal.pt <- cact05_1519nal.pt[order(cact05_1519nal.pt[,3], 
                                            decreasing=T),]
cpotinf05act_1519.nal <- filter(cpotinf05_jov.nal, eda<=19)
cact05_1519nal.inf <- aggregate(fac~p3, cpotinf05act_1519.nal, sum, na.rm=T)
cact05_1519nal.inf$prop <- cact05_1519nal.inf$fac/sum(cact05_1519nal.inf$fac)
act05_1519nal.inf <- cact05_1519nal.inf[order(cact05_1519nal.inf[,3], 
                                              decreasing=T),]
cpotfor05act_1519.nal <- filter(cpotfor05_jov.nal, eda<=19)
cact05_1519nal.for <- aggregate(fac~p3, cpotfor05act_1519.nal, sum, na.rm=T)
cact05_1519nal.for$prop <- cact05_1519nal.for$fac/sum(cact05_1519nal.for$fac)
act05_1519nal.for <- cact05_1519nal.for[order(cact05_1519nal.for[,3], 
                                              decreasing=T),]
      # 20 a 24 anios
        # Escolaridad acumulada
cpotpt05esc_2024.nal <- filter(cpotpt05_jov.nal, eda>=20&eda<=24, anios_esc!=99)
d05esc_2024nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05esc_2024.nal)
mediapt05esc_2024.nal <- svymean(~anios_esc, d05esc_2024nal.pt)
svyttest(anios_esc~0, d05esc_2024nal.pt, na = T)
cpotinf05esc_2024.nal <- filter(cpotinf05_jov.nal, eda>=20&eda<=24, 
                                anios_esc!=99)
d05esc_2024nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf05esc_2024.nal)
mediainf05esc_2024.nal <- svymean(~anios_esc, d05esc_2024nal.inf)
svyttest(anios_esc~0, d05esc_2024nal.inf, na = T)
cpotfor05esc_2024.nal <- filter(cpotfor05_jov.nal, eda>=20&eda<=24, 
                                anios_esc!=99)
d05esc_2024nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor05esc_2024.nal)
mediafor05esc_2024.nal <- svymean(~anios_esc, d05esc_2024nal.for)
svyttest(anios_esc~0, d05esc_2024nal.for, na = T)
        # Jornada de trabajo
cpotpt05jor_2024.nal <- filter(cpotpt05_jov.nal, eda>=20&eda<=24, hrsocup!=0)
d05jor_2024nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05jor_2024.nal)
mediapt05jor_2024.nal <- svymean(~hrsocup, d05jor_2024nal.pt)
svyttest(hrsocup~0, d05jor_2024nal.pt, na = T)
cpotinf05jor_2024.nal <- filter(cpotinf05_jov.nal, eda>=20&eda<=24, hrsocup!=0)
d05jor_2024nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf05jor_2024.nal)
mediainf05jor_2024.nal <- svymean(~hrsocup, d05jor_2024nal.inf)
svyttest(hrsocup~0, d05jor_2024nal.inf, na = T)
cpotfor05jor_2024.nal <- filter(cpotfor05_jov.nal, eda>=20&eda<=24, hrsocup!=0)
d05jor_2024nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor05jor_2024.nal)
mediafor05jor_2024.nal <- svymean(~hrsocup, d05jor_2024nal.for)
svyttest(hrsocup~0, d05jor_2024nal.for, na = T)
        # Trabajo de reproduccion social
cpotpt05basetrs_2024.nal <- filter(cpotpt05_jov.nal, eda>=20&eda<=24)
cpotpt05basetrs_2024.nal[is.na(cpotpt05basetrs_2024.nal)] <- 0
cpotpt05trs_2024.nal <- filter(cpotpt05basetrs_2024.nal, p11_h2!=98, p11_h2!=99, 
                               p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                               p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                               p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrs_2024.nal <- cpotpt05trs_2024.nal$p11_m2+cpotpt05trs_2024.nal$p11_m3+
                   cpotpt05trs_2024.nal$p11_m4+cpotpt05trs_2024.nal$p11_m5+
                   cpotpt05trs_2024.nal$p11_m6+cpotpt05trs_2024.nal$p11_m7+
                   cpotpt05trs_2024.nal$p11_m8
cpotpt05trs_2024.nal$hrs_trs <- cpotpt05trs_2024.nal$p11_h2+
                                cpotpt05trs_2024.nal$p11_h3+
                                cpotpt05trs_2024.nal$p11_h4+
                                cpotpt05trs_2024.nal$p11_h5+
                                cpotpt05trs_2024.nal$p11_h6+
                                cpotpt05trs_2024.nal$p11_h7+
                                cpotpt05trs_2024.nal$p11_h8+(mintrs_2024.nal/60)
d05trs_2024nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05trs_2024.nal)
mediapt05trs_2024.nal <- svymean(~hrs_trs, d05trs_2024nal.pt)
svyttest(hrs_trs~0, d05trs_2024nal.pt, na = T)
cpotinf05basetrs_2024.nal <- filter(cpotinf05_jov.nal, eda>=20&eda<=24)
cpotinf05basetrs_2024.nal[is.na(cpotinf05basetrs_2024.nal)] <- 0
cpotinf05trs_2024.nal <- filter(cpotinf05basetrs_2024.nal, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_2024.nal <- cpotinf05trs_2024.nal$p11_m2+cpotinf05trs_2024.nal$p11_m3+
                   cpotinf05trs_2024.nal$p11_m4+cpotinf05trs_2024.nal$p11_m5+
                   cpotinf05trs_2024.nal$p11_m6+cpotinf05trs_2024.nal$p11_m7+
                   cpotinf05trs_2024.nal$p11_m8
cpotinf05trs_2024.nal$hrs_trs <- cpotinf05trs_2024.nal$p11_h2+
                                 cpotinf05trs_2024.nal$p11_h3+
                                 cpotinf05trs_2024.nal$p11_h4+
                                 cpotinf05trs_2024.nal$p11_h5+
                                 cpotinf05trs_2024.nal$p11_h6+
                                 cpotinf05trs_2024.nal$p11_h7+
                                 cpotinf05trs_2024.nal$p11_h8+
                                 (mintrs_2024.nal/60)
d05trs_2024nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf05trs_2024.nal)
mediainf05trs_2024.nal <- svymean(~hrs_trs, d05trs_2024nal.inf)
svyttest(hrs_trs~0, d05trs_2024nal.inf, na = T)
cpotfor05basetrs_2024.nal <- filter(cpotfor05_jov.nal, eda>=20&eda<=24)
cpotfor05basetrs_2024.nal[is.na(cpotfor05basetrs_2024.nal)] <- 0
cpotfor05trs_2024.nal <- filter(cpotfor05basetrs_2024.nal, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_2024.nal <- cpotfor05trs_2024.nal$p11_m2+cpotfor05trs_2024.nal$p11_m3+
                   cpotfor05trs_2024.nal$p11_m4+cpotfor05trs_2024.nal$p11_m5+
                   cpotfor05trs_2024.nal$p11_m6+cpotfor05trs_2024.nal$p11_m7+
                   cpotfor05trs_2024.nal$p11_m8
cpotfor05trs_2024.nal$hrs_trs <- cpotfor05trs_2024.nal$p11_h2+
                                 cpotfor05trs_2024.nal$p11_h3+
                                 cpotfor05trs_2024.nal$p11_h4+
                                 cpotfor05trs_2024.nal$p11_h5+
                                 cpotfor05trs_2024.nal$p11_h6+
                                 cpotfor05trs_2024.nal$p11_h7+
                                 cpotfor05trs_2024.nal$p11_h8+
                                 (mintrs_2024.nal/60)
d05trs_2024nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor05trs_2024.nal)
mediafor05trs_2024.nal <- svymean(~hrs_trs, d05trs_2024nal.for)
svyttest(hrs_trs~0, d05trs_2024nal.for, na = T)
        # Principal actividad
cpotpt05act_2024.nal <- filter(cpotpt05_jov.nal, eda>=20&eda<=24)
cact05_2024nal.pt <- aggregate(fac~p3, cpotpt05act_2024.nal, sum, na.rm=T)
cact05_2024nal.pt$prop <- cact05_2024nal.pt$fac/sum(cact05_2024nal.pt$fac)
act05_2024nal.pt <- cact05_2024nal.pt[order(cact05_2024nal.pt[,3], 
                                            decreasing=T),]
cpotinf05act_2024.nal <- filter(cpotinf05_jov.nal, eda>=20&eda<=24)
cact05_2024nal.inf <- aggregate(fac~p3, cpotinf05act_2024.nal, sum, na.rm=T)
cact05_2024nal.inf$prop <- cact05_2024nal.inf$fac/sum(cact05_2024nal.inf$fac)
act05_2024nal.inf <- cact05_2024nal.inf[order(cact05_2024nal.inf[,3], 
                                              decreasing=T),]
cpotfor05act_2024.nal <- filter(cpotfor05_jov.nal, eda>=20&eda<=24)
cact05_2024nal.for <- aggregate(fac~p3, cpotfor05act_2024.nal, sum, na.rm=T)
cact05_2024nal.for$prop <- cact05_2024nal.for$fac/sum(cact05_2024nal.for$fac)
act05_2024nal.for <- cact05_2024nal.for[order(cact05_2024nal.for[,3], 
                                              decreasing=T),]
      # 25 a 29 anios
        # Escolaridad acumulada
cpotpt05esc_2529.nal <- filter(cpotpt05_jov.nal, eda>=25&eda<=29, anios_esc!=99)
d05esc_2529nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05esc_2529.nal)
mediapt05esc_2529.nal <- svymean(~anios_esc, d05esc_2529nal.pt)
svyttest(anios_esc~0, d05esc_2529nal.pt, na = T)
cpotinf05esc_2529.nal <- filter(cpotinf05_jov.nal, eda>=25&eda<=29, 
                                anios_esc!=99)
d05esc_2529nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf05esc_2529.nal)
mediainf05esc_2529.nal <- svymean(~anios_esc, d05esc_2529nal.inf)
svyttest(anios_esc~0, d05esc_2529nal.inf, na = T)
cpotfor05esc_2529.nal <- filter(cpotfor05_jov.nal, eda>=25&eda<=29, 
                                anios_esc!=99)
d05esc_2529nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor05esc_2529.nal)
mediafor05esc_2529.nal <- svymean(~anios_esc, d05esc_2529nal.for)
svyttest(anios_esc~0, d05esc_2529nal.for, na = T)
        # Jornada de trabajo
cpotpt05jor_2529.nal <- filter(cpotpt05_jov.nal, eda>=25&eda<=29, hrsocup!=0)
d05jor_2529nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05jor_2529.nal)
mediapt05jor_2529.nal <- svymean(~hrsocup, d05jor_2529nal.pt)
svyttest(hrsocup~0, d05jor_2529nal.pt, na = T)
cpotinf05jor_2529.nal <- filter(cpotinf05_jov.nal, eda>=25&eda<=29, hrsocup!=0)
d05jor_2529nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf05jor_2529.nal)
mediainf05jor_2529.nal <- svymean(~hrsocup, d05jor_2529nal.inf)
svyttest(hrsocup~0, d05jor_2529nal.inf, na = T)
cpotfor05jor_2529.nal <- filter(cpotfor05_jov.nal, eda>=25&eda<=29, hrsocup!=0)
d05jor_2529nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor05jor_2529.nal)
mediafor05jor_2529.nal <- svymean(~hrsocup, d05jor_2529nal.for)
svyttest(hrsocup~0, d05jor_2529nal.for, na = T)
        # Trabajo de reproduccion social
cpotpt05basetrs_2529.nal <- filter(cpotpt05_jov.nal, eda>=25&eda<=29)
cpotpt05basetrs_2529.nal[is.na(cpotpt05basetrs_2529.nal)] <- 0
cpotpt05trs_2529.nal <- filter(cpotpt05basetrs_2529.nal, p11_h2!=98, p11_h2!=99, 
                               p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                               p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                               p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrs_2529.nal <- cpotpt05trs_2529.nal$p11_m2+cpotpt05trs_2529.nal$p11_m3+
                   cpotpt05trs_2529.nal$p11_m4+cpotpt05trs_2529.nal$p11_m5+
                   cpotpt05trs_2529.nal$p11_m6+cpotpt05trs_2529.nal$p11_m7+
                   cpotpt05trs_2529.nal$p11_m8
cpotpt05trs_2529.nal$hrs_trs <- cpotpt05trs_2529.nal$p11_h2+
                                cpotpt05trs_2529.nal$p11_h3+
                                cpotpt05trs_2529.nal$p11_h4+
                                cpotpt05trs_2529.nal$p11_h5+
                                cpotpt05trs_2529.nal$p11_h6+
                                cpotpt05trs_2529.nal$p11_h7+
                                cpotpt05trs_2529.nal$p11_h8+(mintrs_2529.nal/60)
d05trs_2529nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05trs_2529.nal)
mediapt05trs_2529.nal <- svymean(~hrs_trs, d05trs_2529nal.pt)
svyttest(hrs_trs~0, d05trs_2529nal.pt, na = T)
cpotinf05basetrs_2529.nal <- filter(cpotinf05_jov.nal, eda>=25&eda<=29)
cpotinf05basetrs_2529.nal[is.na(cpotinf05basetrs_2529.nal)] <- 0
cpotinf05trs_2529.nal <- filter(cpotinf05basetrs_2529.nal, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_2529.nal <- cpotinf05trs_2529.nal$p11_m2+cpotinf05trs_2529.nal$p11_m3+
                   cpotinf05trs_2529.nal$p11_m4+cpotinf05trs_2529.nal$p11_m5+
                   cpotinf05trs_2529.nal$p11_m6+cpotinf05trs_2529.nal$p11_m7+
                   cpotinf05trs_2529.nal$p11_m8
cpotinf05trs_2529.nal$hrs_trs <- cpotinf05trs_2529.nal$p11_h2+
                                 cpotinf05trs_2529.nal$p11_h3+
                                 cpotinf05trs_2529.nal$p11_h4+
                                 cpotinf05trs_2529.nal$p11_h5+
                                 cpotinf05trs_2529.nal$p11_h6+
                                 cpotinf05trs_2529.nal$p11_h7+
                                 cpotinf05trs_2529.nal$p11_h8+
                                 (mintrs_2529.nal/60)
d05trs_2529nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf05trs_2529.nal)
mediainf05trs_2529.nal <- svymean(~hrs_trs, d05trs_2529nal.inf)
svyttest(hrs_trs~0, d05trs_2529nal.inf, na = T)
cpotfor05basetrs_2529.nal <- filter(cpotfor05_jov.nal, eda>=25&eda<=29)
cpotfor05basetrs_2529.nal[is.na(cpotfor05basetrs_2529.nal)] <- 0
cpotfor05trs_2529.nal <- filter(cpotfor05basetrs_2529.nal, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_2529.nal <- cpotfor05trs_2529.nal$p11_m2+cpotfor05trs_2529.nal$p11_m3+
                   cpotfor05trs_2529.nal$p11_m4+cpotfor05trs_2529.nal$p11_m5+
                   cpotfor05trs_2529.nal$p11_m6+cpotfor05trs_2529.nal$p11_m7+
                   cpotfor05trs_2529.nal$p11_m8
cpotfor05trs_2529.nal$hrs_trs <- cpotfor05trs_2529.nal$p11_h2+
                                 cpotfor05trs_2529.nal$p11_h3+
                                 cpotfor05trs_2529.nal$p11_h4+
                                 cpotfor05trs_2529.nal$p11_h5+
                                 cpotfor05trs_2529.nal$p11_h6+
                                 cpotfor05trs_2529.nal$p11_h7+
                                 cpotfor05trs_2529.nal$p11_h8+
                                 (mintrs_2529.nal/60)
d05trs_2529nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor05trs_2529.nal)
mediafor05trs_2529.nal <- svymean(~hrs_trs, d05trs_2529nal.for)
svyttest(hrs_trs~0, d05trs_2529nal.for, na = T)
        # Principal actividad
cpotpt05act_2529.nal <- filter(cpotpt05_jov.nal, eda>=25&eda<=29)
cact05_2529nal.pt <- aggregate(fac~p3, cpotpt05act_2529.nal, sum, na.rm=T)
cact05_2529nal.pt$prop <- cact05_2529nal.pt$fac/sum(cact05_2529nal.pt$fac)
act05_2529nal.pt <- cact05_2529nal.pt[order(cact05_2529nal.pt[,3], 
                                            decreasing=T),]
cpotinf05act_2529.nal <- filter(cpotinf05_jov.nal, eda>=25&eda<=29)
cact05_2529nal.inf <- aggregate(fac~p3, cpotinf05act_2529.nal, sum, na.rm=T)
cact05_2529nal.inf$prop <- cact05_2529nal.inf$fac/sum(cact05_2529nal.inf$fac)
act05_2529nal.inf <- cact05_2529nal.inf[order(cact05_2529nal.inf[,3], 
                                              decreasing=T),]
cpotfor05act_2529.nal <- filter(cpotfor05_jov.nal, eda>=25&eda<=29)
cact05_2529nal.for <- aggregate(fac~p3, cpotfor05act_2529.nal, sum, na.rm=T)
cact05_2529nal.for$prop <- cact05_2529nal.for$fac/sum(cact05_2529nal.for$fac)
act05_2529nal.for <- cact05_2529nal.for[order(cact05_2529nal.for[,3], 
                                              decreasing=T),]
    # Principales indicadores, escala nacional urbana 2020
cpotpt20_jov.nal<- filter(cpot20corr_tot.nal, eda>=15&eda<=29)
cpotinf20_jov.nal <- filter(cpotinf20_tot.nal, eda>=15&eda<=29)
cpotfor20_jov.nal <- filter(cpotfor20_tot.nal, eda>=15&eda<=29)
sum(cpotinf20_jov.nal$fac)/sum(cpotpt20_jov.nal$fac)
sum(cpotfor20_jov.nal$fac)/sum(cpotpt20_jov.nal$fac)
cpotpt20_nojov.nal<- filter(cpot20corr_tot.nal, eda>=30&eda<=64)
cpotinf20_nojov.nal <- filter(cpotinf20_tot.nal, eda>=30&eda<=64)
cpotfor20_nojov.nal <- filter(cpotfor20_tot.nal, eda>=30&eda<=64)
sum(cpotinf20_nojov.nal$fac)/sum(cpotpt20_nojov.nal$fac)
sum(cpotfor20_nojov.nal$fac)/sum(cpotpt20_nojov.nal$fac)
    # Estadisticas de jovenes a escala nacional urbana y por grupo etario, 2020
      # 15 a 19 anios
        # Escolaridad acumulada
cpotpt20esc_1519.nal <- filter(cpotpt20_jov.nal, eda<=19, anios_esc!=99)
d20esc_1519nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20esc_1519.nal)
mediapt20esc_1519.nal <- svymean(~anios_esc, d20esc_1519nal.pt)
svyttest(anios_esc~0, d20esc_1519nal.pt, na = T)
cpotinf20esc_1519.nal <- filter(cpotinf20_jov.nal, eda<=19, anios_esc!=99)
d20esc_1519nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf20esc_1519.nal)
mediainf20esc_1519.nal <- svymean(~anios_esc, d20esc_1519nal.inf)
svyttest(anios_esc~0, d20esc_1519nal.inf, na = T)
cpotfor20esc_1519.nal <- filter(cpotfor20_jov.nal, eda<=19, anios_esc!=99)
d20esc_1519nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor20esc_1519.nal)
mediafor20esc_1519.nal <- svymean(~anios_esc, d20esc_1519nal.for)
svyttest(anios_esc~0, d20esc_1519nal.for, na = T)
        # Jornada de trabajo
cpotpt20jor_1519.nal <- filter(cpotpt20_jov.nal, eda<=19, hrsocup!=0)
d20jor_1519nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20jor_1519.nal)
mediapt20jor_1519.nal <- svymean(~hrsocup, d20jor_1519nal.pt)
svyttest(hrsocup~0, d20jor_1519nal.pt, na = T)
cpotinf20jor_1519.nal <- filter(cpotinf20_jov.nal, eda<=19, hrsocup!=0)
d20jor_1519nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf20jor_1519.nal)
mediainf20jor_1519.nal <- svymean(~hrsocup, d20jor_1519nal.inf)
svyttest(hrsocup~0, d20jor_1519nal.inf, na = T)
cpotfor20jor_1519.nal <- filter(cpotfor20_jov.nal, eda<=19, hrsocup!=0)
d20jor_1519nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor20jor_1519.nal)
mediafor20jor_1519.nal <- svymean(~hrsocup, d20jor_1519nal.for)
svyttest(hrsocup~0, d20jor_1519nal.for, na = T)
        # Trabajo de reproduccion social
cpotpt20basetrs_1519.nal <- filter(cpotpt20_jov.nal, eda<=19)
cpotpt20basetrs_1519.nal[is.na(cpotpt20basetrs_1519.nal)] <- 0
cpotpt20trs_1519.nal <- filter(cpotpt20basetrs_1519.nal, p11_h2!=98, p11_h2!=99, 
                               p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                               p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                               p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrs_1519.nal <- cpotpt20trs_1519.nal$p11_m2+cpotpt20trs_1519.nal$p11_m3+
                   cpotpt20trs_1519.nal$p11_m4+cpotpt20trs_1519.nal$p11_m5+
                   cpotpt20trs_1519.nal$p11_m6+cpotpt20trs_1519.nal$p11_m7+
                   cpotpt20trs_1519.nal$p11_m8
cpotpt20trs_1519.nal$hrs_trs <- cpotpt20trs_1519.nal$p11_h2+
                                cpotpt20trs_1519.nal$p11_h3+
                                cpotpt20trs_1519.nal$p11_h4+
                                cpotpt20trs_1519.nal$p11_h5+
                                cpotpt20trs_1519.nal$p11_h6+
                                cpotpt20trs_1519.nal$p11_h7+
                                cpotpt20trs_1519.nal$p11_h8+(mintrs_1519.nal/60)
d20trs_1519nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20trs_1519.nal)
mediapt20trs_1519.nal <- svymean(~hrs_trs, d20trs_1519nal.pt)
svyttest(hrs_trs~0, d20trs_1519nal.pt, na = T)
cpotinf20basetrs_1519.nal <- filter(cpotinf20_jov.nal, eda<=19)
cpotinf20basetrs_1519.nal[is.na(cpotinf20basetrs_1519.nal)] <- 0
cpotinf20trs_1519.nal <- filter(cpotinf20basetrs_1519.nal, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_1519.nal <- cpotinf20trs_1519.nal$p11_m2+cpotinf20trs_1519.nal$p11_m3+
                   cpotinf20trs_1519.nal$p11_m4+cpotinf20trs_1519.nal$p11_m5+
                   cpotinf20trs_1519.nal$p11_m6+cpotinf20trs_1519.nal$p11_m7+
                   cpotinf20trs_1519.nal$p11_m8
cpotinf20trs_1519.nal$hrs_trs <- cpotinf20trs_1519.nal$p11_h2+
                                 cpotinf20trs_1519.nal$p11_h3+
                                 cpotinf20trs_1519.nal$p11_h4+
                                 cpotinf20trs_1519.nal$p11_h5+
                                 cpotinf20trs_1519.nal$p11_h6+
                                 cpotinf20trs_1519.nal$p11_h7+
                                 cpotinf20trs_1519.nal$p11_h8+
                                 (mintrs_1519.nal/60)
d20trs_1519nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf20trs_1519.nal)
mediainf20trs_1519.nal <- svymean(~hrs_trs, d20trs_1519nal.inf)
svyttest(hrs_trs~0, d20trs_1519nal.inf, na = T)
cpotfor20basetrs_1519.nal <- filter(cpotfor20_jov.nal, eda<=19)
cpotfor20basetrs_1519.nal[is.na(cpotfor20basetrs_1519.nal)] <- 0
cpotfor20trs_1519.nal <- filter(cpotfor20basetrs_1519.nal, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_1519.nal <- cpotfor20trs_1519.nal$p11_m2+cpotfor20trs_1519.nal$p11_m3+
                   cpotfor20trs_1519.nal$p11_m4+cpotfor20trs_1519.nal$p11_m5+
                   cpotfor20trs_1519.nal$p11_m6+cpotfor20trs_1519.nal$p11_m7+
                   cpotfor20trs_1519.nal$p11_m8
cpotfor20trs_1519.nal$hrs_trs <- cpotfor20trs_1519.nal$p11_h2+
                                 cpotfor20trs_1519.nal$p11_h3+
                                 cpotfor20trs_1519.nal$p11_h4+
                                 cpotfor20trs_1519.nal$p11_h5+
                                 cpotfor20trs_1519.nal$p11_h6+
                                 cpotfor20trs_1519.nal$p11_h7+
                                 cpotfor20trs_1519.nal$p11_h8+
                                 (mintrs_1519.nal/60)
d20trs_1519nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor20trs_1519.nal)
mediafor20trs_1519.nal <- svymean(~hrs_trs, d20trs_1519nal.for)
svyttest(hrs_trs~0, d20trs_1519nal.for, na = T)
        # Principal actividad
cpotpt20act_1519.nal <- filter(cpotpt20_jov.nal, eda<=19)
cact20_1519nal.pt <- aggregate(fac~p3, cpotpt20act_1519.nal, sum, na.rm=T)
cact20_1519nal.pt$prop <- cact20_1519nal.pt$fac/sum(cact20_1519nal.pt$fac)
act20_1519nal.pt <- cact20_1519nal.pt[order(cact20_1519nal.pt[,3], 
                                            decreasing=T),]
cpotinf20act_1519.nal <- filter(cpotinf20_jov.nal, eda<=19)
cact20_1519nal.inf <- aggregate(fac~p3, cpotinf20act_1519.nal, sum, na.rm=T)
cact20_1519nal.inf$prop <- cact20_1519nal.inf$fac/sum(cact20_1519nal.inf$fac)
act20_1519nal.inf <- cact20_1519nal.inf[order(cact20_1519nal.inf[,3], 
                                              decreasing=T),]
cpotfor20act_1519.nal <- filter(cpotfor20_jov.nal, eda<=19)
cact20_1519nal.for <- aggregate(fac~p3, cpotfor20act_1519.nal, sum, na.rm=T)
cact20_1519nal.for$prop <- cact20_1519nal.for$fac/sum(cact20_1519nal.for$fac)
act20_1519nal.for <- cact20_1519nal.for[order(cact20_1519nal.for[,3], 
                                              decreasing=T),]
      # 20 a 24 anios
        # Escolaridad acumulada
cpotpt20esc_2024.nal <- filter(cpotpt20_jov.nal, eda>=20&eda<=24, anios_esc!=99)
d20esc_2024nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20esc_2024.nal)
mediapt20esc_2024.nal <- svymean(~anios_esc, d20esc_2024nal.pt)
svyttest(anios_esc~0, d20esc_2024nal.pt, na = T)
cpotinf20esc_2024.nal <- filter(cpotinf20_jov.nal, eda>=20&eda<=24, 
                                anios_esc!=99)
d20esc_2024nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf20esc_2024.nal)
mediainf20esc_2024.nal <- svymean(~anios_esc, d20esc_2024nal.inf)
svyttest(anios_esc~0, d20esc_2024nal.inf, na = T)
cpotfor20esc_2024.nal <- filter(cpotfor20_jov.nal, eda>=20&eda<=24, 
                                anios_esc!=99)
d20esc_2024nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor20esc_2024.nal)
mediafor20esc_2024.nal <- svymean(~anios_esc, d20esc_2024nal.for)
svyttest(anios_esc~0, d20esc_2024nal.for, na = T)
        # Jornada de trabajo
cpotpt20jor_2024.nal <- filter(cpotpt20_jov.nal, eda>=20&eda<=24, hrsocup!=0)
d20jor_2024nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20jor_2024.nal)
mediapt20jor_2024.nal <- svymean(~hrsocup, d20jor_2024nal.pt)
svyttest(hrsocup~0, d20jor_2024nal.pt, na = T)
cpotinf20jor_2024.nal <- filter(cpotinf20_jov.nal, eda>=20&eda<=24, hrsocup!=0)
d20jor_2024nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf20jor_2024.nal)
mediainf20jor_2024.nal <- svymean(~hrsocup, d20jor_2024nal.inf)
svyttest(hrsocup~0, d20jor_2024nal.inf, na = T)
cpotfor20jor_2024.nal <- filter(cpotfor20_jov.nal, eda>=20&eda<=24, hrsocup!=0)
d20jor_2024nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor20jor_2024.nal)
mediafor20jor_2024.nal <- svymean(~hrsocup, d20jor_2024nal.for)
svyttest(hrsocup~0, d20jor_2024nal.for, na = T)
        # Trabajo de reproduccion social
cpotpt20basetrs_2024.nal <- filter(cpotpt20_jov.nal, eda>=20&eda<=24)
cpotpt20basetrs_2024.nal[is.na(cpotpt20basetrs_2024.nal)] <- 0
cpotpt20trs_2024.nal <- filter(cpotpt20basetrs_2024.nal, p11_h2!=98, p11_h2!=99, 
                               p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                               p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                               p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrs_2024.nal <- cpotpt20trs_2024.nal$p11_m2+cpotpt20trs_2024.nal$p11_m3+
                   cpotpt20trs_2024.nal$p11_m4+cpotpt20trs_2024.nal$p11_m5+
                   cpotpt20trs_2024.nal$p11_m6+cpotpt20trs_2024.nal$p11_m7+
                   cpotpt20trs_2024.nal$p11_m8
cpotpt20trs_2024.nal$hrs_trs <- cpotpt20trs_2024.nal$p11_h2+
                                cpotpt20trs_2024.nal$p11_h3+
                                cpotpt20trs_2024.nal$p11_h4+
                                cpotpt20trs_2024.nal$p11_h5+
                                cpotpt20trs_2024.nal$p11_h6+
                                cpotpt20trs_2024.nal$p11_h7+
                                cpotpt20trs_2024.nal$p11_h8+(mintrs_2024.nal/60)
d20trs_2024nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20trs_2024.nal)
mediapt20trs_2024.nal <- svymean(~hrs_trs, d20trs_2024nal.pt)
svyttest(hrs_trs~0, d20trs_2024nal.pt, na = T)
cpotinf20basetrs_2024.nal <- filter(cpotinf20_jov.nal, eda>=20&eda<=24)
cpotinf20basetrs_2024.nal[is.na(cpotinf20basetrs_2024.nal)] <- 0
cpotinf20trs_2024.nal <- filter(cpotinf20basetrs_2024.nal, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_2024.nal <- cpotinf20trs_2024.nal$p11_m2+cpotinf20trs_2024.nal$p11_m3+
                   cpotinf20trs_2024.nal$p11_m4+cpotinf20trs_2024.nal$p11_m5+
                   cpotinf20trs_2024.nal$p11_m6+cpotinf20trs_2024.nal$p11_m7+
                   cpotinf20trs_2024.nal$p11_m8
cpotinf20trs_2024.nal$hrs_trs <- cpotinf20trs_2024.nal$p11_h2+
                                 cpotinf20trs_2024.nal$p11_h3+
                                 cpotinf20trs_2024.nal$p11_h4+
                                 cpotinf20trs_2024.nal$p11_h5+
                                 cpotinf20trs_2024.nal$p11_h6+
                                 cpotinf20trs_2024.nal$p11_h7+
                                 cpotinf20trs_2024.nal$p11_h8+
                                 (mintrs_2024.nal/60)
d20trs_2024nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf20trs_2024.nal)
mediainf20trs_2024.nal <- svymean(~hrs_trs, d20trs_2024nal.inf)
svyttest(hrs_trs~0, d20trs_2024nal.inf, na = T)
cpotfor20basetrs_2024.nal <- filter(cpotfor20_jov.nal, eda>=20&eda<=24)
cpotfor20basetrs_2024.nal[is.na(cpotfor20basetrs_2024.nal)] <- 0
cpotfor20trs_2024.nal <- filter(cpotfor20basetrs_2024.nal, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_2024.nal <- cpotfor20trs_2024.nal$p11_m2+cpotfor20trs_2024.nal$p11_m3+
                   cpotfor20trs_2024.nal$p11_m4+cpotfor20trs_2024.nal$p11_m5+
                   cpotfor20trs_2024.nal$p11_m6+cpotfor20trs_2024.nal$p11_m7+
                   cpotfor20trs_2024.nal$p11_m8
cpotfor20trs_2024.nal$hrs_trs <- cpotfor20trs_2024.nal$p11_h2+
                                 cpotfor20trs_2024.nal$p11_h3+
                                 cpotfor20trs_2024.nal$p11_h4+
                                 cpotfor20trs_2024.nal$p11_h5+
                                 cpotfor20trs_2024.nal$p11_h6+
                                 cpotfor20trs_2024.nal$p11_h7+
                                 cpotfor20trs_2024.nal$p11_h8+
                                 (mintrs_2024.nal/60)
d20trs_2024nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor20trs_2024.nal)
mediafor20trs_2024.nal <- svymean(~hrs_trs, d20trs_2024nal.for)
svyttest(hrs_trs~0, d20trs_2024nal.for, na = T)
        # Principal actividad
cpotpt20act_2024.nal <- filter(cpotpt20_jov.nal, eda>=20&eda<=24)
cact20_2024nal.pt <- aggregate(fac~p3, cpotpt20act_2024.nal, sum, na.rm=T)
cact20_2024nal.pt$prop <- cact20_2024nal.pt$fac/sum(cact20_2024nal.pt$fac)
act20_2024nal.pt <- cact20_2024nal.pt[order(cact20_2024nal.pt[,3], 
                                            decreasing=T),]
cpotinf20act_2024.nal <- filter(cpotinf20_jov.nal, eda>=20&eda<=24)
cact20_2024nal.inf <- aggregate(fac~p3, cpotinf20act_2024.nal, sum, na.rm=T)
cact20_2024nal.inf$prop <- cact20_2024nal.inf$fac/sum(cact20_2024nal.inf$fac)
act20_2024nal.inf <- cact20_2024nal.inf[order(cact20_2024nal.inf[,3], 
                                              decreasing=T),]
cpotfor20act_2024.nal <- filter(cpotfor20_jov.nal, eda>=20&eda<=24)
cact20_2024nal.for <- aggregate(fac~p3, cpotfor20act_2024.nal, sum, na.rm=T)
cact20_2024nal.for$prop <- cact20_2024nal.for$fac/sum(cact20_2024nal.for$fac)
act20_2024nal.for <- cact20_2024nal.for[order(cact20_2024nal.for[,3], 
                                              decreasing=T),]
      # 25 a 29 anios
        # Escolaridad acumulada
cpotpt20esc_2529.nal <- filter(cpotpt20_jov.nal, eda>=25&eda<=29, anios_esc!=99)
d20esc_2529nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20esc_2529.nal)
mediapt20esc_2529.nal <- svymean(~anios_esc, d20esc_2529nal.pt)
svyttest(anios_esc~0, d20esc_2529nal.pt, na = T)
cpotinf20esc_2529.nal <- filter(cpotinf20_jov.nal, eda>=25&eda<=29, 
                                anios_esc!=99)
d20esc_2529nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf20esc_2529.nal)
mediainf20esc_2529.nal <- svymean(~anios_esc, d20esc_2529nal.inf)
svyttest(anios_esc~0, d20esc_2529nal.inf, na = T)
cpotfor20esc_2529.nal <- filter(cpotfor20_jov.nal, eda>=25&eda<=29, 
                                anios_esc!=99)
d20esc_2529nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor20esc_2529.nal)
mediafor20esc_2529.nal <- svymean(~anios_esc, d20esc_2529nal.for)
svyttest(anios_esc~0, d20esc_2529nal.for, na = T)
        # Jornada de trabajo
cpotpt20jor_2529.nal <- filter(cpotpt20_jov.nal, eda>=25&eda<=29, hrsocup!=0)
d20jor_2529nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20jor_2529.nal)
mediapt20jor_2529.nal <- svymean(~hrsocup, d20jor_2529nal.pt)
svyttest(hrsocup~0, d20jor_2529nal.pt, na = T)
cpotinf20jor_2529.nal <- filter(cpotinf20_jov.nal, eda>=25&eda<=29, hrsocup!=0)
d20jor_2529nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf20jor_2529.nal)
mediainf20jor_2529.nal <- svymean(~hrsocup, d20jor_2529nal.inf)
svyttest(hrsocup~0, d20jor_2529nal.inf, na = T)
cpotfor20jor_2529.nal <- filter(cpotfor20_jov.nal, eda>=25&eda<=29, hrsocup!=0)
d20jor_2529nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor20jor_2529.nal)
mediafor20jor_2529.nal <- svymean(~hrsocup, d20jor_2529nal.for)
svyttest(hrsocup~0, d20jor_2529nal.for, na = T)
        # Trabajo de reproduccion social
cpotpt20basetrs_2529.nal <- filter(cpotpt20_jov.nal, eda>=25&eda<=29)
cpotpt20basetrs_2529.nal[is.na(cpotpt20basetrs_2529.nal)] <- 0
cpotpt20trs_2529.nal <- filter(cpotpt20basetrs_2529.nal, p11_h2!=98, p11_h2!=99, 
                               p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                               p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                               p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrs_2529.nal <- cpotpt20trs_2529.nal$p11_m2+cpotpt20trs_2529.nal$p11_m3+
                   cpotpt20trs_2529.nal$p11_m4+cpotpt20trs_2529.nal$p11_m5+
                   cpotpt20trs_2529.nal$p11_m6+cpotpt20trs_2529.nal$p11_m7+
                   cpotpt20trs_2529.nal$p11_m8
cpotpt20trs_2529.nal$hrs_trs <- cpotpt20trs_2529.nal$p11_h2+
                                cpotpt20trs_2529.nal$p11_h3+
                                cpotpt20trs_2529.nal$p11_h4+
                                cpotpt20trs_2529.nal$p11_h5+
                                cpotpt20trs_2529.nal$p11_h6+
                                cpotpt20trs_2529.nal$p11_h7+
                                cpotpt20trs_2529.nal$p11_h8+(mintrs_2529.nal/60)
d20trs_2529nal.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20trs_2529.nal)
mediapt20trs_2529.nal <- svymean(~hrs_trs, d20trs_2529nal.pt)
svyttest(hrs_trs~0, d20trs_2529nal.pt, na = T)
cpotinf20basetrs_2529.nal <- filter(cpotinf20_jov.nal, eda>=25&eda<=29)
cpotinf20basetrs_2529.nal[is.na(cpotinf20basetrs_2529.nal)] <- 0
cpotinf20trs_2529.nal <- filter(cpotinf20basetrs_2529.nal, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_2529.nal <- cpotinf20trs_2529.nal$p11_m2+cpotinf20trs_2529.nal$p11_m3+
                   cpotinf20trs_2529.nal$p11_m4+cpotinf20trs_2529.nal$p11_m5+
                   cpotinf20trs_2529.nal$p11_m6+cpotinf20trs_2529.nal$p11_m7+
                   cpotinf20trs_2529.nal$p11_m8
cpotinf20trs_2529.nal$hrs_trs <- cpotinf20trs_2529.nal$p11_h2+
                                 cpotinf20trs_2529.nal$p11_h3+
                                 cpotinf20trs_2529.nal$p11_h4+
                                 cpotinf20trs_2529.nal$p11_h5+
                                 cpotinf20trs_2529.nal$p11_h6+
                                 cpotinf20trs_2529.nal$p11_h7+
                                 cpotinf20trs_2529.nal$p11_h8+
                                 (mintrs_2529.nal/60)
d20trs_2529nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotinf20trs_2529.nal)
mediainf20trs_2529.nal <- svymean(~hrs_trs, d20trs_2529nal.inf)
svyttest(hrs_trs~0, d20trs_2529nal.inf, na = T)
cpotfor20basetrs_2529.nal <- filter(cpotfor20_jov.nal, eda>=25&eda<=29)
cpotfor20basetrs_2529.nal[is.na(cpotfor20basetrs_2529.nal)] <- 0
cpotfor20trs_2529.nal <- filter(cpotfor20basetrs_2529.nal, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_2529.nal <- cpotfor20trs_2529.nal$p11_m2+cpotfor20trs_2529.nal$p11_m3+
                   cpotfor20trs_2529.nal$p11_m4+cpotfor20trs_2529.nal$p11_m5+
                   cpotfor20trs_2529.nal$p11_m6+cpotfor20trs_2529.nal$p11_m7+
                   cpotfor20trs_2529.nal$p11_m8
cpotfor20trs_2529.nal$hrs_trs <- cpotfor20trs_2529.nal$p11_h2+
                                 cpotfor20trs_2529.nal$p11_h3+
                                 cpotfor20trs_2529.nal$p11_h4+
                                 cpotfor20trs_2529.nal$p11_h5+
                                 cpotfor20trs_2529.nal$p11_h6+
                                 cpotfor20trs_2529.nal$p11_h7+
                                 cpotfor20trs_2529.nal$p11_h8+
                                 (mintrs_2529.nal/60)
d20trs_2529nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotfor20trs_2529.nal)
mediafor20trs_2529.nal <- svymean(~hrs_trs, d20trs_2529nal.for)
svyttest(hrs_trs~0, d20trs_2529nal.for, na = T)
        # Principal actividad
cpotpt20act_2529.nal <- filter(cpotpt20_jov.nal, eda>=25&eda<=29)
cact20_2529nal.pt <- aggregate(fac~p3, cpotpt20act_2529.nal, sum, na.rm=T)
cact20_2529nal.pt$prop <- cact20_2529nal.pt$fac/sum(cact20_2529nal.pt$fac)
act20_2529nal.pt <- cact20_2529nal.pt[order(cact20_2529nal.pt[,3], 
                                            decreasing=T),]
cpotinf20act_2529.nal <- filter(cpotinf20_jov.nal, eda>=25&eda<=29)
cact20_2529nal.inf <- aggregate(fac~p3, cpotinf20act_2529.nal, sum, na.rm=T)
cact20_2529nal.inf$prop <- cact20_2529nal.inf$fac/sum(cact20_2529nal.inf$fac)
act20_2529nal.inf <- cact20_2529nal.inf[order(cact20_2529nal.inf[,3], 
                                              decreasing=T),]
cpotfor20act_2529.nal <- filter(cpotfor20_jov.nal, eda>=25&eda<=29)
cact20_2529nal.for <- aggregate(fac~p3, cpotfor20act_2529.nal, sum, na.rm=T)
cact20_2529nal.for$prop <- cact20_2529nal.for$fac/sum(cact20_2529nal.for$fac)
act20_2529nal.for <- cact20_2529nal.for[order(cact20_2529nal.for[,3], 
                                              decreasing=T),]
    # Principales indicadores, escala ZMVM 2005
cpotpt05_jov.zmvm<- filter(cpot05corr_tot.zmvm, eda>=15&eda<=29)
cpotinf05_jov.zmvm <- filter(cpotinf05_tot.zmvm, eda>=15&eda<=29)
cpotfor05_jov.zmvm <- filter(cpotfor05_tot.zmvm, eda>=15&eda<=29)
sum(cpotinf05_jov.zmvm$fac)/sum(cpotpt05_jov.zmvm$fac)
sum(cpotfor05_jov.zmvm$fac)/sum(cpotpt05_jov.zmvm$fac)
cpotpt05_nojov.zmvm<- filter(cpot05corr_tot.zmvm, eda>=30&eda<=64)
cpotinf05_nojov.zmvm <- filter(cpotinf05_tot.zmvm, eda>=30&eda<=64)
cpotfor05_nojov.zmvm <- filter(cpotfor05_tot.zmvm, eda>=30&eda<=64)
sum(cpotinf05_nojov.zmvm$fac)/sum(cpotpt05_nojov.zmvm$fac)
sum(cpotfor05_nojov.zmvm$fac)/sum(cpotpt05_nojov.zmvm$fac)
    # Estadisticas de jovenes a escala ZMVM y por grupo etario, 2005
      # 15 a 19 anios
        # Escolaridad acumulada
cpotpt05esc_1519.zmvm <- filter(cpotpt05_jov.zmvm, eda<=19, anios_esc!=99)
d05esc_1519zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt05esc_1519.zmvm)
mediapt05esc_1519.zmvm <- svymean(~anios_esc, d05esc_1519zmvm.pt)
svyttest(anios_esc~0, d05esc_1519zmvm.pt, na = T)
cpotinf05esc_1519.zmvm <- filter(cpotinf05_jov.zmvm, eda<=19, anios_esc!=99)
d05esc_1519zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf05esc_1519.zmvm)
mediainf05esc_1519.zmvm <- svymean(~anios_esc, d05esc_1519zmvm.inf)
svyttest(anios_esc~0, d05esc_1519zmvm.inf, na = T)
cpotfor05esc_1519.zmvm <- filter(cpotfor05_jov.zmvm, eda<=19, anios_esc!=99)
d05esc_1519zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor05esc_1519.zmvm)
mediafor05esc_1519.zmvm <- svymean(~anios_esc, d05esc_1519zmvm.for)
svyttest(anios_esc~0, d05esc_1519zmvm.for, na = T)
        # Jornada de trabajo
cpotpt05jor_1519.zmvm <- filter(cpotpt05_jov.zmvm, eda<=19, hrsocup!=0)
d05jor_1519zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt05jor_1519.zmvm)
mediapt05jor_1519.zmvm <- svymean(~hrsocup, d05jor_1519zmvm.pt)
svyttest(hrsocup~0, d05jor_1519zmvm.pt, na = T)
cpotinf05jor_1519.zmvm <- filter(cpotinf05_jov.zmvm, eda<=19, hrsocup!=0)
d05jor_1519zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf05jor_1519.zmvm)
mediainf05jor_1519.zmvm <- svymean(~hrsocup, d05jor_1519zmvm.inf)
svyttest(hrsocup~0, d05jor_1519zmvm.inf, na = T)
cpotfor05jor_1519.zmvm <- filter(cpotfor05_jov.zmvm, eda<=19, hrsocup!=0)
d05jor_1519zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor05jor_1519.zmvm)
mediafor05jor_1519.zmvm <- svymean(~hrsocup, d05jor_1519zmvm.for)
svyttest(hrsocup~0, d05jor_1519zmvm.for, na = T)
        # Trabajo de reproduccion social
cpotpt05basetrs_1519.zmvm <- filter(cpotpt05_jov.zmvm, eda<=19)
cpotpt05basetrs_1519.zmvm[is.na(cpotpt05basetrs_1519.zmvm)] <- 0
cpotpt05trs_1519.zmvm <- filter(cpotpt05basetrs_1519.zmvm, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99,p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_1519.zmvm <- cpotpt05trs_1519.zmvm$p11_m2+cpotpt05trs_1519.zmvm$p11_m3+
                    cpotpt05trs_1519.zmvm$p11_m4+cpotpt05trs_1519.zmvm$p11_m5+
                    cpotpt05trs_1519.zmvm$p11_m6+cpotpt05trs_1519.zmvm$p11_m7+
                    cpotpt05trs_1519.zmvm$p11_m8
cpotpt05trs_1519.zmvm$hrs_trs <- cpotpt05trs_1519.zmvm$p11_h2+
                                 cpotpt05trs_1519.zmvm$p11_h3+
                                 cpotpt05trs_1519.zmvm$p11_h4+
                                 cpotpt05trs_1519.zmvm$p11_h5+
                                 cpotpt05trs_1519.zmvm$p11_h6+
                                 cpotpt05trs_1519.zmvm$p11_h7+
                                 cpotpt05trs_1519.zmvm$p11_h8+
                                 (mintrs_1519.zmvm/60)
d05trs_1519zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt05trs_1519.zmvm)
mediapt05trs_1519.zmvm <- svymean(~hrs_trs, d05trs_1519zmvm.pt)
svyttest(hrs_trs~0, d05trs_1519zmvm.pt, na = T)
cpotinf05basetrs_1519.zmvm <- filter(cpotinf05_jov.zmvm, eda<=19)
cpotinf05basetrs_1519.zmvm[is.na(cpotinf05basetrs_1519.zmvm)] <- 0
cpotinf05trs_1519.zmvm <- filter(cpotinf05basetrs_1519.zmvm, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_1519.zmvm <- cpotinf05trs_1519.zmvm$p11_m2+cpotinf05trs_1519.zmvm$p11_m3+
                    cpotinf05trs_1519.zmvm$p11_m4+cpotinf05trs_1519.zmvm$p11_m5+
                    cpotinf05trs_1519.zmvm$p11_m6+cpotinf05trs_1519.zmvm$p11_m7+
                    cpotinf05trs_1519.zmvm$p11_m8
cpotinf05trs_1519.zmvm$hrs_trs <- cpotinf05trs_1519.zmvm$p11_h2+
                                  cpotinf05trs_1519.zmvm$p11_h3+
                                  cpotinf05trs_1519.zmvm$p11_h4+
                                  cpotinf05trs_1519.zmvm$p11_h5+
                                  cpotinf05trs_1519.zmvm$p11_h6+
                                  cpotinf05trs_1519.zmvm$p11_h7+
                                  cpotinf05trs_1519.zmvm$p11_h8+
                                  (mintrs_1519.zmvm/60)
d05trs_1519zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf05trs_1519.zmvm)
mediainf05trs_1519.zmvm <- svymean(~hrs_trs, d05trs_1519zmvm.inf)
svyttest(hrs_trs~0, d05trs_1519zmvm.inf, na = T)
cpotfor05basetrs_1519.zmvm <- filter(cpotfor05_jov.zmvm, eda<=19)
cpotfor05basetrs_1519.zmvm[is.na(cpotfor05basetrs_1519.zmvm)] <- 0
cpotfor05trs_1519.zmvm <- filter(cpotfor05basetrs_1519.zmvm, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_1519.zmvm <- cpotfor05trs_1519.zmvm$p11_m2+cpotfor05trs_1519.zmvm$p11_m3+
                    cpotfor05trs_1519.zmvm$p11_m4+cpotfor05trs_1519.zmvm$p11_m5+
                    cpotfor05trs_1519.zmvm$p11_m6+cpotfor05trs_1519.zmvm$p11_m7+
                    cpotfor05trs_1519.zmvm$p11_m8
cpotfor05trs_1519.zmvm$hrs_trs <- cpotfor05trs_1519.zmvm$p11_h2+
                                  cpotfor05trs_1519.zmvm$p11_h3+
                                  cpotfor05trs_1519.zmvm$p11_h4+
                                  cpotfor05trs_1519.zmvm$p11_h5+
                                  cpotfor05trs_1519.zmvm$p11_h6+
                                  cpotfor05trs_1519.zmvm$p11_h7+
                                  cpotfor05trs_1519.zmvm$p11_h8+
                                  (mintrs_1519.zmvm/60)
d05trs_1519zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor05trs_1519.zmvm)
mediafor05trs_1519.zmvm <- svymean(~hrs_trs, d05trs_1519zmvm.for)
svyttest(hrs_trs~0, d05trs_1519zmvm.for, na = T)
        # Principal actividad
cpotpt05act_1519.zmvm <- filter(cpotpt05_jov.zmvm, eda<=19)
cact05_1519zmvm.pt <- aggregate(fac~p3, cpotpt05act_1519.zmvm, sum, na.rm=T)
cact05_1519zmvm.pt$prop <- cact05_1519zmvm.pt$fac/sum(cact05_1519zmvm.pt$fac)
act05_1519zmvm.pt <- cact05_1519zmvm.pt[order(cact05_1519zmvm.pt[,3], 
                                              decreasing=T),]
cpotinf05act_1519.zmvm <- filter(cpotinf05_jov.zmvm, eda<=19)
cact05_1519zmvm.inf <- aggregate(fac~p3, cpotinf05act_1519.zmvm, sum, na.rm=T)
cact05_1519zmvm.inf$prop <- cact05_1519zmvm.inf$fac/sum(cact05_1519zmvm.inf$fac)
act05_1519zmvm.inf <- cact05_1519zmvm.inf[order(cact05_1519zmvm.inf[,3], 
                                                decreasing=T),]
cpotfor05act_1519.zmvm <- filter(cpotfor05_jov.zmvm, eda<=19)
cact05_1519zmvm.for <- aggregate(fac~p3, cpotfor05act_1519.zmvm, sum, na.rm=T)
cact05_1519zmvm.for$prop <- cact05_1519zmvm.for$fac/sum(cact05_1519zmvm.for$fac)
act05_1519zmvm.for <- cact05_1519zmvm.for[order(cact05_1519zmvm.for[,3], 
                                                decreasing=T),]
      # 20 a 24 anios
        # Escolaridad acumulada
cpotpt05esc_2024.zmvm <- filter(cpotpt05_jov.zmvm, eda>=20&eda<=24, 
                                anios_esc!=99)
d05esc_2024zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt05esc_2024.zmvm)
mediapt05esc_2024.zmvm <- svymean(~anios_esc, d05esc_2024zmvm.pt)
svyttest(anios_esc~0, d05esc_2024zmvm.pt, na = T)
cpotinf05esc_2024.zmvm <- filter(cpotinf05_jov.zmvm, eda>=20&eda<=24, 
                                 anios_esc!=99)
d05esc_2024zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf05esc_2024.zmvm)
mediainf05esc_2024.zmvm <- svymean(~anios_esc, d05esc_2024zmvm.inf)
svyttest(anios_esc~0, d05esc_2024zmvm.inf, na = T)
cpotfor05esc_2024.zmvm <- filter(cpotfor05_jov.zmvm, eda>=20&eda<=24, 
                                 anios_esc!=99)
d05esc_2024zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor05esc_2024.zmvm)
mediafor05esc_2024.zmvm <- svymean(~anios_esc, d05esc_2024zmvm.for)
svyttest(anios_esc~0, d05esc_2024zmvm.for, na = T)
        # Jornada de trabajo
cpotpt05jor_2024.zmvm <- filter(cpotpt05_jov.zmvm, eda>=20&eda<=24, hrsocup!=0)
d05jor_2024zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt05jor_2024.zmvm)
mediapt05jor_2024.zmvm <- svymean(~hrsocup, d05jor_2024zmvm.pt)
svyttest(hrsocup~0, d05jor_2024zmvm.pt, na = T)
cpotinf05jor_2024.zmvm <- filter(cpotinf05_jov.zmvm, eda>=20&eda<=24, 
                                 hrsocup!=0)
d05jor_2024zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf05jor_2024.zmvm)
mediainf05jor_2024.zmvm <- svymean(~hrsocup, d05jor_2024zmvm.inf)
svyttest(hrsocup~0, d05jor_2024zmvm.inf, na = T)
cpotfor05jor_2024.zmvm <- filter(cpotfor05_jov.zmvm, eda>=20&eda<=24, 
                                 hrsocup!=0)
d05jor_2024zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor05jor_2024.zmvm)
mediafor05jor_2024.zmvm <- svymean(~hrsocup, d05jor_2024zmvm.for)
svyttest(hrsocup~0, d05jor_2024zmvm.for, na = T)
        # Trabajo de reproduccion social
cpotpt05basetrs_2024.zmvm <- filter(cpotpt05_jov.zmvm, eda>=20&eda<=24)
cpotpt05basetrs_2024.zmvm[is.na(cpotpt05basetrs_2024.zmvm)] <- 0
cpotpt05trs_2024.zmvm <- filter(cpotpt05basetrs_2024.zmvm, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_2024.zmvm <- cpotpt05trs_2024.zmvm$p11_m2+cpotpt05trs_2024.zmvm$p11_m3+
                    cpotpt05trs_2024.zmvm$p11_m4+cpotpt05trs_2024.zmvm$p11_m5+
                    cpotpt05trs_2024.zmvm$p11_m6+cpotpt05trs_2024.zmvm$p11_m7+
                    cpotpt05trs_2024.zmvm$p11_m8
cpotpt05trs_2024.zmvm$hrs_trs <- cpotpt05trs_2024.zmvm$p11_h2+
                                 cpotpt05trs_2024.zmvm$p11_h3+
                                 cpotpt05trs_2024.zmvm$p11_h4+
                                 cpotpt05trs_2024.zmvm$p11_h5+
                                 cpotpt05trs_2024.zmvm$p11_h6+
                                 cpotpt05trs_2024.zmvm$p11_h7+
                                 cpotpt05trs_2024.zmvm$p11_h8+
                                 (mintrs_2024.zmvm/60)
d05trs_2024zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt05trs_2024.zmvm)
mediapt05trs_2024.zmvm <- svymean(~hrs_trs, d05trs_2024zmvm.pt)
svyttest(hrs_trs~0, d05trs_2024zmvm.pt, na = T)
cpotinf05basetrs_2024.zmvm <- filter(cpotinf05_jov.zmvm, eda>=20&eda<=24)
cpotinf05basetrs_2024.zmvm[is.na(cpotinf05basetrs_2024.zmvm)] <- 0
cpotinf05trs_2024.zmvm <- filter(cpotinf05basetrs_2024.zmvm, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_2024.zmvm <- cpotinf05trs_2024.zmvm$p11_m2+cpotinf05trs_2024.zmvm$p11_m3+
                    cpotinf05trs_2024.zmvm$p11_m4+cpotinf05trs_2024.zmvm$p11_m5+
                    cpotinf05trs_2024.zmvm$p11_m6+cpotinf05trs_2024.zmvm$p11_m7+
                    cpotinf05trs_2024.zmvm$p11_m8
cpotinf05trs_2024.zmvm$hrs_trs <- cpotinf05trs_2024.zmvm$p11_h2+
                                  cpotinf05trs_2024.zmvm$p11_h3+
                                  cpotinf05trs_2024.zmvm$p11_h4+
                                  cpotinf05trs_2024.zmvm$p11_h5+
                                  cpotinf05trs_2024.zmvm$p11_h6+
                                  cpotinf05trs_2024.zmvm$p11_h7+
                                  cpotinf05trs_2024.zmvm$p11_h8+
                                  (mintrs_2024.zmvm/60)
d05trs_2024zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf05trs_2024.zmvm)
mediainf05trs_2024.zmvm <- svymean(~hrs_trs, d05trs_2024zmvm.inf)
svyttest(hrs_trs~0, d05trs_2024zmvm.inf, na = T)
cpotfor05basetrs_2024.zmvm <- filter(cpotfor05_jov.zmvm, eda>=20&eda<=24)
cpotfor05basetrs_2024.zmvm[is.na(cpotfor05basetrs_2024.zmvm)] <- 0
cpotfor05trs_2024.zmvm <- filter(cpotfor05basetrs_2024.zmvm, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_2024.zmvm <- cpotfor05trs_2024.zmvm$p11_m2+cpotfor05trs_2024.zmvm$p11_m3+
                    cpotfor05trs_2024.zmvm$p11_m4+cpotfor05trs_2024.zmvm$p11_m5+
                    cpotfor05trs_2024.zmvm$p11_m6+cpotfor05trs_2024.zmvm$p11_m7+
                    cpotfor05trs_2024.zmvm$p11_m8
cpotfor05trs_2024.zmvm$hrs_trs <- cpotfor05trs_2024.zmvm$p11_h2+
                                  cpotfor05trs_2024.zmvm$p11_h3+
                                  cpotfor05trs_2024.zmvm$p11_h4+
                                  cpotfor05trs_2024.zmvm$p11_h5+
                                  cpotfor05trs_2024.zmvm$p11_h6+
                                  cpotfor05trs_2024.zmvm$p11_h7+
                                  cpotfor05trs_2024.zmvm$p11_h8+
                                  (mintrs_2024.zmvm/60)
d05trs_2024zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor05trs_2024.zmvm)
mediafor05trs_2024.zmvm <- svymean(~hrs_trs, d05trs_2024zmvm.for)
svyttest(hrs_trs~0, d05trs_2024zmvm.for, na = T)
        # Principal actividad
cpotpt05act_2024.zmvm <- filter(cpotpt05_jov.zmvm, eda>=20&eda<=24)
cact05_2024zmvm.pt <- aggregate(fac~p3, cpotpt05act_2024.zmvm, sum, na.rm=T)
cact05_2024zmvm.pt$prop <- cact05_2024zmvm.pt$fac/sum(cact05_2024zmvm.pt$fac)
act05_2024zmvm.pt <- cact05_2024zmvm.pt[order(cact05_2024zmvm.pt[,3], 
                                              decreasing=T),]
cpotinf05act_2024.zmvm <- filter(cpotinf05_jov.zmvm, eda>=20&eda<=24)
cact05_2024zmvm.inf <- aggregate(fac~p3, cpotinf05act_2024.zmvm, sum, na.rm=T)
cact05_2024zmvm.inf$prop <- cact05_2024zmvm.inf$fac/sum(cact05_2024zmvm.inf$fac)
act05_2024zmvm.inf <- cact05_2024zmvm.inf[order(cact05_2024zmvm.inf[,3], 
                                                decreasing=T),]
cpotfor05act_2024.zmvm <- filter(cpotfor05_jov.zmvm, eda>=20&eda<=24)
cact05_2024zmvm.for <- aggregate(fac~p3, cpotfor05act_2024.zmvm, sum, na.rm=T)
cact05_2024zmvm.for$prop <- cact05_2024zmvm.for$fac/sum(cact05_2024zmvm.for$fac)
act05_2024zmvm.for <- cact05_2024zmvm.for[order(cact05_2024zmvm.for[,3], 
                                                decreasing=T),]
      # 25 a 29 anios
        # Escolaridad acumulada
cpotpt05esc_2529.zmvm <- filter(cpotpt05_jov.zmvm, eda>=25&eda<=29, 
                                anios_esc!=99)
d05esc_2529zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt05esc_2529.zmvm)
mediapt05esc_2529.zmvm <- svymean(~anios_esc, d05esc_2529zmvm.pt)
svyttest(anios_esc~0, d05esc_2529zmvm.pt, na = T)
cpotinf05esc_2529.zmvm <- filter(cpotinf05_jov.zmvm, eda>=25&eda<=29, 
                                 anios_esc!=99)
d05esc_2529zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf05esc_2529.zmvm)
mediainf05esc_2529.zmvm <- svymean(~anios_esc, d05esc_2529zmvm.inf)
svyttest(anios_esc~0, d05esc_2529zmvm.inf, na = T)
cpotfor05esc_2529.zmvm <- filter(cpotfor05_jov.zmvm, eda>=25&eda<=29, 
                                 anios_esc!=99)
d05esc_2529zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor05esc_2529.zmvm)
mediafor05esc_2529.zmvm <- svymean(~anios_esc, d05esc_2529zmvm.for)
svyttest(anios_esc~0, d05esc_2529zmvm.for, na = T)
        # Jornada de trabajo
cpotpt05jor_2529.zmvm <- filter(cpotpt05_jov.zmvm, eda>=25&eda<=29, hrsocup!=0)
d05jor_2529zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt05jor_2529.zmvm)
mediapt05jor_2529.zmvm <- svymean(~hrsocup, d05jor_2529zmvm.pt)
svyttest(hrsocup~0, d05jor_2529zmvm.pt, na = T)
cpotinf05jor_2529.zmvm <- filter(cpotinf05_jov.zmvm, eda>=25&eda<=29, 
                                 hrsocup!=0)
d05jor_2529zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf05jor_2529.zmvm)
mediainf05jor_2529.zmvm <- svymean(~hrsocup, d05jor_2529zmvm.inf)
svyttest(hrsocup~0, d05jor_2529zmvm.inf, na = T)
cpotfor05jor_2529.zmvm <- filter(cpotfor05_jov.zmvm, eda>=25&eda<=29, 
                                 hrsocup!=0)
d05jor_2529zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor05jor_2529.zmvm)
mediafor05jor_2529.zmvm <- svymean(~hrsocup, d05jor_2529zmvm.for)
svyttest(hrsocup~0, d05jor_2529zmvm.for, na = T)
        # Trabajo de reproduccion social
cpotpt05basetrs_2529.zmvm <- filter(cpotpt05_jov.zmvm, eda>=25&eda<=29)
cpotpt05basetrs_2529.zmvm[is.na(cpotpt05basetrs_2529.zmvm)] <- 0
cpotpt05trs_2529.zmvm <- filter(cpotpt05basetrs_2529.zmvm, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_2529.zmvm <- cpotpt05trs_2529.zmvm$p11_m2+cpotpt05trs_2529.zmvm$p11_m3+
                    cpotpt05trs_2529.zmvm$p11_m4+cpotpt05trs_2529.zmvm$p11_m5+
                    cpotpt05trs_2529.zmvm$p11_m6+cpotpt05trs_2529.zmvm$p11_m7+
                    cpotpt05trs_2529.zmvm$p11_m8
cpotpt05trs_2529.zmvm$hrs_trs <- cpotpt05trs_2529.zmvm$p11_h2+
                                 cpotpt05trs_2529.zmvm$p11_h3+
                                 cpotpt05trs_2529.zmvm$p11_h4+
                                 cpotpt05trs_2529.zmvm$p11_h5+
                                 cpotpt05trs_2529.zmvm$p11_h6+
                                 cpotpt05trs_2529.zmvm$p11_h7+
                                 cpotpt05trs_2529.zmvm$p11_h8+
                                 (mintrs_2529.zmvm/60)
d05trs_2529zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt05trs_2529.zmvm)
mediapt05trs_2529.zmvm <- svymean(~hrs_trs, d05trs_2529zmvm.pt)
svyttest(hrs_trs~0, d05trs_2529zmvm.pt, na = T)
cpotinf05basetrs_2529.zmvm <- filter(cpotinf05_jov.zmvm, eda>=25&eda<=29)
cpotinf05basetrs_2529.zmvm[is.na(cpotinf05basetrs_2529.zmvm)] <- 0
cpotinf05trs_2529.zmvm <- filter(cpotinf05basetrs_2529.zmvm, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_2529.zmvm <- cpotinf05trs_2529.zmvm$p11_m2+cpotinf05trs_2529.zmvm$p11_m3+
                    cpotinf05trs_2529.zmvm$p11_m4+cpotinf05trs_2529.zmvm$p11_m5+
                    cpotinf05trs_2529.zmvm$p11_m6+cpotinf05trs_2529.zmvm$p11_m7+
                    cpotinf05trs_2529.zmvm$p11_m8
cpotinf05trs_2529.zmvm$hrs_trs <- cpotinf05trs_2529.zmvm$p11_h2+
                                  cpotinf05trs_2529.zmvm$p11_h3+
                                  cpotinf05trs_2529.zmvm$p11_h4+
                                  cpotinf05trs_2529.zmvm$p11_h5+
                                  cpotinf05trs_2529.zmvm$p11_h6+
                                  cpotinf05trs_2529.zmvm$p11_h7+
                                  cpotinf05trs_2529.zmvm$p11_h8+
                                  (mintrs_2529.zmvm/60)
d05trs_2529zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf05trs_2529.zmvm)
mediainf05trs_2529.zmvm <- svymean(~hrs_trs, d05trs_2529zmvm.inf)
svyttest(hrs_trs~0, d05trs_2529zmvm.inf, na = T)
cpotfor05basetrs_2529.zmvm <- filter(cpotfor05_jov.zmvm, eda>=25&eda<=29)
cpotfor05basetrs_2529.zmvm[is.na(cpotfor05basetrs_2529.zmvm)] <- 0
cpotfor05trs_2529.zmvm <- filter(cpotfor05basetrs_2529.zmvm, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_2529.zmvm <- cpotfor05trs_2529.zmvm$p11_m2+cpotfor05trs_2529.zmvm$p11_m3+
                    cpotfor05trs_2529.zmvm$p11_m4+cpotfor05trs_2529.zmvm$p11_m5+
                    cpotfor05trs_2529.zmvm$p11_m6+cpotfor05trs_2529.zmvm$p11_m7+
                    cpotfor05trs_2529.zmvm$p11_m8
cpotfor05trs_2529.zmvm$hrs_trs <- cpotfor05trs_2529.zmvm$p11_h2+
                                  cpotfor05trs_2529.zmvm$p11_h3+
                                  cpotfor05trs_2529.zmvm$p11_h4+
                                  cpotfor05trs_2529.zmvm$p11_h5+
                                  cpotfor05trs_2529.zmvm$p11_h6+
                                  cpotfor05trs_2529.zmvm$p11_h7+
                                  cpotfor05trs_2529.zmvm$p11_h8+
                                  (mintrs_2529.zmvm/60)
d05trs_2529zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor05trs_2529.zmvm)
mediafor05trs_2529.zmvm <- svymean(~hrs_trs, d05trs_2529zmvm.for)
svyttest(hrs_trs~0, d05trs_2529zmvm.for, na = T)
        # Principal actividad
cpotpt05act_2529.zmvm <- filter(cpotpt05_jov.zmvm, eda>=25&eda<=29)
cact05_2529zmvm.pt <- aggregate(fac~p3, cpotpt05act_2529.zmvm, sum, na.rm=T)
cact05_2529zmvm.pt$prop <- cact05_2529zmvm.pt$fac/sum(cact05_2529zmvm.pt$fac)
act05_2529zmvm.pt <- cact05_2529zmvm.pt[order(cact05_2529zmvm.pt[,3], 
                                              decreasing=T),]
cpotinf05act_2529.zmvm <- filter(cpotinf05_jov.zmvm, eda>=25&eda<=29)
cact05_2529zmvm.inf <- aggregate(fac~p3, cpotinf05act_2529.zmvm, sum, na.rm=T)
cact05_2529zmvm.inf$prop <- cact05_2529zmvm.inf$fac/sum(cact05_2529zmvm.inf$fac)
act05_2529zmvm.inf <- cact05_2529zmvm.inf[order(cact05_2529zmvm.inf[,3], 
                                                decreasing=T),]
cpotfor05act_2529.zmvm <- filter(cpotfor05_jov.zmvm, eda>=25&eda<=29)
cact05_2529zmvm.for <- aggregate(fac~p3, cpotfor05act_2529.zmvm, sum, na.rm=T)
cact05_2529zmvm.for$prop <- cact05_2529zmvm.for$fac/sum(cact05_2529zmvm.for$fac)
act05_2529zmvm.for <- cact05_2529zmvm.for[order(cact05_2529zmvm.for[,3], 
                                                decreasing=T),]
    # Principales indicadores, escala ZMVM 2020
cpotpt20_jov.zmvm<- filter(cpot20corr_tot.zmvm, eda>=15&eda<=29)
cpotinf20_jov.zmvm <- filter(cpotinf20_tot.zmvm, eda>=15&eda<=29)
cpotfor20_jov.zmvm <- filter(cpotfor20_tot.zmvm, eda>=15&eda<=29)
sum(cpotinf20_jov.zmvm$fac)/sum(cpotpt20_jov.zmvm$fac)
sum(cpotfor20_jov.zmvm$fac)/sum(cpotpt20_jov.zmvm$fac)
cpotpt20_nojov.zmvm<- filter(cpot20corr_tot.zmvm, eda>=30&eda<=64)
cpotinf20_nojov.zmvm <- filter(cpotinf20_tot.zmvm, eda>=30&eda<=64)
cpotfor20_nojov.zmvm <- filter(cpotfor20_tot.zmvm, eda>=30&eda<=64)
sum(cpotinf20_nojov.zmvm$fac)/sum(cpotpt20_nojov.zmvm$fac)
sum(cpotfor20_nojov.zmvm$fac)/sum(cpotpt20_nojov.zmvm$fac)
    # Estadisticas de jovenes a escala ZMVM y por grupo etario, 2020
      # 15 a 19 anios
        # Escolaridad acumulada
cpotpt20esc_1519.zmvm <- filter(cpotpt20_jov.zmvm, eda<=19, anios_esc!=99)
d20esc_1519zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt20esc_1519.zmvm)
mediapt20esc_1519.zmvm <- svymean(~anios_esc, d20esc_1519zmvm.pt)
svyttest(anios_esc~0, d20esc_1519zmvm.pt, na = T)
cpotinf20esc_1519.zmvm <- filter(cpotinf20_jov.zmvm, eda<=19, anios_esc!=99)
d20esc_1519zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf20esc_1519.zmvm)
mediainf20esc_1519.zmvm <- svymean(~anios_esc, d20esc_1519zmvm.inf)
svyttest(anios_esc~0, d20esc_1519zmvm.inf, na = T)
cpotfor20esc_1519.zmvm <- filter(cpotfor20_jov.zmvm, eda<=19, anios_esc!=99)
d20esc_1519zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor20esc_1519.zmvm)
mediafor20esc_1519.zmvm <- svymean(~anios_esc, d20esc_1519zmvm.for)
svyttest(anios_esc~0, d20esc_1519zmvm.for, na = T)
        # Jornada de trabajo
cpotpt20jor_1519.zmvm <- filter(cpotpt20_jov.zmvm, eda<=19, hrsocup!=0)
d20jor_1519zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt20jor_1519.zmvm)
mediapt20jor_1519.zmvm <- svymean(~hrsocup, d20jor_1519zmvm.pt)
svyttest(hrsocup~0, d20jor_1519zmvm.pt, na = T)
cpotinf20jor_1519.zmvm <- filter(cpotinf20_jov.zmvm, eda<=19, hrsocup!=0)
d20jor_1519zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf20jor_1519.zmvm)
mediainf20jor_1519.zmvm <- svymean(~hrsocup, d20jor_1519zmvm.inf)
svyttest(hrsocup~0, d20jor_1519zmvm.inf, na = T)
cpotfor20jor_1519.zmvm <- filter(cpotfor20_jov.zmvm, eda<=19, hrsocup!=0)
d20jor_1519zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor20jor_1519.zmvm)
mediafor20jor_1519.zmvm <- svymean(~hrsocup, d20jor_1519zmvm.for)
svyttest(hrsocup~0, d20jor_1519zmvm.for, na = T)
        # Trabajo de reproduccion social
cpotpt20basetrs_1519.zmvm <- filter(cpotpt20_jov.zmvm, eda<=19)
cpotpt20basetrs_1519.zmvm[is.na(cpotpt20basetrs_1519.zmvm)] <- 0
cpotpt20trs_1519.zmvm <- filter(cpotpt20basetrs_1519.zmvm, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_1519.zmvm <- cpotpt20trs_1519.zmvm$p11_m2+cpotpt20trs_1519.zmvm$p11_m3+
                    cpotpt20trs_1519.zmvm$p11_m4+cpotpt20trs_1519.zmvm$p11_m5+
                    cpotpt20trs_1519.zmvm$p11_m6+cpotpt20trs_1519.zmvm$p11_m7+
                    cpotpt20trs_1519.zmvm$p11_m8
cpotpt20trs_1519.zmvm$hrs_trs <- cpotpt20trs_1519.zmvm$p11_h2+
                                 cpotpt20trs_1519.zmvm$p11_h3+
                                 cpotpt20trs_1519.zmvm$p11_h4+
                                 cpotpt20trs_1519.zmvm$p11_h5+
                                 cpotpt20trs_1519.zmvm$p11_h6+
                                 cpotpt20trs_1519.zmvm$p11_h7+
                                 cpotpt20trs_1519.zmvm$p11_h8+
                                 (mintrs_1519.zmvm/60)
d20trs_1519zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt20trs_1519.zmvm)
mediapt20trs_1519.zmvm <- svymean(~hrs_trs, d20trs_1519zmvm.pt)
svyttest(hrs_trs~0, d20trs_1519zmvm.pt, na = T)
cpotinf20basetrs_1519.zmvm <- filter(cpotinf20_jov.zmvm, eda<=19)
cpotinf20basetrs_1519.zmvm[is.na(cpotinf20basetrs_1519.zmvm)] <- 0
cpotinf20trs_1519.zmvm <- filter(cpotinf20basetrs_1519.zmvm, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_1519.zmvm <- cpotinf20trs_1519.zmvm$p11_m2+cpotinf20trs_1519.zmvm$p11_m3+
                    cpotinf20trs_1519.zmvm$p11_m4+cpotinf20trs_1519.zmvm$p11_m5+
                    cpotinf20trs_1519.zmvm$p11_m6+cpotinf20trs_1519.zmvm$p11_m7+
                    cpotinf20trs_1519.zmvm$p11_m8
cpotinf20trs_1519.zmvm$hrs_trs <- cpotinf20trs_1519.zmvm$p11_h2+
                                  cpotinf20trs_1519.zmvm$p11_h3+
                                  cpotinf20trs_1519.zmvm$p11_h4+
                                  cpotinf20trs_1519.zmvm$p11_h5+
                                  cpotinf20trs_1519.zmvm$p11_h6+
                                  cpotinf20trs_1519.zmvm$p11_h7+
                                  cpotinf20trs_1519.zmvm$p11_h8+
                                  (mintrs_1519.zmvm/60)
d20trs_1519zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf20trs_1519.zmvm)
mediainf20trs_1519.zmvm <- svymean(~hrs_trs, d20trs_1519zmvm.inf)
svyttest(hrs_trs~0, d20trs_1519zmvm.inf, na = T)
cpotfor20basetrs_1519.zmvm <- filter(cpotfor20_jov.zmvm, eda<=19)
cpotfor20basetrs_1519.zmvm[is.na(cpotfor20basetrs_1519.zmvm)] <- 0
cpotfor20trs_1519.zmvm <- filter(cpotfor20basetrs_1519.zmvm, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_1519.zmvm <- cpotfor20trs_1519.zmvm$p11_m2+cpotfor20trs_1519.zmvm$p11_m3+
                    cpotfor20trs_1519.zmvm$p11_m4+cpotfor20trs_1519.zmvm$p11_m5+
                    cpotfor20trs_1519.zmvm$p11_m6+cpotfor20trs_1519.zmvm$p11_m7+
                    cpotfor20trs_1519.zmvm$p11_m8
cpotfor20trs_1519.zmvm$hrs_trs <- cpotfor20trs_1519.zmvm$p11_h2+
                                  cpotfor20trs_1519.zmvm$p11_h3+
                                  cpotfor20trs_1519.zmvm$p11_h4+
                                  cpotfor20trs_1519.zmvm$p11_h5+
                                  cpotfor20trs_1519.zmvm$p11_h6+
                                  cpotfor20trs_1519.zmvm$p11_h7+
                                  cpotfor20trs_1519.zmvm$p11_h8+
                                  (mintrs_1519.zmvm/60)
d20trs_1519zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor20trs_1519.zmvm)
mediafor20trs_1519.zmvm <- svymean(~hrs_trs, d20trs_1519zmvm.for)
svyttest(hrs_trs~0, d20trs_1519zmvm.for, na = T)
        # Principal actividad
cpotpt20act_1519.zmvm <- filter(cpotpt20_jov.zmvm, eda<=19)
cact20_1519zmvm.pt <- aggregate(fac~p3, cpotpt20act_1519.zmvm, sum, na.rm=T)
cact20_1519zmvm.pt$prop <- cact20_1519zmvm.pt$fac/sum(cact20_1519zmvm.pt$fac)
act20_1519zmvm.pt <- cact20_1519zmvm.pt[order(cact20_1519zmvm.pt[,3], 
                                              decreasing=T),]
cpotinf20act_1519.zmvm <- filter(cpotinf20_jov.zmvm, eda<=19)
cact20_1519zmvm.inf <- aggregate(fac~p3, cpotinf20act_1519.zmvm, sum, na.rm=T)
cact20_1519zmvm.inf$prop <- cact20_1519zmvm.inf$fac/sum(cact20_1519zmvm.inf$fac)
act20_1519zmvm.inf <- cact20_1519zmvm.inf[order(cact20_1519zmvm.inf[,3], 
                                                decreasing=T),]
cpotfor20act_1519.zmvm <- filter(cpotfor20_jov.zmvm, eda<=19)
cact20_1519zmvm.for <- aggregate(fac~p3, cpotfor20act_1519.zmvm, sum, na.rm=T)
cact20_1519zmvm.for$prop <- cact20_1519zmvm.for$fac/sum(cact20_1519zmvm.for$fac)
act20_1519zmvm.for <- cact20_1519zmvm.for[order(cact20_1519zmvm.for[,3], 
                                                decreasing=T),]
      # 20 a 24 anios
        # Escolaridad acumulada
cpotpt20esc_2024.zmvm <- filter(cpotpt20_jov.zmvm, eda>=20&eda<=24, anios_esc!=99)
d20esc_2024zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt20esc_2024.zmvm)
mediapt20esc_2024.zmvm <- svymean(~anios_esc, d20esc_2024zmvm.pt)
svyttest(anios_esc~0, d20esc_2024zmvm.pt, na = T)
cpotinf20esc_2024.zmvm <- filter(cpotinf20_jov.zmvm, eda>=20&eda<=24, 
                                 anios_esc!=99)
d20esc_2024zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf20esc_2024.zmvm)
mediainf20esc_2024.zmvm <- svymean(~anios_esc, d20esc_2024zmvm.inf)
svyttest(anios_esc~0, d20esc_2024zmvm.inf, na = T)
cpotfor20esc_2024.zmvm <- filter(cpotfor20_jov.zmvm, eda>=20&eda<=24, 
                                 anios_esc!=99)
d20esc_2024zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor20esc_2024.zmvm)
mediafor20esc_2024.zmvm <- svymean(~anios_esc, d20esc_2024zmvm.for)
svyttest(anios_esc~0, d20esc_2024zmvm.for, na = T)
        # Jornada de trabajo
cpotpt20jor_2024.zmvm <- filter(cpotpt20_jov.zmvm, eda>=20&eda<=24, hrsocup!=0)
d20jor_2024zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt20jor_2024.zmvm)
mediapt20jor_2024.zmvm <- svymean(~hrsocup, d20jor_2024zmvm.pt)
svyttest(hrsocup~0, d20jor_2024zmvm.pt, na = T)
cpotinf20jor_2024.zmvm <- filter(cpotinf20_jov.zmvm, eda>=20&eda<=24, 
                                 hrsocup!=0)
d20jor_2024zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf20jor_2024.zmvm)
mediainf20jor_2024.zmvm <- svymean(~hrsocup, d20jor_2024zmvm.inf)
svyttest(hrsocup~0, d20jor_2024zmvm.inf, na = T)
cpotfor20jor_2024.zmvm <- filter(cpotfor20_jov.zmvm, eda>=20&eda<=24, 
                                 hrsocup!=0)
d20jor_2024zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor20jor_2024.zmvm)
mediafor20jor_2024.zmvm <- svymean(~hrsocup, d20jor_2024zmvm.for)
svyttest(hrsocup~0, d20jor_2024zmvm.for, na = T)
        # Trabajo de reproduccion social
cpotpt20basetrs_2024.zmvm <- filter(cpotpt20_jov.zmvm, eda>=20&eda<=24)
cpotpt20basetrs_2024.zmvm[is.na(cpotpt20basetrs_2024.zmvm)] <- 0
cpotpt20trs_2024.zmvm <- filter(cpotpt20basetrs_2024.zmvm, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_2024.zmvm <- cpotpt20trs_2024.zmvm$p11_m2+cpotpt20trs_2024.zmvm$p11_m3+
                    cpotpt20trs_2024.zmvm$p11_m4+cpotpt20trs_2024.zmvm$p11_m5+
                    cpotpt20trs_2024.zmvm$p11_m6+cpotpt20trs_2024.zmvm$p11_m7+
                    cpotpt20trs_2024.zmvm$p11_m8
cpotpt20trs_2024.zmvm$hrs_trs <- cpotpt20trs_2024.zmvm$p11_h2+
                                 cpotpt20trs_2024.zmvm$p11_h3+
                                 cpotpt20trs_2024.zmvm$p11_h4+
                                 cpotpt20trs_2024.zmvm$p11_h5+
                                 cpotpt20trs_2024.zmvm$p11_h6+
                                 cpotpt20trs_2024.zmvm$p11_h7+
                                 cpotpt20trs_2024.zmvm$p11_h8+
                                 (mintrs_2024.zmvm/60)
d20trs_2024zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt20trs_2024.zmvm)
mediapt20trs_2024.zmvm <- svymean(~hrs_trs, d20trs_2024zmvm.pt)
svyttest(hrs_trs~0, d20trs_2024zmvm.pt, na = T)
cpotinf20basetrs_2024.zmvm <- filter(cpotinf20_jov.zmvm, eda>=20&eda<=24)
cpotinf20basetrs_2024.zmvm[is.na(cpotinf20basetrs_2024.zmvm)] <- 0
cpotinf20trs_2024.zmvm <- filter(cpotinf20basetrs_2024.zmvm, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_2024.zmvm <- cpotinf20trs_2024.zmvm$p11_m2+cpotinf20trs_2024.zmvm$p11_m3+
                    cpotinf20trs_2024.zmvm$p11_m4+cpotinf20trs_2024.zmvm$p11_m5+
                    cpotinf20trs_2024.zmvm$p11_m6+cpotinf20trs_2024.zmvm$p11_m7+
                    cpotinf20trs_2024.zmvm$p11_m8
cpotinf20trs_2024.zmvm$hrs_trs <- cpotinf20trs_2024.zmvm$p11_h2+
                                  cpotinf20trs_2024.zmvm$p11_h3+
                                  cpotinf20trs_2024.zmvm$p11_h4+
                                  cpotinf20trs_2024.zmvm$p11_h5+
                                  cpotinf20trs_2024.zmvm$p11_h6+
                                  cpotinf20trs_2024.zmvm$p11_h7+
                                  cpotinf20trs_2024.zmvm$p11_h8+
                                  (mintrs_2024.zmvm/60)
d20trs_2024zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf20trs_2024.zmvm)
mediainf20trs_2024.zmvm <- svymean(~hrs_trs, d20trs_2024zmvm.inf)
svyttest(hrs_trs~0, d20trs_2024zmvm.inf, na = T)
cpotfor20basetrs_2024.zmvm <- filter(cpotfor20_jov.zmvm, eda>=20&eda<=24)
cpotfor20basetrs_2024.zmvm[is.na(cpotfor20basetrs_2024.zmvm)] <- 0
cpotfor20trs_2024.zmvm <- filter(cpotfor20basetrs_2024.zmvm, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_2024.zmvm <- cpotfor20trs_2024.zmvm$p11_m2+cpotfor20trs_2024.zmvm$p11_m3+
                    cpotfor20trs_2024.zmvm$p11_m4+cpotfor20trs_2024.zmvm$p11_m5+
                    cpotfor20trs_2024.zmvm$p11_m6+cpotfor20trs_2024.zmvm$p11_m7+
                    cpotfor20trs_2024.zmvm$p11_m8
cpotfor20trs_2024.zmvm$hrs_trs <- cpotfor20trs_2024.zmvm$p11_h2+
                                  cpotfor20trs_2024.zmvm$p11_h3+
                                  cpotfor20trs_2024.zmvm$p11_h4+
                                  cpotfor20trs_2024.zmvm$p11_h5+
                                  cpotfor20trs_2024.zmvm$p11_h6+
                                  cpotfor20trs_2024.zmvm$p11_h7+
                                  cpotfor20trs_2024.zmvm$p11_h8+
                                  (mintrs_2024.zmvm/60)
d20trs_2024zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor20trs_2024.zmvm)
mediafor20trs_2024.zmvm <- svymean(~hrs_trs, d20trs_2024zmvm.for)
svyttest(hrs_trs~0, d20trs_2024zmvm.for, na = T)
        # Principal actividad
cpotpt20act_2024.zmvm <- filter(cpotpt20_jov.zmvm, eda>=20&eda<=24)
cact20_2024zmvm.pt <- aggregate(fac~p3, cpotpt20act_2024.zmvm, sum, na.rm=T)
cact20_2024zmvm.pt$prop <- cact20_2024zmvm.pt$fac/sum(cact20_2024zmvm.pt$fac)
act20_2024zmvm.pt <- cact20_2024zmvm.pt[order(cact20_2024zmvm.pt[,3], 
                                              decreasing=T),]
cpotinf20act_2024.zmvm <- filter(cpotinf20_jov.zmvm, eda>=20&eda<=24)
cact20_2024zmvm.inf <- aggregate(fac~p3, cpotinf20act_2024.zmvm, sum, na.rm=T)
cact20_2024zmvm.inf$prop <- cact20_2024zmvm.inf$fac/sum(cact20_2024zmvm.inf$fac)
act20_2024zmvm.inf <- cact20_2024zmvm.inf[order(cact20_2024zmvm.inf[,3], 
                                                decreasing=T),]
cpotfor20act_2024.zmvm <- filter(cpotfor20_jov.zmvm, eda>=20&eda<=24)
cact20_2024zmvm.for <- aggregate(fac~p3, cpotfor20act_2024.zmvm, sum, na.rm=T)
cact20_2024zmvm.for$prop <- cact20_2024zmvm.for$fac/sum(cact20_2024zmvm.for$fac)
act20_2024zmvm.for <- cact20_2024zmvm.for[order(cact20_2024zmvm.for[,3], 
                                                decreasing=T),]
      # 25 a 29 anios
        # Escolaridad acumulada
cpotpt20esc_2529.zmvm <- filter(cpotpt20_jov.zmvm, eda>=25&eda<=29, 
                                anios_esc!=99)
d20esc_2529zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt20esc_2529.zmvm)
mediapt20esc_2529.zmvm <- svymean(~anios_esc, d20esc_2529zmvm.pt)
svyttest(anios_esc~0, d20esc_2529zmvm.pt, na = T)
cpotinf20esc_2529.zmvm <- filter(cpotinf20_jov.zmvm, eda>=25&eda<=29, 
                                 anios_esc!=99)
d20esc_2529zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf20esc_2529.zmvm)
mediainf20esc_2529.zmvm <- svymean(~anios_esc, d20esc_2529zmvm.inf)
svyttest(anios_esc~0, d20esc_2529zmvm.inf, na = T)
cpotfor20esc_2529.zmvm <- filter(cpotfor20_jov.zmvm, eda>=25&eda<=29, 
                                 anios_esc!=99)
d20esc_2529zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor20esc_2529.zmvm)
mediafor20esc_2529.zmvm <- svymean(~anios_esc, d20esc_2529zmvm.for)
svyttest(anios_esc~0, d20esc_2529zmvm.for, na = T)
        # Jornada de trabajo
cpotpt20jor_2529.zmvm <- filter(cpotpt20_jov.zmvm, eda>=25&eda<=29, hrsocup!=0)
d20jor_2529zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt20jor_2529.zmvm)
mediapt20jor_2529.zmvm <- svymean(~hrsocup, d20jor_2529zmvm.pt)
svyttest(hrsocup~0, d20jor_2529zmvm.pt, na = T)
cpotinf20jor_2529.zmvm <- filter(cpotinf20_jov.zmvm, eda>=25&eda<=29, 
                                 hrsocup!=0)
d20jor_2529zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf20jor_2529.zmvm)
mediainf20jor_2529.zmvm <- svymean(~hrsocup, d20jor_2529zmvm.inf)
svyttest(hrsocup~0, d20jor_2529zmvm.inf, na = T)
cpotfor20jor_2529.zmvm <- filter(cpotfor20_jov.zmvm, eda>=25&eda<=29, 
                                 hrsocup!=0)
d20jor_2529zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor20jor_2529.zmvm)
mediafor20jor_2529.zmvm <- svymean(~hrsocup, d20jor_2529zmvm.for)
svyttest(hrsocup~0, d20jor_2529zmvm.for, na = T)
        # Trabajo de reproduccion social
cpotpt20basetrs_2529.zmvm <- filter(cpotpt20_jov.zmvm, eda>=25&eda<=29)
cpotpt20basetrs_2529.zmvm[is.na(cpotpt20basetrs_2529.zmvm)] <- 0
cpotpt20trs_2529.zmvm <- filter(cpotpt20basetrs_2529.zmvm, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_2529.zmvm <- cpotpt20trs_2529.zmvm$p11_m2+cpotpt20trs_2529.zmvm$p11_m3+
                    cpotpt20trs_2529.zmvm$p11_m4+cpotpt20trs_2529.zmvm$p11_m5+
                    cpotpt20trs_2529.zmvm$p11_m6+cpotpt20trs_2529.zmvm$p11_m7+
                    cpotpt20trs_2529.zmvm$p11_m8
cpotpt20trs_2529.zmvm$hrs_trs <- cpotpt20trs_2529.zmvm$p11_h2+
                                 cpotpt20trs_2529.zmvm$p11_h3+
                                 cpotpt20trs_2529.zmvm$p11_h4+
                                 cpotpt20trs_2529.zmvm$p11_h5+
                                 cpotpt20trs_2529.zmvm$p11_h6+
                                 cpotpt20trs_2529.zmvm$p11_h7+
                                 cpotpt20trs_2529.zmvm$p11_h8+
                                 (mintrs_2529.zmvm/60)
d20trs_2529zmvm.pt <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                data=cpotpt20trs_2529.zmvm)
mediapt20trs_2529.zmvm <- svymean(~hrs_trs, d20trs_2529zmvm.pt)
svyttest(hrs_trs~0, d20trs_2529zmvm.pt, na = T)
cpotinf20basetrs_2529.zmvm <- filter(cpotinf20_jov.zmvm, eda>=25&eda<=29)
cpotinf20basetrs_2529.zmvm[is.na(cpotinf20basetrs_2529.zmvm)] <- 0
cpotinf20trs_2529.zmvm <- filter(cpotinf20basetrs_2529.zmvm, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_2529.zmvm <- cpotinf20trs_2529.zmvm$p11_m2+cpotinf20trs_2529.zmvm$p11_m3+
                    cpotinf20trs_2529.zmvm$p11_m4+cpotinf20trs_2529.zmvm$p11_m5+
                    cpotinf20trs_2529.zmvm$p11_m6+cpotinf20trs_2529.zmvm$p11_m7+
                    cpotinf20trs_2529.zmvm$p11_m8
cpotinf20trs_2529.zmvm$hrs_trs <- cpotinf20trs_2529.zmvm$p11_h2+
                                  cpotinf20trs_2529.zmvm$p11_h3+
                                  cpotinf20trs_2529.zmvm$p11_h4+
                                  cpotinf20trs_2529.zmvm$p11_h5+
                                  cpotinf20trs_2529.zmvm$p11_h6+
                                  cpotinf20trs_2529.zmvm$p11_h7+
                                  cpotinf20trs_2529.zmvm$p11_h8+
                                  (mintrs_2529.zmvm/60)
d20trs_2529zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotinf20trs_2529.zmvm)
mediainf20trs_2529.zmvm <- svymean(~hrs_trs, d20trs_2529zmvm.inf)
svyttest(hrs_trs~0, d20trs_2529zmvm.inf, na = T)
cpotfor20basetrs_2529.zmvm <- filter(cpotfor20_jov.zmvm, eda>=25&eda<=29)
cpotfor20basetrs_2529.zmvm[is.na(cpotfor20basetrs_2529.zmvm)] <- 0
cpotfor20trs_2529.zmvm <- filter(cpotfor20basetrs_2529.zmvm, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98,  
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_2529.zmvm <- cpotfor20trs_2529.zmvm$p11_m2+cpotfor20trs_2529.zmvm$p11_m3+
                    cpotfor20trs_2529.zmvm$p11_m4+cpotfor20trs_2529.zmvm$p11_m5+
                    cpotfor20trs_2529.zmvm$p11_m6+cpotfor20trs_2529.zmvm$p11_m7+
                    cpotfor20trs_2529.zmvm$p11_m8
cpotfor20trs_2529.zmvm$hrs_trs <- cpotfor20trs_2529.zmvm$p11_h2+
                                  cpotfor20trs_2529.zmvm$p11_h3+
                                  cpotfor20trs_2529.zmvm$p11_h4+
                                  cpotfor20trs_2529.zmvm$p11_h5+
                                  cpotfor20trs_2529.zmvm$p11_h6+
                                  cpotfor20trs_2529.zmvm$p11_h7+
                                  cpotfor20trs_2529.zmvm$p11_h8+
                                  (mintrs_2529.zmvm/60)
d20trs_2529zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotfor20trs_2529.zmvm)
mediafor20trs_2529.zmvm <- svymean(~hrs_trs, d20trs_2529zmvm.for)
svyttest(hrs_trs~0, d20trs_2529zmvm.for, na = T)
        # Principal actividad
cpotpt20act_2529.zmvm <- filter(cpotpt20_jov.zmvm, eda>=25&eda<=29)
cact20_2529zmvm.pt <- aggregate(fac~p3, cpotpt20act_2529.zmvm, sum, na.rm=T)
cact20_2529zmvm.pt$prop <- cact20_2529zmvm.pt$fac/sum(cact20_2529zmvm.pt$fac)
act20_2529zmvm.pt <- cact20_2529zmvm.pt[order(cact20_2529zmvm.pt[,3], 
                                              decreasing=T),]
cpotinf20act_2529.zmvm <- filter(cpotinf20_jov.zmvm, eda>=25&eda<=29)
cact20_2529zmvm.inf <- aggregate(fac~p3, cpotinf20act_2529.zmvm, sum, na.rm=T)
cact20_2529zmvm.inf$prop <- cact20_2529zmvm.inf$fac/sum(cact20_2529zmvm.inf$fac)
act20_2529zmvm.inf <- cact20_2529zmvm.inf[order(cact20_2529zmvm.inf[,3], 
                                                decreasing=T),]
cpotfor20act_2529.zmvm <- filter(cpotfor20_jov.zmvm, eda>=25&eda<=29)
cact20_2529zmvm.for <- aggregate(fac~p3, cpotfor20act_2529.zmvm, sum, na.rm=T)
cact20_2529zmvm.for$prop <- cact20_2529zmvm.for$fac/sum(cact20_2529zmvm.for$fac)
act20_2529zmvm.for <- cact20_2529zmvm.for[order(cact20_2529zmvm.for[,3], 
                                                decreasing=T),]
    # Vector de extraccion de datos
      # Empleo formal e informal por grupo etario, escala y año
        # Guardado de principales variables
potpt05_jov.nal <- sum(cpotpt05_jov.nal$fac)
potinf05_jov.nal <- sum(cpotinf05_jov.nal$fac)
potfor05_jov.nal <- sum(cpotfor05_jov.nal$fac)
potpt05_nojov.nal <- sum(cpotpt05_nojov.nal$fac)
potinf05_nojov.nal <- sum(cpotinf05_nojov.nal$fac)
potfor05_nojov.nal <- sum(cpotfor05_nojov.nal$fac)
potpt05_jov.zmvm <- sum(cpotpt05_jov.zmvm$fac)
potinf05_jov.zmvm <- sum(cpotinf05_jov.zmvm$fac)
potfor05_jov.zmvm <- sum(cpotfor05_jov.zmvm$fac)
potpt05_nojov.zmvm <- sum(cpotpt05_nojov.zmvm$fac)
potinf05_nojov.zmvm <- sum(cpotinf05_nojov.zmvm$fac)
potfor05_nojov.zmvm <- sum(cpotfor05_nojov.zmvm$fac)
potpt20_jov.nal <- sum(cpotpt20_jov.nal$fac)
potinf20_jov.nal <- sum(cpotinf20_jov.nal$fac)
potfor20_jov.nal <- sum(cpotfor20_jov.nal$fac)
potpt20_nojov.nal <- sum(cpotpt20_nojov.nal$fac)
potinf20_nojov.nal <- sum(cpotinf20_nojov.nal$fac)
potfor20_nojov.nal <- sum(cpotfor20_nojov.nal$fac)
potpt20_jov.zmvm <- sum(cpotpt20_jov.zmvm$fac)
potinf20_jov.zmvm <- sum(cpotinf20_jov.zmvm$fac)
potfor20_jov.zmvm <- sum(cpotfor20_jov.zmvm$fac)
potpt20_nojov.zmvm <- sum(cpotpt20_nojov.zmvm$fac)
potinf20_nojov.zmvm <- sum(cpotinf20_nojov.zmvm$fac)
potfor20_nojov.zmvm <- sum(cpotfor20_nojov.zmvm$fac)
        # Vector
extrac_emp.tot <- c(potpt05_jov.nal, potinf05_jov.nal, potfor05_jov.nal, 
                    potpt05_nojov.nal, potinf05_nojov.nal, potfor05_nojov.nal, 
                    potpt05_jov.zmvm, potinf05_jov.zmvm, potfor05_jov.zmvm, 
                    potpt05_nojov.zmvm, potinf05_nojov.zmvm, 
                    potfor05_nojov.zmvm, potpt20_jov.nal, potinf20_jov.nal, 
                    potfor20_jov.nal, potpt20_nojov.nal, potinf20_nojov.nal, 
                    potfor20_nojov.nal, potpt20_jov.zmvm, potinf20_jov.zmvm, 
                    potfor20_jov.zmvm, potpt20_nojov.zmvm, potinf20_nojov.zmvm, 
                    potfor20_nojov.zmvm)
      # Empleo formal e informal juvenil por grupo etario y año 
        # Escala ZMVM
          # Filtrado de principales variables
cpotpt05_1519.zmvm <- filter(cpotpt05_jov.zmvm, eda<=19) 
cpotpt05_2024.zmvm <- filter(cpotpt05_jov.zmvm, eda>=20&eda<=24)
cpotpt05_2529.zmvm <- filter(cpotpt05_jov.zmvm, eda>=25&eda<=29)
cpotinf05_1519.zmvm <- filter(cpotinf05_jov.zmvm, eda<=19) 
cpotinf05_2024.zmvm <- filter(cpotinf05_jov.zmvm, eda>=20&eda<=24)
cpotinf05_2529.zmvm <- filter(cpotinf05_jov.zmvm, eda>=25&eda<=29)
cpotfor05_1519.zmvm <- filter(cpotfor05_jov.zmvm, eda<=19) 
cpotfor05_2024.zmvm <- filter(cpotfor05_jov.zmvm, eda>=20&eda<=24)
cpotfor05_2529.zmvm <- filter(cpotfor05_jov.zmvm, eda>=25&eda<=29)
cpotpt20_1519.zmvm <- filter(cpotpt20_jov.zmvm, eda<=19) 
cpotpt20_2024.zmvm <- filter(cpotpt20_jov.zmvm, eda>=20&eda<=24)
cpotpt20_2529.zmvm <- filter(cpotpt20_jov.zmvm, eda>=25&eda<=29)
cpotinf20_1519.zmvm <- filter(cpotinf20_jov.zmvm, eda<=19) 
cpotinf20_2024.zmvm <- filter(cpotinf20_jov.zmvm, eda>=20&eda<=24)
cpotinf20_2529.zmvm <- filter(cpotinf20_jov.zmvm, eda>=25&eda<=29)
cpotfor20_1519.zmvm <- filter(cpotfor20_jov.zmvm, eda<=19) 
cpotfor20_2024.zmvm <- filter(cpotfor20_jov.zmvm, eda>=20&eda<=24)
cpotfor20_2529.zmvm <- filter(cpotfor20_jov.zmvm, eda>=25&eda<=29)
          # Guardado de principales variables
potpt05_1519.zmvm <- sum(cpotpt05_1519.zmvm$fac) 
potpt05_2024.zmvm <- sum(cpotpt05_2024.zmvm$fac)
potpt05_2529.zmvm <- sum(cpotpt05_2529.zmvm$fac)
potinf05_1519.zmvm <- sum(cpotinf05_1519.zmvm$fac) 
potinf05_2024.zmvm <- sum(cpotinf05_2024.zmvm$fac)
potinf05_2529.zmvm <- sum(cpotinf05_2529.zmvm$fac)
potfor05_1519.zmvm <- sum(cpotfor05_1519.zmvm$fac) 
potfor05_2024.zmvm <- sum(cpotfor05_2024.zmvm$fac)
potfor05_2529.zmvm <- sum(cpotfor05_2529.zmvm$fac)
potpt20_1519.zmvm <- sum(cpotpt20_1519.zmvm$fac) 
potpt20_2024.zmvm <- sum(cpotpt20_2024.zmvm$fac)
potpt20_2529.zmvm <- sum(cpotpt20_2529.zmvm$fac)
potinf20_1519.zmvm <- sum(cpotinf20_1519.zmvm$fac) 
potinf20_2024.zmvm <- sum(cpotinf20_2024.zmvm$fac)
potinf20_2529.zmvm <- sum(cpotinf20_2529.zmvm$fac)
potfor20_1519.zmvm <- sum(cpotfor20_1519.zmvm$fac) 
potfor20_2024.zmvm <- sum(cpotfor20_2024.zmvm$fac)
potfor20_2529.zmvm <- sum(cpotfor20_2529.zmvm$fac)
          # Vector
extrac_emp.jovzmvm <- c(potpt05_1519.zmvm, potpt05_2024.zmvm, potpt05_2529.zmvm,
                        potinf05_1519.zmvm, potinf05_2024.zmvm, 
                        potinf05_2529.zmvm, potfor05_1519.zmvm, 
                        potfor05_2024.zmvm, potfor05_2529.zmvm, 
                        potpt20_1519.zmvm, potpt20_2024.zmvm, potpt20_2529.zmvm, 
                        potinf20_1519.zmvm, potinf20_2024.zmvm, 
                        potinf20_2529.zmvm, potfor20_1519.zmvm, 
                        potfor20_2024.zmvm, potfor20_2529.zmvm) 
        # Escala nacional urbana
          # Filtrado de principales variables
cpotpt05_1519.nal <- filter(cpotpt05_jov.nal, eda<=19) 
cpotpt05_2024.nal <- filter(cpotpt05_jov.nal, eda>=20&eda<=24)
cpotpt05_2529.nal <- filter(cpotpt05_jov.nal, eda>=25&eda<=29)
cpotinf05_1519.nal <- filter(cpotinf05_jov.nal, eda<=19) 
cpotinf05_2024.nal <- filter(cpotinf05_jov.nal, eda>=20&eda<=24)
cpotinf05_2529.nal <- filter(cpotinf05_jov.nal, eda>=25&eda<=29)
cpotfor05_1519.nal <- filter(cpotfor05_jov.nal, eda<=19) 
cpotfor05_2024.nal <- filter(cpotfor05_jov.nal, eda>=20&eda<=24)
cpotfor05_2529.nal <- filter(cpotfor05_jov.nal, eda>=25&eda<=29)
cpotpt20_1519.nal <- filter(cpotpt20_jov.nal, eda<=19) 
cpotpt20_2024.nal <- filter(cpotpt20_jov.nal, eda>=20&eda<=24)
cpotpt20_2529.nal <- filter(cpotpt20_jov.nal, eda>=25&eda<=29)
cpotinf20_1519.nal <- filter(cpotinf20_jov.nal, eda<=19) 
cpotinf20_2024.nal <- filter(cpotinf20_jov.nal, eda>=20&eda<=24)
cpotinf20_2529.nal <- filter(cpotinf20_jov.nal, eda>=25&eda<=29)
cpotfor20_1519.nal <- filter(cpotfor20_jov.nal, eda<=19) 
cpotfor20_2024.nal <- filter(cpotfor20_jov.nal, eda>=20&eda<=24)
cpotfor20_2529.nal <- filter(cpotfor20_jov.nal, eda>=25&eda<=29)
          # Guardado de principales variables
potpt05_1519.nal <- sum(cpotpt05_1519.nal$fac) 
potpt05_2024.nal <- sum(cpotpt05_2024.nal$fac)
potpt05_2529.nal <- sum(cpotpt05_2529.nal$fac)
potinf05_1519.nal <- sum(cpotinf05_1519.nal$fac) 
potinf05_2024.nal <- sum(cpotinf05_2024.nal$fac)
potinf05_2529.nal <- sum(cpotinf05_2529.nal$fac)
potfor05_1519.nal <- sum(cpotfor05_1519.nal$fac) 
potfor05_2024.nal <- sum(cpotfor05_2024.nal$fac)
potfor05_2529.nal <- sum(cpotfor05_2529.nal$fac)
potpt20_1519.nal <- sum(cpotpt20_1519.nal$fac) 
potpt20_2024.nal <- sum(cpotpt20_2024.nal$fac)
potpt20_2529.nal <- sum(cpotpt20_2529.nal$fac)
potinf20_1519.nal <- sum(cpotinf20_1519.nal$fac) 
potinf20_2024.nal <- sum(cpotinf20_2024.nal$fac)
potinf20_2529.nal <- sum(cpotinf20_2529.nal$fac)
potfor20_1519.nal <- sum(cpotfor20_1519.nal$fac) 
potfor20_2024.nal <- sum(cpotfor20_2024.nal$fac)
potfor20_2529.nal <- sum(cpotfor20_2529.nal$fac)
          # Vector
extrac_emp.jovnal <- c(potpt05_1519.nal, potpt05_2024.nal, potpt05_2529.nal,
                       potinf05_1519.nal, potinf05_2024.nal, 
                       potinf05_2529.nal, potfor05_1519.nal, 
                       potfor05_2024.nal, potfor05_2529.nal, 
                       potpt20_1519.nal, potpt20_2024.nal, potpt20_2529.nal, 
                       potinf20_1519.nal, potinf20_2024.nal, 
                       potinf20_2529.nal, potfor20_1519.nal, 
                       potfor20_2024.nal, potfor20_2529.nal)
      # Empleo formal e informal juvenil por grupo etario, características y año 
        # Vector de caracteristicas basicas a escala nacional urbana y ZMVM
extrac_emp.comp <- c(mediapt05esc_1519.nal, mediapt05esc_2024.nal, 
                     mediapt05esc_2529.nal, mediapt05jor_1519.nal, 
                     mediapt05jor_2024.nal, mediapt05jor_2529.nal, 
                     mediapt05trs_1519.nal, mediapt05trs_2024.nal, 
                     mediapt05trs_2529.nal, mediainf05esc_1519.nal, 
                     mediainf05esc_2024.nal, mediainf05esc_2529.nal, 
                     mediainf05jor_1519.nal, mediainf05jor_2024.nal, 
                     mediainf05jor_2529.nal, mediainf05trs_1519.nal, 
                     mediainf05trs_2024.nal, mediainf05trs_2529.nal, 
                     mediafor05esc_1519.nal, mediafor05esc_2024.nal, 
                     mediafor05esc_2529.nal, mediafor05jor_1519.nal, 
                     mediafor05jor_2024.nal, mediafor05jor_2529.nal, 
                     mediafor05trs_1519.nal, mediafor05trs_2024.nal, 
                     mediafor05trs_2529.nal, mediapt05esc_1519.zmvm, 
                     mediapt05esc_2024.zmvm, mediapt05esc_2529.zmvm, 
                     mediapt05jor_1519.zmvm, mediapt05jor_2024.zmvm, 
                     mediapt05jor_2529.zmvm, mediapt05trs_1519.zmvm, 
                     mediapt05trs_2024.zmvm, mediapt05trs_2529.zmvm, 
                     mediainf05esc_1519.zmvm, mediainf05esc_2024.zmvm, 
                     mediainf05esc_2529.zmvm, mediainf05jor_1519.zmvm, 
                     mediainf05jor_2024.zmvm, mediainf05jor_2529.zmvm, 
                     mediainf05trs_1519.zmvm, mediainf05trs_2024.zmvm, 
                     mediainf05trs_2529.zmvm, mediafor05esc_1519.zmvm, 
                     mediafor05esc_2024.zmvm, mediafor05esc_2529.zmvm, 
                     mediafor05jor_1519.zmvm, mediafor05jor_2024.zmvm, 
                     mediafor05jor_2529.zmvm, mediafor05trs_1519.zmvm, 
                     mediafor05trs_2024.zmvm, mediafor05trs_2529.zmvm, 
                     mediapt20esc_1519.nal, mediapt20esc_2024.nal, 
                     mediapt20esc_2529.nal, mediapt20jor_1519.nal, 
                     mediapt20jor_2024.nal, mediapt20jor_2529.nal, 
                     mediapt20trs_1519.nal, mediapt20trs_2024.nal, 
                     mediapt20trs_2529.nal, mediainf20esc_1519.nal, 
                     mediainf20esc_2024.nal, mediainf20esc_2529.nal, 
                     mediainf20jor_1519.nal, mediainf20jor_2024.nal, 
                     mediainf20jor_2529.nal, mediainf20trs_1519.nal, 
                     mediainf20trs_2024.nal, mediainf20trs_2529.nal, 
                     mediafor20esc_1519.nal, mediafor20esc_2024.nal, 
                     mediafor20esc_2529.nal, mediafor20jor_1519.nal, 
                     mediafor20jor_2024.nal, mediafor20jor_2529.nal, 
                     mediafor20trs_1519.nal, mediafor20trs_2024.nal, 
                     mediafor20trs_2529.nal, mediapt20esc_1519.zmvm, 
                     mediapt20esc_2024.zmvm, mediapt20esc_2529.zmvm, 
                     mediapt20jor_1519.zmvm, mediapt20jor_2024.zmvm, 
                     mediapt20jor_2529.zmvm, mediapt20trs_1519.zmvm, 
                     mediapt20trs_2024.zmvm, mediapt20trs_2529.zmvm, 
                     mediainf20esc_1519.zmvm, mediainf20esc_2024.zmvm, 
                     mediainf20esc_2529.zmvm, mediainf20jor_1519.zmvm, 
                     mediainf20jor_2024.zmvm, mediainf20jor_2529.zmvm, 
                     mediainf20trs_1519.zmvm, mediainf20trs_2024.zmvm, 
                     mediainf20trs_2529.zmvm, mediafor20esc_1519.zmvm, 
                     mediafor20esc_2024.zmvm, mediafor20esc_2529.zmvm, 
                     mediafor20jor_1519.zmvm, mediafor20jor_2024.zmvm, 
                     mediafor20jor_2529.zmvm, mediafor20trs_1519.zmvm, 
                     mediafor20trs_2024.zmvm, mediafor20trs_2529.zmvm)
        # Vector de actividades a escala nacional urbana y ZMVM
extrac_emp.activ <- rbind((act05_1519nal.pt [1:4, ]), NA, 
                          (act05_2024nal.pt [1:4, ]), NA, 
                          (act05_2529nal.pt [1:4, ]) , NA, 
                          (act05_1519nal.inf [1:4, ]) , NA, 
                          (act05_2024nal.inf [1:4, ]), NA, 
                          (act05_2529nal.inf [1:4, ]), NA, 
                          (act05_1519nal.for [1:4, ]), NA, 
                          (act05_2024nal.for [1:4, ]), NA, 
                          (act05_2529nal.for [1:4, ]), NA, 
                          (act05_1519zmvm.pt [1:4, ]), NA, 
                          (act05_2024zmvm.pt [1:4, ]), NA, 
                          (act05_2529zmvm.pt [1:4, ]) , NA, 
                          (act05_1519zmvm.inf [1:4, ]) , NA, 
                          (act05_2024zmvm.inf [1:4, ]), NA, 
                          (act05_2529zmvm.inf [1:4, ]), NA, 
                          (act05_1519zmvm.for [1:4, ]), NA, 
                          (act05_2024zmvm.for [1:4, ]), NA, 
                          (act05_2529zmvm.for [1:4, ]), NA, 
                          (act20_1519nal.pt [1:4, ]), NA, 
                          (act20_2024nal.pt [1:4, ]), NA, 
                          (act20_2529nal.pt [1:4, ]) , NA, 
                          (act20_1519nal.inf [1:4, ]) , NA, 
                          (act20_2024nal.inf [1:4, ]), NA, 
                          (act20_2529nal.inf [1:4, ]), NA, 
                          (act20_1519nal.for [1:4, ]), NA, 
                          (act20_2024nal.for [1:4, ]), NA, 
                          (act20_2529nal.for [1:4, ]), NA, 
                          (act20_1519zmvm.pt [1:4, ]), NA, 
                          (act20_2024zmvm.pt [1:4, ]), NA, 
                          (act20_2529zmvm.pt [1:4, ]) , NA, 
                          (act20_1519zmvm.inf [1:4, ]) , NA, 
                          (act20_2024zmvm.inf [1:4, ]), NA, 
                          (act20_2529zmvm.inf [1:4, ]), NA, 
                          (act20_1519zmvm.for [1:4, ]), NA, 
                          (act20_2024zmvm.for [1:4, ]), NA, 
                          (act20_2529zmvm.for [1:4, ]))
    # Estadisticas para jovenes y no jovenes por escala y anio
      # Escala urbana nacional 2005
        # Escolaridad acumulada
cpotpt05esc_jovt.nal <- filter(cpotpt05_jov.nal, anios_esc!=99)
d05esc_jovtnal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05esc_jovt.nal)
media05esc_jovt.nal <- svymean(~anios_esc, d05esc_jovtnal)
svyttest(anios_esc~0, d05esc_jovtnal, na = T)
cpotpt05esc_nojovt.nal <- filter(cpotpt05_nojov.nal, anios_esc!=99)
d05esc_nojovtnal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt05esc_nojovt.nal)
media05esc_nojovt.nal <- svymean(~anios_esc, d05esc_nojovtnal)
svyttest(anios_esc~0, d05esc_nojovtnal, na = T)
        # Jornada de trabajo
cpotpt05jor_jovt.nal <- filter(cpotpt05_jov.nal, hrsocup!=0)
d05jor_jovtnal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05jor_jovt.nal)
media05jor_jovt.nal <- svymean(~hrsocup, d05jor_jovtnal)
svyttest(hrsocup~0, d05jor_jovtnal, na = T)
cpotpt05jor_nojovt.nal <- filter(cpotpt05_nojov.nal, hrsocup!=0)
d05jor_nojovtnal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt05jor_nojovt.nal)
media05jor_nojovt.nal <- svymean(~hrsocup, d05jor_nojovtnal)
svyttest(hrsocup~0, d05jor_nojovtnal, na = T)
        # Trabajo de reproducción social
cpotpt05basetrs_jovt.nal <- filter(cpotpt05_jov.nal)
cpotpt05basetrs_jovt.nal[is.na(cpotpt05basetrs_jovt.nal)] <- 0
cpotpt05trs_jovt.nal <- filter(cpotpt05basetrs_jovt.nal, p11_h2!=98, p11_h2!=99, 
                               p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                               p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                               p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrs_jovt.nal <- cpotpt05trs_jovt.nal$p11_m2+cpotpt05trs_jovt.nal$p11_m3+
                   cpotpt05trs_jovt.nal$p11_m4+cpotpt05trs_jovt.nal$p11_m5+
                   cpotpt05trs_jovt.nal$p11_m6+cpotpt05trs_jovt.nal$p11_m7+
                   cpotpt05trs_jovt.nal$p11_m8
cpotpt05trs_jovt.nal$hrs_trs <- cpotpt05trs_jovt.nal$p11_h2+
                                cpotpt05trs_jovt.nal$p11_h3+
                                cpotpt05trs_jovt.nal$p11_h4+
                                cpotpt05trs_jovt.nal$p11_h5+
                                cpotpt05trs_jovt.nal$p11_h6+
                                cpotpt05trs_jovt.nal$p11_h7+
                                cpotpt05trs_jovt.nal$p11_h8+(mintrs_jovt.nal/60)
d05trs_jovtnal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05trs_jovt.nal)
media05trs_jovt.nal <- svymean(~hrs_trs, d05trs_jovtnal)
svyttest(hrs_trs~0, d05trs_jovtnal, na = T)
cpotpt05basetrs_nojovt.nal <- filter(cpotpt05_nojov.nal)
cpotpt05basetrs_nojovt.nal[is.na(cpotpt05basetrs_nojovt.nal)] <- 0
cpotpt05trs_nojovt.nal <- filter(cpotpt05basetrs_nojovt.nal, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_nojovt.nal <- cpotpt05trs_nojovt.nal$p11_m2+
                     cpotpt05trs_nojovt.nal$p11_m3+
                     cpotpt05trs_nojovt.nal$p11_m4+
                     cpotpt05trs_nojovt.nal$p11_m5+
                     cpotpt05trs_nojovt.nal$p11_m6+
                     cpotpt05trs_nojovt.nal$p11_m7+
                     cpotpt05trs_nojovt.nal$p11_m8
cpotpt05trs_nojovt.nal$hrs_trs <- cpotpt05trs_nojovt.nal$p11_h2+
                                  cpotpt05trs_nojovt.nal$p11_h3+
                                  cpotpt05trs_nojovt.nal$p11_h4+
                                  cpotpt05trs_nojovt.nal$p11_h5+
                                  cpotpt05trs_nojovt.nal$p11_h6+
                                  cpotpt05trs_nojovt.nal$p11_h7+
                                  cpotpt05trs_nojovt.nal$p11_h8+
                                  (mintrs_nojovt.nal/60)
d05trs_nojovtnal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt05trs_nojovt.nal)
media05trs_nojovt.nal <- svymean(~hrs_trs, d05trs_nojovtnal)
svyttest(hrs_trs~0, d05trs_nojovtnal, na = T)
        # Principal actividad
cpotpt05act_jovt.nal <- filter(cpotpt05_jov.nal)
cact05_jovtnal <- aggregate(fac~p3, cpotpt05act_jovt.nal, sum, na.rm=T)
cact05_jovtnal$prop <- cact05_jovtnal$fac/sum(cact05_jovtnal$fac)
act05_jovtnal <- cact05_jovtnal[order(cact05_jovtnal[,3], decreasing=T),]
cpotpt05act_nojovt.nal <- filter(cpotpt05_nojov.nal)
cact05_nojovtnal <- aggregate(fac~p3, cpotpt05act_nojovt.nal, sum, na.rm=T)
cact05_nojovtnal$prop <- cact05_nojovtnal$fac/sum(cact05_nojovtnal$fac)
act05_nojovtnal <- cact05_nojovtnal[order(cact05_nojovtnal[,3], decreasing=T),]
      # Escala urbana nacional 2020
        # Escolaridad acumulada
cpotpt20esc_jovt.nal <- filter(cpotpt20_jov.nal, anios_esc!=99)
d20esc_jovtnal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotpt20esc_jovt.nal)
media20esc_jovt.nal <- svymean(~anios_esc, d20esc_jovtnal)
svyttest(anios_esc~0, d20esc_jovtnal, na = T)
cpotpt20esc_nojovt.nal <- filter(cpotpt20_nojov.nal, anios_esc!=99)
d20esc_nojovtnal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20esc_nojovt.nal)
media20esc_nojovt.nal <- svymean(~anios_esc, d20esc_nojovtnal)
svyttest(anios_esc~0, d20esc_nojovtnal, na = T)
        # Jornada de trabajo
cpotpt20jor_jovt.nal <- filter(cpotpt20_jov.nal, hrsocup!=0)
d20jor_jovtnal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotpt20jor_jovt.nal)
media20jor_jovt.nal <- svymean(~hrsocup, d20jor_jovtnal)
svyttest(hrsocup~0, d20jor_jovtnal, na = T)
cpotpt20jor_nojovt.nal <- filter(cpotpt20_nojov.nal, hrsocup!=0)
d20jor_nojovtnal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20jor_nojovt.nal)
media20jor_nojovt.nal <- svymean(~hrsocup, d20jor_nojovtnal)
svyttest(hrsocup~0, d20jor_nojovtnal, na = T)
        # Trabajo de reproducción social
cpotpt20basetrs_jovt.nal <- filter(cpotpt20_jov.nal)
cpotpt20basetrs_jovt.nal[is.na(cpotpt20basetrs_jovt.nal)] <- 0
cpotpt20trs_jovt.nal <- filter(cpotpt20basetrs_jovt.nal, p11_h2!=98, p11_h2!=99, 
                               p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                               p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                               p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrs_jovt.nal <- cpotpt20trs_jovt.nal$p11_m2+cpotpt20trs_jovt.nal$p11_m3+
                   cpotpt20trs_jovt.nal$p11_m4+cpotpt20trs_jovt.nal$p11_m5+
                   cpotpt20trs_jovt.nal$p11_m6+cpotpt20trs_jovt.nal$p11_m7+
                   cpotpt20trs_jovt.nal$p11_m8
cpotpt20trs_jovt.nal$hrs_trs <- cpotpt20trs_jovt.nal$p11_h2+
                                cpotpt20trs_jovt.nal$p11_h3+
                                cpotpt20trs_jovt.nal$p11_h4+
                                cpotpt20trs_jovt.nal$p11_h5+
                                cpotpt20trs_jovt.nal$p11_h6+
                                cpotpt20trs_jovt.nal$p11_h7+
                                cpotpt20trs_jovt.nal$p11_h8+(mintrs_jovt.nal/60)
d20trs_jovtnal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotpt20trs_jovt.nal)
media20trs_jovt.nal <- svymean(~hrs_trs, d20trs_jovtnal)
svyttest(hrs_trs~0, d20trs_jovtnal, na = T)
cpotpt20basetrs_nojovt.nal <- filter(cpotpt20_nojov.nal)
cpotpt20basetrs_nojovt.nal[is.na(cpotpt20basetrs_nojovt.nal)] <- 0
cpotpt20trs_nojovt.nal <- filter(cpotpt20basetrs_nojovt.nal, p11_h2!=98, 
                                 p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                 p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                                 p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                 p11_h8!=99)
mintrs_nojovt.nal <- cpotpt20trs_nojovt.nal$p11_m2+
                     cpotpt20trs_nojovt.nal$p11_m3+
                     cpotpt20trs_nojovt.nal$p11_m4+
                     cpotpt20trs_nojovt.nal$p11_m5+
                     cpotpt20trs_nojovt.nal$p11_m6+
                     cpotpt20trs_nojovt.nal$p11_m7+
                     cpotpt20trs_nojovt.nal$p11_m8
cpotpt20trs_nojovt.nal$hrs_trs <- cpotpt20trs_nojovt.nal$p11_h2+
                                  cpotpt20trs_nojovt.nal$p11_h3+
                                  cpotpt20trs_nojovt.nal$p11_h4+
                                  cpotpt20trs_nojovt.nal$p11_h5+
                                  cpotpt20trs_nojovt.nal$p11_h6+
                                  cpotpt20trs_nojovt.nal$p11_h7+
                                  cpotpt20trs_nojovt.nal$p11_h8+
                                  (mintrs_nojovt.nal/60)
d20trs_nojovtnal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20trs_nojovt.nal)
media20trs_nojovt.nal <- svymean(~hrs_trs, d20trs_nojovtnal)
svyttest(hrs_trs~0, d20trs_nojovtnal, na = T)
        # Principal actividad
cpotpt20act_jovt.nal <- filter(cpotpt20_jov.nal)
cact20_jovtnal <- aggregate(fac~p3, cpotpt20act_jovt.nal, sum, na.rm=T)
cact20_jovtnal$prop <- cact20_jovtnal$fac/sum(cact20_jovtnal$fac)
act20_jovtnal <- cact20_jovtnal[order(cact20_jovtnal[,3], decreasing=T),]
cpotpt20act_nojovt.nal <- filter(cpotpt20_nojov.nal)
cact20_nojovtnal <- aggregate(fac~p3, cpotpt20act_nojovt.nal, sum, na.rm=T)
cact20_nojovtnal$prop <- cact20_nojovtnal$fac/sum(cact20_nojovtnal$fac)
act20_nojovtnal <- cact20_nojovtnal[order(cact20_nojovtnal[,3], decreasing=T),]
      # Escala ZMVM 2005
        # Escolaridad acumulada
cpotpt05esc_jovt.zmvm <- filter(cpotpt05_jov.zmvm, anios_esc!=99)
d05esc_jovtzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt05esc_jovt.zmvm)
media05esc_jovt.zmvm <- svymean(~anios_esc, d05esc_jovtzmvm)
svyttest(anios_esc~0, d05esc_jovtzmvm, na = T)
cpotpt05esc_nojovt.zmvm <- filter(cpotpt05_nojov.zmvm, anios_esc!=99)
d05esc_nojovtzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05esc_nojovt.zmvm)
media05esc_nojovt.zmvm <- svymean(~anios_esc, d05esc_nojovtzmvm)
svyttest(anios_esc~0, d05esc_nojovtzmvm, na = T)
        # Jornada de trabajo
cpotpt05jor_jovt.zmvm <- filter(cpotpt05_jov.zmvm, hrsocup!=0)
d05jor_jovtzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt05jor_jovt.zmvm)
media05jor_jovt.zmvm <- svymean(~hrsocup, d05jor_jovtzmvm)
svyttest(hrsocup~0, d05jor_jovtzmvm, na = T)
cpotpt05jor_nojovt.zmvm <- filter(cpotpt05_nojov.zmvm, hrsocup!=0)
d05jor_nojovtzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05jor_nojovt.zmvm)
media05jor_nojovt.zmvm <- svymean(~hrsocup, d05jor_nojovtzmvm)
svyttest(hrsocup~0, d05jor_nojovtzmvm, na = T)
        # Trabajo de reproducción social
cpotpt05basetrs_jovt.zmvm <- filter(cpotpt05_jov.zmvm)
cpotpt05basetrs_jovt.zmvm[is.na(cpotpt05basetrs_jovt.zmvm)] <- 0
cpotpt05trs_jovt.zmvm <- filter(cpotpt05basetrs_jovt.zmvm, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_jovt.zmvm <- cpotpt05trs_jovt.zmvm$p11_m2+cpotpt05trs_jovt.zmvm$p11_m3+
                    cpotpt05trs_jovt.zmvm$p11_m4+cpotpt05trs_jovt.zmvm$p11_m5+
                    cpotpt05trs_jovt.zmvm$p11_m6+cpotpt05trs_jovt.zmvm$p11_m7+
                    cpotpt05trs_jovt.zmvm$p11_m8
cpotpt05trs_jovt.zmvm$hrs_trs <- cpotpt05trs_jovt.zmvm$p11_h2+
                                 cpotpt05trs_jovt.zmvm$p11_h3+
                                 cpotpt05trs_jovt.zmvm$p11_h4+
                                 cpotpt05trs_jovt.zmvm$p11_h5+
                                 cpotpt05trs_jovt.zmvm$p11_h6+
                                 cpotpt05trs_jovt.zmvm$p11_h7+
                                 cpotpt05trs_jovt.zmvm$p11_h8+
                                 (mintrs_jovt.zmvm/60)
d05trs_jovtzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt05trs_jovt.zmvm)
media05trs_jovt.zmvm <- svymean(~hrs_trs, d05trs_jovtzmvm)
svyttest(hrs_trs~0, d05trs_jovtzmvm, na = T)
cpotpt05basetrs_nojovt.zmvm <- filter(cpotpt05_nojov.zmvm)
cpotpt05basetrs_nojovt.zmvm[is.na(cpotpt05basetrs_nojovt.zmvm)] <- 0
cpotpt05trs_nojovt.zmvm <- filter(cpotpt05basetrs_nojovt.zmvm, p11_h2!=98, 
                                  p11_h2!=99, p11_h3!=98, p11_h3!=99, 
                                  p11_h4!=98, p11_h4!=99, p11_h5!=98, 
                                  p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                                  p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                  p11_h8!=99)
mintrs_nojovt.zmvm <- cpotpt05trs_nojovt.zmvm$p11_m2+
                      cpotpt05trs_nojovt.zmvm$p11_m3+
                      cpotpt05trs_nojovt.zmvm$p11_m4+
                      cpotpt05trs_nojovt.zmvm$p11_m5+
                      cpotpt05trs_nojovt.zmvm$p11_m6+
                      cpotpt05trs_nojovt.zmvm$p11_m7+
                      cpotpt05trs_nojovt.zmvm$p11_m8
cpotpt05trs_nojovt.zmvm$hrs_trs <- cpotpt05trs_nojovt.zmvm$p11_h2+
                                   cpotpt05trs_nojovt.zmvm$p11_h3+
                                   cpotpt05trs_nojovt.zmvm$p11_h4+
                                   cpotpt05trs_nojovt.zmvm$p11_h5+
                                   cpotpt05trs_nojovt.zmvm$p11_h6+
                                   cpotpt05trs_nojovt.zmvm$p11_h7+
                                   cpotpt05trs_nojovt.zmvm$p11_h8+
                                   (mintrs_nojovt.zmvm/60)
d05trs_nojovtzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05trs_nojovt.zmvm)
media05trs_nojovt.zmvm <- svymean(~hrs_trs, d05trs_nojovtzmvm)
svyttest(hrs_trs~0, d05trs_nojovtzmvm, na = T)
        # Principal actividad
cpotpt05act_jovt.zmvm <- filter(cpotpt05_jov.zmvm)
cact05_jovtzmvm <- aggregate(fac~p3, cpotpt05act_jovt.zmvm, sum, na.rm=T)
cact05_jovtzmvm$prop <- cact05_jovtzmvm$fac/sum(cact05_jovtzmvm$fac)
act05_jovtzmvm <- cact05_jovtzmvm[order(cact05_jovtzmvm[,3], decreasing=T),]
cpotpt05act_nojovt.zmvm <- filter(cpotpt05_nojov.zmvm)
cact05_nojovtzmvm <- aggregate(fac~p3, cpotpt05act_nojovt.zmvm, sum, na.rm=T)
cact05_nojovtzmvm$prop <- cact05_nojovtzmvm$fac/sum(cact05_nojovtzmvm$fac)
act05_nojovtzmvm <- cact05_nojovtzmvm[order(cact05_nojovtzmvm[,3], 
                                            decreasing=T),]
      # Escala ZMVM 2020
        # Escolaridad acumulada
cpotpt20esc_jovt.zmvm <- filter(cpotpt20_jov.zmvm, anios_esc!=99)
d20esc_jovtzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt20esc_jovt.zmvm)
media20esc_jovt.zmvm <- svymean(~anios_esc, d20esc_jovtzmvm)
svyttest(anios_esc~0, d20esc_jovtzmvm, na = T)
cpotpt20esc_nojovt.zmvm <- filter(cpotpt20_nojov.zmvm, anios_esc!=99)
d20esc_nojovtzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20esc_nojovt.zmvm)
media20esc_nojovt.zmvm <- svymean(~anios_esc, d20esc_nojovtzmvm)
svyttest(anios_esc~0, d20esc_nojovtzmvm, na = T)
        # Jornada de trabajo
cpotpt20jor_jovt.zmvm <- filter(cpotpt20_jov.zmvm, hrsocup!=0)
d20jor_jovtzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt20jor_jovt.zmvm)
media20jor_jovt.zmvm <- svymean(~hrsocup, d20jor_jovtzmvm)
svyttest(hrsocup~0, d20jor_jovtzmvm, na = T)
cpotpt20jor_nojovt.zmvm <- filter(cpotpt20_nojov.zmvm, hrsocup!=0)
d20jor_nojovtzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20jor_nojovt.zmvm)
media20jor_nojovt.zmvm <- svymean(~hrsocup, d20jor_nojovtzmvm)
svyttest(hrsocup~0, d20jor_nojovtzmvm, na = T)
        # Trabajo de reproducción social
cpotpt20basetrs_jovt.zmvm <- filter(cpotpt20_jov.zmvm)
cpotpt20basetrs_jovt.zmvm[is.na(cpotpt20basetrs_jovt.zmvm)] <- 0
cpotpt20trs_jovt.zmvm <- filter(cpotpt20basetrs_jovt.zmvm, p11_h2!=98, 
                                p11_h2!=99, p11_h3!=98, p11_h3!=99, p11_h4!=98, 
                                p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                                p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                p11_h8!=99)
mintrs_jovt.zmvm <- cpotpt20trs_jovt.zmvm$p11_m2+cpotpt20trs_jovt.zmvm$p11_m3+
                    cpotpt20trs_jovt.zmvm$p11_m4+cpotpt20trs_jovt.zmvm$p11_m5+
                    cpotpt20trs_jovt.zmvm$p11_m6+cpotpt20trs_jovt.zmvm$p11_m7+
                    cpotpt20trs_jovt.zmvm$p11_m8
cpotpt20trs_jovt.zmvm$hrs_trs <- cpotpt20trs_jovt.zmvm$p11_h2+
                                 cpotpt20trs_jovt.zmvm$p11_h3+
                                 cpotpt20trs_jovt.zmvm$p11_h4+
                                 cpotpt20trs_jovt.zmvm$p11_h5+
                                 cpotpt20trs_jovt.zmvm$p11_h6+
                                 cpotpt20trs_jovt.zmvm$p11_h7+
                                 cpotpt20trs_jovt.zmvm$p11_h8+
                                 (mintrs_jovt.zmvm/60)
d20trs_jovtzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt20trs_jovt.zmvm)
media20trs_jovt.zmvm <- svymean(~hrs_trs, d20trs_jovtzmvm)
svyttest(hrs_trs~0, d20trs_jovtzmvm, na = T)
cpotpt20basetrs_nojovt.zmvm <- filter(cpotpt20_nojov.zmvm)
cpotpt20basetrs_nojovt.zmvm[is.na(cpotpt20basetrs_nojovt.zmvm)] <- 0
cpotpt20trs_nojovt.zmvm <- filter(cpotpt20basetrs_nojovt.zmvm, p11_h2!=98, 
                                  p11_h2!=99, p11_h3!=98, p11_h3!=99, 
                                  p11_h4!=98, p11_h4!=99, p11_h5!=98, 
                                  p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                                  p11_h7!=98, p11_h7!=99, p11_h8!=98, 
                                  p11_h8!=99)
mintrs_nojovt.zmvm <- cpotpt20trs_nojovt.zmvm$p11_m2+
                      cpotpt20trs_nojovt.zmvm$p11_m3+
                      cpotpt20trs_nojovt.zmvm$p11_m4+
                      cpotpt20trs_nojovt.zmvm$p11_m5+
                      cpotpt20trs_nojovt.zmvm$p11_m6+
                      cpotpt20trs_nojovt.zmvm$p11_m7+
                      cpotpt20trs_nojovt.zmvm$p11_m8
cpotpt20trs_nojovt.zmvm$hrs_trs <- cpotpt20trs_nojovt.zmvm$p11_h2+
                                   cpotpt20trs_nojovt.zmvm$p11_h3+
                                   cpotpt20trs_nojovt.zmvm$p11_h4+
                                   cpotpt20trs_nojovt.zmvm$p11_h5+
                                   cpotpt20trs_nojovt.zmvm$p11_h6+
                                   cpotpt20trs_nojovt.zmvm$p11_h7+
                                   cpotpt20trs_nojovt.zmvm$p11_h8+
                                   (mintrs_nojovt.zmvm/60)
d20trs_nojovtzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20trs_nojovt.zmvm)
media20trs_nojovt.zmvm <- svymean(~hrs_trs, d20trs_nojovtzmvm)
svyttest(hrs_trs~0, d20trs_nojovtzmvm, na = T)
        # Principal actividad
cpotpt20act_jovt.zmvm <- filter(cpotpt20_jov.zmvm)
cact20_jovtzmvm <- aggregate(fac~p3, cpotpt20act_jovt.zmvm, sum, na.rm=T)
cact20_jovtzmvm$prop <- cact20_jovtzmvm$fac/sum(cact20_jovtzmvm$fac)
act20_jovtzmvm <- cact20_jovtzmvm[order(cact20_jovtzmvm[,3], decreasing=T),]
cpotpt20act_nojovt.zmvm <- filter(cpotpt20_nojov.zmvm)
cact20_nojovtzmvm <- aggregate(fac~p3, cpotpt20act_nojovt.zmvm, sum, na.rm=T)
cact20_nojovtzmvm$prop <- cact20_nojovtzmvm$fac/sum(cact20_nojovtzmvm$fac)
act20_nojovtzmvm <- cact20_nojovtzmvm[order(cact20_nojovtzmvm[,3], 
                                            decreasing=T),]
      # Vector de extraccion de datos
extrac_emp.jynj <- c(media05esc_jovt.nal, media05esc_nojovt.nal, 
                     media05jor_jovt.nal, media05jor_nojovt.nal, 
                     media05trs_jovt.nal, media05trs_nojovt.nal, 
                     media05esc_jovt.zmvm, media05esc_nojovt.zmvm, 
                     media05jor_jovt.zmvm, media05jor_nojovt.zmvm, 
                     media05trs_jovt.zmvm, media05trs_nojovt.zmvm, 
                     media20esc_jovt.nal, media20esc_nojovt.nal, 
                     media20jor_jovt.nal, media20jor_nojovt.nal, 
                     media20trs_jovt.nal, media20trs_nojovt.nal, 
                     media20esc_jovt.zmvm, media20esc_nojovt.zmvm, 
                     media20jor_jovt.zmvm, media20jor_nojovt.zmvm, 
                     media20trs_jovt.zmvm, media20trs_nojovt.zmvm)
extrac_emp.actjynj <- rbind((act05_jovtnal [1:4, ]), NA, 
                            (act05_nojovtnal [1:4, ]), NA, 
                            (act05_jovtzmvm [1:4, ]), NA, 
                            (act05_nojovtzmvm [1:4, ]), NA, 
                            (act20_jovtnal [1:4, ]), NA, 
                            (act20_nojovtnal [1:4, ]), NA, 
                            (act20_jovtzmvm [1:4, ]), NA, 
                            (act20_nojovtzmvm [1:4, ]))
    # Estadisticas de los jovenes de la ZMVM por tipo de trabajo (formal/
    # informal) y anio
      # Total de jovenes 2005
        # Edad
cpotpt05eda_infzmvm <- filter(cpotinf05_jov.zmvm)
d05edapt_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data= cpotpt05eda_infzmvm)
media05edapt_infzmvm <- svymean(~eda, d05edapt_infzmvm)
svyttest(eda~0, d05edapt_infzmvm, na = T)
cpotpt05eda_forzmvm <- filter(cpotfor05_jov.zmvm)
d05edapt_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data= cpotpt05eda_forzmvm)
media05edapt_forzmvm <- svymean(~eda, d05edapt_forzmvm)
svyttest(eda~0, d05edapt_forzmvm, na = T)
        # Escolaridad acumulada
cpotpt05esc_infzmvm <- filter(cpotinf05_jov.zmvm, anios_esc!=99)
d05escpt_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotpt05esc_infzmvm)
media05escpt_infzmvm <- svymean(~anios_esc, d05escpt_infzmvm)
svyttest(anios_esc~0, d05escpt_infzmvm, na = T)
cpotpt05esc_forzmvm <- filter(cpotfor05_jov.zmvm, anios_esc!=99)
d05escpt_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotpt05esc_forzmvm)
media05escpt_forzmvm <- svymean(~anios_esc, d05escpt_forzmvm)
svyttest(anios_esc~0, d05escpt_forzmvm, na = T)
        # Jornada de trabajo
cpotpt05jor_infzmvm <- filter(cpotinf05_jov.zmvm, hrsocup!=0)
d05jorpt_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotpt05jor_infzmvm)
media05jorpt_infzmvm <- svymean(~hrsocup, d05jorpt_infzmvm)
svyttest(hrsocup~0, d05jorpt_infzmvm, na = T)
cpotpt05jor_forzmvm <- filter(cpotfor05_jov.zmvm, hrsocup!=0)
d05jorpt_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotpt05jor_forzmvm)
media05jorpt_forzmvm <- svymean(~hrsocup, d05jorpt_forzmvm)
svyttest(hrsocup~0, d05jorpt_forzmvm, na = T)
        # Trabajo de reproducción social
cpotpt05basetrs_infzmvm <- filter(cpotinf05_jov.zmvm)
cpotpt05basetrs_infzmvm[is.na(cpotpt05basetrs_infzmvm)] <- 0
cpotpt05trs_infzmvm <- filter(cpotpt05basetrs_infzmvm, p11_h2!=98, p11_h2!=99, 
                              p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                              p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                              p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrspt_infzmvm <- cpotpt05trs_infzmvm$p11_m2+cpotpt05trs_infzmvm$p11_m3+
  cpotpt05trs_infzmvm$p11_m4+cpotpt05trs_infzmvm$p11_m5+
  cpotpt05trs_infzmvm$p11_m6+cpotpt05trs_infzmvm$p11_m7+
  cpotpt05trs_infzmvm$p11_m8
cpotpt05trs_infzmvm$hrs_trs <- cpotpt05trs_infzmvm$p11_h2+
  cpotpt05trs_infzmvm$p11_h3+
  cpotpt05trs_infzmvm$p11_h4+
  cpotpt05trs_infzmvm$p11_h5+
  cpotpt05trs_infzmvm$p11_h6+
  cpotpt05trs_infzmvm$p11_h7+
  cpotpt05trs_infzmvm$p11_h8+(mintrspt_infzmvm/60)
d05trspt_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotpt05trs_infzmvm)
media05trspt_infzmvm <- svymean(~hrs_trs, d05trspt_infzmvm)
svyttest(hrs_trs~0, d05trspt_infzmvm, na = T)
cpotpt05basetrs_forzmvm <- filter(cpotfor05_jov.zmvm)
cpotpt05basetrs_forzmvm[is.na(cpotpt05basetrs_forzmvm)] <- 0
cpotpt05trs_forzmvm <- filter(cpotpt05basetrs_forzmvm, p11_h2!=98, p11_h2!=99, 
                              p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                              p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                              p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrspt_forzmvm <- cpotpt05trs_forzmvm$p11_m2+cpotpt05trs_forzmvm$p11_m3+
  cpotpt05trs_forzmvm$p11_m4+cpotpt05trs_forzmvm$p11_m5+
  cpotpt05trs_forzmvm$p11_m6+cpotpt05trs_forzmvm$p11_m7+
  cpotpt05trs_forzmvm$p11_m8
cpotpt05trs_forzmvm$hrs_trs <- cpotpt05trs_forzmvm$p11_h2+
  cpotpt05trs_forzmvm$p11_h3+
  cpotpt05trs_forzmvm$p11_h4+
  cpotpt05trs_forzmvm$p11_h5+
  cpotpt05trs_forzmvm$p11_h6+
  cpotpt05trs_forzmvm$p11_h7+
  cpotpt05trs_forzmvm$p11_h8+(mintrspt_forzmvm/60)
d05trspt_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotpt05trs_forzmvm)
media05trspt_forzmvm <- svymean(~hrs_trs, d05trspt_forzmvm)
svyttest(hrs_trs~0, d05trspt_forzmvm, na = T)
        # Principal actividad
cpotpt05act_infzmvm <- filter(cpotinf05_jov.zmvm)
cactpt05_infzmvm <- aggregate(fac~p3, cpotpt05act_infzmvm, sum, na.rm=T)
cactpt05_infzmvm$prop <- cactpt05_infzmvm$fac/sum(cactpt05_infzmvm$fac)
actpt05_infzmvm <- cactpt05_infzmvm[order(cactpt05_infzmvm[,3], decreasing=T),]
cpotpt05act_forzmvm <- filter(cpotfor05_jov.zmvm)
cactpt05_forzmvm <- aggregate(fac~p3, cpotpt05act_forzmvm, sum, na.rm=T)
cactpt05_forzmvm$prop <- cactpt05_forzmvm$fac/sum(cactpt05_forzmvm$fac)
actpt05_forzmvm <- cactpt05_forzmvm[order(cactpt05_forzmvm[,3], decreasing=T),]
      # Jovenes hombres 2005
        # Edad
cpoth05eda_infzmvm <- filter(cpotinf05_jov.zmvm, sex==1)
d05edah_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data= cpoth05eda_infzmvm)
media05edah_infzmvm <- svymean(~eda, d05edah_infzmvm)
svyttest(eda~0, d05edah_infzmvm, na = T)
cpoth05eda_forzmvm <- filter(cpotfor05_jov.zmvm, sex==1)
d05edah_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data= cpoth05eda_forzmvm)
media05edah_forzmvm <- svymean(~eda, d05edah_forzmvm)
svyttest(eda~0, d05edah_forzmvm, na = T)
        # Escolaridad acumulada
cpoth05esc_infzmvm <- filter(cpotinf05_jov.zmvm, anios_esc!=99, sex==1)
d05esch_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpoth05esc_infzmvm)
media05esch_infzmvm <- svymean(~anios_esc, d05esch_infzmvm)
svyttest(anios_esc~0, d05esch_infzmvm, na = T)
cpoth05esc_forzmvm <- filter(cpotfor05_jov.zmvm, anios_esc!=99, sex==1)
d05esch_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpoth05esc_forzmvm)
media05esch_forzmvm <- svymean(~anios_esc, d05esch_forzmvm)
svyttest(anios_esc~0, d05esch_forzmvm, na = T)
        # Jornada de trabajo
cpoth05jor_infzmvm <- filter(cpotinf05_jov.zmvm, sex==1, hrsocup!=0)
d05jorh_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpoth05jor_infzmvm)
media05jorh_infzmvm <- svymean(~hrsocup, d05jorh_infzmvm)
svyttest(hrsocup~0, d05jorh_infzmvm, na = T)
cpoth05jor_forzmvm <- filter(cpotfor05_jov.zmvm, sex==1, hrsocup!=0)
d05jorh_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpoth05jor_forzmvm)
media05jorh_forzmvm <- svymean(~hrsocup, d05jorh_forzmvm)
svyttest(hrsocup~0, d05jorh_forzmvm, na = T)
        # Trabajo de reproducción social
cpoth05basetrs_infzmvm <- filter(cpotinf05_jov.zmvm, sex==1)
cpoth05basetrs_infzmvm[is.na(cpoth05basetrs_infzmvm)] <- 0
cpoth05trs_infzmvm <- filter(cpoth05basetrs_infzmvm, p11_h2!=98, p11_h2!=99, 
                             p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                             p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                             p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrsh_infzmvm <- cpoth05trs_infzmvm$p11_m2+cpoth05trs_infzmvm$p11_m3+
                   cpoth05trs_infzmvm$p11_m4+cpoth05trs_infzmvm$p11_m5+
                   cpoth05trs_infzmvm$p11_m6+cpoth05trs_infzmvm$p11_m7+
                   cpoth05trs_infzmvm$p11_m8
cpoth05trs_infzmvm$hrs_trs <- cpoth05trs_infzmvm$p11_h2+
                              cpoth05trs_infzmvm$p11_h3+
                              cpoth05trs_infzmvm$p11_h4+
                              cpoth05trs_infzmvm$p11_h5+
                              cpoth05trs_infzmvm$p11_h6+
                              cpoth05trs_infzmvm$p11_h7+
                              cpoth05trs_infzmvm$p11_h8+(mintrsh_infzmvm/60)
d05trsh_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpoth05trs_infzmvm)
media05trsh_infzmvm <- svymean(~hrs_trs, d05trsh_infzmvm)
svyttest(hrs_trs~0, d05trsh_infzmvm, na = T)
cpoth05basetrs_forzmvm <- filter(cpotfor05_jov.zmvm, sex==1)
cpoth05basetrs_forzmvm[is.na(cpoth05basetrs_forzmvm)] <- 0
cpoth05trs_forzmvm <- filter(cpoth05basetrs_forzmvm, p11_h2!=98, p11_h2!=99, 
                             p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                             p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                             p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrsh_forzmvm <- cpoth05trs_forzmvm$p11_m2+cpoth05trs_forzmvm$p11_m3+
                   cpoth05trs_forzmvm$p11_m4+cpoth05trs_forzmvm$p11_m5+
                   cpoth05trs_forzmvm$p11_m6+cpoth05trs_forzmvm$p11_m7+
                   cpoth05trs_forzmvm$p11_m8
cpoth05trs_forzmvm$hrs_trs <- cpoth05trs_forzmvm$p11_h2+
                              cpoth05trs_forzmvm$p11_h3+
                              cpoth05trs_forzmvm$p11_h4+
                              cpoth05trs_forzmvm$p11_h5+
                              cpoth05trs_forzmvm$p11_h6+
                              cpoth05trs_forzmvm$p11_h7+
                              cpoth05trs_forzmvm$p11_h8+(mintrsh_forzmvm/60)
d05trsh_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpoth05trs_forzmvm)
media05trsh_forzmvm <- svymean(~hrs_trs, d05trsh_forzmvm)
svyttest(hrs_trs~0, d05trsh_forzmvm, na = T)
        # Principal actividad
cpoth05act_infzmvm <- filter(cpotinf05_jov.zmvm, sex==1)
cacth05_infzmvm <- aggregate(fac~p3, cpoth05act_infzmvm, sum, na.rm=T)
cacth05_infzmvm$prop <- cacth05_infzmvm$fac/sum(cacth05_infzmvm$fac)
acth05_infzmvm <- cacth05_infzmvm[order(cacth05_infzmvm[,3], decreasing=T),]
cpoth05act_forzmvm <- filter(cpotfor05_jov.zmvm, sex==1)
cacth05_forzmvm <- aggregate(fac~p3, cpoth05act_forzmvm, sum, na.rm=T)
cacth05_forzmvm$prop <- cacth05_forzmvm$fac/sum(cacth05_forzmvm$fac)
acth05_forzmvm <- cacth05_forzmvm[order(cacth05_forzmvm[,3], decreasing=T),]
      # Jovenes mujeres 2005
        # Edad
cpotm05eda_infzmvm <- filter(cpotinf05_jov.zmvm, sex==2)
d05edam_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data= cpotm05eda_infzmvm)
media05edam_infzmvm <- svymean(~eda, d05edam_infzmvm)
svyttest(eda~0, d05edam_infzmvm, na = T)
cpotm05eda_forzmvm <- filter(cpotfor05_jov.zmvm, sex==2)
d05edam_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data= cpotm05eda_forzmvm)
media05edam_forzmvm <- svymean(~eda, d05edam_forzmvm)
svyttest(eda~0, d05edam_forzmvm, na = T)
        # Escolaridad acumulada
cpotm05esc_infzmvm <- filter(cpotinf05_jov.zmvm, anios_esc!=99, sex==2)
d05escm_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotm05esc_infzmvm)
media05escm_infzmvm <- svymean(~anios_esc, d05escm_infzmvm)
svyttest(anios_esc~0, d05escm_infzmvm, na = T)
cpotm05esc_forzmvm <- filter(cpotfor05_jov.zmvm, anios_esc!=99, sex==2)
d05escm_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotm05esc_forzmvm)
media05escm_forzmvm <- svymean(~anios_esc, d05escm_forzmvm)
svyttest(anios_esc~0, d05escm_forzmvm, na = T)
        # Jornada de trabajo
cpotm05jor_infzmvm <- filter(cpotinf05_jov.zmvm, sex==2, hrsocup!=0)
d05jorm_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotm05jor_infzmvm)
media05jorm_infzmvm <- svymean(~hrsocup, d05jorm_infzmvm)
svyttest(hrsocup~0, d05jorm_infzmvm, na = T)
cpotm05jor_forzmvm <- filter(cpotfor05_jov.zmvm, sex==2, hrsocup!=0)
d05jorm_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotm05jor_forzmvm)
media05jorm_forzmvm <- svymean(~hrsocup, d05jorm_forzmvm)
svyttest(hrsocup~0, d05jorm_forzmvm, na = T)
        # Trabajo de reproducción social
cpotm05basetrs_infzmvm <- filter(cpotinf05_jov.zmvm, sex==2)
cpotm05basetrs_infzmvm[is.na(cpotm05basetrs_infzmvm)] <- 0
cpotm05trs_infzmvm <- filter(cpotm05basetrs_infzmvm, p11_h2!=98, p11_h2!=99, 
                             p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                             p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                             p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrsm_infzmvm <- cpotm05trs_infzmvm$p11_m2+cpotm05trs_infzmvm$p11_m3+
                   cpotm05trs_infzmvm$p11_m4+cpotm05trs_infzmvm$p11_m5+
                   cpotm05trs_infzmvm$p11_m6+cpotm05trs_infzmvm$p11_m7+
                   cpotm05trs_infzmvm$p11_m8
cpotm05trs_infzmvm$hrs_trs <- cpotm05trs_infzmvm$p11_h2+
                              cpotm05trs_infzmvm$p11_h3+
                              cpotm05trs_infzmvm$p11_h4+
                              cpotm05trs_infzmvm$p11_h5+
                              cpotm05trs_infzmvm$p11_h6+
                              cpotm05trs_infzmvm$p11_h7+
                              cpotm05trs_infzmvm$p11_h8+(mintrsm_infzmvm/60)
d05trsm_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotm05trs_infzmvm)
media05trsm_infzmvm <- svymean(~hrs_trs, d05trsm_infzmvm)
svyttest(hrs_trs~0, d05trsm_infzmvm, na = T)
cpotm05basetrs_forzmvm <- filter(cpotfor05_jov.zmvm, sex==2)
cpotm05basetrs_forzmvm[is.na(cpotm05basetrs_forzmvm)] <- 0
cpotm05trs_forzmvm <- filter(cpotm05basetrs_forzmvm, p11_h2!=98, p11_h2!=99, 
                             p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                             p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                             p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrsm_forzmvm <- cpotm05trs_forzmvm$p11_m2+cpotm05trs_forzmvm$p11_m3+
                   cpotm05trs_forzmvm$p11_m4+cpotm05trs_forzmvm$p11_m5+
                   cpotm05trs_forzmvm$p11_m6+cpotm05trs_forzmvm$p11_m7+
                   cpotm05trs_forzmvm$p11_m8
cpotm05trs_forzmvm$hrs_trs <- cpotm05trs_forzmvm$p11_h2+
                              cpotm05trs_forzmvm$p11_h3+
                              cpotm05trs_forzmvm$p11_h4+
                              cpotm05trs_forzmvm$p11_h5+
                              cpotm05trs_forzmvm$p11_h6+
                              cpotm05trs_forzmvm$p11_h7+
                              cpotm05trs_forzmvm$p11_h8+(mintrsm_forzmvm/60)
d05trsm_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotm05trs_forzmvm)
media05trsm_forzmvm <- svymean(~hrs_trs, d05trsm_forzmvm)
svyttest(hrs_trs~0, d05trsm_forzmvm, na = T)
        # Principal actividad
cpotm05act_infzmvm <- filter(cpotinf05_jov.zmvm, sex==2)
cactm05_infzmvm <- aggregate(fac~p3, cpotm05act_infzmvm, sum, na.rm=T)
cactm05_infzmvm$prop <- cactm05_infzmvm$fac/sum(cactm05_infzmvm$fac)
actm05_infzmvm <- cactm05_infzmvm[order(cactm05_infzmvm[,3], decreasing=T),]
cpotm05act_forzmvm <- filter(cpotfor05_jov.zmvm, sex==2)
cactm05_forzmvm <- aggregate(fac~p3, cpotm05act_forzmvm, sum, na.rm=T)
cactm05_forzmvm$prop <- cactm05_forzmvm$fac/sum(cactm05_forzmvm$fac)
actm05_forzmvm <- cactm05_forzmvm[order(cactm05_forzmvm[,3], decreasing=T),]
      # Total de jovenes 2020
        # Edad
cpotpt20eda_infzmvm <- filter(cpotinf20_jov.zmvm)
d20edapt_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data= cpotpt20eda_infzmvm)
media20edapt_infzmvm <- svymean(~eda, d20edapt_infzmvm)
svyttest(eda~0, d20edapt_infzmvm, na = T)
cpotpt20eda_forzmvm <- filter(cpotfor20_jov.zmvm)
d20edapt_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data= cpotpt20eda_forzmvm)
media20edapt_forzmvm <- svymean(~eda, d20edapt_forzmvm)
svyttest(eda~0, d20edapt_forzmvm, na = T)
        # Escolaridad acumulada
cpotpt20esc_infzmvm <- filter(cpotinf20_jov.zmvm, anios_esc!=99)
d20escpt_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20esc_infzmvm)
media20escpt_infzmvm <- svymean(~anios_esc, d20escpt_infzmvm)
svyttest(anios_esc~0, d20escpt_infzmvm, na = T)
cpotpt20esc_forzmvm <- filter(cpotfor20_jov.zmvm, anios_esc!=99)
d20escpt_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20esc_forzmvm)
media20escpt_forzmvm <- svymean(~anios_esc, d20escpt_forzmvm)
svyttest(anios_esc~0, d20escpt_forzmvm, na = T)
        # Jornada de trabajo
cpotpt20jor_infzmvm <- filter(cpotinf20_jov.zmvm, hrsocup!=0)
d20jorpt_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20jor_infzmvm)
media20jorpt_infzmvm <- svymean(~hrsocup, d20jorpt_infzmvm)
svyttest(hrsocup~0, d20jorpt_infzmvm, na = T)
cpotpt20jor_forzmvm <- filter(cpotfor20_jov.zmvm, hrsocup!=0)
d20jorpt_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20jor_forzmvm)
media20jorpt_forzmvm <- svymean(~hrsocup, d20jorpt_forzmvm)
svyttest(hrsocup~0, d20jorpt_forzmvm, na = T)
        # Trabajo de reproducción social
cpotpt20basetrs_infzmvm <- filter(cpotinf20_jov.zmvm)
cpotpt20basetrs_infzmvm[is.na(cpotpt20basetrs_infzmvm)] <- 0
cpotpt20trs_infzmvm <- filter(cpotpt20basetrs_infzmvm, p11_h2!=98, p11_h2!=99, 
                              p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                              p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                              p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrspt_infzmvm <- cpotpt20trs_infzmvm$p11_m2+cpotpt20trs_infzmvm$p11_m3+
                    cpotpt20trs_infzmvm$p11_m4+cpotpt20trs_infzmvm$p11_m5+
                    cpotpt20trs_infzmvm$p11_m6+cpotpt20trs_infzmvm$p11_m7+
                    cpotpt20trs_infzmvm$p11_m8
cpotpt20trs_infzmvm$hrs_trs <- cpotpt20trs_infzmvm$p11_h2+
                               cpotpt20trs_infzmvm$p11_h3+
                               cpotpt20trs_infzmvm$p11_h4+
                               cpotpt20trs_infzmvm$p11_h5+
                               cpotpt20trs_infzmvm$p11_h6+
                               cpotpt20trs_infzmvm$p11_h7+
                               cpotpt20trs_infzmvm$p11_h8+(mintrspt_infzmvm/60)
d20trspt_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20trs_infzmvm)
media20trspt_infzmvm <- svymean(~hrs_trs, d20trspt_infzmvm)
svyttest(hrs_trs~0, d20trspt_infzmvm, na = T)
cpotpt20basetrs_forzmvm <- filter(cpotfor20_jov.zmvm)
cpotpt20basetrs_forzmvm[is.na(cpotpt20basetrs_forzmvm)] <- 0
cpotpt20trs_forzmvm <- filter(cpotpt20basetrs_forzmvm, p11_h2!=98, p11_h2!=99, 
                              p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                              p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                              p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrspt_forzmvm <- cpotpt20trs_forzmvm$p11_m2+cpotpt20trs_forzmvm$p11_m3+
                    cpotpt20trs_forzmvm$p11_m4+cpotpt20trs_forzmvm$p11_m5+
                    cpotpt20trs_forzmvm$p11_m6+cpotpt20trs_forzmvm$p11_m7+
                    cpotpt20trs_forzmvm$p11_m8
cpotpt20trs_forzmvm$hrs_trs <- cpotpt20trs_forzmvm$p11_h2+
                               cpotpt20trs_forzmvm$p11_h3+
                               cpotpt20trs_forzmvm$p11_h4+
                               cpotpt20trs_forzmvm$p11_h5+
                               cpotpt20trs_forzmvm$p11_h6+
                               cpotpt20trs_forzmvm$p11_h7+
                               cpotpt20trs_forzmvm$p11_h8+(mintrspt_forzmvm/60)
d20trspt_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20trs_forzmvm)
media20trspt_forzmvm <- svymean(~hrs_trs, d20trspt_forzmvm)
svyttest(hrs_trs~0, d20trspt_forzmvm, na = T)
        # Principal actividad
cpotpt20act_infzmvm <- filter(cpotinf20_jov.zmvm)
cactpt20_infzmvm <- aggregate(fac~p3, cpotpt20act_infzmvm, sum, na.rm=T)
cactpt20_infzmvm$prop <- cactpt20_infzmvm$fac/sum(cactpt20_infzmvm$fac)
actpt20_infzmvm <- cactpt20_infzmvm[order(cactpt20_infzmvm[,3], decreasing=T),]
cpotpt20act_forzmvm <- filter(cpotfor20_jov.zmvm)
cactpt20_forzmvm <- aggregate(fac~p3, cpotpt20act_forzmvm, sum, na.rm=T)
cactpt20_forzmvm$prop <- cactpt20_forzmvm$fac/sum(cactpt20_forzmvm$fac)
actpt20_forzmvm <- cactpt20_forzmvm[order(cactpt20_forzmvm[,3], decreasing=T),]
      # Jovenes hombres 2020
        # Edad
cpoth20eda_infzmvm <- filter(cpotinf20_jov.zmvm, sex==1)
d20edah_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data= cpoth20eda_infzmvm)
media20edah_infzmvm <- svymean(~eda, d20edah_infzmvm)
svyttest(eda~0, d20edah_infzmvm, na = T)
cpoth20eda_forzmvm <- filter(cpotfor20_jov.zmvm, sex==1)
d20edah_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data= cpoth20eda_forzmvm)
media20edah_forzmvm <- svymean(~eda, d20edah_forzmvm)
svyttest(eda~0, d20edah_forzmvm, na = T)
        # Escolaridad acumulada
cpoth20esc_infzmvm <- filter(cpotinf20_jov.zmvm, anios_esc!=99, sex==1)
d20esch_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpoth20esc_infzmvm)
media20esch_infzmvm <- svymean(~anios_esc, d20esch_infzmvm)
svyttest(anios_esc~0, d20esch_infzmvm, na = T)
cpoth20esc_forzmvm <- filter(cpotfor20_jov.zmvm, anios_esc!=99, sex==1)
d20esch_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpoth20esc_forzmvm)
media20esch_forzmvm <- svymean(~anios_esc, d20esch_forzmvm)
svyttest(anios_esc~0, d20esch_forzmvm, na = T)
        # Jornada de trabajo
cpoth20jor_infzmvm <- filter(cpotinf20_jov.zmvm, sex==1, hrsocup!=0)
d20jorh_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpoth20jor_infzmvm)
media20jorh_infzmvm <- svymean(~hrsocup, d20jorh_infzmvm)
svyttest(hrsocup~0, d20jorh_infzmvm, na = T)
cpoth20jor_forzmvm <- filter(cpotfor20_jov.zmvm, sex==1, hrsocup!=0)
d20jorh_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpoth20jor_forzmvm)
media20jorh_forzmvm <- svymean(~hrsocup, d20jorh_forzmvm)
svyttest(hrsocup~0, d20jorh_forzmvm, na = T)
        # Trabajo de reproducción social
cpoth20basetrs_infzmvm <- filter(cpotinf20_jov.zmvm, sex==1)
cpoth20basetrs_infzmvm[is.na(cpoth20basetrs_infzmvm)] <- 0
cpoth20trs_infzmvm <- filter(cpoth20basetrs_infzmvm, p11_h2!=98, p11_h2!=99, 
                             p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                             p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                             p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrsh_infzmvm <- cpoth20trs_infzmvm$p11_m2+cpoth20trs_infzmvm$p11_m3+
                   cpoth20trs_infzmvm$p11_m4+cpoth20trs_infzmvm$p11_m5+
                   cpoth20trs_infzmvm$p11_m6+cpoth20trs_infzmvm$p11_m7+
                   cpoth20trs_infzmvm$p11_m8
cpoth20trs_infzmvm$hrs_trs <- cpoth20trs_infzmvm$p11_h2+
                              cpoth20trs_infzmvm$p11_h3+
                              cpoth20trs_infzmvm$p11_h4+
                              cpoth20trs_infzmvm$p11_h5+
                              cpoth20trs_infzmvm$p11_h6+
                              cpoth20trs_infzmvm$p11_h7+
                              cpoth20trs_infzmvm$p11_h8+(mintrsh_infzmvm/60)
d20trsh_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpoth20trs_infzmvm)
media20trsh_infzmvm <- svymean(~hrs_trs, d20trsh_infzmvm)
svyttest(hrs_trs~0, d20trsh_infzmvm, na = T)
cpoth20basetrs_forzmvm <- filter(cpotfor20_jov.zmvm, sex==1)
cpoth20basetrs_forzmvm[is.na(cpoth20basetrs_forzmvm)] <- 0
cpoth20trs_forzmvm <- filter(cpoth20basetrs_forzmvm, p11_h2!=98, p11_h2!=99, 
                             p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                             p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                             p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrsh_forzmvm <- cpoth20trs_forzmvm$p11_m2+cpoth20trs_forzmvm$p11_m3+
                   cpoth20trs_forzmvm$p11_m4+cpoth20trs_forzmvm$p11_m5+
                   cpoth20trs_forzmvm$p11_m6+cpoth20trs_forzmvm$p11_m7+
                   cpoth20trs_forzmvm$p11_m8
cpoth20trs_forzmvm$hrs_trs <- cpoth20trs_forzmvm$p11_h2+
                              cpoth20trs_forzmvm$p11_h3+
                              cpoth20trs_forzmvm$p11_h4+
                              cpoth20trs_forzmvm$p11_h5+
                              cpoth20trs_forzmvm$p11_h6+
                              cpoth20trs_forzmvm$p11_h7+
                              cpoth20trs_forzmvm$p11_h8+(mintrsh_forzmvm/60)
d20trsh_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpoth20trs_forzmvm)
media20trsh_forzmvm <- svymean(~hrs_trs, d20trsh_forzmvm)
svyttest(hrs_trs~0, d20trsh_forzmvm, na = T)
        # Principal actividad
cpoth20act_infzmvm <- filter(cpotinf20_jov.zmvm, sex==1)
cacth20_infzmvm <- aggregate(fac~p3, cpoth20act_infzmvm, sum, na.rm=T)
cacth20_infzmvm$prop <- cacth20_infzmvm$fac/sum(cacth20_infzmvm$fac)
acth20_infzmvm <- cacth20_infzmvm[order(cacth20_infzmvm[,3], decreasing=T),]
cpoth20act_forzmvm <- filter(cpotfor20_jov.zmvm, sex==1)
cacth20_forzmvm <- aggregate(fac~p3, cpoth20act_forzmvm, sum, na.rm=T)
cacth20_forzmvm$prop <- cacth20_forzmvm$fac/sum(cacth20_forzmvm$fac)
acth20_forzmvm <- cacth20_forzmvm[order(cacth20_forzmvm[,3], decreasing=T),]
      # Jovenes mujeres 2020
        # Edad
cpotm20eda_infzmvm <- filter(cpotinf20_jov.zmvm, sex==2)
d20edam_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data= cpotm20eda_infzmvm)
media20edam_infzmvm <- svymean(~eda, d20edam_infzmvm)
svyttest(eda~0, d20edam_infzmvm, na = T)
cpotm20eda_forzmvm <- filter(cpotfor20_jov.zmvm, sex==2)
d20edam_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data= cpotm20eda_forzmvm)
media20edam_forzmvm <- svymean(~eda, d20edam_forzmvm)
svyttest(eda~0, d20edam_forzmvm, na = T)
        # Escolaridad acumulada
cpotm20esc_infzmvm <- filter(cpotinf20_jov.zmvm, anios_esc!=99, sex==2)
d20escm_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotm20esc_infzmvm)
media20escm_infzmvm <- svymean(~anios_esc, d20escm_infzmvm)
svyttest(anios_esc~0, d20escm_infzmvm, na = T)
cpotm20esc_forzmvm <- filter(cpotfor20_jov.zmvm, anios_esc!=99, sex==2)
d20escm_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotm20esc_forzmvm)
media20escm_forzmvm <- svymean(~anios_esc, d20escm_forzmvm)
svyttest(anios_esc~0, d20escm_forzmvm, na = T)
        # Jornada de trabajo
cpotm20jor_infzmvm <- filter(cpotinf20_jov.zmvm, sex==2, hrsocup!=0)
d20jorm_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotm20jor_infzmvm)
media20jorm_infzmvm <- svymean(~hrsocup, d20jorm_infzmvm)
svyttest(hrsocup~0, d20jorm_infzmvm, na = T)
cpotm20jor_forzmvm <- filter(cpotfor20_jov.zmvm, sex==2, hrsocup!=0)
d20jorm_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotm20jor_forzmvm)
media20jorm_forzmvm <- svymean(~hrsocup, d20jorm_forzmvm)
svyttest(hrsocup~0, d20jorm_forzmvm, na = T)
        # Trabajo de reproducción social
cpotm20basetrs_infzmvm <- filter(cpotinf20_jov.zmvm, sex==2)
cpotm20basetrs_infzmvm[is.na(cpotm20basetrs_infzmvm)] <- 0
cpotm20trs_infzmvm <- filter(cpotm20basetrs_infzmvm, p11_h2!=98, p11_h2!=99, 
                             p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                             p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                             p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrsm_infzmvm <- cpotm20trs_infzmvm$p11_m2+cpotm20trs_infzmvm$p11_m3+
                   cpotm20trs_infzmvm$p11_m4+cpotm20trs_infzmvm$p11_m5+
                   cpotm20trs_infzmvm$p11_m6+cpotm20trs_infzmvm$p11_m7+
                   cpotm20trs_infzmvm$p11_m8
cpotm20trs_infzmvm$hrs_trs <- cpotm20trs_infzmvm$p11_h2+
                              cpotm20trs_infzmvm$p11_h3+
                              cpotm20trs_infzmvm$p11_h4+
                              cpotm20trs_infzmvm$p11_h5+
                              cpotm20trs_infzmvm$p11_h6+
                              cpotm20trs_infzmvm$p11_h7+
                              cpotm20trs_infzmvm$p11_h8+(mintrsm_infzmvm/60)
d20trsm_infzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotm20trs_infzmvm)
media20trsm_infzmvm <- svymean(~hrs_trs, d20trsm_infzmvm)
svyttest(hrs_trs~0, d20trsm_infzmvm, na = T)
cpotm20basetrs_forzmvm <- filter(cpotfor20_jov.zmvm, sex==2)
cpotm20basetrs_forzmvm[is.na(cpotm20basetrs_forzmvm)] <- 0
cpotm20trs_forzmvm <- filter(cpotm20basetrs_forzmvm, p11_h2!=98, p11_h2!=99, 
                             p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                             p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                             p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
mintrsm_forzmvm <- cpotm20trs_forzmvm$p11_m2+cpotm20trs_forzmvm$p11_m3+
                   cpotm20trs_forzmvm$p11_m4+cpotm20trs_forzmvm$p11_m5+
                   cpotm20trs_forzmvm$p11_m6+cpotm20trs_forzmvm$p11_m7+
                   cpotm20trs_forzmvm$p11_m8
cpotm20trs_forzmvm$hrs_trs <- cpotm20trs_forzmvm$p11_h2+
                              cpotm20trs_forzmvm$p11_h3+
                              cpotm20trs_forzmvm$p11_h4+
                              cpotm20trs_forzmvm$p11_h5+
                              cpotm20trs_forzmvm$p11_h6+
                              cpotm20trs_forzmvm$p11_h7+
                              cpotm20trs_forzmvm$p11_h8+(mintrsm_forzmvm/60)
d20trsm_forzmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotm20trs_forzmvm)
media20trsm_forzmvm <- svymean(~hrs_trs, d20trsm_forzmvm)
svyttest(hrs_trs~0, d20trsm_forzmvm, na = T)
        # Principal actividad
cpotm20act_infzmvm <- filter(cpotinf20_jov.zmvm, sex==2)
cactm20_infzmvm <- aggregate(fac~p3, cpotm20act_infzmvm, sum, na.rm=T)
cactm20_infzmvm$prop <- cactm20_infzmvm$fac/sum(cactm20_infzmvm$fac)
actm20_infzmvm <- cactm20_infzmvm[order(cactm20_infzmvm[,3], decreasing=T),]
cpotm20act_forzmvm <- filter(cpotfor20_jov.zmvm, sex==2)
cactm20_forzmvm <- aggregate(fac~p3, cpotm20act_forzmvm, sum, na.rm=T)
cactm20_forzmvm$prop <- cactm20_forzmvm$fac/sum(cactm20_forzmvm$fac)
actm20_forzmvm <- cactm20_forzmvm[order(cactm20_forzmvm[,3], decreasing=T),]
      # Vector de extraccion de datos
extrac_zmvm.gen <- c(media05edapt_infzmvm, media05edapt_forzmvm, 
                     media05escpt_infzmvm, media05escpt_forzmvm, 
                     media05jorpt_infzmvm, media05jorpt_forzmvm, 
                     media05trspt_infzmvm, media05trspt_forzmvm, 
                     media05edah_infzmvm, media05edah_forzmvm, 
                     media05esch_infzmvm, media05esch_forzmvm, 
                     media05jorh_infzmvm, media05jorh_forzmvm, 
                     media05trsh_infzmvm, media05trsh_forzmvm, 
                     media05edam_infzmvm, media05edam_forzmvm, 
                     media05escm_infzmvm, media05escm_forzmvm, 
                     media05jorm_infzmvm, media05jorm_forzmvm, 
                     media05trsm_infzmvm, media05trsm_forzmvm, 
                     media20edapt_infzmvm, media20edapt_forzmvm, 
                     media20escpt_infzmvm, media20escpt_forzmvm, 
                     media20jorpt_infzmvm, media20jorpt_forzmvm, 
                     media20trspt_infzmvm, media20trspt_forzmvm, 
                     media20edah_infzmvm, media20edah_forzmvm, 
                     media20esch_infzmvm, media20esch_forzmvm, 
                     media20jorh_infzmvm, media20jorh_forzmvm, 
                     media20trsh_infzmvm, media20trsh_forzmvm,
                     media20edam_infzmvm, media20edam_forzmvm, 
                     media20escm_infzmvm, media20escm_forzmvm, 
                     media20jorm_infzmvm, media20jorm_forzmvm, 
                     media20trsm_infzmvm, media20trsm_forzmvm)
extrac_act.jovfei <- rbind((actpt05_infzmvm [1:4, ]), NA, 
                           (actpt05_forzmvm [1:4, ]), NA, 
                           (acth05_infzmvm [1:4, ]), NA, 
                           (acth05_forzmvm [1:4, ]), NA, 
                           (actm05_infzmvm [1:4, ]), NA, 
                           (actm05_forzmvm [1:4, ]), NA, 
                           (actpt20_infzmvm [1:4, ]), NA, 
                           (actpt20_forzmvm [1:4, ]), NA, 
                           (acth20_infzmvm [1:4, ]), NA, 
                           (acth20_forzmvm [1:4, ]), NA, 
                           (actm20_infzmvm [1:4, ]), NA, 
                           (actm20_forzmvm [1:4, ]))
# Estadisticas ampliadas del empleo informal juvenil en la ZMVM por anio
    # Estadisticas generales 2005
      # Edad
dis05_gen.t <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotinf05_jov.zmvm)
svymean(~eda, dis05_gen.t)
svyttest(eda~0, dis05_gen.t, na = T)
sqrt(svyvar(~eda, dis05_gen.t))
min(cpotinf05_jov.zmvm$eda)
max(cpotinf05_jov.zmvm$eda)
svyquantile(~eda, dis05_gen.t, quantiles=c(0.25,0.5,0.75), ci=F)
      # Escolaridad acumulada
cpotinf05esc_jzmvm <- filter(cpotinf05_jov.zmvm, anios_esc!=99)
dis05_esc.t <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotinf05esc_jzmvm)
svymean(~anios_esc, dis05_esc.t)
svyttest(anios_esc~0, dis05_esc.t, na = T)
sqrt(svyvar(~anios_esc, dis05_esc.t))
min(cpotinf05esc_jzmvm$anios_esc)
max(cpotinf05esc_jzmvm$anios_esc)
svyquantile(~anios_esc, dis05_esc.t, quantiles=c(0.25,0.5,0.75), ci=F)
      # Horas trabajadas
cpotinf05jor_jzmvm <- filter(cpotinf05_jov.zmvm, hrsocup!=0)
dis05_jor.t <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                         data=cpotinf05jor_jzmvm)
svymean(~hrsocup, dis05_jor.t)
svyttest(hrsocup~0, dis05_jor.t, na = T)
sqrt(svyvar(~hrsocup, dis05_jor.t))
min(cpotinf05jor_jzmvm $hrsocup)
max(cpotinf05jor_jzmvm $hrsocup)
svyquantile(~hrsocup, dis05_jor.t, quantiles=c(0.25,0.5,0.75), ci=F)
      # Horas de trabajo de reproduccion social
cpotinf05btrs_jzmvm <- cpotinf05_jov.zmvm
cpotinf05btrs_jzmvm[is.na(cpotinf05btrs_jzmvm)] <- 0
cpotinf05trs_jzmvm <- filter(cpotinf05btrs_jzmvm, p11_h2!=98, p11_h2!=99, 
                               p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                               p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                               p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
min05j_trs <- cpotinf05trs_jzmvm$p11_m2+cpotinf05trs_jzmvm$p11_m3+
              cpotinf05trs_jzmvm$p11_m4+cpotinf05trs_jzmvm$p11_m5+
              cpotinf05trs_jzmvm$p11_m6+cpotinf05trs_jzmvm$p11_m7+
              cpotinf05trs_jzmvm$p11_m8
cpotinf05trs_jzmvm$hrs_trs <- cpotinf05trs_jzmvm$p11_h2+
                              cpotinf05trs_jzmvm$p11_h3+
                              cpotinf05trs_jzmvm$p11_h4+
                              cpotinf05trs_jzmvm$p11_h5+
                              cpotinf05trs_jzmvm$p11_h6+
                              cpotinf05trs_jzmvm$p11_h7+
                              cpotinf05trs_jzmvm$p11_h8+(min05j_trs/60)
dis05_trs.t <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotinf05trs_jzmvm)
svymean(~hrs_trs, dis05_trs.t)
svyttest(hrs_trs~0, dis05_trs.t, na = T)
sqrt(svyvar(~hrs_trs, dis05_trs.t))
min(cpotinf05trs_jzmvm$hrs_trs)
max(cpotinf05trs_jzmvm$hrs_trs)
svyquantile(~hrs_trs, dis05_trs.t, quantiles=c(0.25,0.5,0.75), ci=F)
      # Estadisticas generales 2020
        # Edad
dis20_gen.t <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                         data=cpotinf20_jov.zmvm)
svymean(~eda, dis20_gen.t)
svyttest(eda~0, dis20_gen.t, na = T)
sqrt(svyvar(~eda, dis20_gen.t))
min(cpotinf20_jov.zmvm$eda)
max(cpotinf20_jov.zmvm$eda)
svyquantile(~eda, dis20_gen.t, quantiles=c(0.25,0.5,0.75), ci=F)
        # Escolaridad acumulada
cpotinf20esc_jzmvm <- filter(cpotinf20_jov.zmvm, anios_esc!=99)
dis20_esc.t <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                         data=cpotinf20esc_jzmvm)
svymean(~anios_esc, dis20_esc.t)
svyttest(anios_esc~0, dis20_esc.t, na = T)
sqrt(svyvar(~anios_esc, dis20_esc.t))
min(cpotinf20esc_jzmvm$anios_esc)
max(cpotinf20esc_jzmvm$anios_esc)
svyquantile(~anios_esc, dis20_esc.t, quantiles=c(0.25,0.5,0.75), ci=F)
        # Horas trabajadas
cpotinf20jor_jzmvm <- filter(cpotinf20_jov.zmvm, hrsocup!=0)
dis20_jor.t <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                         data=cpotinf20jor_jzmvm)
svymean(~hrsocup, dis20_jor.t)
svyttest(hrsocup~0, dis20_jor.t, na = T)
sqrt(svyvar(~hrsocup, dis20_jor.t))
min(cpotinf20jor_jzmvm $hrsocup)
max(cpotinf20jor_jzmvm $hrsocup)
svyquantile(~hrsocup, dis20_jor.t, quantiles=c(0.25,0.5,0.75), ci=F)
        # Horas de trabajo de reproduccion social
cpotinf20btrs_jzmvm <- cpotinf20_jov.zmvm
cpotinf20btrs_jzmvm[is.na(cpotinf20btrs_jzmvm)] <- 0
cpotinf20trs_jzmvm <- filter(cpotinf20btrs_jzmvm, p11_h2!=98, p11_h2!=99, 
                             p11_h3!=98, p11_h3!=99, p11_h4!=98, p11_h4!=99,
                             p11_h5!=98, p11_h5!=99, p11_h6!=98, p11_h6!=99, 
                             p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
min20j_trs <- cpotinf20trs_jzmvm$p11_m2+cpotinf20trs_jzmvm$p11_m3+
              cpotinf20trs_jzmvm$p11_m4+cpotinf20trs_jzmvm$p11_m5+
              cpotinf20trs_jzmvm$p11_m6+cpotinf20trs_jzmvm$p11_m7+
              cpotinf20trs_jzmvm$p11_m8
cpotinf20trs_jzmvm$hrs_trs <- cpotinf20trs_jzmvm$p11_h2+
                              cpotinf20trs_jzmvm$p11_h3+
                              cpotinf20trs_jzmvm$p11_h4+
                              cpotinf20trs_jzmvm$p11_h5+
                              cpotinf20trs_jzmvm$p11_h6+
                              cpotinf20trs_jzmvm$p11_h7+
                              cpotinf20trs_jzmvm$p11_h8+(min20j_trs/60)
dis20_trs.t <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                         data=cpotinf20trs_jzmvm)
svymean(~hrs_trs, dis20_trs.t)
svyttest(hrs_trs~0, dis20_trs.t, na = T)
sqrt(svyvar(~hrs_trs, dis20_trs.t))
min(cpotinf20trs_jzmvm$hrs_trs)
max(cpotinf20trs_jzmvm$hrs_trs)
svyquantile(~hrs_trs, dis20_trs.t, quantiles=c(0.25,0.5,0.75), ci=F)
#-------------------------------------------------------------------------------
  # Modelos econometricos
informalt05 <- cpotinf05_tot.zmvm
formalt05 <- cpotfor05_tot.zmvm
informalt05$emp.ppal.amp <- 1
formalt05$emp.ppal.amp <- 0
base05.zmvm <- rbind(informalt05, formalt05)
base05_mt1 <- base05.zmvm
base05_mt1[is.na(base05_mt1)] <- 0
base05_mt2 <- filter(base05_mt1, p11_h2!=98, p11_h2!=99, p11_h3!=98, p11_h3!=99, 
                     p11_h4!=98, p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                     p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
min05.trs <- base05_mt2$p11_m2+base05_mt2$p11_m3+ base05_mt2$p11_m4+base05_mt2$p11_m5+
             base05_mt2$p11_m6+base05_mt2$p11_m7+base05_mt2$p11_m8
base05_mt2$hrs_trs <- base05_mt2$p11_h2+base05_mt2$p11_h3+base05_mt2$p11_h4+
                      base05_mt2$p11_h5+base05_mt2$p11_h6+base05_mt2$p11_h7+
                      base05_mt2$p11_h8+(min05.trs/60)
base05_mt3 <- filter(base05_mt2, anios_esc!=99)
base05_mt4 <- filter(base05_mt3, e_con!=9)
base05_mt5 <- base05_mt4
base05_mt5$e_civil <- ifelse(base05_mt4$e_con==1|base05_mt4$e_con==5, 0, 1)
base05_mt6 <- base05_mt5
base05_mt6$asis_esc <- ifelse(base05_mt5$cs_p17!=1, 0, 1)
base05_mt7 <- base05_mt6
base05_mt7$inmigr <- ifelse(base05_mt6$cs_nr_mot==0, 0, 1)
base05_mt8 <- base05_mt7
base05_mt8$sex<- ifelse(base05_mt7$sex==1, 0, 1)
base05_mt9 <- base05_mt8
base05_mt9$eda_2 <- base05_mt8$eda*base05_mt8$eda
master_base05 <- base05_mt9
diseniot05 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac, nest=T,
                        data= master_base05)
svyquantile(~hrs_trs, diseniot05, quantiles=c(0.25, 0.5, 0.75), ci=F)
svymean(~hrs_trs, diseniot05)
master_base05$h_trs <- ifelse(master_base05$hrs_trs>15.893, 1, 0)
diseniot05 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac, nest=T,
                        data= master_base05)
modelo05_t1 <- svyglm(emp.ppal.amp ~ sex + eda + eda_2 + anios_esc + e_civil + 
                      h_trs + asis_esc + inmigr + sex*h_trs, design= diseniot05, 
                      family=quasibinomial (link='logit'))
summary(modelo05_t1)
informalt20 <- cpotinf20_tot.zmvm
formalt20 <- cpotfor20_tot.zmvm
informalt20$emp.ppal.amp <- 1
formalt20$emp.ppal.amp <- 0
base20.zmvm <- rbind(informalt20, formalt20)
base20_mt1 <- base20.zmvm
base20_mt1[is.na(base20_mt1)] <- 0
base20_mt2 <- filter(base20_mt1, p11_h2!=98, p11_h2!=99, p11_h3!=98, p11_h3!=99, 
                   p11_h4!=98, p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                   p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
min20.trs <- base20_mt2$p11_m2+base20_mt2$p11_m3+ base20_mt2$p11_m4+base20_mt2$p11_m5+
             base20_mt2$p11_m6+base20_mt2$p11_m7+base20_mt2$p11_m8
base20_mt2$hrs_trs <- base20_mt2$p11_h2+base20_mt2$p11_h3+base20_mt2$p11_h4+
                      base20_mt2$p11_h5+base20_mt2$p11_h6+base20_mt2$p11_h7+
                      base20_mt2$p11_h8+(min20.trs/60)
base20_mt3 <- filter(base20_mt2, anios_esc!=99)
base20_mt4 <- base20_mt3
base20_mt4$e_civil <- ifelse(base20_mt3$e_con==1|base20_mt3$e_con==5, 0, 1)
base20_mt5 <- base20_mt4
base20_mt5$asis_esc <- ifelse(base20_mt4$cs_p17!=1, 0, 1)
base20_mt6 <- base20_mt5
base20_mt6$inmigr <- ifelse(base20_mt5$cs_nr_mot==0, 0, 1)
base20_mt7 <- base20_mt6
base20_mt7$sex<- ifelse(base20_mt6$sex==1, 0, 1)
base20_mt8 <- base20_mt7
base20_mt8$eda_2 <- base20_mt7$eda*base20_mt7$eda
master_base20 <- base20_mt8
diseniot20 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac, nest=T,
                        data= master_base20)
svyquantile(~hrs_trs, diseniot20, quantiles=c(0.25, 0.5, 0.75), ci=F)
svymean(~hrs_trs, diseniot20)
master_base20$h_trs <- ifelse(master_base20$hrs_trs>14.724, 1, 0)
diseniot20 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac, nest=T,
                      data= master_base20)
modelo20_t1 <- svyglm(emp.ppal.amp ~ sex + eda + eda_2 + anios_esc + e_civil + 
                      hrs_trs + asis_esc + inmigr + sex*h_trs, 
                      design= diseniot20, family=quasibinomial (link='logit'))
summary(modelo20_t1)

    # Variables adicionales 2005
master.base_2005v1 <- filter (master_base05, rama!=7)
master.base_2005v2 <- filter (master.base_2005v1, pos_ocu!=5)
master.base_2005v3 <- filter (master.base_2005v2, c_ocu11c!=11)
master.base_2005 <- master.base_2005v3
master.base_2005$p_ocupa <- ifelse(master.base_2005$pos_ocu==3, 1, 0)
master.base_2005$sector <- ifelse(master.base_2005$rama==6|
                                  master.base_2005$rama==1|
                                  master.base_2005$rama==3, 1, 0)
master.base_2005$con_ocupa <- ifelse(master.base_2005$c_ocu11c==1|
                                     master.base_2005$c_ocu11c==2|
                                     master.base_2005$c_ocu11c==3|
                                     master.base_2005$c_ocu11c==4|
                                     master.base_2005$c_ocu11c==9, 0, 1)
dt05 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac, nest=T,
                        data= master.base_2005)
svyquantile(~hrs_trs, dt05, quantiles=c(0.25, 0.5, 0.75), ci=F)
svymean(~hrs_trs, dt05)
base_comp.05<- master.base_2005
base_comp.05$htrs <- ifelse(base_comp.05$hrs_trs>15.895, 1, 0)
dt05 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac, nest=T,
                        data= base_comp.05)
mc_05 <- svyglm(emp.ppal.amp ~ sex + eda + eda_2 + anios_esc + e_civil + 
                  hrs_trs + asis_esc + p_ocupa + sector + con_ocupa + hrs_trs*sex, 
                  design= dt05, family=quasibinomial 
                  (link='logit'))
summary(mc_05)
    # Test de Wald
wald.test (b = coef (mc_05), Sigma = vcov (mc_05), Terms=2:10)
    # Valores predichos 
valores_predic05 <- predict(mc_05, type = "response")
tabla_cont05 <- table(base_comp.05$emp.ppal.amp, round(valores_predic05))
tabla_cont05
    # Curva ROC
library(pROC)
predicciones <- predict(mc_05, type = "response")
roc_datos <- roc(base_comp.05$emp.ppal.amp, predicciones)
auc(roc_datos)
    # Efectos marginales promedio
l_escalar05 <- mean(dlogis(predict(mc_05, type='link')))
l_escalar05*coef(mc_05)
margins(mc_05, design=dt05)
summary(margins(mc_05, design=dt05))
    # Variables adicionales 2020
master.base_2020v1 <- filter (master_base20, rama!=7)
master.base_2020v2 <- filter (master.base_2020v1, pos_ocu!=5)
master.base_2020v3 <- filter (master.base_2020v2, c_ocu11c!=11)
master.base_2020 <- master.base_2020v3
master.base_2020$p_ocupa <- ifelse(master.base_2020v3$pos_ocu==3, 1, 0)
master.base_2020$sector <- ifelse(master.base_2020v3$rama==6|
                                    master.base_2020v3$rama==1|
                                    master.base_2020v3$rama==3, 1, 0)
master.base_2020$con_ocupa <- ifelse(master.base_2020$c_ocu11c==1|
                                       master.base_2020$c_ocu11c==2|
                                       master.base_2020$c_ocu11c==3|
                                       master.base_2020$c_ocu11c==4|
                                       master.base_2020$c_ocu11c==9, 0, 1)
dt20 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac, nest=T,
                  data= master.base_2020)
svyquantile(~hrs_trs, dt20, quantiles=c(0.25, 0.5, 0.75), ci=F)
svymean(~hrs_trs, dt20)
base_comp.20<- master.base_2020
base_comp.20$htrs <- ifelse(base_comp.20$hrs_trs>14.762, 1, 0)
dt20 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac, nest=T,
                  data= base_comp.20)
mc_20 <- svyglm(emp.ppal.amp ~ sex + eda + eda_2 + anios_esc + e_civil + 
                hrs_trs + asis_esc + p_ocupa + sector + con_ocupa + hrs_trs*sex, 
                design= dt20, family=quasibinomial 
                (link='logit'))
summary(mc_20)
    # Variable jefe de familia informal/formal (no activos)
informalt20 <- cpotinf20_tot.zmvm
formalt20 <- cpotfor20_tot.zmvm
informalt20$emp.ppal.amp <- 1
formalt20$emp.ppal.amp <- 0
baset20 <- rbind(informalt20, formalt20)
baset20$llave_hog <- substr(baset20$llave, start=1, stop=13)
baset20$jefe_fam <- 2
jefet20 <- filter(baset20, par_c==101) 
neaynoct20 <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                     (eda>=15&eda<=98), (clase1==2|clase2==2), cd_a=='01')
neaynoct20$llave_hog <- substr(neaynoct20$llave, start=1, stop=13)
neaynoct20$jefe_fam <- 2
jefetneaynoc20 <- filter(neaynoct20, par_c==101)
base_jov_noact <- rbind(cpnea20_jov.zmvm, cpeanoc20_jov.zmvm)
sum(base_jov_noact$fac)
base_jov_noact$llave_hog <- substr(base_jov_noact$llave, start=1, stop=13)
jefe_jov_noact20 <- filter(base_jov_noact, par_c==101) 
jefe_jov_noact20$jefe_fam <- 0 # Parte 1
sum(jefe_jov_noact20$fac)  
jefe_jov_noact20$emp.ppal.amp <- 0
sum(jefe_jov_noact20$fac)/sum(base_jov_noact$fac)
nojefe_jov_noact20 <- filter(base_jov_noact, par_c!=101) 
sum(nojefe_jov_noact20$fac)
(sum(jefe_jov_noact20$fac)+sum(nojefe_jov_noact20$fac))/2617278
base1s <- subset(nojefe_jov_noact20, select = c(llave, llave_hog, fac, eda))
base2s <- subset(jefet20, select = c(llave, llave_hog, jefe_fam, emp.ppal.amp))
base1_2s <-merge(x=base1s, y=base2s, by='llave_hog', all.x=T)
jefe.famocup20_nact <- filter(base1_2s, jefe_fam==2) 
jefe.famocup20_nact$jefe_fam <- 1 #Parte 2
sum(jefe.famocup20_nact$fac)
sum(jefe.famocup20_nact$fac)/sum(base_jov_noact$fac)
base1_2nas <- mutate_at(base1_2s, c('jefe_fam'), ~replace(., is.na(.), 0))
base3s <- filter(base1_2nas, jefe_fam==0)
base4s <- subset(jefetneaynoc20, select = c(llave, llave_hog, jefe_fam))
base3_4s <- merge(x=base3s, y=base4s, by='llave_hog', all.x=T)
jefe.famnocup20_nact <- filter(base3_4s, jefe_fam.y==2) # Parte para tratamiento
sum(jefe.famnocup20_nact$fac) # Parte 3
sum(jefe.famnocup20_nact$fac)/sum(base_jov_noact$fac)
base3_4nas <- mutate_at(base3_4s, c('jefe_fam.y'), ~replace(., is.na(.), 0))
base5s <- filter(base3_4nas, jefe_fam.y==0)
pobzmvm <- filter(dem_coe20, cd_a=='01')
base5_jefes <- filter(pobzmvm, llave=='010940382051004')
jefe.casoespecials <- base5s # No se definió informal según propia definición 
                            # Por no contestar si tenía SAR y Salud
jefe.casoespecials$emp.ppal.amp <- 0  
jefe.casoespecials$jefe_fam.y <- 1 # Parte 4
sum(jefe.casoespecials$fac)
sum(jefe.casoespecials$fac)/sum(base_jov_noact$fac)
(sum(jefe_jov_noact20$fac) + sum(jefe.famocup20_nact$fac) + 
  sum(jefe.famnocup20_nact$fac) + sum(jefe.casoespecials$fac)) / 2617278
sum(jefe.famnocup20_nact$fac)/ 2617278 # Parte para tratamiento
base_edamaxs <- baset20 %>%
  group_by(llave_hog) %>%
  slice(which.max(eda))
jefeocp.nojovs <- filter(base_edamaxs)
base6s <- jefe.famnocup20_nact
base6s <- subset(base6s, select = c(llave.x, llave_hog, fac,eda))
base7s <- subset(jefeocp.nojovs, select = c(llave, llave_hog, jefe_fam, 
                                            emp.ppal.amp,eda))
base6_7s <- merge(x=base6s, y=base7s, by='llave_hog', all.x=T)
jefeocupado_jovenesnocup <- filter(base6_7s, jefe_fam==2)
jefeocupado_jovenesnocup$jefe_fam <- ifelse(jefeocupado_jovenesnocup$eda.x<=
  jefeocupado_jovenesnocup$eda.y, 1,0) # Parte 5
sum(jefeocupado_jovenesnocup$fac)/ 2617278 
(sum(jefe_jov_noact20$fac) + sum(jefe.famocup20_nact$fac) + 
    sum(jefe.casoespecials$fac) + sum(jefeocupado_jovenesnocup$fac))/ 2617278
base6_7nas <- mutate_at(base6_7s, c('jefe_fam'), ~replace(., is.na(.), 0))
base8s <- filter(base6_7nas, jefe_fam==0)
base8s <- subset(base8s, select = c(llave.x, llave_hog, fac, eda.x))
base_edamaxss <- neaynoct20 %>%
  group_by(llave_hog) %>%
  slice(which.max(eda))
jefenoocp.nojovs <- filter(base_edamaxss)
jefenoocp.nojovs$emp.ppal.amp <- 0
base9s <- subset(jefenoocp.nojovs, select = c(llave, llave_hog, jefe_fam, 
                                  emp.ppal.amp))
base8_9s <- merge(x=base8s, y=base9s, by='llave_hog', all.x=T)
jefenoocupado_jovenesnocup <- filter(base8_9s, jefe_fam==2)
jefenoocupado_jovenesnocup$jefe_fam <- 0 # Parte 6
sum(jefenoocupado_jovenesnocup$fac)/ 2617278 
(sum(jefe_jov_noact20$fac) + sum(jefe.famocup20_nact$fac) + 
    sum(jefe.casoespecials$fac) + sum(jefeocupado_jovenesnocup$fac)+
    sum(jefenoocupado_jovenesnocup$fac))/ 2617278
# Extracción de conjuntos para variable de jefe de familia
jf1s <- subset(jefe_jov_noact20, select = c(llave, fac, jefe_fam, emp.ppal.amp))
jf2s <- subset(jefe.famocup20_nact, select = c(llave.x, fac, jefe_fam, 
                                           emp.ppal.amp))
jf2s <- rename(jf2s, llave=llave.x)
jf3s <- subset(jefe.casoespecials, select = c(llave.x, fac, jefe_fam.y,
                                              emp.ppal.amp))
jf3s <- rename(jf3s, llave=llave.x, jefe_fam=jefe_fam.y)
jf4s <- subset(jefeocupado_jovenesnocup, select = c(llave.x, fac, jefe_fam, 
                                               emp.ppal.amp))
jf4s <- rename(jf4s, llave=llave.x)
jf5s <- subset(jefenoocupado_jovenesnocup, select = c(llave.x, fac, jefe_fam, 
                                                    emp.ppal.amp))
jf5s <- rename(jf5s, llave=llave.x)
jov_fjs <- rbind(jf1s, jf2s, jf3s, jf4s, jf5s)
sum(jov_fjs$fac)==pnea20_jov.zmvm + peanoc20_jov.zmvm # Comprobacion vector correcto
jovnocup_jefefam <- jov_fjs
jovnocup_jefefam$jf_inf <- jov_fjs$jefe_fam*jov_fjs$emp.ppal.amp
jf_vecfin_jovnact <- subset(jovnocup_jefefam, select = c (llave, fac, jf_inf))
    # Modelo probabilistico sobre determinantes de la informalidad
      # Variable jefe de familia informal/formal
informalt20 <- cpotinf20_tot.zmvm
formalt20 <- cpotfor20_tot.zmvm
informalt20$emp.ppal.amp <- 1
formalt20$emp.ppal.amp <- 0
baset20 <- rbind(informalt20, formalt20)
baset20$llave_hog <- substr(baset20$llave, start=1, stop=13)
baset20$jefe_fam <- 2
jefet20 <- filter(baset20, par_c==101) 
neaynoct20 <- filter(dem_coe20, r_def.x==0, (c_res==1|c_res==3), 
                     (eda>=15&eda<=98), (clase1==2|clase2==2), cd_a=='01')
neaynoct20$llave_hog <- substr(neaynoct20$llave, start=1, stop=13)
neaynoct20$jefe_fam <- 2
jefetneaynoc20 <- filter(neaynoct20, par_c==101)
        # Filtro de jovenes jefes de familia
basej20 <- filter(baset20, eda>=15&eda<=29)
jefej20 <- filter(basej20, par_c==101) 
jefej20$jefe_fam <- 0 # Parte 1
sum(jefej20$fac)
nojefej20 <- filter(basej20, par_c!=101) 
sum(nojefej20$fac)
sum(jefej20$fac)+sum(nojefej20$fac)
base1 <- subset(nojefej20, select = c(llave, llave_hog, fac, eda))
base2 <- subset(jefet20, select = c(llave, llave_hog, jefe_fam, emp.ppal.amp))
base1_2 <-merge(x=base1, y=base2, by='llave_hog', all.x=T)
jefe.famocup20 <- filter(base1_2, jefe_fam==2) 
jefe.famocup20$jefe_fam <- 1 # Parte 2
sum(jefe.famocup20$fac)
base1_2na <- mutate_at(base1_2, c('jefe_fam'), ~replace(., is.na(.), 0))
base3 <- filter(base1_2na, jefe_fam==0)
base4 <- subset(jefetneaynoc20, select = c(llave, llave_hog, jefe_fam))
base3_4 <- merge(x=base3, y=base4, by='llave_hog', all.x=T)
jefe.famnocup20 <- filter(base3_4, jefe_fam.y==2) # Parte 3 para tratamiento
sum(jefe.famnocup20$fac) # Comprueba jovenes que falta definir jefe de familia
comprueba_h <- jefe.famnocup20%>%count (llave_hog, sort = T)
base3_4na <- mutate_at(base3_4, c('jefe_fam.y'), ~replace(., is.na(.), 0))
base5 <- filter(base3_4na, jefe_fam.y==0)
pobzmvm <- filter(dem_coe20, cd_a=='01')
base5_jefe <- filter(pobzmvm, llave=='010940382051001'|
                     llave=='010940427051001'|llave=='011540298031101')
jefe.casoespecial <- base5 # No se definió informal según propia definición 
                           # Por no contestar si tenía SAR y Salud
jefe.casoespecial$emp.ppal.amp <- 1  
jefe.casoespecial$jefe_fam.y <- 1 # Parte 2.2
sum(jefe.casoespecial$fac)
        # Hogares de los jovenes con jefes de familia no ocupados
vec_hog <- c('0109400120110', '0109404960210', '0109402340210', '0115401020310', 
             '0109400120510', '0109404960510', '0109402460310', '0115401220510', 
             '0109400150510', '0109405010310', '0109402530510', '0115401220510', 
             '0109400220410', '0109405010310', '0109402580410', '0115401260210', 
             '0109400270110', '0109405050510', '0109402580410', '0115401310410', 
             '0109400270210', '0109405100510', '0109402590310', '0115401430410', 
             '0109400280210', '0109405200510', '0109402590510', '0115401440510', 
             '0109400310210', '0109405250210', '0109402590510', '0115401600510', 
             '0109400310510', '0109405260310', '0109402640210', '0115401630210', 
             '0109400310510', '0109405260310', '0109402670410', '0115401660310', 
             '0109400350510', '0109405260310', '0109402710410', '0115401660310', 
             '0109400430510', '0109405280210', '0109402880210', '0115401690310', 
             '0109400430510', '0109405280210', '0109402880210', '0115401700410', 
             '0109400450210', '0109405280310', '0109402880510', '0115401700410', 
             '0109400460510', '0109405280310', '0109402900310', '0115401720110', 
             '0109400460510', '0109405280510', '0109402930310', '0115401740510', 
             '0109400560510', '0109405320110', '0109402950310', '0115401820210', 
             '0109400570310', '0109405350210', '0109402960310', '0115401820210', 
             '0109400570410', '0109405500310', '0109403150310', '0115401820210', 
             '0109400660510', '0109405570510', '0109403180410', '0115401870110', 
             '0109400810510', '0109405570510', '0109403200310', '0115401890310', 
             '0109400810510', '0109405580410', '0109403250210', '0115401920110', 
             '0109400870410', '0109405620110', '0109403300110', '0115401920210', 
             '0109400870410', '0109405660110', '0109403300110', '0115401960210', 
             '0109400930510', '0109405680110', '0109403380110', '0115401990410', 
             '0109401030410', '0109405680510', '0109403380110', '0115402010210', 
             '0109401050210', '0109405710310', '0109403420510', '0115402060110', 
             '0109401050410', '0109405710410', '0109403430310', '0115402060110', 
             '0109401090110', '0109405730410', '0109403450210', '0115402060110', 
             '0109401140410', '0109405730410', '0109403450210', '0115402060110', 
             '0109401180510', '0109405960510', '0109403540310', '0115402080110', 
             '0109401180510', '0109405960520', '0109403690410', '0115402080110', 
             '0109401200210', '0109406060210', '0109403740310', '0115402090510', 
             '0109401230310', '0109406060410', '0109403770210', '0115402130410', 
             '0109401290211', '0109406150510', '0109403850510', '0115402130410', 
             '0109401290211', '0109406160410', '0109403850510', '0115402190110', 
             '0109401290211', '0109406190110', '0109403860210', '0115402190110', 
             '0109401310210', '0109406200510', '0109403870510', '0115402190410', 
             '0109401380410', '0109406210410', '0109403870520', '0115402290410', 
             '0109401390110', '0109406210410', '0109404010310', '0115402310110', 
             '0109401390110', '0109406240410', '0109404040310', '0115402380310', 
             '0109401400310', '0109406240410', '0109404050110', '0115402400210', 
             '0109401400310', '0109406250210', '0109404060310', '0115402450310', 
             '0109401430310', '0109406250210', '0109404100410', '0115402470210', 
             '0109401430410', '0109406380310', '0109404100410', '0115402540410', 
             '0109401460110', '0115400030310', '0109404120210', '0115402580410', 
             '0109401460110', '0115400050511', '0109404140110', '0115402720510', 
             '0109401460110', '0115400060110', '0109404140110', '0115402780210', 
             '0109401460110', '0115400060110', '0109404150110', '0115402890410', 
             '0109401460510', '0115400100510', '0109404160510', '0115402890410', 
             '0109401500510', '0115400100510', '0109404200210', '0115402930510', 
             '0109401530210', '0115400100510', '0109404220410', '0115402930510', 
             '0109401540110', '0115400180110', '0109404290210', '0115402930510', 
             '0109401540510', '0115400250210', '0109404300410', '0115402950510', 
             '0109401650510', '0115400290410', '0109404300410', '0115403120410', 
             '0109401730410', '0115400290410', '0109404330210', '0115403140510', 
             '0109401730410', '0115400350510', '0109404340210', '0115403190510', 
             '0109401740410', '0115400400510', '0109404370510', '0115403200410', 
             '0109401770411', '0115400460410', '0109404460410', '0115403220510', 
             '0109401800210', '0115400460410', '0109404460410', '0115403220510', 
             '0109401810210', '0115400480110', '0109404500310', '0115403240310', 
             '0109401840110', '0115400540210', '0109404500310', '0115403330210', 
             '0109401870510', '0115400550410', '0109404610210', '0115403330210', 
             '0109401900110', '0115400620510', '0109404640411', '0115403340410', 
             '0109401940410', '0115400670110', '0109404650510', '0115403390110', 
             '0109401970110', '0115400680410', '0109404730510', '0115403390110', 
             '0109402000210', '0115400710110', '0109404730510', '0115403450210', 
             '0109402000210', '0115400710110', '0109404730510', '0115403560310', 
             '0109402050110', '0115400810410', '0109404750110', '0115403580510', 
             '0109402050210', '0115400830410', '0109404750210', '0115403580510', 
             '0109402060510', '0115400830410', '0109404750310', '0115403760410', 
             '0109402090410', '0115400840410', '0109404790110', '0115403770110', 
             '0109402090410', '0115400890310', '0109404800310', '0115403770510', 
             '0109402090510', '0115400950110', '0109404850510', '0115403770510', 
             '0109402090510', '0115401010210', '0109404900510', '0115403800110', 
             '0109402260210', '0115401010510')
base_hselec <- baset20
base_hselec <- base_hselec %>% filter(llave_hog %in% vec_hog)
hog_2int <- base_hselec%>%count (llave_hog, sort = T)
        # Vector de hogares conformados por dos personas (joven y jefe 
        # no ocupado)
vec_h2i <- c('0109402340210', '0115401630210', '0109400120510', '0109405320110',
             '0109402460310', '0115401690310', '0109400150510', '0109405350210',
             '0109402640210', '0115401720110', '0109400220410', '0109405580410',
             '0109402900310', '0115401740510', '0109400270210', '0109405660110',
             '0109402930310', '0115401920110', '0109400570310', '0109405680110',
             '0109403180410', '0115401920210', '0109401050410', '0109405710410',
             '0109403420510', '0115401960210', '0109401090110', '0109405960510',
             '0109403740310', '0115401990410', '0109401140410', '0109405960520',
             '0109403770210', '0115402010210', '0109401200210', '0109406150510',
             '0109403870510', '0115402190410', '0109401430310', '0109406190110',
             '0109403870520', '0115402380310', '0109401530210', '0115400030310',
             '0109404050110', '0115402400210', '0109401540110', '0115400250210',
             '0109404060310', '0115402580410', '0109401540510', '0115400670110',
             '0109404330210', '0115402720510', '0109401650510', '0115400810410',
             '0109404640411', '0115402950510', '0109401740410', '0115400890310',
             '0109404750110', '0115403190510', '0109401770411', '0115400950110',
             '0109404750210', '0115403200410', '0109401800210', '0115401010510',
             '0109404900510', '0115403560310', '0109401810210', '0115401260210',
             '0109405100510', '0115403760410', '0109401870510', '0115401310410',
             '0109405200510', '0115403770110', '0109401900110', '0115401430410',
             '0115403800110', '0109402050210', '0115401440510')
base_hog2i <- base_hselec
base_hog2i <- base_hselec %>% filter(llave_hog %in% vec_h2i)
jefenoocup.fam2int <- subset(base_hog2i, select = c(llave, llave_hog, jefe_fam, 
                                                    fac,emp.ppal.amp))
jefenoocup.fam2int$jefe_fam <- 0 # Parte 3.1
sum(jefenoocup.fam2int$fac)
summary(base_hog2i$eda) # Confirma hogares de dos personas (jovenes y jefes pnea)

base6 <- jefe.famnocup20 %>% filter(!llave_hog %in% vec_h2i)
base6 <- subset(base6, select = c(llave.x, llave_hog, fac,eda))
sum(base6$fac) # Jovenes que faltan de jefe del hogar 
        # Integrantes de hogares que pueden ser jefes de familia respecto a 
        # la base 6
base_exchog2i <- base_hselec #excluye hogares de dos integrantes
base_exchog2i <- base_hselec %>% filter(!llave_hog %in% vec_h2i)
base_eh2i <- base_exchog2i
base_edamax <- base_eh2i %>%
  group_by(llave_hog) %>%
  slice(which.max(eda))
jefeocp.nojov <- filter(base_edamax, eda>=30)
base7 <- subset(jefeocp.nojov, select = c(llave, llave_hog, jefe_fam, 
                                          emp.ppal.amp))
base6_7 <- merge(x=base6, y=base7, by='llave_hog', all.x=T)
jefe2intnojoven <- filter(base6_7, jefe_fam==2)
jefe2intnojoven$jefe_fam <- 1 # Parte 3.2
sum(jefe2intnojoven$fac)
base6_7na <- mutate_at(base6_7, c('jefe_fam'), ~replace(., is.na(.), 0))
base8 <- filter(base6_7na, jefe_fam==0)
base8 <- subset(base8, select = c(llave.x, llave_hog, jefe_fam, eda, fac))
sum(base8$fac)
jefeocp.jov <- filter(base_edamax, eda<30)
base9 <- subset(jefeocp.jov, select = c(llave, llave_hog, jefe_fam, 
                                          emp.ppal.amp,eda))
base8_9 <- merge(x=base8, y=base9, by='llave_hog', all.x=T)
sum(base8_9$fac)
casoespecialjoven <- filter(base8_9, llave.x== '011540293051002'|
                            llave.x== '011540293051004')
casoespecialjoven$jefe_fam.y <- 1 # Parte 3.3.1
base10.sinespecial <- filter(base8_9, llave.x!='011540293051002'&
                               llave.x!= '011540293051004')
sum(base10.sinespecial$fac)
basejefesjov <- base10.sinespecial
basejefesjov$jefe.jov <- 0
basejefesjov$jefe.jov <- ifelse(basejefesjov$llave.x==basejefesjov$llave, 1, 0)
joven.conjefejoven<- filter(basejefesjov, jefe.jov==0)
joven.conjefejoven$jefe_fam.y <- 1 # Parte 3.3.2
sum(joven.conjefejoven$fac)
jovenmayor.sinjefe <- filter(basejefesjov, jefe.jov==1)
jovenmayor.sinjefe$jefe_fam.y <- 0 # Parte 3.3.3
sum(jovenmayor.sinjefe$fac)
        # Extracción de conjuntos para variable de jefe de familia
jf1 <- subset(jefej20, select = c(llave, fac, jefe_fam, emp.ppal.amp))
jf2.1 <- subset(jefe.famocup20, select = c(llave.x, fac, jefe_fam, 
                                           emp.ppal.amp))
jf2.1 <- rename(jf2.1, llave=llave.x)
jf2.2 <- subset(jefe.casoespecial, select = c(llave.x, fac, jefe_fam.y,
                                              emp.ppal.amp))
jf2.2 <- rename(jf2.2, llave=llave.x, jefe_fam=jefe_fam.y)
jf3.1 <- subset(jefenoocup.fam2int, select = c(llave, fac, jefe_fam, 
                                               emp.ppal.amp))
jf3.2 <- subset(jefe2intnojoven, select = c(llave.x, fac, jefe_fam, 
                                            emp.ppal.amp))
jf3.2 <- rename(jf3.2, llave=llave.x)
jf3.3.1 <- subset(casoespecialjoven, select = c(llave.x, fac, jefe_fam.y, 
                                              emp.ppal.amp))
jf3.3.1 <- rename(jf3.3.1, llave=llave.x, jefe_fam=jefe_fam.y)
jf3.3.2 <- subset(joven.conjefejoven, select = c(llave.x, fac, jefe_fam.y, 
                                                 emp.ppal.amp))
jf3.3.2 <- rename(jf3.3.2, llave=llave.x, jefe_fam=jefe_fam.y)
jf3.3.3 <- subset(jovenmayor.sinjefe, select = c(llave.x, fac, jefe_fam.y, 
                                                 emp.ppal.amp))
jf3.3.3 <-rename(jf3.3.3, llave=llave.x, jefe_fam=jefe_fam.y)
jov_fj <- rbind(jf1, jf2.1, jf2.2, jf3.1, jf3.2, jf3.3.1, jf3.3.2, jf3.3.3)
sum(jov_fj$fac)==pot20corr_jov.zmvm # Comprobacion vector correcto
jov_jefefam <- jov_fj
jov_jefefam$jf_inf <- jov_fj$jefe_fam*jov_fj$emp.ppal.amp
jf_vecfin <- subset(jov_jefefam, select = c (llave, fac, jf_inf))
      # Adecuacion de base
base.zmvmt <- rbind(informalt20, formalt20)
base.zmvmjp <- filter(base.zmvmt, eda>=15&eda<=29)
base.zmvmj <- dplyr::select(base.zmvmjp, -emp_ppal.amp, -fac.x, -fac.y)
base_m1 <- merge(x=base.zmvmj, y=jf_vecfin, by='llave', all.x=T)
base_m1$comprob <- base_m1$fac.x/base_m1$fac.y
sum(base_m1$comprob) # Comprueba que union de bases es correcta
base_m2 <- base_m1
base_m2[is.na(base_m2)] <- 0
base_m3 <- filter(base_m2, p11_h2!=98, p11_h2!=99, p11_h3!=98, p11_h3!=99, 
                  p11_h4!=98, p11_h4!=99, p11_h5!=98, p11_h5!=99, p11_h6!=98, 
                  p11_h6!=99, p11_h7!=98, p11_h7!=99, p11_h8!=98, p11_h8!=99)
minutos_trs <- base_m3$p11_m2+base_m3$p11_m3+ base_m3$p11_m4+base_m3$p11_m5+
               base_m3$p11_m6+base_m3$p11_m7+base_m3$p11_m8
base_m3$hrs_trs <- base_m3$p11_h2+base_m3$p11_h3+base_m3$p11_h4+base_m3$p11_h5+
                   base_m3$p11_h6+base_m3$p11_h7+base_m3$p11_h8+(minutos_trs/60)
base_m4 <- filter(base_m3, anios_esc!=99)
base_m5 <- base_m4
base_m5$e_civil <- ifelse(base_m4$e_con==1|base_m4$e_con==5, 0, 1)
base_m6 <- base_m5
base_m6$asis_esc <- ifelse(base_m5$cs_p17!=1, 0, 1)
base_m7 <- base_m6
base_m7$inmigr <- ifelse(base_m6$cs_nr_mot==0, 0, 1)
base_m8 <- base_m7
base_m8$sex<- ifelse(base_m7$sex==1, 0, 1)
base_m9 <- base_m8
base_m9$eda_2 <- base_m9$eda*base_m9$eda
base_maestra <- base_m9
# Variables adicionales
base_maestrav1 <- filter (base_maestra, rama!=7)
base_maestrav2 <- filter (base_maestrav1 , pos_ocu!=5)
base_maestrav3 <- filter (base_maestrav2, c_ocu11c!=11)
base_jov2020 <- base_maestrav3
base_jov2020$p_ocupa <- ifelse(base_jov2020$pos_ocu==3, 1, 0)
base_jov2020$sector <- ifelse(base_jov2020$rama==6|
                                base_jov2020$rama==1|
                                base_jov2020$rama==3, 1, 0)
base_jov2020$con_ocupa <- ifelse(base_jov2020$c_ocu11c==1|
                                 base_jov2020$c_ocu11c==2|
                                 base_jov2020$c_ocu11c==3|
                                 base_jov2020$c_ocu11c==4|
                                 base_jov2020$c_ocu11c==9, 0, 1)
dj20 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac.x, nest=T,
                  data= base_jov2020)
svyquantile(~jf_inf, dj20, quantiles=c(0.25, 0.5, 0.75), ci=F)
svymean(~hrs_trs, dj20)
svymean(~anios_esc, dj20)
summary(base_jov2020$pos_ocu)
base_jov.20 <- base_jov2020
base_jov.20$h_trs <- ifelse(base_jov.20$hrs_trs>11.652, 1, 0)
dj20 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac.x, nest=T,
                  data= base_jov.20)
mcj_20 <- svyglm(emp.ppal.amp ~ sex + eda + anios_esc + e_civil + 
                  hrs_trs + asis_esc + p_ocupa + sector + con_ocupa + jf_inf + 
                 hrs_trs*sex, 
                  design= dj20, family=quasibinomial 
                (link='logit'))
marginales_modelo <- margins(mcj_20, design=dj20)
tabla_marginales <- summary(marginales_modelo)
odds_ratios <- exp(coef(mcj_20))
odds_ratios
coeficientes <- coef(mcj_20)
odds_ratios <- exp(coeficientes)
resumen_modelo <- coef(mcj_20)
valores_p <- resumen_modelo$coefficients[, "Pr(>|t|)"]
datos_referencia <- read.csv('variables_adic.csv', header=T, sep=',', fileEncoding='UTF-8')
prob_referencia <- predict(mcj_20, newdata = datos_referencia, type = "response")
datos_incrementados <- datos_referencia
datos_incrementados$eda <- datos_incrementados$eda + 1
prob_incrementada <- predict(mcj_20, newdata = datos_incrementados, type = "response")
efecto_marginal_edad <- prob_incrementada - prob_referencia
efecto_marginal_edad 
    # Grafica PROC
predicciones <- predict(mcj_20, type = "response")
roc_datos <- roc(base_jov.20$emp.ppal.amp, predicciones)
auc(roc_datos)
resp <- base_jov.20$emp.ppal.amp
roc_data <- roc(resp, predicciones)
plot(roc_data)
    # Test de Wald
wald.test (b = coef (mcj_20), Sigma = vcov (mcj_20), Terms=2:10)
    # Valores predichos 
valores_predicj20 <- predict(mcj_20, type = "response")
tabla_contj20 <- table(base_jov.20$emp.ppal.amp, round(valores_predicj20))
tabla_contj20 # Accuracy de 71.2 %
table(base_jov.20$emp.ppal.amp, predict(mcj_20, type = "response") > 0.5)
    # Pseudo R cuadrado
logit_0 <- update(mcj_20, emp.ppal.amp~1)
R_McFadden_l <- 1-as.vector(logLik(mcj_20)/logLik(logit_0))
R_McFadden_l
    # Residuos
residuos_ajustados.mcj20 <- residuals(mcj_20, type = "response")
residuos_pearson.mcj20 <- residuals(mcj_20, type = "pearson")
plot(residuos_ajustados.mcj20 ~ fitted(mcj_20))
plot(residuos_pearson.mcj20  ~ fitted(mcj_20))
plot(mcj_20, which = 1)
    # Prueba de bondad de ajuste de Kolmogorov-Smirnov y prueba de Hosmer-Lemeshow
ks.test(valores_predicj20, "punif")
hoslem.test(mcj_20$fitted.values, base_jov.20$emp.ppal.amp)       
base_jov2020.subord <- filter(base_jov2020, pos_ocu==1)
djs20 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac.x, nest=T,
                  data= base_jov2020.subord)
svyquantile(~hrs_trs, djs20, quantiles=c(0.25, 0.5, 0.75), ci=F)
svymean(~hrs_trs, djs20)
base.subord_jov20 <- base_jov2020.subord
base.subord_jov20$h_trs <- ifelse(base.subord_jov20$hrs_trs>10.953, 1, 0)
djs20 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac.x, nest=T,
                  data= base.subord_jov20)
mcjs_20 <- svyglm(emp.ppal.amp ~ sex + eda + anios_esc + e_civil + 
                   h_trs + asis_esc + sector + con_ocupa + jf_inf, 
                 design= djs20, family=quasibinomial 
                 (link='logit'))
summary(mcjs_20)
resultados <- summary(mcjs_20)$coefficients
#-------------------------------------------------------------------------------
set.seed(2023) 
# Tratamiento inicial de los ingresos 
        # Año 2020
cpotinf2020_tot.nal <- cpotinf20_tot.nal
cpotfor2020_tot.nal <- cpotfor20_tot.nal
cpotinf2020_tot.nal$emp_ppal.amp <- 1
cpotfor2020_tot.nal$emp_ppal.amp <- 2
cpot20_totgral.nal <- rbind(cpotinf2020_tot.nal, cpotfor2020_tot.nal)
cpot20_totg.nal <- cpot20_totgral.nal
cpot20_totg.nal %>% count (clase3, sort = T)
cpot20_totg.nal %>% count (pos_ocu, sort = T)
cpot20_totg.nal %>% count (ing7c, sort = T)
cpot20_totg.nal %>% count (ingocup, sort = T)
cpot20_totg.nal %>% count (ing_x_hrs, sort = T)
cpot20_totg.nal$clase_ing <- NA
cpot20_totg.nal$clase_ing <- ifelse(cpot20_totg.nal$clase3==2, 0, 
                                    cpot20_totg.nal$clase_ing)
cpot20_totg.nal$clase_ing <- ifelse(cpot20_totg.nal$ing_x_hrs>0, 
                                    cpot20_totg.nal$ing_x_hrs, 
                                    cpot20_totg.nal$clase_ing)
summary(cpot20_totg.nal$clase_ing)
cpot20_totg.nal %>% count (c_ocu11c, sort = T)
cpot20_totg.nal %>% count (pos_ocu, sort = T)
cpot20_totg.nal %>% count (rama, sort = T)
cpot20_totg.nal %>% count (anios_esc, sort = T)
cpot20_totg.nal %>% count (e_con, sort = T)
base_ing.p20 <- cpot20_totg.nal
base_ing.p20 <- filter(base_ing.p20, e_con!=9)
base_ing.p20 <- filter(base_ing.p20, anios_esc!=99)
base_ing.p20 <- filter(base_ing.p20, rama!=7)
base_ing.p20 <- filter(base_ing.p20, c_ocu11c!=11)
base_ing.p20$eda2 <- base_ing.p20$eda*base_ing.p20$eda
base_ing.p20$ln_ing_x_hrs <- log(base_ing.p20$ing_x_hrs) 
base_ing.p20$e_civil <- 0
base_ing.p20$e_civil <- ifelse(base_ing.p20$e_con==1|
                               base_ing.p20$e_con==5, 1, 0)
    # 1: casado/unido; 0: soltero o alguna vez unido
base_ing.p20$z_geo <- 0
base_ing.p20$z_geo <- ifelse(base_ing.p20$zona==1, 1, 0) # 1: frontera norte; 
    # 0: el resto del pais
base_ing.p20$c_ocu11c <- factor(base_ing.p20$c_ocu11c)
base_ing.p20$rama<- factor(base_ing.p20$rama)
base_ing.p20$pos_ocu <- factor(base_ing.p20$pos_ocu)
base_ing.p20$e_civ <- factor(base_ing.p20$e_con)
ing_p_20 <- model.matrix(~ pos_ocu - 1, base_ing.p20)
ing_r_20 <- model.matrix(~ rama -1, base_ing.p20)
ing_c_20 <- model.matrix(~ c_ocu11c -1, base_ing.p20)
ing_e_20 <- model.matrix(~ e_civ -1, base_ing.p20)
base_ing20 <- cbind(base_ing.p20, ing_p_20, ing_r_20, ing_c_20, ing_e_20)
baseing_conpago20 <- subset(base_ing20, !(clase_ing==0 & !is.na(clase_ing)))
baseing_conpago20$ln_ing_x_hrs <- ifelse(is.na(baseing_conpago20$clase_ing), NA, 
                                         baseing_conpago20$ln_ing_x_hrs)
dis_ing20 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac, nest=T,
                       data= baseing_conpago20)
mod.20_ing1 <- svyglm(ln_ing_x_hrs ~ eda + eda2 + sex + e_civ1 + e_civ2 + 
                      e_civ3 + e_civ4 + e_civ5 + e_civ6 + anios_esc + hrsocup + 
                      zona + pos_ocu1 + pos_ocu2 + pos_ocu3 + pos_ocu4 + rama1 + 
                      rama2 + rama3 + rama4 + rama5 + rama6 + c_ocu11c1 + 
                      c_ocu11c2 + c_ocu11c3 + c_ocu11c5 + c_ocu11c6 + 
                      c_ocu11c7 + c_ocu11c8 + c_ocu11c9 + c_ocu11c10, 
                      design= dis_ing20)
summary(mod.20_ing1)
library(mice)
base_imp20 <- baseing_conpago20
base_im20 <- filter(base_imp20, sex==2)
pred_im20 <- base_im20[, c('eda', 'eda2', 'e_civ1', 'e_civ2', 'e_civ3',
                       'e_civ4', 'e_civ5', 'e_civ6', 'anios_esc', 'hrsocup', 
                       'zona', 'pos_ocu1', 'pos_ocu2', 'pos_ocu3', 'pos_ocu4',
                       'rama1', 'rama2', 'rama3', 'rama4', 'rama5', 'rama6',
                       'c_ocu11c1', 'c_ocu11c2', 'c_ocu11c3', 'c_ocu11c5', 
                       'c_ocu11c6', 'c_ocu11c7', 'c_ocu11c8', 'c_ocu11c9',
                       'c_ocu11c10','ln_ing_x_hrs')]
pred_im20$ln_ing_x_hrs <- as.factor(pred_im20$ln_ing_x_hrs)
imp_im20 <- mice(pred_im20, method = "pmm", m = 15, maxit = 10)
imputados_im20 <- complete(imp_im20)
imputm_ing20 <- subset(imputados_im20, select = c(eda, ln_ing_x_hrs))
id_facm20 <- subset(base_im20, select = c(llave, fac, sex, emp_ppal.amp, cd_a, 
                                          p3, rama, pos_ocu, c_ocu11c, upm, 
                                          est_d, hrsocup))
base_imputadosm20 <- cbind(imputm_ing20, id_facm20)
base_imputadosm20$ln_ing_hora<- as.numeric(as.character
                                       (base_imputadosm20$ln_ing_x_hrs))
base_imputadosm20$ing_est_hora <- exp(base_imputadosm20$ln_ing_hora)
base_ih20 <- filter(base_imp20, sex==1)
pred_ih20 <- base_ih20[, c('eda', 'eda2', 'e_civ1', 'e_civ2', 'e_civ3',
                       'e_civ4', 'e_civ5', 'e_civ6', 'anios_esc', 'hrsocup', 
                       'zona', 'pos_ocu1', 'pos_ocu2', 'pos_ocu3', 'pos_ocu4',
                       'rama1', 'rama2', 'rama3', 'rama4', 'rama5', 'rama6',
                       'c_ocu11c1', 'c_ocu11c2', 'c_ocu11c3', 'c_ocu11c5', 
                       'c_ocu11c6', 'c_ocu11c7', 'c_ocu11c8', 'c_ocu11c9',
                       'c_ocu11c10','ln_ing_x_hrs')]
pred_ih20$ln_ing_x_hrs <- as.factor(pred_ih20$ln_ing_x_hrs)
imp_ih20 <- mice(pred_ih20, method = "pmm", m = 15, maxit = 10)
imputados_ih20 <- complete(imp_ih20)
imputh_ing20 <- subset(imputados_ih20, select = c(eda, ln_ing_x_hrs))
id_fach20 <- subset(base_ih20, select = c(llave, fac, sex, emp_ppal.amp, cd_a, 
                                          p3, rama, pos_ocu, c_ocu11c, upm, 
                                          est_d, hrsocup))
base_imputadosh20 <- cbind(imputh_ing20, id_fach20)
base_imputadosh20$ln_ing_hora<- as.numeric(as.character
                                        (base_imputadosh20$ln_ing_x_hrs))
base_imputadosh20$ing_est_hora <- exp(base_imputadosh20$ln_ing_hora)
base.ing_maestra20 <- rbind(base_imputadosm20, base_imputadosh20)
        # Estadistica basica ingresos (jovenes y no jovenes, nacional urbana 
        # y ZMVM)
cpotpt20ing_jov.nal <- filter(base.ing_maestra20, eda>=15&eda<=29)
d20ing_jov.nal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20ing_jov.nal)
media20ing_jov.nal <- svymean(~ing_est_hora, d20ing_jov.nal)

cpotpt20ing_nojov.nal <- filter(base.ing_maestra20, eda>=30&eda<=64)
d20ing_nojov.nal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20ing_nojov.nal)
media20ing_nojov.nal <- svymean(~ing_est_hora, d20ing_nojov.nal)

cpotpt20ing_jov.zmvm <- filter(base.ing_maestra20, eda>=15&eda<=29, cd_a=='01')
d20ing_jov.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt20ing_jov.zmvm)
media20ing_jov.zmvm <- svymean(~ing_est_hora, d20ing_jov.zmvm)

cpotpt20ing_nojov.zmvm <- filter(base.ing_maestra20, eda>=30&eda<=64, 
                                 cd_a=='01')
d20ing_nojov.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20ing_nojov.zmvm)
media20ing_nojov.zmvm <- svymean(~ing_est_hora, d20ing_nojov.zmvm)
        # Estadistica de ingresos por grupo etario y tipo de trabajo (nacional 
        # urbana y ZMVM)
cpotpt20ing_1519.nal <- filter(base.ing_maestra20, eda>=15&eda<=19)
d20ing_1519.nal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt20ing_1519.nal)
media20ing_1519.nal <- svymean(~ing_est_hora, d20ing_1519.nal)

cpotpt20ing_2024.nal <- filter(base.ing_maestra20, eda>=20&eda<=24)
d20ing_2024.nal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt20ing_2024.nal)
media20ing_2024.nal <- svymean(~ing_est_hora, d20ing_2024.nal)

cpotpt20ing_2529.nal <- filter(base.ing_maestra20, eda>=25&eda<=29)
d20ing_2529.nal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt20ing_2529.nal)
media20ing_2529.nal <- svymean(~ing_est_hora, d20ing_2529.nal)

cpotpt20ing_1519.zmvm <- filter(base.ing_maestra20, eda>=15&eda<=19, cd_a=='01')
d20ing_1519.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20ing_1519.zmvm)
media20ing_1519.zmvm <- svymean(~ing_est_hora, d20ing_1519.zmvm)

cpotpt20ing_2024.zmvm <- filter(base.ing_maestra20, eda>=20&eda<=24, cd_a=='01')
d20ing_2024.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20ing_2024.zmvm)
media20ing_2024.zmvm <- svymean(~ing_est_hora, d20ing_2024.zmvm)

cpotpt20ing_2529.zmvm <- filter(base.ing_maestra20, eda>=25&eda<=29, cd_a=='01')
d20ing_2529.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt20ing_2529.zmvm)
media20ing_2529.zmvm <- svymean(~ing_est_hora, d20ing_2529.zmvm)

cpotpt20ing_1519.nal.inf <- filter(base.ing_maestra20, emp_ppal.amp==1, 
                                   eda>=15&eda<=19)
d20ing_1519.nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt20ing_1519.nal.inf)
media20ing_1519.nal.inf <- svymean(~ing_est_hora, d20ing_1519.nal.inf)

cpotpt20ing_2024.nal.inf <- filter(base.ing_maestra20, emp_ppal.amp==1, 
                                   eda>=20&eda<=24)
d20ing_2024.nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt20ing_2024.nal.inf)
media20ing_2024.nal.inf <- svymean(~ing_est_hora, d20ing_2024.nal.inf)

cpotpt20ing_2529.nal.inf <- filter(base.ing_maestra20, emp_ppal.amp==1, 
                                   eda>=25&eda<=29)
d20ing_2529.nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt20ing_2529.nal.inf)
media20ing_2529.nal.inf <- svymean(~ing_est_hora, d20ing_2529.nal.inf)

cpotpt20ing_1519.zmvm.inf <- filter(base.ing_maestra20, emp_ppal.amp==1, 
                                    eda>=15&eda<=19, cd_a=='01')
d20ing_1519.zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                  data=cpotpt20ing_1519.zmvm.inf)
media20ing_1519.zmvm.inf <- svymean(~ing_est_hora, d20ing_1519.zmvm.inf)

cpotpt20ing_2024.zmvm.inf <- filter(base.ing_maestra20, emp_ppal.amp==1, 
                                    eda>=20&eda<=24, cd_a=='01')
d20ing_2024.zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                  data=cpotpt20ing_2024.zmvm.inf)
media20ing_2024.zmvm.inf <- svymean(~ing_est_hora, d20ing_2024.zmvm.inf)

cpotpt20ing_2529.zmvm.inf <- filter(base.ing_maestra20, emp_ppal.amp==1, 
                                    eda>=25&eda<=29, cd_a=='01')
d20ing_2529.zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                  data=cpotpt20ing_2529.zmvm.inf)
media20ing_2529.zmvm.inf <- svymean(~ing_est_hora, d20ing_2529.zmvm.inf)

cpotpt20ing_1519.nal.for <- filter(base.ing_maestra20, emp_ppal.amp==2, 
                                   eda>=15&eda<=19)
d20ing_1519.nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt20ing_1519.nal.for)
media20ing_1519.nal.for <- svymean(~ing_est_hora, d20ing_1519.nal.for)

cpotpt20ing_2024.nal.for <- filter(base.ing_maestra20, emp_ppal.amp==2, 
                                   eda>=20&eda<=24)
d20ing_2024.nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt20ing_2024.nal.for)
media20ing_2024.nal.for <- svymean(~ing_est_hora, d20ing_2024.nal.for)

cpotpt20ing_2529.nal.for <- filter(base.ing_maestra20, emp_ppal.amp==2, 
                                   eda>=25&eda<=29)
d20ing_2529.nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt20ing_2529.nal.for)
media20ing_2529.nal.for <- svymean(~ing_est_hora, d20ing_2529.nal.for)

cpotpt20ing_1519.zmvm.for <- filter(base.ing_maestra20, emp_ppal.amp==2, 
                                    eda>=15&eda<=19, cd_a=='01')
d20ing_1519.zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                  data=cpotpt20ing_1519.zmvm.for)
media20ing_1519.zmvm.for <- svymean(~ing_est_hora, d20ing_1519.zmvm.for)

cpotpt20ing_2024.zmvm.for <- filter(base.ing_maestra20, emp_ppal.amp==2, 
                                    eda>=20&eda<=24, cd_a=='01')
d20ing_2024.zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                  data=cpotpt20ing_2024.zmvm.for)
media20ing_2024.zmvm.for <- svymean(~ing_est_hora, d20ing_2024.zmvm.for)

cpotpt20ing_2529.zmvm.for <- filter(base.ing_maestra20, emp_ppal.amp==2, 
                                    eda>=25&eda<=29, cd_a=='01')
d20ing_2529.zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                  data=cpotpt20ing_2529.zmvm.for)
media20ing_2529.zmvm.for <- svymean(~ing_est_hora, d20ing_2529.zmvm.for)
        #Estadistica ingresos ZMVM jovenes (hombres y mujeres)
cpotpt20ing_jovti.zmvm <- filter(base.ing_maestra20, eda>=15&eda<=29, 
                                 cd_a=='01', emp_ppal.amp==1)
d20ing_jovti.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt20ing_jovti.zmvm)
media20ing_jovti.zmvm <- svymean(~ing_est_hora, d20ing_jovti.zmvm)

cpotpt20ing_jovtf.zmvm <- filter(base.ing_maestra20, eda>=15&eda<=29, 
                                 cd_a=='01', emp_ppal.amp==2)
d20ing_jovtf.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20ing_jovtf.zmvm)
media20ing_jovtf.zmvm <- svymean(~ing_est_hora, d20ing_jovtf.zmvm)

cpotpt20ing_jovhi.zmvm <- filter(base.ing_maestra20, eda>=15&eda<=29,
                                 cd_a=='01', emp_ppal.amp==1, sex==1)
d20ing_jovhi.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20ing_jovhi.zmvm)
media20ing_jovhi.zmvm <- svymean(~ing_est_hora, d20ing_jovhi.zmvm)

svyquantile(~ing_est_hora, d20ing_jovhi.zmvm, quantiles=c(0.25,0.5,0.75), ci=F)
cpotpt20ing_jovhf.zmvm <- filter(base.ing_maestra20, eda>=15&eda<=29,
                                 cd_a=='01', emp_ppal.amp==2, sex==1)
d20ing_jovhf.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20ing_jovhf.zmvm)
media20ing_jovhf.zmvm <- svymean(~ing_est_hora, d20ing_jovhf.zmvm)

svyquantile(~ing_est_hora, d20ing_jovhf.zmvm, quantiles=c(0.25,0.5,0.75), ci=F)
cpotpt20ing_jovmi.zmvm <- filter(base.ing_maestra20, eda>=15&eda<=29,
                                 cd_a=='01', emp_ppal.amp==1, sex==2)
d20ing_jovmi.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20ing_jovmi.zmvm)
media20ing_jovmi.zmvm <- svymean(~ing_est_hora, d20ing_jovmi.zmvm)

svyquantile(~ing_est_hora, d20ing_jovmi.zmvm, quantiles=c(0.25,0.5,0.75), ci=F)
cpotpt20ing_jovmf.zmvm <- filter(base.ing_maestra20, eda>=15&eda<=29, 
                                 cd_a=='01', emp_ppal.amp==2, sex==2)
d20ing_jovmf.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt20ing_jovmf.zmvm)
media20ing_jovmf.zmvm <- svymean(~ing_est_hora, d20ing_jovmf.zmvm)
svyquantile(~ing_est_hora, d20ing_jovmf.zmvm, quantiles=c(0.25,0.5,0.75), ci=F)
        # Resumen ingresos de jovenes ZMVM 2020
cpotpt20ing_jov.zmvm <- filter(base.ing_maestra20, eda>=15&eda<=29, cd_a=='01')
d20ing_jov.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt20ing_jov.zmvm)
svymean(~ing_est_hora, d20ing_jov.zmvm)
sqrt(svyvar(~ing_est_hora, d20ing_jov.zmvm))
min(cpotpt20ing_jov.zmvm$ing_est_hora)
max(cpotpt20ing_jov.zmvm$ing_est_hora)
svyquantile(~ing_est_hora, d20ing_jov.zmvm, quantiles=c(0.25,0.5,0.75), ci=F)
        # Tratamiento inicial de los ingresos 
        # Año 2005
cpotinf2005_tot.nal <- cpotinf05_tot.nal
cpotfor2005_tot.nal <- cpotfor05_tot.nal
cpotinf2005_tot.nal$emp_ppal.amp <- 1
cpotfor2005_tot.nal$emp_ppal.amp <- 2
cpot05_totgral.nal <- rbind(cpotinf2005_tot.nal, cpotfor2005_tot.nal)
cpot05_totg.nal <- cpot05_totgral.nal
cpot05_totg.nal %>% count (clase3, sort = T)
cpot05_totg.nal %>% count (pos_ocu, sort = T)
cpot05_totg.nal %>% count (ing7c, sort = T)
cpot05_totg.nal %>% count (ingocup, sort = T)
cpot05_totg.nal %>% count (ing_x_hrs, sort = T)
cpot05_totg.nal$clase_ing <- NA
cpot05_totg.nal$clase_ing <- ifelse(cpot05_totg.nal$clase3==2, 0, 
                                    cpot05_totg.nal$clase_ing)
cpot05_totg.nal$clase_ing <- ifelse(cpot05_totg.nal$ing_x_hrs>0, 
                                    cpot05_totg.nal$ing_x_hrs, 
                                    cpot05_totg.nal$clase_ing)
summary(cpot05_totg.nal$clase_ing)
cpot05_totg.nal %>% count (c_ocu11c, sort = T)
cpot05_totg.nal %>% count (pos_ocu, sort = T)
cpot05_totg.nal %>% count (rama, sort = T)
cpot05_totg.nal %>% count (anios_esc, sort = T)
cpot05_totg.nal %>% count (e_con, sort = T)
base_ing.p05 <- cpot05_totg.nal
base_ing.p05 <- filter(base_ing.p05, e_con!=9)
base_ing.p05 <- filter(base_ing.p05, anios_esc!=99)
base_ing.p05 <- filter(base_ing.p05, rama!=7)
base_ing.p05 <- filter(base_ing.p05, c_ocu11c!=11)
base_ing.p05$eda2 <- base_ing.p05$eda*base_ing.p05$eda
base_ing.p05$ln_ing_x_hrs <- log(base_ing.p05$ing_x_hrs) 
base_ing.p05$e_civil <- 0
base_ing.p05$e_civil <- ifelse(base_ing.p05$e_con==1|
                                 base_ing.p05$e_con==5, 1, 0)
# 1: casado/unido; 0: soltero o alguna vez unido
base_ing.p05$z_geo <- 0
base_ing.p05$z_geo <- ifelse(base_ing.p05$zona==1, 1, 0) # 1: frontera norte; 
# 0: el resto del pais
base_ing.p05$c_ocu11c <- factor(base_ing.p05$c_ocu11c)
base_ing.p05$rama<- factor(base_ing.p05$rama)
base_ing.p05$pos_ocu <- factor(base_ing.p05$pos_ocu)
base_ing.p05$e_civ <- factor(base_ing.p05$e_con)
ing_p_05 <- model.matrix(~ pos_ocu - 1, base_ing.p05)
ing_r_05 <- model.matrix(~ rama -1, base_ing.p05)
ing_c_05 <- model.matrix(~ c_ocu11c -1, base_ing.p05)
ing_e_05 <- model.matrix(~ e_civ -1, base_ing.p05)
base_ing05 <- cbind(base_ing.p05, ing_p_05, ing_r_05, ing_c_05, ing_e_05)
baseing_conpago05 <- subset(base_ing05, !(clase_ing==0 & !is.na(clase_ing)))
baseing_conpago05$ln_ing_x_hrs <- ifelse(is.na(baseing_conpago05$clase_ing), NA, 
                                         baseing_conpago05$ln_ing_x_hrs)
dis_ing05 <- svydesign(ids= ~upm, strata= ~est_d, weights= ~fac, nest=T,
                       data= baseing_conpago05)
mod.05_ing1 <- svyglm(ln_ing_x_hrs ~ eda + eda2 + sex + e_civ1 + e_civ2 + 
                        e_civ3 + e_civ4 + e_civ5 + e_civ6 + anios_esc + hrsocup + 
                        zona + pos_ocu1 + pos_ocu2 + pos_ocu3 + pos_ocu4 + rama1 + 
                        rama2 + rama3 + rama4 + rama5 + rama6 + c_ocu11c1 + 
                        c_ocu11c2 + c_ocu11c3 + c_ocu11c5 + c_ocu11c6 + 
                        c_ocu11c7 + c_ocu11c8 + c_ocu11c9 + c_ocu11c10, 
                        design= dis_ing05)
summary(mod.05_ing1)
base_imp05 <- baseing_conpago05
base_im05 <- filter(base_imp05, sex==2)
pred_im05 <- base_im05[, c('eda', 'eda2', 'e_civ1', 'e_civ2', 'e_civ3',
                           'e_civ4', 'e_civ5', 'e_civ6', 'anios_esc', 'hrsocup', 
                           'zona', 'pos_ocu1', 'pos_ocu2', 'pos_ocu3', 'pos_ocu4',
                           'rama1', 'rama2', 'rama3', 'rama4', 'rama5', 'rama6',
                           'c_ocu11c1', 'c_ocu11c2', 'c_ocu11c3', 'c_ocu11c5', 
                           'c_ocu11c6', 'c_ocu11c7', 'c_ocu11c8', 'c_ocu11c9',
                           'c_ocu11c10','ln_ing_x_hrs')]
pred_im05$ln_ing_x_hrs <- as.factor(pred_im05$ln_ing_x_hrs)
imp_im05 <- mice(pred_im05, method = "pmm", m = 15, maxit = 10)
imputados_im05 <- complete(imp_im05)
imputm_ing05 <- subset(imputados_im05, select = c(eda, ln_ing_x_hrs))
id_facm05 <- subset(base_im05, select = c(llave, fac, sex, emp_ppal.amp, cd_a, 
                                          p3, rama, pos_ocu, c_ocu11c, upm, 
                                          est_d, hrsocup))
base_imputadosm05 <- cbind(imputm_ing05, id_facm05)
base_imputadosm05$ln_ing_hora<- as.numeric(as.character
                                           (base_imputadosm05$ln_ing_x_hrs))
base_imputadosm05$ing_est_hora <- exp(base_imputadosm05$ln_ing_hora)
base_ih05 <- filter(base_imp05, sex==1)
pred_ih05 <- base_ih05[, c('eda', 'eda2', 'e_civ1', 'e_civ2', 'e_civ3',
                           'e_civ4', 'e_civ5', 'e_civ6', 'anios_esc', 'hrsocup', 
                           'zona', 'pos_ocu1', 'pos_ocu2', 'pos_ocu3', 'pos_ocu4',
                           'rama1', 'rama2', 'rama3', 'rama4', 'rama5', 'rama6',
                           'c_ocu11c1', 'c_ocu11c2', 'c_ocu11c3', 'c_ocu11c5', 
                           'c_ocu11c6', 'c_ocu11c7', 'c_ocu11c8', 'c_ocu11c9',
                           'c_ocu11c10','ln_ing_x_hrs')]
pred_ih05$ln_ing_x_hrs <- as.factor(pred_ih05$ln_ing_x_hrs)
imp_ih05 <- mice(pred_ih05, method = "pmm", m = 15, maxit = 10)
imputados_ih05 <- complete(imp_ih05)
imputh_ing05 <- subset(imputados_ih05, select = c(eda, ln_ing_x_hrs))
id_fach05 <- subset(base_ih05, select = c(llave, fac, sex, emp_ppal.amp, cd_a, 
                                          p3, rama, pos_ocu, c_ocu11c, upm, 
                                          est_d, hrsocup))
base_imputadosh05 <- cbind(imputh_ing05, id_fach05)
base_imputadosh05$ln_ing_hora<- as.numeric(as.character
                                           (base_imputadosh05$ln_ing_x_hrs))
base_imputadosh05$ing_est_hora <- exp(base_imputadosh05$ln_ing_hora)
base.ing_maestra05 <- rbind(base_imputadosm05, base_imputadosh05)
      # Estadistica basica ingresos (jovenes y no jovenes, nacional urbana 
      # y ZMVM)
cpotpt05ing_jov.nal <- filter(base.ing_maestra05, eda>=15&eda<=29)
d05ing_jov.nal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=cpotpt05ing_jov.nal)
media05ing_jov.nal <- svymean(~ing_est_hora, d05ing_jov.nal)

cpotpt05ing_nojov.nal <- filter(base.ing_maestra05, eda>=30&eda<=64)
d05ing_nojov.nal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt05ing_nojov.nal)
media05ing_nojov.nal <- svymean(~ing_est_hora, d05ing_nojov.nal)

cpotpt05ing_jov.zmvm <- filter(base.ing_maestra05, eda>=15&eda<=29, cd_a=='01')
d05ing_jov.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt05ing_jov.zmvm)
media05ing_jov.zmvm <- svymean(~ing_est_hora, d05ing_jov.zmvm)

cpotpt05ing_nojov.zmvm <- filter(base.ing_maestra05, eda>=30&eda<=64, 
                                 cd_a=='01')
d05ing_nojov.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05ing_nojov.zmvm)
media05ing_nojov.zmvm <- svymean(~ing_est_hora, d05ing_nojov.zmvm)

# Estadistica de ingresos por grupo etario y tipo de trabajo (nacional 
# urbana y ZMVM)
cpotpt05ing_1519.nal <- filter(base.ing_maestra05, eda>=15&eda<=19)
d05ing_1519.nal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt05ing_1519.nal)
media05ing_1519.nal <- svymean(~ing_est_hora, d05ing_1519.nal)

cpotpt05ing_2024.nal <- filter(base.ing_maestra05, eda>=05&eda<=24)
d05ing_2024.nal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt05ing_2024.nal)
media05ing_2024.nal <- svymean(~ing_est_hora, d05ing_2024.nal)

cpotpt05ing_2529.nal <- filter(base.ing_maestra05, eda>=25&eda<=29)
d05ing_2529.nal <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt05ing_2529.nal)
media05ing_2529.nal <- svymean(~ing_est_hora, d05ing_2529.nal)

cpotpt05ing_1519.zmvm <- filter(base.ing_maestra05, eda>=15&eda<=19, cd_a=='01')
d05ing_1519.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt05ing_1519.zmvm)
media05ing_1519.zmvm <- svymean(~ing_est_hora, d05ing_1519.zmvm)

cpotpt05ing_2024.zmvm <- filter(base.ing_maestra05, eda>=05&eda<=24, cd_a=='01')
d05ing_2024.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt05ing_2024.zmvm)
media05ing_2024.zmvm <- svymean(~ing_est_hora, d05ing_2024.zmvm)

cpotpt05ing_2529.zmvm <- filter(base.ing_maestra05, eda>=25&eda<=29, cd_a=='01')
d05ing_2529.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=cpotpt05ing_2529.zmvm)
media05ing_2529.zmvm <- svymean(~ing_est_hora, d05ing_2529.zmvm)

cpotpt05ing_1519.nal.inf <- filter(base.ing_maestra05, emp_ppal.amp==1, 
                                   eda>=15&eda<=19)
d05ing_1519.nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt05ing_1519.nal.inf)
media05ing_1519.nal.inf <- svymean(~ing_est_hora, d05ing_1519.nal.inf)

cpotpt05ing_2024.nal.inf <- filter(base.ing_maestra05, emp_ppal.amp==1, 
                                   eda>=05&eda<=24)
d05ing_2024.nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt05ing_2024.nal.inf)
media05ing_2024.nal.inf <- svymean(~ing_est_hora, d05ing_2024.nal.inf)

cpotpt05ing_2529.nal.inf <- filter(base.ing_maestra05, emp_ppal.amp==1, 
                                   eda>=25&eda<=29)
d05ing_2529.nal.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt05ing_2529.nal.inf)
media05ing_2529.nal.inf <- svymean(~ing_est_hora, d05ing_2529.nal.inf)

cpotpt05ing_1519.zmvm.inf <- filter(base.ing_maestra05, emp_ppal.amp==1, 
                                    eda>=15&eda<=19, cd_a=='01')
d05ing_1519.zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                  data=cpotpt05ing_1519.zmvm.inf)
media05ing_1519.zmvm.inf <- svymean(~ing_est_hora, d05ing_1519.zmvm.inf)

cpotpt05ing_2024.zmvm.inf <- filter(base.ing_maestra05, emp_ppal.amp==1, 
                                    eda>=05&eda<=24, cd_a=='01')
d05ing_2024.zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                  data=cpotpt05ing_2024.zmvm.inf)
media05ing_2024.zmvm.inf <- svymean(~ing_est_hora, d05ing_2024.zmvm.inf)

cpotpt05ing_2529.zmvm.inf <- filter(base.ing_maestra05, emp_ppal.amp==1, 
                                    eda>=25&eda<=29, cd_a=='01')
d05ing_2529.zmvm.inf <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                  data=cpotpt05ing_2529.zmvm.inf)
media05ing_2529.zmvm.inf <- svymean(~ing_est_hora, d05ing_2529.zmvm.inf)

cpotpt05ing_1519.nal.for <- filter(base.ing_maestra05, emp_ppal.amp==2, 
                                   eda>=15&eda<=19)
d05ing_1519.nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt05ing_1519.nal.for)
media05ing_1519.nal.for <- svymean(~ing_est_hora, d05ing_1519.nal.for)

cpotpt05ing_2024.nal.for <- filter(base.ing_maestra05, emp_ppal.amp==2, 
                                   eda>=05&eda<=24)
d05ing_2024.nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt05ing_2024.nal.for)
media05ing_2024.nal.for <- svymean(~ing_est_hora, d05ing_2024.nal.for)

cpotpt05ing_2529.nal.for <- filter(base.ing_maestra05, emp_ppal.amp==2, 
                                   eda>=25&eda<=29)
d05ing_2529.nal.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                 data=cpotpt05ing_2529.nal.for)
media05ing_2529.nal.for <- svymean(~ing_est_hora, d05ing_2529.nal.for)

cpotpt05ing_1519.zmvm.for <- filter(base.ing_maestra05, emp_ppal.amp==2, 
                                    eda>=15&eda<=19, cd_a=='01')
d05ing_1519.zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                  data=cpotpt05ing_1519.zmvm.for)
media05ing_1519.zmvm.for <- svymean(~ing_est_hora, d05ing_1519.zmvm.for)

cpotpt05ing_2024.zmvm.for <- filter(base.ing_maestra05, emp_ppal.amp==2, 
                                    eda>=05&eda<=24, cd_a=='01')
d05ing_2024.zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                  data=cpotpt05ing_2024.zmvm.for)
media05ing_2024.zmvm.for <- svymean(~ing_est_hora, d05ing_2024.zmvm.for)

cpotpt05ing_2529.zmvm.for <- filter(base.ing_maestra05, emp_ppal.amp==2, 
                                    eda>=25&eda<=29, cd_a=='01')
d05ing_2529.zmvm.for <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                                  data=cpotpt05ing_2529.zmvm.for)
media05ing_2529.zmvm.for <- svymean(~ing_est_hora, d05ing_2529.zmvm.for)

#Estadistica ingresos ZMVM jovenes (hombres y mujeres)
cpotpt05ing_jovti.zmvm <- filter(base.ing_maestra05, eda>=15&eda<=29, 
                                 cd_a=='01', emp_ppal.amp==1)
d05ing_jovti.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05ing_jovti.zmvm)
media05ing_jovti.zmvm <- svymean(~ing_est_hora, d05ing_jovti.zmvm)

cpotpt05ing_jovtf.zmvm <- filter(base.ing_maestra05, eda>=15&eda<=29, 
                                 cd_a=='01', emp_ppal.amp==2)
d05ing_jovtf.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05ing_jovtf.zmvm)
media05ing_jovtf.zmvm <- svymean(~ing_est_hora, d05ing_jovtf.zmvm)

cpotpt05ing_jovhi.zmvm <- filter(base.ing_maestra05, eda>=15&eda<=29, 
                                 cd_a=='01', emp_ppal.amp==1, sex==1)
d05ing_jovhi.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05ing_jovhi.zmvm)
media05ing_jovhi.zmvm <- svymean(~ing_est_hora, d05ing_jovhi.zmvm)

svyquantile(~ing_est_hora, d05ing_jovhi.zmvm, quantiles=c(0.25,0.5,0.75), ci=F)
cpotpt05ing_jovhf.zmvm <- filter(base.ing_maestra05, eda>=15&eda<=29, 
                                 cd_a=='01', emp_ppal.amp==2, sex==1)
d05ing_jovhf.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05ing_jovhf.zmvm)
media05ing_jovhf.zmvm <- svymean(~ing_est_hora, d05ing_jovhf.zmvm)

svyquantile(~ing_est_hora, d05ing_jovhf.zmvm, quantiles=c(0.25,0.5,0.75), ci=F)
cpotpt05ing_jovmi.zmvm <- filter(base.ing_maestra05, eda>=15&eda<=29, 
                                 cd_a=='01', emp_ppal.amp==1, sex==2)
d05ing_jovmi.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05ing_jovmi.zmvm)
media05ing_jovmi.zmvm <- svymean(~ing_est_hora, d05ing_jovmi.zmvm)

svyquantile(~ing_est_hora, d05ing_jovmi.zmvm, quantiles=c(0.25,0.5,0.75), ci=F)
cpotpt05ing_jovmf.zmvm <- filter(base.ing_maestra05, eda>=15&eda<=29, 
                                 cd_a=='01', emp_ppal.amp==2, sex==2)
d05ing_jovmf.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                               data=cpotpt05ing_jovmf.zmvm)
media05ing_jovmf.zmvm <- svymean(~ing_est_hora, d05ing_jovmf.zmvm)

svyquantile(~ing_est_hora, d05ing_jovmf.zmvm, quantiles=c(0.25,0.5,0.75), ci=F)
        # Resumen ingresos de jovenes ZMVM 2005
cpotpt05ing_jov.zmvm <- filter(base.ing_maestra05, eda>=15&eda<=29, cd_a=='01')
d05ing_jov.zmvm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=cpotpt05ing_jov.zmvm)
svymean(~ing_est_hora, d05ing_jov.zmvm)
sqrt(svyvar(~ing_est_hora, d05ing_jov.zmvm))
min(cpotpt05ing_jov.zmvm$ing_est_hora)
max(cpotpt05ing_jov.zmvm$ing_est_hora)
svyquantile(~ing_est_hora, d05ing_jov.zmvm, quantiles=c(0.25,0.5,0.75), ci=F)

extrac_sal.gre <- c(media05ing_jov.nal, media05ing_nojov.nal, 
                    media05ing_jov.zmvm, media05ing_nojov.zmvm, 
                    media20ing_jov.nal, media20ing_nojov.nal, 
                    media20ing_jov.zmvm, media20ing_nojov.zmvm)
extrac_sal.ge <- c(media05ing_1519.nal, media05ing_2024.nal, media05ing_2529.nal,
                   media05ing_1519.nal.inf, media05ing_2024.nal.inf, 
                   media05ing_2529.nal.inf, media05ing_1519.nal.for, 
                   media05ing_2024.nal.for, media05ing_2529.nal.for, 
                   media05ing_1519.zmvm, media05ing_2024.zmvm, 
                   media05ing_2529.zmvm, media05ing_1519.zmvm.inf, 
                   media05ing_2024.zmvm.inf, media05ing_2529.zmvm.inf, 
                   media05ing_1519.zmvm.for, media05ing_2024.zmvm.for, 
                   media05ing_2529.zmvm.for, media20ing_1519.nal, 
                   media20ing_2024.nal, media20ing_2529.nal,
                   media20ing_1519.nal.inf, media20ing_2024.nal.inf, 
                   media20ing_2529.nal.inf, media20ing_1519.nal.for, 
                   media20ing_2024.nal.for, media20ing_2529.nal.for, 
                   media20ing_1519.zmvm, media20ing_2024.zmvm, 
                   media20ing_2529.zmvm, media20ing_1519.zmvm.inf, 
                   media20ing_2024.zmvm.inf, media20ing_2529.zmvm.inf, 
                   media20ing_1519.zmvm.for, media20ing_2024.zmvm.for, 
                   media20ing_2529.zmvm.for)
extrac_sal.te <- c(media05ing_jovti.zmvm, media05ing_jovtf.zmvm, 
                   media05ing_jovhi.zmvm, media05ing_jovhf.zmvm,
                   media05ing_jovmi.zmvm, media05ing_jovmf.zmvm, 
                   media20ing_jovti.zmvm, media20ing_jovtf.zmvm, 
                   media20ing_jovhi.zmvm, media20ing_jovhf.zmvm,
                   media20ing_jovmi.zmvm, media20ing_jovmf.zmvm)
#-------------------------------------------------------------------------------
      # Brecha de ingresos
base.ing_maestra05$ing_men <- 0
base.ing_maestra20$ing_men <- 0
base.ing_maestra05$ing_men <- (base.ing_maestra05$hrsocup*
                               base.ing_maestra05$ing_est_hora)*(30/7)
base.ing_maestra20$ing_men <- (base.ing_maestra20$hrsocup*
                               base.ing_maestra20$ing_est_hora)*(30/7)

baseing_maestra05_jov.nalh <- filter(base.ing_maestra05, eda>=15&eda<=29, 
                                     sex==1)
baseing_maestra20_jov.nalh <- filter(base.ing_maestra20, eda>=15&eda<=29, 
                                     sex==1)
baseing_maestra05_nojov.nalh <- filter(base.ing_maestra05, eda>=30&eda<=64, 
                                       sex==1)
baseing_maestra20_nojov.nalh <- filter(base.ing_maestra20, eda>=30&eda<=64, 
                                       sex==1)
baseing_maestra05_jov.zmvmh <- filter(base.ing_maestra05, eda>=15&eda<=29, 
                                     cd_a=='01', sex==1)
baseing_maestra20_jov.zmvmh <- filter(base.ing_maestra20, eda>=15&eda<=29, 
                                    cd_a=='01', sex==1)
baseing_maestra05_nojov.zmvmh <- filter(base.ing_maestra05, eda>=30&eda<=64, 
                                       cd_a=='01', sex==1)
baseing_maestra20_nojov.zmvmh <- filter(base.ing_maestra20, eda>=30&eda<=64, 
                                       cd_a=='01', sex==1)

baseing_maestra05_jov.nalm <- filter(base.ing_maestra05, eda>=15&eda<=29, 
                                     sex==2)
baseing_maestra20_jov.nalm <- filter(base.ing_maestra20, eda>=15&eda<=29, 
                                     sex==2)
baseing_maestra05_nojov.nalm <- filter(base.ing_maestra05, eda>=30&eda<=64, 
                                       sex==2)
baseing_maestra20_nojov.nalm <- filter(base.ing_maestra20, eda>=30&eda<=64, 
                                       sex==2)
baseing_maestra05_jov.zmvmm <- filter(base.ing_maestra05, eda>=15&eda<=29, 
                                      cd_a=='01', sex==2)
baseing_maestra20_jov.zmvmm <- filter(base.ing_maestra20, eda>=15&eda<=29, 
                                     cd_a=='01', sex==2)
baseing_maestra05_nojov.zmvmm <- filter(base.ing_maestra05, eda>=30&eda<=64, 
                                        cd_a=='01', sex==2)
baseing_maestra20_nojov.zmvmm <- filter(base.ing_maestra20, eda>=30&eda<=64, 
                                        cd_a=='01', sex==2)


d_05_jovnal.h <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                       data=baseing_maestra05_jov.nalh)
d_20_jovnal.h <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                           data=baseing_maestra20_jov.nalh)
d_05_nojovnal.h <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                           data=baseing_maestra05_nojov.nalh)
d_20_nojovnal.h <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                           data=baseing_maestra20_nojov.nalh)
d_05_jovzmvm.h <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=baseing_maestra05_jov.zmvmh)
d_20_jovzmvm.h <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=baseing_maestra20_jov.zmvmh)
d_05_nojovzmvm.h <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=baseing_maestra05_nojov.zmvmh)
d_20_nojovzmvm.h <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=baseing_maestra20_nojov.zmvmh)
d_05_jovnal.m <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                           data=baseing_maestra05_jov.nalm)
d_20_jovnal.m <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                           data=baseing_maestra20_jov.nalm)
d_05_nojovnal.m <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=baseing_maestra05_nojov.nalm)
d_20_nojovnal.m <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                             data=baseing_maestra20_nojov.nalm)
d_05_jovzmvm.m <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=baseing_maestra05_jov.zmvmm)
d_20_jovzmvm.m <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                            data=baseing_maestra20_jov.zmvmm)
d_05_nojovzmvm.m <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=baseing_maestra05_nojov.zmvmm)
d_20_nojovzmvm.m <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                              data=baseing_maestra20_nojov.zmvmm)

svyquantile(~ing_men, d_20_nojovnal.h, quantiles=c(0.5), ci=F)
svyquantile(~ing_men, d_20_nojovnal.m, quantiles=c(0.5), ci=F)

baseing_maestra20_1519h <- filter(base.ing_maestra20, eda>=15&eda<=19, 
                                  cd_a=='01', sex==1)
baseing_maestra20_2024h <- filter(base.ing_maestra20, eda>=20&eda<=24, 
                                  cd_a=='01', sex==1)
baseing_maestra20_2529h <- filter(base.ing_maestra20, eda>=25&eda<=29, 
                                  cd_a=='01', sex==1)
baseing_maestra20_1519m <- filter(base.ing_maestra20, eda>=15&eda<=19, 
                                  cd_a=='01', sex==2)
baseing_maestra20_2024m <- filter(base.ing_maestra20, eda>=20&eda<=24, 
                                  cd_a=='01', sex==2)
baseing_maestra20_2529m <- filter(base.ing_maestra20, eda>=25&eda<=29, 
                                  cd_a=='01', sex==2)
baseing_maestra05_1519h <- filter(base.ing_maestra05, eda>=15&eda<=19, 
                                  cd_a=='01', sex==1)
baseing_maestra05_2024h <- filter(base.ing_maestra05, eda>=20&eda<=24, 
                                  cd_a=='01', sex==1)
baseing_maestra05_2529h <- filter(base.ing_maestra05, eda>=25&eda<=29, 
                                  cd_a=='01', sex==1)
baseing_maestra05_1519m <- filter(base.ing_maestra05, eda>=15&eda<=19, 
                                  cd_a=='01', sex==2)
baseing_maestra05_2024m <- filter(base.ing_maestra05, eda>=20&eda<=24, 
                                  cd_a=='01', sex==2)
baseing_maestra05_2529m <- filter(base.ing_maestra05, eda>=25&eda<=29, 
                                  cd_a=='01', sex==2)
d05ah <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_maestra05_1519h)
d05bh <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_maestra05_2024h)
d05ch <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_maestra05_2529h)
d20ah <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_maestra20_1519h)
d20bh <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_maestra20_2024h)
d20ch <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_maestra20_2529h)
d05am <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_maestra05_1519m)
d05bm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_maestra05_2024m)
d05cm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_maestra05_2529m)
d20am <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_maestra20_1519m)
d20bm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_maestra20_2024m)
d20cm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_maestra20_2529m)

svyquantile(~ing_est_hora, d05ah, quantiles=c(0.5), ci=F)
svyquantile(~ing_est_hora, d05am, quantiles=c(0.5), ci=F)

baseing_zmvm05.hinf <- filter(base.ing_maestra05, eda>=15&eda<=29, cd_a=='01',
                              emp_ppal.amp==1, sex==1)
baseing_zmvm05.minf <- filter(base.ing_maestra05, eda>=15&eda<=29, cd_a=='01',
                              emp_ppal.amp==1, sex==2)
baseing_zmvm05.hfor <- filter(base.ing_maestra05, eda>=15&eda<=29, cd_a=='01',
                              emp_ppal.amp==2, sex==1)
baseing_zmvm05.mfor <- filter(base.ing_maestra05, eda>=15&eda<=29, cd_a=='01',
                              emp_ppal.amp==2, sex==2)
baseing_nal05.hinf <- filter(base.ing_maestra05, eda>=15&eda<=29,
                              emp_ppal.amp==1, sex==1)
baseing_nal05.minf <- filter(base.ing_maestra05, eda>=15&eda<=29,
                              emp_ppal.amp==1, sex==2)
baseing_nal05.hfor <- filter(base.ing_maestra05, eda>=15&eda<=29,
                             emp_ppal.amp==2, sex==1)
baseing_nal05.mfor <- filter(base.ing_maestra05, eda>=15&eda<=29,
                             emp_ppal.amp==2, sex==2)
baseing_zmvm20.hinf <- filter(base.ing_maestra20, eda>=15&eda<=29, cd_a=='01',
                              emp_ppal.amp==1, sex==1)
baseing_zmvm20.minf <- filter(base.ing_maestra20, eda>=15&eda<=29, cd_a=='01',
                              emp_ppal.amp==1, sex==2)
baseing_zmvm20.hfor <- filter(base.ing_maestra20, eda>=15&eda<=29, cd_a=='01',
                              emp_ppal.amp==2, sex==1)
baseing_zmvm20.mfor <- filter(base.ing_maestra20, eda>=15&eda<=29, cd_a=='01',
                              emp_ppal.amp==2, sex==2)
baseing_nal20.hinf <- filter(base.ing_maestra20, eda>=15&eda<=29,
                             emp_ppal.amp==1, sex==1)
baseing_nal20.minf <- filter(base.ing_maestra20, eda>=15&eda<=29,
                             emp_ppal.amp==1, sex==2)
baseing_nal20.hfor <- filter(base.ing_maestra20, eda>=15&eda<=29,
                             emp_ppal.amp==2, sex==1)
baseing_nal20.mfor <- filter(base.ing_maestra20, eda>=15&eda<=29,
                             emp_ppal.amp==2, sex==2)
dz05ih <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_zmvm05.hinf)
dz05im <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                   data=baseing_zmvm05.minf)
dz05fh <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_zmvm05.hfor)
dz05fm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_zmvm05.mfor)
dz20ih <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_zmvm20.hinf)
dz20im <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_zmvm20.minf)
dz20fh <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_zmvm20.hfor)
dz20fm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_zmvm20.mfor)
dn05ih <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_nal05.hinf)
dn05im <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_nal05.minf)
dn05fh <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_nal05.hfor)
dn05fm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_nal05.mfor)
dn20ih <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_nal20.hinf)
dn20im <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_nal20.minf)
dn20fh <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_nal20.hfor)
dn20fm <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                    data=baseing_nal20.mfor)
svyquantile(~ing_men, dn20ih, quantiles=c(0.5), ci=F)
svyquantile(~ing_men, dn20im, quantiles=c(0.5), ci=F)
    # Datos observados ingresos (sin imputacion)
base_ing2005n <- filter(baseing_conpago05, !is.na(clase_ing))
base_ing2005n$ing_ajus <- base_ing2005n$ing_x_hrs * base_ing2005n$fac
sum_ing2005n <- aggregate(ing_ajus ~ rama, data = base_ing2005n, sum)
sum_fac2005n <- aggregate(fac ~ rama, data = base_ing2005n, sum)
ing_table05n <- cbind(sum_ing2005n, sum_fac2005n)
ing_table05n$ing_est <- ing_table05n$ing_ajus/ing_table05n$fac
sum(base_ing2005n$ing_ajus)/sum(base_ing2005n$fac)
base_ing2020n <- filter(baseing_conpago20, !is.na(clase_ing))
base_ing2020n$ing_ajus <- base_ing2020n$ing_x_hrs * base_ing2020n$fac
sum_ing2020n <- aggregate(ing_ajus ~ rama, data = base_ing2020n, sum)
sum_fac2020n <- aggregate(fac ~ rama, data = base_ing2020n, sum)
ing_table20n <- cbind(sum_ing2020n, sum_fac2020n)
ing_table20n$ing_est <- ing_table20n$ing_ajus/ing_table20n$fac
sum(base_ing2020n$ing_ajus)/sum(base_ing2020n$fac)
base_ing2005z <- filter(baseing_conpago05, !is.na(clase_ing), cd_a=='01')
base_ing2005z$ing_ajus <- base_ing2005z$ing_x_hrs * base_ing2005z$fac
sum_ing2005z <- aggregate(ing_ajus ~ rama, data = base_ing2005z, sum)
sum_fac2005z <- aggregate(fac ~ rama, data = base_ing2005z, sum)
ing_table05z <- cbind(sum_ing2005z, sum_fac2005z)
ing_table05z$ing_est <- ing_table05z$ing_ajus/ing_table05z$fac
sum(base_ing2005z$ing_ajus)/sum(base_ing2005z$fac)
base_ing2020z <- filter(baseing_conpago20, !is.na(clase_ing), cd_a=='01')
base_ing2020z$ing_ajus <- base_ing2020z$ing_x_hrs * base_ing2020z$fac
sum_ing2020z <- aggregate(ing_ajus ~ rama, data = base_ing2020z, sum)
sum_fac2020z <- aggregate(fac ~ rama, data = base_ing2020z, sum)
ing_table20z <- cbind(sum_ing2020z, sum_fac2020z)
ing_table20z$ing_est <- ing_table20z$ing_ajus/ing_table20z$fac
sum(base_ing2020z$ing_ajus)/sum(base_ing2020z$fac)
part05n <- subset(ing_table05n, select=c(ing_est))
part05z <- subset(ing_table05z, select=c(ing_est))
part20n <- subset(ing_table20n, select=c(ing_est))
part20z <- subset(ing_table20z, select=c(ing_est))
partes_ing <- cbind(part05n, part20n, part05z, part20z)
ds05ingn <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                      data=base_ing2005n)
ds20ingn <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                      data=base_ing2020n)
ds05ingz <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                      data=base_ing2005z)
ds20ingz <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                      data=base_ing2020z)
v1o <- svyvar(~ing_x_hrs, design = ds05ingn)
v2o <- svyvar(~ing_x_hrs, design = ds20ingn)
v3o <- svyvar(~ing_x_hrs, design = ds05ingz)
v4o <- svyvar(~ing_x_hrs, design = ds20ingz)
sqrt(v1o)
sqrt(v2o)
sqrt(v3o)
sqrt(v4o)
svyquantile(~ing_x_hrs, ds05ingn, quantiles=c(0.5), ci=F)
svyquantile(~ing_x_hrs, ds20ingn, quantiles=c(0.5), ci=F)
svyquantile(~ing_x_hrs, ds05ingz, quantiles=c(0.5), ci=F)
svyquantile(~ing_x_hrs, ds20ingz, quantiles=c(0.5), ci=F)
svyquantile(~ing_x_hrs, ds05ingn, quantiles=c(0.25, 0.75), ci=F)
svyquantile(~ing_x_hrs, ds20ingn, quantiles=c(0.25, 0.75), ci=F)
svyquantile(~ing_x_hrs, ds05ingz, quantiles=c(0.25, 0.75), ci=F)
svyquantile(~ing_x_hrs, ds20ingz, quantiles=c(0.25, 0.75), ci=F)
base_ing2005n.est <- rbind(base_imputadosm05, base_imputadosh05)
base_ing2005n.est$ing_ajus <- base_ing2005n.est$ing_est_hora*
  base_ing2005n.est$fac
sum_ing2005n.est <- aggregate(ing_ajus ~ rama, data = base_ing2005n.est, 
                              sum)
sum_fac2005n.est <- aggregate(fac ~ rama, data = base_ing2005n.est, sum)
ing_table05n.est <- cbind(sum_ing2005n.est, sum_fac2005n.est)
ing_table05n.est$ing_est <- ing_table05n.est$ing_ajus/ing_table05n.est$fac
sum(base_ing2005n.est$ing_ajus)/sum(base_ing2005n.est$fac)
base_ing2020n.est <- rbind(base_imputadosm20, base_imputadosh20)
base_ing2020n.est$ing_ajus <- base_ing2020n.est$ing_est_hora*
  base_ing2020n.est$fac
sum_ing2020n.est <- aggregate(ing_ajus ~ rama, data = base_ing2020n.est, 
                              sum)
sum_fac2020n.est <- aggregate(fac ~ rama, data = base_ing2020n.est, sum)
ing_table20n.est <- cbind(sum_ing2020n.est, sum_fac2020n.est)
ing_table20n.est$ing_est <- ing_table20n.est$ing_ajus/ing_table20n.est$fac
sum(base_ing2020n.est$ing_ajus)/sum(base_ing2020n.est$fac)
base_ing2005z.est <- filter(base_ing2005n.est, cd_a=='01')
base_ing2005z.est$ing_ajus <- base_ing2005z.est$ing_est_hora*
  base_ing2005z.est$fac
sum_ing2005z.est <- aggregate(ing_ajus ~ rama, data = base_ing2005z.est, 
                              sum)
sum_fac2005z.est <- aggregate(fac ~ rama, data = base_ing2005z.est, sum)
ing_table05z.est <- cbind(sum_ing2005z.est, sum_fac2005z.est)
ing_table05z.est$ing_est <- ing_table05z.est$ing_ajus/ing_table05z.est$fac
sum(base_ing2005z.est$ing_ajus)/sum(base_ing2005z.est$fac)
base_ing2020z.est <- filter(base_ing2020n.est, cd_a=='01')
base_ing2020z.est$ing_ajus <- base_ing2020z.est$ing_est_hora*
  base_ing2020z.est$fac
sum_ing2020z.est <- aggregate(ing_ajus ~ rama, data = base_ing2020z.est, 
                              sum)
sum_fac2020z.est <- aggregate(fac ~ rama, data = base_ing2020z.est, sum)
ing_table20z.est <- cbind(sum_ing2020z.est, sum_fac2020z.est)
ing_table20z.est$ing_est <- ing_table20z.est$ing_ajus/ing_table20z.est$fac
sum(base_ing2020z.est$ing_ajus)/sum(base_ing2020z.est$fac)
part05n.est <- subset(ing_table05n.est, select=c(ing_est))
part05z.est <- subset(ing_table05z.est, select=c(ing_est))
part20n.est <- subset(ing_table20n.est, select=c(ing_est))
part20z.est <- subset(ing_table20z.est, select=c(ing_est))
partes_ing.est <- cbind(part05n.est, part20n.est, part05z.est, part20z.est)
ds05ingne <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                       data=base_ing2005n.est)
ds20ingne <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                       data=base_ing2020n.est)
ds05ingze <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                       data=base_ing2005z.est)
ds20ingze <- svydesign(ids=~upm, strata=~est_d, weights=~fac, nest=T,
                       data=base_ing2020z.est)
v1e <- svyvar(~ing_est_hora, design = ds05ingne)
v2e <- svyvar(~ing_est_hora, design = ds20ingne)
v3e <- svyvar(~ing_est_hora, design = ds05ingze)
v4e <- svyvar(~ing_est_hora, design = ds20ingze)
sqrt(v1e)
sqrt(v2e)
sqrt(v3e)
sqrt(v4e)
svyquantile(~ing_est_hora, ds05ingne, quantiles=c(0.5), ci=F)
svyquantile(~ing_est_hora, ds20ingne, quantiles=c(0.5), ci=F)
svyquantile(~ing_est_hora, ds05ingze, quantiles=c(0.5), ci=F)
svyquantile(~ing_est_hora, ds20ingze, quantiles=c(0.5), ci=F)
svyquantile(~ing_est_hora, ds05ingne, quantiles=c(0.25, 0.75), ci=F)
svyquantile(~ing_est_hora, ds20ingne, quantiles=c(0.25, 0.75), ci=F)
svyquantile(~ing_est_hora, ds05ingze, quantiles=c(0.25, 0.75), ci=F)
svyquantile(~ing_est_hora, ds20ingze, quantiles=c(0.25, 0.75), ci=F)
#-------------------------------------------------------------------------------
  # Datos segundo nivel
    # Calidad urbana y disponibilidad de transporte por AGEB
ent_urb.cdmx <- read.csv('TI_MANZANA_EU_09.csv', header=T, sep=',', 
                         fileEncoding ='latin1')
ent_urb.hgo<- read.csv('TI_MANZANA_EU_13.csv', header=T, sep=',', 
                       fileEncoding ='latin1')
ent_urb.edomex <- read.csv('TI_MANZANA_EU_15.csv', header=T, sep=',', 
                           fileEncoding ='latin1')
ent_urb.zmvm <- rbind(ent_urb.cdmx, ent_urb.hgo, ent_urb.edomex)
ent_urb.zmvm$ENT <- as.character(ent_urb.zmvm$ENT)
ent_urb.zmvm$ENT <- str_pad(ent_urb.zmvm$ENT, width=2, pad='0')
ent_urb.zmvm$MUN <- as.character(ent_urb.zmvm$MUN)
ent_urb.zmvm$MUN <- str_pad(ent_urb.zmvm$MUN, width=3, pad='0')
ent_urb.zmvm$LOC <- as.character(ent_urb.zmvm$LOC)
ent_urb.zmvm$LOC <- str_pad(ent_urb.zmvm$LOC, width=4, pad='0')
ent_urb.zmvm$AGEB <- as.character(ent_urb.zmvm$AGEB)
ent_urb.zmvm$AGEB <- str_pad(ent_urb.zmvm$AGEB, width=4, pad='0')
ent_urb.zmvm$ID_MUN <- paste0(ent_urb.zmvm$ENT, ent_urb.zmvm$MUN)
ent_urb.zmvm$ID_AGEB <- paste0(ent_urb.zmvm$ENT, ent_urb.zmvm$MUN, 
                              ent_urb.zmvm$LOC, ent_urb.zmvm$AGEB)
eu_base.zmvm <- filter(ent_urb.zmvm, ID_MUN=='09010'|	ID_MUN=='09012'|	
                         ID_MUN=='09015'|	ID_MUN=='09017'|	ID_MUN=='09011'|	
                         ID_MUN=='09002'|	ID_MUN=='09003'|	ID_MUN=='09013'|	
                         ID_MUN=='09004'|	ID_MUN=='09016'|	ID_MUN=='09005'|	
                         ID_MUN=='09008'|	ID_MUN=='09007'|	ID_MUN=='09006'|	
                         ID_MUN=='09009'| ID_MUN=='09014'|	ID_MUN=='13069'|	
                         ID_MUN=='15002'|	ID_MUN=='15092'|	ID_MUN=='15108'|	
                         ID_MUN=='15091'|	ID_MUN=='15099'|	ID_MUN=='15025'|	
                         ID_MUN=='15016'|	ID_MUN=='15069'|	ID_MUN=='15057'|	
                         ID_MUN=='15060'|	ID_MUN=='15015'|	ID_MUN=='15029'|	
                         ID_MUN=='15095'|	ID_MUN=='15112'|	ID_MUN=='15046'|	
                         ID_MUN=='15030'| ID_MUN=='15044'|	ID_MUN=='15036'|	
                         ID_MUN=='15053'|	ID_MUN=='15100'|	ID_MUN=='15038'|	
                         ID_MUN=='15024'|	ID_MUN=='15121'|	ID_MUN=='15084'|	
                         ID_MUN=='15010'|	ID_MUN=='15122'|	ID_MUN=='15094'|	                          
                         ID_MUN=='15050'|	ID_MUN=='15011'|	ID_MUN=='15020'|	
                         ID_MUN=='15059'| ID_MUN=='15023'|	ID_MUN=='15028'|	
                         ID_MUN=='15033'| ID_MUN=='15017'|	ID_MUN=='15093'|	
                         ID_MUN=='15083'|	ID_MUN=='15031'|	ID_MUN=='15070'|	
                         ID_MUN=='15096'|	ID_MUN=='15109'|	ID_MUN=='15125'|	
                         ID_MUN=='15061'|	ID_MUN=='15058'|	ID_MUN=='15022'|	
                         ID_MUN=='15068'|	ID_MUN=='15081'|	ID_MUN=='15089'|	
                         ID_MUN=='15037'|	ID_MUN=='15013'|	ID_MUN=='15065'|	
                         ID_MUN=='15120'|	ID_MUN=='15039'|	ID_MUN=='15104'|	
                         ID_MUN=='15075'|	ID_MUN=='15035'|	ID_MUN=='15103'|	
                         ID_MUN=='15009'| ID_MUN=='15034')
dim(eu_base.zmvm)
t.ent_urb <- aggregate(cbind(PERSONAS, TOTVIAL, PAVIM_N, ADOQ_N, C_PASOPEAT_N, 
                             C_DRENAJEP_N, C_ARBOL_N, C_ALUM_N, C_BANQ_N) ~ 
                             ID_AGEB, data = eu_base.zmvm, sum)
t.ent_urb$C_RECUB_N <- t.ent_urb$PAVIM_N + t.ent_urb$ADOQ_N
dim(t.ent_urb)
t.ent_urb$recubre_p <- t.ent_urb$C_RECUB_N/t.ent_urb$TOTVIAL
t.ent_urb$peaton_p <- t.ent_urb$C_PASOPEAT_N/t.ent_urb$TOTVIAL
t.ent_urb$drenaje_p <- t.ent_urb$C_DRENAJEP_N/t.ent_urb$TOTVIAL
t.ent_urb$arbol_p <- t.ent_urb$C_ARBOL_N/t.ent_urb$TOTVIAL
t.ent_urb$alumbra_p <- t.ent_urb$C_ALUM_N/t.ent_urb$TOTVIAL
t.ent_urb$acera_p <- t.ent_urb$C_BANQ_N/t.ent_urb$TOTVIAL
t.ent_urb$recubre_n <- ((t.ent_urb$recubre_p-mean(t.ent_urb$recubre_p[is.finite
                       (t.ent_urb$recubre_p)]))/sd(t.ent_urb$recubre_p[is.finite
                       (t.ent_urb$recubre_p)]))*10
t.ent_urb$peaton_n <- ((t.ent_urb$peaton_p-mean(t.ent_urb$peaton_p[is.finite
                      (t.ent_urb$peaton_p)]))/sd(t.ent_urb$peaton_p[is.finite
                      (t.ent_urb$peaton_p)]))*10
t.ent_urb$drenaje_n <- ((t.ent_urb$drenaje_p-mean(t.ent_urb$drenaje_p[is.finite
                       (t.ent_urb$drenaje_p)]))/sd(t.ent_urb$drenaje_p[is.finite
                       (t.ent_urb$drenaje_p)]))*10
t.ent_urb$arbol_n <- ((t.ent_urb$arbol_p-mean(t.ent_urb$arbol_p[is.finite
                     (t.ent_urb$arbol_p)]))/sd(t.ent_urb$arbol_p[is.finite
                     (t.ent_urb$arbol_p)]))*10
t.ent_urb$alumbra_n <- ((t.ent_urb$alumbra_p-mean(t.ent_urb$alumbra_p[is.finite
                       (t.ent_urb$alumbra_p)]))/sd(t.ent_urb$alumbra_p[is.finite
                       (t.ent_urb$alumbra_p)]))*10
t.ent_urb$acera_n <- ((t.ent_urb$acera_p-mean(t.ent_urb$acera_p[is.finite
                     (t.ent_urb$acera_p)]))/sd(t.ent_urb$acera_p[is.finite
                     (t.ent_urb$acera_p)]))*10
min(t.ent_urb$recubre_n[is.finite(t.ent_urb$recubre_n)])
min(t.ent_urb$peaton_n[is.finite(t.ent_urb$peaton_n)])
min(t.ent_urb$drenaje_n[is.finite(t.ent_urb$drenaje_n)])
min(t.ent_urb$arbol_n[is.finite(t.ent_urb$arbol_n)])
min(t.ent_urb$alumbra_n[is.finite(t.ent_urb$alumbra_n)])
min(t.ent_urb$acera_n[is.finite(t.ent_urb$acera_n)])
t.ent_urb$recubre_s <- t.ent_urb$recubre_n+31
t.ent_urb$peaton_s<- t.ent_urb$peaton_n+31
t.ent_urb$drenaje_s <- t.ent_urb$drenaje_n+31
t.ent_urb$arbol_s <- t.ent_urb$arbol_n+31
t.ent_urb$alumbra_s <- t.ent_urb$alumbra_n+31
t.ent_urb$acera_s <- t.ent_urb$acera_n+31
t.ent_urb$ind_ent_urb <- t.ent_urb$recubre_s + t.ent_urb$peaton_s + 
                         t.ent_urb$drenaje_s + t.ent_urb$arbol_s + 
                         t.ent_urb$alumbra_s + t.ent_urb$acera_s
dim(t.ent_urb)
summary(t.ent_urb$ind_ent_urb)
quantile(t.ent_urb$ind_ent_urb, c(0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
disp_transp <- aggregate(cbind(C_TRANSCOL_N, TOTVIAL) ~ 
                         ID_AGEB, data = eu_base.zmvm, sum)
disp_transp$p_transporte <- disp_transp$C_TRANSCOL_N/disp_transp$TOTVIAL
dim(disp_transp)
summary(disp_transp$p_transporte)
quantile(disp_transp$p_transporte, c(0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
    # Desempleo y educación acumulada por AGEB
p_results.cdmx <- read.csv('RESAGEBURB_09CSV20.csv', header=T, sep=',', 
                         fileEncoding ='latin1')
p_results.hgo<- read.csv('RESAGEBURB_13CSV20.csv', header=T, sep=',', 
                       fileEncoding ='latin1')
p_results.edomex <- read.csv('RESAGEBURB_15CSV20.csv', header=T, sep=',', 
                           fileEncoding ='latin1')
p_results.cdmx$ENT <- 09
p_results.hgo$ENT <- 13
p_results.edomex$ENT <- 15
p_results.zmvm <- rbind(p_results.cdmx, p_results.hgo, p_results.edomex)
p_results.zmvm$ENT <- as.character(p_results.zmvm$ENT)
p_results.zmvm$ENT <- str_pad(p_results.zmvm$ENT, width=2, pad='0')
p_results.zmvm$MUN <- as.character(p_results.zmvm$MUN)
p_results.zmvm$MUN <- str_pad(p_results.zmvm$MUN, width=3, pad='0')
p_results.zmvm$LOC <- as.character(p_results.zmvm$LOC)
p_results.zmvm$LOC <- str_pad(p_results.zmvm$LOC, width=4, pad='0')
p_results.zmvm$AGEB <- as.character(p_results.zmvm$AGEB)
p_results.zmvm$AGEB <- str_pad(p_results.zmvm$AGEB, width=4, pad='0')
p_results.zmvm$ID_MUN <- paste0(p_results.zmvm$ENT, p_results.zmvm$MUN)
p_results.zmvm$ID_AGEB <- paste0(p_results.zmvm$ENT, p_results.zmvm$MUN, 
                                 p_results.zmvm$LOC, p_results.zmvm$AGEB)
prin_base.zmvm <- filter(p_results.zmvm, ID_MUN=='09010'|	ID_MUN=='09012'|	
                         ID_MUN=='09015'|	ID_MUN=='09017'|	ID_MUN=='09011'|	
                         ID_MUN=='09002'|	ID_MUN=='09003'|	ID_MUN=='09013'|	
                         ID_MUN=='09004'|	ID_MUN=='09016'|	ID_MUN=='09005'|	
                         ID_MUN=='09008'|	ID_MUN=='09007'|	ID_MUN=='09006'|	
                         ID_MUN=='09009'| ID_MUN=='09014'|	ID_MUN=='13069'|	
                         ID_MUN=='15002'|	ID_MUN=='15092'|	ID_MUN=='15108'|	
                         ID_MUN=='15091'|	ID_MUN=='15099'|	ID_MUN=='15025'|	
                         ID_MUN=='15016'|	ID_MUN=='15069'|	ID_MUN=='15057'|	
                         ID_MUN=='15060'|	ID_MUN=='15015'|	ID_MUN=='15029'|	
                         ID_MUN=='15095'|	ID_MUN=='15112'|	ID_MUN=='15046'|	
                         ID_MUN=='15030'| ID_MUN=='15044'|	ID_MUN=='15036'|	
                         ID_MUN=='15053'|	ID_MUN=='15100'|	ID_MUN=='15038'|	
                         ID_MUN=='15024'|	ID_MUN=='15121'|	ID_MUN=='15084'|	
                         ID_MUN=='15010'|	ID_MUN=='15122'|	ID_MUN=='15094'|
                         ID_MUN=='15050'|	ID_MUN=='15011'|	ID_MUN=='15020'|	
                         ID_MUN=='15059'| ID_MUN=='15023'|	ID_MUN=='15028'|	
                         ID_MUN=='15033'| ID_MUN=='15017'|	ID_MUN=='15093'|	
                         ID_MUN=='15083'|	ID_MUN=='15031'|	ID_MUN=='15070'|	
                         ID_MUN=='15096'|	ID_MUN=='15109'|	ID_MUN=='15125'|	
                         ID_MUN=='15061'|	ID_MUN=='15058'|	ID_MUN=='15022'|	
                         ID_MUN=='15068'|	ID_MUN=='15081'|	ID_MUN=='15089'|	
                         ID_MUN=='15037'|	ID_MUN=='15013'|	ID_MUN=='15065'|	
                         ID_MUN=='15120'|	ID_MUN=='15039'|	ID_MUN=='15104'|	
                         ID_MUN=='15075'|	ID_MUN=='15035'|	ID_MUN=='15103'|	
                         ID_MUN=='15009'| ID_MUN=='15034')
prin_zmvm.ageb <- filter(prin_base.zmvm, MZA==0, AGEB!='0000')
dim(prin_zmvm.ageb)
prin_zmvm.ageb$PEA <- as.numeric(prin_zmvm.ageb$PEA)
prin_zmvm.ageb$PDESOCUP <- as.numeric(prin_zmvm.ageb$PDESOCUP)
prin_zmvm.ageb$POCUPADA <- as.numeric(prin_zmvm.ageb$POCUPADA)
prin_zmvm.ageb$PDESOCUP <- prin_zmvm.ageb$PEA - prin_zmvm.ageb$POCUPADA
prin_zmvm.ageb$t_desocup <- prin_zmvm.ageb$PDESOCUP/prin_zmvm.ageb$PEA
dim(prin_zmvm.ageb)
summary(prin_zmvm.ageb$t_desocup)
quantile(prin_zmvm.ageb$t_desocup, c(0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
prin_zmvm.ageb$educa_acum <- as.numeric(prin_zmvm.ageb$GRAPROES)
summary(prin_zmvm.ageb$educa_acum)
quantile(prin_zmvm.ageb$educa_acum, c(0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
    # Indice de marginacion urbana por AGEB
ind_marg_nal <- read.csv('IMU_2020.csv', header=T, sep=',', 
                             fileEncoding ='latin1')
ind_marg_nal$ENT <- as.character(ind_marg_nal$ENT)
ind_marg_nal$ENT <- str_pad(ind_marg_nal$ENT, width=2, pad='0')
ind_marg_nal$MUN <- as.character(ind_marg_nal$MUN)
ind_marg_nal$MUN <- str_pad(ind_marg_nal$MUN, width=3, pad='0')
ind_marg_nal$ID_MUN <- paste0(ind_marg_nal$ENT, ind_marg_nal$MUN)
ind_marg_zmvm <- filter(ind_marg_nal, ID_MUN=='09010'|	ID_MUN=='09012'|	
                           ID_MUN=='09015'|	ID_MUN=='09017'|	ID_MUN=='09011'|	
                           ID_MUN=='09002'|	ID_MUN=='09003'|	ID_MUN=='09013'|	
                           ID_MUN=='09004'|	ID_MUN=='09016'|	ID_MUN=='09005'|	
                           ID_MUN=='09008'|	ID_MUN=='09007'|	ID_MUN=='09006'|	
                           ID_MUN=='09009'| ID_MUN=='09014'|	ID_MUN=='13069'|	
                           ID_MUN=='15002'|	ID_MUN=='15092'|	ID_MUN=='15108'|	
                           ID_MUN=='15091'|	ID_MUN=='15099'|	ID_MUN=='15025'|	
                           ID_MUN=='15016'|	ID_MUN=='15069'|	ID_MUN=='15057'|	
                           ID_MUN=='15060'|	ID_MUN=='15015'|	ID_MUN=='15029'|	
                           ID_MUN=='15095'|	ID_MUN=='15112'|	ID_MUN=='15046'|	
                           ID_MUN=='15030'| ID_MUN=='15044'|	ID_MUN=='15036'|	
                           ID_MUN=='15053'|	ID_MUN=='15100'|	ID_MUN=='15038'|	
                           ID_MUN=='15024'|	ID_MUN=='15121'|	ID_MUN=='15084'|	
                           ID_MUN=='15010'|	ID_MUN=='15122'|	ID_MUN=='15094'|	                          
                           ID_MUN=='15050'|	ID_MUN=='15011'|	ID_MUN=='15020'|	
                           ID_MUN=='15059'| ID_MUN=='15023'|	ID_MUN=='15028'|	
                           ID_MUN=='15033'| ID_MUN=='15017'|	ID_MUN=='15093'|	
                           ID_MUN=='15083'|	ID_MUN=='15031'|	ID_MUN=='15070'|	
                           ID_MUN=='15096'|	ID_MUN=='15109'|	ID_MUN=='15125'|	
                           ID_MUN=='15061'|	ID_MUN=='15058'|	ID_MUN=='15022'|	
                           ID_MUN=='15068'|	ID_MUN=='15081'|	ID_MUN=='15089'|	
                           ID_MUN=='15037'|	ID_MUN=='15013'|	ID_MUN=='15065'|	
                           ID_MUN=='15120'|	ID_MUN=='15039'|	ID_MUN=='15104'|	
                           ID_MUN=='15075'|	ID_MUN=='15035'|	ID_MUN=='15103'|	
                           ID_MUN=='15009'| ID_MUN=='15034')
dim(ind_marg_zmvm)
summary(ind_marg_zmvm$IM_2020)
quantile(ind_marg_zmvm$IM_2020, c(0.2, 0.4, 0.6, 0.8, 1))
    # Distancias AGEBs al Zocalo de la CDMX
distancias_agebs <- read.csv('distancias_agebs_zocalo.csv', header=T, sep=',', 
                             fileEncoding='latin1')
distancias_agebs$ID_AGEB <- as.character(distancias_agebs$TargetID)
distancias_agebs$ID_AGEB <- str_pad(distancias_agebs$ID_AGEB, width=13, pad='0')
dim(distancias_agebs)
summary(distancias_agebs$Distance)
quantile(distancias_agebs$Distance, c(0.2, 0.4, 0.6, 0.8, 1))
    # Calidad urbana y disponibilidad de transporte por municipio
t.urb_entmun <- aggregate(cbind(PERSONAS, TOTVIAL, PAVIM_N, ADOQ_N, 
                                C_PASOPEAT_N, C_DRENAJEP_N, C_ARBOL_N, 
                                C_ALUM_N, C_BANQ_N) ~ ID_MUN, data = 
                                eu_base.zmvm, sum)
t.urb_entmun$C_RECUB_N <- t.urb_entmun$PAVIM_N + t.urb_entmun$ADOQ_N
dim(t.urb_entmun)
summary(t.urb_entmun)
t.urb_entmun$recubre_p <- t.urb_entmun$C_RECUB_N/t.urb_entmun$TOTVIAL
t.urb_entmun$peaton_p <- t.urb_entmun$C_PASOPEAT_N/t.urb_entmun$TOTVIAL
t.urb_entmun$drenaje_p <- t.urb_entmun$C_DRENAJEP_N/t.urb_entmun$TOTVIAL
t.urb_entmun$arbol_p <- t.urb_entmun$C_ARBOL_N/t.urb_entmun$TOTVIAL
t.urb_entmun$alumbra_p <- t.urb_entmun$C_ALUM_N/t.urb_entmun$TOTVIAL
t.urb_entmun$acera_p <- t.urb_entmun$C_BANQ_N/t.urb_entmun$TOTVIAL
t.urb_entmun$recubre_n <- ((t.urb_entmun$recubre_p-mean(t.urb_entmun$recubre_p
                           [is.finite(t.urb_entmun$recubre_p)]))/sd(t.urb_entmun
                           $recubre_p[is.finite(t.urb_entmun$recubre_p)]))*10
t.urb_entmun$peaton_n <- ((t.urb_entmun$peaton_p-mean(t.urb_entmun$peaton_p
                          [is.finite(t.urb_entmun$peaton_p)]))/sd(t.urb_entmun$
                          peaton_p[is.finite(t.urb_entmun$peaton_p)]))*10
t.urb_entmun$drenaje_n <- ((t.urb_entmun$drenaje_p-mean(t.urb_entmun$drenaje_p
                           [is.finite(t.urb_entmun$drenaje_p)]))/sd(t.urb_entmun
                           $drenaje_p[is.finite(t.urb_entmun$drenaje_p)]))*10
t.urb_entmun$arbol_n <- ((t.urb_entmun$arbol_p-mean(t.urb_entmun$arbol_p
                         [is.finite(t.urb_entmun$arbol_p)]))/sd(t.urb_entmun$
                         arbol_p[is.finite(t.urb_entmun$arbol_p)]))*10
t.urb_entmun$alumbra_n <- ((t.urb_entmun$alumbra_p-mean(t.urb_entmun$alumbra_p
                           [is.finite(t.urb_entmun$alumbra_p)]))/sd(t.urb_entmun
                           $alumbra_p[is.finite(t.urb_entmun$alumbra_p)]))*10
t.urb_entmun$acera_n <- ((t.urb_entmun$acera_p-mean(t.urb_entmun$acera_p
                         [is.finite(t.urb_entmun$acera_p)]))/sd(t.urb_entmun$
                         acera_p[is.finite(t.urb_entmun$acera_p)]))*10
min(t.urb_entmun$recubre_n[is.finite(t.urb_entmun$recubre_n)])
min(t.urb_entmun$peaton_n[is.finite(t.urb_entmun$peaton_n)])
min(t.urb_entmun$drenaje_n[is.finite(t.urb_entmun$drenaje_n)])
min(t.urb_entmun$arbol_n[is.finite(t.urb_entmun$arbol_n)])
min(t.urb_entmun$alumbra_n[is.finite(t.urb_entmun$alumbra_n)])
min(t.urb_entmun$acera_n[is.finite(t.urb_entmun$acera_n)])
t.urb_entmun$recubre_s <- t.urb_entmun$recubre_n+21
t.urb_entmun$peaton_s<- t.urb_entmun$peaton_n+21
t.urb_entmun$drenaje_s <- t.urb_entmun$drenaje_n+21
t.urb_entmun$arbol_s <- t.urb_entmun$arbol_n+21
t.urb_entmun$alumbra_s <- t.urb_entmun$alumbra_n+21
t.urb_entmun$acera_s <- t.urb_entmun$acera_n+21
t.urb_entmun$ind_ent_urb <- t.urb_entmun$recubre_s + t.urb_entmun$peaton_s + 
                            t.urb_entmun$drenaje_s + t.urb_entmun$arbol_s + 
                            t.urb_entmun$alumbra_s + t.urb_entmun$acera_s
summary(t.urb_entmun$ind_ent_urb[is.finite(t.urb_entmun$ind_ent_urb)])
disp_tr_mun <- aggregate(cbind(C_TRANSCOL_N, TOTVIAL) ~ ID_MUN, data = 
                         eu_base.zmvm, sum)
disp_tr_mun$p_transporte <- (disp_tr_mun$C_TRANSCOL_N/disp_tr_mun$TOTVIAL)*100
summary(disp_tr_mun$p_transporte)
quantile(disp_tr_mun$p_transporte, c(0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
    # Desempleo y educación acumulada por municipio
prin_zmvm.mun <- filter(prin_base.zmvm, ENT!='00'&MUN!='000'&LOC=='0000')
dim(prin_zmvm.mun)
prin_zmvm.mun$PEA <- as.numeric(prin_zmvm.mun$PEA)
prin_zmvm.mun$PDESOCUP <- as.numeric(prin_zmvm.mun$PDESOCUP)
prin_zmvm.mun$POCUPADA <- as.numeric(prin_zmvm.mun$POCUPADA)
prin_zmvm.mun$PDESOCUP <- prin_zmvm.mun$PEA - prin_zmvm.mun$POCUPADA
prin_zmvm.mun$t_desocup <- (prin_zmvm.mun$PDESOCUP/prin_zmvm.mun$PEA)*100
summary(prin_zmvm.mun$t_desocup)
quantile(prin_zmvm.mun$t_desocup, c(0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
prin_zmvm.mun$educa_acum <- as.numeric(prin_zmvm.mun$GRAPROES)
summary(prin_zmvm.mun$educa_acum)
quantile(prin_zmvm.mun$educa_acum, c(0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
    # Indice de marginacion urbana por municipio
indm_mun_zmvm <- aggregate(IM_2020 ~ ID_MUN, data = ind_marg_zmvm, mean)
summary(indm_mun_zmvm$IM_2020)
quantile(indm_mun_zmvm$IM_2020, c(0.2, 0.4, 0.6, 0.8, 1))
    # Distancias municipios al Zocalo de la CDMX
distancias_mun <- read.csv('distancias_municipios_zocalo.csv', header=T, 
                           sep=',', fileEncoding='latin1')
distancias_mun$ID_MUN <- as.character(distancias_mun$TargetID)
distancias_mun$ID_MUN <- str_pad(distancias_mun$ID_MUN, width=5, pad='0')
dim(distancias_mun)
summary(distancias_mun$Distance)
quantile(distancias_mun$Distance, c(0.2, 0.4, 0.6, 0.8, 1))
    # Base variables de segundo nivel municipios
b2n_1 <- subset(t.urb_entmun, select = c(ID_MUN, ind_ent_urb))
b2n_2 <- subset(disp_tr_mun, select = c(ID_MUN, p_transporte)) 
b2n_3 <- subset(prin_zmvm.mun, select = c(ID_MUN, t_desocup))
b2n_4 <- subset(prin_zmvm.mun, select = c(ID_MUN, educa_acum))
b2n_5 <- subset(indm_mun_zmvm, select = c(ID_MUN, IM_2020))
b2n_6 <- subset(distancias_mun, select = c(ID_MUN, Distance))
b2n_12 <- merge(b2n_1, b2n_2, by = 'ID_MUN', all = T)
b2n_34 <- merge(b2n_3, b2n_4, by = 'ID_MUN', all = T)
b2n_56 <- merge(b2n_5, b2n_6, by = 'ID_MUN', all = T)
b2n_12.34 <- merge(b2n_12, b2n_34, by = 'ID_MUN', all = T)
b2n_tot <- merge(b2n_12.34, b2n_56, by = 'ID_MUN', all = T)

basep_indiv <- base_jov.20
basep_indiv$ent <- as.character(basep_indiv$ent)
basep_indiv$ent <- str_pad(basep_indiv$ent, width=2, pad='0')
basep_indiv$mun <- as.character(basep_indiv$mun)
basep_indiv$mun <- str_pad(basep_indiv$mun, width=3, pad='0')
basep_indiv$ID_MUN <- paste0(basep_indiv$ent, basep_indiv$mun)
basecompleta_mun <- merge(basep_indiv, b2n_tot, by = 'ID_MUN', all = F)

#-------------------------------------------------------------------------------
  # Modelo multinivel (ejemplo)
# library(lme4)
m1_multi_mun <- glmer(emp.ppal.amp ~ 1 + (1 | ID_MUN), data = basecompleta_mun, 
                      family = binomial(link='logit'))
summary(m1_multi_mun)

m1_multi_ <- glmer(emp.ppal.amp ~ sex + eda + anios_esc + e_civil + h_trs + 
                      asis_esc + p_ocupa + sector + con_ocupa + jf_inf +
                      (1|ind_ent_urb) + (1|p_transporte) + (1|t_desocup) + 
                      (1|educa_acum) + (1|IM_2020) + (1|Distance), data =  
                      basecompleta_mun, family = binomial (link='logit'))
#-------------------------------------------------------------------------------
  # Otros
noremunerado20 <- rbind(mh20_jov.zmvm.49, mh20_jov.zmvm.39, mh20_jov.zmvm.19)
norem20_familiares <- filter(noremunerado20, p3h==3)
noremunerado05 <- rbind(mh05_jov.zmvm.49, mh05_jov.zmvm.39, mh05_jov.zmvm.19)
norem05_familiares <- filter(noremunerado05, p3h==3)
base_segundonivel <- read.csv('segundo_nivel.csv', header=T, sep=',', 
                              fileEncoding='latin1')
ggplot(base_segundonivel, aes(educa_acum, IM_2020)) +
  geom_point(shape = 16, size = 1, fill = "gray30", color = "gray30") +
  labs(x = "Años de escolaridad", y = "Índice de marginación") +
  ylim(95, 130) + theme(panel.background = element_rect(fill = "white", color = "black"))

ggplot(base_segundonivel, aes(IM_2020, cal_urbana)) +
  geom_point(shape = 16, size = 1, fill = "gray30", color = "gray30") +
  labs(x = "Índice de marginación", y = "Índice de calidad urbana") +
  xlim(95, 130) + theme(panel.background = element_rect(fill = "white", color = "black"))

ggplot(base_segundonivel, aes(educa_acum, cal_urbana)) +
  geom_point(shape = 16, size = 1, fill = "gray30", color = "gray30") +
  labs(x = "Años de escolaridad", y = "Índice de calidad urbana") +
  ylim(0, 100) + theme(panel.background = element_rect(fill = "white", color = "black"))

ggplot(base_segundonivel, aes(dis_km, cal_urbana)) +
  geom_point(shape = 16, size = 1, fill = "gray30", color = "gray30") +
  labs(x = "Distancia al centro de la Ciudad de México", y = "Índice de calidad urbana") +
  ylim(0, 100) + theme(panel.background = element_rect(fill = "white", color = "black"))

  # Estadísticas segundo empleo informal
c2potinf20_jov.zmvm <- cpotinf20_jov.zmvm 
c2potinf20_jov.zmvm$tipo.empleo <- 1
c2potfor20_jov.zmvm <- cpotfor20_jov.zmvm 
c2potfor20_jov.zmvm$tipo.empleo <- 0
jovenes_zmvm_2020 <- rbind(c2potinf20_jov.zmvm, c2potfor20_jov.zmvm)
count(jovenes_zmvm_2020, p7)
count(jovenes_zmvm_2020, p7d)
c2potinf05_jov.zmvm <- cpotinf05_jov.zmvm 
c2potinf05_jov.zmvm$tipo.empleo <- 1
c2potfor05_jov.zmvm <- cpotfor05_jov.zmvm 
c2potfor05_jov.zmvm$tipo.empleo <- 0
jovenes_zmvm_2005 <- rbind(c2potinf05_jov.zmvm, c2potfor05_jov.zmvm)
count(jovenes_zmvm_2005, p7)
count(jovenes_zmvm_2005, p7d)
c2potinf20_jov.nal <- cpotinf20_jov.nal 
c2potinf20_jov.nal$tipo.empleo <- 1
c2potfor20_jov.nal <- cpotfor20_jov.nal 
c2potfor20_jov.nal$tipo.empleo <- 0
jovenes_nal_2020 <- rbind(c2potinf20_jov.nal, c2potfor20_jov.nal)
count(jovenes_nal_2020, p7)
count(jovenes_nal_2020, p7d)
c2potinf05_jov.nal <- cpotinf05_jov.nal 
c2potinf05_jov.nal$tipo.empleo <- 1
c2potfor05_jov.nal <- cpotfor05_jov.nal 
c2potfor05_jov.nal$tipo.empleo <- 0
jovenes_nal_2005 <- rbind(c2potinf05_jov.nal, c2potfor05_jov.nal)
count(jovenes_nal_2005, p7)
count(jovenes_nal_2005, p7d)
c2potinf20_nojov.zmvm <- cpotinf20_nojov.zmvm 
c2potinf20_nojov.zmvm$tipo.empleo <- 1
c2potfor20_nojov.zmvm <- cpotfor20_nojov.zmvm 
c2potfor20_nojov.zmvm$tipo.empleo <- 0
nojovenes_zmvm_2020 <- rbind(c2potinf20_nojov.zmvm, c2potfor20_nojov.zmvm)
count(nojovenes_zmvm_2020, p7)
count(nojovenes_zmvm_2020, p7d)
c2potinf05_nojov.zmvm <- cpotinf05_nojov.zmvm 
c2potinf05_nojov.zmvm$tipo.empleo <- 1
c2potfor05_nojov.zmvm <- cpotfor05_nojov.zmvm 
c2potfor05_nojov.zmvm$tipo.empleo <- 0
nojovenes_zmvm_2005 <- rbind(c2potinf05_nojov.zmvm, c2potfor05_nojov.zmvm)
count(nojovenes_zmvm_2005, p7)
count(nojovenes_zmvm_2005, p7d)
c2potinf20_nojov.nal <- cpotinf20_nojov.nal 
c2potinf20_nojov.nal$tipo.empleo <- 1
c2potfor20_nojov.nal <- cpotfor20_nojov.nal 
c2potfor20_nojov.nal$tipo.empleo <- 0
nojovenes_nal_2020 <- rbind(c2potinf20_nojov.nal, c2potfor20_nojov.nal)
count(nojovenes_nal_2020, p7)
count(nojovenes_nal_2020, p7d)
c2potinf05_nojov.nal <- cpotinf05_nojov.nal 
c2potinf05_nojov.nal$tipo.empleo <- 1
c2potfor05_nojov.nal <- cpotfor05_nojov.nal 
c2potfor05_nojov.nal$tipo.empleo <- 0
nojovenes_nal_2005 <- rbind(c2potinf05_nojov.nal, c2potfor05_nojov.nal)
count(nojovenes_nal_2005, p7)
count(nojovenes_nal_2005, p7d)
base_act <- jovenes_nal_2020
base_actual0<- base_act[complete.cases(base_act$p7), ]
base_actual <-  base_actual0[base_actual0$p7!= 9, ]
bb1 <- subset(base_actual, select = c(tipo.empleo, fac, p7, p7d, sex))
bb1_2ocup <- filter(bb1, bb1$p7==1|bb1$p7==2|bb1$p7==3|bb1$p7==4|bb1$p7==5|
                      bb1$p7==6)
bb1_filtrados <- bb1_2ocup[complete.cases(bb1_2ocup$p7d), ]
bb1_filtrados2 <-  bb1_filtrados[bb1_filtrados$p7d!= 9, ]
sinseguridad <- filter(bb1_filtrados2, bb1_filtrados2$p7d==4)
estadistica <- filter(sinseguridad, sinseguridad$tipo.empleo==1)
sum(bb1_2ocup$fac)/sum(base_actual$fac)
sum(sinseguridad$fac)/sum(bb1_filtrados2$fac)
sum(estadistica$fac)/sum(sinseguridad$fac)