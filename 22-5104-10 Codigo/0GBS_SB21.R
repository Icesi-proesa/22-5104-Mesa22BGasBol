rm(list = ls())

## Script maestro gasto de bolsillo 2021

## Librerias
options(scipen = 999)
library(openxlsx)
library(dplyr)
library(data.table)
library(ggplot2)
library(patchwork)
library(haven)
library(reldist)
library(Hmisc)


## Directorios (Working directory - General)

#wg <- ("C:/Users/Usuario/Universidad Icesi (@icesi.edu.co)/")
wg <- ("D:/Usuarios/1130650186/Universidad Icesi (@icesi.edu.co)/")
#wg <- ("D:/Usuarios/80088802/Universidad Icesi (@icesi.edu.co)/")



# Working Directory
wd <- paste0(wg,'Proesa - 22-5104-Mesa22BGasBol/')
#wd_bd <- ("D:/Usuarios/1130650186/Universidad Icesi (@icesi.edu.co)/Proesa - PP22-7004-Ecv21/")
wd_code <- paste0(wd,'22-5104-10 Codigo/')
wd_resul <- paste0(wd,'22-5104-20 Resul/')
#wd_data <- paste0(wd,'22-5104-30 Data/')

wd_prim <- paste0(wg,'Proesa - PP22-7004-Ecv21/')
wd_base <- paste0(wg,'Proesa - PB22-5104-Mesa22GasBol/')



## 0.Extraccion bases primario ----

#source(paste0(wd_code,"/1GBSDatosPrimarios.R"))



## 1. Estimacion gasto de bolsillo ----

source(paste0(wd_code, "/2GBSEstimaciones.R"))

## 2. Salidas (Tablas y Gráficos)

source(paste0(wd_code, "/3GBSSalidas.R"))



