# Scrip para extraer datos necesario de Primario


## 1 Cargar Datos

#Base ECV modulo salud
#salud <- fread(paste0(wd_prim,'Salud.csv'),fill=TRUE)
salud0 <- read_sav(paste0(wd_prim,'Salud.sav'))

#Base ECV modulo servicios del hogar
#SVH <- fread(paste0(wd_prim,'Servicios del hogar.csv'),fill=TRUE)
SVH0 <- read_sav(paste0(wd_prim,'Servicios del hogar.sav'))

#Base ECV modulo Gastos de los hogares (Gastos por Item)
dt_GHItem <- read_sav(paste0(wd_prim,'Gastos de los hogares (Gastos por Item).sav'))

#Base ECV modulo Tenencia y financiacion de la vivienda que ocupa el hogar
TenFinanViv <- read_dta(paste0(wd_prim,'Tenencia y financiacion de la vivienda que ocupa el hogar.dta'))


#Base ECV modulo datos de la vivienda
dt_datosVivi <- read_sav(paste0(wd_prim,'Datos de la vivienda.sav'))



## 2 Renombrar variables
salud1 <- salud0 %>% mutate (idv=DIRECTORIO,
                           idh=SECUENCIA_P,
                           idi= SECUENCIA_ENCUESTA)

SVH1 <- SVH0 %>% mutate (idv=DIRECTORIO,
                       idh=SECUENCIA_ENCUESTA)


dt_GHItem <- dt_GHItem %>% 
  mutate(idv=DIRECTORIO,
         idh=SECUENCIA_P)

TenFinanViv <- TenFinanViv %>% 
  mutate(idv=directorio,
         idh=secuencia_encuesta)


setnames(dt_datosVivi,'DIRECTORIO','idv')


## 3 Escoger Variables relevantes
salud2 <- salud1 %>% select(idv,idh, idi, FEX_C,P8551,P3176, P3178S1A1, P3178S2A1, P3178S3A1,P3179S1A1, P3179S2A1, P3179S3A1,
                            P3181S1, P3182S1, P3183S1, P3184S1, P3185S1, P3186S1, P3187S2,
                            P3188S1A1, P3188S2A1, P3188S3A1,P3189S1A1, P3189S2A1)


SVH2 <- SVH1 %>% 
  rowwise() %>% 
  mutate(GElectri = P5018/P5018S1,
         GGasNat = P3163/P3163S1,
         GAlcantari = P5034/P5034S1,
         GRecBasu = P5044/P5044S1,
         GAcued = P5067/P5067S1,
         GComCocinar = P8540,
         GservPubHogar = sum(c(GElectri,GGasNat,GAlcantari,GRecBasu,GAcued,GComCocinar),na.rm=T)) %>% 
  select(idv,idh,FEX_C,CANT_PERSONAS_HOGAR,GservPubHogar,I_HOGAR,I_UGASTO,PERCAPITA, I_OU)
  

# Reemplazo los valores 99 y 98 por NA
dt_GHItem <- mutate_all(dt_GHItem, ~ (replace(., .=='99'| .=='98', NA)))

dt_GHItem <- dt_GHItem %>% 
  select("idv","idh","SECUENCIA_ENCUESTA","ORDEN","FEX_C","P3204","P3204S1","P3204S2")

dt_GHItem <- as.data.table(dt_GHItem)

#mensualizo el gasto
dt_GHItem[,FactorMensual:=1]
#Gasto en los ultimos 7 dias
dt_GHItem[,FactorMensual:=ifelse(P3204==1,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==2,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==3,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==4,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==5,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==6,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==7,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==8,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==9,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==10,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==11,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==12,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==13,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==14,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==15,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==16,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==17,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==18,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==19,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==20,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==21,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==22,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==23,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==24,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==25,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==26,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==27,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==28,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==29,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==30,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==31,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==32,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==33,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==34,4.28,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==35,4.28,FactorMensual)]
#Gasto en los ultimos 3 meses
dt_GHItem[,FactorMensual:=ifelse(P3204==68,1/3,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==69,1/3,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==70,1/3,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==71,1/3,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==72,1/3,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==73,1/3,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==74,1/3,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==75,1/3,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==76,1/3,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==77,1/3,FactorMensual)]
#Gasto en los ultimos 12 meses
dt_GHItem[,FactorMensual:=ifelse(P3204==78,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==79,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==80,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==81,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==82,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==83,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==84,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==85,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==86,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==87,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==88,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==89,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==90,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==91,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==92,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==93,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==94,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==95,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==96,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==97,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==98,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==99,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==100,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==101,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==102,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==103,1/12,FactorMensual)]
dt_GHItem[,FactorMensual:=ifelse(P3204==104,1/12,FactorMensual)]

#Gasto mes
dt_GHItem[ ,GastoMes:=P3204S1*FactorMensual]



TenFinanViv0 <- TenFinanViv %>% 
  select('idv','idh','fex_c','p3198','p5610','p8693','p5140','p5650')

TenFinanViv0 <- mutate_all(TenFinanViv0, ~ (replace(., .=='99'| .=='98', NA)))

## 4 Ordenar filas por identificadores
salud3 <- salud2 %>% arrange(idv,idh,idi)
SVH <- SVH2 %>% arrange(idv,idh)

# Reemplazo los valores 99 y 98 por NA
salud <- mutate_all(salud3, ~ (replace(., .=='99'| .=='98', NA)))



## 5 Guardar datos base para calculos

# Directorio para guardar datos
setwd(wd_base)

# R
save(salud,file=paste0(wd_base,"salud.RData"))
save(SVH,file=paste0(wd_base,"SVH.RData"))
save(dt_GHItem,file=paste0(wd_base,"GHogar.RData"))
save(TenFinanViv0,file=paste0(wd_base,"TenFinanViv.RData"))
save(dt_datosVivi,file=paste0(wd_base,"dt_datosVivi.RData"))

# Excel
write.xlsx(salud,paste0(wd_base,"salud.xlsx"))
write.xlsx(SVH,paste0(wd_base,"SVH.xlsx"))
write.xlsx(dt_GHItem,paste0(wd_base,"GHogar.xlsx"))
write.xlsx(TenFinanViv0,paste0(wd_base,"TenFinanViv.xlsx"))
write.xlsx(dt_datosVivi,paste0(wd_base,"dt_datosVivi.xlsx"))

## 6 Limpiar
rm(list=ls(pattern="salud"))
rm(list=ls(pattern="SVH"))




