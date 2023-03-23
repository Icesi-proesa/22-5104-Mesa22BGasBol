 # Script para estimacion de GBS ----

## 1 Cargar datos
setwd(wd_base)
 
load("salud.RData")
salud0 <- as.data.frame(salud)

load("SVH.RData")
SVH0 <- as.data.frame(SVH)

load("GHogar.RData")
GHogar0 <- dt_GHItem
GHogar0 <- as.data.frame(GHogar0)

load("TenFinanViv.RData")
TenFinanViv0 <- as.data.frame(TenFinanViv0)

load('dt_datosVivi.RData')
dt_datosVivi <- as.data.frame(dt_datosVivi)

# Creo variables del gasto en salud

salud1 <- salud0 %>% rowwise() %>% mutate(g_eps = P8551,
                                          g_pvos = P3176,
                                          g_consulta = sum(c(P3178S1A1 , P3178S2A1 , P3178S3A1),na.rm=TRUE),
                                          g_odontologia = sum(c(P3179S1A1 , P3179S2A1 , P3179S3A1),na.rm=TRUE),
                                          g_vacunas = P3181S1,
                                          g_med = P3182S1,
                                          g_lab = P3183S1,
                                          g_rehab = P3184S1,
                                          g_terap = P3185S1,
                                          g_transp = P3186S1,
                                          g_lentes = P3187S2/12,
                                          g_cirugia = sum(c(P3188S1A1 , P3188S2A1 , P3188S3A1)/12,na.rm=TRUE),
                                          g_hospita = sum(c(P3189S1A1 , P3189S2A1)/12,na.rm=TRUE))



## ordeno columnas

salud2 <- salud1 %>% select(idv,idh, idi,FEX_C,g_eps,g_pvos,g_consulta,g_odontologia, g_vacunas,
                              g_med,g_lab,g_rehab,g_terap,g_transp,g_lentes,g_cirugia,g_hospita,
                              P8551,P3176, P3178S1A1, P3178S2A1, P3178S3A1,P3179S1A1, P3179S2A1, P3179S3A1,
                              P3181S1, P3182S1, P3183S1, P3184S1, P3185S1, P3186S1, P3187S2,
                              P3188S1A1, P3188S2A1, P3188S3A1,P3189S1A1, P3189S2A1)
                                 



salud3 <- as.data.frame(salud2)



#Agrupo por idv ,idh

salud4 <- salud3 %>% group_by(idv, idh) %>% summarise(FEX_C =min(FEX_C),
                                                      P8551 = sum(P8551,na.rm = TRUE),
                                                      g_eps = sum(g_eps,na.rm = TRUE),
                                                      P3176 = sum(P3176,na.rm = TRUE),
                                                      g_pvos = sum(g_pvos,na.rm = TRUE),
                                                      P3178S1A1 = sum(P3178S1A1,na.rm = TRUE),
                                                      P3178S2A1 = sum(P3178S2A1,na.rm = TRUE),
                                                      P3179S3A1 = sum(P3179S3A1,na.rm = TRUE),
                                                      g_consulta = sum(g_consulta,na.rm = TRUE),
                                                      P3179S1A1 = sum(P3179S1A1,na.rm = TRUE),
                                                      P3179S2A1 = sum(P3179S2A1,na.rm = TRUE),
                                                      P3179S3A1 = sum(P3179S3A1,na.rm = TRUE),
                                                      g_odontologia = sum(g_odontologia,na.rm = TRUE),
                                                      P3181S1 = sum(P3181S1,na.rm = TRUE),
                                                      g_vacunas = sum(g_vacunas,na.rm = TRUE),
                                                      P3182S1 = sum(P3182S1,na.rm = TRUE),
                                                      g_med = sum(g_med,na.rm = TRUE),
                                                      P3183S1 = sum(P3183S1,na.rm = TRUE),
                                                      g_lab = sum(g_lab,na.rm = TRUE),
                                                      P3185S1 = sum(P3185S1,na.rm = TRUE),
                                                      g_terap = sum(g_terap,na.rm =TRUE),
                                                      P3184S1 = sum(P3184S1,na.rm = TRUE),
                                                      g_rehab = sum(g_rehab,na.rm = TRUE),
                                                      P3186S1 = sum(P3186S1,na.rm = TRUE),
                                                      g_transp = sum(g_transp,na.rm = TRUE),
                                                      P3187S2 = sum(P3187S2,na.rm = TRUE)/12,
                                                      g_lentes = sum(g_lentes,na.rm = TRUE),
                                                      P3188S1A1 = sum(P3188S1A1,na.rm = TRUE)/12,
                                                      P3188S2A1 = sum(P3188S2A1,na.rm = TRUE)/12,
                                                      P3188S3A1 = sum(P3188S3A1,na.rm = TRUE)/12,
                                                      g_cirugia = sum(g_cirugia,na.rm = TRUE),
                                                      P3189S1A1 = sum(P3189S1A1,na.rm = TRUE)/12,
                                                      P3189S2A1 = sum(P3189S2A1,na.rm = TRUE)/12,
                                                      g_hospita = sum(g_hospita,na.rm=TRUE))
                                                        
                                                        
                                             
                                                             

salud5 <- as.data.frame(salud4)



#Gasto del hogar por item

#Selecciono items de interes(Articulos de higiene personal)
GHogarAseo <- GHogar0 %>% 
  filter( P3204 == 39 | P3204 == 40 | P3204 == 60 | P3204 == 61)

GHogarAseo1 <- GHogarAseo %>% 
  group_by(idv,idh) %>% 
  summarise(GArtAseo = sum(GastoMes,na.rm = T))
            
 

#Gasto total del hogar 

#Gasto total modulo Tenencia y financiacion de la vivienda que ocupa el hogar
GTenFinanViv <- TenFinanViv0 %>% 
  group_by(idv,idh) %>% 
  summarise(P3198 = sum(p3198, na.rm = T),
            P5610 = sum(p5610/12,na.rm = T),
            P8693 = sum(p8693/12,na.rm = T),
            P5140 = sum(p5140,na.rm = T),
            P5650 = sum(p5650,na.rm = T)) %>% 
  rowwise() %>% 
  mutate(GTHogarTenFinanViv = sum(P3198,P5610,P8693,P5140,P5650,na.rm = T)) %>% 
  select('idv','idh','GTHogarTenFinanViv')
            

#Gasto total modulo Gastos de los hogares (Gastos por Item)
GHogar <- GHogar0 %>% 
  group_by(idv,idh) %>% 
  summarise(GTHogarItem = sum(GastoMes,na.rm = T))
            

SVHF <- merge(SVH0,GTenFinanViv,by.x =c("idv", "idh") , by.y =c("idv","idh") , all=T)
SVHF <- merge(SVHF,GHogarAseo1,by.x =c("idv", "idh") , by.y =c("idv","idh") , all=T)
SVHF <- merge(SVHF,GHogar,by.x =c("idv", "idh") , by.y =c("idv","idh") , all=T)

SVHF <- SVHF %>% 
  rowwise() %>% 
  mutate(GTHogar = sum(c(GservPubHogar,GTHogarTenFinanViv,GTHogarItem,na.rm=T))) %>% 
  select('idv','idh','FEX_C','CANT_PERSONAS_HOGAR','GTHogar','GArtAseo','I_HOGAR','I_UGASTO','PERCAPITA','I_OU')

#### BD final para calculo####

### Merge
BdCalculos0 <- merge(SVHF, salud5, by.x =c("idv", "idh") , by.y =c("idv","idh") , all=TRUE)                                                                      




#### 1. Calculo por hogares ####

## Calculo el gasto de bolsillo mensual por hogar ####


## calculo gasto de bolsillo mensual
GBS21_Microdatos0 <- BdCalculos0 %>% rowwise() %>% mutate(GbolMenBasico = sum(c(g_consulta,g_odontologia,g_vacunas,g_lab,g_rehab,g_terap,g_lentes,g_cirugia,g_hospita),na.rm=TRUE),
                                                          GbolMenGeneral_tran = sum(c(GbolMenBasico,  g_transp),na.rm=TRUE),
                                                          GbolMenGeneral_med = sum(c(GbolMenBasico, g_med),na.rm=TRUE),
                                                          GbolMenAmpliado = sum(c(GbolMenBasico,g_transp,g_med) ,na.rm=TRUE),
                                                          GbolMenSaludPrivado = sum(c(GbolMenAmpliado,g_eps,g_pvos), na.rm = TRUE))
                                                      
                                                                    


GBS21_Microdatos1 <- as.data.frame(GBS21_Microdatos0)


## gasto de bolsillo como proporcion de los ingresos mensual####

GBS21_Microdatos2 <- GBS21_Microdatos1 %>% mutate (GbolMenBasico_PIng = (GbolMenBasico/I_HOGAR),
                                                   GbolMenGeneral_tran_PIng = (GbolMenGeneral_tran/I_HOGAR),
                                                   GbolMenGeneral_med_PIng = (GbolMenGeneral_med/I_HOGAR),
                                                   GbolMenAmpliado_PIng = (GbolMenAmpliado/I_HOGAR),
                                                   GbolMenSaludPrivado_PIng = (GbolMenSaludPrivado/I_HOGAR))

GBS21_Microdatos2 <- mutate_all(GBS21_Microdatos2, ~ (replace(., .=='Inf'| .=='-Inf' | .=='NaN' , NA)))
                                                   
#sapply(GBS21_Microdatos2, function(x) sum(is.na(x)))

## percapita, catastrofico y empobrecedor


GBS21_Microdatos2 <- GBS21_Microdatos2 %>% mutate(I_HOGAR2 = I_HOGAR)
GBS21_Microdatos2$I_HOGAR2[GBS21_Microdatos2$I_HOGAR2==0]=20000



GBS21_Microdatos3 <- GBS21_Microdatos2 %>% mutate(GbolMenBasico_PCap = (GbolMenBasico/CANT_PERSONAS_HOGAR),
                                                  GbolMenGeneral_tran_PCap = (GbolMenGeneral_tran/CANT_PERSONAS_HOGAR),
                                                  GbolMenGeneral_med_PCap = (GbolMenGeneral_med/CANT_PERSONAS_HOGAR),
                                                  GbolMenAmpliado_PCap = (GbolMenAmpliado/CANT_PERSONAS_HOGAR),
                                                  GbolMenSaludPrivado_PCap = (GbolMenSaludPrivado/CANT_PERSONAS_HOGAR),
                                                  
                                                  GbolMenBasico_Catast = (GbolMenBasico/I_HOGAR2),
                                                  GbolMenGeneral_tran_Catast = (GbolMenGeneral_tran/I_HOGAR2),
                                                  GbolMenGeneral_med_Catast = (GbolMenGeneral_med/I_HOGAR2),
                                                  GbolMenAmpliado_Catast = (GbolMenAmpliado/I_HOGAR2),
                                                  
                                                  GbolMenBasico_Empob = ( I_HOGAR - GbolMenBasico),
                                                  GbolMenGeneral_tran_Empob = (I_HOGAR - GbolMenGeneral_tran),
                                                  GbolMenGeneral_med_Empob = (I_HOGAR - GbolMenGeneral_med),
                                                  GbolMenAmpliado_Empob = (I_HOGAR - GbolMenAmpliado))
                                                  
                                                 
                                                  
                                                
                                                 
## Selecicono variables para exportar base microdatos
GBS21_Microdatos4 <- GBS21_Microdatos3 %>% select(idv,idh,CANT_PERSONAS_HOGAR, FEX_C.y,I_HOGAR,I_HOGAR2,GTHogar,P8551,g_eps,P3176,g_pvos,
                                                  P3178S1A1,P3178S2A1,P3179S3A1,g_consulta,
                                                  P3179S1A1,P3179S2A1,P3179S3A1,g_odontologia,
                                                  P3181S1,g_vacunas,P3182S1,g_med,P3183S1,g_lab,P3184S1,g_rehab,P3185S1,g_terap,GArtAseo,
                                                  P3186S1,g_transp,P3187S2,g_lentes,
                                                  P3188S1A1,P3188S2A1,P3188S3A1,g_cirugia,
                                                  P3189S1A1,P3189S2A1,g_hospita,
                                                  GbolMenBasico,GbolMenGeneral_tran,GbolMenGeneral_med,GbolMenAmpliado,GbolMenSaludPrivado,
                                                  GbolMenBasico_PIng,GbolMenGeneral_tran_PIng,GbolMenGeneral_med_PIng,GbolMenAmpliado_PIng,GbolMenSaludPrivado_PIng,
                                                  GbolMenBasico_PCap,GbolMenGeneral_tran_PCap,GbolMenGeneral_med_PCap,GbolMenAmpliado_PCap,GbolMenSaludPrivado_PCap,
                                                  GbolMenBasico_Catast,GbolMenGeneral_tran_Catast,GbolMenGeneral_med_Catast,GbolMenAmpliado_Catast,
                                                  GbolMenBasico_Empob, GbolMenGeneral_tran_Empob, GbolMenGeneral_med_Empob,GbolMenAmpliado_Empob)


GBS21_Microdatos4 <- merge(GBS21_Microdatos4,dt_datosVivi[,c('idv','P1_DEPARTAMENTO','REGION','CLASE')], by='idv', all.x = T)



## calsif hogares Quintiles por ingreso

QI_HOGAR <- as.data.frame(wtd.quantile(GBS21_Microdatos4$I_HOGAR ,weight=GBS21_Microdatos4$FEX_C.y ,prob=seq(0, 1, 1/5)))



GBS21_Microdatos5 <- GBS21_Microdatos4 %>% mutate(QuinIngClasif = ifelse(I_HOGAR <= 600000,"Q1",
                                                                      ifelse(I_HOGAR> 600000 & I_HOGAR <= 1053333,"Q2",
                                                                             ifelse(I_HOGAR > 1053333 & I_HOGAR<= 1653750,"Q3",
                                                                                    ifelse(I_HOGAR > 1653750 & I_HOGAR <=2991667,"Q4",
                                                                                           ifelse(I_HOGAR > 2991667 & I_HOGAR <= 335915000,"Q5",""))))))

GBS21_Microdatos6 <- GBS21_Microdatos5 %>% select(-(I_HOGAR2))


GBS21_Microdatos7 <- GBS21_Microdatos6 %>% mutate(MPosGbolMenBasico = ifelse(GbolMenBasico>0,1,0),
                                                  MPosGbolMenGeneral_tran = ifelse(GbolMenGeneral_tran>0,1,0),
                                                  MPosGbolMenGeneral_med = ifelse(GbolMenGeneral_med>0,1,0),
                                                  MPosGbolMenAmpliado = ifelse(GbolMenAmpliado>0,1,0),
                                                  MPosGbolMenSaludPrivado = ifelse(GbolMenSaludPrivado>0,1,0))





#Multiplico por el factor de expansion cada variable

GBS21_Estimadores0 <- GBS21_Microdatos4 %>% mutate(CANT_PERSONAS_HOGAR = CANT_PERSONAS_HOGAR*FEX_C.y,
                                                   I_HOGAR = I_HOGAR*FEX_C.y,
                                                   g_eps = g_eps*FEX_C.y,
                                                   g_pvos = g_pvos*FEX_C.y,
                                                   g_consulta = g_consulta*FEX_C.y,
                                                   g_odontologia = g_odontologia*FEX_C.y,
                                                   g_vacunas = g_vacunas*FEX_C.y,
                                                   g_med = g_med*FEX_C.y,
                                                   g_lab = g_lab*FEX_C.y,
                                                   GArtAseo = GArtAseo*FEX_C.y,
                                                   g_rehab = g_rehab*FEX_C.y,
                                                   g_terap = g_terap*FEX_C.y,
                                                   g_transp = g_transp*FEX_C.y,
                                                   g_lentes = g_lentes*FEX_C.y,
                                                   g_cirugia = g_cirugia*FEX_C.y,
                                                   g_hospita = g_hospita*FEX_C.y,
                                                   G_HOGAR = GTHogar*FEX_C.y)


                        

## suma
GBS21_Estimadores0 <- GBS21_Estimadores0 %>% mutate(I_HOGAR2 = I_HOGAR)
GBS21_Estimadores0$I_HOGAR2[GBS21_Estimadores0$I_HOGAR2==0]=20000
GBS21_Estimadores1 <- colSums(GBS21_Estimadores0,na.rm =TRUE)
GBS21_Estimadores1 <- as.data.frame(GBS21_Estimadores1)

## Transpongo la base

GBS21_Estimadores2 <- transpose(GBS21_Estimadores1)
colnames(GBS21_Estimadores2) <- rownames(GBS21_Estimadores1)



#### 1 .Calculo agregado####

#### Calculo el gasto de bolsillo mensual####

GBS21_Estimadores3 <- GBS21_Estimadores2 %>% mutate(GbolMenBasico = sum(g_consulta,g_odontologia,g_vacunas,g_lab,g_rehab,g_terap,g_lentes,g_cirugia,g_hospita,na.rm = T),
                                                    GbolMenGeneral_tran = sum(GbolMenBasico , g_transp,na.rm = T),
                                                    GbolMenGeneral_med = sum(GbolMenBasico , g_med ,na.rm = T),
                                                    GbolMenAmpliado = sum(GbolMenBasico , g_transp , g_med,na.rm = T),
                                                    GbolMenSaludPrivado = sum(GbolMenAmpliado , g_eps , g_pvos,na.rm = T))



## Divido por 1000000 
GBS21_Estimadores4 <- GBS21_Estimadores3 %>% mutate(I_HOGAR = I_HOGAR/1000000,
                                                    G_HOGAR = G_HOGAR/1000000,
                                                    I_HOGAR2 = I_HOGAR2/1000000,
                                                    GbolMenBasico = GbolMenBasico/1000000,
                                                    GbolMenGeneral_tran = GbolMenGeneral_tran/1000000,
                                                    GbolMenGeneral_med = GbolMenGeneral_med/1000000,
                                                    GbolMenAmpliado = GbolMenAmpliado/1000000,
                                                    GbolMenSaludPrivado = GbolMenSaludPrivado/1000000)
                                                 

                                
                                


#### GB como proporcion del ingreso del hogar, percapita y catastrofico ####

GBS21_Estimadores5 <- GBS21_Estimadores4 %>% mutate (GbolMenBasico_PIng = (GbolMenBasico/I_HOGAR),
                                                     GbolMenGeneral_tran_PIng = (GbolMenGeneral_tran/I_HOGAR),
                                                     GbolMenGeneral_med_PIng = (GbolMenGeneral_med/I_HOGAR),
                                                     GbolMenAmpliado_PIng = (GbolMenAmpliado/I_HOGAR),
                                                     GbolMenSaludPrivado_PIng = (GbolMenSaludPrivado/I_HOGAR),
                                                     
                                                     
                                                     GbolMenBasico_PCap = ((GbolMenBasico/CANT_PERSONAS_HOGAR)*1000000),
                                                     GbolMenGeneral_tran_PCap = ((GbolMenGeneral_tran/CANT_PERSONAS_HOGAR)*1000000),
                                                     GbolMenGeneral_med_PCap = ((GbolMenGeneral_med/CANT_PERSONAS_HOGAR)*1000000),
                                                     GbolMenAmpliado_PCap = ((GbolMenAmpliado/CANT_PERSONAS_HOGAR)*1000000),
                                                     GbolMenSaludPrivado_PCap = ((GbolMenSaludPrivado/CANT_PERSONAS_HOGAR)*1000000),
                                                     
                                                     GbolMenBasico_Catast = (GbolMenBasico/I_HOGAR2),
                                                     GbolMenGeneral_tran_Catast = (GbolMenGeneral_tran/I_HOGAR2),
                                                     GbolMenGeneral_med_Catast = (GbolMenGeneral_med/I_HOGAR2),
                                                     GbolMenAmpliado_Catast = (GbolMenAmpliado/I_HOGAR2),
                                                     GbolMenSaludPrivado_Catast = (GbolMenSaludPrivado/I_HOGAR2),
                                                     
                                                     GbolMenBasico_Empob = (I_HOGAR - GbolMenBasico),
                                                     GbolMenGeneral_tran_Empob = (I_HOGAR - GbolMenGeneral_tran),
                                                     GbolMenGeneral_med_Empob = (I_HOGAR - GbolMenGeneral_med),
                                                     GbolMenAmpliado_Empob = (I_HOGAR - GbolMenAmpliado),
                                                     GbolMenSaludPrivado_Empob = (I_HOGAR - GbolMenSaludPrivado))
                                                     



PIB <- c(1176694000)



GBS21_Estimadores6 <- cbind(GBS21_Estimadores5, PIB)


## gasto de bolsillo como proporción del pib

GBS21_Estimadores7 <- GBS21_Estimadores6 %>% mutate (GbolBasico_PPib = ((GbolMenBasico*12)/PIB),
                                                     GbolGeneral_tran_PPib = ((GbolMenGeneral_tran*12)/PIB),
                                                     GbolGeneral_med_PPib = ((GbolMenGeneral_med*12)/PIB),
                                                     GbolAmpliado_PPib = ((GbolMenAmpliado*12)/PIB),
                                                     GbolSaludPrivado_PPib = ((GbolMenSaludPrivado*12)/PIB))

### Creo variables para calcular gasto catastrofico y empobrecedor agregado

BDEmpoCatas0 <- GBS21_Microdatos4 %>% mutate(HogCatsBasico = ifelse(GbolMenBasico_Catast > 0.4,FEX_C.y,0),
                                             HogCatsGen_tran = ifelse(GbolMenGeneral_tran_Catast > 0.4,FEX_C.y,0),
                                             HogCatsGen_Med = ifelse(GbolMenGeneral_med_Catast > 0.4,FEX_C.y,0),
                                             HogCatsGen_Ampl = ifelse(GbolMenAmpliado_Catast > 0.4,FEX_C.y,0),
                                             
                                             LineaPobHog = CANT_PERSONAS_HOGAR*354031,
                                             
                                             EraPobAnteGBS = ifelse(I_HOGAR < LineaPobHog,1,0),
                                             Basic_LuegGBSPobre = ifelse(GbolMenBasico_Empob < LineaPobHog,1,0),
                                             GenTran_LuegGBSPobre = ifelse(GbolMenGeneral_tran_Empob < LineaPobHog,1,0),
                                             GenMed_LuegGBSPobre = ifelse(GbolMenGeneral_med_Empob < LineaPobHog,1,0),
                                             Ampl_LuegGBSPobre = ifelse(GbolMenAmpliado_Empob < LineaPobHog,1,0),
                                             
                                             HogEmpobBasico = ifelse(EraPobAnteGBS == 0 & Basic_LuegGBSPobre == 1,FEX_C.y,0),
                                             HogEmpobGen_tran = ifelse(EraPobAnteGBS == 0 & GenTran_LuegGBSPobre == 1,FEX_C.y,0),
                                             HogEmpobGen_med = ifelse(EraPobAnteGBS == 0 & GenMed_LuegGBSPobre == 1,FEX_C.y,0),
                                             HogEmpobAmpli = ifelse(EraPobAnteGBS == 0 & Ampl_LuegGBSPobre == 1,FEX_C.y,0))


BDEmpoCatas1 <- BDEmpoCatas0 %>% select(HogCatsBasico ,HogCatsGen_tran, HogCatsGen_Med,HogCatsGen_Ampl,
                                        HogEmpobBasico, HogEmpobGen_tran, HogEmpobGen_med, HogEmpobAmpli)

#Sumo
HogEmpCats0 <- colSums(BDEmpoCatas1,na.rm =TRUE)
HogEmpCats0 <- as.data.frame(HogEmpCats0)
#transpongo la base
HogEmpCats1 <- transpose(HogEmpCats0)
colnames(HogEmpCats1) <- rownames(HogEmpCats0)



GBS21_Estimadores8 <- cbind(GBS21_Estimadores7,HogEmpCats1)


GBS21_Estimadores9 <- GBS21_Estimadores8 %>% mutate(HogGCatsBasico_PTHog = HogCatsBasico/FEX_C.y,
                                                    HogGCatsGen_tran_PTHog = HogCatsGen_tran/FEX_C.y,
                                                    HogGCatsGen_Med_PTHog = HogCatsGen_Med/FEX_C.y,
                                                    HogGCatsGen_Ampl_PTHog = HogCatsGen_Ampl/FEX_C.y,
                                                    
                                                    HogGEmpobBasico_PTHog = HogEmpobBasico/FEX_C.y,
                                                    HogGEmpobGen_tran_PTHog = HogEmpobGen_tran/FEX_C.y,
                                                    HogGEmpobGen_med_PTHog = HogEmpobGen_med/FEX_C.y,
                                                    HogGEmpobAmpli_PTHog = HogEmpobAmpli/FEX_C.y)
                                                    
                                                   

GBS21_Estimadores10 <- GBS21_Estimadores9 %>% select(I_HOGAR,G_HOGAR,PIB,CANT_PERSONAS_HOGAR,FEX_C.y,
                                                    GbolMenBasico,GbolMenGeneral_tran,GbolMenGeneral_med,GbolMenAmpliado,GbolMenSaludPrivado,
                                                    GbolMenBasico_PIng ,GbolMenGeneral_tran_PIng ,GbolMenGeneral_med_PIng ,GbolMenAmpliado_PIng,GbolMenSaludPrivado_PIng,
                                                    GbolMenBasico_PCap,GbolMenGeneral_tran_PCap,GbolMenGeneral_med_PCap,GbolMenAmpliado_PCap,GbolMenSaludPrivado_PCap,
                                                    GbolBasico_PPib ,GbolGeneral_tran_PPib ,GbolGeneral_med_PPib ,GbolAmpliado_PPib ,GbolSaludPrivado_PPib,
                                                    HogCatsBasico, HogCatsGen_tran, HogCatsGen_Med, HogCatsGen_Ampl,
                                                    HogGCatsBasico_PTHog, HogGCatsGen_tran_PTHog, HogGCatsGen_Med_PTHog, HogGCatsGen_Ampl_PTHog,
                                                    HogEmpobBasico, HogEmpobGen_tran, HogEmpobGen_med, HogEmpobAmpli,
                                                    HogGEmpobBasico_PTHog, HogGEmpobGen_tran_PTHog, HogGEmpobGen_med_PTHog, HogGEmpobAmpli_PTHog)
                                                    
                                                    
                                                    
## Calculo proporcion de hogares con gasto positivo

#Basico
GBS21_HogPosBasico <- GBS21_Microdatos7 %>% filter(MPosGbolMenBasico == 1) %>%
  select(FEX_C.y,GbolMenBasico,CANT_PERSONAS_HOGAR) %>% 
  mutate(Personas = CANT_PERSONAS_HOGAR*FEX_C.y)


#Sumo
GBS21_HogPosBasico1 <- colSums(GBS21_HogPosBasico,na.rm =TRUE)
GBS21_HogPosBasico1 <- as.data.frame(GBS21_HogPosBasico1)
#transpongo la base
GBS21_HogPosBasico2 <- transpose(GBS21_HogPosBasico1)
colnames(GBS21_HogPosBasico2) <- rownames(GBS21_HogPosBasico1)

GBS21_HogPosBasico2 <- GBS21_HogPosBasico2 %>% select(FEX_C.y,Personas) %>% rename(GbolMenBasico = FEX_C.y,PersonasGbolMenBasico=Personas) 


#GenTran
GBS21_HogPosGen_tra <- GBS21_Microdatos7 %>% filter(MPosGbolMenGeneral_tran == 1) %>%
  select(FEX_C.y,GbolMenGeneral_tran,CANT_PERSONAS_HOGAR) %>% 
  mutate(Personas = CANT_PERSONAS_HOGAR*FEX_C.y)



#Sumo
GBS21_HogPosGen_tra1 <- colSums(GBS21_HogPosGen_tra,na.rm =TRUE)
GBS21_HogPosGen_tra1 <- as.data.frame(GBS21_HogPosGen_tra1)
#transpongo la base
GBS21_HogPosGen_tra2 <- transpose(GBS21_HogPosGen_tra1)
colnames(GBS21_HogPosGen_tra2) <- rownames(GBS21_HogPosGen_tra1)

GBS21_HogPosGen_tra2 <- GBS21_HogPosGen_tra2 %>%  select(FEX_C.y,Personas) %>% rename(GbolMenGeneral_tran = FEX_C.y,PersonasGbolMenGeneral_tran=Personas) 


#GenMed
GBS21_HogPosGen_med <- GBS21_Microdatos7 %>% filter(MPosGbolMenGeneral_med == 1) %>%
  select(FEX_C.y,GbolMenGeneral_med,CANT_PERSONAS_HOGAR) %>% 
  mutate(Personas = CANT_PERSONAS_HOGAR*FEX_C.y)



#Sumo
GBS21_HogPosGen_med1 <- colSums(GBS21_HogPosGen_med,na.rm =TRUE)
GBS21_HogPosGen_med1 <- as.data.frame(GBS21_HogPosGen_med1)
#transpongo la base
GBS21_HogPosGen_med2 <- transpose(GBS21_HogPosGen_med1)
colnames(GBS21_HogPosGen_med2) <- rownames(GBS21_HogPosGen_med1)

GBS21_HogPosGen_med2 <- GBS21_HogPosGen_med2 %>% select(FEX_C.y,Personas) %>% rename(GbolMenGeneral_med = FEX_C.y,PersonasGbolMenGeneral_med=Personas) 

##Ampliado
GBS21_HogPosAmpl <- GBS21_Microdatos7 %>% filter(MPosGbolMenAmpliado == 1) %>%
  select(FEX_C.y,GbolMenAmpliado,CANT_PERSONAS_HOGAR) %>% 
  mutate(Personas = CANT_PERSONAS_HOGAR*FEX_C.y)



#Sumo
GBS21_HogPosAmpl1 <- colSums(GBS21_HogPosAmpl,na.rm =TRUE)
GBS21_HogPosAmpl1 <- as.data.frame(GBS21_HogPosAmpl1)
#transpongo la base
GBS21_HogPosAmpl2 <- transpose(GBS21_HogPosAmpl1)
colnames(GBS21_HogPosAmpl2) <- rownames(GBS21_HogPosAmpl1)

GBS21_HogPosAmpl2 <- GBS21_HogPosAmpl2 %>% select(FEX_C.y,Personas) %>% rename(GbolMenAmpliado = FEX_C.y,PersonasGbolMenAmpliado=Personas)


##Pivado
GBS21_HogPosPriv <- GBS21_Microdatos7 %>% filter(MPosGbolMenSaludPrivado == 1) %>%
  select(FEX_C.y,GbolMenSaludPrivado,CANT_PERSONAS_HOGAR) %>% 
  mutate(Personas = CANT_PERSONAS_HOGAR*FEX_C.y)



#Sumo
GBS21_HogPosPriv1 <- colSums(GBS21_HogPosPriv,na.rm =TRUE)
GBS21_HogPosPriv1 <- as.data.frame(GBS21_HogPosPriv1)
#transpongo la base
GBS21_HogPosPriv2 <- transpose(GBS21_HogPosPriv1)
colnames(GBS21_HogPosPriv2) <- rownames(GBS21_HogPosPriv1)

GBS21_HogPosPriv2 <- GBS21_HogPosPriv2 %>% select(FEX_C.y,Personas) %>% rename(GbolMenSaludPrivado = FEX_C.y,PersonasGbolMenSaludPrivado=Personas)


##min,max,media,mediana,sd

GBS21_HogPosBasico$categoria <- "a.GbolMenBasico"
GBS21_HogPosBasico3 <- GBS21_HogPosBasico %>% rename(Gbol = GbolMenBasico)
GBS21_HogPosGen_tra$categoria <- "b.GbolMenGeneral_tran"
GBS21_HogPosGen_tra3 <- GBS21_HogPosGen_tra %>% rename(Gbol = GbolMenGeneral_tran)
GBS21_HogPosGen_med$categoria <- "c.GbolMenGeneral_med"
GBS21_HogPosGen_med3 <- GBS21_HogPosGen_med %>% rename(Gbol = GbolMenGeneral_med)
GBS21_HogPosAmpl$categoria <- "d.GbolMenAmpliado"
GBS21_HogPosAmpl3 <- GBS21_HogPosAmpl %>% rename(Gbol = GbolMenAmpliado)
GBS21_HogPosPriv$categoria <- "e.GbolMenPrivado"
GBS21_HogPosPriv3 <- GBS21_HogPosPriv %>% rename(Gbol = GbolMenSaludPrivado)

GBS21_HogPosTotal <- rbind(GBS21_HogPosBasico3,GBS21_HogPosGen_tra3,GBS21_HogPosGen_med3,GBS21_HogPosAmpl3,GBS21_HogPosPriv3)

Estadisticas <- GBS21_HogPosTotal %>% group_by(categoria) %>% summarise(Minimo = min(Gbol, weight = FEX_C.y ),
                                                                        Mediana = median(Gbol, weight = FEX_C.y),
                                                                        Media = mean(Gbol, weight = FEX_C.y),
                                                                        Maximo = max(Gbol, weight = FEX_C.y),
                                                                        Desviacion_estandar = sd(Gbol))



##Hogares y proporcion de gasto positivo
GBS21_Positivo <- cbind(GBS21_HogPosBasico2,GBS21_HogPosGen_tra2,GBS21_HogPosGen_med2,GBS21_HogPosAmpl2,GBS21_HogPosPriv2)
THogares <- sum(GBS21_Microdatos7$FEX_C.y)
TPersonas <- sum(GBS21_Microdatos7$CANT_PERSONAS_HOGAR)

GBS21_Positivo2 <- transpose(GBS21_Positivo)
rownames(GBS21_Positivo2) <- colnames(GBS21_Positivo)

GBS21_Positivo2 <- GBS21_Positivo2 %>% rename(Hogares = V1) %>% 
  mutate(PropHogares  = (Hogares/THogares)*100)



GBS21_Positivo3 <- cbind(GBS21_Positivo2,Estadisticas) %>% select(Hogares,PropHogares,Mediana,Media,Desviacion_estandar,Minimo,Maximo)



## Hogares y proporcion del gasto todos 

GBS21_HogBasico <- GBS21_Microdatos6 %>% select(FEX_C.y,GbolMenBasico)

GBS21_HogBasico$categoria <- "GbolMenBasico"
GBS21_HogBasico1 <- GBS21_HogBasico %>% rename(Gbol = GbolMenBasico)



GBS21_HogGen_Tran <- GBS21_Microdatos6 %>% select(FEX_C.y, GbolMenGeneral_tran)

GBS21_HogGen_Tran$categoria <- "GbolMenGeneral_tran"
GBS21_HogGen_Tran1 <- GBS21_HogGen_Tran %>% rename(Gbol = GbolMenGeneral_tran)


GBS21_HogGen_Med <- GBS21_Microdatos6 %>% select(FEX_C.y, GbolMenGeneral_med)

GBS21_HogGen_Med$categoria <- "GbolMenGeneral_med"
GBS21_HogGen_Med1 <- GBS21_HogGen_Med %>% rename(Gbol = GbolMenGeneral_med)


GBS21_HogAmpliado <- GBS21_Microdatos6 %>% select(FEX_C.y, GbolMenAmpliado)

GBS21_HogAmpliado$categoria <- "GbolMenAmpliado"
GBS21_HogAmpliado1 <- GBS21_HogAmpliado %>% rename(Gbol = GbolMenAmpliado)


GBS21_HogPrivado <- GBS21_Microdatos6 %>% select(FEX_C.y, GbolMenSaludPrivado)

GBS21_HogPrivado$categoria <- "GbolMenPrivado"
GBS21_HogPrivado1 <- GBS21_HogPrivado %>% rename(Gbol = GbolMenSaludPrivado)




GBS21_HogTodos <- rbind(GBS21_HogBasico1,GBS21_HogGen_Tran1,GBS21_HogGen_Med1,GBS21_HogAmpliado1,GBS21_HogPrivado1)


EstadisticasTodos <- GBS21_HogTodos %>% group_by(categoria) %>% summarise(Hogares = sum(FEX_C.y),
                                                                          Minimo = min(Gbol, weight = FEX_C.y),
                                                                          Mediana = median(Gbol,weight = FEX_C.y),
                                                                          Media = mean(Gbol,weight = FEX_C.y),
                                                                          Maximo = max(Gbol,weight = FEX_C.y),
                                                                          Desviacion_estandar = sd(Gbol))
                                                                          
  
                                                                        


EstadisticasTodos <- EstadisticasTodos %>% select(categoria,Hogares,Mediana,Media,Desviacion_estandar,Minimo,Maximo)








## Calculo de gasto de bolsillo Urbano-Rural

dt_EstUrbRur <- GBS21_Estimadores0 %>% 
  group_by(CLASE) %>% 
  summarise(Hogares = sum(FEX_C.y,na.rm = T),
            Personas = sum(CANT_PERSONAS_HOGAR,na.rm = T),
            I_HOGAR = sum(I_HOGAR, na.rm = T),
            GBS_B = sum(g_consulta,g_odontologia,g_vacunas,g_lab,g_rehab,g_terap,g_lentes,g_cirugia,g_hospita,na.rm = T),
            GBS_T = sum(GBS_B,g_transp,na.rm = T),
            GBS_M = sum(GBS_B,g_med,na.rm = T),
            GBS_A = sum(GBS_B,g_med,g_transp,na.rm = T),
            GS = sum(GBS_A, g_eps, g_pvos,na.rm = T))

dt_EstUrbRur <- dt_EstUrbRur %>% mutate( I_HOGAR = (I_HOGAR*12)/1000000,
                                         GBS_B = (GBS_B*12)/1000000,
                                         GBS_T = (GBS_T*12)/1000000,
                                         GBS_M = (GBS_M*12)/1000000,
                                         GBS_A = (GBS_A*12)/1000000,
                                         GS = (GS*12)/1000000,
                                         'GBS_B/IH' = GBS_B/I_HOGAR,
                                         'GBS_T/IH' = GBS_T/I_HOGAR,
                                         'GBS_M/IH' = GBS_M/I_HOGAR,
                                         'GBS_A/IH' = GBS_A/I_HOGAR,
                                         'GS/IH' = GS/I_HOGAR,
                                         'GBS_B/GS' = GBS_B/GS,
                                         'GBS_T/GS' = GBS_T/GS,
                                         'GBS_M/GS' = GBS_M/GS,
                                         'GBS_A/GS' = GBS_A/GS)
                      

dt_EstUrbRur$CLASE[dt_EstUrbRur$CLASE == 1] <- 'Urbano'   
dt_EstUrbRur$CLASE[dt_EstUrbRur$CLASE == 2] <- 'Rural' 


# ## Gasto en bebidas alcoholicas, azucaradas,leche y sus derivados 
# 
# #Selecciono items de interes
# dt_GHogar <- dt_GHItem 
# 
# VMilk <- c(09,10)
# VSweet <- c(25)
# VAlcoh <- c(27)
# 
# 
# # se crean variables de acuerdo a cada bebida
# dt_GHogar[P3204 %in% VMilk ,Milk:= (GastoMes)]
# dt_GHogar[P3204 %in% VSweet ,Sweet:= (GastoMes)]
# dt_GHogar[P3204 %in% VAlcoh ,Alcoh:= (GastoMes)]
# 
# #Se agrupa por idv ,idh
# dt_GHogar1 <- dt_GHogar %>% 
#   group_by(idv, idh) %>% 
#   summarise(FEX_C =min(FEX_C),
#             Milk = sum(Milk,na.rm = TRUE),
#             Sweet = sum(Sweet,na.rm = TRUE),
#             Alcoh = sum(Alcoh,na.rm = TRUE))
# 
# 
# dt_GHogar2 <- merge(dt_GHogar1,SVH0[,c('idv','idh','FEX_C','CANT_PERSONAS_HOGAR','I_HOGAR')],by.x =c("idv", "idh") , by.y =c("idv","idh"), all=T)
# 
# dt_GHogar2$FEX_C.x <- NULL
# 
# setnames(dt_GHogar2,'FEX_C.y','FEX_C')
# 
# 
# 
# ## calsif hogares Quintiles por ingreso
# 
# QI_HOGAR <- as.data.frame(wtd.quantile(dt_GHogar2$I_HOGAR ,weight=dt_GHogar2$FEX_C ,prob=seq(0, 1, 1/5)))
# 
# 
# 
# dt_GHogar3 <- dt_GHogar2 %>% 
#   mutate(QuinIngClasif = ifelse(I_HOGAR <= 	600000,"Q1",
#                                 ifelse(I_HOGAR> 	600000 & I_HOGAR <= 	1053333,"Q2",
#                                        ifelse(I_HOGAR > 	1053333 & I_HOGAR<= 1653667,"Q3",
#                                               ifelse(I_HOGAR > 1653667 & I_HOGAR <=2991667,"Q4",
#                                                      ifelse(I_HOGAR > 2991667 & I_HOGAR <= 335915000,"Q5",""))))))
# 
# 
# ## Multiplico por el factor de expansion
# 
# dt_GHogar4 <- dt_GHogar3 %>% 
#   mutate(Milk = Milk*FEX_C,
#          Sweet = Sweet*FEX_C,
#          Alcoh = Alcoh*FEX_C,
#          I_HOGAR = I_HOGAR*FEX_C,
#          CANT_PERSONAS_HOGAR = CANT_PERSONAS_HOGAR*FEX_C)
#                                                                          
# ## se clasifica hogares con gasto positivo en cada uno de las bebidas
# dt_GHogar5 <- dt_GHogar4 %>% 
#   mutate(GPos_Milk = ifelse(Milk > 0,1,0),
#          GPos_Sweet = ifelse(Sweet > 0,1,0),
#          GPos_Alcoh = ifelse(Alcoh > 0,1,0))
# 
# 
# write.xlsx(dt_GHogar5,paste0(wd_resul,'dt_GHogar.xlsx'))
# 
# ##Milk
# dt_milk <- dt_GHogar5 %>% 
#   filter(GPos_Milk == 1) %>% 
#   select("Milk", "FEX_C","CANT_PERSONAS_HOGAR","I_HOGAR")
# 
# #Sumo
# dt_milk1 <- colSums(dt_milk,na.rm =TRUE)
# dt_milk1 <- as.data.frame(dt_milk1)
# #transpongo la base
# dt_milk2 <- transpose(dt_milk1)
# colnames(dt_milk2) <- rownames(dt_milk1)
# 
# ## Divido por 1000000 
# dt_milk2 <- dt_milk2 %>% 
#   mutate(Milk = Milk/1000000,
#          I_HOGAR = I_HOGAR/1000000,
#          Item = 'Milk') 
# 
# setnames(dt_milk2 ,'Milk','Gasto')
# 
# ##Sweet
# dt_Sweet <- dt_GHogar5 %>% 
#   filter(GPos_Sweet == 1) %>% 
#   select("Sweet", "FEX_C","CANT_PERSONAS_HOGAR","I_HOGAR")
# 
# #Sumo
# dt_Sweet1 <- colSums(dt_Sweet,na.rm =TRUE)
# dt_Sweet1 <- as.data.frame(dt_Sweet1)
# #transpongo la base
# dt_Sweet2 <- transpose(dt_Sweet1)
# colnames(dt_Sweet2) <- rownames(dt_Sweet1)
# 
# ## Divido por 1000000 
# dt_Sweet2 <- dt_Sweet2 %>% 
#   mutate(Sweet = Sweet/1000000,
#          I_HOGAR = I_HOGAR/1000000,
#          Item = 'Sweet') 
# 
# setnames(dt_Sweet2 ,'Sweet','Gasto')
# 
# ##Alcoh
# dt_Alcoh <- dt_GHogar5 %>% 
#   filter(GPos_Alcoh == 1) %>% 
#   select("Alcoh", "FEX_C","CANT_PERSONAS_HOGAR","I_HOGAR")
# 
# #Sumo
# dt_Alcoh1 <- colSums(dt_Alcoh,na.rm =TRUE)
# dt_Alcoh1 <- as.data.frame(dt_Alcoh1)
# #transpongo la base
# dt_Alcoh2 <- transpose(dt_Alcoh1)
# colnames(dt_Alcoh2) <- rownames(dt_Alcoh1)
# 
# ## Divido por 1000000 (cifras en miles de millones)
# dt_Alcoh2 <- dt_Alcoh2 %>% 
#   mutate(Alcoh = Alcoh/1000000,
#          I_HOGAR = I_HOGAR/1000000,
#          Item = 'Alcoh')
# 
# setnames(dt_Alcoh2,'Alcoh','Gasto')
# 
# 
# ## Uno tabla gasto
# 
# dt_Gasto <- rbind(dt_milk2,dt_Sweet2,dt_Alcoh2)
# 
# write.xlsx(dt_Gasto,paste0(wd_resul,'dt_Gasto.xlsx'))  
# 
# 
# ##Graficos
# 
# #Quintil de ingreso
# 
# dt_QI <- dt_GHogar3 
# 
# 
# 
# ## Marco BD por debajo del 5% y por encima del 95%  para el gasto  y  como proporcion del ingreso                                             
# dt_QI1 <- dt_QI %>% mutate(marQ_Milk = ifelse(Milk<=wtd.quantile(Milk,weight = FEX_C, prob = 0.05) | Milk>=wtd.quantile(Milk,weight = FEX_C, prob = 0.95),1,0),
#                            marQ_Sweet = ifelse(Sweet<=wtd.quantile(Sweet,weight = FEX_C, prob = 0.05) | Sweet>=wtd.quantile(Sweet,weight = FEX_C, prob = 0.95),1,0),
#                            marQ_Alcoh = ifelse(Alcoh<=wtd.quantile(Alcoh,weight = FEX_C, prob = 0.05) | Alcoh>=wtd.quantile(Alcoh,weight = FEX_C, prob = 0.95),1,0)
#                           )
# 
# dt_QIMilk <- dt_QI1 %>% 
#   filter(marQ_Milk == 0) %>%
#   select(Milk,QuinIngClasif,FEX_C,I_HOGAR)
# 
# dt_QISweet <- dt_QI1 %>% 
#   filter(marQ_Sweet == 0) %>%
#   select(Sweet,QuinIngClasif,FEX_C,I_HOGAR)
# 
# dt_QIAlcoh <- dt_QI1 %>% 
#   filter(marQ_Alcoh == 0) %>%
#   select(Alcoh,QuinIngClasif,FEX_C,I_HOGAR)
#   
#   
#   
#   
# 
# 
# ##Milk
# 
# Boxplot_Milk <- dt_QIMilk %>% 
#   ggplot(aes(x = QuinIngClasif, y = Milk/1000, weight = FEX_C)) + 
#   geom_boxplot(fill = c("#045A8D","#0570B0","#3690C0","#74A9CF","#A6BDDB"))+
#   ylab("Gasto Anual
#    (Miles COP$)")+
#   xlab("Milk")+
#   scale_y_continuous(breaks=seq(0,120,10))+
#   theme(panel.background = element_rect(fill = "white"),
#         axis.text.y = element_text(size = 12) ,
#         axis.text.x = element_text(size = 14) ,
#         axis.title.y = element_text(size = 15) ,
#         axis.title.x = element_text(size = 15) ,
#         panel.grid = element_line(color="#dcdcdc"))
# 
# 
# ##Sweet
# 
# Boxplot_Sweet <- dt_QISweet %>% 
#   ggplot(aes(x = QuinIngClasif, y = Sweet/1000, weight = FEX_C)) + 
#   geom_boxplot(fill = c("#045A8D","#0570B0","#3690C0","#74A9CF","#A6BDDB"))+
#   ylab("Gasto Anual
#    (Miles COP$)")+
#   xlab("Sweet")+
#   scale_y_continuous(breaks=seq(0,145,10))+
#   theme(panel.background = element_rect(fill = "white"),
#         axis.text.y = element_text(size = 12) ,
#         axis.text.x = element_text(size = 14) ,
#         axis.title.y = element_text(size = 15) ,
#         axis.title.x = element_text(size = 15) ,
#         panel.grid = element_line(color="#dcdcdc"))
# 
# 
# ##Alcoh
# 
# Boxplot_Alcoh <- dt_QIAlcoh %>% 
#   ggplot(aes(x = QuinIngClasif, y = Alcoh, weight = FEX_C)) + 
#   geom_boxplot(fill = c("#045A8D","#0570B0","#3690C0","#74A9CF","#A6BDDB"))+
#   ylab("Gasto Anual
#    (Miles de millones COP$)")+
#   xlab("Alcohol")+
#   scale_y_continuous(breaks=seq(0,145,10))+
#   theme(panel.background = element_rect(fill = "white"),
#         axis.text.y = element_text(size = 12) ,
#         axis.text.x = element_text(size = 14) ,
#         axis.title.y = element_text(size = 15) ,
#         axis.title.x = element_text(size = 15) ,
#         panel.grid = element_line(color="#dcdcdc"))
# 
# 
# ##Histogramas
# 
# ## Milk
# histo_milk <- dt_QIMilk %>% 
#   ggplot(aes(x = Milk/1000, weight = FEX_C)) +  
#   geom_histogram(aes(y = ..density..),  fill = '#5181AC', alpha = 0.8) +
#   scale_x_continuous(breaks=seq(0,1400,100))+
#   ggtitle("Milk")+
#   ylab("") + 
#   xlab("Gasto Anual
#    (Miles COP$)") +
#   theme(legend.title=element_blank(),
#         plot.title = element_text(size = 17, hjust = 0.5),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 15),
#         axis.title.y = element_text(size = 15))
# 
# ## Sweet
# histo_Sweet <- dt_QISweet %>% 
#   ggplot(aes(x = Sweet/1000, weight = FEX_C)) +  
#   geom_histogram(aes(y = ..density..),  fill = '#5181AC', alpha = 0.8) +
#   scale_x_continuous(breaks=seq(0,800,100))+
#   ggtitle("Sweet")+
#   ylab("") + 
#   xlab("Gasto Anual
#    (Miles COP$)") +
#   theme(legend.title=element_blank(),
#         plot.title = element_text(size = 17, hjust = 0.5),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 15),
#         axis.title.y = element_text(size = 15))
# 
# 
# 
# ## Alcohol
# histo_Alcoh <- dt_QIAlcoh %>% 
#   ggplot(aes(x = Alcoh/1000, weight = FEX_C)) +  
#   geom_histogram(aes(y = ..density..),  fill = '#5181AC', alpha = 0.8) +
#   scale_x_continuous(breaks=seq(0,65,10))+
#   ggtitle("Alcohol")+
#   ylab("") + 
#   xlab("Gasto Anual
#    (Miles COP$)") +
#   theme(legend.title=element_blank(),
#         plot.title = element_text(size = 17, hjust = 0.5),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 15),
#         axis.title.y = element_text(size = 15))
