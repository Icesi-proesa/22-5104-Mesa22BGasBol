


# Quintiles del gasto----


#Mensual


QMenBasicol <- as.data.frame(wtd.quantile(GBS21_Microdatos5$GbolMenBasico,weight = GBS21_Microdatos5$FEX_C.y,  prob=seq(0, 1, 1/20)))
QMenGeneral_tran <- as.data.frame(wtd.quantile(GBS21_Microdatos5$GbolMenGeneral_tran,weight = GBS21_Microdatos5$FEX_C.y, prob=seq(0, 1, 1/20)))
QMenGeneral_med <- as.data.frame(wtd.quantile(GBS21_Microdatos5$GbolMenGeneral_med,weight = GBS21_Microdatos5$FEX_C.y, prob=seq(0, 1, 1/20)))
QMenAmpliado <- as.data.frame(wtd.quantile(GBS21_Microdatos5$GbolMenAmpliado,weight = GBS21_Microdatos5$FEX_C.y, prob=seq(0, 1, 1/20)))
QMenSaludPrivado <- as.data.frame(wtd.quantile(GBS21_Microdatos5$GbolMenSaludPrivado,weight = GBS21_Microdatos5$FEX_C.y, prob=seq(0, 1, 1/20)))


GBolQui0 <- cbind(QMenBasicol,QMenGeneral_tran,QMenGeneral_med,QMenAmpliado,QMenSaludPrivado)

GBolQui1 <- GBolQui0 %>% rename(GbolMenBasicol = "wtd.quantile(GBS21_Microdatos5$GbolMenBasico, weight = GBS21_Microdatos5$FEX_C.y, prob = seq(0, 1, 1/20))",
                                GbolMenGeneral_tran = "wtd.quantile(GBS21_Microdatos5$GbolMenGeneral_tran, weight = GBS21_Microdatos5$FEX_C.y, prob = seq(0, 1, 1/20))",
                                GbolMenGeneral_med = "wtd.quantile(GBS21_Microdatos5$GbolMenGeneral_med, weight = GBS21_Microdatos5$FEX_C.y, prob = seq(0, 1, 1/20))",
                                GbolMenAmpliado = "wtd.quantile(GBS21_Microdatos5$GbolMenAmpliado, weight = GBS21_Microdatos5$FEX_C.y, prob = seq(0, 1, 1/20))",
                                GbolMenSaludPrivado = "wtd.quantile(GBS21_Microdatos5$GbolMenSaludPrivado, weight = GBS21_Microdatos5$FEX_C.y, prob = seq(0, 1, 1/20))")

## Transpongo la base

GBolQui2 <- transpose(GBolQui1)
colnames(GBolQui2) <- rownames(GBolQui1)

GBolQui2 <- GBolQui2 %>% mutate(GBS =c("GbolMenBasicol","GbolMenGeneral_tran","GbolMenGeneral_med","GbolMenAmpliado","GbolMenSaludPrivado")) %>%
  select("GBS", "  0%" ,"  5%"," 10%" ," 15%"," 20%"," 25%"," 30%"," 35%"," 40%"," 45%"," 50%"," 55%"," 60%"," 65%"," 70%"," 75%"," 80%"," 85%"," 90%"," 95%","100%")




# Quintiles del gasto como proporcion del ingreso----


#Mensual


QMenBasicol_PIng <- as.data.frame(wtd.quantile(GBS21_Microdatos5$GbolMenBasico/GBS21_Microdatos5$I_HOGAR2,weight = GBS21_Microdatos5$FEX_C.y,  prob=seq(0, 1, 1/20)))
QMenGeneral_tran_PIng <- as.data.frame(wtd.quantile(GBS21_Microdatos5$GbolMenGeneral_tran/GBS21_Microdatos5$I_HOGAR2,weight = GBS21_Microdatos5$FEX_C.y, prob=seq(0, 1, 1/20)))
QMenGeneral_med_PIng <- as.data.frame(wtd.quantile(GBS21_Microdatos5$GbolMenGeneral_med/GBS21_Microdatos5$I_HOGAR2,weight = GBS21_Microdatos5$FEX_C.y, prob=seq(0, 1, 1/20)))
QMenAmpliado_PIng <- as.data.frame(wtd.quantile(GBS21_Microdatos5$GbolMenAmpliado/GBS21_Microdatos5$I_HOGAR2,weight = GBS21_Microdatos5$FEX_C.y, prob=seq(0, 1, 1/20)))
QMenSaludPrivado_PIng <- as.data.frame(wtd.quantile(GBS21_Microdatos5$GbolMenSaludPrivado/GBS21_Microdatos5$I_HOGAR2,weight = GBS21_Microdatos5$FEX_C.y, prob=seq(0, 1, 1/20)))


GBolQui0_PIng <- cbind(QMenBasicol_PIng,QMenGeneral_tran_PIng,QMenGeneral_med_PIng,QMenAmpliado_PIng,QMenSaludPrivado_PIng)

GBolQui1_PIng <- GBolQui0_PIng %>% rename(GbolMenBasicol = "wtd.quantile(GBS21_Microdatos5$GbolMenBasico/GBS21_Microdatos5$I_HOGAR2, weight = GBS21_Microdatos5$FEX_C.y, prob = seq(0, 1, 1/20))",
                                          GbolMenGeneral_tran = "wtd.quantile(GBS21_Microdatos5$GbolMenGeneral_tran/GBS21_Microdatos5$I_HOGAR2, weight = GBS21_Microdatos5$FEX_C.y, prob = seq(0, 1, 1/20))",
                                          GbolMenGeneral_med = "wtd.quantile(GBS21_Microdatos5$GbolMenGeneral_med/GBS21_Microdatos5$I_HOGAR2, weight = GBS21_Microdatos5$FEX_C.y, prob = seq(0, 1, 1/20))",
                                          GbolMenAmpliado = "wtd.quantile(GBS21_Microdatos5$GbolMenAmpliado/GBS21_Microdatos5$I_HOGAR2, weight = GBS21_Microdatos5$FEX_C.y, prob = seq(0, 1, 1/20))",
                                          GbolMenSaludPrivado = "wtd.quantile(GBS21_Microdatos5$GbolMenSaludPrivado/GBS21_Microdatos5$I_HOGAR2, weight = GBS21_Microdatos5$FEX_C.y, prob = seq(0, 1, 1/20))")

## Transpongo la base

GBolQui2_PIng <- transpose(GBolQui1_PIng)
colnames(GBolQui2_PIng) <- rownames(GBolQui1_PIng)

GBolQui2_PIng <- GBolQui2_PIng %>% mutate(GBS =c("GbolMenBasicol","GbolMenGeneral_tran","GbolMenGeneral_med","GbolMenAmpliado","GbolMenSaludPrivado")) %>%
  select("GBS", "  0%" ,"  5%"," 10%" ," 15%"," 20%"," 25%"," 30%"," 35%"," 40%"," 45%"," 50%"," 55%"," 60%"," 65%"," 70%"," 75%"," 80%"," 85%"," 90%"," 95%","100%")





####Graficos gasto de bolsillo por hogares####

## Marco BD por debajo del 5% y por encima del 95%  para el gasto  y  como proporcion del ingreso                                             
GBS21_Graficos0 <- GBS21_Microdatos5 %>% mutate(marQ_GbolMenBasico = ifelse(GbolMenBasico<=wtd.quantile(GbolMenBasico,weight = FEX_C.y, prob = 0.05) | GbolMenBasico>=wtd.quantile(GbolMenBasico,weight = FEX_C.y, prob = 0.95),1,0),
                                                marQ_GbolMenGeneral_tran = ifelse(GbolMenGeneral_tran<=wtd.quantile(GbolMenGeneral_tran,weight = FEX_C.y, prob = 0.05)  |  GbolMenGeneral_tran>=wtd.quantile( GbolMenGeneral_tran,weight = FEX_C.y, prob = 0.95) ,1,0),
                                                marQ_GbolMenGeneral_med = ifelse(GbolMenGeneral_med<=wtd.quantile( GbolMenGeneral_med,weight = FEX_C.y, prob = 0.05) |  GbolMenGeneral_med>=wtd.quantile( GbolMenGeneral_med,weight = FEX_C.y, prob = 0.95),1,0),
                                                marQ_GbolMenAmpliado = ifelse(GbolMenAmpliado<=wtd.quantile( GbolMenAmpliado,weight = FEX_C.y, prob = 0.05) |  GbolMenAmpliado>=wtd.quantile( GbolMenAmpliado,weight = FEX_C.y, prob = 0.95) ,1,0),
                                                marQ_GbolMenSaludPrivado = ifelse(GbolMenSaludPrivado<=wtd.quantile(GbolMenSaludPrivado,weight = FEX_C.y, prob = 0.05) | GbolMenSaludPrivado>=wtd.quantile(GbolMenSaludPrivado,weight = FEX_C.y, prob = 0.95),1,0),
                                                
                                                marQ_GbolMenBasico_PIng = ifelse(GbolMenBasico/I_HOGAR2<=wtd.quantile(GbolMenBasico/I_HOGAR2,weight = FEX_C.y, prob = 0.05) | GbolMenBasico/I_HOGAR2>=wtd.quantile(GbolMenBasico/I_HOGAR2,weight = FEX_C.y, prob = 0.95),1,0),
                                                marQ_GbolMenGeneral_tran_PIng = ifelse(GbolMenGeneral_tran/I_HOGAR2<=wtd.quantile(GbolMenGeneral_tran/I_HOGAR2,weight = FEX_C.y, prob = 0.05)  |  GbolMenGeneral_tran/I_HOGAR2>=wtd.quantile( GbolMenGeneral_tran/I_HOGAR2,weight = FEX_C.y, prob = 0.95) ,1,0),
                                                marQ_GbolMenGeneral_med_PIng = ifelse(GbolMenGeneral_med/I_HOGAR2<=wtd.quantile( GbolMenGeneral_med/I_HOGAR2,weight = FEX_C.y, prob = 0.05) |  GbolMenGeneral_med/I_HOGAR2>=wtd.quantile( GbolMenGeneral_med/I_HOGAR2,weight = FEX_C.y, prob = 0.95),1,0),
                                                marQ_GbolMenAmpliado_PIng = ifelse(GbolMenAmpliado/I_HOGAR2<=wtd.quantile( GbolMenAmpliado/I_HOGAR2,weight = FEX_C.y, prob = 0.05) |  GbolMenAmpliado/I_HOGAR2>=wtd.quantile( GbolMenAmpliado/I_HOGAR2,weight = FEX_C.y, prob = 0.95) ,1,0),
                                                marQ_GbolMenSaludPrivado_PIng = ifelse(GbolMenSaludPrivado/I_HOGAR2<=wtd.quantile(GbolMenSaludPrivado/I_HOGAR2,weight = FEX_C.y, prob = 0.05) | GbolMenSaludPrivado/I_HOGAR2>=wtd.quantile(GbolMenSaludPrivado/I_HOGAR2,weight = FEX_C.y, prob = 0.95),1,0))

  


## BD marcadas para graficar boxplot por Quintiles de ingreso  del gasto

GBS21_Graficos1 <- GBS21_Graficos0 %>% filter(marQ_GbolMenBasico == 0) %>%
  select(GbolMenBasico,QuinIngClasif,FEX_C.y)



GBS21_Graficos2 <- GBS21_Graficos0 %>% filter(marQ_GbolMenGeneral_tran == 0) %>%
  select(GbolMenGeneral_tran,QuinIngClasif,FEX_C.y)
 

GBS21_Graficos3 <- GBS21_Graficos0 %>% filter( marQ_GbolMenGeneral_med == 0) %>% 
  select(GbolMenGeneral_med,QuinIngClasif,FEX_C.y) 


GBS21_Graficos4 <- GBS21_Graficos0 %>% filter( marQ_GbolMenAmpliado == 0) %>% 
  select(GbolMenAmpliado,QuinIngClasif,FEX_C.y)


## BD marcadas para graficar boxplot por Quintiles de ingreso como proporcion del ingreso

GBS21_Graficos1_PIng <- GBS21_Graficos0 %>% filter(marQ_GbolMenBasico_PIng == 0) %>%
  select(GbolMenBasico,QuinIngClasif,FEX_C.y,I_HOGAR2)



GBS21_Graficos2_PIng <- GBS21_Graficos0 %>% filter(marQ_GbolMenGeneral_tran_PIng == 0) %>%
  select(GbolMenGeneral_tran,QuinIngClasif,FEX_C.y,I_HOGAR2)


GBS21_Graficos3_PIng <- GBS21_Graficos0 %>% filter( marQ_GbolMenGeneral_med_PIng == 0) %>% 
  select(GbolMenGeneral_med,QuinIngClasif,FEX_C.y,I_HOGAR2) 


GBS21_Graficos4_PIng <- GBS21_Graficos0 %>% filter( marQ_GbolMenAmpliado_PIng == 0) %>% 
  select(GbolMenAmpliado,QuinIngClasif,FEX_C.y,I_HOGAR2)



## Box-Plot Quintiles


library(sysfonts)
library(showtext)
font_add_google("Source Sans Pro")
showtext_auto()

##Basico

stat_box_data1 <- function(y, upper_limit = max(GBS21_Graficos1$GbolMenBasico) * 0.0019758) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Mdn =', round(median(y), 1), '\n')
                    
    )
  )
}



Boxplot_basic <- GBS21_Graficos1 %>% 
  ggplot(aes(x = QuinIngClasif,
             y = GbolMenBasico/1000,
             weight = FEX_C.y )) + 
  geom_boxplot(fill = c("#006177","#3c8bb1","#bdc875","#28408d","#3c7bcc"))+
  ylab("GBS Mensual(Miles COP$)")+
  xlab("GBS B")+
  scale_y_continuous(breaks=seq(0,220,20), limits = c(0,219))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(family = "Source Sans Pro"),
        
        axis.text.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.text.x = element_text(size = 8*.pt,color = '#1e180c'),
        axis.title.y = element_text(size = 6.5*.pt,color = '#1e180c',) ,
        axis.title.x = element_text(size = 6.5*.pt,color = '#1e180c') ,
        panel.grid = element_line(color="#1e180c",size = 0.088))
        

Boxplot_basic <-Boxplot_basic + stat_summary(
  fun.data = stat_box_data1, 
  geom = "text", 
  hjust = 0.5,
  vjust = 0.6,
  size = 6.5,
  color = '#1e180c'
)


setwd(wd_resul)
ggsave(file ="GBS_B_Niv.pdf",width = 15,  height = 10, dpi = 1080)



##Gen_tran

stat_box_data2 <- function(y, upper_limit = max(GBS21_Graficos2$GbolMenGeneral_tran) * 0.0015365) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Mdn =', round(median(y), 2), '\n')
                    
    )
  )
}

Boxplot_gen_tran <- GBS21_Graficos2 %>% 
  ggplot(aes(x = QuinIngClasif, y = GbolMenGeneral_tran/1000, weight = FEX_C.y)) + 
  geom_boxplot(fill = c("#006177","#3c8bb1","#bdc875","#28408d","#3c7bcc"))+
  ylab("GBS Mensual(Miles COP$)")+
  xlab("GBS T")+
  scale_y_continuous(breaks=seq(0,220,20), limits = c(0,219))+
  
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(family = "Source Sans Pro"),
        axis.text.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.text.x = element_text(size = 8*.pt,color = '#1e180c'),
        axis.title.y = element_text(size = 6.5* .pt,color = '#1e180c',) ,
        axis.title.x = element_text(size = 6.5*.pt,color = '#1e180c') ,
        panel.grid = element_line(color="#1e180c",size = 0.088))
  
Boxplot_gen_tran <-Boxplot_gen_tran + stat_summary(
  fun.data = stat_box_data2, 
  geom = "text", 
  hjust = 0.5,
  vjust = 0.6,
  size = 6.5,
  color = '#1e180c'
)
         


setwd(wd_resul)
ggsave(file ="GBS_T_Niv.pdf",width = 15,  height = 10, dpi = 1080)

 

##Gen_med

stat_box_data3 <- function(y, upper_limit = max(GBS21_Graficos3$GbolMenGeneral_med) * 0.001193) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Mdn =', round(median(y), 1), '\n')
                    
    )
  )
}

Boxplot_gen_med <- GBS21_Graficos3 %>% 
  ggplot(aes(x = QuinIngClasif, y = GbolMenGeneral_med/1000, weight = FEX_C.y)) + 
  geom_boxplot(fill = c("#006177","#3c8bb1","#bdc875","#28408d","#3c7bcc"))+
    ylab("GBS Mensual(Miles COP$)")+
  xlab("GBS M")+
  scale_y_continuous(breaks=seq(0,220,20), limits = c(0,219))+
  
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(family = "Source Sans Pro"),
        axis.text.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.text.x = element_text(size = 8*.pt,color = '#1e180c'),
        axis.title.y = element_text(size = 6.5* .pt,color = '#1e180c',) ,
        axis.title.x = element_text(size = 6.5*.pt,color = '#1e180c') ,
        panel.grid = element_line(color="#1e180c",size = 0.088))

Boxplot_gen_med <- Boxplot_gen_med + stat_summary(
  fun.data = stat_box_data3, 
  geom = "text", 
  hjust = 0.5,
  vjust = 0.6,
  size = 6.5,
  color = '#1e180c'
)

setwd(wd_resul)
ggsave(file ="GBS_M_Niv.pdf",width = 15,  height = 10, dpi = 1080)


##Ampliado
stat_box_data4 <- function(y, upper_limit = max(GBS21_Graficos4$GbolMenAmpliado) * 0.0010425) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Mdn =', round(median(y), 1), '\n')
                    
    )
  )
}

Boxplot_Ampliado <- GBS21_Graficos4 %>% 
  ggplot(aes(x = QuinIngClasif, y = GbolMenAmpliado/1000,weight = FEX_C.y)) + 
  geom_boxplot(fill = c("#006177","#3c8bb1","#bdc875","#28408d","#3c7bcc"))+
  ylab("GBS Mensual(Miles COP$)")+
  xlab("GBS A")+
  scale_y_continuous(breaks=seq(0,220,20), limits = c(0,219))+
  
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(family = "Source Sans Pro"),
        axis.text.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.text.x = element_text(size = 8*.pt,color = '#1e180c'),
        axis.title.y = element_text(size = 6.5* .pt,color = '#1e180c',) ,
        axis.title.x = element_text(size = 6.5*.pt,color = '#1e180c') ,
        panel.grid = element_line(color="#1e180c",size = 0.088))

Boxplot_Ampliado <-Boxplot_Ampliado + 
  stat_summary(fun.data = stat_box_data4, 
               geom = "text", 
               hjust = 0.5,
               vjust = 0.6,
               size = 6.5,
               color = '#1e180c'
              
  )
 

setwd(wd_resul)
ggsave(file ="GBS_A_Niv.pdf",width = 15,  height = 10, dpi = 1080) 



BoxPlotQui <- (Boxplot_basic | Boxplot_gen_tran  ) / (Boxplot_gen_med | Boxplot_Ampliado)

BoxPlotQui <- BoxPlotQui + plot_annotation(caption ="Notas.Fuente:Calculos PROESA-Encuesta de Calidad de Vida(ECV) 2021,DANE,Colombia.Valores ponderados por factor de expansión.GBS=Gasto de Bolsillo en Salud.GBS-B=GBS Basico,GBS-T=GBS Transporte,
                                                     GBS-M=GBS Medicamentos,GBS-A=GBS Ampliado.Q1-Q5=Quintiles de ingreso, calculados a partir de ingresos totales de los hogares,cifras en miles de pesos corrientes 2021 (Miles de COP$), Mdn=Mediana")
                                                      
  
plot(BoxPlotQui)
                               
setwd(wd_resul)
ggsave(file ="GBolBoxMesNiv.pdf",width = 15,  height = 10, dpi = 1080)





## Box-Plot Quintiles proporcion del ingreso 


##Basico

stat_box_data1_PIng <- function(y, upper_limit = max(GBS21_Graficos1_PIng$GbolMenBasico/GBS21_Graficos1_PIng$I_HOGAR2) * 236) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Mdn =', round(median(y), 3), '\n')
                    
    )
  )
}



Boxplot_basic_PIng <- GBS21_Graficos1_PIng %>% 
  ggplot(aes(x = QuinIngClasif, y = (GbolMenBasico/I_HOGAR2)*100, weight = FEX_C.y )) + 
  geom_boxplot(fill = c("#006177","#3c8bb1","#bdc875","#28408d","#3c7bcc"))+
  ylab("GBS Mensual(% Ingreso Hogar)")+
  xlab("GBS B")+
  scale_y_continuous(breaks=seq(0,14,2), limits = c(0,13.9))+
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(family = "Source Sans Pro"),
        axis.text.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.text.x = element_text(size = 8*.pt,color = '#1e180c'),
        axis.title.y = element_text(size = 6.5* .pt,color = '#1e180c',) ,
        axis.title.x = element_text(size = 6.5*.pt,color = '#1e180c') ,
        panel.grid = element_line(color="#1e180c",size = 0.088))



Boxplot_basic_PIng <-Boxplot_basic_PIng + stat_summary(
  fun.data = stat_box_data1_PIng, 
  geom = "text", 
  hjust = 0.5,
  vjust = 0.01,
  size = 6.5,
  color = '#1e180c'
)



setwd(wd_resul)
ggsave(file ="GBS_B_Ping.pdf",width = 15,  height = 10, dpi = 1080)


##Gen_tran

stat_box_data2_PIng <- function(y, upper_limit = max(GBS21_Graficos2_PIng$GbolMenGeneral_tran/GBS21_Graficos2_PIng$I_HOGAR2) * 161.4891) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Mdn =', round(median(y), 3), '\n')
                    
    )
  )
}

Boxplot_gen_tran_PIng <- GBS21_Graficos2_PIng %>% 
  ggplot(aes(x = QuinIngClasif, y = (GbolMenGeneral_tran/I_HOGAR2)*100, weight = FEX_C.y)) + 
  geom_boxplot(fill = c("#006177","#3c8bb1","#bdc875","#28408d","#3c7bcc"))+
  ylab("GBS Mensual(% Ingreso Hogar)")+
  xlab("GBS T")+
  scale_y_continuous(breaks=seq(0,14,2), limits = c(0,13.89999))+
  
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(family = "Source Sans Pro"),
        axis.text.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.text.x = element_text(size = 8*.pt,color = '#1e180c'),
        axis.title.y = element_text(size = 6.5* .pt,color = '#1e180c',) ,
        axis.title.x = element_text(size = 6.5*.pt,color = '#1e180c') ,
        panel.grid = element_line(color="#1e180c",size = 0.088))

Boxplot_gen_tran_PIng <-Boxplot_gen_tran_PIng + stat_summary(
  fun.data = stat_box_data2_PIng, 
  geom = "text", 
  hjust = 0.5,
  vjust = 0.6,
  size = 6.5,
  color = '#1e180c'
)

plot(Boxplot_gen_tran_PIng)

setwd(wd_resul)
ggsave(file ="GBS_T_Ping.pdf",width = 15,  height = 10, dpi = 1080)

##Gen_med

stat_box_data3_PIng <- function(y, upper_limit = max(GBS21_Graficos3_PIng$GbolMenGeneral_med/GBS21_Graficos3_PIng$I_HOGAR2) * 139.5) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Mdn =', round(median(y), 3), '\n')
                    
    )
  )
}

Boxplot_gen_med_PIng <- GBS21_Graficos3_PIng %>% 
  ggplot(aes(x = QuinIngClasif, y = (GbolMenGeneral_med/I_HOGAR2)*100, weight = FEX_C.y)) + 
  geom_boxplot(fill = c("#006177","#3c8bb1","#bdc875","#28408d","#3c7bcc"))+
  ylab("GBS Mensual(% Ingreso Hogar)")+
  xlab("GBS M")+
  scale_y_continuous(breaks=seq(0,14,2), limits = c(0,13.999))+
  
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(family = "Source Sans Pro"),
        axis.text.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.text.x = element_text(size = 8*.pt,color = '#1e180c'),
        axis.title.y = element_text(size = 6.5* .pt,color = '#1e180c',) ,
        axis.title.x = element_text(size = 6.5*.pt,color = '#1e180c') ,
        panel.grid = element_line(color="#1e180c",size = 0.088))

Boxplot_gen_med_PIng <- Boxplot_gen_med_PIng + stat_summary(
  fun.data = stat_box_data3_PIng, 
  geom = "text", 
  hjust = 0.5,
  vjust = 0.6,
  size = 6.5,
  color = '#1e180c'
)

plot(Boxplot_gen_med_PIng)

setwd(wd_resul)
ggsave(file ="GBS_M_Ping.pdf",width = 15,  height = 10, dpi = 1080)

##Ampliado
stat_box_data4_PIng <- function(y, upper_limit = max(GBS21_Graficos4_PIng$GbolMenAmpliado/GBS21_Graficos4_PIng$I_HOGAR2) * 108.5) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Mdn =', round(median(y), 3), '\n')
                    
    )
  )
}

Boxplot_Ampliado_PIng <- GBS21_Graficos4_PIng %>% 
  ggplot(aes(x = QuinIngClasif, y = (GbolMenAmpliado/I_HOGAR2)*100,weight = FEX_C.y)) + 
  geom_boxplot(fill = c("#006177","#3c8bb1","#bdc875","#28408d","#3c7bcc"))+
  ylab("GBS Mensual(% Ingreso Hogar)")+
  xlab("GBS A")+
  scale_y_continuous(breaks=seq(0,14,2), limits = c(0,13.999))+
  
  theme(panel.background = element_rect(fill = "white"),
        text = element_text(family = "Source Sans Pro"),
        axis.text.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.text.x = element_text(size = 8*.pt,color = '#1e180c'),
        axis.title.y = element_text(size = 6.5* .pt,color = '#1e180c',) ,
        axis.title.x = element_text(size = 6.5*.pt,color = '#1e180c') ,
        panel.grid = element_line(color="#1e180c",size = 0.088))

Boxplot_Ampliado_PIng <-Boxplot_Ampliado_PIng + 
  stat_summary(fun.data = stat_box_data4_PIng, 
               geom = "text", 
               hjust = 0.5,
               vjust = 0.6,
               size = 6.5,
               color = '#1e180c'
               
  )
plot(Boxplot_Ampliado_PIng)


setwd(wd_resul)
ggsave(file ="GBS_A_Ping.pdf",width = 15,  height = 10, dpi = 1080)

BoxPlotQui_PIng <- (Boxplot_basic_PIng | Boxplot_gen_tran_PIng  ) / (Boxplot_gen_med_PIng| Boxplot_Ampliado_PIng)

BoxPlotQui_PIng <- BoxPlotQui_PIng + plot_annotation(caption = "Notas.Fuente: Calculos PROESA - Encuesta de Calidad de Vida (ECV) 2021, DANE , Colombia.Valores ponderados por factor de expansión.GBS=Gasto de Bolsillo en Salud.
                                                                GBS-B=GBS Basico, GBS-T=GBS Transporte, GBS-M=GBS Medicamentos, GBS-A=GBS Ampliado.Q1-Q5=Quintiles de ingreso, calculados a partir de ingresos totales de los hogares, Mdn=Mediana")

plot(BoxPlotQui_PIng)


setwd(wd_resul)
ggsave(file ="GBolBoxMesPIng.pdf",width = 15,  height = 10, dpi = 1080)



##Histogramas

 ## Basico
histograma_basic <- GBS21_Graficos1 %>% 
  ggplot(aes(x = GbolMenBasico/1000, weight = FEX_C.y)) +  
  geom_histogram(aes(y = ..density..),  fill = '#bdc875') +
    scale_x_continuous(breaks=seq(0,220,10))+
  
 
  geom_density(color = '#1e180c', size = 0.8,adjust = 2) + 
  ggtitle("GBS B")+
  ylab("FRECUENCIA RELATIVA (PROPORCIÓN DE HOGARES)") + 
  xlab("GBS MENSUAL(MILES COP$)") +
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill = "white"),
        text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(size = 17, hjust = 0.5),
        axis.text.x = element_text(size = 8*.pt,color = '#1e180c'),
        axis.text.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.title.x = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.title.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        panel.grid = element_line(color="#1e180c",size = 0.088))+
  coord_flip()+
  geom_vline(xintercept = median(GBS21_Graficos1$GbolMenBasico/1000),
                          col = "#1e180c",
                          lwd = 1/.pt,
             linetype = "dashed")+
  annotate("text",                        # Add text for mean
           x = mean(GBS21_Graficos1$GbolMenBasico/1000) * 0.85,
           y = mean(GBS21_Graficos1$GbolMenBasico/1000) * 0.001,
           label = paste("MEDIANA =", round(median(GBS21_Graficos1$GbolMenBasico/1000),2)),
                         size = 7,
                         col = '#006177' )
 
setwd(wd_resul)
ggsave(file ="HIST_GBS_B_NIV.pdf",width = 15,  height = 10, dpi = 1080)

  
## General_tra
histograma_gen_tran <- GBS21_Graficos2 %>% 
  ggplot(aes(x = GbolMenGeneral_tran/1000, weight = FEX_C.y)) +  
  geom_histogram(aes(y = ..density..),  fill = '#bdc875') +
  scale_x_continuous(breaks=seq(0,220,10))+
  geom_density(color = '#1e180c', size = 0.8,adjust = 2) + 
  ggtitle("GBS T")+
  ylab("FRECUENCIA RELATIVA (PROPORCIÓN DE HOGARES)") + 
  xlab("GBS MENSUAL(MILES COP$)") +
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill = "white"),
        text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(size = 17, hjust = 0.5),
        axis.text.x = element_text(size = 8*.pt,color = '#1e180c'),
        axis.text.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.title.x = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.title.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        panel.grid = element_line(color="#1e180c",size = 0.088))+
  coord_flip()+
  geom_vline(xintercept = median(GBS21_Graficos1$GbolMenBasico/1000),
             col = "#1e180c",
             lwd = 1/.pt,
             linetype = "dashed")+
  annotate("text",                        # Add text for mean
           x = mean(GBS21_Graficos1$GbolMenBasico/1000) * 0.9,
           y = mean(GBS21_Graficos1$GbolMenBasico/1000) * 0.0007,
           label = paste("MEDIANA =", round(median(GBS21_Graficos2$GbolMenGeneral_tran/1000),2)),
           size = 7,
           col = '#006177' )

setwd(wd_resul)
ggsave(file ="HIST_GBS_T_NIV.pdf",width = 15,  height = 10, dpi = 1080)


## General_med
histograma_gen_med <- GBS21_Graficos3 %>% 
  ggplot(aes(x = GbolMenGeneral_med /1000, weight = FEX_C.y)) +  
  geom_histogram(aes(y = ..density..),  fill = '#bdc875') +
  scale_x_continuous(breaks=seq(0,220,20))+
  geom_density(color = '#1e180c', size = 0.8,adjust = 2) + 
  ggtitle("GBS M")+
  ylab("FRECUENCIA RELATIVA (PROPORCIÓN DE HOGARES)") + 
  xlab("GBS MENSUAL(MILES COP$)") +
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill = "white"),
        text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(size = 17, hjust = 0.5),
        axis.text.x = element_text(size = 8*.pt,color = '#1e180c'),
        axis.text.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.title.x = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.title.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        panel.grid = element_line(color="#1e180c",size = 0.088))+
  coord_flip()+
  geom_vline(xintercept = median(GBS21_Graficos3$GbolMenGeneral_med/1000),
             col = "#1e180c",
             lwd = 1/.pt,
             linetype = "dashed")+
  annotate("text",                        # Add text for mean
           x = mean(GBS21_Graficos1$GbolMenBasico/1000) * 1.24,
           y = mean(GBS21_Graficos1$GbolMenBasico/1000) * 0.0007,
           label = paste("MEDIANA =", round(median(GBS21_Graficos3$GbolMenGeneral_med/1000),2)),
           size = 7,
           col = '#006177' )

setwd(wd_resul)
ggsave(file ="HIST_GBS_M_NIV.pdf",width = 15,  height = 10, dpi = 1080)


## Ampliado
histograma_Ampl <- GBS21_Graficos4 %>% 
  ggplot(aes(x = GbolMenAmpliado /1000, weight = FEX_C.y)) +  
  geom_histogram(aes(y = ..density..),  fill = '#bdc875') +
  scale_x_continuous(breaks=seq(0,280,20))+
  geom_density(color = '#1e180c', size = 0.8,adjust = 2) + 
  ggtitle("GBS A")+
  ylab("FRECUENCIA RELATIVA (PROPORCIÓN DE HOGARES)") + 
  xlab("GBS MENSUAL(MILES COP$)") +
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill = "white"),
        text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(size = 17, hjust = 0.5),
        axis.text.x = element_text(size = 8*.pt,color = '#1e180c'),
        axis.text.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.title.x = element_text(size = 6.5*.pt,color = '#1e180c'),
        axis.title.y = element_text(size = 6.5*.pt,color = '#1e180c'),
        panel.grid = element_line(color="#1e180c",size = 0.088))+
  coord_flip()+
  geom_vline(xintercept = median(GBS21_Graficos4$GbolMenAmpliado/1000),
             col = "#1e180c",
             lwd = 1/.pt,
             linetype = "dashed")+
  annotate("text",                        # Add text for mean
           x = mean(GBS21_Graficos1$GbolMenBasico/1000) * 1.39,
           y = mean(GBS21_Graficos1$GbolMenBasico/1000) * 0.00044,
           label = paste("MEDIANA =", round(median(GBS21_Graficos4$GbolMenAmpliado/1000),2)),
           size = 7,
           col = '#006177' )


setwd(wd_resul)
ggsave(file ="HIST_GBS_A_NIV.pdf",width = 15,  height = 10, dpi = 1080)


HistGbs <- (histograma_basic | histograma_gen_tran  ) / (histograma_gen_med | histograma_Ampl)

HistGbs <- HistGbs + plot_annotation(caption = "Notas.Fuente:Calculos PROESA-Encuesta de Calidad de Vida(ECV) 2021,DANE,Colombia.Valores ponderados por factor de expansión.GBS=Gasto de Bolsillo en Salud.GBS-B=GBS Basico,GBS-T=GBS Transporte,
                                                     GBS-M=GBS Medicamentos,GBS-A=GBS Ampliado.,cifras en miles de pesos corrientes 2021 (Miles de COP$)")

plot(HistGbs)
  
setwd(wd_resul)
ggsave(file ="GBolHistMesNiv.pdf",width = 15,  height = 10, dpi = 1080)  



## Salidas


# Microdatos
write.xlsx(GBS21_Microdatos6,paste0(wd_resul,"1GBS21_Microdatos.xlsx"))


#Estimadores
##Creo libro de excel
wb <- createWorkbook()
##creo las hojas
addWorksheet(wb,sheetName = "GBS21_Macros")
writeData(wb,"GBS21_Macros",GBS21_Estimadores10)

addWorksheet(wb,sheetName = "GBS21_Quintiles")
writeData(wb,"GBS21_Quintiles",GBolQui2)

addWorksheet(wb,sheetName = "GBS21_Quintiles_PIng")
writeData(wb,"GBS21_Quintiles_PIng",GBolQui2_PIng)

addWorksheet(wb,sheetName = "GBS21_MicroTodos")
writeData(wb,"GBS21_MicroTodos",EstadisticasTodos)

addWorksheet(wb,sheetName = "GBS21_MicroPositivo")
writeData(wb,"GBS21_MicroPositivo",GBS21_Positivo3,row.names=T)

saveWorkbook(wb,file = paste0(wd_resul, "2GBS21_Estimadores.xlsx"),overwrite = T)

GBS21_Estimadores <- list("GBS21_Macros" = GBS21_Estimadores10, "GBS21_Quintiles"= GBolQui2, "GBS21_Quintiles_PIng" = GBolQui2_PIng ,"GBS21_MicroTodos" = EstadisticasTodos ,"GBS21_MicroPositivo" = GBS21_Positivo3 )

write.xlsx(GBS21_Estimadores,paste0(wd_resul,"2GBS21_Estimadores.xlsx"),row.names=T)                                 
