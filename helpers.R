
getData <- function(vuosi, datadir, datafile,
                    sarakkeet = c(1,3,6,12,16,18,19,21,22,32),
                    headerNames = c("vaali", 
                                    "kuntanro",
                                    "vaalipiiri", "puolue", 
                                    "kunta", "etunimi", 
                                    "sukunimi", "ika", 
                                    "ammatti", "aiemmin")){
  data <- read_csv2(paste0(datadir,"/", datafile), 
                    col_names=FALSE,
                    show_col_types = FALSE) %>%
    select(all_of(sarakkeet)) 
  colnames(data) <- headerNames
  data <- data %>%
    mutate(ika = as.numeric(ika),
           vuosi=as.factor(vuosi),
           aiempi_arvo = aiemmin, 
           aiemmin = (aiemmin != "0000000")) %>%
    relocate(vuosi)  #%>%
  #   mutate(ika = as.numeric(ika),
  #          aiempi = as.numeric(aiempi) > 0) 
  
  
  return(data)
}



getKuntaKoodit <- function(vuosi, datadir, datafile){
  
  data <- read_csv2(paste0(datadir,"/", datafile), 
                    col_names=FALSE,
                    show_col_types = FALSE,
                    skip=1) %>%
    mutate(kuntanro = str_remove_all(X1, pattern = "'") %>% as.numeric(),
           kunta = str_remove_all(X3, pattern="\"")) %>%
    select(kuntanro,kunta)
  return(data)
}


getVakiluvut <- function(vuosi, datadir, datafile){
  
  data <- read_csv2(paste0(datadir,"/", datafile), 
                    col_names=FALSE,
                    show_col_types = FALSE,
                    skip=2) %>%
    mutate(kunta = X1,
           vakiluku = X3) %>%
    select(kunta,vakiluku)
  return(data)
}


teeTilasto <- function(data, min.lista=13, min.otos = 20){ 
  outdata <- data %>%
    filter(puolue %in% testiPuolueet) %>%
    group_by(vaali, vuosi, kunta, kuntanro, puolue) %>%
    summarize(ika.ka = mean(ika, na.rm=TRUE),
              kuntavaali = kuntavaali[1],
              vakiluku = vakiluku[1],
              listan_pituus = n(),
              kokeneet = sum(aiemmin, na.rm=TRUE)) %>%
    
    mutate(vanhojen_osuus = 100*kokeneet/listan_pituus) %>%
    filter(!is.na(vanhojen_osuus)) %>%
    filter(vanhojen_osuus > 0) %>%
    
    mutate(lista_maksimi = max(listan_pituus)) %>%
    
    mutate(tayttoaste = listan_pituus/lista_maksimi) %>%
    filter(lista_maksimi >= 13) %>%
    filter(listan_pituus >= 20) 
  
  return(outdata)
}




plotTilasto <- function(data, wrap = TRUE, caption="Vanhojen ehdokkaiden osuudet", 
                        puoluevarit = c( "KOK" = "blue",
                                          "KESK" = "lightgreen",
                                          "PS" = "yellow",
                                          "RKP" = "cyan",
                                          "SDP" = "pink",
                                          "VAS" = "red",
                                          "VIHR" = "darkgreen" )
                        ){
  medianval <- data %>% pull(vanhojen_osuus) %>% median()
  
  plotdata <- data %>%
    mutate(vuosi=as.factor(vuosi))
  
  gx <- ggplot(plotdata, aes(x=reorder(puolue,vanhojen_osuus,median, decreasing=TRUE), 
                             y=vanhojen_osuus, col = puolue))+
    geom_boxplot(notch=TRUE)+
    geom_hline(yintercept=medianval,lty=2)+
    xlab("Puolueet")+
    ylab("Vanhojen ehdokkaiden osuus")+
    theme(legend.position = "none")+
    scale_color_manual(values = puoluevarit) +
    ggtitle(paste0(caption))
  
  if (wrap) { gx <- gx +facet_wrap(~vuosi)}
  
  return(gx)
}


plotIka <- function(data,  caption ="", ymin=18, ymax=80, 
                    puoluevarit = c( "KOK" = "blue",
                                      "KESK" = "lightgreen",
                                      "PS" = "yellow",
                                      "RKP" = "cyan",
                                      "SDP" = "pink",
                                      "VAS" = "red",
                                      "VIHR" = "darkgreen" )){
  plotdata <- data
  
  medianval <- plotdata %>% pull(ika) %>% median()
  
  gx <- ggplot(data, aes(x=reorder(puolue,ika,median, decreasing=TRUE), 
                         y=ika, col=puolue, grp = puolue))+
    geom_boxplot(notch=TRUE)+
    geom_hline(yintercept=medianval,lty=2)+
    coord_cartesian(ylim=c(ymin,ymax))+
    scale_color_manual(values = puoluevarit) +
    ggtitle(caption)
  
}



plotIkaEffect <- function(data, caption="", ymin=NA, ymax=NA,
                          puoluevarit = c( "KOK" = "blue",
                                           "KESK" = "lightgreen",
                                           "PS" = "yellow",
                                           "RKP" = "cyan",
                                           "SDP" = "pink",
                                           "VAS" = "red",
                                           "VIHR" = "darkgreen" )){
  gx <- ggplot(data, aes(x=ika.ka, y=vanhojen_osuus))+
    geom_smooth(method="lm", col="black", lty= 2,  se=FALSE)+
    geom_point(aes(group=puolue, col=puolue), alpha=0.5, size=0.3)+
    geom_smooth(aes(group=puolue, col=puolue),method="lm", se=FALSE)+
    scale_color_manual(values = puoluevarit) +
    coord_cartesian(ylim=c(ymin,ymax))+
    ggtitle(caption)
}


plotIkaMat <- function(data, caption="", ymin=NA, ymax=NA,
                          puoluevarit = c( "KOK" = "blue",
                                           "KESK" = "lightgreen",
                                           "PS" = "yellow",
                                           "RKP" = "cyan",
                                           "SDP" = "pink",
                                           "VAS" = "red",
                                           "VIHR" = "darkgreen" )){
  gx <- ggplot(data, aes(x=ika, y=p.aiemmin))+
    geom_smooth(method="lm", col="black", lty= 2,  se=FALSE)+
    geom_point(aes(group=puolue, col=puolue), alpha=0.5, size=0.3)+
    geom_smooth(aes(group=puolue, col=puolue),method="lm", se=FALSE)+
    scale_color_manual(values = puoluevarit) +
    #coord_cartesian(ylim=c(ymin,ymax))+
    ggtitle(caption)
  return(gx)
}