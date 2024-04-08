# Code Written by: Mr Naveen Kumar P
# Guided by: Mr. Tarun and Dr. Prabhu G

#------------- Functions ----------------------------------------------------
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}
library(openxlsx)
library(soiltexture)
library(ggplot2)

#' percentage of sand or clay
#'
#' Calculates the percentages of sand, silt, and clay
#' @param RL hydrometer reading in calgon solution
#' @param RH hydrometer reading in soil suspension
#' @param MS weight of oven dry soil (g)
#' @return percentage of sand or clay
#' @examples
#' temp1 <- Particle_Percentage (RL =5,RH =25, MS = 35 );
#' temp2 <- Particle_Percentage(RL = c(5,4,3),RH =c(25,24,25), MS = c(35,34,36));
#' @export
# 1. Particle_Percentage
Particle_Percentage <- function(RL,RH, MS){
  Result <- ((RH -RL)/MS)*100;
  return(round(Result,4));
}


#' Soil Bulk Density
#'
#' Calculates the bulk density of soil
#' @param MS weight of oven dry soil (Mg)
#' @param VT total volume (m3)
#' @return bulk density of soil (Mg/m3)
#' @examples
#' temp1 <- SBD(MS =155,VT =117.81);
#' temp2 <- SBD(MS = c(155,149,141),VT =c(117.81,117.81,117.81));
#' @export
# 2. Soil Bulk Density
SBD <- function(MS,VT){
  Result <- (MS/VT);
  return(round(Result,4));
}

#' Soil Particle Density
#'
#' Calculates the particle density of soil
#' @param MS weight of oven dry soil (Mg)
#' @param VS volume of solids (m3)
#' @return particle density of soil (Mg/m3)
#' @examples
#' temp1 <- SPD(MS =155,VS =59.05);
#' temp2 <- SPD(MS = c(155,149,141),VS =c(61.52,59.05,53.26));
#' @export
# 3. Soil Particle Density
SPD <- function(MS,VS){
  Result <- (MS/VS);
  return(round(Result,4));
}

#' Total Porosity
#'
#' Calculates the total porosity (%)
#' @param BD bulk density (Mg/m3)
#' @param PD particle density (Mg/m3)
#' @return Total Porosity (%)
#' @examples
#' temp1 <- TP(BD =1.53,PD =2.65);
#' temp2 <- TP(BD = c(1.55,1.49,1.41),PD =c(2.61,2.59,2.53));
#' @export
# 4. Total Porosity
TP <- function(BD,PD){
  Result <-(1-(BD/PD))*100;
  return(round(Result,4));
}

#' Gravimetric water content 
#'
#' Calculates the gravimetric water content (%) 
#' @param Mw mass of moisture (g)
#' @param Ms mass of oven dry soil (g)
#' @return gravimetric water content (%) 
#' @examples
#' temp1 <- GWC(Mw =23,Ms =155);
#' temp2 <- GWC(Mw = c(25,36,29),Ms =c(155,168,158));
#' @export
# 5. Gravimetric water content
GWC <- function(Mw,Ms){
  Result <- (Mw/Ms)*100;
  return(round(Result,4));
}

#' Volumetric water content  
#'
#' Calculates the volumetric water content (% or cm3/cm3) 
#' @param gwc gravimetric water content (%)
#' @param bd bulk density of soil (Mg/m3)
#' @return volumetric water content (% or cm3/cm3)
#' @examples
#' temp1 <- VWC(gwc =23,bd =1.55);
#' temp2 <- VWC(gwc = c(25,36,29),bd =c(1.55,1.68,1.58));
#' @export
# 6. Volumetric water content 
VWC <- function(gwc,bd){
  Result <- (gwc*bd);
  return(round(Result,4));
}

#' Depth of soil water (DSW) (mm/cm or cm/m) 
#'
#' Calculates the Depth of soil water (DSW) (mm/cm or cm/m)  
#' @param vwc volumetric water content (%)
#' @param sd  soil depth (cm or m)
#' @return Depth of soil water (DSW) (mm/cm or cm/m) 
#' @examples
#' temp1 <- DSW(vwc =23,sd =15);
#' temp2 <- DSW(vwc = c(25,36,29),sd =c(15,20,10));
#' @export
# 7. Depth of soil water (DSW) (mm/cm or cm/m)  
DSW <- function(vwc,sd){
  Result <- ((vwc/100)*sd);
  return(round(Result,4));
}

#'Air filled porosity (AFP) (%) 
#'
#' Calculates the Air filled porosity (AFP) (%)   
#' @param tp total porosity (%)
#' @param gwc  gravimetric water content (%)
#' @return Air filled porosity (AFP) (%) 
#' @examples
#' temp1 <- AFP(tp =49,gwc =15);
#' temp2 <- AFP(tp = c(48,44,49),gwc =c(15,20,10));
#' @export
# 8. Air filled porosity (AFP) (%) 
AFP <- function(tp,gwc){
  Result <- (tp-gwc);
  return(round(Result,4));
}

#'Water filled pore space (WFPS) (%) 
#'
#' Calculates the Water filled pore space (WFPS) (%)  
#' @param gwc  gravimetric water content (%)
#' @param bd  bulk density (Mg/m3)
#' @param pd  particle density (Mg/m3)
#' @return Water filled pore space (WFPS) (%)  
#' @examples
#' temp1 <- WFPS(gwc= 13,bd=1.31, pd=2.53);
#' temp2 <- WFPS(gwc = c(14,16,19),bd =c(1.35,1.19,1.23),pd=c(2.54,2.45,2.31));
#' @export
# 9. Water filled pore space (WFPS) (%) 
WFPS <- function(gwc,bd,pd){
  Result <- (gwc*bd)/((1-(bd/pd))*2.65);
  return(round(Result,4));
}

#'Degree of saturation (DS) (%)
#'
#' Calculates the Degree of saturation (DS) (%)
#' @param vwc  volumetric water content (%)
#' @param tp  total porosity (%)
#' @return Degree of saturation (DS) (%) 
#' @examples
#' temp1 <- DS(vwc= 19,tp=49.5);
#' temp2 <- DS(vwc = c(19,23,25),tp =c(48,46,49));
#' @export
# 10. Degree of saturation (DS) (%)
DS <- function(vwc,tp){
  Result <- (vwc/tp)*100;
  return(round(Result,4));
}

#'Void ratio (VR) 
#'
#' Calculates the Void ratio (VR) 
#' @param tp  total porosity (%)
#' @return Void ratio (VR) 
#' @examples
#' temp1 <- VR(tp=49.5);
#' temp2 <- VR(tp =c(48,46,49));
#' @export
# 11. Void ratio (VR) 
VR <- function(tp){
  Result <- (tp/(100-tp));
  return(round(Result,4));
}


#'Soil organic carbon stock (SOCS) (Mg/ha) 
#'
#' Calculates the Soil organic carbon stock (SOCS) (Mg/ha) 
#' @param ar  area (m2)
#' @param soc  soil organic carbon (%)
#' @param bd  bulk density (Mg/m3)
#' @param sd  soil depth (cm)
#' @return Soil organic carbon stock (SOCS) (Mg/ha) 
#' @examples
#' temp1 <- SOCS(ar=10000,soc=0.69,bd=1.56,sd=15);
#' @export
# 12. Soil organic carbon stock (SOCS) (Mg/ha) 
SOCS <- function(ar,soc,bd,sd){
  Result <- (ar*soc*bd*(sd/100))/100;
  return(round(Result,4));
}

#'Soil organic carbon sequestration rate  (SOCSR) (Mg/ha/year)  
#'
#' Calculates the Soil organic carbon sequestration rate  (SOCS) (Mg/ha/year) 
#' @param FSOCS  Final SOC stock (Mg/ha)
#' @param ISOCS Initial SOC stock (Mg/ha)
#' @param YT  Time (Years)
#' @return Soil organic carbon sequestration rate  (SOCSR) (Mg/ha/year) 
#' @examples
#' temp1 <- SOCSR(FSOCS=13.62,ISOCS=10.52,YT = 5);
#' @export
# 12. Soil organic carbon sequestration rate  (SOCSR) (Mg/ha/year) 
SOCSR <- function(FSOCS,ISOCS, YT){
  Result <- (FSOCS-ISOCS)/YT;
  return(round(Result,4));
}

#'Relative root density (RLD) (cm/cm3) 
#'
#' Calculates the Relative root density (RLD) (cm/cm3)  
#' @param trl  Total root length in a core (cm)
#' @param vsc  Volume of the core (cm3)
#' @return Relative root density (RLD) (cm/cm3) 
#' @examples
#' temp1 <- RLD(trl=2400,vsc=1507);
#' @export
# 13. Relative root density (RLD) (cm/cm3) 
RLD <- function(trl,vsc){
  Result <- (trl)/vsc;
  return(round(Result,4));
}


#'Root surface density (RSD) (cm2/cm3)
#'
#' Calculates the Root surface density (RSD) (cm2/cm3) 
#' @param trsa  Total root surface area in a core (cm2)
#' @param  vsc  Volume of the core (cm3)
#' @return Root surface density (RSD) (cm2/cm3)
#' @examples
#' temp1 <- RSD(trsa=247,vsc=1507);
#' @export
# 14. Root surface density (RSD) (cm2/cm3)
RSD <- function(trsa,vsc){
  Result <- (trsa)/vsc;
  return(round(Result,4));
}

#'Relative weight density (RWD) (mg/cm3)
#'
#' Calculates the Relative weight density (RWD) (mg/cm3) 
#' @param trdw  Total root dry weight in a core (mg)
#' @param  vsc  Volume of the core (cm3)
#' @return Relative weight density (RWD) (mg/cm3)
#' @examples
#' temp1 <- RWD(trdw=250,vsc=1507);
#' @export
# 15. Relative weight density (RWD) (mg/cm3)
RWD <- function(trdw,vsc){
  Result <- (trdw)/vsc;
  return(round(Result,4));
}


#'Soil binding capacity (SBC) (kg/cm2/plant)
#'
#' Calculates the Soil binding capacity (SBC) (kg/cm2/plant)
#' @param trdw  Total root dry weight in a core (mg)
#' @param  r  radius of root (mm)
#' @return Soil binding capacity (SBC) (kg/cm2/plant)
#' @examples
#' temp1 <- SBC(trdw=250,r=0.165);
#' @export
# 16. Soil binding capacity (SBC) (kg/cm2/plant)
SBC <- function(trdw,r){
  Result <- (trdw/(1000*1000))/(r*r);
  return(round(Result,4));
}


#' Mean weight diameter (MWD)
#'
#' Calculates the Mean weight diameter (MWD)
#' @param WC Weight of soil retained (g)
#' @param ADC average diameter between two sieves (mm)
#' @return Mean weight diameter (MWD), mm
#' #' @examples
#' temp1 <- MWD(WC = c(4.237,42.42,36.326,20.34),
#' ADC=c(5,1.1265,0.153,0.0265));
#' @export
# 1. Particle_Percentage
MWD<- function(WC,ADC){
  total_WC <- sum(WC)
  WC1 <- WC/total_WC
  product <- WC1*ADC
  Result <- sum(product);
  return(round(Result,4));
}

#' Geometric mean diameter (GMD)
#'
#' Calculates the Geometric mean diameter (GMD)
#' @param WC Weight of soil retained (g)
#' @param ADC average diameter between two sieves (mm)
#' @return Geometric mean diameter (GMD), mm
#' #' @examples
#' temp1 <- GMD(WC = c(4.237,42.42,36.326,20.34),
#' ADC=c(5,1.1265,0.153,0.0265));
#' @export
# 1. Particle_Percentage
GMD<- function(WC,ADC){
  total_WC <- sum(WC)
  WC1 <- WC/total_WC
  product <- WC1*log10(ADC)
  temp1 <- sum(product)/sum(WC1)
  Result <- exp(temp1);
  return(round(Result,4));
}

#' Calculating the indices from the excel file
#'
#' Calculates different weed indices from the given excel file
#' @param file_path path for your excel file
#' @return different weed indices
#' @examples
#' file_path <- "G:/Naveen_spectral_similarity_index/Weed_Indices/Weed index-1.xlsx";
#' all_Weed_indices <- Soil_Indices(file_path);
#' @export
#
Soil_Indices<- function(...){
  fname <- file.choose()
  sh_name<-getSheetNames(fname)
  
  if(length(sh_name)>0){
  Final_list <-list()
  for (nk in 1:length(sh_name)) {
    df <- openxlsx::read.xlsx(fname,sheet = sh_name[nk],startRow = 3)
    no_col <- ncol(df)
  
  if(no_col == 43){
      text_df <- df[,c(1:6)]
      text_df <- na.omit(text_df)
      
      BD_df <- df[,c(7:10)]
      BD_df <- na.omit(BD_df)
      
      PD_df <- df[,c(11:16)]
      PD_df<- na.omit(PD_df)
      
      GWC_df <-df[,c(17:20)]
      GWC_df <- na.omit(GWC_df)
      
      ASD_df <-df[,c(21:29)]
      ASD_df <- na.omit(ASD_df)
      
      SOC_df <-df[,c(30:37)]
      SOC_df <- na.omit(SOC_df)
      
      Root_df <- df[,c(38:43)]
      Root_df <- na.omit(Root_df)
      
      # Calculation for soil physical properties
      if(nrow(text_df>0)){
          clay_df <- Particle_Percentage(RL = text_df$RL_7h,
                                        RH = text_df$R7h,
                                        MS = text_df$Oven.dry.soil)
          sand_df <- 100-Particle_Percentage(RL = text_df$RL_40s,
                                         RH = text_df$R40s,
                                         MS = text_df$Oven.dry.soil)
          silt_df <- 100-(clay_df+sand_df)
          
          # Create a data frame of soil textures:
          text_df1 <- data.frame( 
            "CLAY"  = clay_df, 
            "SILT"  = silt_df, 
            "SAND"  = sand_df 
            ) 
          
          # Classify according to the USDA classification
          text_classes <- TT.points.in.classes( 
            tri.data    = text_df1, 
            class.sys   = "USDA.TT",
            PiC.type    = "t", 
            collapse    = ";"
          )   #
          
        final_text_df <- cbind.fill(text_df1,text_classes)
        rownames(final_text_df)<-text_df[1:nrow(text_df),1]
        colnames(final_text_df)<-c("Clay","Silt","Sand","Texture")
        
        TT.plot(
          class.sys = "USDA.TT",
          tri.data = text_df1,
          css.names = c("CLAY", "SILT", "SAND"),
          main = "Soil texture triangle",
          cex.axis = 0.8,
          cex.lab = 0.8
        )
        }
      else{warning("There is no data for calculate soil texture")}
      
      if (nrow(BD_df>0)) {
          n1_bd <-nrow(BD_df)
          volume1 <- pi*(BD_df$r^2)*BD_df$h
          BD1 <- as.data.frame (SBD(BD_df$Ms,volume1))
          rownames(BD1)<-BD_df[1:(n1_bd),1]
          colnames(BD1)<-"Bulk density"
          }else{warning("There is no data for calculating the soil bulk density")}
      
      if (nrow(PD_df>0)) {
        VS1 <-(PD_df$PYW-PD_df$PY)-(PD_df$PYSW-PD_df$PYS)
        PD1 <-as.data.frame(SPD(PD_df$Ms,VS1))
        rownames(PD1)<-PD_df[1:nrow(PD_df),1]
        colnames(PD1)<-"Particle density"
      }else{warning("There is no data for calculating the soil particle density")}
      
      if (nrow(BD1)== nrow(PD1)){
        TP1 <- TP (BD1,PD1)
        colnames(TP1)<-"Total Porosity"
      }else{warning("Length of bulk and particle density are not same")}
      
      if (length(TP1>0)) {
        VR1 <- VR(TP1)
        Final_porosity_df<-cbind.fill(TP1,VR1)
        colnames(Final_porosity_df)<-c("Total porosity","Void ratio")
      }else{warning("There is no total porosity data for 
        calculating the void ratio ")}
      
      if (nrow(GWC_df >0)) {
        GWC1<-GWC((GWC_df$FM-GWC_df$Ms),GWC_df$Ms)
        
        if (length(BD1>0)) {
          VWC1 <- VWC(GWC1,BD1)
          DSW1 <- DSW(VWC1,GWC_df$SD)
            }
          else{warning("There is no bulk density data for calculating the
                       volumetric water content and depth of soil water")}
        
        if (length(TP1>0)&&length(GWC1>0) ) {
          AFP1 <- AFP(TP1,GWC1)
          }
        else{warning("There is missing data in total porosity or gravimetric water content
        for calculating the air filled porosity")}
        
        if ((length(BD1>0)&&length(PD1>0))&&(length(GWC1>0))) {
          WFPS1 <- WFPS(GWC1,BD1,PD1)
        }
        else{warning("There is missing data in bulk or particle density
        for calculating the water filled pore space")}
        
        if ((length(BD1>0)&&length(PD1>0))&&(length(GWC1>0))) {
          WFPS1 <- WFPS(GWC1,BD1,PD1)
        }
        else{warning("There is missing data in bulk or particle density
        for calculating the water filled pore space")}
        
        if ((length(VWC1>0)&&length(TP1>0))) {
          DS1 <- DS(VWC1,TP1)
        }
        else{warning("There is missing data in VWC or total porosity
        for calculating the degree of saturation")}
        
        Water_df <- cbind.fill("GWC"=GWC1,
                                "VWC"=VWC1,
                                "DSW"=DSW1,
                               "AFP"=AFP1,
                               "WFPS"=WFPS1,
                               "DS"=DS1)
        colnames(Water_df)<-c("GWC","VWC",
                              "DSW","AFP",
                              "WFPS","DS")
      }else{warning("There is no data for calculating 
                    the soil water parameters")}
      
      if (nrow(ASD_df>0)) {
        n1_bd <-nrow(ASD_df)
        WC_df <-ASD_df[,2:5]
        ADC_df <-ASD_df[,6:9]
        MWD1<- data.frame()
        GMD1<- data.frame()
        
        for(j in (1:n1_bd)) {
          MWD2 <-MWD(WC_df[j,],ADC_df[j,])
          GMD2 <-GMD(WC_df[j,],ADC_df[j,])
          MWD1 <- rbind(MWD1,MWD2)
          GMD1 <- rbind(GMD1,GMD2)
          }
        
        ASD_df1 <- cbind.fill(MWD1,GMD1)
        rownames(ASD_df1)<-ASD_df[1:(n1_bd),1]
        colnames(ASD_df1)<-c("MWD","GMD")
      }else{warning("There is no data for calculating the
                    soil aggregate/particle size distribution parameters")}
      
      if (nrow(SOC_df>0)) {
        n1_bd <-nrow(SOC_df)
        SOCS1<-SOCS(SOC_df$Area,SOC_df$SOC,
                    SOC_df$BD,SOC_df$SD)
        SOCSR1<-SOCSR(SOC_df$FSOCS,SOC_df$ISOCS,
                      SOC_df$YT)
        SOC_df1 <- cbind.fill(SOCS1,SOCSR1)
        rownames(SOC_df1)<-SOC_df[1:(n1_bd),1]
        colnames(SOC_df1)<-c("SOCS","SOCSR")
      }else{warning("There is no data for calculating the
                    soil chemical parameters")}
     
       if (nrow(Root_df>0)) {
        n1_bd <-nrow(Root_df)
        RLD1<-RLD(Root_df$TRL,Root_df$VSC)
        RSD1 <-RSD(Root_df$TRSA,Root_df$VSC)
        RWD1 <- RWD(Root_df$TRDW,Root_df$VSC)
        SBC1 <- SBC(Root_df$TRDW,Root_df$r)
        
        Root_df1 <- cbind.fill(RLD1,RSD1,RWD1,SBC1)
        rownames(Root_df1)<-Root_df[1:(n1_bd),1]
        colnames(Root_df1)<-c("RLD","RSD","RWD","SBC")
      }else{warning("There is no data for calculating the
                    root parameters")}
      
      res_list <-list("Soil texture" = final_text_df,
                      "Bulk density"=BD1,
                      "Particle density"=PD1,
                      "Porosity"=Final_porosity_df,
                      "Water parameters"=Water_df,
                      "Aggregate size distribution" =ASD_df1,
                      "Organic carbon"=SOC_df1,
                      "Root parameters"=Root_df1)
  }
    else if (no_col ==37){
      text_df <- df[,c(1:6)]
      text_df <- na.omit(text_df)
      
      BD_df <- df[,c(7:10)]
      BD_df <- na.omit(BD_df)
      
      PD_df <- df[,c(11:16)]
      PD_df<- na.omit(PD_df)
      
      GWC_df <-df[,c(17:20)]
      GWC_df <- na.omit(GWC_df)
      
      ASD_df <-df[,c(21:29)]
      ASD_df <- na.omit(ASD_df)
      
      SOC_df <-df[,c(30:37)]
      SOC_df <- na.omit(SOC_df)
      
      # Calculation for soil physical properties
      if(nrow(text_df>0)){
        clay_df <- Particle_Percentage(RL = text_df$RL_7h,
                                       RH = text_df$R7h,
                                       MS = text_df$Oven.dry.soil)
        sand_df <- 100-Particle_Percentage(RL = text_df$RL_40s,
                                           RH = text_df$R40s,
                                           MS = text_df$Oven.dry.soil)
        silt_df <- 100-(clay_df+sand_df)
        
        # Create a data frame of soil textures:
        text_df1 <- data.frame( 
          "CLAY"  = clay_df, 
          "SILT"  = silt_df, 
          "SAND"  = sand_df 
        ) 
        
        # Classify according to the USDA classification
        text_classes <- TT.points.in.classes( 
          tri.data    = text_df1, 
          class.sys   = "USDA.TT",
          PiC.type    = "t", 
          collapse    = ";"
        )   #
        
        final_text_df <- cbind.fill(text_df1,text_classes)
        rownames(final_text_df)<-text_df[1:nrow(text_df),1]
        colnames(final_text_df)<-c("Clay","Silt","Sand","Texture")
        
        TT.plot(
          class.sys = "USDA.TT",
          tri.data = text_df1,
          css.names = c("CLAY", "SILT", "SAND"),
          main = "Soil texture triangle",
          cex.axis = 0.8,
          cex.lab = 0.8
        )
      }
      else{warning("There is no data for calculate soil texture")}
      
      if (nrow(BD_df>0)) {
        n1_bd <-nrow(BD_df)
        volume1 <- pi*(BD_df$r^2)*BD_df$h
        BD1 <- as.data.frame (SBD(BD_df$Ms,volume1))
        rownames(BD1)<-BD_df[1:(n1_bd),1]
        colnames(BD1)<-"Bulk density"
      }else{warning("There is no data for calculating the soil bulk density")}
      
      if (nrow(PD_df>0)) {
        VS1 <-(PD_df$PYW-PD_df$PY)-(PD_df$PYSW-PD_df$PYS)
        PD1 <-as.data.frame(SPD(PD_df$Ms,VS1))
        rownames(PD1)<-PD_df[1:nrow(PD_df),1]
        colnames(PD1)<-"Particle density"
      }else{warning("There is no data for calculating the soil particle density")}
      
      if (nrow(BD1)== nrow(PD1)){
        TP1 <- TP (BD1,PD1)
        colnames(TP1)<-"Total Porosity"
      }else{warning("Length of bulk and particle density are not same")}
      
      if (length(TP1>0)) {
        VR1 <- VR(TP1)
        Final_porosity_df<-cbind.fill(TP1,VR1)
        colnames(Final_porosity_df)<-c("Total porosity","Void ratio")
      }else{warning("There is no total porosity data for 
        calculating the void ratio ")}
      
      if (nrow(GWC_df >0)) {
        GWC1<-GWC((GWC_df$FM-GWC_df$Ms),GWC_df$Ms)
        
        if (length(BD1>0)) {
          VWC1 <- VWC(GWC1,BD1)
          DSW1 <- DSW(VWC1,GWC_df$SD)
        }
        else{warning("There is no bulk density data for calculating the
                       volumetric water content and depth of soil water")}
        
        if (length(TP1>0)&&length(GWC1>0) ) {
          AFP1 <- AFP(TP1,GWC1)
        }
        else{warning("There is missing data in total porosity or gravimetric water content
        for calculating the air filled porosity")}
        
        if ((length(BD1>0)&&length(PD1>0))&&(length(GWC1>0))) {
          WFPS1 <- WFPS(GWC1,BD1,PD1)
        }
        else{warning("There is missing data in bulk or particle density
        for calculating the water filled pore space")}
        
        if ((length(BD1>0)&&length(PD1>0))&&(length(GWC1>0))) {
          WFPS1 <- WFPS(GWC1,BD1,PD1)
        }
        else{warning("There is missing data in bulk or particle density
        for calculating the water filled pore space")}
        
        if ((length(VWC1>0)&&length(TP1>0))) {
          DS1 <- DS(VWC1,TP1)
        }
        else{warning("There is missing data in VWC or total porosity
        for calculating the degree of saturation")}
        
        Water_df <- cbind.fill("GWC"=GWC1,
                               "VWC"=VWC1,
                               "DSW"=DSW1,
                               "AFP"=AFP1,
                               "WFPS"=WFPS1,
                               "DS"=DS1)
        colnames(Water_df)<-c("GWC","VWC",
                              "DSW","AFP",
                              "WFPS","DS")
      }else{warning("There is no data for calculating 
                    the soil water parameters")}
      
      if (nrow(ASD_df>0)) {
        n1_bd <-nrow(ASD_df)
        WC_df <-ASD_df[,2:5]
        ADC_df <-ASD_df[,6:9]
        MWD1<- data.frame()
        GMD1<- data.frame()
        
        for(j in (1:n1_bd)) {
          MWD2 <-MWD(WC_df[j,],ADC_df[j,])
          GMD2 <-GMD(WC_df[j,],ADC_df[j,])
          MWD1 <- rbind(MWD1,MWD2)
          GMD1 <- rbind(GMD1,GMD2)
        }
        
        ASD_df1 <- cbind.fill(MWD1,GMD1)
        rownames(ASD_df1)<-ASD_df[1:(n1_bd),1]
        colnames(ASD_df1)<-c("MWD","GMD")
      }else{warning("There is no data for calculating the
                    soil aggregate/particle size distribution parameters")}
      
      if (nrow(SOC_df>0)) {
        n1_bd <-nrow(SOC_df)
        SOCS1<-SOCS(SOC_df$Area,SOC_df$SOC,
                    SOC_df$BD,SOC_df$SD)
        SOCSR1<-SOCSR(SOC_df$FSOCS,SOC_df$ISOCS,
                      SOC_df$YT)
        SOC_df1 <- cbind.fill(SOCS1,SOCSR1)
        rownames(SOC_df1)<-SOC_df[1:(n1_bd),1]
        colnames(SOC_df1)<-c("SOCS","SOCSR")
      }else{warning("There is no data for calculating the
                    soil chemical parameters")}
      res_list <-list("Soil texture" = final_text_df,
                      "Bulk density"=BD1,
                      "Particle density"=PD1,
                      "Porosity"=Final_porosity_df,
                      "Water parameters"=Water_df,
                      "Aggregate size distribution" =ASD_df1,
                      "Organic carbon"=SOC_df1
                      )
    }
  else if (no_col ==29){
    text_df <- df[,c(1:6)]
    text_df <- na.omit(text_df)
    
    BD_df <- df[,c(7:10)]
    BD_df <- na.omit(BD_df)
    
    PD_df <- df[,c(11:16)]
    PD_df<- na.omit(PD_df)
    
    GWC_df <-df[,c(17:20)]
    GWC_df <- na.omit(GWC_df)
    
    ASD_df <-df[,c(21:29)]
    ASD_df <- na.omit(ASD_df)
    
    # Calculation for soil physical properties
    if(nrow(text_df>0)){
      clay_df <- Particle_Percentage(RL = text_df$RL_7h,
                                     RH = text_df$R7h,
                                     MS = text_df$Oven.dry.soil)
      sand_df <- 100-Particle_Percentage(RL = text_df$RL_40s,
                                         RH = text_df$R40s,
                                         MS = text_df$Oven.dry.soil)
      silt_df <- 100-(clay_df+sand_df)
      
      # Create a data frame of soil textures:
      text_df1 <- data.frame( 
        "CLAY"  = clay_df, 
        "SILT"  = silt_df, 
        "SAND"  = sand_df 
      ) 
      
      # Classify according to the USDA classification
      text_classes <- TT.points.in.classes( 
        tri.data    = text_df1, 
        class.sys   = "USDA.TT",
        PiC.type    = "t", 
        collapse    = ";"
      )   #
      
      final_text_df <- cbind.fill(text_df1,text_classes)
      rownames(final_text_df)<-text_df[1:nrow(text_df),1]
      colnames(final_text_df)<-c("Clay","Silt","Sand","Texture")
      
      TT.plot(
        class.sys = "USDA.TT",
        tri.data = text_df1,
        css.names = c("CLAY", "SILT", "SAND"),
        main = "Soil texture triangle",
        cex.axis = 0.8,
        cex.lab = 0.8
      )
    }
    else{warning("There is no data for calculate soil texture")}
    
    if (nrow(BD_df>0)) {
      n1_bd <-nrow(BD_df)
      volume1 <- pi*(BD_df$r^2)*BD_df$h
      BD1 <- as.data.frame (SBD(BD_df$Ms,volume1))
      rownames(BD1)<-BD_df[1:(n1_bd),1]
      colnames(BD1)<-"Bulk density"
    }else{warning("There is no data for calculating the soil bulk density")}
    
    if (nrow(PD_df>0)) {
      VS1 <-(PD_df$PYW-PD_df$PY)-(PD_df$PYSW-PD_df$PYS)
      PD1 <-as.data.frame(SPD(PD_df$Ms,VS1))
      rownames(PD1)<-PD_df[1:nrow(PD_df),1]
      colnames(PD1)<-"Particle density"
    }else{warning("There is no data for calculating the soil particle density")}
    
    if (nrow(BD1)== nrow(PD1)){
      TP1 <- TP (BD1,PD1)
      colnames(TP1)<-"Total Porosity"
    }else{warning("Length of bulk and particle density are not same")}
    
    if (length(TP1>0)) {
      VR1 <- VR(TP1)
      Final_porosity_df<-cbind.fill(TP1,VR1)
      colnames(Final_porosity_df)<-c("Total porosity","Void ratio")
    }else{warning("There is no total porosity data for 
        calculating the void ratio ")}
    
    if (nrow(GWC_df >0)) {
      GWC1<-GWC((GWC_df$FM-GWC_df$Ms),GWC_df$Ms)
      
      if (length(BD1>0)) {
        VWC1 <- VWC(GWC1,BD1)
        DSW1 <- DSW(VWC1,GWC_df$SD)
      }
      else{warning("There is no bulk density data for calculating the
                       volumetric water content and depth of soil water")}
      
      if (length(TP1>0)&&length(GWC1>0) ) {
        AFP1 <- AFP(TP1,GWC1)
      }
      else{warning("There is missing data in total porosity or gravimetric water content
        for calculating the air filled porosity")}
      
      if ((length(BD1>0)&&length(PD1>0))&&(length(GWC1>0))) {
        WFPS1 <- WFPS(GWC1,BD1,PD1)
      }
      else{warning("There is missing data in bulk or particle density
        for calculating the water filled pore space")}
      
      if ((length(BD1>0)&&length(PD1>0))&&(length(GWC1>0))) {
        WFPS1 <- WFPS(GWC1,BD1,PD1)
      }
      else{warning("There is missing data in bulk or particle density
        for calculating the water filled pore space")}
      
      if ((length(VWC1>0)&&length(TP1>0))) {
        DS1 <- DS(VWC1,TP1)
      }
      else{warning("There is missing data in VWC or total porosity
        for calculating the degree of saturation")}
      
      Water_df <- cbind.fill("GWC"=GWC1,
                             "VWC"=VWC1,
                             "DSW"=DSW1,
                             "AFP"=AFP1,
                             "WFPS"=WFPS1,
                             "DS"=DS1)
      colnames(Water_df)<-c("GWC","VWC",
                            "DSW","AFP",
                            "WFPS","DS")
    }else{warning("There is no data for calculating 
                    the soil water parameters")}
    
    if (nrow(ASD_df>0)) {
      n1_bd <-nrow(ASD_df)
      WC_df <-ASD_df[,2:5]
      ADC_df <-ASD_df[,6:9]
      MWD1<- data.frame()
      GMD1<- data.frame()
      
      for(j in (1:n1_bd)) {
        MWD2 <-MWD(WC_df[j,],ADC_df[j,])
        GMD2 <-GMD(WC_df[j,],ADC_df[j,])
        MWD1 <- rbind(MWD1,MWD2)
        GMD1 <- rbind(GMD1,GMD2)
      }
      
      ASD_df1 <- cbind.fill(MWD1,GMD1)
      rownames(ASD_df1)<-ASD_df[1:(n1_bd),1]
      colnames(ASD_df1)<-c("MWD","GMD")
    }else{warning("There is no data for calculating the
                    soil aggregate/particle size distribution parameters")}
  
    res_list <-list("Soil texture" = final_text_df,
                    "Bulk density"=BD1,
                    "Particle density"=PD1,
                    "Porosity"=Final_porosity_df,
                    "Water parameters"=Water_df,
                    "Aggregate size distribution" =ASD_df1
                    )
    }
  else if (no_col == 20){
    text_df <- df[,c(1:6)]
    text_df <- na.omit(text_df)
    
    BD_df <- df[,c(7:10)]
    BD_df <- na.omit(BD_df)
    
    PD_df <- df[,c(11:16)]
    PD_df<- na.omit(PD_df)
    
    GWC_df <-df[,c(17:20)]
    GWC_df <- na.omit(GWC_df)
    
    # Calculation for soil physical properties
    if(nrow(text_df>0)){
      clay_df <- Particle_Percentage(RL = text_df$RL_7h,
                                     RH = text_df$R7h,
                                     MS = text_df$Oven.dry.soil)
      sand_df <- 100-Particle_Percentage(RL = text_df$RL_40s,
                                         RH = text_df$R40s,
                                         MS = text_df$Oven.dry.soil)
      silt_df <- 100-(clay_df+sand_df)
      
      # Create a data frame of soil textures:
      text_df1 <- data.frame( 
        "CLAY"  = clay_df, 
        "SILT"  = silt_df, 
        "SAND"  = sand_df 
      ) 
      
      # Classify according to the USDA classification
      text_classes <- TT.points.in.classes( 
        tri.data    = text_df1, 
        class.sys   = "USDA.TT",
        PiC.type    = "t", 
        collapse    = ";"
      )   #
      
      final_text_df <- cbind.fill(text_df1,text_classes)
      rownames(final_text_df)<-text_df[1:nrow(text_df),1]
      colnames(final_text_df)<-c("Clay","Silt","Sand","Texture")
      
      TT.plot(
        class.sys = "USDA.TT",
        tri.data = text_df1,
        css.names = c("CLAY", "SILT", "SAND"),
        main = "Soil texture triangle",
        cex.axis = 0.8,
        cex.lab = 0.8
      )
    }
    else{warning("There is no data for calculate soil texture")}
    
    if (nrow(BD_df>0)) {
      n1_bd <-nrow(BD_df)
      volume1 <- pi*(BD_df$r^2)*BD_df$h
      BD1 <- as.data.frame (SBD(BD_df$Ms,volume1))
      rownames(BD1)<-BD_df[1:(n1_bd),1]
      colnames(BD1)<-"Bulk density"
    }else{warning("There is no data for calculating the soil bulk density")}
    
    if (nrow(PD_df>0)) {
      VS1 <-(PD_df$PYW-PD_df$PY)-(PD_df$PYSW-PD_df$PYS)
      PD1 <-as.data.frame(SPD(PD_df$Ms,VS1))
      rownames(PD1)<-PD_df[1:nrow(PD_df),1]
      colnames(PD1)<-"Particle density"
    }else{warning("There is no data for calculating the soil particle density")}
    
    if (nrow(BD1)== nrow(PD1)){
      TP1 <- TP (BD1,PD1)
      colnames(TP1)<-"Total Porosity"
    }else{warning("Length of bulk and particle density are not same")}
    
    if (length(TP1>0)) {
      VR1 <- VR(TP1)
      Final_porosity_df<-cbind.fill(TP1,VR1)
      colnames(Final_porosity_df)<-c("Total porosity","Void ratio")
    }else{warning("There is no total porosity data for 
        calculating the void ratio ")}
    
    if (nrow(GWC_df >0)) {
      GWC1<-GWC((GWC_df$FM-GWC_df$Ms),GWC_df$Ms)
      
      if (length(BD1>0)) {
        VWC1 <- VWC(GWC1,BD1)
        DSW1 <- DSW(VWC1,GWC_df$SD)
      }
      else{warning("There is no bulk density data for calculating the
                       volumetric water content and depth of soil water")}
      
      if (length(TP1>0)&&length(GWC1>0) ) {
        AFP1 <- AFP(TP1,GWC1)
      }
      else{warning("There is missing data in total porosity or gravimetric water content
        for calculating the air filled porosity")}
      
      if ((length(BD1>0)&&length(PD1>0))&&(length(GWC1>0))) {
        WFPS1 <- WFPS(GWC1,BD1,PD1)
      }
      else{warning("There is missing data in bulk or particle density
        for calculating the water filled pore space")}
      
      if ((length(BD1>0)&&length(PD1>0))&&(length(GWC1>0))) {
        WFPS1 <- WFPS(GWC1,BD1,PD1)
      }
      else{warning("There is missing data in bulk or particle density
        for calculating the water filled pore space")}
      
      if ((length(VWC1>0)&&length(TP1>0))) {
        DS1 <- DS(VWC1,TP1)
      }
      else{warning("There is missing data in VWC or total porosity
        for calculating the degree of saturation")}
      
      Water_df <- cbind.fill("GWC"=GWC1,
                             "VWC"=VWC1,
                             "DSW"=DSW1,
                             "AFP"=AFP1,
                             "WFPS"=WFPS1,
                             "DS"=DS1)
      colnames(Water_df)<-c("GWC","VWC",
                            "DSW","AFP",
                            "WFPS","DS")
    }else{warning("There is no data for calculating 
                    the soil water parameters")}
    
    res_list <-list("Soil texture" = final_text_df,
                    "Bulk density"=BD1,
                    "Particle density"=PD1,
                    "Porosity"=Final_porosity_df,
                    "Water parameters"=Water_df
    )
    }
  else if (no_col == 16){
    text_df <- df[,c(1:6)]
    text_df <- na.omit(text_df)
    
    BD_df <- df[,c(7:10)]
    BD_df <- na.omit(BD_df)
    
    PD_df <- df[,c(11:16)]
    PD_df<- na.omit(PD_df)
    
    # Calculation for soil physical properties
    if(nrow(text_df>0)){
      clay_df <- Particle_Percentage(RL = text_df$RL_7h,
                                     RH = text_df$R7h,
                                     MS = text_df$Oven.dry.soil)
      sand_df <- 100-Particle_Percentage(RL = text_df$RL_40s,
                                         RH = text_df$R40s,
                                         MS = text_df$Oven.dry.soil)
      silt_df <- 100-(clay_df+sand_df)
      
      # Create a data frame of soil textures:
      text_df1 <- data.frame( 
        "CLAY"  = clay_df, 
        "SILT"  = silt_df, 
        "SAND"  = sand_df 
      ) 
      
      # Classify according to the USDA classification
      text_classes <- TT.points.in.classes( 
        tri.data    = text_df1, 
        class.sys   = "USDA.TT",
        PiC.type    = "t", 
        collapse    = ";"
      )   #
      
      final_text_df <- cbind.fill(text_df1,text_classes)
      rownames(final_text_df)<-text_df[1:nrow(text_df),1]
      colnames(final_text_df)<-c("Clay","Silt","Sand","Texture")
      
      TT.plot(
        class.sys = "USDA.TT",
        tri.data = text_df1,
        css.names = c("CLAY", "SILT", "SAND"),
        main = "Soil texture triangle",
        cex.axis = 0.8,
        cex.lab = 0.8
      )
    }
    else{warning("There is no data for calculate soil texture")}
    
    if (nrow(BD_df>0)) {
      n1_bd <-nrow(BD_df)
      volume1 <- pi*(BD_df$r^2)*BD_df$h
      BD1 <- as.data.frame (SBD(BD_df$Ms,volume1))
      rownames(BD1)<-BD_df[1:(n1_bd),1]
      colnames(BD1)<-"Bulk density"
    }else{warning("There is no data for calculating the soil bulk density")}
    
    if (nrow(PD_df>0)) {
      VS1 <-(PD_df$PYW-PD_df$PY)-(PD_df$PYSW-PD_df$PYS)
      PD1 <-as.data.frame(SPD(PD_df$Ms,VS1))
      rownames(PD1)<-PD_df[1:nrow(PD_df),1]
      colnames(PD1)<-"Particle density"
    }else{warning("There is no data for calculating the soil particle density")}
    
    if (nrow(BD1)== nrow(PD1)){
      TP1 <- TP (BD1,PD1)
      colnames(TP1)<-"Total Porosity"
    }else{warning("Length of bulk and particle density are not same")}
    
    if (length(TP1>0)) {
      VR1 <- VR(TP1)
      Final_porosity_df<-cbind.fill(TP1,VR1)
      colnames(Final_porosity_df)<-c("Total porosity","Void ratio")
    }else{warning("There is no total porosity data for 
        calculating the void ratio ")}
    
    res_list <-list("Soil texture" = final_text_df,
                    "Bulk density"=BD1,
                    "Particle density"=PD1,
                    "Porosity"=Final_porosity_df)
  }
  else if (no_col == 10){
    text_df <- df[,c(1:6)]
    text_df <- na.omit(text_df)
    
    BD_df <- df[,c(7:10)]
    BD_df <- na.omit(BD_df)
    
   # Calculation for soil physical properties
    if(nrow(text_df>0)){
      clay_df <- Particle_Percentage(RL = text_df$RL_7h,
                                     RH = text_df$R7h,
                                     MS = text_df$Oven.dry.soil)
      sand_df <- 100-Particle_Percentage(RL = text_df$RL_40s,
                                         RH = text_df$R40s,
                                         MS = text_df$Oven.dry.soil)
      silt_df <- 100-(clay_df+sand_df)
      
      # Create a data frame of soil textures:
      text_df1 <- data.frame( 
        "CLAY"  = clay_df, 
        "SILT"  = silt_df, 
        "SAND"  = sand_df 
      ) 
      
      # Classify according to the USDA classification
      text_classes <- TT.points.in.classes( 
        tri.data    = text_df1, 
        class.sys   = "USDA.TT",
        PiC.type    = "t", 
        collapse    = ";"
      )   #
      
      final_text_df <- cbind.fill(text_df1,text_classes)
      rownames(final_text_df)<-text_df[1:nrow(text_df),1]
      colnames(final_text_df)<-c("Clay","Silt","Sand","Texture")
      
      TT.plot(
        class.sys = "USDA.TT",
        tri.data = text_df1,
        css.names = c("CLAY", "SILT", "SAND"),
        main = "Soil texture triangle",
        cex.axis = 0.8,
        cex.lab = 0.8
      )
    }
    else{warning("There is no data for calculate soil texture")}
    
    if (nrow(BD_df>0)) {
      n1_bd <-nrow(BD_df)
      volume1 <- pi*(BD_df$r^2)*BD_df$h
      BD1 <- as.data.frame (SBD(BD_df$Ms,volume1))
      rownames(BD1)<-BD_df[1:(n1_bd),1]
      colnames(BD1)<-"Bulk density"
    }else{warning("There is no data for calculating the soil bulk density")}
    
          res_list <-list("Soil texture" = final_text_df,
                      "Bulk density"=BD1
                      )
  }
    else if (no_col == 6){
      text_df <- df[,c(1:6)]
      text_df <- na.omit(text_df)
      
      # Calculation for soil physical properties
      if(nrow(text_df>0)){
        clay_df <- Particle_Percentage(RL = text_df$RL_7h,
                                       RH = text_df$R7h,
                                       MS = text_df$Oven.dry.soil)
        sand_df <- 100-Particle_Percentage(RL = text_df$RL_40s,
                                           RH = text_df$R40s,
                                           MS = text_df$Oven.dry.soil)
        silt_df <- 100-(clay_df+sand_df)
        
        # Create a data frame of soil textures:
        text_df1 <- data.frame( 
          "CLAY"  = clay_df, 
          "SILT"  = silt_df, 
          "SAND"  = sand_df 
        ) 
        
        # Classify according to the USDA classification
        text_classes <- TT.points.in.classes( 
          tri.data    = text_df1, 
          class.sys   = "USDA.TT",
          PiC.type    = "t", 
          collapse    = ";"
        )   
        final_text_df <- cbind.fill(text_df1,text_classes)
        rownames(final_text_df)<-text_df[1:nrow(text_df),1]
        colnames(final_text_df)<-c("Clay","Silt","Sand","Texture")
        
        TT.plot(
          class.sys = "USDA.TT",
          tri.data = text_df1,
          css.names = c("CLAY", "SILT", "SAND"),
          main = "Soil texture triangle",
          cex.axis = 0.8,
          cex.lab = 0.8
        )
      }
      else{warning("There is no data for calculate soil texture")}
      res_list <-list("Soil texture" = final_text_df)
    }
  else{warning("Kindly check your excel datasheet. There is no data for indices calculation 
                or the data is not in proper arrangement as pescribed by the package. 
                *** For data formating, Kindly visit the github webpage:
                https://github.com/Naveen1098/Weed_Indices/tree/main")}
  Final_list[[length(Final_list)+1]]<-res_list
  }}

  Clay_df <-data.frame()
  Silt_df <-data.frame()
  Sand_df <-data.frame()
  Texture_df <-data.frame()
  BD_df <- data.frame()
  PD_df <- data.frame()
  TP_df <- data.frame()
  VR_df <- data.frame()
  GWC_df <- data.frame()
  VWC_df <- data.frame()
  DSW_df <- data.frame()
  AFP_df <- data.frame()
  WFPS_df <- data.frame()
  DS_df <- data.frame()
  MWD_df <- data.frame()
  GMD_df <- data.frame()
  SOCS_df <- data.frame()
  SOCSR_df <- data.frame()
  RLD_df <- data.frame()
  RSD_df <- data.frame()
  RWD_df <- data.frame()
  SBC_df <- data.frame()
  
  for(kn in 1:length(sh_name)){
    df1 <- Final_list[kn]
    
    if (length(df1[[1]]) == 8){
    text_df2 <- as.data.frame(df1[[1]]$`Soil texture`)
    Clay1<- text_df2$Clay
    Silt1<- text_df2$Silt
    Sand1<- text_df2$Sand
    Texture1<- text_df2$Texture
    
    Bulk_density2 <- as.data.frame(df1[[1]]$`Bulk density`)
    BD1 <- Bulk_density2$`Bulk density`
    
    Particle_density2 <- as.data.frame(df1[[1]]$`Particle density`)
    PD1 <- Particle_density2$`Particle density`
    
    Porosity2 <- as.data.frame(df1[[1]]$`Porosity`)
    TP1 <- Porosity2$`Total porosity`
    VR1 <- Porosity2$`Void ratio`
    
    Water_parameters2 <- as.data.frame(df1[[1]]$`Water parameters`)
    GWC1 <-Water_parameters2$GWC
    VWC1 <-Water_parameters2$VWC
    DSW1 <-Water_parameters2$DSW
    AFP1 <-Water_parameters2$AFP
    WFPS1<-Water_parameters2$WFPS
    DS1<-Water_parameters2$DS
    
    ASD2 <- as.data.frame(df1[[1]]$`Aggregate size distribution`)
    MWD1 <-ASD2$MWD
    GMD1<-ASD2$GMD
    
    OC2 <- as.data.frame(df1[[1]]$`Organic carbon`)
    SOCS1<-OC2$SOCS
    SOCSR1<-OC2$SOCSR
    
    RP2 <- as.data.frame(df1[[1]]$`Root parameters`)
    RLD1 <-RP2$RLD
    RSD1 <-RP2$RSD
    RWD1 <-RP2$RWD
    SBC1 <-RP2$SBC
    
    Clay_df <-cbind.fill(Clay_df,Clay1)
    Silt_df <-cbind.fill(Silt_df,Silt1)
    Sand_df <-cbind.fill(Sand_df,Sand1)
    Texture_df <-cbind.fill(Texture_df,Texture1)
    BD_df <- cbind.fill(BD_df,BD1)
    PD_df <- cbind.fill(PD_df,PD1)
    TP_df <- cbind.fill(TP_df,TP1)
    VR_df <- cbind.fill(VR_df,VR1)
    GWC_df <- cbind.fill(GWC_df,GWC1)
    VWC_df <- cbind.fill(VWC_df,VWC1)
    DSW_df <- cbind.fill(DSW_df,DSW1)
    AFP_df <- cbind.fill(AFP_df,AFP1)
    WFPS_df <- cbind.fill(WFPS_df,WFPS1)
    DS_df <- cbind.fill(DS_df,DS1)
    MWD_df <- cbind.fill(MWD_df,MWD1)
    GMD_df <- cbind.fill(GMD_df,GMD1)
    SOCS_df <- cbind.fill(SOCS_df,SOCS1)
    SOCSR_df <- cbind.fill(SOCSR_df,SOCSR1)
    RLD_df <- cbind.fill(RLD_df,RLD1)
    RSD_df <- cbind.fill(RSD_df,RSD1)
    RWD_df <- cbind.fill(RWD_df,RWD1)
    SBC_df <- cbind.fill(SBC_df,SBC1)
    }
    else if (length(df1[[1]]) == 7){
      text_df2 <- as.data.frame(df1[[1]]$`Soil texture`)
      Clay1<- text_df2$Clay
      Silt1<- text_df2$Silt
      Sand1<- text_df2$Sand
      Texture1<- text_df2$Texture
      
      Bulk_density2 <- as.data.frame(df1[[1]]$`Bulk density`)
      BD1 <- Bulk_density2$`Bulk density`
      
      Particle_density2 <- as.data.frame(df1[[1]]$`Particle density`)
      PD1 <- Particle_density2$`Particle density`
      
      Porosity2 <- as.data.frame(df1[[1]]$`Porosity`)
      TP1 <- Porosity2$`Total porosity`
      VR1 <- Porosity2$`Void ratio`
      
      Water_parameters2 <- as.data.frame(df1[[1]]$`Water parameters`)
      GWC1 <-Water_parameters2$GWC
      VWC1 <-Water_parameters2$VWC
      DSW1 <-Water_parameters2$DSW
      AFP1 <-Water_parameters2$AFP
      WFPS1<-Water_parameters2$WFPS
      DS1<-Water_parameters2$DS
      
      ASD2 <- as.data.frame(df1[[1]]$`Aggregate size distribution`)
      MWD1 <-ASD2$MWD
      GMD1<-ASD2$GMD
      
      OC2 <- as.data.frame(df1[[1]]$`Organic carbon`)
      SOCS1<-OC2$SOCS
      SOCSR1<-OC2$SOCSR
      
      Clay_df <-cbind.fill(Clay_df,Clay1)
      Silt_df <-cbind.fill(Silt_df,Silt1)
      Sand_df <-cbind.fill(Sand_df,Sand1)
      Texture_df <-cbind.fill(Texture_df,Texture1)
      BD_df <- cbind.fill(BD_df,BD1)
      PD_df <- cbind.fill(PD_df,PD1)
      TP_df <- cbind.fill(TP_df,TP1)
      VR_df <- cbind.fill(VR_df,VR1)
      GWC_df <- cbind.fill(GWC_df,GWC1)
      VWC_df <- cbind.fill(VWC_df,VWC1)
      DSW_df <- cbind.fill(DSW_df,DSW1)
      AFP_df <- cbind.fill(AFP_df,AFP1)
      WFPS_df <- cbind.fill(WFPS_df,WFPS1)
      DS_df <- cbind.fill(DS_df,DS1)
      MWD_df <- cbind.fill(MWD_df,MWD1)
      GMD_df <- cbind.fill(GMD_df,GMD1)
      SOCS_df <- cbind.fill(SOCS_df,SOCS1)
      SOCSR_df <- cbind.fill(SOCSR_df,SOCSR1)}
    else if (length(df1[[1]]) == 6){
      text_df2 <- as.data.frame(df1[[1]]$`Soil texture`)
      Clay1<- text_df2$Clay
      Silt1<- text_df2$Silt
      Sand1<- text_df2$Sand
      Texture1<- text_df2$Texture
      
      Bulk_density2 <- as.data.frame(df1[[1]]$`Bulk density`)
      BD1 <- Bulk_density2$`Bulk density`
      
      Particle_density2 <- as.data.frame(df1[[1]]$`Particle density`)
      PD1 <- Particle_density2$`Particle density`
      
      Porosity2 <- as.data.frame(df1[[1]]$`Porosity`)
      TP1 <- Porosity2$`Total porosity`
      VR1 <- Porosity2$`Void ratio`
      
      Water_parameters2 <- as.data.frame(df1[[1]]$`Water parameters`)
      GWC1 <-Water_parameters2$GWC
      VWC1 <-Water_parameters2$VWC
      DSW1 <-Water_parameters2$DSW
      AFP1 <-Water_parameters2$AFP
      WFPS1<-Water_parameters2$WFPS
      DS1<-Water_parameters2$DS
      
      ASD2 <- as.data.frame(df1[[1]]$`Aggregate size distribution`)
      MWD1 <-ASD2$MWD
      GMD1<-ASD2$GMD
     
      Clay_df <-cbind.fill(Clay_df,Clay1)
      Silt_df <-cbind.fill(Silt_df,Silt1)
      Sand_df <-cbind.fill(Sand_df,Sand1)
      Texture_df <-cbind.fill(Texture_df,Texture1)
      BD_df <- cbind.fill(BD_df,BD1)
      PD_df <- cbind.fill(PD_df,PD1)
      TP_df <- cbind.fill(TP_df,TP1)
      VR_df <- cbind.fill(VR_df,VR1)
      GWC_df <- cbind.fill(GWC_df,GWC1)
      VWC_df <- cbind.fill(VWC_df,VWC1)
      DSW_df <- cbind.fill(DSW_df,DSW1)
      AFP_df <- cbind.fill(AFP_df,AFP1)
      WFPS_df <- cbind.fill(WFPS_df,WFPS1)
      DS_df <- cbind.fill(DS_df,DS1)
      MWD_df <- cbind.fill(MWD_df,MWD1)
      GMD_df <- cbind.fill(GMD_df,GMD1)}
    else if (length(df1[[1]]) == 5){
      text_df2 <- as.data.frame(df1[[1]]$`Soil texture`)
      Clay1<- text_df2$Clay
      Silt1<- text_df2$Silt
      Sand1<- text_df2$Sand
      Texture1<- text_df2$Texture
      
      Bulk_density2 <- as.data.frame(df1[[1]]$`Bulk density`)
      BD1 <- Bulk_density2$`Bulk density`
      
      Particle_density2 <- as.data.frame(df1[[1]]$`Particle density`)
      PD1 <- Particle_density2$`Particle density`
      
      Porosity2 <- as.data.frame(df1[[1]]$`Porosity`)
      TP1 <- Porosity2$`Total porosity`
      VR1 <- Porosity2$`Void ratio`
      
      Water_parameters2 <- as.data.frame(df1[[1]]$`Water parameters`)
      GWC1 <-Water_parameters2$GWC
      VWC1 <-Water_parameters2$VWC
      DSW1 <-Water_parameters2$DSW
      AFP1 <-Water_parameters2$AFP
      WFPS1<-Water_parameters2$WFPS
      DS1<-Water_parameters2$DS
      
      Clay_df <-cbind.fill(Clay_df,Clay1)
      Silt_df <-cbind.fill(Silt_df,Silt1)
      Sand_df <-cbind.fill(Sand_df,Sand1)
      Texture_df <-cbind.fill(Texture_df,Texture1)
      BD_df <- cbind.fill(BD_df,BD1)
      PD_df <- cbind.fill(PD_df,PD1)
      TP_df <- cbind.fill(TP_df,TP1)
      VR_df <- cbind.fill(VR_df,VR1)
      GWC_df <- cbind.fill(GWC_df,GWC1)
      VWC_df <- cbind.fill(VWC_df,VWC1)
      DSW_df <- cbind.fill(DSW_df,DSW1)
      AFP_df <- cbind.fill(AFP_df,AFP1)
      WFPS_df <- cbind.fill(WFPS_df,WFPS1)
      DS_df <- cbind.fill(DS_df,DS_df1)}
    else if (length(df1[[1]]) == 4){
      text_df2 <- as.data.frame(df1[[1]]$`Soil texture`)
      Clay1<- text_df2$Clay
      Silt1<- text_df2$Silt
      Sand1<- text_df2$Sand
      Texture1<- text_df2$Texture
      
      Bulk_density2 <- as.data.frame(df1[[1]]$`Bulk density`)
      BD1 <- Bulk_density2$`Bulk density`
      
      Particle_density2 <- as.data.frame(df1[[1]]$`Particle density`)
      PD1 <- Particle_density2$`Particle density`
      
      Porosity2 <- as.data.frame(df1[[1]]$`Porosity`)
      TP1 <- Porosity2$`Total porosity`
      VR1 <- Porosity2$`Void ratio`

      Clay_df <-cbind.fill(Clay_df,Clay1)
      Silt_df <-cbind.fill(Silt_df,Silt1)
      Sand_df <-cbind.fill(Sand_df,Sand1)
      Texture_df <-cbind.fill(Texture_df,Texture1)
      BD_df <- cbind.fill(BD_df,BD1)
      PD_df <- cbind.fill(PD_df,PD1)
      TP_df <- cbind.fill(TP_df,TP1)
      VR_df <- cbind.fill(VR_df,VR1)}
    else if (length(df1[[1]]) == 3){
      text_df2 <- as.data.frame(df1[[1]]$`Soil texture`)
      Clay1<- text_df2$Clay
      Silt1<- text_df2$Silt
      Sand1<- text_df2$Sand
      Texture1<- text_df2$Texture
      
      Bulk_density2 <- as.data.frame(df1[[1]]$`Bulk density`)
      BD1 <- Bulk_density2$`Bulk density`
      
      Particle_density2 <- as.data.frame(df1[[1]]$`Particle density`)
      PD1 <- Particle_density2$`Particle density`

      Clay_df <-cbind.fill(Clay_df,Clay1)
      Silt_df <-cbind.fill(Silt_df,Silt1)
      Sand_df <-cbind.fill(Sand_df,Sand1)
      Texture_df <-cbind.fill(Texture_df,Texture1)
      BD_df <- cbind.fill(BD_df,BD1)
      PD_df <- cbind.fill(PD_df,PD1)}
    else if (length(df1[[1]]) == 2){
      text_df2 <- as.data.frame(df1[[1]]$`Soil texture`)
      Clay1<- text_df2$Clay
      Silt1<- text_df2$Silt
      Sand1<- text_df2$Sand
      Texture1<- text_df2$Texture
      
      Bulk_density2 <- as.data.frame(df1[[1]]$`Bulk density`)
      BD1 <- Bulk_density2$`Bulk density`
      
      Clay_df <-cbind.fill(Clay_df,Clay1)
      Silt_df <-cbind.fill(Silt_df,Silt1)
      Sand_df <-cbind.fill(Sand_df,Sand1)
      Texture_df <-cbind.fill(Texture_df,Texture1)
      BD_df <- cbind.fill(BD_df,BD1)}
    else if (length(df1[[1]]) == 1){
      text_df2 <- as.data.frame(df1[[1]]$`Soil texture`)
      Clay1<- text_df2$Clay
      Silt1<- text_df2$Silt
      Sand1<- text_df2$Sand
      Texture1<- text_df2$Texture
  
      Clay_df <-cbind.fill(Clay_df,Clay1)
      Silt_df <-cbind.fill(Silt_df,Silt1)
      Sand_df <-cbind.fill(Sand_df,Sand1)
      Texture_df <-cbind.fill(Texture_df,Texture1)}}
    
  names_col <-paste0('R',(1:length(sh_name)))
  colnames(Clay_df)<-names_col
  colnames(Silt_df)<-names_col
  colnames(Sand_df)<-names_col
  colnames(Texture_df)<-names_col
  if(nrow(BD_df) != 0){colnames(BD_df)<-names_col}
  if(nrow(PD_df) != 0){colnames(PD_df)<-names_col}
  if(nrow(TP_df) != 0){colnames(TP_df)<-names_col}
  if(nrow(VR_df) != 0){colnames(VR_df)<-names_col}
  if(nrow(GWC_df) != 0){colnames(GWC_df)<-names_col}
  if(nrow(VWC_df) != 0){colnames(VWC_df)<-names_col}
  if(nrow(DSW_df) != 0){colnames(DSW_df)<-names_col}
  if(nrow(AFP_df) != 0){colnames(AFP_df)<-names_col}
  if(nrow(WFPS_df) != 0){colnames(WFPS_df)<-names_col}
  if(nrow(DS_df) != 0){colnames(DS_df)<-names_col}
  if(nrow(MWD_df) != 0){colnames(MWD_df)<-names_col}
  if(nrow(GMD_df) != 0){colnames(GMD_df)<-names_col} 
  if(nrow(SOCS_df) != 0){colnames(SOCS_df)<-names_col}
  if(nrow(SOCSR_df) != 0){colnames(SOCSR_df)<-names_col}
  if(nrow(RLD_df) != 0){colnames(RLD_df)<-names_col}
  if(nrow(RSD_df) != 0){colnames(RSD_df)<-names_col}
  if(nrow(RWD_df) != 0){colnames(RWD_df)<-names_col}
  if(nrow(SBC_df) != 0){colnames(SBC_df)<-names_col}

  trt<-df$Trt 
  Clay_df <-cbind("Clay"=trt,Clay_df)
  Silt_df <-cbind("Silt"=trt,Silt_df)
  Sand_df <-cbind("Sand"=trt,Sand_df)
  Texture_df <-cbind("Texture"=trt,Texture_df)
  BD_df <-cbind("Bulk density"=trt,BD_df)
  PD_df <-cbind("Particle density"=trt,PD_df)
  TP_df <-cbind("Total Porosity"=trt,TP_df)
  VR_df <-cbind("Void ratio"=trt,VR_df)
  GWC_df <-cbind("GWC"=trt,GWC_df)
  VWC_df <-cbind("VWC"=trt,VWC_df)
  DSW_df <-cbind("DSW"=trt,DSW_df)
  AFP_df <-cbind("AFP"=trt,AFP_df)
  WFPS_df <-cbind("WFPS"=trt,WFPS_df)
  DS_df <-cbind("DS"=trt,DS_df)
  MWD_df <-cbind("MWD"=trt,MWD_df)
  GMD_df <-cbind("GMD"=trt,GMD_df)
  SOCS_df <-cbind("SOCS"=trt,SOCS_df)
  SOCSR_df <-cbind("SOCSR"=trt,SOCSR_df)
  RLD_df <-cbind("RLD"=trt,RLD_df)
  RSD_df <-cbind("RSD"=trt,RSD_df)
  RWD_df <-cbind("RWD"=trt,RWD_df)
  SBC_df <-cbind("SBC"=trt,SBC_df)
 
  Final_Texture <- cbind.fill(Clay_df,Silt_df,Sand_df,Texture_df)
  Final_Porosity <- cbind.fill(TP_df,VR_df)
  Final_Water_parameters <-cbind.fill(GWC_df,VWC_df,DSW_df,AFP_df,WFPS_df,DS_df)
  Final_ASD <-cbind.fill(MWD_df,GMD_df)
  Final_OC <-cbind.fill(SOCS_df,SOCSR_df)
  Final_Root <- cbind.fill(RLD_df,RSD_df,RWD_df,SBC_df)
  
  F_list <- list("Soil texture"=Final_Texture,
                 "Bulk density"=BD_df,
                 "Particle density"=PD_df,
                 "Porosity"=Final_Porosity,
                 "Water parameters"=Final_Water_parameters,
                 "Aggregate size distribution"=Final_ASD,
                 "Organic carbon"=Final_OC,
                 "Root parameters"=Final_Root)
  
  # print(res_list)
  path_user <- dirname(fname)
  path_user
  newfilename <- "SoilIndices_Results.xlsx"
  newfilepath <- file.path(path_user, newfilename)
  
  paste0("Saved file path:-  ",newfilepath)
  
  wb = createWorkbook()
  Map(function(data, nameofsheet){     
    addWorksheet(wb, nameofsheet)
    writeData(wb, nameofsheet, data,rowNames=TRUE)
  }, F_list, names(F_list))
  
  saveWorkbook(wb, file = newfilepath, overwrite = TRUE)
  return(F_list);

}
 
