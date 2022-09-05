# SODA - SPATIAL OUTLIER DETECTION ALGORITHM

  ### Setting the data

  setwd("....")
  
  getwd() 

  ### Packages 

  if(require("tcltk2")){
    print("tcltk2 is loaded correctly")
  } else {
    print("trying to install tcltk2")
    install.packages("tcltk2")
    if(require(tcltk2)){
      print("tcltk2 installed and loaded")
    } else {
      stop("could not install tcltk2")
    }
  }
  
  if(require("geoR")){
    print("geoR is loaded correctly")
  } else {
    print("trying to install geoR")
    install.packages("geoR")
    if(require(geoR)){
      print("geoR installed and loaded")
    } else {
      stop("could not install geoR")
    }
  }
  
  if(require("rgdal")){
    print("rgdal is loaded correctly")
  } else {
    print("trying to install rgdal")
    install.packages("rgdal")
    if(require(rgdal)){
      print("rgdal installed and loaded")
    } else {
      stop("could not install rgdal")
    }
  }
  
  if(require("rgeos")){
    print("rgeos is loaded correctly")
  } else {
    print("trying to install rgeos")
    install.packages("rgeos")
    if(require(rgeos)){
      print("rgeos installed and loaded")
    } else {
      stop("could not install rgeos")
    }
  }
  
  if(require("plyr")){
    print("plyr is loaded correctly")
  } else {
    print("trying to install plyr")
    install.packages("plyr")
    if(require(plyr)){
      print("plyr installed and loaded")
    } else {
      stop("could not install plyr")
    }
  }
  
  if(require("raster")){
    print("raster is loaded correctly")
  } else {
    print("trying to install raster")
    install.packages("raster")
    if(require(raster)){
      print("raster installed and loaded")
    } else {
      stop("could not install raster")
    }
  }
  
  if(require("ggplot2")){
    print("ggplot2 is loaded correctly")
  } else {
    print("trying to install ggplot2")
    install.packages("ggplot2")
    if(require(ggplot2)){
      print("ggplot2 installed and loaded")
    } else {
      stop("could not install ggplot2")
    }
  }
  
  if(require("moments")){
    print("moments is loaded correctly")
  } else {
    print("trying to install moments")
    install.packages("moments")
    if(require(moments)){
      print("moments installed and loaded")
    } else {
      stop("could not install moments")
    }
  }
  
  if(require("robustbase")){
    print("robustbase is loaded correctly")
  } else {
    print("trying to install robustbase")
    install.packages("robustbase")
    if(require(robustbase)){
      print("robustbase installed and loaded")
    } else {
      stop("could not install robustbase")
    }
  }
  
  ### List of packages

  pkg <- c("geoR","moments","rgeos","tcltk2",
           "raster", "rgdal", "ggplot2","plyr", "robustbase")
  
  sapply(pkg, require, character.only=TRUE)

  ### Reading the data
  
  ### shp format

  options(digits = 12)
  library(rgdal)
  data.shape<-readOGR(dsn=",",layer="xyzest.shp")
  summary(data.shape)

  ### txt format

  txt <- read.table("XYZest.txt", sep=";", dec=".", header = TRUE)
  coordinates(txt) <- ~X+Y
  #Definido sistema de proje??o do arquivo txt
  proj4string(txt) = CRS("+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs")
  

  aux<-data.shape@data
  
  ### Analysis performed on the text file:

  aux<-txt
  
  ### Generate file for independence analysis

  write.table(aux, "data_para_semivariogram.txt", dec=",")
  
  
  ### Data reading for independence analysis

  data <- read.geodata("data_para_semivariogram.txt", header=T, dec=",",coords=1:2, data.col=3)
  names(data)
  data
  
  # Exploratory analysis

  (res=summary(data))
  
  attach(res)

  ### Main measures

  (med = round(mean(aux$Z_est),3))
  (min = round(min(aux$Z_est),3))
  (max = round(max(aux$Z_est),3))
  (des = round(sd(aux$Z_est),4))
  (var = round(var(aux$Z_est),4))
  (CV = round(100*sd(aux$Z_est)/mean(aux$Z_est),2))
  (curt = round(kurtosis(aux$Z_est),2))
  (assim = round(skewness(aux$Z_est),2))
  (n=length(aux$Z_est))
  (dist.min= round((distances.summary[1]),3))
  (dist.max= round((distances.summary[2]),3))

  ### Exporting information:

  sink("Resultados.txt", type="output", append=T)
  cat("##### Doctoral thesis #####\n Professor Italo O. Ferreira \n italo.ferreira@ufv.br\n\n AEDO Methodology - Effective Algorithm for Outlier Detection
      \n Exploratory Data Analysis:","\n",
      "------------------------------------------------------","\n",
      n, "observations"    ,"\n",
      "Mean:"      ,med,'metros'  ,"\n",
      "Minimum:"  ,min,"metros"  ,"\n",
      "Maximum:"     ,max,"metros"  ,"\n",
      "variance:"  ,var,"metros?"  ,"\n",
      "Standard deviation:"     ,des,"metros"  ,"\n",
      "CV:"         ,CV,"%"  ,"\n",
      "Coef. of kurtosis: "         ,curt    ,"\n",
      "Coef. of asymmetry:"      ,assim  ,"\n",
      "dist.min:"  ,dist.min,"metros"  ,"\n",
      "dist.max:" ,dist.max,"metros" ,"\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Resultados.txt")

  ### Graphs for exploratory analysis

  windows(8,6,title="Graphs for exploratory analysis")
  par(mfrow=c(2,2), family="serif")
  hist(aux$Z_est, xlab="Depths (m)", ylab= "Frequency", main="Histogram")
  plot(density(aux$Z_est), xlab="Depths (m)", ylab= "Frequency", main=" Density")
  boxplot(aux$Z_est, ylab= "Depths (m)", main="Boxplot (Tukey)")
  qqnorm(aux$Z_est, xlab="Theoretical Quantiles", ylab= "Sampled Quantities", main="Normal Q-Q Plot")
  qqline(aux$Z_est,lty=2, col='red')
  par(mfrow=c(1,1), family="serif")
  
  windows(8,6,canvas="snow2",title="Depths (m)")
  ggplot(aux, aes(x = X, y = Y, colour = Z)) + geom_point()+
    xlab("E (m)") + ylab("N (m)") + ggtitle("Study area") +
    theme_bw()+theme(plot.title = element_text(hjust = 0.5))

 
  # Independence Analysis (semivariogram)
 

  ## Empirical semivariogram 

  ### Construction of 4 semivariograms:

  #1 - with range equal to 100% of the maximum distance
  #2 - with range equal to 75% of the maximum distance
  #3 - with range equal to 50% of the maximum distance
  #4 - with range equal to 25% of maximum distance
  
  windows(8,6,title="Omnidirectional Semivariogram")
  escala.y=2*var
  
  par(mfrow=c(2,2), family="serif")
  vario.emp.1 <- variog(data,max.dist=(dist.max), direction="omnidirectional")
  plot(vario.emp.1,ylim=c(0,escala.y),xlab="Distancies (m)",ylab="Semivariance (m?)", main=("100% of the Maximum Distance"))
  abline(var(data$data),0, col="gray60", lty=2, lwd=2)
  legend("topleft","Sample Variance", col="gray60",lty=2, lwd=2,bty='n')
  
  vario.emp.1 <- variog(data,max.dist=(0.75*dist.max), direction="omnidirectional")
  plot(vario.emp.1,ylim=c(0,escala.y),xlab="Distancies (m)",ylab="Semivariance (m?)", main=("75% of the Maximum Distance"))
  abline(var(data$data),0, col="gray60", lty=2, lwd=2)
  legend("topleft","Sample Variance", col="gray60",lty=2, lwd=2,bty='n')
  
  vario.emp.1 <- variog(data,max.dist=(0.50*dist.max), direction="omnidirectional")
  plot(vario.emp.1,ylim=c(0,escala.y),xlab="Distancies (m)",ylab="Semivariance (m?)", main=("50% of the Maximum Distance"))
  abline(var(data$data),0, col="gray60", lty=2, lwd=2)
  legend("topleft","Sample Variance", col="gray60",lty=2, lwd=2,bty='n')
  
  vario.emp.1 <- variog(data,max.dist=(0.25*dist.max), direction="omnidirectional")
  plot(vario.emp.1,ylim=c(0,escala.y),xlab="Distancies (m)",ylab="Semivariance (m?)", main=("25% of the Maximum Distance"))
  abline(var(data$data),0, col="gray60", lty=2, lwd=2)
  legend("topleft","Sample Variance", col="gray60",lty=2, lwd=2,bty='n')
  par(mfrow=c(1,1), family="serif")
  
  dist.max
  0.75*dist.max
  0.50*dist.max
  0.25*dist.max
  
  # Omnidirectional Semivariogram of Discrepancies/Monte Carlo Envelope
  
  M<-(0.25*dist.max) #Semivariogram of discrepancies for distance from M m.
  
  windows(8,6,title="Omnidirectional Semivariogram")
  par(mfrow=c(1,1), family="serif")
  vario.emp.1 <- variog(data,max.dist= M, direction="omnidirectional")
  plot(vario.emp.1, ylim=c(0,escala.y),xlab="Distancies (m)",ylab="Semivariance (m?)", main=("Depth Semivariogram")) 
  abline(var(data$data),0, col="gray60", lty=2, lwd=2)
  legend("topleft","Sample Variance", col="gray60",lty=2, lwd=2,bty='n')

  vario.env <- variog.mc.env(data, obj.v=vario.emp.1)
  plot(vario.emp.1, env=vario.env,ylim=c(0,escala.y),xlab="Distancies (m)",ylab="Semivariance (m?)", main=("Depth Semivariogram")) 
  abline(var(data$data),0, col="gray60", lty=2, lwd=2)
  legend("topleft","Sample Variance", col="gray60",lty=2, lwd=2,bty='n')
  
  
  ### Exporting information:

  sink("Resultados.txt", type="output", append=T)
  cat("Semivariogram calculation results:","\n",
      "------------------------------------------------------","\n",
      vario.emp.1$n.data, "observa??es"    ,"\n",
      "Disatncies:"      , vario.emp.1$u  ,"\n",
      "Semivariance:"  ,vario.emp.1$v  ,"\n",
      "Number of pairs in each lot:"     ,vario.emp.1$n  ,"\n",
      "Standard deviation of each lot:"  ,vario.emp.1$sd,"\n",
      "Maximum distance:"     ,vario.emp.1$max.dist,"\n",
      "Direction:"         ,vario.emp.1$direction  ,"\n",
      "------------------------------------------------------","\n",
      fill=F)
  sink()
  shell.exec("Resultados.txt")
 
  ###########################################################################
  ###Independent sample, continue analysis...
  ###Dependent sample, perform statistical analysis and work with RPs. 
  ###########################################################################
  
-----------------------------------------------------------------------------

  # Search Radius Determination: 3x the minimum distance

  ############################################################################
  ### Attention!
  ############################################################################

  ### Analysis performed on the shp file

  aux1<-data.shape
  
  ### Analysis performed on the text file

  aux1<-txt
  
  ### Radius of each buffer

  dist <- gDistance(aux1, byid=T) # apply to all points
  dist[which(dist==0)] <- NA 
  raio <- 3* mean (apply(dist,1, min, na.rm=T)) 
  #raio <- 3* dist.min # geostatistical analysis
  raio

  ### User-entered radius

  raio  <- as.numeric(readline("Inform the value of the radius that will be used to analyze the data:"))
  raio
  
  ### Delta method constant:

  #1 for uneven reliefs or artificial channels (high variability)
  #2 for undulating reliefs (medium variability)
  #3 for flat reliefs (low variability)
  
  c <- 1 
  
  ### Apply outlier detection techniques

  out_box_ALL <- out_MAD_ALL<- out_sd_ALL<- NULL
  count_ptsInBuffer <- NULL
  aa <- Sys.time()

  for (i in 1:dim(aux1)[1])
  {
    ptsInBuffer <- NULL
    out_box <- out_MAD <- out_sd<-NULL
    bufferedPoints <- gBuffer(aux1[i,],width=raio,byid=TRUE)
    bufPolygons <- bufferedPoints@polygons
    bufSpPolygons <- SpatialPolygons((bufPolygons))
     plot(bufSpPolygons)
     points(aux1)
    bufSpPolygonDf <-SpatialPolygonsDataFrame(bufSpPolygons,bufferedPoints@data)
    crs(bufSpPolygonDf) <- crs(aux1)
    ptsInBuffer <- which(gIntersects(aux1, bufSpPolygonDf, byid=TRUE)==TRUE)
    count_ptsInBuffer <- c(ptsInBuffer, count_ptsInBuffer)
    #Minimum number to perform outlier analysis
    if (length(ptsInBuffer)>7)
    {
        
        print(i)
      
      # ======= ADJUSTED BOXPLOT =========

      #outlier identification for each buffer by the ADJUSTED BOXPLOT
    
      out_box <- ptsInBuffer[which(aux1@data$Z[ptsInBuffer]==
                                     unique(adjbox(aux1@data$Z[ptsInBuffer], plot=FALSE)$out))]
      if (sum(!is.na(out_box)) != 0) {out_box_ALL <-  c(out_box_ALL, out_box)}

      ### End analysis by adjusted boxplot
            
      # ======= MODIFIED Z-SCORE =========
      
      ### outlier identification for each buffer by the MODIFIED Z-SCORE

      dz <- aux1@data$Z[ptsInBuffer]

      ZSM = abs((0.6745*(dz-median(dz)))/(mad(dz,constant = 1)))
      
      treshold <- 3.5

      out_MAD <- ptsInBuffer[(ZSM > treshold)]
      
      if (sum(!is.na(out_MAD)) != 0) 
      {out_MAD_ALL <-  c(out_MAD_ALL, out_MAD)}
      #Fim an?lise pelo z-score modificado

      # ======= Delta method =========
      
      if (mad(aux1@data$Z) > mad(aux1@data$Z[ptsInBuffer]))
      {       
        lim <- 0.5*(mad(aux1@data$Z)+mad(aux1@data$Z[ptsInBuffer]))
      }
      if (mad(aux1@data$Z) <= mad(aux1@data$Z[ptsInBuffer]))
      {      
        lim <- mad(aux1@data$Z)
      }
      
      treshold_low <-  median(aux1@data$Z[ptsInBuffer])- c * lim   # lower threshold
      treshold_high <-  median(aux1@data$Z[ptsInBuffer])+c * lim   # upper threshold
      
      out_sd <- ptsInBuffer[(aux1@data$Z[ptsInBuffer] < treshold_low)
                            | (aux1@data$Z[ptsInBuffer] > treshold_high)]
      
      if (sum(!is.na(out_sd)) != 0) 
      {out_sd_ALL <-  c(out_sd_ALL, out_sd)}
      
    }    
  }

  Sys.time() - aa
  
  ### Analyze and locate outliers
  
  # ======= AJUSTED BOXPLOT =========

  if (sum(!is.na(out_box_ALL)) != 0)
  {
      n_ocorrencia_BOX <- merge.data.frame(count(count_ptsInBuffer),count(out_box_ALL), by = c("x","x"))
      n_ocorrencia_BOX <- cbind(n_ocorrencia_BOX, round(n_ocorrencia_BOX[,3]/n_ocorrencia_BOX[,2],2))
      colnames(n_ocorrencia_BOX) <- c("ponto","freq_lido", "freq_out", "percentual")  # applying name to table attributes 
      
      #Probability of the data being a spike 

      p <- 0.5 
        
      pts_eliminados <- n_ocorrencia_BOX$ponto[which(n_ocorrencia_BOX$percentual>=p)]
      aux1_BOX <- aux1[-pts_eliminados,]
      length(n_ocorrencia_BOX$ponto[which(n_ocorrencia_BOX$percentual>=p)])
      
  }
  
  if (sum(!is.na(out_box_ALL)) == 0)
  {aux1_BOX <- aux1}
  
  windows(8,6,title="Spikes detected by the Adjusted Boxplot")
  par(mfrow=c(1,1), family="serif")
  #Plot data base without outlier
  plot(aux1_BOX, pch=3, col=4,
       xlab="E (m)", ylab= "N (m)", main="Spikes detected by the Adjusted Boxplot") 
  #Plot the outliers
  points(aux1[pts_eliminados,], pch=19, col=2)
  legend('bottomleft',legend=c('Bathymetric Points','Spikes'),col=c(4, 2),pch=c(3,19))
  
  # ======= MODIFIED Z-SCORE =========
  
  if (sum(!is.na(out_MAD_ALL)) != 0)
  {
      n_ocorrencia_MAD <- merge.data.frame(count(count_ptsInBuffer),count(out_MAD_ALL), by = c("x","x"))
      n_ocorrencia_MAD <- cbind(n_ocorrencia_MAD, round(n_ocorrencia_MAD[,3]/n_ocorrencia_MAD[,2],2))
      colnames(n_ocorrencia_MAD) <- c("ponto","freq_lido", "freq_out", "percentual")  # applying name to table attributes 
      
      #Probability of the data being a spike 
 
      p <- 0.8
      
      pts_eliminados <- n_ocorrencia_MAD$ponto[which(n_ocorrencia_MAD$percentual>=p)]
      aux1_MAD <- aux1[-pts_eliminados,]
      length(n_ocorrencia_MAD$ponto[which(n_ocorrencia_MAD$percentual>=p)])
      
  }
  
  if (sum(!is.na(out_MAD_ALL)) == 0)
  {aux1_MAD <- aux1}
  
  windows(8,6,title="Spikes Detected by Modified Z-Score")
  par(mfrow=c(1,1), family="serif")
  #Plot data base without outlier
  plot(aux1_MAD, pch=3, col=4,
       xlab="E (m)", ylab= "N (m)", main="Spikes Detected by Modified Z-Score")   
  #Plot the outliers
  points(aux1[pts_eliminados,], pch=19, col=2)    
  legend('bottomleft',legend=c('Bathymetric Points','Spikes'),col=c(4, 2),pch=c(3,19))
  
  # ======= DELTA METHOD =========
  
  if (sum(!is.na(out_sd_ALL)) != 0)
  {
      n_ocorrencia_sd <- merge.data.frame(count(count_ptsInBuffer),count(out_sd_ALL), by = c("x","x"))
      n_ocorrencia_sd <- cbind(n_ocorrencia_sd, round(n_ocorrencia_sd[,3]/n_ocorrencia_sd[,2],3))
      colnames(n_ocorrencia_sd) <- c("ponto","freq_lido", "freq_out", "percentual")  # applying name to table attributes 
      
      #Probability of the data being a spike 

      p <- 0.5 
      
      pts_eliminados <- n_ocorrencia_sd$ponto[which(n_ocorrencia_sd$percentual>=p)]
      aux1_sd <- aux1[-pts_eliminados,]
      length(n_ocorrencia_sd$ponto[which(n_ocorrencia_sd$percentual>=p)])
    
  }
  
  if (sum(!is.na(out_sd_ALL)) == 0)
  {aux1_sd <- aux1}
  
  windows(8,6,title="Spikes Detected by The Delta Method")
  par(mfrow=c(1,1), family="serif")
  #Plot data base without outlier
  plot(aux1_sd, pch=3, col=4,
       xlab="E (m)", ylab= "N (m)", main="Spikes Detected by The Delta Method") 
  #Plot the outliers
  points(aux1[pts_eliminados,], pch=19, col=2)    
  legend('bottomleft',legend=c('Bathymetric Points','Spikes'),col=c(4, 2),pch=c(3,19))
  
 
  ### Save tables for analysis

  write.table(n_ocorrencia_BOX, "Tab_boxplot_Ajustado.txt", dec=",")
  write.table(n_ocorrencia_MAD, "Tab_Z_Scores.txt", dec=",")
  write.table(n_ocorrencia_sd, "Tab_3_Delta.txt", dec=",")
  
  ### Comparison of the agreement between the spikes detected by each threshold

  Repetidos <- function(z,b)
  {BZ <- unique( c(z, b))
  return(((abs(length (BZ) - length(c(z, b))))/ (min (length(z), length(b))))*100)
  }  
 
  
  ### Adjusted Boxplot and Modified Z-Score

  ### General

  Repetidos(n_ocorrencia_BOX$ponto, n_ocorrencia_MAD$ponto)
  
  ### Refined

  Repetidos(n_ocorrencia_BOX$ponto[which(n_ocorrencia_BOX$percentual>=0.5)], 
            n_ocorrencia_MAD$ponto[which(n_ocorrencia_MAD$percentual>=0.8)])
  
  ### Adjusted and Delta boxplot

  ### General

  Repetidos(n_ocorrencia_BOX$ponto, n_ocorrencia_sd$ponto)
  
  ### Refined

  Repetidos(n_ocorrencia_BOX$ponto[which(n_ocorrencia_BOX$percentual>=0.5)], 
            n_ocorrencia_sd$ponto[which(n_ocorrencia_sd$percentual>=0.5)])
  
  ### Delta and Modified Z-Score

  ### General

  Repetidos(n_ocorrencia_sd$ponto, n_ocorrencia_MAD$ponto)
  
  ### Refined

  Repetidos(n_ocorrencia_sd$ponto[which(n_ocorrencia_sd$percentual>=0.5)], 
            n_ocorrencia_MAD$ponto[which(n_ocorrencia_MAD$percentual>=0.8)])
  
  ### Generate dps file without outliers in txt format (X, Y, Z, dz)
 
  write.table(aux1_BOX, "data_semout_boxplot_ajustado.txt", dec=",")
  
  write.table(aux1_MAD, "data_semout_ZSM.txt", dec=",")
  
  write.table(aux1_sd, "data_semout_delta.txt", dec=",")
  
  
  ### Generate dps file without outliers in shp format (X, Y, Z, dz) 

  writeOGR(aux1_BOX, dsn=".", layer="data_semout_boxplot_ajustado", driver="ESRI Shapefile")
  
  writeOGR(aux1_MAD, dsn=".", layer="data_semout_ZSM", driver="ESRI Shapefile")
  
  writeOGR(aux1_sd, dsn=".", layer="data_semout_delta", driver="ESRI Shapefile")
  
  
  
