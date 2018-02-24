#funcion auxiliar
#Brahian Cano Urrego
#Universidad Nacional de Colombia
#Modulo[R]
#fecha:10/04/2016
#--------------------------------------------------------------------------------------------
gano2=function(x){
  ganar=FALSE
#coje cada posible forma de ganar,luego indica que alguien gano y grafica una linea animada
#la cual cruza la forma ganadora
  
  suma3=sum(ma[1,3],ma[2,2],ma[3,1])
  suma2=sum(ma[1,1],ma[2,2],ma[3,3])
  #----------------------------------------------------------------------------------------
  if(suma3==0 | suma3==3){
    ganar=TRUE
    figura1 <- cbind(c(3,2.5),c(0,0.5))
    lines(figura1, col = "darkgoldenrod2"  ,lwd=1.8)
    
    for(i in 1:5){
      
      lines(figura1, col = "white", lwd = 1.8)
      figura1[2,1] <- figura1[2,1]-0.5
      figura1[2,2] <- figura1[2,2]+0.5
      lines(figura1, col = "darkgoldenrod2" ,lwd=1.8)
      Sys.sleep(0.5)
    }
    
  }
  #----------------------------------------------------------------------------------------
  if(suma2==0 | suma2==3){
    ganar=TRUE
    figura1 <- cbind(c(0,0.5),c(0,0.5))
    lines(figura1, col = "darkgoldenrod2"  ,lwd=1.8)
    
    for(i in 1:5){
      
      lines(figura1, col = "white", lwd = 1.8)
      figura1[2,1] <- figura1[2,1]+0.5
      figura1[2,2] <- figura1[2,2]+0.5
      lines(figura1, col = "darkgoldenrod2" ,lwd=1.8)
      Sys.sleep(0.5)
    }
    }
  #----------------------------------------------------------------------------------------
  if(sum(ma[1,1],ma[1,2],ma[1,3])==0 | sum(ma[1,1],ma[1,2],ma[1,3])==3 ){
    ganar=TRUE
    figura1 <- cbind(c(0,0.5),c(0.5,0.5))
    lines(figura1, col = "darkgoldenrod2"  ,lwd=1.8)
    
    for(i in 1:5){
      
      lines(figura1, col = "white", lwd = 1.8)
      figura1[2,1] <- figura1[2,1]+0.5
      lines(figura1, col = "darkgoldenrod2" ,lwd=1.8)
      Sys.sleep(0.5)
    }
  }
  #----------------------------------------------------------------------------------------
  if(sum(ma[2,1],ma[2,2],ma[2,3])==0 | sum(ma[2,1],ma[2,2],ma[2,3])==3){
    ganar=TRUE
    figura1 <- cbind(c(0,0.5),c(1.5,1.5))
    lines(figura1, col = "darkgoldenrod2"  ,lwd=1.8)
    
    for(i in 1:5){
      
      lines(figura1, col = "white", lwd = 1.8)
      figura1[2,1] <- figura1[2,1]+0.5
      lines(figura1, col = "darkgoldenrod2" ,lwd=1.8)
      Sys.sleep(0.5)
    }
  }
  #----------------------------------------------------------------------------------------
  if(sum(ma[3,1],ma[3,2],ma[3,3])==0 | sum(ma[3,1],ma[3,2],ma[3,3])==3){
    ganar=TRUE
    figura1 <- cbind(c(0,0.5),c(2.5,2.5))
    lines(figura1, col = "darkgoldenrod2"  ,lwd=1.8)
    
    for(i in 1:5){
      
      lines(figura1, col = "white", lwd = 1.8)
      figura1[2,1] <- figura1[2,1]+0.5
      lines(figura1, col = "darkgoldenrod2" ,lwd=1.8)
      Sys.sleep(0.5)
    }
  }
  #----------------------------------------------------------------------------------------
  if(sum(ma[1,1],ma[2,1],ma[3,1])==0 | sum(ma[1,1],ma[2,1],ma[3,1])==3 ){
    ganar=TRUE
    figura1 <- cbind(c(0.5,0.5),c(3,2.5))
    lines(figura1, col = "darkgoldenrod2"  ,lwd=1.8)
    
    for(i in 1:5){
      
      lines(figura1, col = "white", lwd = 1.8)
      figura1[2,2] <- figura1[2,2]-0.5
      lines(figura1, col = "darkgoldenrod2" ,lwd=1.8)
      Sys.sleep(0.5)
    }
  }
  #----------------------------------------------------------------------------------------
  if(sum(ma[1,2],ma[2,2],ma[3,2])==0 | sum(ma[1,2],ma[2,2],ma[3,2])==3){
    ganar=TRUE
    figura1 <- cbind(c(1.5,1.5),c(3,2.5))
    lines(figura1, col = "darkgoldenrod2"  ,lwd=1.8)
    
    for(i in 1:5){
      
      lines(figura1, col = "white", lwd = 1.8)
      figura1[2,2] <- figura1[2,2]-0.5
      lines(figura1, col = "darkgoldenrod2" ,lwd=1.8)
      Sys.sleep(0.5)
    }
  }
  #----------------------------------------------------------------------------------------
  if(sum(ma[1,3],ma[2,3],ma[3,3])==0 | sum(ma[1,3],ma[2,3],ma[3,3])==3){
    ganar=TRUE
    figura1 <- cbind(c(2.5,2.5),c(3,2.5))
    lines(figura1, col = "darkgoldenrod2"  ,lwd=1.8)
    
    for(i in 1:5){
      
      lines(figura1, col = "white", lwd = 1.8)
      figura1[2,2] <- figura1[2,2]-0.5
      lines(figura1, col = "darkgoldenrod2" ,lwd=1.8)
      Sys.sleep(0.5)
    }
  }
  return(ganar)
}


