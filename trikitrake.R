#creacion de un triqui
#Brahian Cano Urrego
#Universidad Nacional de Colombia
#Modulo[R]
#fecha:10/04/2016
#--------------------------------------------------------------------------------------------
#invoca a las funciones necesarias------------------------------------------------------------

source("funcion.R")
source("funcion2.R")
source("funcion3.R")
#crea una matriz para el proximo proceso de seleccion de ganador y pide 

#las fichas para los jugador----------------------------------------------------------------
m=matrix(6,nrow=3,ncol=3)
lista1=list()
lista2=list()
j1=as.numeric(readline(prompt = "jugador 1,selecciona equis(1) o circulo(0,cero):"))
if(j1==1){
  j2=0
  f1=4
  f2=1
}else{
  j2=1
  f1=1
  f2=4
}
#contador, boleano para los calculos proximos-------------------------------------------------

x=FALSE
i=2

# Establece las coordenadas del plano cartesiano del triqui-------------------------------------

plot(c(0,4),c(0,4), type = "n", axes = FALSE,
     ylab = "", xlab = "", asp = 1)
segments(c(0,0,1,2),c(1,2,0,0),c(3,3,1,2),c(1,2,3,3))

#crea el ciclo que se detiene cuando un jugador gana "x=TRUE" o pasan 9 turnos-----------------

while(x==FALSE & i<11){
  y=i%%2
  rect(3,2,5,3,col="white",border="white")
  text(4,2.5,paste("Turno: jugador",y+1),cex=0.8)
  
  #se discrimina segun el turno de jugador,luego se pide un lugar con la pulsacion y
  #se grafica la ficha seleccionada,y al final se le asigna un valor en la matriz
  #segun el jugador---------------------------------------------------------------------------
  
  if(i%%2==1){
    lugar <- locator(n=1)
    for(h in 1:3){
      if(lugar[[1]]>h-1 & lugar[[1]]<=h){
        lugar[1]=0.5+(h-1)
        for(j in 1:3){
          if(lugar[[2]]<=j & lugar[[2]]>j-1){
            lugar[2]=0.5+(j-1)
            points(lugar, pch = f2, col = "blue", cex = 6,lwd=3.5)
            m[4-h,4-j]=j2
            x=gano()
          }
        }
      }
    }
   
    
  }else{
    lugar <- locator(n=1)
    for(h in 1:3){
      if(lugar[[1]]>h-1 & lugar[[1]]<=h){
        lugar[1]=0.5+(h-1)
        for(j in 1:3){
          if(lugar[[2]]<=j & lugar[[2]]>j-1){
            lugar[2]=0.5+(j-1)
            points(lugar, pch = f1, col = "red", cex = 6,lwd=3.5)
            
            
            
            m[4-h,4-j]=j1
            x=gano()                   
          }
        }
      }
    }
  }
  
#si alguien gano,modifica la matriz original para poder hacer una correcta graficacion
#de linea luego--------------------------------------------------------------------------  
  
i=i+1
if(x==TRUE){
  rect(3,2,5,3,col="white",border="white")
  text(1.5,3.5,paste("FELICIDADES TIO, JODER JUGADOR",y+1,"HAS GANADO"),cex=0.8)
   ma=t(m)
   col1=ma[1:3,1]
   col3=ma[1:3,3]
   ma[1:3,1]=col3
   ma[1:3,3]=col1
   fil1=ma[1,1:3]
   fil3=ma[3,1:3]
   ma[1,1:3]=fil3
   ma[3,1:3]=fil1
   
   #esta es la funcion q detecta quien gana y grafica la linea------------------------
   
   gano2()
} 
}

#si hubo un empate,tambien se modifica la matriz-----------------------------------------

if(x==FALSE){
  Y=FALSE
  ma=t(m)
  col1=ma[1:3,1]
  col3=ma[1:3,3]
  ma[1:3,1]=col3
  ma[1:3,3]=col1
  fil1=ma[1,1:3]
  fil3=ma[3,1:3]
  ma[1,1:3]=fil3
  ma[3,1:3]=fil1
  
  #mientras que nadie gane se indicaran turnos segun un piedra,papel,tijerazo------------------
  
  while(Y==FALSE){

  text(1.5,3.5,paste("DUELO A MUERTE","AHORA SE REEMPLAZAN LAS FICHAS"),cex=0.8)
  Sys.sleep(2.5)
  rect(-1,3,4,4,col="white",border="white")
  
  #funcion para el piedra papel tijerazo,ademas nos indica la figura,turno y el 
  #jugador que le toca el turno-------------------------------------------------
  
  jugador=gano3()
  
  #pide un lugar de pulsacion,borra la anterior figura,grafica su debida figura en color amarillo
  #y sigue introduciendo datos al matriz-----------------------------------------------------
  
  rect(3,2,5,3,col="white",border="white")
  text(4,2.5,paste("Turno: jugador",jugador[2]),cex=0.8)
  lugar1=locator(n=1)
  for(h in 1:3){
    if(lugar1[[1]]>h-1 & lugar1[[1]]<=h){
      
      lugar1$x=0.2+(h-1)
      for(j in 1:3){
        if(lugar1[[2]]<=j & lugar1[[2]]>j-1){
          lugar1$y=0.2+(j-1)
          cuadro=c(lugar1$x,lugar1$y,(lugar1$x+0.6),lugar1$y+0.6)
          rect(cuadro[1],cuadro[2],cuadro[3],cuadro[4],col="white",border="white")
          lugar1$x=lugar1$x+0.3
          lugar1$y=lugar1$y+0.3
          #animacion--------------------------------------------------------------------------
          lim=lugar1$x+0.5
          lugar1$x=3.5
          ma[j,h]=jugador[3]
          if(jugador[1]==4 ){
            jugador[1]=3
            
          }
          points(lugar1, pch = jugador[1], col ="yellow3", cex = 4,lwd=3.5)
                  
                }
              }
            }
  }
  #si la figura es una equis, se abanzara con una cruz y al final volvera a equis--------------
  for(i in 1:(8-2*lim)){
    points(lugar1, pch = jugador[1], col ="white", cex = 4,lwd=3.5)
    if(i==(8-2*lim) & jugador[1]==3){
      jugador[1]=4
    }
    lugar1$x= lugar1$x -0.5
    points(lugar1, pch = jugador[1], col ="yellow3", cex = 4,lwd=3.5)
    segments(c(0,0,1,2),c(1,2,0,0),c(3,3,1,2),c(1,2,3,3))
    
    Sys.sleep(1)
  }
  
#indica si alguien ha ganado para poder romper el ciclo--------------------------------------
  
Y=gano2()
        
  }
  
  #felicita a la persona inteligente o suertuda que logro ganar----------------------------
  
  text(1.5,3.5,paste("FELICIDADES TIO, JODER JUGADOR",jugador[2],"HAS GANADO"),cex=0.8)
  rect(3,2,5,3,col="white",border="white")
}
    
  
  











