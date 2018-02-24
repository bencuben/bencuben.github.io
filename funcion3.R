#funcion auxiliar
#Brahian Cano Urrego
#Universidad Nacional de Colombia
#Modulo[R]
#fecha:10/04/2016
#--------------------------------------------------------------------------------------------

#para el empate se hace un piedra papel tijerazo, para ver a q jugador le toca el turno

gano3=function(x){
  ppt=c("piedra","papel","tijera")
  ganador=FALSE
  while(ganador==FALSE){
    
    #eleccion aleatoria de objeto------------------------------------------------------------
    
    n1=sample(1:3,1)
    n2=sample(1:3,1)
    print(paste("jugador 1:",ppt[n1]))
    print(paste("jugador 2:",ppt[n2]))
    
    #empieza a reconocer las formas de ganar y si entra al condicional
    #luego indica la figura,el jugador ganador y el valor de la matriz del jugador ganador---
    
    if(n1==n2){
      print("empate,repetir piedra,papel tijera")
    }
    if((n1==1 & n2==2) |(n1==2 & n2==1) ){
      if(n1==2){
        jugador=f1
        ganador=1
        turno=j1
      }else{
        jugador=f2
        ganador=2
        turno=j2
      }
    }
    #----------------------------------------------------------------------------------------
    if((n1==2 & n2==3) |(n1==3 & n2==2) ){
      if(n1==3){
        jugador=f1
        ganador=1
        turno=j1
      }else{
        jugador=f2
        ganador=2
        turno=j2
      }
    }
    #----------------------------------------------------------------------------------------
    if((n1==3 & n2==1) |(n1==1 & n2==3) ){
      if(n1==1){
        jugador=f1
        ganador=1
        turno=j1
      }else{
        jugador=f2
        ganador=2
        turno=j2
      }
    }
  }
  return(c(jugador,ganador,turno))
}
