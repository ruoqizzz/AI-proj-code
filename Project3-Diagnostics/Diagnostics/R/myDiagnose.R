
mydiagnose <- function(network, cases){
  U = c("Pn","TB","LC","Br")

  N = 1000

  estimates = matrix(0,nrow=nrow(cases),ncol=length(U))
  for (r in 1:nrow(cases)) {
    samples = data.frame(Pn = rep(0,N), Te = rep(0,N), VTB = rep(0,N), TB = rep(0,N), Sm = rep(0,N),
                         LC = rep(0,N), Br = rep(0,N), XR = rep(0,N), Dy = rep(0,N))
    samples[1,] = cases[x,]
    samples$Pn[1] = sample(0:1,1)
    samples$TB[1] = sample(0:1,1)
    samples$LC[1] = sample(0:1,1)
    samples$Br[1] = sample(0:1,1)

    for (i in 2:N) {
      temp = samples[i-1,]
      probT = calProb(temp, network)
      for (u in U) {
        temp[[u]] = 1
      }else{
        temp[[u]] = 0
      }

    }
  return(estimates)
}

calProb <- function(s,network){
  # P = P(Pn)*P(Sm)*P(VTB)
  #     *P(Te|Pn)*P(TB|VTB)*P(LC|Sm)*P(Br|Sm)
  #     *P(Dy|LC,Br)*P(XR|Pn,TB,LC)
  Pn = as.numeric(s[1])
  Te = as.numeric(s[2])
  VTB = as.numeric(s[3])
  TB = as.numeric(s[4])
  Sm = as.numeric(s[5])
  LC = as.numeric(s[6])
  Br = as.numeric(s[7])
  XR = as.numeric(s[8])
  Dy = as.numeric(s[9])

  # P(Pn),P(Sm),P(VTB)
  probPn = network$Pn[1,Pn+1]
  probSm = network$Sm[1,Sm+1]
  probVTB = network$VTB[1,VTB+1]

  #P(Te|Pn),P(TB|VTB),P(LC|Sm),P(Br|Sm)
  probTe = dnorm(Te,network$Te[Pn+1,1],network$Te[Pn+1,2])
  probTB = network$TB[VTB+1,TB+1]
  probLC = network$LC[Sm+1,LC+1]
  probBr = network$Br[Sm+1,Br+1]

  # P(Dy|LC,Br)
  if(LC==0){
    if(Br==0){
      probDy = network$Dy[1,Dy+1]
    }else{
      probDy = network$Dy[3,Dy+1]
    }
  }else{
    if(Br==1){
      probDy = network$Dy[4,Dy+1]
    }else{
      probDy = network$Dy[2,Dy+1]
    }
  }

  # P(XR|Pn,TB,LC)
  if(Pn==0){
    if(TB==0){
      if(LC==0){
        probXR = network$XR[1,XR+1]
      }else{
        probXR = network$XR[2,XR+1]
      }
    }else{
      if(LC==0){
        probXR = network$XR[3,XR+1]
      }else{
        probXR = network$XR[4,XR+1]
      }
    }
  }else{
    if(TB==0){
      if(LC==0){
        probXR = network$XR[5,XR+1]
      }else{
        probXR = network$XR[6,XR+1]
      }
    }else{
      if(LC==0){
        probXR = network$XR[7,XR+1]
      }else{
        probXR = network$XR[8,XR+1]
      }
    }
  }
  return(probPn*probTe*probVTB*probTB*probSm*probLC*probBr*probXR*probDy)
}
