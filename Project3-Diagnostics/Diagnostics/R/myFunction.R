myLearn <- function(hist){
  # probability of Pn
  # probPn[1,1]: (Pn==0)
  # probPn[1,2]: (Pn==1)
  probPn = matrix(0,1,2)
  tablePn = table(hist$Pn)
  probPn[1,1] = as.numeric(tablePn[1])
  probPn[1,2] = as.numeric(tablePn[2])
  probPn = probPn/sum(probPn)
  
  # probability of VTB
  probVTB = matrix(0,1,2)
  tableVTB = table(hist$VTB)
  probVTB[1,1] = as.numeric(tableVTB[1])
  probVTB[1,2] = as.numeric(tableVTB[2])
  probVTB = probVTB/sum(probVTB)
  
  # probability of Sm
  probSm = matrix(0,1,2)
  tableSm = table(hist$Sm)
  probSm[1,1] = as.numeric(tableSm[1])
  probSm[1,2] = as.numeric(tableSm[2])
  probSm = probSm/sum(probSm)
  
  # probability of Te giving Pn
  # Can not use table because Te is not 0/1
  # probTe[1,]when Pn = 0, the mean and sd of Te
  # probTe[2,]when Pn = 1, then mean and sd of Te
  probTe = matrix(0,2,2)
  Pn0 = hist$Te[hist$Pn==0]
  Pn1 = hist$Te[hist$Pn==1]
  probTe[1,] = c(mean(Pn0),sd(Pn0))
  probTe[2,] = c(mean(Pn1),sd(Pn1))
  
  # probability of TB giving VTB
  # !!!!!!!!!!!! wrong
  probTB = matrix(0,2,2)
  # probTB[x,y]: when VTB = x-1, prob of TB = y-1
  # e.g. probTB[1,]: when VTB = 0, prob of TB = 0 and TB = 1
  tableTB = table(hist$VTB,hist$TB)
  probTB[1,] = tableTB[1,]/sum(tableTB[1,])
  probTB[2,] = tableTB[2,]/sum(tableTB[2,])

  
  # probability of LC giving Sm
  probLC = matrix(0,2,2)
  # probLC[x,y]: when Sm = x-1, prob of LC = y-1
  # e.g. probVTB[1,]: when Sm = 0, prob of LC = 0 and LC = 1
  tableLC = table(hist$Sm,hist$LC)
  probLC[1,] = tableLC[1,]/sum(tableLC[1,])
  probLC[2,] = tableLC[2,]/sum(tableLC[2,])

  
  # probability of Br giving Sm
  probBr = matrix(0,2,2)
  # probLC[x,y]: when Sm = x-1, prob of Br = y-1
  # e.g. probVTB[1,]: when Sm = 0, prob of Br = 0 and LC = 1
  tableBr = table(hist$Sm,hist$Br)
  probBr[1,] = tableBr[1,]/sum(tableBr[1,])
  probBr[2,] = tableBr[2,]/sum(tableBr[2,])

  
  
  # probability of Dy giving LC and Br
  probDy = matrix(0,4,2)
  tableDy = table(hist$Br, hist$LC, hist$Dy)
  # Br = 0, LC = 0, the prob of Dy = 0 and Dy = 1
  probDy[1,] = tableDy[1,1,]/sum(tableDy[1,1,])
  # Br = 0, LC = 1, the prob of Dy = 0 and Dy = 1
  probDy[2,] = tableDy[1,2,]/sum(tableDy[1,2,])
  # Br = 1, LC = 0
  probDy[3,] = tableDy[2,1,]/sum(tableDy[2,1,])
  # Br = 1, LC = 1
  probDy[4,] = tableDy[2,2,]/sum(tableDy[2,2,])
  
  # probability of XR giving Pn, TB and LC
  probXR = matrix(0,8,2)
  tableXR = table(hist$Pn, hist$TB, hist$LC, hist$XR)
  # Pn = 0, TB = 0, LC = 0
  probXR[1,] = tableXR[1,1,1,]/sum(tableXR[1,1,1,])
  # Pn = 0, TB = 0, LC = 1
  probXR[2,] = tableXR[1,1,2,]/sum(tableXR[1,1,2,])
  # Pn = 0, TB = 1, LC = 0
  probXR[3,] = tableXR[1,2,1,]/sum(tableXR[1,2,1,])
  # Pn = 0, TB = 1, LC = 1
  probXR[4,] = tableXR[1,2,2,]/sum(tableXR[1,2,2,])
  # Pn = 1, TB = 0, LC = 0
  probXR[5,] = tableXR[2,1,1,]/sum(tableXR[2,1,1,])
  # Pn = 1, TB = 0, LC = 1
  probXR[6,] = tableXR[2,1,2,]/sum(tableXR[2,1,2,])
  # Pn = 1, TB = 1, LC = 0
  probXR[7,] = tableXR[2,2,1,]/sum(tableXR[2,2,1,])
  # Pn = 1, TB = 1, LC = 1
  probXR[8,] = tableXR[2,2,2,]/sum(tableXR[2,2,2,])
  network = list(Pn = probPn, Br = probBr, Dy = probDy, LC = probLC, Sm = probSm,
                 VTB = probVTB, TB = probTB, XR = probXR, Te = probTe)
  return(network)
}

myDiagnose <- function(network, cases){
  U = c(1,4,6,7)
  E = c(2,3,5,8,9)
  # the number of sample
  N = 10000
  # the number of burn
  burn = 1000
  estimates = matrix(0,nrow=nrow(cases),ncol=length(U))
  for (r in 1:nrow(cases)) {
    samples = matrix(nrow=N-burn, ncol=length(cases))
    # set all variable. in E to their known values
    for (e in E) {
      samples[,e] = cases[r,e]
    }
    # randomly assign values in U
    for (u in U) {
      samples[1,u] = sample(0:1,1)
    }
    startP = samples[1,]
    # MCMC Algorithm Metropolis in Gibbs
    probStart = calProb(samples[1,], network)

    for(i in 1:N){
      for(u in U){
        startP[u] = as.numeric(!startP[u])
        probNew = calProb(startP,network)
        if(probNew < probStart){
          if(runif(1) > probNew/probStart){
            # discard
            startP[u] = as.numeric(!startP[u])
            probNew = probStart
          }
        }
        probStart = probNew
      }
      if(i > burn){
      samples[i-burn,] = as.numeric(startP);
    }
    }
    
  estimates[r,] = colMeans(samples[,U]) 
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
