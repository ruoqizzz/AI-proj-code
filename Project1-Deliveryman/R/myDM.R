
myFirstDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if(car$load==0) {
    x=findNearestPackage(roads,car,packages)
    toGo = x[1]
    nextMove = x[2]
    cat("\nGoal Pavckage is : [")
    cat(packages[toGo,1])
    cat(", ")
    cat(packages[toGo,2])
    cat("]\n")
  } else {
    toGo=car$load
    nextMove = findNextMove2Package(roads,car,packages)
    cat("\nDelivery adress is : [")
    cat(packages[toGo,3])
    cat(", ")
    cat(packages[toGo,4])
    cat("]\nNext move is: ")
    cat(nextMove)
    cat("\n")
  }
  car$nextMove=nextMove
  car$mem=list();
  return (car)
}

mySecondDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if(car$load==0) {
    x=findNearestPackag2Delivery(roads,car,packages)
    toGo = x[1]
    nextMove = x[2]
    # cat("\nGoal Pavckage is : [")
    # cat(packages[toGo,1])
    # cat(", ")
    # cat(packages[toGo,2])
    # cat("]\n")
  } else {
    toGo=car$load
    nextMove = findNextMove2Package(roads,car,packages)
    # cat("\nDelivery adress is : [")
    # cat(packages[toGo,3])
    # cat(", ")
    # cat(packages[toGo,4])
    # cat("]\nNext move is: ")
    # cat(nextMove)
    # cat("\n")
  }
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}



findNextMove2Package=function(roads,car,packages) {
  toGo = car$load
  nextMove = 0
  randomDigit = runif(1,0,1)
  # cat("\nrandomDigit: ")
  # cat(randomDigit)
  if(randomDigit < 0) {
    if (car$x<packages[toGo,3]) {nextMove=6}
    else if (car$x>packages[toGo,3]) {nextMove=4}
    else if (car$y<packages[toGo,4]) {nextMove=8}
    else if (car$y>packages[toGo,4]) {nextMove=2}
    else {nextMove=5}
  }else {
    # < which move, cost>
    costForEachMove = matrix(rep(100,4*2),nrow=4,ncol=2)

    # move up nextMove = 8 
    if(car$y!=10) {
      realcost = roads$vroads[car$x,car$y]
      nextY = car$y+1
      heuristCost = abs(packages[toGo,3]-car$x)+abs(packages[toGo,4]-nextY)
      costForEachMove[1,] = c(8, realcost+heuristCost)
    }
    # move down nextMove = 2
    if(car$y!=1) {
      nextY = car$y-1
      realcost = roads$vroads[car$x,nextY]
      heuristCost = abs(packages[toGo,3]-car$x)+abs(packages[toGo,4]-nextY)
      costForEachMove[2,] = c(2, realcost+heuristCost)
    }
    # move left nextMove = 4
    if(car$x!=1) {
      nextX = car$x-1
      realcost = roads$hroads[nextX,car$y]
      heuristCost = abs(packages[toGo,3]-nextX)+abs(packages[toGo,4]-car$y)
      costForEachMove[3,] = c(4, realcost+heuristCost)
    }
    # move right nextMove = 6
    if(car$x!=10){
      realcost = roads$hroads[car$x,car$y]
      nextX = car$x+1
      heuristCost = abs(packages[toGo,3]-nextX)+abs(packages[toGo,4]-car$y)
      costForEachMove[4,] = c(6, realcost+heuristCost)
    }
    cat("\ncostForEachMove\n")
    cat(costForEachMove)
    bestIndex = which.min(costForEachMove[,2])
    # bestIndex = bestIndex[bestIndex != car$mem[length(car$mem)-1]]
    if(is.null(bestIndex)){
      nextMove  = 5
    }else{
      nextMove = costForEachMove[bestIndex,1]
    }
    
  }
  return(nextMove)
}


findNearestPackage=function(roads,car,packages) {
  toGo=0
  nextMove = 0
  randomDigit = runif(1,0,1)
  if(randomDigit<0){
    toGo=which(packages[,5]==0)[1]
    if (car$x<packages[toGo,1]) {nextMove=6}
    else if (car$x>packages[toGo,1]) {nextMove=4}
    else if (car$y<packages[toGo,2]) {nextMove=8}
    else if (car$y>packages[toGo,2]) {nextMove=2}
    else {nextMove=5}
    }else {
    # the best package for each node and its cost  <which move,which package, cost>
    bestPackForEachMove = matrix(rep(100,4*3),nrow=4,ncol=3)
    # find undilivered package index
    undilivered = which(packages[,5]==0)
    # move up nextMove = 8 
    if(car$y!=10) {
      realcost = roads$vroads[car$x,car$y]
      nextY = car$y+1
      heuristMin = 20 # max dim+dim
      heuristMinIndex = 0
      j = 0
      heurist = 0
      # get the nearest package 
      for (j in undilivered) {
        heurist = abs(packages[j,1]-car$x)+abs(packages[j,2]-nextY)
        if(heurist < heuristMin) {
          heuristMin = heurist
          heuristMinIndex = j
        }
      }
      bestPackForEachMove[1,] = c(8,heuristMinIndex,heuristMin+realcost)
    }
    # move down nextMove = 2
    if(car$y!=1) {
      nextY = car$y-1
      realcost = roads$vroads[car$x,nextY]
      heuristMin = 20 # max dim+dim
      heuristMinIndex = 0
      j = 0
      heurist = 0
      # get the nearest package 
      for (j in undilivered) {
        heurist = abs(packages[j,1]-car$x)+abs(packages[j,2]-nextY)
        if(heurist < heuristMin) {
          heuristMin = heurist
          heuristMinIndex = j
        }
      }
      bestPackForEachMove[2,] = c(2,heuristMinIndex,heuristMin+realcost)
    }
    # move left nextMove = 4
    if(car$x!=1) {
      nextX = car$x-1
      # cat(nextX)
      # cat(car$y)
      realcost = roads$hroads[nextX,car$y]      
      heuristMin = 20 # max dim+dim
      heuristMinIndex = 0
      j = 0
      heurist = 0
      # get the nearest package 
      for (j in undilivered) {
        heurist = abs(packages[j,1]-nextX)+abs(packages[j,2]-car$y)
        if(heurist < heuristMin) {
          heuristMin = heurist
          heuristMinIndex = j
        }
      }
      bestPackForEachMove[3,] = c(4,heuristMinIndex,heuristMin+realcost)
    }
    # move right nextMove = 6
    if(car$x!=10) {
      realcost = roads$hroads[car$x,car$y]
      nextX = car$x+1
      heuristMin = 20 # max dim+dim
      heuristMinIndex = 0
      j = 0
      heurist = 0
      # get the nearest package 
      for (j in undilivered) {
        heurist = abs(packages[j,1]-nextX)+abs(packages[j,2]-car$y)
        if(heurist < heuristMin) {
          heuristMin = heurist
          heuristMinIndex = j
        }
      }
      bestPackForEachMove[4,] = c(6,heuristMinIndex,heuristMin+realcost)
    }
    cat("\n bestPackForEachMove:")
    cat(bestPackForEachMove)
    bestIndex = which.min(bestPackForEachMove[,3])
    # bestIndex = bestIndex[bestIndex != car$mem[length(car$mem)-1]]
    if(is.null(bestIndex)){
      nextMove  = 5
    }else{
      nextMove = bestPackForEachMove[bestIndex,1]
      toGo = bestPackForEachMove[bestIndex, 2]
    }
  }
  return(c(toGo, nextMove))
}

findNearestPackag2Delivery=function(roads,car,packages) {
  toGo=0
  nextMove = 0
  randomDigit = runif(1,0,1)
  if(randomDigit<0){
    toGo=which(packages[,5]==0)[1]
    if (car$x<packages[toGo,1]) {nextMove=6}
    else if (car$x>packages[toGo,1]) {nextMove=4}
    else if (car$y<packages[toGo,2]) {nextMove=8}
    else if (car$y>packages[toGo,2]) {nextMove=2}
    else {nextMove=5}
    }else {
    # the best package for each node and its cost  <which move,which package, cost>
    bestPackForEachMove = matrix(rep(100,4*3),nrow=4,ncol=3)
    # find undilivered package index
    undilivered = which(packages[,5]==0)
    # move up nextMove = 8 
    if(car$y!=10) {
      realcost = roads$vroads[car$x,car$y]
      nextY = car$y+1
      heuristMin = 20 # max dim+dim
      heuristMinIndex = 0
      j = 0
      heurist = 0
      # get the nearest package 
      for (j in undilivered) {
        heurist2pakage = abs(packages[j,1]-car$x)+abs(packages[j,2]-nextY)
        heurist2delivery = abs(packages[j,3]-car$x)+abs(packages[j,4]-nextY)
        heurist = heurist2delivery + heurist2pakage
        if(heurist < heuristMin) {
          heuristMin = heurist
          heuristMinIndex = j
        }
      }
      bestPackForEachMove[1,] = c(8,heuristMinIndex,heuristMin+realcost)
    }
    # move down nextMove = 2
    if(car$y!=1) {
      nextY = car$y-1
      realcost = roads$vroads[car$x,nextY]
      heuristMin = 20 # max dim+dim
      heuristMinIndex = 0
      j = 0
      heurist = 0
      # get the nearest package 
      for (j in undilivered) {
        heurist2pakage = abs(packages[j,1]-car$x)+abs(packages[j,2]-nextY)
        heurist2delivery = abs(packages[j,3]-car$x)+abs(packages[j,4]-nextY)
        heurist = heurist2delivery + heurist2pakage
        if(heurist < heuristMin) {
          heuristMin = heurist
          heuristMinIndex = j
        }
      }
      bestPackForEachMove[2,] = c(2,heuristMinIndex,heuristMin+realcost)
    }
    # move left nextMove = 4
    if(car$x!=1) {
      nextX = car$x-1
      # cat(nextX)
      # cat(car$y)
      realcost = roads$hroads[nextX,car$y]

      heuristMin = 20 # max dim+dim
      heuristMinIndex = 0
      j = 0
      heurist = 0
      # get the nearest package 
      for (j in undilivered) {
        heurist2pakage = abs(packages[j,1]-nextX)+abs(packages[j,2]-car$y)
        heurist2delivery = abs(packages[j,3]-nextX)+abs(packages[j,4]-car$y)
        heurist = heurist2delivery + heurist2pakage
        if(heurist < heuristMin) {
          heuristMin = heurist
          heuristMinIndex = j
        }
      }
      bestPackForEachMove[3,] = c(4,heuristMinIndex,heuristMin+realcost)
    }
    # move right nextMove = 6
    if(car$x!=10) {
      realcost = roads$hroads[car$x,car$y]

      nextX = car$x+1
      heuristMin = 20 # max dim+dim
      heuristMinIndex = 0
      j = 0
      heurist = 0
      # get the nearest package 
      for (j in undilivered) {
        heurist2pakage = abs(packages[j,1]-nextX)+abs(packages[j,2]-car$y)
        heurist2delivery = abs(packages[j,3]-nextX)+abs(packages[j,4]-car$y)
        heurist = heurist2delivery + heurist2pakage
        if(heurist < heuristMin) {
          heuristMin = heurist
          heuristMinIndex = j
        }
      }
      bestPackForEachMove[4,] = c(6,heuristMinIndex,heuristMin+realcost)
    }
    bestIndex = which.min(bestPackForEachMove[,3])
    # bestIndex = bestIndex[bestIndex != car$mem[length(car$mem)-1]]
    if(is.null(bestIndex)){
      nextMove  = 5
    }else{
      toGo = bestPackForEachMove[bestIndex, 2]
      nextMove = bestPackForEachMove[bestIndex, 1]
    }
  }
  
  return(c(toGo, nextMove))
}

