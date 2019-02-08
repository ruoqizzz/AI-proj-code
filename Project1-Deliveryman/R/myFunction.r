myFunction=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  
  routes = permutations(nrow(packages))
  routesDistance = vector(mode="integer",nrow(routes));
  for (i in 1:nrow(routes)) {
    routesDistance[i] = abs(packages[routes[i, 1], 1] - 1) + abs(packages[routes[i, 1], 2] - 1) +
                    abs(packages[routes[i, 1], 3] - packages[routes[i, 1], 1]) + abs(packages[routes[i, 1], 4] - packages[routes[i, 1], 2]) + 
                    abs(packages[routes[i, 2], 1] - packages[routes[i, 1], 3]) + abs(packages[routes[i, 2], 2] - packages[routes[i, 1], 4]) +
                    abs(packages[routes[i, 2], 3] - packages[routes[i, 2], 1]) + abs(packages[routes[i, 2], 4] - packages[routes[i, 2], 2]) +
                    abs(packages[routes[i, 3], 1] - packages[routes[i, 2], 3]) + abs(packages[routes[i, 3], 2] - packages[routes[i, 2], 4]) +
                    abs(packages[routes[i, 3], 3] - packages[routes[i, 3], 1]) + abs(packages[routes[i, 3], 4] - packages[routes[i, 3], 2]) +
                    abs(packages[routes[i, 4], 1] - packages[routes[i, 3], 3]) + abs(packages[routes[i, 4], 2] - packages[routes[i, 3], 4]) +
                    abs(packages[routes[i, 4], 3] - packages[routes[i, 4], 1]) + abs(packages[routes[i, 4], 4] - packages[routes[i, 4], 2]) +
                    abs(packages[routes[i, 5], 1] - packages[routes[i, 4], 3]) + abs(packages[routes[i, 5], 2] - packages[routes[i, 4], 4]) +
                    abs(packages[routes[i, 5], 3] - packages[routes[i, 5], 1]) + abs(packages[routes[i, 5], 4] - packages[routes[i, 5], 2])
  }

  minRoute = routes[which.min(routesDistance),]
  if(car$load==0) {
    undelivered = which(packages[,5]==0)
    toGo = intersect(minRoute,undelivered)[1]
    X = astar(roads,c(car$x,car$y),c(packages[toGo,1],packages[toGo,2]))
    nextMove = X[1]
    cost = X[2]
    # cat("\nGoal Pavckage is : [",packages[toGo,1],",",packages[toGo,2],"]\n")
  } else {
    toGo=car$load
    nextMove = findNextMove2Delivery(roads,car,packages)
    # cat("\nDelivery adress i s : [",packages[toGo,3],",",packages[toGo,4],"]\nNext move is: ",nextMove,"\n")
  }
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}

permutations <- function(packs){
  if(packs==1){
    return(matrix(1))
  } else {
    recur <- permutations(packs-1)
    p <- nrow(recur)
    mat <- matrix(nrow=packs*p,ncol=packs)
    for(i in 1:packs){
      mat[(i-1)*p+1:p,] <- cbind(i,recur+(recur>=i))
    }
    return(mat)
  }
}

# findGoalPackage=function(roads,car,packages) {
#   # find undelivered packages  index..
#   undelivered=which(packages[,5]==0)
#   # cat("undelivered:",undelivered,"\n")
#   bestPathForEachPackage = NULL
#   for(i in undelivered){
#     if(all.equal(c(car$x,car$y),c(packages[i,1],packages[i,2]))==TRUE){
#       return(c(5,i))
#     }else{
#       # car(x,y) -> package
#       X = astar(roads,c(car$x,car$y),c(packages[i,1],packages[i,2]))
#       # pacakge -> delivery adress (use one more astar may affect the time)
#       Y = astar(roads,c(packages[i,1],packages[i,2]),c(packages[i,3],packages[i,4]))
#       nextMove = X[1]
#       # cost = X[2] + abs(packages[i,3]-packages[i,1]) + abs(packages[i,4]-packages[i,2]) # MEAN 179.472

#       cost = X[2] + Y[2]*0.3
#       bestPathForEachPackage = rbind(bestPathForEachPackage,c(i,nextMove,cost))
#     }
#   }
#   if(is.vector(bestPathForEachPackage)){
#     nextMove = bestPathForEachPackage[2]
#     toGo = bestPathForEachPackage[1]
#   }else{
#     bestIndex = which.min(bestPathForEachPackage[,3])
#     nextMove = bestPathForEachPackage[bestIndex,2]
#     toGo = bestPathForEachPackage[bestIndex,1]
#     # cat(bestPathForEachPackage[bestIndex,],"\n")
#   }
  
#   return(c(nextMove,toGo))
# }

findNextMove2Delivery=function(roads,car,packages){
  toGo = car$load
  X = astar(roads,c(car$x,car$y),c(packages[toGo,3],packages[toGo,4]))
  nextMove = X[1]
  cost = X[2]
  return(nextMove)
}

#A* search algorithms to find the minimum path between 2 points
astar=function(roads,startp,endp){
  # cat("start: (",startp,")\t end:(",endp,")\n")
  # start point
  sx = startp[1]
  sy = startp[2]
  # end point
  ex = endp[1]
  ey = endp[2]
  # the parent coordinates of node (x,y): [ParentX[x,y],ParentY[x,y]]
  ParentX = matrix(NA, nrow=10, ncol=10)
  ParentY = matrix(NA, nrow=10, ncol=10)
  # Open[x,y]:  if node(x,y) in the open list
  # Closed[x,y]: node(x,y) in the close list
  Closed = matrix(NA, nrow=10, ncol=10)
  Open = matrix(NA, nrow=10, ncol=10)
  #
  Gmatrix = matrix(NA, nrow=10, ncol=10)
  Fmatrix = matrix(NA, nrow=10, ncol=10)
  
  x=sx
  y=sy
  Open[x,y]=1
  ParentX[x,y]=NA
  ParentY[x,y]=NA
  Gmatrix[x,y]=0
  Fmatrix[x,y]=Gmatrix[x,y] + abs(x-ex)+abs(y-ey)
  
  while(TRUE){
    # current = node in openlist with the minimum F
    OpenNode = which(!is.na(Open), arr.ind=T)
    current=OpenNode[1,]
    f = Fmatrix[OpenNode[1,1],OpenNode[1,2]]
    for(i in 1:nrow(OpenNode)){
      if(Fmatrix[OpenNode[i,1],OpenNode[i,2]]<f){
        f=Fmatrix[OpenNode[i,1],OpenNode[i,2]]
        current=OpenNode[i,]
      }
    }
    
    # find the goal
    if(current[1]==ex && current[2]==ey){
      f = Fmatrix[current[1],current[2]]
      path = c(current[1],current[2])
      while (!is.na(ParentX[current[1],current[2]])) {
        current = c(ParentX[current[1],current[2]],ParentY[current[1],current[2]])
        path = rbind(path, c(current[1],current[2]))
      }
      
      steps = dim(path)[1]
      if(is.null(steps)){
        nextMove=5
        
        # cat("path: \n",path,"\n")
      }else{
        dx = path[steps-1,1] - sx
        dy = path[steps-1,2] - sy
        if(dx==1){
          nextMove=6
        }else if(dx==-1){
          nextMove=4
        }
        if(dy==1){
          nextMove=8
        }else if(dy==-1){
          nextMove=2
        }
        
        # cat("path: \n")
        # for(i in 1:steps){
        #   cat(path[i,],"\n")
        # }
      }
      
      return(c(nextMove,f))
    }
    
    # remove current node from Openlist, add current node to Closedlist
    Open[current[1],current[2]] = NA
    Closed[current[1],current[2]] = 1
    
    # find neighbours
    if(current[1]>1){
      # left neighbour
      x=current[1]-1
      y=current[2]
      # check if this neighbour is in closedset
      if(is.na(Closed[x,y])){
        # check if this neighbour is in openset
        if(!is.na(Open[x,y])){ # if the neighbour is in openset, keep the one with smaller f
          g = Gmatrix[current[1],current[2]] + roads$hroads[x,y]
          f = g + abs(x-ex)+abs(y-ey)*1.8
          if(Fmatrix[x,y]>f){
            Gmatrix[x,y] = g
            Fmatrix[x,y] = f
            ParentX[x,y]=current[1]
            ParentY[x,y]=current[2]
          }
        }else{ # if the neighbour is not in openset, add it to openset
          Open[x,y]=1
          ParentX[x,y]=current[1]
          ParentY[x,y]=current[2]
          Gmatrix[x,y] = Gmatrix[current[1],current[2]] + roads$hroads[x,y]
          Fmatrix[x,y] = Gmatrix[x,y] + abs(x-ex)+abs(y-ey)*1.8
        }
      }
    }
    if(current[1]<10){
      # right neighbour
      x=current[1]+1
      y=current[2]
      # check if this neighbour is in closedset
      if(is.na(Closed[x,y])){
        # check if this neighbour is in openset
        if(!is.na(Open[x,y])){ # if the neighbour is in openset, keep the one with smaller f
          g = Gmatrix[current[1],current[2]] + roads$hroads[x-1,y]
          f = g + abs(x-ex)+abs(y-ey)*1.8
          if(Fmatrix[x,y]>f){
            Gmatrix[x,y] = g
            Fmatrix[x,y] = f
            ParentX[x,y]=current[1]
            ParentY[x,y]=current[2]
          }
        }else{ # if the neighbour is not in openset, add it to openset
          Open[x,y]=1
          ParentX[x,y]=current[1]
          ParentY[x,y]=current[2]
          Gmatrix[x,y] = Gmatrix[current[1],current[2]] + roads$hroads[x-1,y]
          Fmatrix[x,y] = Gmatrix[x,y] + abs(x-ex)+abs(y-ey)*2
        }
      }
    }
    if(current[2]>1){
      # down neighbour
      x=current[1]
      y=current[2]-1
      # check if this neighbour is in closedset
      if(is.na(Closed[x,y])){
        # check if this neighbour is in openset
        if(!is.na(Open[x,y])){ # if the neighbour is in openset, keep the one with smaller f
          g = Gmatrix[current[1],current[2]] + roads$vroads[x,y]
          f = g + abs(x-ex)+abs(y-ey)*1.8
          if(Fmatrix[x,y]>f){
            Gmatrix[x,y] = g
            Fmatrix[x,y] = f
            ParentX[x,y]=current[1]
            ParentY[x,y]=current[2]
          }
        }else{ # if the neighbour is not in openset, add it to openset
          Open[x,y]=1
          ParentX[x,y]=current[1]
          ParentY[x,y]=current[2]
          Gmatrix[x,y] = Gmatrix[current[1],current[2]] + roads$vroads[x,y]
          Fmatrix[x,y] = Gmatrix[x,y] + abs(x-ex)+abs(y-ey)*2
        }
      }
    }
    if(current[2]<10){
      # up neighbour
      x=current[1]
      y=current[2]+1
      # check if this neighbour is in closedset
      if(is.na(Closed[x,y])){
        # check if this neighbour is in openset
        if(!is.na(Open[x,y])){ # if the neighbour is in openset, keep the one with smaller f
          g = Gmatrix[current[1],current[2]] + roads$vroads[x,y-1]
          f = g + abs(x-ex)+abs(y-ey)*2
          if(Fmatrix[x,y]>f){
            Gmatrix[x,y] = g
            Fmatrix[x,y] = f
            ParentX[x,y]=current[1]
            ParentY[x,y]=current[2]
          }
        }else{ # if the neighbour is not in openset, add it to openset
          Open[x,y]=1
          ParentX[x,y]=current[1]
          ParentY[x,y]=current[2]
          Gmatrix[x,y] = Gmatrix[current[1],current[2]] + roads$vroads[x,y-1]
          Fmatrix[x,y] = Gmatrix[x,y] + abs(x-ex)+abs(y-ey)*2
        }
      }
    }
  }
}


