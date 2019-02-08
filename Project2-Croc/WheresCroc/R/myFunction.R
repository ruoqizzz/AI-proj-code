myFunction <- function(moveInfo,readings,positions,edges,probs) {
  if(moveInfo$mem$status==0) {
    stateTransProbs = matrix(rep(0,40*40),40,40)
    for (i in 1:40) {
      indexes = getOptions(i,edges)
      n = length(indexes)
      for (j in indexes) {
        stateTransProbs[i,j] = 1/n
      }
    }
    moveInfo$mem$stateTransProbs = stateTransProbs
  }else{
    stateTransProbs = moveInfo$mem$stateTransProbs
  }
  # initialize
  if(is.null(moveInfo$moves)) {
    moveInfo$mem$pNodes = rep(0,times=40)
    for (i in 1:40) {
      # except two backpackers
      moveInfo$mem$pNodes[i] = 1/38
    }
    # suppose they are not eaten, then their position is safe
    moveInfo$mem$pNodes[positions[1]] = 0
    moveInfo$mem$pNodes[positions[2]] = 0
  }
  # this step probability of each nodes
  probNodes = rep(0, times=40)
  # chech if backpackers are dead
  if(positions[1] < 0 && !is.na(positions[1])) {
    probNodes[abs(positions[1])] = 1
  }else if(positions[2] < 0 && !is.na(positions[2])){
    probNodes[abs(positions[2])] = 1
  }else {
    # Both backpackers are alive
    for (i in 1:40) {
      # probability of each node with reading
      probReading = dnorm(readings[1],probs$salinity[i,1],probs$salinity[i,2]) *
        dnorm(readings[2],probs$phosphate[i,1],probs$phosphate[i,2]) *
        dnorm(readings[3],probs$nitrogen[i,1],probs$nitrogen[i,2])
      for (j in getOptions(i,edges)) {
        probNodes[i] = probNodes[i] + moveInfo$mem$pNodes[j]*stateTransProbs[j,i]*probReading
      }
    }
    # positions backpacker are won't have croc
    probNodes[positions[1]] = 0
    probNodes[positions[2]] = 0
    probNodes = probNodes/sum(probNodes)
  }
  moveInfo$mem$pNodes = probNodes
  # get the max index
  maxIndex = which.max(probNodes) 
  # get the best path to maxIndex  
  if(maxIndex == positions[3]){
    moveInfo$moves = c(0,0)
  }else{
    bestPath = getShortestPath(positions[3],maxIndex,edges)
    if(length(bestPath)==2){
      moveInfo$moves = c(bestPath[2],0)
    }else{
      moveInfo$moves = c(bestPath[2],bestPath[3])
    }
  }
  return(moveInfo)
}


appendNewNode <- function (newNode,routes){
  length = length(routes);
  if(length == 0) {
    routes[1] = list(newNode);
  }
  else{
    for(i in 1:length){
      if(routes[[i]]$pos == newNode$pos ){
        if(routes[[i]]$cost   >= newNode$cost ){
          routes = routes[-i];
          length = length -1;
          break
        }else{
          return(routes)
        }
      }
    }
    
    for (i in 1:length) {
      if(routes[[i]]$cost  >= newNode$cost)
      {
        routes = append(routes,list(newNode), i-1);
        return(routes)
      }
    }
    routes[length+1] = list(newNode);
  }
  return (routes)
}

getShortestPath = function(start,goal,edges){
  startNode = list(pos=start, cost=0, path=start);
  routes = list(startNode)
  expanded = NA
  
  flag = 1;
  counter = 1
  while (flag) {
    expanded = routes[[1]]
    routes = routes[-1]

    if(expanded$pos == goal ){
      flag = 0
      return(expanded$path)
    }
    
    for (i in 1:length(edges[,1])) {
      if(edges[i,1] == expanded$pos){
        newNode = list(pos= edges[i,2], cost=expanded$cost +1, path=expanded$path)
        newNode$path[length(newNode$path)+1] = edges[i,2]
        routes = appendNewNode(newNode,routes)
      }
      else if(edges[i,2] == expanded$pos){
        newNode = list(pos= edges[i,1], cost=expanded$cost +1, path=expanded$path)
        newNode$path[length(newNode$path)+1] = edges[i,1]
        routes = appendNewNode(newNode,routes)
      }
    }
  }
}

getOptions <- function(point,edges) {
  # points it can access  and itself
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}