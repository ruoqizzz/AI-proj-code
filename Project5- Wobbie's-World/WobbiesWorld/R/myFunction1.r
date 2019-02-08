myFunction <- function (maze){

  decideAction <- function(cont, maze_){
	  # Init
	  if(is.null(cont$Qtable)){
	    cont$actions = c(2,4,5,6,8)
	    cont$Qtable = matrix(0,27*27,5)
#	    cont$Visit = rep(0,27*27)
	    # learning rate
	    cont$lr = 0.2
	    # learning rate decay
	    cont$lrd = 0.001
	    # e_greedy
	    cont$epsilon = 0.5
	    cont$epsrd = 0.01
	    # reward decay
	    cont$gamma = 0.9
	    cont$lastState = c(0,0)
	  }
	  wbIndex = which(maze_$maze$x==maze_$wobbie[1] & maze_$maze$y==maze_$wobbie[2])
	  monIndex = which(maze_$maze$x==maze_$monster1[1] & maze_$maze$y==maze_$monster1[2])
	  
	  qValues = cont$Qtable[(wbIndex-1)*27+monIndex,]
	  nextActionIndexes = which(qValues==max(qValues))
	  if(length(nextActionIndexes)>1){
	    nextIndex = sample(nextActionIndexes,1)
	  }else{
	    nextIndex = nextActionIndexes
	  }
	  nextAction = cont$actions[nextIndex]
	  # e greedy
	  if(runif(1)>cont$epsilon){
	    nextAction = sample(cont$actions,1)
	  }
	  # lastInfo = c(state,actionIndex)
	  cont$lastInfo = c((wbIndex-1)*27+monIndex,nextIndex)
	  return(list(move = nextAction, control = cont))
	}

	update <- function(cont, maze_){
	  wbIndex = which(maze_$maze$x==maze_$wobbie[1] & maze_$maze$y==maze_$wobbie[2])
	  monIndex = which(maze_$maze$x==maze_$monster1[1] & maze_$maze$y==maze_$monster1[2])
	  lastInfo = cont$lastInfo
	  # cat(lastInfo)
	  q_predict = cont$Qtable[lastInfo[1],lastInfo[2]]
	  if(maze$alive && !maze$finished){
	    q_target = maze_$reward + cont$gamma * max(cont$Qtable[(wbIndex-1)*27+monIndex,])  # next state is not terminal
	  }else{
	    q_target = r
	  }
	  
#	  learningrate = cont$lr * 10000 / (10000+cont$Visit[(wbIndex-1)*27+monIndex])
#	  learningrate = cont$lr - cont$lrd*cont$Visit[(wbIndex-1)*27+monIndex]
#	  if(learningrate<0.01){
#	    learningrate = 0.01
#	  }
	  cont$Visit[(wbIndex-1)*27+monIndex] = cont$Visit[(wbIndex-1)*27+monIndex] + 1
	  cont$Qtable[lastInfo[1],lastInfo[2]] = cont$Qtable[cont$lastInfo[1],cont$lastInfo[2]] - cont$lr*(q_predict - q_target)
		cont$epsilon = cont$epsilon + cont$epsrd
		if(cont$lr>0.01){
		  cont$lr = cont$lr - cont$lrd
		}
    

		
	  return(cont)
	}

	list(decideAction=decideAction,update=update,doRand=TRUE)
}