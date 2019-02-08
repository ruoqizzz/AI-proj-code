#' Make the maze
#'
#' This function makes the maze, and you can call this to familiarize yourself with the maze
#' object. You will use this object to set up your controller, and to recieve
#' information from the environment in every turn. The fields of the maze list are given in details below.
#'
#' df - A data frame of possible x and y positions (this gives the maze)
#'
#' wobbie - The location of Wobbie
#'
#' goal - The location of the goal
#'
#' monster1 - The location of the monster
#'
#' reward - Reward from the last turn
#'
#' alive - If Wobbie is alive
#'
#' finished - If the game is over (won)
#'
#' monsters - How many monsters are in the maze
#'
#' lastAction - The last action taken by Wobbie
#'
#' @param monsters How many monsters to place in the maze. Default is 1, and you will be evaluated with 1.
#' @return A list giving the maze object with the fields as described above.
#' @export
makeWobbiesWorld=function(monsters=1) {
  x=c(1,1,1,1,1, 2,2,2,2, 3,3,3,3,3, 4,4,4, 5,5,5,5,5, 6,6,6,6,6)
  y=c(1,2,4,5,6, 2,3,4,6, 1,2,4,5,6, 1,4,6, 1,2,3,4,6, 1,2,4,5,6)
  df=data.frame(x=x,y=y)
  out=list()
  if (monsters==2)
    out=list(maze=df,wobbie=c(1,6),goal=c(6,1),monster1=c(1,1),monster2=c(6,6),
             reward=0,alive=T,finished=F,monsters=monsters,lastAction=0)
  else if (monsters==1)
    out=list(maze=df,wobbie=c(1,6),goal=c(6,1),monster1=c(5,2),
             reward=0,alive=T,finished=F,monsters=monsters,lastAction=0)
  class(out)="Maze"
  return(out)
}
#' @keywords internal
plot.Maze=function(maze,main,pause){
  plot(maze$maze,main=main,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,7),ylim=c(0,7))
  if (!maze$alive)
    points(maze$wobbie[1],maze$wobbie[2],col="red",pch=4,cex=3)
  else
    points(maze$wobbie[1],maze$wobbie[2],col="blue",pch=18,cex=3)
  points(maze$monster1[1],maze$monster1[2],col="red",pch=15,cex=3)
  if (maze$monsters==2)
    points(maze$monster2[1],maze$monster2[2],col="red",pch=15,cex=3)
  points(maze$goal[1],maze$goal[2],pch=4,cex=2,col="green")
  Sys.sleep(pause)
}
#' @keywords internal
update=function(maze,action){
  inMaze=function(loc){
    length(which(maze$maze$x==loc[1] & maze$maze$y==loc[2]))==1
  }
  oldX=wobbieX=maze$wobbie[1]
  oldY=wobbieY=maze$wobbie[2]
  if (action==4 && inMaze(c(oldX-1,oldY)))
    wobbieX=oldX-1
  if (action==6 && inMaze(c(oldX+1,oldY)))
    wobbieX=oldX+1
  if (action==2 && inMaze(c(oldX,oldY-1)))
    wobbieY=oldY-1
  if (action==8 && inMaze(c(oldX,oldY+1)))
    wobbieY=oldY+1
  maze$wobbie=c(wobbieX,wobbieY)
  maze$lastAction=action

  # Check if Wobbie walks into monster
  if (all(maze$monster1==maze$wobbie) ||
      (maze$monsters==2 && all(maze$monster2==maze$wobbie))) {
    maze$alive=FALSE
    maze$reward=-1000
    return (maze)
  }

  monMoves=function(loc) {
    out=c()
    if(inMaze(c(loc[1]-1,loc[2])))
      out=c(out,4)
    if(inMaze(c(loc[1]+1,loc[2])))
      out=c(out,6)
    if(inMaze(c(loc[1],loc[2]-1)))
      out=c(out,2)
    if(inMaze(c(loc[1],loc[2]+1)))
      out=c(out,8)
    return (out)
  }

  # Move monster 1
  moves1=monMoves(maze$monster1)
  if (length(moves1)==1)
    m1=moves1
  else
    m1=sample(moves1,1)
  if(m1==4)
    maze$monster1[1]=maze$monster1[1]-1
  else if (m1==6)
    maze$monster1[1]=maze$monster1[1]+1
  else if (m1==2)
    maze$monster1[2]=maze$monster1[2]-1
  else if (m1==8)
    maze$monster1[2]=maze$monster1[2]+1

  # Move monster 2
  if (maze$monsters==2) {
    moves2=monMoves(maze$monster2)
    if (length(moves2)==1)
      m2=moves2
    else
      m2=sample(moves2,1)
    if(m2==4)
      maze$monster2[1]=maze$monster2[1]-1
    else if (m2==6)
      maze$monster2[1]=maze$monster2[1]+1
    else if (m2==2)
      maze$monster2[2]=maze$monster2[2]-1
    else if (m2==8)
      maze$monster2[2]=maze$monster2[2]+1
  }

  # Check if monster catches Wobbie
  if (all(maze$monster1==maze$wobbie) ||
      (maze$monsters==2 && all(maze$monster2==maze$wobbie))) {
    maze$alive=FALSE
    maze$reward=-1000
    return (maze)
  }

  if (all(maze$wobbie==maze$goal)) {
    maze$finished=TRUE
    maze$reward=1000
    return (maze)
  }

  maze$reward=-1
  return (maze)
}
#' @keywords internal
takeTurn=function(maze,control) {
  actionList=control$decideAction(control,maze)
  action=actionList$move
  #print(action)
  control=actionList$control
  maze=update(maze,action)
  #if (maze$finished)
  #  browser()
  control=control$update(control,maze)
  list (maze=maze,control=control)
}

