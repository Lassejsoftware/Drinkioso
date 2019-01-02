#
# 31-12-2018
#
# todo: doc 
#
#
getTeam <- function(user = NULL, opts = NULL, val = NULL, col = NULL){
  colVec = c("red", "blue")
  if (!is.null(val)){
    team = ifelse(test = val>0, yes = colVec[1], no = colVec[2])
    vec = val == 0
    team[vec] = "black"
    return(team)
  }
  if (!is.null(col)){
    col = tolower(col)
    val = ifelse(col == colVec[1], yes = 1, no = -1)
    return(val)
  }
  if (is.null(user) & is.null(opts)){
    return()
  }
  teamList = list(
    t1 = c("Garbacz", "hellegskov"),
    t2 = c("Slendrick", "knoe1703", "camillask")
  )
  names(teamList) <- colVec
  if (identical(opts, "teams")){
    return(teamList)
  }
  
  team = useDict(user, teamList)
  if (identical(opts,"num")){
    team = ifelse(test = team == colVec[1], yes = 1, no = -1)
  } 
  return(team)
}