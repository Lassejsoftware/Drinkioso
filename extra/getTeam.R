#
# 31-12-2018
#
# todo: doc. Consider making teamList a global
#
#
getTeam <- function(user = NULL, opts = NULL, val = NULL, col = NULL){
  colVec = c("red", "blue")
  if (identical(opts, "name")){
    return(proper(colVec))
  }
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
  
  if (file.exists("../drinkiosoData/teamList.rds")){
    # print("Loading team")
    teamList = readRDS("../drinkiosoData/teamList.rds")
  } else {
    teamList = list(
      t1 = c("Garbacz", "hellegskov", "JayTea"),
      t2 = c("Slendrick", "knoe1703", "camillask")
    )
    names(teamList) <- colVec
    saveRDS(teamList,"../drinkiosoData/teamList.rds")
  }
  
  if (identical(opts, "teams")){
    teamList = teamList[names(teamList) != "unknown"]
    return(teamList)
  }
  
  team = useDict(user, teamList)
  team[!(team %in% colVec)] = "unknown"
  if (identical(opts,"num")){
    team = ifelse(test = team == colVec[1], yes = 1, no = -1)
  } 
  return(team)
}
