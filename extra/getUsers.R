#
# 30-12-2018
#
# Will fetch users in user folder
#
getUsers <- function(team = F){
  users = tools::file_path_sans_ext(dir("../drinkiosoData/checkinHist"))
  if (team){
    users = users[getTeam(user = users) != "unknown"]
  }
  return(users)
}
