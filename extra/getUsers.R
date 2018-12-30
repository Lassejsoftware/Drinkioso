#
# 30-12-2018
#
# Will fetch users in user folder
#
getUsers <- function(){
  users = tools::file_path_sans_ext(dir("checkinHist"))
  return(users)
}
