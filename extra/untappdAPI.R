#
# 03-09-2018
#
# wrapper for untapped API. Doc: https://untappd.com/api/docs
# 
# todo: Implement minLeft(might only work with authentication), tryCatch for non existent endpoints 
#
# Dep: source("extra/config.R")
#
untappdAPI <- function(method = NULL, param = NULL, name=NULL, query = NULL, minLeft = 10){
  if (is.null(method) || is.numeric(method)){
    print("Specify the method correctly!")
    return(NULL)
  }
  # get some creds!
  if (is.null(name)){
    config = getCreds()
  } else {
    config = getCreds(name = name)
  }
  # Fix bad method name.
  if (substr(method,nchar(method),nchar(method)) != "/"){
    method = paste0(method,"/")
  }
  # Call untappd
  final = paste0(config$link,method,param,"?client_id=",config$id,"&client_secret=",config$secret)
  if (is.null(query)){
    returnData = fromJSON(final)
  } else {
    q = ""
    for (i in names(query)){
      q = paste0(q,"&",i,"=",query[[i]])
    }
    final = paste0(final,q)
    returnData = fromJSON(final)
  }
  # Check for valid response.
  if (returnData$meta$code == 200){
    return(returnData)
  } else {
    print(paste0("Something went wrong with code ",returnData$meta$code))
    return(NULL)
  }
}
# For testing
#
# method = "user/checkins/"
# user = "Garbacz"
# query = list(limit = 10)
# untappdAPI(method = method, param = user, query = query)
#