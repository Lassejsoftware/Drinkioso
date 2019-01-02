#
# 15-09-2018
#
# Small helper functions
#
# Proper case
proper <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
}

# Put in names which needs tranlation. dict = list(out = c("in1","in2")). Supposed to ignore case
# todo: Extend to data sets.
useDict <- function(name,dict){
  if(is.null(name) || is.null(dict)){
    print("Specify both name and dict")
    return()
  }
  name = paste0("\\b",trimws(name),"\\b")
  outNames = names(dict)
  uNames = unique(name)
  out = vector(mode = "character", length = length(name))
  for (i in 1:length(uNames)){
    inds = name == uNames[i]
    res <- lapply(dict, function(ch) {grep(uNames[i], ch, ignore.case = T)})
    # sapply(res, function(x) length(x) > 0)
    logi = sapply(res, function(x) length(x) > 0)
    if (sum(logi) == 1){
      temp = outNames[sapply(res, function(x) length(x) > 0)][1]
    } else {
      temp = NA
    }
    if (!is.na(temp)){
      out[inds] = temp
    } else {out[inds] = uNames[i]}
  }
  out = gsub(pattern = "\\\\b",replacement = "",out)
  return(out)
}

# translate untappd time to R-time
untappd2POSIXct <- function(tVec){
  time = as.character(tVec)
  time = substr(time,6,25)
  if (is.na(strptime("01 oct","%d %b"))){
    time = gsub("oct", "okt",time,ignore.case = T)
    time = gsub("may", "maj",time,ignore.case = T)
  }
  time = as.POSIXct(strptime(time, "%d %b %Y %H:%M:%S")) 
  return(time)
}

# Count unique
# todo:
count_unique <- function(df, var = NULL){
  if (is.null(ncol(df))){
    agg = aggregate(list(N = df), by = list(name = df), length) %>% arrange(.,desc(N))
    return(agg)
  }
  if (ncol(df)>1){
    agg = aggregate(list(N = df[[var]]), by = list(temp = df[[var]]), length) %>% arrange(.,desc(N))
    names(agg)[1] = var
    return(agg)
  } 
}
