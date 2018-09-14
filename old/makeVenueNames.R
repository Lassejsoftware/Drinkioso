#
# 29-08-2018
#
# Spits out a dataframe with is, slug and name
#
makeVenueNames <- function(dat, na_rm = 1, inv=0){
  if (inv == 0){
    if (sum(c("venue_id","venue_slug") %in% names(dat)) != 2 ){
      print("Input data frame does not have the required variables!")
      return(NULL)
    }
    if (na_rm == 1){
      dat = subset(dat, !is.na(dat$venue_id))
    }
    if (dim(dat)[1] == 0){
      print("No venues found")
      return()
    }
    venN = paste0(dat$venue_slug,"_",dat$venue_id)
    return(venN)
  }
  if (inv==1){
    # DANGER! This is very much dependent on venue_slug NEVER having _ in its name! 
    out = unlist(strsplit(dat, split = "_"))
    if (length(out)%%2 != 0){
      print("The split seemed go to very bad")
      return(NULL)
    }
    vec1 = seq(1, length(out), by = 2)
    vec2 = seq(2, length(out), by = 2)
    df = data.frame(venue_slug = out[vec1], venue_id = out[vec2])
    return(df)
  }
}
