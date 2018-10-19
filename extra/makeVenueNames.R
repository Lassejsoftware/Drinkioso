#
# 29-08-2018
# 
# Spits out a dataframe with is, slug and name
#
makeVenueNames <- function(dat, na_rm = 1){
  if (sum(c("venue_id") %in% names(dat)) != 2 ){
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
  out = data.frame(venue_id = dat$venue_id, 
                   venue_slug = dat$venue_slug, 
                   #name = paste0(dat$venue_slug,"_",dat$venue_id,".rds")
                   name = paste0(dat$venue_id,".rds")
                   )
  return(out)
}
