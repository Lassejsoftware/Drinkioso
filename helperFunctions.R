#
# 15-09-2018
#
# Small helper functions
#
# Proper case
proper <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
}