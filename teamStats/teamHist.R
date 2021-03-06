#
# 31-12-2018
#
# histogram of owned bars
# 
# 
#
teamHist <- function(score, isBar = F){
  xlab = 13
  delta = 5
  
  score$val = abs(score$val)
  
  BREAKS = seq(0,max(score$val), delta)
  score$cuts = cut(score$val, BREAKS, ordered_result = T, include.lowest = F)
  
  score = subset(score, !is.na(score$cuts) )
  if (isBar){
    score = subset(score, score$isBar)
  }
  score$col = proper(score$col)
  score$col = factor(score$col, levels = unique(score$col))
  
  p <- ggplot(score) + geom_bar(aes(x = cuts, fill = col), position = "dodge") +
    labs(x = "Venue Score", y = "Number of bars", title = "Histogram of venue scores", fill = "Team") + 
    scale_fill_manual(values = c(Blue = "#619CFF",
                                 Red =  "#F8766D")) +
    theme(
      axis.text.x = element_text(size = xlab),
      axis.text.y = element_text(size = xlab)
    )
  return(p)
}

