## Generates anchor points,
## Takes Y outcome matrix (Yexc)
## and the ouput of pair_gen_anchors()

anchors = function (genAnchorsPair, Yexc){                                                                                
   ##  generate anchor points
   Yfake = genAnchorsPair[[1]]
   Yfake[is.na(Yfake)] = 9
   colnames(Yfake) = names(Yexc)

   theta_fake = genAnchorsPair[[2]]
   Yall = as.matrix(rbind(Yfake, Yexc))
   YList<- list("Yall" = Yall, "Yfake" = Yfake)
 return (YList)
 }
