# #' @examples
# #' # S109_1 example

# data("S109_coding_1")
# data("S109_votes")
# data("S109_rollcalls")

# computeResponse = function (M_codes,votes,roll){
#   coding_ids = M_codes[1, ]
#   M_codes = M_codes[-1, ]
#   # Add voteview rollnumber identifier
#   M_codes$rollnumber = roll$rollnumber
#   # Recode response data
#   Y = reshape2::dcast(data.frame(r=votes$rollnumber,
#                        id=votes$icpsr, v=votes$cast_code), id ~ r)
#   cid = Y$id

#   Y = Y[,-1]
#   Y[is.na(Y)] = 9
#   Y[Y == 2] = 1
#   Y[Y == 3] = 1
#   Y[Y == 4] = 0
#   Y[Y == 5] = 0
#   Y[Y == 6] = 0
#   Y[Y == 7] = 9
#   Y[Y == 8] = 9

#   N = nrow(Y)
#   K = ncol(Y)

#   codings = unique(as.numeric(coding_ids))
#   codings = codings[!is.na(codings)]
#   ds = sapply(codings, function(x) sum(coding_ids==x, na.rm=T))
#   Ms = sapply(ds, function(x) array(0, c(x, x, K)))

#   for (i in 1:K){
#     for(j in 1:length(Ms)){
#       Ms[[j]][,,i] = diag(M_codes[i, which(coding_ids == j)])
#     }
#   }

#   no_nays = which(apply(Y, 2, function(x) all(x != 0)))
#   Y = Y[,-no_nays]

#   for (j in 1:length(Ms)){
#     Ms[[j]][is.na(Ms[[j]])] = 0
#     Ms[[j]] = Ms[[j]][,,-no_nays]
#   }

#   N = nrow(Y)
#   K = ncol(Y)

#   for (j in 1:length(ds)){
#     d = ds[j]
#     M = Ms[[j]]
#     exc = which(apply(M, 3, function(x) all(diag(x)==0)))

#     if (length(exc) == 0){
#       Yexc = Y
#       Mexc = M
#     }else{
#       Yexc = Y[, -exc]
#       Mexc = M[,,-exc]
#     }

#   }
#   excList<- list("Yexc" = Yexc, "Mexc" = Mexc,"d"=d)
#   return(excList)
# }


# anchors = function (tl, Yexc){
#    # generate anchor points
#   Yfake = tl[[1]]
#   Yfake[is.na(Yfake)] = 9
#   colnames(Yfake) = names(Yexc)

#   theta_fake = tl[[2]]
#   Yall = as.matrix(rbind(Yfake, Yexc))
#   YList<- list("Yall" = Yall, "Yfake" = Yfake)
# return (YList)
# }


# l1<-computeResponse(S109_coding_1,S109_votes,S109_rollcalls)
# l2<-pair_gen_anchors(l1$Mexc,5)
# l3<-anchors(l2,l1$Yexc)
# d_theta_fix<-l2$theta_fake
# d_which_fix<-1:nrow(l3$Yfake)
# irt<-M_constrained_irt(l3$Yall,l1$d,abs(l1$Mexc)*2, theta_fix = d_theta_fix,which_fix = d_which_fix,nburn=100,nsamp=100,thin=1, learn_Omega=TRUE)
