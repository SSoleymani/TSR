TSR_1award_1Peer = function(T,rf,price1_beg,price1_ref,Peer1_vol,Award1,Peer1_Return_History,n_sims,seed){

  set.seed(seed)
# library(readxl)
# library(pracma)
# # library(xlsx) #load the package

# read_excel_allsheets <- function(filename) {
#   sheets <- excel_sheets(filename)
#   x <- lapply(sheets, function(X) read_excel(filename, sheet = X))
#   names(x) <- sheets
#   list2env(x,envir=.GlobalEnv)
# }

# cleaned = function(df){
#   df = as.data.frame(df)  
#   df = df[complete.cases(df),]
#     df = as.matrix(df)
#     df= apply(df,MARGIN = 2,FUN = as.numeric)
#     return(df)
#   }

# setwd("C:/Users/soleysi/Desktop/Channel 1/MMP_LTIP_2 February 2017/Magellan_LTIP_2 Feb 2017/E. EY Analysis/R corroboration")
# read_excel_allsheets(filename = "raw data input.xlsx")

# n_sims = 1000000
# T = Basic_Assumptions[1,2]
# rf = Basic_Assumptions[5,2]

#price1_beg = as.matrix(cleaned(price1_beg))
#price1_ref = as.matrix(cleaned(price1_ref))
price1_beg = as.numeric(price1_beg)
price1_ref = as.numeric(price1_ref)
#Award1 = cleaned(Award1)
#Peer1_Returns = cleaned(Peer1_Return_History)
Peer1_Returns = (Peer1_Return_History)
vol1 = as.numeric(Peer1_vol)

corr_matrix1 = cor(Peer1_Returns)
# corr_output1 = corr_matrix1
# corr_output1[upper.tri(x = corr_output1,diag = F)] = NA
# write.xlsx(x = corr_output1, file = "output_results.xlsx", sheetName = "Correlation1", row.names = TRUE)

n_peer1 = nrow(corr_matrix1)
eigen_sys1 = eigen(corr_matrix1)
eigen_vec1 = eigen_sys1$vectors
eigen_vec1_inv = solve(eigen_vec1)
sim_mat1 = eigen_vec1 %*% diag(sqrt(eigen_sys1$values)) %*% eigen_vec1_inv
uncorr_sample1 = matrix(rnorm((n_peer1)*n_sims), nrow = n_peer1)
corr_sample1 = sim_mat1 %*% uncorr_sample1




price1_end = price1_beg *exp((rf- vol1^2/2)*T + corr_sample1*vol1*sqrt(T))

## check! these values should be near zero
(test = price1_beg - rowMeans(price1_end)*exp(-rf*T))

TSR_return1 = price1_end/price1_ref - 1
TSR1_rank_subject_reverse = apply(X = TSR_return1,FUN = rank,MARGIN = 2) # higher is better, 1 means worst!
TSR1_rank_subject = nrow(TSR_return1) - TSR1_rank_subject_reverse +1
TSR1_percentile = (TSR1_rank_subject_reverse-1)/(nrow(TSR1_rank_subject_reverse)-1)

# hist(TSR1_rank_subject[1,],breaks = 100)
# hist(TSR1_percentile[1,],breaks = 100)

mean(TSR1_percentile[1,])
interp1(x = Award1[,1],y = Award1[,2],xi = mean(TSR1_percentile[1,]),method = "linear")
award1_peer1_vec = interp1(x = Award1[,1],y = Award1[,2],xi = TSR1_percentile[1,],method = "linear")
(mean(award1_peer1_vec))
return(award1_peer1 = mean(award1_peer1_vec * price1_end[1,] * exp(-rf*T)))
}
#(award1_peer1 = mean(award1_peer1_vec * price1_end[1,] * exp(-rf*T)))


### for sensivity table refer to previous code. ###