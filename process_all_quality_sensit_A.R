# #                    
# # KOFGI,
# # KOFGIdf,KOFGIdj,
# # KOFEcGI,KOFSoGI,KOFPoGI
# # KOFEcGIdf	KOFEcGIdj, KOFSoGIdf	KOFSoGIdj  KOFPoGIdf,	KOFPoGIdj,
# # KOFTrGI,KOFFiGI,KOFIpGI,KOFInGI,KOFCuGI,KOFPoGI,	
#  KOFTrGIdf,	KOFTrGIdj, KOFFiGIdf,	KOFFiGIdj,	
#  KOFIpGIdf,	KOFIpGIdj, KOFInGIdf,	KOFInGIdj, KOFCuGIdf,	KOFCuGIdj,
#  KOFPoGIdf,	KOFPoGIdj,
#                    unemploy:Quality))
# 


rm(list=ls())
getwd()


list_trt=c("KOFTrGIdf",	"KOFTrGIdj", "KOFFiGIdf",	"KOFFiGIdj",	
           "KOFIpGIdf",	"KOFIpGIdj", "KOFInGIdf",	"KOFInGIdj", "KOFCuGIdf",	"KOFCuGIdj",
           "KOFPoGIdf",	"KOFPoGIdj")

all_libraries= c("SL.mean","SL.glm","SL.step.forward"
                 ,"SL.glmnet","SL.earth"#,"SL.gam"
                 ,"SL.ranger"#, "SL.xgboost"
)

library(doParallel)

#cluster your analysis
{
  num_cores <- detectCores()
  cl <- makeCluster(num_cores-2)
  clusterEvalQ(cl,c(library(lmtp),library(dplyr)))
  clusterExport(cl, list("all_libraries"))
  
  try_this <- function(x, list_trt) {
    
    #which trt
    which_trt=list_trt[x]
    
    #PREPARE MATRIX
    loaded_data=load(file="OUD_Astd_AllGI_DALYs.RData") 
    title_here=unique(final_matrix$cause)
    
    #only small locations (MOnaco, Miu, Vanuatu, Taiwan etc ...) are missing
    Y_0_matrix<-final_matrix %>%
      filter(year==1990) %>%
      mutate(Y_0=Y) %>%
      select(c(location,Y_0)) 
    
    A_W_matrix <- final_matrix %>% #it should be A_W_matrix
      filter(year==2018) %>% #2018
      select(-c(Y,unhappy)) 
    
    Y_matrix <- final_matrix %>%
      filter(year==2019) %>%
      select(c(location,Y)) 
    
    #log Y
    #numerize V strata
    final_matrix_2019 <- Y_matrix %>%
      left_join(Y_0_matrix,by=c("location")) %>%
      left_join(A_W_matrix,by=c("location")) %>%
      mutate(log_Y=log(Y),
             log_Y_0=log(Y_0)) %>%
      rename(trt_OI=which_trt)
    
    #run a complete cases function to identify participating locations
    final_matrix_2019=final_matrix_2019[complete.cases(final_matrix_2019),]
    
    numbers_of_bins=4
    final_matrix_quality_2019 <- final_matrix_2019 %>% 
      filter(Quality>=3) %>%
      mutate(Quality = Quality - 3,
             categ_KOFGI_labels=cut(trt_OI,
                                    breaks=unique(quantile(trt_OI,probs=seq(0,1,by=1/numbers_of_bins))),
                                    labels = NULL,
                                    include.lowest = TRUE),
             categ_KOFGI=cut(trt_OI,
                             breaks=unique(quantile(trt_OI,probs=seq(0,1,by=1/numbers_of_bins))),
                             labels = FALSE,
                             include.lowest = TRUE)
      )

    
    W=colnames(select(final_matrix_quality_2019,c(unemploy:Quality,log_Y_0)))
    
    #static categ
    static_categ_1 <- function(data,trt) { rep(1,length(data[[trt]])) }
    static_categ_2 <- function(data,trt) { rep(2,length(data[[trt]])) }
    static_categ_3 <- function(data,trt) { rep(3,length(data[[trt]])) }
    static_categ_4 <- function(data,trt) { rep(4,length(data[[trt]])) }
    
   
    lmtp_psi <- function(trt,shift,intervention_type, data) {
      psi <-  lmtp_tmle(data=data, 
                        outcome="log_Y",
                        trt=trt, #KOFGI or categ_KOFGI,
                        outcome_type="continuous", 
                        baseline = W,
                        time_vary = NULL, 
                        shift = shift,
                        learners_outcome=all_libraries,
                        learners_trt = all_libraries,
                        #k = 1, 
                        folds = 20, 
                        intervention_type = intervention_type)
    }
    
    ### Run MSM categ
    psi_1<-lmtp_psi("categ_KOFGI",static_categ_1,"static", data=final_matrix_quality_2019)
    psi_2<-lmtp_psi("categ_KOFGI",static_categ_2,"static", data=final_matrix_quality_2019)
    psi_3<-lmtp_psi("categ_KOFGI",static_categ_3,"static", data=final_matrix_quality_2019)
    psi_4<-lmtp_psi("categ_KOFGI",static_categ_4,"static", data=final_matrix_quality_2019)
    
    
    results_MSM_categ<-lmtp_contrast(psi_2,psi_3,psi_4,ref=psi_1)
    
    return(list(psi_1, psi_2,psi_3,psi_4,
                results_MSM_categ,
                final_matrix_quality_2019))
  }
  
  system.time(psi_results_data <-clusterApply(cl,1:length(list_trt),try_this,list_trt=list_trt))
  
  stopCluster(cl)
}

save(psi_results_data,
     file="results_lmtp_Quality_sensit_A.RData")

for (i in 1:12) {
  print(list_trt[i])
  print(psi_results_data[[i]][[5]])
}
