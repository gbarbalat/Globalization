# run analysis based on outcomes without outliers
# see process_all_quality for more detail

rm(list=ls())
getwd()
library(doParallel)

list_files=list.files(pattern = "Astd_DALYs.RData")#

all_libraries= c("SL.mean","SL.glm","SL.step.forward"
                 ,"SL.glmnet","SL.earth"#,"SL.gam"
                 ,"SL.ranger"#, "SL.xgboost"
)



#cluster your analysis
{
  num_cores <- detectCores()
  cl <- makeCluster(num_cores-1)
  clusterEvalQ(cl,c(library(lmtp),library(dplyr)))
  clusterExport(cl, list("all_libraries"))
  
  try_this <- function(x, list_files) {
    
    #PREPARE MATRIX
    loaded_data=load(file=list_files[x]) 
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
    final_matrix_2019 <- Y_matrix %>%
      left_join(Y_0_matrix,by=c("location")) %>%
      left_join(A_W_matrix,by=c("location")) %>%
      mutate(log_Y=log(Y),
             log_Y_0=log(Y_0))
    
    #run a complete cases function 
    final_matrix_2019=final_matrix_2019[complete.cases(final_matrix_2019),]
    
    numbers_of_bins=4
    final_matrix_quality_2019 <- final_matrix_2019 %>% 
      filter(Quality>=3) %>%
      mutate(Quality = Quality - 3,
             categ_KOFGI_labels=cut(KOFGI,
                                    breaks=unique(quantile(KOFGI,probs=seq(0,1,by=1/numbers_of_bins))),
                                    labels = NULL,
                                    include.lowest = TRUE),
             categ_KOFGI=cut(KOFGI,
                             breaks=unique(quantile(KOFGI,probs=seq(0,1,by=1/numbers_of_bins))),
                             labels = FALSE,
                             include.lowest = TRUE)
      )%>%
      fastDummies::dummy_cols(select_columns = "categ_KOFGI") %>%
      mutate(bin_KOFGI=case_when(
        KOFGI>median(KOFGI, na.rm=TRUE) ~ 1,#categ_KOFGI_1==1 ~ 0,
        KOFGI<=median(KOFGI, na.rm=TRUE) ~ 0#categ_KOFGI_1==0 ~ 1
      ),
      bin2_KOFGI=case_when(
        categ_KOFGI_1==1 ~ 0,
        categ_KOFGI_1==0 ~ 1
      )
      )
    
    W=colnames(select(final_matrix_quality_2019,c(unemploy:Quality,log_Y_0)))
    
    #static categ
    static_categ_1 <- function(data,trt) { rep(1,length(data[[trt]])) }
    static_categ_2 <- function(data,trt) { rep(2,length(data[[trt]])) }
    static_categ_3 <- function(data,trt) { rep(3,length(data[[trt]])) }
    static_categ_4 <- function(data,trt) { rep(4,length(data[[trt]])) }
    
    
    lmtp_psi <- function(trt,shift,intervention_type, data, trim) {
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
                        intervention_type = intervention_type,
                        .trim=trim)
    }
    
    ### take out outliers
    final_matrix_quality_2019 <- final_matrix_quality_2019 %>%
      filter(log_Y<quantile(log_Y,0.99),
             log_Y>quantile(log_Y,0.01))
    
    ### Run MSM categ with different levels of trimming density ratios (propensity weights)
    psi_1<-lmtp_psi("categ_KOFGI",static_categ_1,"static", final_matrix_quality_2019,0.999)
    psi_2<-lmtp_psi("categ_KOFGI",static_categ_2,"static", final_matrix_quality_2019,0.999)
    psi_3<-lmtp_psi("categ_KOFGI",static_categ_3,"static", final_matrix_quality_2019,0.999)
    psi_4<-lmtp_psi("categ_KOFGI",static_categ_4,"static", final_matrix_quality_2019,0.999)
    
    psi_1_t1<-lmtp_psi("categ_KOFGI",static_categ_1,"static", final_matrix_quality_2019,0.990)
    psi_2_t1<-lmtp_psi("categ_KOFGI",static_categ_2,"static", final_matrix_quality_2019,0.990)
    psi_3_t1<-lmtp_psi("categ_KOFGI",static_categ_3,"static", final_matrix_quality_2019,0.990)
    psi_4_t1<-lmtp_psi("categ_KOFGI",static_categ_4,"static", final_matrix_quality_2019,0.990)
    
    psi_1_t2<-lmtp_psi("categ_KOFGI",static_categ_1,"static", final_matrix_quality_2019,0.975)
    psi_2_t2<-lmtp_psi("categ_KOFGI",static_categ_2,"static", final_matrix_quality_2019,0.975)
    psi_3_t2<-lmtp_psi("categ_KOFGI",static_categ_3,"static", final_matrix_quality_2019,0.975)
    psi_4_t2<-lmtp_psi("categ_KOFGI",static_categ_4,"static", final_matrix_quality_2019,0.975)
    
    results_MSM_categ<-lmtp_contrast(psi_2,psi_3,psi_4,ref=psi_1)
    results_MSM_categ_t1<-lmtp_contrast(psi_2_t1,psi_3_t1,psi_4_t1,ref=psi_1_t1)
    results_MSM_categ_t2<-lmtp_contrast(psi_2_t2,psi_3_t2,psi_4_t2,ref=psi_1_t2)

    
    return(list(psi_1, psi_2,psi_3,psi_4,
                psi_1_t1, psi_2_t1,psi_3_t1,psi_4_t1,
                psi_1_t2, psi_2_t2,psi_3_t2,psi_4_t2,
                results_MSM_categ,results_MSM_categ_t1,results_MSM_categ_t2,
                final_matrix_quality_2019))
  }
  
  system.time(psi_results_data <-clusterApply(cl,1:length(list_files),try_this,list_files=list_files))
  
  stopCluster(cl)
}

save(psi_results_data,
     file="results_lmtp_Quality_sensit_Y.RData")
