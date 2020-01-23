
#define parameters
setwd("C:/Users/feldhaus/Desktop/Dokumente/01_ERC/07_Results/03_Analysis_717/08_ElasticNet")
nr_it = 100              #number of iterations for bootstrapping
nr_cv_folds = 20          #number of cross-validation folds

#library
library(softImpute)
library(reshape2)
library(glmnet)
library(ggplot2)
library(readr)
library(xlsx)
library(ggrepel)
library(readxl)
library(corrplot)

#load data
load("C:/Users/feldhaus/Desktop/Dokumente/01_ERC/07_Results/03_Analysis_717/09_PCA/PCs_Quest_PCAs_oblimin_principal_notc_622.RData")

cond     <- c("PlacExp", "NocExp", "PlacCond", "NocCond")


#filter for outlier participants
spss_lf2       <- cbind(spss_lf2,PCs_quest_lf,rot_PCs_QST,rot_PCs_nBack)
spss_lf_filt   <- spss_lf2[which(spss_lf2$filter_newQST == 0 & spss_lf2$Block == 1),]

#define parameters
Preds_org      <- colnames(spss_lf2)[c(416:472,497:507,473:481)]
#define parameters
Preds          <- c(colnames(spss_lf_filt)[507:549], colnames(spss_lf_filt)[c(224,228,231)])
Preds_colms    <- c(507:549,224,228,231)
Preds_Quest    <- 1:34
Preds_QST      <- 35:40
Preds_nBack    <- 41:43
Preds_other    <- 44:46

Preds_intercorr = spss_lf_filt[,Preds]
corrplot(cor(Preds_intercorr,use = "pairwise.complete.obs"), tl.cex =.7, tl.col = 'black')
names(Preds_intercorr)[c(5, 14, 18,19,20,21,22,33,41,42,43)] <- c("CES-D", "DPQ","GEPAQ-F",
                                                                  "GEPAQ-M", "GKE", "ACS1", 
                                                                  "ACS2", "SES-17", "n-Back Hyperalgesia",
                                                                  "n-back1", "n-back2") 
Preds_intercorr_sort <- Preds_intercorr[,c(35:40,41:43,21,22,6,7,8,9:13,5,14:20,23:34,1:4,44:46)]
corrplot(cor(Preds_intercorr_sort,use = "pairwise.complete.obs"), tl.cex =.7, tl.col = 'black')


#scale parameters
spss_lf_centering <- data.frame(scale(spss_lf_filt[,c(507:549,224,228,231)], center = T, scale = T))
spss_lf_centered  <- data.frame(spss_lf_filt[,c(22,23,26,27,37,220)],spss_lf_centering)

#create a matrix with Predictors 
just_preds <- spss_lf_centered

#standards for all modalities
vec_id     <- c(array(1,34), array(2,6), array(3,3), array(4,3))

###### LASSO for each Modality
for (nr_mod in 1:4){
  mses       <- numeric(nr_it)
  mins       <- numeric(nr_it)
  maxes      <- numeric(nr_it)
  coefs      <- matrix(NA,nr_it,length(Preds))
  coefs_shuff<- matrix(NA,nr_it,length(Preds))
  pred_ys    <- matrix(NA,nr_it,dim(just_preds)[1])
  sse_rsq    <- matrix(NA,nr_it,2)
  #Loop for iterations
  
  for(i in 1:nr_it){
    #imputation using softImpute (by Hastie) to estimate missing values
    fits           <- softImpute(just_preds,trace=F, type="svd", maxit = 1000)
    compl_matr     <- complete(just_preds,fits)
    variable_names <- colnames(compl_matr)
    
    #regress out experimenter and CalibVAS
    fit14 <- lm(compl_matr[,1]~compl_matr$Experimenter+compl_matr$CalibVAS60)
    fit15 <- lm(compl_matr[,2]~compl_matr$Experimenter+compl_matr$CalibVAS60)
    fit16 <- lm(compl_matr[,3]~compl_matr$Experimenter+compl_matr$CalibVAS60)
    fit17 <- lm(compl_matr[,4]~compl_matr$Experimenter+compl_matr$CalibVAS60)
    # summary(fit14)
    y1 <- fit14$residuals
    y2 <- fit15$residuals
    y3 <- fit16$residuals
    y4 <- fit17$residuals
    # plot(compl_matr[,1]~y1)
    ys_nosc     <- data.frame(y1,y2,y3,y4)
    
    #scale all variables (preds and y-variables)
    x           <- scale(compl_matr[,-c(1:6)]) 
    ys          <- scale(ys_nosc)
    
    #delete variables which are not needed
    rm(y1,y2,y3,y4)
    rm(fit14,fit15,fit16,fit17)
    
    #define data to use
    y           <- as.numeric(ys[,nr_mod])
    
    #Sum of Squares Total
    sst_y      <- sum(y^2)
    
    #LASSO MAIN PART
    cvfits    <- cv.glmnet(x, y, alpha=1, nfolds=nr_cv_folds)
    loc       <- which(cvfits$lambda==cvfits$lambda.min)
    maxes[i]  <- max(cvfits$lambda)
    mins[i]   <- min(cvfits$lambda)
    mses[i]   <- cvfits$cvm[loc]
    c_i       <- coef(cvfits,cvfits$lambda.min)
    coefs[i,c_i@i[-1]] <- c_i@x[-1]
    #if you want to calculate the effect size
    #cvfits$glmnet.fit$dev.ratio[which(cvfits$glmnet.fit$lambda == cvfits$lambda.min)]
    #calculate the predicted values
    pred_y      <- predict(cvfits, s = cvfits$lambda.min, newx = x)
    pred_ys[i,] <- pred_y
    #Sum of Squares Error
    sse         <- sum((pred_y - y)^2)
    #R squared
    rsq         <- 1 - sse/sst_y
    sse_rsq[i,] <- c(sse,rsq)
    
    #check status
    print(paste0("Nr",nr_mod,", NrIt",i))
  }
  #What are the average coefficients?
  a_na_igno   <- colMeans(coefs, na.rm = T, dims = 1)
  coefs[is.na(coefs)] <- 0 ### taking 0s instead of NAs!
  a           <- colMeans(coefs, na.rm = T, dims = 1)
  v_coef      <- data.frame(x1=1:dim(x)[2], a_abs = abs(a), a = a, a_na_igno = a_na_igno,
                            vec_id, variable_names[-c(1:6)]) 
  plot1       <- ggplot(v_coef, aes(x=x, y=a))+
    geom_point(shape=2)
  #To keep for each modality
  assign(paste0("coefs",nr_mod),coefs)
  assign(paste0("plot1_",nr_mod),plot1)
  assign(paste0("v_coef",nr_mod),v_coef)
  assign(paste0("sse_rsq",nr_mod),sse_rsq)
  assign(paste0("pred_ys",nr_mod),pred_ys)
}

mean(sse_rsq2[,2])
sd(sse_rsq2[,2])
cor(ys_nosc[,2],rowMeans(t(pred_ys2)), use="pairwise.complete.obs") 
cor.test(ys_nosc[,2],rowMeans(t(pred_ys2)), use="pairwise.complete.obs")$p.value
plot(ys_nosc[,2],rowMeans(t(pred_ys2)))
abline(lm(rowMeans(t(pred_ys2))~ys_nosc[,2]), col = 'blue')

mean(sse_rsq3[,2])
sd(sse_rsq3[,2])
cor(ys_nosc[,4],rowMeans(t(pred_ys3)), use="pairwise.complete.obs") 
cor.test(ys_nosc[,4],rowMeans(t(pred_ys3)), use="pairwise.complete.obs")$p.value
plot(ys_nosc[,4],rowMeans(t(pred_ys3)))
abline(lm(rowMeans(t(pred_ys3))~ys_nosc[,3]), col = 'blue')

mean(sse_rsq4[,2])
sd(sse_rsq4[,2])
cor(ys_nosc[,4],rowMeans(t(pred_ys4)), use="pairwise.complete.obs") 
cor.test(ys_nosc[,4],rowMeans(t(pred_ys4)), use="pairwise.complete.obs")$p.value
plot(ys_nosc[,4],rowMeans(t(pred_ys4)))
abline(lm(rowMeans(t(pred_ys4))~ys_nosc[,4]), col = 'blue')

rm(c_i,cvfits,fits,plot1,pred_y,v_coef,a,i,loc,maxes,mins,mses,nr_mod,rsq,sse,sst_y,y)

# count how often variables were selected in LASSO

for (nr_mod2 in 1:4){
  coefs      <- get(paste0("coefs",nr_mod2))
  NrItChosen <- colSums(coefs !=0)
  v_coef     <- cbind(get(paste0("v_coef",nr_mod2)),NrItChosen)
  assign(paste0("v_coef",nr_mod2),v_coef)
}

for (nr_mod2 in 1:4){
  v_coef     <- get(paste0("v_coef",nr_mod2))
  v_coef_ord <- data.frame(ord = 1:dim(v_coef)[1], v_coef[order(-v_coef$a_abs),])
  cols       <- rep("green", dim(v_coef_ord)[1])
  cols[v_coef_ord$vec_id == 2] <- "blue"
  cols[v_coef_ord$vec_id == 3] <- "red"
  cols[v_coef_ord$vec_id == 4] <- "grey20"
  nona <- length(which(!is.na(v_coef_ord$a_abs)))
  plot2      <- ggplot(v_coef_ord, aes(x=ord, y=a_abs))+
    geom_bar(stat="identity", fill=cols[1:nona])+
    coord_cartesian(ylim = c(0, 0.15),xlim = c(0, length(Preds))) +
    #annotate("text", x = 75, y = 0.035, 
    #         label = paste0("R? = ", round(mean(sse_rsq1[,2]),3)), cex = 5) +
    ggtitle(paste0("LASSO - ", cond[nr_mod2])) +
    labs(x="Order by Coefficient Value", y="Coefficient Value") + 
    theme_classic(base_size = 16)
  assign(paste0("plot2_",nr_mod2),plot2)
  assign(paste0("v_coef_ord",nr_mod2),v_coef_ord)
}

# better plot NR it Chosen

for (nr_mod2 in 1:4){
  v_coef     <- get(paste0("v_coef",nr_mod2))
  v_coef_ord <- data.frame(ord = 1:dim(v_coef)[1], v_coef[order(-v_coef$NrItChosen),])
  cols       <- rep("green", dim(v_coef_ord)[1])
  cols[v_coef_ord$vec_id == 2] <- "blue"
  cols[v_coef_ord$vec_id == 3] <- "red"
  cols[v_coef_ord$vec_id == 4] <- "grey20"
  nona <- length(which(!is.na(v_coef_ord$a_abs)))
  plot2      <- ggplot(v_coef_ord, aes(x=ord, y=NrItChosen))+
    geom_bar(stat="identity", fill=cols[1:nona])+
    coord_cartesian(ylim = c(0, nr_it),xlim = c(0, length(Preds))) +
    #annotate("text", x = 75, y = 0.035, 
    #         label = paste0("R? = ", round(mean(sse_rsq1[,2]),3)), cex = 5) +
    ggtitle(paste0("LASSO - ", cond[nr_mod2])) +
    labs(x="Order by Coefficient Value", y="Coefficient Value") + 
    theme_minimal(base_size = 16)
  assign(paste0("plot3_",nr_mod2),plot2)
  assign(paste0("v_coef_ord",nr_mod2),v_coef_ord)
}

# export coefficients
res4lasso <- rbind(v_coef_ord1,v_coef_ord3,v_coef_ord2,v_coef_ord4)
write.xlsx(res4lasso, "LASSO_newQST_PCs_rotated_100it_20fold.xlsx")

