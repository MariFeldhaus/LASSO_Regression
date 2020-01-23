# plot LASSO next to each other 

library(grid)
library(reshape2)
library(ggplot2)

setwd("C:/Users/feldhaus/Desktop/Dokumente/05_Diss/01_plots_study1/")
load("NR_Chosen2_all.RData")
load("Coefs_all.RData")
load("NR_Chosen2_Sex1.RData")
load("Coefs_sex1.RData")
load("NR_Chosen2_Sex2.RData")
load("Coefs_sex2.RData")
load("NR_Chosen2_Exp1.RData")
load("Coefs_exp1.RData")
load("NR_Chosen2_Exp2.RData")
load("Coefs_exp2.RData")

#add sex variable to sex subsubsets
NR_Chosen2_Sex1$x1 <- as.numeric(NR_Chosen2_Sex1$x1)
NR_Chosen2_Sex1 <- rbind(NR_Chosen2_Sex1[order(NR_Chosen2_Sex1$x1),][1:43,],
                         c(44,'Sex',0,0,0,0,0,'Sex','128.Sex'),
                         NR_Chosen2_Sex1[order(NR_Chosen2_Sex1$x1),][44:45,])
NR_Chosen2_Sex1[45,1] = 45
NR_Chosen2_Sex1[46,1] = 46
NR_Chosen2_Sex1[,2] = NR_Chosen2_all[order(NR_Chosen2_all$x1),2]

coefs1_sex1 <- cbind(coefs1_sex1[,1:43],rep(0,1000),coefs1_sex1[,44:45])
coefs2_sex1 <- cbind(coefs2_sex1[,1:43],rep(0,1000),coefs2_sex1[,44:45])
coefs3_sex1 <- cbind(coefs3_sex1[,1:43],rep(0,1000),coefs3_sex1[,44:45])
coefs4_sex1 <- cbind(coefs4_sex1[,1:43],rep(0,1000),coefs4_sex1[,44:45])

NR_Chosen2_Sex2$x1 <- as.numeric(NR_Chosen2_Sex2$x1)
NR_Chosen2_Sex2 <- rbind(NR_Chosen2_Sex2[order(NR_Chosen2_Sex2$x1),][1:43,],
                         c(44,'Sex',0,0,0,0,0,'Sex','128.Sex'),
                         NR_Chosen2_Sex2[order(NR_Chosen2_Sex2$x1),][44:45,])
NR_Chosen2_Sex2[45,1] = 45
NR_Chosen2_Sex2[46,1] = 46
NR_Chosen2_Sex2[,2] = NR_Chosen2_all[order(NR_Chosen2_all$x1),2]

coefs1_sex2 <- cbind(coefs1_sex2[,1:43],rep(0,1000),coefs1_sex2[,44:45])
coefs2_sex2 <- cbind(coefs2_sex2[,1:43],rep(0,1000),coefs2_sex2[,44:45])
coefs3_sex2 <- cbind(coefs3_sex2[,1:43],rep(0,1000),coefs3_sex2[,44:45])
coefs4_sex2 <- cbind(coefs4_sex2[,1:43],rep(0,1000),coefs4_sex2[,44:45])

NR_Chosen2 <- NR_Chosen2_all

# different order PredName4 for new plot
NR_Chosen3 <- NR_Chosen2[order(NR_Chosen2$x1),]
NR_Chosen3$PredGroup <- c(rep(1,34),rep(2,6),rep(3,3),rep(4,3))
a <- c(NR_Chosen3[order(-NR_Chosen3[which(NR_Chosen3$PredGroup == 1),7]),8], 
       NR_Chosen3[(order(-NR_Chosen3[which(NR_Chosen3$PredGroup == 2),7]) 
                   +length(which(NR_Chosen3$PredGroup == 1))),8],
       NR_Chosen3[(order(-NR_Chosen3[which(NR_Chosen3$PredGroup == 3),7])
                   +length(which(NR_Chosen3$PredGroup == 1))
                   +length(which(NR_Chosen3$PredGroup == 2))),8],
       NR_Chosen3[(order(-NR_Chosen3[which(NR_Chosen3$PredGroup == 4),7])
                   +length(which(NR_Chosen3$PredGroup == 1))
                   +length(which(NR_Chosen3$PredGroup == 2))
                   +length(which(NR_Chosen3$PredGroup == 3))),8])
for (i in 100:(100+length(a)[1])){
  a[(i-100)] <- paste0(i,". ", a[(i-100)])}

order1 <- c(order(-NR_Chosen3[which(NR_Chosen3$PredGroup == 1),7]),
            (order(-NR_Chosen3[which(NR_Chosen3$PredGroup == 2),7]) 
             +length(which(NR_Chosen3$PredGroup == 1))),
            (order(-NR_Chosen3[which(NR_Chosen3$PredGroup == 3),7])
             +length(which(NR_Chosen3$PredGroup == 1))
             +length(which(NR_Chosen3$PredGroup == 2))),
            (order(-NR_Chosen3[which(NR_Chosen3$PredGroup == 4),7])
             +length(which(NR_Chosen3$PredGroup == 1))
             +length(which(NR_Chosen3$PredGroup == 2))
             +length(which(NR_Chosen3$PredGroup == 3))))
NR_Chosen3 <- NR_Chosen3[order1,]
NR_Chosen3$PredName4 <- a
#EBs2 <- data.frame(v_coef_ord1[order(v_coef_ord1$x1),][,c(2,7)])
EBs2 <- data.frame(NR_Chosen2[order(NR_Chosen2[1]),][,c(1,2)])
colnames(EBs2)[2] <- "PredName"
EBs2$ErrorbarPE <- apply(coefs1,2, FUN = "sd")
EBs2$ErrorbarPC <- apply(coefs3,2, FUN = "sd")
EBs2$ErrorbarNE <- apply(coefs2,2, FUN = "sd")
EBs2$ErrorbarNC <- apply(coefs4,2, FUN = "sd")
EBs2 <- EBs2[NR_Chosen3$x1,]
CumVals3 <- data.frame(NR_Chosen3$PredName4)
CumVals3$absPE2 <- NR_Chosen3$PE
CumVals3$absPC2 <- NR_Chosen3$NE + NR_Chosen3$NC + NR_Chosen3$PC
CumVals3$absNE2 <- NR_Chosen3$NE + NR_Chosen3$NC
CumVals3$absNC2 <- NR_Chosen3$NC

# long format for stacked barplot
NR_Chosen_lf3 <- melt(NR_Chosen3, id.vars = c("x1", "PredName", "PredName2","PredName3","PredName4","PredGroup", "NrOverall"))
EBs_lf3 <-  melt(EBs2, id.vars = c("x1", "PredName"))
CumVals4 <- melt(CumVals3, id.vars = "NR_Chosen3.PredName4")
NR_Chosen_lf3$EBslow  <- CumVals4$value - EBs_lf3$value
NR_Chosen_lf3$EBshigh <- CumVals4$value + EBs_lf3$value
NR_Chosen_lf3$rank <- 1:dim(NR_Chosen_lf3)[1]

##

g1_all <- ggplot(NR_Chosen_lf3) + 
  geom_bar(aes(x = PredName4, y = value, fill = variable), stat = "identity") + 
  geom_errorbar(data = NR_Chosen_lf3, mapping = aes(x = PredName4, ymin = EBslow,ymax = EBshigh),width = 0, size = 0.5) +
  scale_x_discrete(labels=c(NR_Chosen3$PredName2)) +
  theme_classic(base_size = 10) + 
  scale_y_continuous(sec.axis = dup_axis(),limits = c(-.2,.2)) +
  scale_fill_manual(values=c("#92c5de", "#0571b0", "#f4a582", "#ca0020" )) +
  theme(axis.text.x = element_text(hjust = 1,vjust = 0.5, angle = 90)) +
  xlab("") + ylab("LASSO coefficient value") 


# delete non-selected predictors and change order
NR_Chosen4 <- NR_Chosen2[order(NR_Chosen2$x1),]
NR_Chosen4$PredGroup <- c(rep(4,34),rep(1,6),rep(2,3),rep(3,3))
#NR_Chosen4 <- NR_Chosen4[-c(which(NR_Chosen4$NrOverall == 0)),]

order2 <- c((order(-NR_Chosen4[which(NR_Chosen4$PredGroup == 1),7]) 
             +length(which(NR_Chosen4$PredGroup == 4))),
            (order(-NR_Chosen4[which(NR_Chosen4$PredGroup == 2),7])
             +length(which(NR_Chosen4$PredGroup == 4))
             +length(which(NR_Chosen4$PredGroup == 1))),
            (order(-NR_Chosen4[which(NR_Chosen4$PredGroup == 3),7])
             +length(which(NR_Chosen4$PredGroup == 4))
             +length(which(NR_Chosen4$PredGroup == 1))
             +length(which(NR_Chosen4$PredGroup == 2))),
            order(-NR_Chosen4[which(NR_Chosen4$PredGroup == 4),7]))
NR_Chosen4 <- NR_Chosen4[order2,]
a2 <- NR_Chosen4$PredName2
for (i in 100:(100+length(a2)[1])){
  a2[(i-100)] <- paste0(i,". ", a2[(i-100)])}
NR_Chosen4$PredName4 <- a2

EBs3 <- data.frame(NR_Chosen2[order(NR_Chosen2[1]),][,c(1,2)])
colnames(EBs3)[2] <- "PredName"
EBs3$ErrorbarPE <- apply(coefs1,2, FUN = "sd")
EBs3$ErrorbarPC <- apply(coefs3,2, FUN = "sd")
EBs3$ErrorbarNE <- apply(coefs2,2, FUN = "sd")
EBs3$ErrorbarNC <- apply(coefs4,2, FUN = "sd")
EBs3 <- EBs3[NR_Chosen4$x1,]
CumVals4 <- data.frame(NR_Chosen4$PredName4)
CumVals4$absPE2 <- NR_Chosen4$PE
CumVals4$absPC2 <- NR_Chosen4$NE + NR_Chosen4$NC + NR_Chosen4$PC
CumVals4$absNE2 <- NR_Chosen4$NE + NR_Chosen4$NC
CumVals4$absNC2 <- NR_Chosen4$NC

# long format for stacked barplot
NR_Chosen_lf4 <- melt(NR_Chosen4, id.vars = c("x1", "PredName", "PredName2","PredName3","PredName4","PredGroup", "NrOverall"))
EBs_lf4       <- melt(EBs3, id.vars = c("x1", "PredName"))
CumVals_lf4   <- melt(CumVals4, id.vars = "NR_Chosen4.PredName4")
NR_Chosen_lf4$EBslow  <- CumVals_lf4$value - EBs_lf4$value
NR_Chosen_lf4$EBshigh <- CumVals_lf4$value + EBs_lf4$value
NR_Chosen_lf4$rank <- 1:dim(NR_Chosen_lf4)[1]

g2_all <- ggplot(NR_Chosen_lf4) + 
  geom_bar(aes(x = PredName4, y = value, fill = variable), stat = "identity", width = .7) + 
  geom_errorbar(data = NR_Chosen_lf4, mapping = aes(x = PredName4, ymin = EBslow,ymax = EBshigh),width = 0, size = 0.5) +
  theme_light(base_size = 10) + 
  scale_y_continuous(sec.axis = dup_axis(),limits = c(-.2,.2)) +
  scale_fill_manual(values=c("#92c5de", "#0571b0", "#f4a582", "#ca0020" )) +
  theme(axis.text.x = element_text(hjust = 1,vjust = 0.5, angle = 90, size = 9),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank(),
        strip.text.y = element_blank()) +
  xlab("") + ylab("LASSO coefficient value")  +
  facet_grid(.~PredGroup, scale="free_x", space="free") 
g2_all


