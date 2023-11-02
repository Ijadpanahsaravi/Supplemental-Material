
setwd("F:/total/test3")
library(ggplot2)
##All co-culture files combined as a mixtotal.csv
mixtot <- read.csv("mixtotal.csv")
mixtot.t0 <- mixtot[mixtot$time == 0,]
write.csv(mixtot.t0, "mixtot.t0.csv" )
#colnames(mixtot)
#unique(mixtot$species)

##All mono-cultured data sets merged as a total.csv 
tot <- read.csv("total.csv")
tot.t0 <- tot[tot$time == 0,]
write.csv(tot.t0, "tot.t0.csv" )
#colnames(tot)
#unique(tot$species)

## select 1000 number spores of each species randomly 
#unique(tot.t0$species)
no.of.obs <- table(tot.t0$species)
nig1000 <- tot.t0[tot.t0$species == "A.niger", ][sample(1:no.of.obs["A.niger"] ,1000),]
nid1000 <- tot.t0[tot.t0$species == "A.nidulans", ][sample(1:no.of.obs["A.nidulans"] ,1000),]
cla1000 <- tot.t0[tot.t0$species == "A.clavatus", ][sample(1:no.of.obs["A.clavatus"] ,1000),]
ory1000 <- tot.t0[tot.t0$species == "A.oryzae", ][sample(1:no.of.obs["A.oryzae"] ,1000),]
ter1000 <- tot.t0[tot.t0$species == "A.terreus", ][sample(1:no.of.obs["A.terreus"] ,1000),]


score.species <- function(use.pop,speset1,speset2){
  spe.score <- matrix(NA,nrow=nrow(use.pop),ncol=2)
  for ( i in 1:nrow(use.pop)){
    spe1.dist <- sqrt(((use.pop$Area[i]/400 - speset1$Area/400)^2) + ((use.pop$Contrast[i] - speset1$Contrast)^2))
    spe2.dist <- sqrt(((use.pop$Area[i]/400 - speset2$Area/400)^2) + ((use.pop$Contrast[i] - speset2$Contrast)^2))
    spe.choise <- rep(c("Spe_1","Spe_2"),each=1000)
    # c(nig.dist,nid.dist)[order(c(nig.dist,nid.dist))]
    spe1.score <- sum(spe.choise[order(c(spe1.dist,spe2.dist))][1:100] == "Spe_1")
    spe2.score <- sum(spe.choise[order(c(spe1.dist,spe2.dist))][1:100] == "Spe_2")
    spe.score[i,1] <- spe1.score
    spe.score[i,2] <- spe2.score
    
  }
  outp <- data.frame(use.pop,spe.score)
  return(outp)
}

#######
# A.niger co-cultured with A.nidulans
nignid <- mixtot.t0[mixtot.t0$species == "nig/nid",]
ok1 <- score.species(nignid,nig1000,nid1000)

ggplot(ok1)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size= 0.5)+xlim(50, 180)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+
theme_classic(base_size = 25)+labs(title="")+ 
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))


selc <- ok1$X1-50 > 45
#ggplot(ok1[selc,])+
  #geom_jitter(aes(Area,Contrast,col=X1),height = 0.001,width = 0.1, size= 0.5)+xlim(50, 180)+ ylim(0.05,0.50)+
  #scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+
  #theme_classic(base_size = 25)+labs(title="")+
  #theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))


my.number <- nignid[selc,]$ObjectId
mixtot <- read.csv("mixtotal.csv")
mixnignid <- mixtot[mixtot$species == "nig/nid",]
mixnignid2  <- mixnignid [mixnignid $ObjectId %in% my.number,]
write.csv(mixnignid2 , "nigselectedwithnid.csv")

nmix <- read.csv("nigselectedwithnid.csv")
nmix <- nmix[nmix$time == 0,]

p <- ggplot(ok1)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size= 0.5)+xlim(50, 180)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+
  theme_classic(base_size = 25)
p + geom_jitter(data= nmix ,aes (Area,Contrast),color = "navy"  ,height = 0.001,width = 0.1, size= 0.5)+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"))

##A.niger assumed co-cultured with A.nidulans(A. niger selection with A. nidulans)
nig <- tot.t0[tot.t0$species == "A.niger",]
ok <- score.species(nig,nig1000,nid1000)

ggplot(ok)+
  geom_jitter(aes(Area,Contrast,col=X1),height = 0.001,width = 0.1, size = 0.5 )+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+ theme_classic(base_size = 25)
  
ggplot(ok)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size = 0.5)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+ theme_classic(base_size = 25)
  theme_classic(base_size = 25)+labs(title="")

selc <- ok$X1-50 > 45
ggplot(ok[selc,])+
  geom_jitter(aes(Area,Contrast,col=X1),height = 0.001,width = 0.1, size = 0.5)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+
  theme_classic(base_size = 25)+labs(title="")

my.number <- nig[selc,]$ObjectId
tot <- read.csv("total.csv")
nig_assum_nid <- tot[tot$species == "A.niger",]
nig_assum_nid2 <- nig_assum_nid[nig_assum_nid$ObjectId %in% my.number,]
write.csv(nig_assum_nid2, "nigassumedwithnid.csv")

n <- read.csv("nigassumedwithnid.csv")
n <- n[n$time == 0,]

p <- ggplot(ok)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size= 0.5)+xlim(50, 180)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)
p + geom_jitter(data= n ,aes (Area,Contrast),color = "navy" ,height = 0.001,width = 0.1, size= 0.5 )+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"))


############################################################3
##A.niger co-cultered with A.terreus
nigter<- mixtot.t0[mixtot.t0$species == "nig/ter",]
ok1 <- score.species(nigter,nig1000,ter1000)

ggplot(ok1)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size= 0.5)+xlim(50, 180)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+
  theme_classic(base_size = 25)+labs(title="")+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))


selc <- ok1$X1-50 > 45
ggplot(ok1[selc,])+
  geom_jitter(aes(Area,Contrast,col=X1),height = 0.001,width = 0.1, size =0.5)+xlim(50, 180)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+
  theme_classic(base_size = 25)+labs(title="")+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))

my.number <- nigter[selc,]$ObjectId
mixtot <- read.csv("mixtotal.csv")
mixnigter <- mixtot[mixtot$species == "nig/ter",]
mixnigter2 <- mixnigter[mixnigter$ObjectId %in% my.number,]
write.csv(mixnigter2, "nigselectedwithter.csv")

tmix <- read.csv("nigselectedwithter.csv")
tmix <- tmix[tmix$time == 0,]

p <- ggplot(ok1)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size =0.5)+xlim(50, 180)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)
p + geom_jitter(data= tmix ,aes (Area,Contrast),color = "navy",height = 0.001,width = 0.1, size =0.5  )+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))

##A.niger assumed co-cultered with A.terreus(A. niger selection with A. terreus)
nig <- tot.t0[tot.t0$species == "A.niger",]
ok <- score.species(nig,nig1000,ter1000)

#ggplot(ok)+
  #geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size =0.5)+
  #scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+
  #theme_classic(base_size = 25)+labs(title="")

ggplot(ok)+
  geom_jitter(aes(Area,Contrast,col=X1),height = 0.001,width = 0.1, size =0.5)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+
  theme_classic(base_size = 25)+labs(title="")

selc <- ok$X1-50 > 45
ggplot(ok[selc,])+
  geom_jitter(aes(Area,Contrast,col=X1),height = 0.001,width = 0.1, size =0.5)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+
  theme_classic(base_size = 25)+labs(title="")

my.number <- nig[selc,]$ObjectId
tot <- read.csv("total.csv")
nig_assum_ter<- tot[tot$species == "A.niger",]
nig_assum_ter2 <- nig_assum_ter[nig_assum_ter$ObjectId %in% my.number,]
write.csv(nig_assum_ter2, "nigassumedwithter.csv")

t <- read.csv("nigassumedwithter.csv")
t <- t[t$time == 0,]

p <- ggplot(ok)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size =0.5)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)
p + geom_jitter(data= t ,aes (Area,Contrast),color = "navy" ,height = 0.001,width = 0.1, size =0.5 )+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))

####################################################################
##A.niger co-culterd with A.clavatus
nigclv<- mixtot.t0[mixtot.t0$species == "nig/clv",]
ok1 <- score.species(nigclv,nig1000,cla1000)

ggplot(ok1)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size=0.5)+xlim(50, 180)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))


selc <- ok1$X1-50 > 45
ggplot(ok1[selc,])+
  geom_jitter(aes(Area,Contrast,col=X1),height = 0.001,width = 0.1, size=0.5)+xlim(50, 180)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))


my.number <- nigclv[selc,]$ObjectId
mixtot <- read.csv("mixtotal.csv")
mixnigclv <- mixtot[mixtot$species == "nig/clv",]
mixnigclv2 <- mixnigclv[mixnigclv$ObjectId %in% my.number,]
write.csv(mixnigclv2, "nigselectedwithclv.csv")

cmix <- read.csv("nigselectedwithclv.csv")
cmix <- cmix[cmix$time == 0,]

p <- ggplot(ok1)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size=0.5)+xlim(50, 180)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)
p + geom_jitter(data= cmix ,aes (Area,Contrast),color = "navy" ,height = 0.001,width = 0.1, size=0.5 )+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))

#A.niger assumed co-culterd with A.clavatus(A. niger selection with A. clavatus)

nig <- tot.t0[tot.t0$species == "A.niger",]
ok <- score.species(nig,nig1000,cla1000)

ggplot(ok)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size=0.5)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)

ggplot(ok)+
  geom_jitter(aes(Area,Contrast,col=X1),height = 0.001,width = 0.1, size=0.5)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)

selc <- ok$X1-50 > 45
ggplot(ok[selc,])+
  geom_jitter(aes(Area,Contrast,col=X1),height = 0.001,width = 0.1, size=0.5)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic()

my.number <- nig[selc,]$ObjectId
tot <- read.csv("total.csv")
nig_assum_clv <- tot[tot$species == "A.niger",]
nig_assum_clv2 <- nig_assum_clv[nig_assum_clv$ObjectId %in% my.number,]
write.csv(nig_assum_clv2, "nigassumedwithclv.csv")

c <- read.csv("nigassumedwithclv.csv")
c <- c[c$time == 0,]

p <- ggplot(ok)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size=0.5)+ xlim(50, 200)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)
p + geom_jitter(data= c ,aes (Area,Contrast),color = "navy" ,height = 0.001,width = 0.1, size=0.5 )+
theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))

################################################################
#A.niger co-culture with A.oryzae
nigory<- mixtot.t0[mixtot.t0$species == "nig/ory",]
ok1 <- score.species(nigory,nig1000,ory1000)

ggplot(ok1)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size= 0.5)+xlim(50, 220)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))


selc <- ok1$X1-50 > 45
ggplot(ok1[selc,])+
  geom_jitter(aes(Area,Contrast,col=X1),height = 0.001,width = 0.1, size= 0.5)+xlim(50, 220)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))


my.number <- nigory[selc,]$ObjectId
mixtot <- read.csv("mixtotal.csv")
mixnigory <- mixtot[mixtot$species == "nig/ory",]
mixnigory2 <- mixnigory[mixnigory$ObjectId %in% my.number,]
write.csv(mixnigory2, "nigselectedwithory.csv")

omix <- read.csv("nigselectedwithory.csv")
omix <- omix[omix$time == 0,]

p <- ggplot(ok1)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size= 0.5)+xlim(50, 220)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)
p + geom_jitter(data= omix ,aes (Area,Contrast),color = "navy"  ,height = 0.001,width = 0.1, size= 0.5)+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))

##A.niger assumed co-culture with A.oryzae (A. niger selection with A. oryzae)
nig <- tot.t0[tot.t0$species == "A.niger",]
ok <- score.species(nig,nig1000,ory1000)

ggplot(ok)+
  geom_jitter(aes(Area,Contrast,col=X1),height = 0.001,width = 0.1, size= 0.5)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)

ggplot(ok)+
  geom_point(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size= 0.5)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)

selc <- ok$X1-50 > 45
ggplot(ok[selc,])+
  geom_jitter(aes(Area,Contrast,col=X1),height = 0.001,width = 0.1, size= 0.5)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)

my.number <- nig[selc,]$ObjectId
tot <- read.csv("total.csv")
nig_assum_ory <- tot[tot$species == "A.niger",]
nig_assum_ory2 <- nig_assum_ory[nig_assum_ory$ObjectId %in% my.number,]
write.csv(nig_assum_ory2, "nigassumedwithory.csv")

o <- read.csv("nigassumedwithory.csv")
o <- o[o$time == 0,]

p <- ggplot(ok)+
  geom_jitter(aes(Area,Contrast,col=X1-50),height = 0.001,width = 0.1, size= 0.5)+xlim(50, 220)+ ylim(0.05,0.50)+
  scale_color_gradient2(low = "red3",mid = "black",high = "lightblue")+theme_classic(base_size = 25)
p + geom_jitter(data= o ,aes (Area,Contrast),color = "navy" ,height = 0.001,width = 0.1, size= 0.5)+
  theme(legend.position = "none",axis.text.y = element_text(color = "black"),axis.text.x = element_text(color = "black"))



####################
setwd("F:/total/test results")
dt <- read.csv("pmax assump.csv")
subger <- dt[dt$var == "Germ tube formation", ]
subsw <- dt[dt$var == "Swelling", ]

ggplot(data = subger ) + geom_point(aes(x=species, y=Pmax), color= "red", size = 3) +
geom_errorbar(aes(x=species, ymin=min, ymax= max), width = 0.4, color ="red", size = 1)+
theme_classic()+ facet_grid(var~Treatment)+ 
geom_text(aes(x=species, y=Pmax, label = species, angle = 90), size= 4, vjust = 2)+ 
  theme(axis.text.x=element_blank()) + ylim(c(-10, 80))+ labs(title = "Pmax with 95% confidence intervals")

ggplot(data = subsw) + geom_point(aes(x=species, y=Pmax), color= "red", size = 3) +
geom_errorbar(aes(x=species, ymin=min, ymax= max), width = 0.4, color ="red", size = 1)+
theme_classic()+ facet_grid(var~Treatment)+ 
geom_text(aes(x=species, y=Pmax, label = species, angle = 90), size= 4, vjust = 2)+ 
theme(axis.text.x=element_blank()) + ylim(c(-10, 110))+ labs(title = "Pmax with 95% confidence intervals")
  

setwd("F:/total/test2")

replicate1 <- read.csv("M1.csv")
replicate2 <- read.csv("M2.csv")
replicate3 <- read.csv("M3.csv")

# Combine the data frames row-wise
merged_data <- rbind(replicate1, replicate2, replicate3)
 merged_data <- read.csv("merged_data.csv")

# Write the merged data to a new CSV file

merged_data.t0 <- merged_data[merged_data$time == 0,]

colnames(merged_data)

filtered_data <- merged_data[merged_data$time <= 900,]

library(ggplot2)
ggplot(filtered_data,aes(time,Area)) + geom_point()+
geom_smooth()+
facet_wrap(.~well)


unique(merged_data$well)
high.density <- merged_data[merged_data$well %in% c("Pro- 40000", "Ala- 40000","Arg- 40000"),]

write.csv(high.density, "high.density.csv")



merged_data.t24 <- merged_data[merged_data$time == 1380,]



ggplot(merged_data.t24) +
  geom_density_2d_filled(aes(Area,Circularity),bins=20)+
  geom_hline(yintercept=c(0.1,0.2,0.3,0.4),col="white",alpha=0.5)+
  geom_vline(xintercept=c(50,100,150,200),col="white",alpha=0.5)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(.~well)+
  scale_fill_manual(values = turbo(20))+
  ggtitle("Size and Contrast at t=0")+
  theme(panel.spacing = unit(0.1,"cm"),
        legend.position = "right",
        strip.text = element_text(size=20),
        axis.title = element_text(size=16),
        axis.text = element_text(size=12))











