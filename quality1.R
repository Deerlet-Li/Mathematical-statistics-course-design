#数据预处理
redd=read.csv('winequality-red.csv')#数据框
red=as.matrix(redd)#整个数据(矩阵)
X_data=red[,1:11]#11原指标
quality=red[,12]#结果
hist(quality,breaks=6,xlab='quality',ylab='频数',main='',col=NULL)
#数据正态性检验
ks.test(X_data[,1],'pnorm',mean=mean(X_data[,1]),sd=sqrt(var(X_data[,1])));hist(X_data[,1],xlab='fixed acidity',ylab='频数',main='')
ks.test(X_data[,2],'pnorm',mean=mean(X_data[,2]),sd=sqrt(var(X_data[,2])));hist(X_data[,2],xlab='volatile acidity',ylab='频数',main='')
ks.test(X_data[,3],'pnorm',mean=mean(X_data[,3]),sd=sqrt(var(X_data[,3])));hist(X_data[,3],xlab='citric acid',ylab='频数',main='')
ks.test(X_data[,4],'pnorm',mean=mean(X_data[,4]),sd=sqrt(var(X_data[,4])));hist(X_data[,4],xlab='residual sugar',ylab='频数',main='')
ks.test(X_data[,5],'pnorm',mean=mean(X_data[,5]),sd=sqrt(var(X_data[,5])));hist(X_data[,5],xlab='chlorides',ylab='频数',main='')
ks.test(X_data[,6],'pnorm',mean=mean(X_data[,6]),sd=sqrt(var(X_data[,6])));hist(X_data[,6],xlab='free sulfur dioxide',ylab='频数',main='')
ks.test(X_data[,7],'pnorm',mean=mean(X_data[,7]),sd=sqrt(var(X_data[,7])));hist(X_data[,7],xlab='total sulfur dioxide',ylab='频数',main='')
ks.test(X_data[,8],'pnorm',mean=mean(X_data[,8]),sd=sqrt(var(X_data[,8])));hist(X_data[,8],xlab='density',ylab='频数',main='')
ks.test(X_data[,9],'pnorm',mean=mean(X_data[,9]),sd=sqrt(var(X_data[,9])));hist(X_data[,9],xlab='pH',ylab='频数',main='')
ks.test(X_data[,10],'pnorm',mean=mean(X_data[,10]),sd=sqrt(var(X_data[,10])));hist(X_data[,10],xlab='sulphates',ylab='频数',main='')
ks.test(X_data[,11],'pnorm',mean=mean(X_data[,11]),sd=sqrt(var(X_data[,11])));hist(X_data[,11],xlab='alcohol',ylab='频数',main='')
#相关性分析1
A=cor(X_data,method='spearman')#各个指标之间的相关系数
B=cor(X_data,quality,method='spearman')#指标和评价结果之间的相关系数
library(ggplot2)
ggplot(data=redd,aes(x=fixed.acidity,y=pH))+geom_point()+theme_bw()
ggplot(data=redd,aes(x=fixed.acidity,y=density))+geom_point()+theme_bw()
ggplot(data=redd,aes(x=fixed.acidity,y=citric.acid))+geom_point()+theme_bw()
ggplot(data=redd,aes(x=pH,y=citric.acid))+geom_point()+theme_bw()
ggplot(data=redd,aes(x=volatile.acidity,y=citric.acid))+geom_point()+theme_bw()
ggplot(data=redd,aes(x=pH,y=density))+geom_point()+theme_bw()
ggplot(data=redd,aes(x=alcohol,y=density))+geom_point()+theme_bw()
ggplot(data=redd,aes(x=total.sulfur.dioxide,y=free.sulfur.dioxide))+geom_point()+theme_bw()
#相关性分析2--根据箱线图
y=order(quality)
y3=y[1:10];y4=y[11:63];y5=y[64:744];y6=y[745:1382];y7=y[1383:1581];y8=y[1582:1599]
quality_3=red[y3,];quality_4=red[y4,];quality_5=red[y5,]
quality_6=red[y6,];quality_7=red[y7,];quality_8=red[y8,]
boxplot(quality_3[,1],quality_4[,1],quality_5[,1],quality_6[,1],quality_7[,1],quality_8[,1],ylab='fixed acidity',col=NULL)
boxplot(quality_3[,2],quality_4[,2],quality_5[,2],quality_6[,2],quality_7[,2],quality_8[,2],ylab='volatile acidity',col=NULL)
boxplot(quality_3[,3],quality_4[,3],quality_5[,3],quality_6[,3],quality_7[,3],quality_8[,3],ylab='citric acid',col=NULL)
boxplot(quality_3[,4],quality_4[,4],quality_5[,4],quality_6[,4],quality_7[,4],quality_8[,4],ylab='residual sugar',col=NULL)
boxplot(quality_3[,5],quality_4[,5],quality_5[,5],quality_6[,5],quality_7[,5],quality_8[,5],ylab='chlorides',col=NULL)
boxplot(quality_3[,6],quality_4[,6],quality_5[,6],quality_6[,6],quality_7[,6],quality_8[,6],ylab='free sulfur dioxide',col=NULL)
boxplot(quality_3[,7],quality_4[,7],quality_5[,7],quality_6[,7],quality_7[,7],quality_8[,7],ylab='total sulfur dioxide',col=NULL)
boxplot(quality_3[,8],quality_4[,8],quality_5[,8],quality_6[,8],quality_7[,8],quality_8[,8],ylab='density',col=NULL)
boxplot(quality_3[,9],quality_4[,9],quality_5[,9],quality_6[,9],quality_7[,9],quality_8[,9],ylab='pH',col=NULL)
boxplot(quality_3[,10],quality_4[,10],quality_5[,10],quality_6[,10],quality_7[,10],quality_8[,10],ylab='sulphates',col=NULL)
boxplot(quality_3[,11],quality_4[,11],quality_5[,11],quality_6[,11],quality_7[,11],quality_8[,11],ylab='alcohol',col=NULL)
#相关性分析3--相关性系数热力图
library(corrplot)
reed=as.matrix(redd[,1:11])
colnames(reed)=c('x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11')
a=cor(reed,method='spearman')
corrplot(a,method='circle',type='upper',mar=c(0,0,0,0),bg='white',addCoef.col=NULL,rect.col='black',rect.lwd=2,tl.pos='lt',tl.cex=1,tl.col='black',cl.pose=NULL)
corrplot(a,method='number',add=TRUE,type='lower',col='black',diag=FALSE,tl.pos='n',cl.pos=NULL)
#指标各自回归建立方程
red_stand=scale(red[,1:11])
red_stand=cbind(red_stand,quality)
red_stand=as.data.frame(red_stand)
tlm1=lm(quality~fixed.acidity,data=red_stand);summary(tlm1)
tlm2=lm(quality~volatile.acidity,data=red_stand);summary(tlm2)
tlm3=lm(quality~citric.acid,data=red_stand);summary(tlm3)
tlm4=lm(quality~residual.sugar,data=red_stand);summary(tlm4)
tlm5=lm(quality~chlorides,data=red_stand);summary(tlm5)
tlm6=lm(quality~free.sulfur.dioxide,data=red_stand);summary(tlm6)
tlm7=lm(quality~total.sulfur.dioxide,data=red_stand);summary(tlm7)
tlm8=lm(quality~density,data=red_stand);summary(tlm8)
tlm9=lm(quality~pH,data=red_stand);summary(tlm9)
tlm10=lm(quality~sulphates,data=red_stand);summary(tlm10)
tlm11=lm(quality~alcohol,data=red_stand);summary(tlm11)
tlmm=lm(quality~.,data=red_stand);summary(tlmm)
#AIC挑选主要指标
tlm=lm(quality~.,data=red_stand)
summary(tlm)#11维变量线性回归
tstep=step(tlm)
summary(tstep)#逐步回归分析
drop1(tstep)#逐步回归分析的优化
tlm=lm(quality~volatile.acidity+chlorides+total.sulfur.dioxide+pH+sulphates+alcohol,data=red_stand)
summary(tlm)#最终回归方程
#质量分布可视化
quality_fac=factor(quality,levels=c(3,4,5,6,7,8))
ggplot(redd,aes(x=alcohol,y=sulphates,color=quality))+geom_point()+scale_color_gradientn(colours=c('#0000AA','#0000AA','#FDDDAA','#FFDDAA','#FF0000','#FF0000'))+theme_bw()
ggplot(redd,aes(x=alcohol,y=volatile.acidity,color=quality))+geom_point()+scale_color_gradientn(colours=c('#0000AA','#0000AA','#FDDDAA','#FFDDAA','#FF0000','#FF0000'))+theme_bw()
ggplot(redd,aes(x=volatile.acidity,y=sulphates,color=quality))+geom_point()+scale_color_gradientn(colours=c('#0000AA','#0000AA','#FDDDAA','#FFDDAA','#FF0000','#FF0000'))+theme_bw()
#误差检验
par(mfrow=c(2,2))
plot(tlm)
plot(tlm,which=4)
plot(tlm,which=1)
