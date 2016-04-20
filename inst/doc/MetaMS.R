### R code from vignette source 'MetaMS.rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: MetaMS.rnw:53-58 (eval = FALSE)
###################################################
## install.packages("devtools") ##  Install devtools from R
## library(devtools) ## load devtools
## install_github("stuchly/MetaMS") ## Install MetaMS
## library(MetaMS) ## load MetaMS
## vignette("MetaMS") ## see this vignette


###################################################
### code chunk number 2: MetaMS.rnw:81-84
###################################################
library(MetaMS)
data(AnnotationAM)
head(AnnotationAM)


###################################################
### code chunk number 3: MetaMS.rnw:93-94 (eval = FALSE)
###################################################
## data_table<-read.table(filename,header=TRUE,sep="\t")


###################################################
### code chunk number 4: MetaMS.rnw:106-107 (eval = FALSE)
###################################################
## colnames(data_table)[sapply(data.table,is.numeric)]


###################################################
### code chunk number 5: MetaMS.rnw:138-140
###################################################
filename<-system.file("extdata","Bileck.txt",package="MetaMS")
filename


###################################################
### code chunk number 6: MetaMS.rnw:149-150 (eval = FALSE)
###################################################
## Res1= analyze.MSfile(MSfile="filename.txt", Metadata= "Christoforou", output="myfile2")


###################################################
### code chunk number 7: MetaMS.rnw:161-162 (eval = FALSE)
###################################################
## Res2= analyze.MSfile(MSfile="filename.txt", Metadata= "Christoforou", output="myfile2",markers=c(3:7))


###################################################
### code chunk number 8: MetaMS.rnw:171-172 (eval = FALSE)
###################################################
## ?AnnotationAM


###################################################
### code chunk number 9: MetaMS.rnw:180-181 (eval = FALSE)
###################################################
## Res3= analyze.MSfile(MSfile="filename.txt", Metadata= "Christoforou", output="myfile3",group=0,cluster.metadata=TRUE)


###################################################
### code chunk number 10: MetaMS.rnw:189-190 (eval = FALSE)
###################################################
## Res4= analyze.MSfile(MSfile="filename.txt", Metadata= "Larance", output="myfile4")


###################################################
### code chunk number 11: MetaMS.rnw:209-216
###################################################
file2<-system.file("extdata","Data_Fig_1b.txt",package="MetaMS")

##cluster with respect MSfile only (cluster.metadata=FALSE by default)
res2<-analyze.MSfile(MSfile=file2,Metadata=c("Christoforou"),output="res2",markers=c(3:5))
data2<-get.data(res2,data.only=TRUE)
cls2_1<-get.clusters(res2,rID=1) #rID=1 annotation with respect to markers[1]; default
head(cls2_1)


###################################################
### code chunk number 12: MetaMS.rnw:226-227
###################################################
data2<-data.frame(data2,main_component1=cls2_1$main_component[data2$cluster])


###################################################
### code chunk number 13: MetaMS.rnw:243-244
###################################################
file1<-system.file("extdata","Data_Fig_1a.txt",package="MetaMS")


###################################################
### code chunk number 14: MetaMS.rnw:255-258
###################################################
##proteins identified by gene-name -> annotation.ID=2 (see ?AnnotationAM)
##cluster with respect metadata only (group=0)
res1<-analyze.MSfile(MSfile=file1,Metadata=c("Christoforou"),output="res1",group=0,cluster.metadata=TRUE)


###################################################
### code chunk number 15: MetaMS.rnw:276-280
###################################################
file2<-system.file("extdata","Data_Fig_1b.txt",package="MetaMS")

##cluster with respect MSfile only (cluster.metadata=FALSE by default)
res2<-analyze.MSfile(MSfile=file2,Metadata=c("Christoforou"),output="res2")


###################################################
### code chunk number 16: MetaMS.rnw:287-292
###################################################
##compare multiple files component fractionation with Metadata
files1<-system.file("extdata",c("Bileck.txt","Thakar.txt","Carvalho.txt"),package="MetaMS")

res3<-analyze.MSfile(MSfile=files1,Metadata=c("Christoforou"),output="res3")



###################################################
### code chunk number 17: MetaMS.rnw:301-302
###################################################
res4<-analyze.MSfile(MSfile=file2,Metadata=c("Christoforou"),output="res2_4annot",clusters=480,markers=c(3,4,6,7))


###################################################
### code chunk number 18: MetaMS.rnw:308-310 (eval = FALSE)
###################################################
## par(mfrow=c(5,3),mar=c(1, 4, 2.2, 1) + 0.1)
## plot.rocAM(res4) #plot in  6 rows and 3 columns


###################################################
### code chunk number 19: produce-plot
###################################################
pdf('roc1.pdf')
par(mfrow=c(5,3),mar=c(1, 4, 2.2, 1) + 0.1,cex=0.7)
plot.rocAM(res4) #plot in 5 rows and 4 columns    # Produces Figure \ref{fig1}
dev.off()


