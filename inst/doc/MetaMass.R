### R code from vignette source 'MetaMass.rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: MetaMass.rnw:59-64 (eval = FALSE)
###################################################
## install.packages("devtools") ##  Install devtools from R
## library(devtools) ## load devtools
## install_github("stuchly/MetaMass") ## Install MetaMass
## library(MetaMass) ## load MetaMass
## vignette("MetaMass") ## see this vignette


###################################################
### code chunk number 2: MetaMass.rnw:86-89
###################################################
library(MetaMass)
data(AnnotationAM)
head(AnnotationAM)


###################################################
### code chunk number 3: MetaMass.rnw:98-99 (eval = FALSE)
###################################################
## data_table<-read.table(filename,header=TRUE,sep="\t")


###################################################
### code chunk number 4: MetaMass.rnw:111-112 (eval = FALSE)
###################################################
## colnames(data_table)[sapply(data.table,is.numeric)]


###################################################
### code chunk number 5: MetaMass.rnw:143-145
###################################################
filename<-system.file("extdata","Bileck.txt",package="MetaMass")
filename


###################################################
### code chunk number 6: MetaMass.rnw:163-164 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "Data_Fig1b.txt", Metadata = "Christoforou", output = "Fig1b")


###################################################
### code chunk number 7: MetaMass.rnw:182-183 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "Data_Fig1b.txt", Metadata = "Christoforou", output = "Fig2acurves", markers = c(3:7))


###################################################
### code chunk number 8: MetaMass.rnw:200-201 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "Data_Fig1b.txt", Metadata = "Christoforou", output = "Fig1bUniprot", markers =4)


###################################################
### code chunk number 9: MetaMass.rnw:213-214 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "study4.txt", Metadata = "Christoforou", output = "study4")


###################################################
### code chunk number 10: MetaMass.rnw:224-225 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = c("study4.txt","study9.txt", "study10.txt"), Metadata = "Christoforou", output = "study4910")


###################################################
### code chunk number 11: MetaMass.rnw:257-264
###################################################
file2<-system.file("extdata","Data_Fig_1b.txt",package="MetaMass")

##cluster with respect MSfile only (cluster.metadata=FALSE by default)
res2<-analyze.MSfile(MSfile=file2,Metadata=c("Christoforou"),output="res2",markers=c(3:5))
data2<-get.data(res2,data.only=TRUE)
cls2_1<-get.clusters(res2,rID=1) #rID=1 annotation with respect to markers[1]; default
head(cls2_1)


###################################################
### code chunk number 12: MetaMass.rnw:274-275
###################################################
data2<-data.frame(data2,main_component1=cls2_1$main_component[data2$cluster])


###################################################
### code chunk number 13: MetaMass.rnw:293-294
###################################################
file1<-system.file("extdata","Data_Fig_1a.txt",package="MetaMass")


###################################################
### code chunk number 14: MetaMass.rnw:304-307
###################################################
##proteins identified by gene-name -> annotation.ID=2 (see ?AnnotationAM)
##cluster with respect metadata only (group=0)
res1<-analyze.MSfile(MSfile=file1,Metadata=c("Christoforou"),output="res1",group=0,cluster.metadata=TRUE)


###################################################
### code chunk number 15: MetaMass.rnw:327-331
###################################################
file2<-system.file("extdata","Data_Fig_1b.txt",package="MetaMass")

##cluster with respect MSfile only (cluster.metadata=FALSE by default)
res2<-analyze.MSfile(MSfile=file2,Metadata=c("Christoforou"),output="res2")


###################################################
### code chunk number 16: MetaMass.rnw:341-345
###################################################
##compare multiple files component fractionation with Metadata
files1<-system.file("extdata",c("Bileck.txt","Thakar.txt","Carvalho.txt"),package="MetaMass")

res3<-analyze.MSfile(MSfile=files1,Metadata=c("Christoforou"),output="res3")


###################################################
### code chunk number 17: MetaMass.rnw:357-358
###################################################
res4<-analyze.MSfile(MSfile=file2,Metadata=c("Christoforou"),output="res2_4annot",clusters=480,markers=c(3,4,6,7))


###################################################
### code chunk number 18: MetaMass.rnw:364-366 (eval = FALSE)
###################################################
## par(mfrow=c(3,3),mar=c(1, 4, 2.2, 1) + 0.1,cex=0.45)
## plot.prAM(res4) #plot in  3 rows and 3 columns


###################################################
### code chunk number 19: produce-plot
###################################################
pdf('roc1.pdf')
par(mfrow=c(3,3),mar=c(1, 4, 2.2, 1) + 0.1,cex=0.45)
plot.prAM(res4) #plot in 3 rows and 3 columns    # Produces Figure \ref{fig1}
dev.off()


###################################################
### code chunk number 20: MetaMass.rnw:398-400
###################################################
files2<-system.file("extdata",c("Bileck.txt","Thakar.txt","Carvalho.txt","Andreyev.txt","Rodriguez.txt"),package="MetaMass")
res3<-analyze.MSfile(MSfile=files2,Metadata=c("Christoforou"),output="res3intersect")


###################################################
### code chunk number 21: MetaMass.rnw:406-408
###################################################
files2<-system.file("extdata",c("Bileck.txt","Thakar.txt","Carvalho.txt","Andreyev.txt","Rodriguez.txt"),package="MetaMass")
res3<-analyze.MSfile(MSfile=files2,Metadata=c("Christoforou"),output="res3_5",overlap=3)


###################################################
### code chunk number 22: MetaMass.rnw:414-416
###################################################
files2<-system.file("extdata",c("Bileck.txt","Thakar.txt","Carvalho.txt","Andreyev.txt","Rodriguez.txt"),package="MetaMass")
res3<-analyze.MSfile(MSfile=files2,Metadata=c("Christoforou"),output="res3_6",overlap=5)


