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
### code chunk number 2: MetaMass.rnw:87-90
###################################################
library(MetaMass)
data(AnnotationAM)
head(AnnotationAM)


###################################################
### code chunk number 3: MetaMass.rnw:99-100 (eval = FALSE)
###################################################
## data_table<-read.table(filename,header=TRUE,sep="\t")


###################################################
### code chunk number 4: MetaMass.rnw:112-113 (eval = FALSE)
###################################################
## colnames(data_table)[sapply(data.table,is.numeric)]


###################################################
### code chunk number 5: MetaMass.rnw:130-132
###################################################
data(levelsC)
levelsC


###################################################
### code chunk number 6: MetaMass.rnw:167-169
###################################################
filename<-system.file("extdata","Bileck.txt",package="MetaMass")
filename


###################################################
### code chunk number 7: MetaMass.rnw:192-195 (eval = FALSE)
###################################################
## library(MetaMass)
## analyze.MSfile(MSfile = "Data_Fig2a.txt",Metadata = "Christoforou",overlap = 2, clusters = 1400, Annotation = "MyMarkers.txt",
## markers =c(3,4,7), output = "Fig2a")


###################################################
### code chunk number 8: MetaMass.rnw:237-239 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "study1.txt", clusters=432, Metadata = c("Carvalho", "Bileck", "Thakar"), Annotation = "MyMarkers.txt",
## markers=c(3,4,6,7), output = "study1_MyMark")


###################################################
### code chunk number 9: MetaMass.rnw:245-246 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "study4_9_10.txt",clusters = 473,Annotation = "MyMarkers.txt", markers=c(3,4,6,7),output = "study4_9_10_MyMark")


###################################################
### code chunk number 10: MetaMass.rnw:257-267 (eval = FALSE)
###################################################
## 
## analyze.MSfile(MSfile = "Data_Fig2a.txt", Annotation = "MyMarkers.txt", markers=4, clusters=1400, output = "Fig2aUGOovl")
## 
## analyze.MSfile(MSfile = "Data_Fig2a.txt", Annotation = "MyMarkers.txt", markers=3, clusters=1400, output = "Fig2aStudy1UGOovl")
## 
## analyze.MSfile(MSfile = "Data_Fig2a.txt", Annotation = "MyMarkers.txt", markers=6, clusters=1400, output = "Fig2aUniGOsum")
## 
## analyze.MSfile(MSfile = "Data_Fig2a.txt", Annotation = "MyMarkers.txt", markers=7, clusters=1400, output = "Fig2aHPAsupportive")
## 
## analyze.MSfile(MSfile = "Data_Fig2a.txt", Annotation = "MyMarkers.txt", markers=8, clusters=1400, output = "Fig2aHPAuncertain")


###################################################
### code chunk number 11: MetaMass.rnw:319-326
###################################################
file2<-system.file("extdata","Data_Fig_1b.txt",package="MetaMass")

##cluster with respect MSfile only (cluster.metadata=FALSE by default)
res2<-analyze.MSfile(MSfile=file2,Metadata=c("Christoforou"),output="res2",markers=c(3:5))
data2<-get.data(res2,data.only=TRUE)
cls2_1<-get.clusters(res2,rID=1) #rID=1 annotation with respect to markers[1]; default
head(cls2_1)


###################################################
### code chunk number 12: MetaMass.rnw:336-337
###################################################
data2<-data.frame(data2,main_component1=cls2_1$main_component[data2$cluster])


###################################################
### code chunk number 13: MetaMass.rnw:354-355
###################################################
file1<-system.file("extdata","Data_Fig_1a.txt",package="MetaMass")


###################################################
### code chunk number 14: MetaMass.rnw:365-368
###################################################
##proteins identified by gene-name -> annotation.ID=2 (see ?AnnotationAM)
##cluster with respect metadata only (group=0)
res1<-analyze.MSfile(MSfile=file1,Metadata=c("Christoforou"),output="res1",group=0,cluster.metadata=TRUE)


###################################################
### code chunk number 15: MetaMass.rnw:388-395
###################################################
file2<-system.file("extdata","Data_Fig2a.txt",package="MetaMass")
MyMarkers<-system.file("extdata","MyMarkers.txt",package="MetaMass")

##cluster with respect MSfile only (cluster.metadata=FALSE by default)
library(MetaMass)
analyze.MSfile(MSfile = file2,Metadata = "Christoforou", clusters = 1400, Annotation = MyMarkers,
markers =c(3,4,7), output = "Fig2a")


###################################################
### code chunk number 16: MetaMass.rnw:404-408
###################################################
##compare multiple files component fractionation with Metadata
files1<-system.file("extdata",c("Bileck.txt","Thakar.txt","Carvalho.txt"),package="MetaMass")

res3<-analyze.MSfile(MSfile=files1,Metadata=c("Christoforou"),output="res3")


###################################################
### code chunk number 17: MetaMass.rnw:418-422
###################################################
file1<-system.file("extdata","Christoforou.txt",package="MetaMass")
MyMarkers<-system.file("extdata","MyMarkers.txt",package="MetaMass")
analyze.MSfile(MSfile = file1, clusters=432, Metadata = c("Carvalho", "Bileck", "Thakar"), Annotation = MyMarkers,
markers=c(3,4,6,7), output = "study1_MyMark")


###################################################
### code chunk number 18: MetaMass.rnw:428-430
###################################################
files1<-system.file("extdata",c("Bileck.txt","Thakar.txt","Carvalho.txt"),package="MetaMass")
analyze.MSfile(MSfile = files1,clusters = 473,Annotation = MyMarkers, markers=c(3,4,6,7),output = "study4_9_10_MyMark")


###################################################
### code chunk number 19: MetaMass.rnw:441-442
###################################################
res4<-analyze.MSfile(MSfile=file2,Metadata=c("Christoforou"),output="res2_4annot",clusters=480,markers=c(3,4,6,7))


###################################################
### code chunk number 20: MetaMass.rnw:448-450 (eval = FALSE)
###################################################
## par(mfrow=c(3,3),mar=c(1, 4, 2.2, 1) + 0.1,cex=0.45)
## plot.prAM(res4) #plot in  3 rows and 3 columns


###################################################
### code chunk number 21: produce-plot
###################################################
pdf('roc1.pdf')
par(mfrow=c(3,3),mar=c(1, 4, 2.2, 1) + 0.1,cex=0.45)
plot.prAM(res4) #plot in 3 rows and 3 columns    # Produces Figure \ref{fig1}
dev.off()


###################################################
### code chunk number 22: MetaMass.rnw:483-485
###################################################
files2<-system.file("extdata",c("Bileck.txt","Thakar.txt","Carvalho.txt","Andreyev.txt","Rodriguez.txt"),package="MetaMass")
res3<-analyze.MSfile(MSfile=files2,Metadata=c("Christoforou"),output="res3intersect")


###################################################
### code chunk number 23: MetaMass.rnw:491-493
###################################################
files2<-system.file("extdata",c("Bileck.txt","Thakar.txt","Carvalho.txt","Andreyev.txt","Rodriguez.txt"),package="MetaMass")
res3<-analyze.MSfile(MSfile=files2,Metadata=c("Christoforou"),output="res3_3",overlap=3)


###################################################
### code chunk number 24: MetaMass.rnw:499-501
###################################################
files2<-system.file("extdata",c("Bileck.txt","Thakar.txt","Carvalho.txt","Andreyev.txt","Rodriguez.txt"),package="MetaMass")
res3<-analyze.MSfile(MSfile=files2,Metadata=c("Christoforou"),output="res3_5",overlap=5,markers=c(3:7))


###################################################
### code chunk number 25: produce-plot
###################################################
pdf('roc2.pdf')
par(mfrow=c(3,3),mar=c(1, 4, 2.2, 1) + 0.1,cex=0.45)
plot.prAM(res3,legend.position="topleft") #plot in 3 rows and 3 columns    # Produces Figure \ref{fig1}
dev.off()


###################################################
### code chunk number 26: MetaMass.rnw:523-536
###################################################
file2<-system.file("extdata","Data_Fig2a.txt",package="MetaMass")
MyMarkers<-system.file("extdata","MyMarkers.txt",package="MetaMass")


analyze.MSfile(MSfile = file2, Annotation = MyMarkers, markers=4, clusters=1400, output = "Fig2aUGOovl")

analyze.MSfile(MSfile = file2, Annotation = MyMarkers, markers=3, clusters=1400, output = "Fig2aStudy1UGOovl")

analyze.MSfile(MSfile = file2, Annotation = MyMarkers, markers=6, clusters=1400, output = "Fig2aUniGOsum")

analyze.MSfile(MSfile = file2, Annotation = MyMarkers, markers=7, clusters=1400, output = "Fig2aHPAsupportive")

analyze.MSfile(MSfile =file2, Annotation = MyMarkers, markers=8, clusters=1400, output = "Fig2aHPAuncertain")


