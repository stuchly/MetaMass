### R code from vignette source 'MetaMass.rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: MetaMass.rnw:57-62 (eval = FALSE)
###################################################
## install.packages("devtools") ##  Install devtools from R
## library(devtools) ## load devtools
## install_github("stuchly/MetaMass") ## Install MetaMass
## library(MetaMass) ## load MetaMass
## vignette("MetaMass") ## see more detailed vignette


###################################################
### code chunk number 2: MetaMass.rnw:96-98
###################################################
filename<-system.file("extdata","Bileck.txt",package="MetaMass")
filename


###################################################
### code chunk number 3: MetaMass.rnw:109-110 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "Data_Fig2a.txt", overlap=2, output = "Fig2a")


###################################################
### code chunk number 4: MetaMass.rnw:129-130 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "Data_Fig2a.txt", overlap=2, output = "Fig2acurves", markers = c(3:8))


###################################################
### code chunk number 5: MetaMass.rnw:144-145 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "Data_Fig2a.txt", overlap=2, output = "Fig2aUniGOoverlap", markers = 4)


###################################################
### code chunk number 6: MetaMass.rnw:158-159 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "study4.txt", overlap=1, output = "study4", markers = 8)


###################################################
### code chunk number 7: MetaMass.rnw:169-170 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = c("study4.txt","study9.txt", "study10.txt"),  overlap=2, output = "study4910")


###################################################
### code chunk number 8: MetaMass.rnw:186-187 (eval = FALSE)
###################################################
## data_table<-read.table(filename,header=TRUE,sep="\t")


###################################################
### code chunk number 9: MetaMass.rnw:199-200 (eval = FALSE)
###################################################
## colnames(data_table)[sapply(data.table,is.numeric)]


###################################################
### code chunk number 10: MetaMass.rnw:218-220
###################################################
data(levelsC)
levelsC


###################################################
### code chunk number 11: MetaMass.rnw:240-247
###################################################
file2<-system.file("extdata","Data_Fig_1b.txt",package="MetaMass")

##cluster with respect MSfile only (cluster.metadata=FALSE by default)
res2<-analyze.MSfile(MSfile=file2,Metadata=c("Christoforou"),output="res2",markers=c(3:5))
data2<-get.data(res2,data.only=TRUE)
cls2_1<-get.clusters(res2,rID=1) #rID=1 annotation with respect to markers[1]; default
head(cls2_1)


###################################################
### code chunk number 12: MetaMass.rnw:257-258
###################################################
data2<-data.frame(data2,main_component1=cls2_1$main_component[data2$cluster])


