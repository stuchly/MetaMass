### R code from vignette source 'SimpleVignette.rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: SimpleVignette.rnw:57-62 (eval = FALSE)
###################################################
## install.packages("devtools") ##  Install devtools from R
## library(devtools) ## load devtools
## install_github("stuchly/MetaMass") ## Install MetaMass
## library(MetaMass) ## load MetaMass
## vignette("MetaMass") ## see more detailed vignette


###################################################
### code chunk number 2: SimpleVignette.rnw:96-98
###################################################
filename<-system.file("extdata","Bileck.txt",package="MetaMass")
filename


###################################################
### code chunk number 3: SimpleVignette.rnw:109-110 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "Data_Fig2a.txt", overlap=2, output = "Fig2a")


###################################################
### code chunk number 4: SimpleVignette.rnw:129-130 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "Data_Fig2a.txt", overlap=2, output = "Fig2acurves", markers = c(3:8))


###################################################
### code chunk number 5: SimpleVignette.rnw:144-145 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "Data_Fig2a.txt", overlap=2, output = "Fig2aUniGOoverlap", markers = 4)


###################################################
### code chunk number 6: SimpleVignette.rnw:158-159 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = "study4.txt", overlap=1, output = "study4", markers = 8)


###################################################
### code chunk number 7: SimpleVignette.rnw:169-170 (eval = FALSE)
###################################################
## analyze.MSfile(MSfile = c("study4.txt","study9.txt", "study10.txt"),  overlap=2, output = "study4910")


