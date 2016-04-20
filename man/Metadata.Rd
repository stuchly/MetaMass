\name{Meatadata}
\docType{data}
\alias{Metadata}
\title{Mass spectrometry data for meta analysis}
\description{
  This data set can be used as metadata for analysis of mass
  spectrometry data
}
\usage{
Christoforou
Bileck
Rodriguez
Andreyev
Thakar
Larance
Carvalho

or as extdata

filename<-system.file("extdata","Christoforou.txt",package="MetaMS")
filename<-system.file("extdata","Bileck.txt",package="MetaMS")
filename<-system.file("extdata","Rodriguez.txt",package="MetaMS")
filename<-system.file("extdata","Andreyev.txt",package="MetaMS")
filename<-system.file("extdata","Thakar.txt",package="MetaMS")
filename<-system.file("extdata","Larance.txt",package="MetaMS")
filename<-system.file("extdata","Carvalho.txt",package="MetaMS")

}
\format{
  data.frame

  [,1] genename character

  [,2:ncol] numerical values separated by empty columns
}
\source{}
\references{
  Christoforou, Mulvey, Breckels, et al. (2016) 10.1038/ncomms9992

  Ana M. Rodriguez-Pineiro, Sjoerd van der Post, Malin E. V. Johansson,
  Kristina A. Thomsson, Alexey I. Nesvizhskii, and Gunnar C. Hansson
  (2012) 10.1021/pr2010988

  Andreyev, Shen, Guan, et al. (2009) 10.1074/mcp.M900432-MCP200
  Carvalho, Ribeiro, Voabil, Penque, Jensen, Molina, and Matthiesen
  (2014) 10.1074/mcp.M113.034363

  Bileck, Kreutz, Muqaku, Slany, and Gerner (2014) 10.1021/pr5008625

  Thakar, Karaca, Port, Urlaub, and Kehlenbach (2012)
  10.1074/mcp.M112.024877

  Larance, Kirkwood, Xirodimas, Lundberg, Uhlen, and Lamond (2011)
  10.1074/mcp.M111.014407

  Ana Sofia Carvalho, Helena Ribeiro, Paula Voabil, Deborah Penque, Ole
  N Jensen, Henrik Molina, Rune Matthiese (2014) 10.1074/mcp.M113.034363

}
\keyword{datasets}

\examples{
data(Rodriguez)
res2<-analyze.MSfile(Rodriguez,markers=c(3:7),output.cdt="outcdt41.cdt",clusters=1000,Metadata="Christoforou",group=0,cluster.metadata=TRUE)

}
