\name{get.data}
\alias{get.data}
\title{get.data}
\description{
 retrieves MS data.frame merged with Annotation from AnnoMass object
}
\usage{
get.data(AM,annotation=TRUE,data.only=FALSE,out=FALSE)
}
\arguments{
  \item{AM}{AnnoMass object. Result of function \code{analyze.MSfile}}
  \item{annotation}{if the annotation should be merged with data}
  \item{data.only}{only data (numerical) columns, protein ID and cluster
    membership is returned}
  \item{out}{the table in the same format as in output_table txtx file
    is returned}
  \item{fulltext}{only with out=TRUE; fullText annoations from
    data(full_text_annotation) columns are added}
}

\value{
  data.frame. One line per protein.

}


\examples{
file1<-system.file("extdata","Rodriguez.txt",package="MetaMass")
file2<-system.file("extdata","Carvalho.txt",package="MetaMass")
file1
file2
res<-analyze.MSfile(MSfile=c(file1,file2),Metadata=c("Christoforou","Rodriguez"),markers=c(3,4,5,6,7))

head(get.data(res))
head(get.data(res,annotation=FALSE,data.only=TRUE))
}
