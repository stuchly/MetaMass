\name{analyze.MSfile}
\alias{analyze.MSfile}
\title{Meta-analysis of Mass Spectrometry data file/frame}
\description{
  Annotates MS data file with respect cellular localisation
}
\usage{
analyze.MSfile(MSfile,Annotation=NULL,Metadata="Christoforou",annotation.ID=1,data.ID=2,markers=3,group_names=NULL,clusters=500,output.data="annotated_data.txt",output.cdt="heatmap.cdt",output.roc="roc.pdf",sep="\t",method="kmeans",metric="euclidean",iter.max=100,nstart=1,group=NULL,subset=NULL,sort.by=1,cluster.metadata=FALSE,overlap=NULL)}
\arguments{
  \item{MSfile}{data.frame or character vector of path to the text
    file(s) with MS data. See details.}

  \item{Annotation}{data.frame or character of path to the text
    file with Annotation data. If NULL the AnnotationAM data is used
    (\code{?AnnotationAM}). If custom annotation file is used the first
    column must be uniprot ID.
  }

  \item{Metadata}{character vector, name(s) of MS data present in the
    package see \code{?Metadata}}

  \item{annotation.ID}{integer. Column index of protein ID (such as
    uniprot ID) in the Annotation data.frame). IDs must match to those
    in annotation.ID column.
    Default for Annotation=NULL is genename
  }

  \item{data.ID}{integer. Column index of protein ID (such as
    uniprot ID) in the MSfile(s) or data.frame respectively. IDs must match to those in annotation.ID column}

  \item{markers}{integer vector. Column indices of cellular
    localizations in the Annotation data.frame. If a vector is provided
    the annotation is pefermed separately with respect all indicated columns}

  \item{group_names}{optional. character vector with names of the
    studies in the MSfile(s). See details}

  \item{clusters}{positive integer or NULL. Number of clusters to be
  created. If NULL number of clusters is estimated as nrow(Data)\%/\%5}

  \item{output}{character. prefix of files where the results will be stored.
    output files:
    output_table.txt - annoataed data with cluster assignement
    ouput_javatree.cdt - heatmap for java treeview
    output_pr.pdf -  Roc curves main_fraction will be saved. see ?plot.rocAM}

  \item{sep}{to specify a character which delimits fields. default is
    tab-delimited text.}

  \item{method}{chracter. clustering algorithm to be used. acceptable values are
    "kmeans", "pam" or "order". Pam (partitioning around medoids is more robust,
    but could be time consuming.
  "crude" just orders the data with respect the first column (splitting
    the ties in oposite direction with respect following columns) a
    divides the sequence in desired number of clusters. Usefull for
    simple fractionation methods see example section.}

  \item{metric}{character. metric to be used for building distance
    matrix for clustering. acceptable values for "pam" are
    "euclidean","manhattan" or "correlation". For "kmeans" see \code{?Kmeans}
    argument "method"}

  \item{iter.max}{The maximum number of iterations allowed.}

  \item{nstart}{ how many random sets should be chosen}

  \item{group}{vector of positive integers, 0 or NULL. Which studies
    should be used for clustering. if NULL all studies are used, if 0 none
    is and only Metadata are clustered}

  \item{subset}{vector of integers or NULL. Lines in the data.frame (see
    \code{?get.data}) used for clustering. if NULL all lines are used}

  \item{sort.by}{integer. with respect to which annotation (see
    markers argument) should be the output sorted. This marker set will
    be exported as reference into the cdt file gene description.}

  \item{cluster.metada}{logical. Should we cluster the Metadata?}

  \item{overlap}{if null only proteins detected in all studies and Metadata will be used for clustering. If integer (k) only proteins detected t least k studies will be used for clustering}

}
\value{Object of class AnnoMass}

\details{the data are clustered and annotated wih rescpect to chosen
  annonation(s); see ?get.clusters for the meaning of "assigned_location" and
  "main_component".
data in outupfiles (_table and _treeview) are sorted first with respect
  to the assigned_location (in the order in data(levelsC) data.frame a ties are
  splitted first with recpect to the number of annotations in the
  clusters and then with respect to precision_assigned_location score. The
  values of markers[sort.by] annotation are used.}
\examples{

##Example 1
file1<-system.file("extdata","Data_Fig_1a.txt",package="MetaMass")
file1

##proteins identified by genename -> annotation.ID=2 (see ?AnnotationAM)
##cluster with respect metadata only (group=0)
res1<-analyze.MSfile(MSfile=file1,Metadata=c("Christoforou"),output="res1",group=0,cluster.metadata=TRUE)

##Example 2
file2<-system.file("extdata","Data_Fig_1b.txt",package="MetaMass")
file2
##cluster with respect MSfile only (cluster.metadata=FALSE)
res2<-analyze.MSfile(MSfile=file2,Metadata=c("Christoforou"),output="res2")

##Example 3
##compare multiple files component fractionation with Metadata
files1<-system.file("extdata",c("Bileck.txt","Thakar.txt","Carvalho.txt"),package="MetaMass")
files1
res3<-analyze.MSfile(MSfile=files1,Metadata=c("Christoforou"),output="res3")


}
