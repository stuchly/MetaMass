construct.AM<-function(annotation,data,annotation.ID=1,data.ID=1,annotation.component=3,group_names=NULL,meta_gr=1){
    colnames(data)<-paste("data",gsub("\\s","_",gsub("\\s+"," ",colnames(data))),sep="_")
    data.ID<-colnames(data)[data.ID]
    if(any(duplicated(data[,data.ID]))) warning("Protein IDs duplicated in MS data.frame")
    if (any(duplicated(data[,data.ID]))) {
        warning("Duplicated IDs are not alowed. Duplicated IDs are removed.")
        data<-data[-which(duplicated(data[,data.ID[1]])),]
    }
    colnames(annotation)<-paste("Annot",gsub("\\s","_",gsub("\\s+"," ",colnames(annotation))),sep="_")
    annotation.ID<-colnames(annotation)[annotation.ID]
    annotation.component<-colnames(annotation)[annotation.component]
    if(any(duplicated(annotation[,annotation.ID]))) warning("Protein IDs duplicated in Annotation data.frame")
    if (any(duplicated((annotation[,annotation.ID])))) {
        warning("Duplicated IDs are not alowed. Duplicated IDs are removed.")
        annotation<-annotation[-which(duplicated(annotation[,annotation.ID])),]
    }
    empty.lines<-which(is.na(annotation[,annotation.ID]) | annotation[,annotation.ID]=="")
    if (length(empty.lines)>0) annotation<-annotation[-empty.lines,]

    empty.lines<-which(is.na(data[,data.ID]) | data[,data.ID]=="")
    if (length(empty.lines)>0) data<-data[-empty.lines,]

    for (i in annotation.component) annotation[,i]<-toupper(annotation[,i])
    if (length(annotation.component)==1){
        components<-unique(annotation[,annotation.component])
        empty.lines<-which(components=="" | is.na(components))
        if (length(empty.lines)>0) components<-components[-empty.lines]
        components<-list(components)
    } else {
        components<-lapply(annotation.component,FUN=function(i,annotation){fr<-annotation[,i];ss<-which(fr=="" | is.na(fr)); if(length(ss)>0) fr<-fr[-ss];return(unique(fr))},annotation=annotation)
    }


    for (i in 1:length(annotation.component)) message(paste("Cellular components:",paste(components[[i]],collapse=" ")))
    data.cols<-which(sapply(data,is.numeric))

    message(paste("MS data columns:", paste(data.cols,collapse=" ")))
    jumps<-which(diff(data.cols)>1)
    ngroups<-length(jumps)+1
    groups<-list()[1:ngroups]
    start<-1
    names(jumps)<-NULL
    if (length(jumps)>0){
        for (i in 1:length(jumps)){
            groups[[i]]<-c(start,jumps[i])
            start<-jumps[i]+1

        }
    }
    groups[[ngroups]]<-c(start,length(data.cols))

    present<-matrix(1,ncol=ngroups,nrow=nrow(data))
    if (!is.null(group_names)){
        if (length(groups)!=length(group_names)) stop("Nb of groups and group names differ")
        colnames(present)<-names(groups)<-group_names
    }

    for (i in meta_gr:ngroups){
        cls<-c(groups[[i]][1]:groups[[i]][2])
        ss<-rowSums(data[,data.cols[cls]])
        selNA<-which(ss==length(cls))
        present[selNA,i]<-0


    }


    res<-list(annotation=list(annotation=annotation,ID=annotation.ID,components=components,components.col=annotation.component),data=list(data=data,ID=data.ID,data.cols=data.cols,groups=groups,present=present))
    class(res)<-"AnnoMass"

    return(res)


}

cluster.groups<-function(AM,group=NULL,subset=NULL){
    sel<-1:nrow(AM$data$data)
    if (!is.null(subset)) sel<-subset
    grS<-NULL
    if (!is.null(group)){
        grS<-NULL
        for (i in group){
            grS<-c(grS,c(AM$data$groups[[i]][1]:AM$data$groups[[i]][2]))
        }
    }
    if (is.null(grS)) grS<-1:length(AM$data$data.cols)
    data<-AM$data$data[sel,]
    if (is.null(group)) group<-1:length(AM$data$groups)
    selNA<-NULL
    for (i in group){
        cls<-c(AM$data$groups[[i]][1]:AM$data$groups[[i]][2])
        ss<-rowSums(data[,AM$data$data.cols[cls]])
        selNA<-unique(c(selNA,which(ss==length(cls))))


    }

    if (length(selNA)>0){

        message(paste("Only", length(sel)-length(selNA), "proteins used in analysis"))
        data<-data[-selNA,AM$data$data.cols[grS]]
    }
    if (nrow(data)==0) stop()
    studies_correlation<-as.dist(1-cor(data))
    hcl<-hclust(studies_correlation)
    return(hcl)




}

run.annotation<-function(AM,method="kmeans",metric="correlation",clusters=60,iter.max=100,nstart=1,group=NULL,subset=NULL){
    sel<-1:nrow(AM$data$data)
    grS<-NULL

    if (!is.null(group)){

        grS<-NULL
        for (i in group){
            grS<-c(grS,c(AM$data$groups[[i]][1]:AM$data$groups[[i]][2]))
        }
    }

    if (is.null(grS)) grS<-1:length(AM$data$data.cols)
    if (!is.null(subset)) sel<-subset
    if (length(sel)==0) stop("No annotated protein ID in data")

    message("Clustering...")
    if (method=="kmeans") {
        if (metric=="euclidean") res.cluster<-kmeans(AM$data$data[sel,AM$data$data.cols[grS]],centers=clusters,nstart=nstart,iter.max=iter.max) else res.cluster<-Kmeans(x=AM$data$data[sel,AM$data$data.cols[grS]],method=metric,centers=clusters,iter.max=iter.max,nstart=nstart)
    }
    if (method=="pam"){
        if (metric %in% c("euclidean","manhattan")) res.cluster<-pam(x=AM$data$data[sel,AM$data$data.cols[grS]],metric=metric,k=clusters)
        if (metric=="correlation"){
            message("building distance matrix")
            dd<-as.dist(1-cor(t(AM$data$data[sel,AM$data$data.cols[grS]])))
            message("done")
            res.cluster<-pam(x=dd,k=clusters)
        }


    }
    if (method=="crude"){
        data_loc<-AM$data$data[sel,AM$data$data.cols[grS]]
        if (ncol(data_loc)>1) for (ii in 2:ncol(data_loc)) data_loc[,ii]<--data_loc[,ii]
        ord<-do.call(order,data_loc)

        res.cluster<-list(cluster=rep(NA,length(sel)))

        cls<-data.frame(ord=ord,cls=rep(1:clusters,each=ceiling(length(sel)/clusters))[1:length(sel)])

        res.cluster$cluster[cls$ord]<-cls$cls

    }

    message("Running cluster annotations...")

    AM$data$data$clusters<-NA

    AM$data$data$clusters[sel]=res.cluster$cluster
    AM$results$clusters<-list()[1:length(AM$annotation$components)]
    for (i in 1:length(AM$annotation$components)){
        incMat<-matrix(NA,nrow=clusters,ncol=length(AM$annotation$components[[i]]))

        colnames(incMat)<-AM$annotation$components[[i]]

        resData<-merge(AM$annotation$annotation,AM$data$data,by.x=AM$annotation$ID,by.y=AM$data$ID)

        TP.FP<-rep(NA,clusters)
        for (fr in AM$annotation$components[[i]]){
            for (cl in 1:clusters){

                incMat[cl,fr]<-length(grep(fr,resData[resData$clusters==cl,AM$annotation$components.col[i]]))
            }
        }
        incMat<-data.frame(cluster=1:clusters,incMat,Nb_of_annotations=NA,purity_main_component=NA,main_component=NA,stringsAsFactors=FALSE)

        incMat$Nb_of_annotations=rowSums(incMat[,AM$annotation$components[[i]]])

        loc<-incMat[,AM$annotation$components[[i]]]/incMat$Nb_of_annotations

        whichMain<-apply(loc,MARGIN=1,FUN=function(x) which.max(x)[1])
        purity_main_component<-sapply(1:length(whichMain),FUN=function(i,whichMain,loc) loc[i,whichMain[i]],loc=loc,whichMain=whichMain)
        ss<-which(sapply(purity_main_component,is.null))

        if (length(ss)>0) purity_main_component[ss]<-NA
        incMat$purity_main_component<-unlist(purity_main_component)

        incMat$main_component<-AM$annotation$components[[i]][whichMain]
        colnames(loc)<-paste(colnames(loc),"ratio",sep="_")
        AM$results$clusters[[i]]<-data.frame(incMat,loc,stringsAsFactors=FALSE)
        for (cl in 1:clusters){

            fr<-incMat$main_component[cl]


            NbFr<-length(grep(fr,resData[,AM$annotation$components.col[i]]))

            TP<-length(grep(fr,resData[resData$clusters==cl,AM$annotation$components.col[i]]))

            FP<-length(which(!(resData[resData$clusters==cl,AM$annotation$components.col[i]] %in% c("",fr)) & !is.na(resData[resData$clusters==cl,AM$annotation$components.col[i]])))

            NbOther<-length(which(!(resData[,AM$annotation$components.col[i]] %in% c("",fr)) & !is.na(resData[,AM$annotation$components.col[i]])))

            TP.FP[cl]<-(TP/NbFr)/(FP/NbOther)



        }
        ##AM$results$clusters[[i]]$TP.FP<-TP.FP
        AM$data$data[,paste("main_component",i,sep="_")]<-AM$results$clusters[[i]]$main_component[AM$data$data$clusters]
        AM$data$data[,paste("purity_main_component",i,sep="_")]<-AM$results$clusters[[i]]$purity_main_component[AM$data$data$clusters]

    }
    return(AM)



}

order.AM<-function(AM,ordering=NULL,rID=1){
    if (is.null(ordering)) ord<-order(AM$results$clusters[[rID]]$main_component) else ord<-ordering
    cls<-1:nrow(AM$results$clusters[[rID]])
    names(cls)<-ord

    AM$results$clusters[[rID]]$updated_order<-cls[as.character(1:nrow(AM$results$clusters[[rID]]))]
    AM$data$data$updated_order<-AM$results$clusters[[rID]]$updated_order[AM$data$data$clusters]
    ord<-order(AM$data$data$updated_order)
    AM$data$data<-AM$data$data[ord,]

    return(AM)

}
write.cdt<-function(AM,file="results.cdt",annotation=FALSE,rsID=1){
    AM$data$data[,1]<-as.character(AM$data$data[,1])
    dd<-AM$data$data
    dd<-data.frame(dd,ord=1:nrow(dd))
    dd<-merge(AM$annotation$annotation,dd,by.x=AM$annotation$ID,by.y=AM$data$ID)
    ord<-order(dd$ord)
    Annot<-dd[ord,AM$annotation$components.col[rsID]]
    if (length(grep("assigned_location",colnames(AM$data$data)))>0) AM$data$data[,1]<-paste(AM$data$data[,1],AM$data$data$clusters,"Marker=",Annot,"Assign=",AM$data$data[,paste("assigned_location",rsID,sep="_")]) else AM$data$data[,1]<-paste(AM$data$data[,1],AM$data$data$clusters,"Marker=",Annot,"Assign=",AM$data$data[,paste("main_component",rsID,sep="_")])
    data<-AM$data$data
    data.cols<-AM$data$data.cols
    endC<-max(data.cols)
    data[,data.cols]<-t(scale(t(data[,data.cols]),center=FALSE))
    data<-data[,1:endC]
    data<-data.frame(Gene_cluster=data[,1],NAME=data[,1],GWEIGHT=1,data[,-1],stringsAsFactors=FALSE)
    data2<-data[1,]
    for (i in 1:ncol(data2)) data2[,i]<-as.character(data2[,i])
    data2[1,]<-c("EWEIGHT","",rep(1,ncol(data)-2))
    data<-rbind(data2,data)

    write.table(data,file=file,row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)
    return(invisible(data))
}

get.data<-function(AM,annotation=TRUE,data.only=FALSE,out=FALSE,fulltext=FALSE){
    if(out){
        ss<-which(colnames(AM$data$data)=="updated_order")
        AM$data$data<-AM$data$data[,-ss]
        ss<-(AM$data$data.cols[length(AM$data$data.cols)]+1):ncol(AM$data$data)
        s1<-grep("^main_component_([0-9]+)$",colnames(AM$data$data))
        s1p<-grep("^purity_main_component_([0-9]+)$|^main_component_([0-9]+)$",colnames(AM$data$data)[ss])
        ss2<-(AM$data$data.cols[length(AM$data$data.cols)]+2):ncol(AM$data$data)
        anots<-as.numeric(gsub("^main_component_([0-9]+)$","\\1",colnames(AM$data$data)[s1]))
        cc<-colnames(AM$data$data)[ss2]
        for (i in anots) cc<-gsub(paste("_",i,"$",sep=""),sub("^Annot","",AM$annotation$components.col[i]),cc)
        colnames(AM$data$data)[ss2]<-cc

        if (fulltext){
            Annot_out<-AM$annotation$annotation[,c(colnames(AM$annotation$annotation)[1],AM$annotation$ID,AM$annotation$components.col)]
            data(full_text_annotation,envir =  environment())
            Annot_out<-merge(Annot_out,full_text_annotation[,-2],by.x=1,by.y=1,all.x=TRUE)[,-1]
            AA<<-Annot_out
        } else {
            Annot_out<-AM$annotation$annotation[,c(AM$annotation$ID,AM$annotation$components.col)]
        }
        return(merge(Annot_out,cbind(AM$data$data[,ss[-s1p]],AM$data$data[,-ss]),by.x=AM$annotation$ID,by.y=AM$data$ID))
    }
    if (!annotation) {
        if (data.only){
            out<-cbind(AM$data$data[,AM$data$ID],cluster=AM$data$data$updated_order,AM$data$data[,AM$data$data.cols])
            colnames(out)[1]<-AM$data$ID
            return(out)
        } else {return(AM$data$data)}
    }  else {
        if (data.only){
            out<-cbind(AM$data$data[,AM$data$ID],cluster=AM$data$data$updated_order,AM$data$data[,AM$data$data.cols])
            colnames(out)[1]<-AM$data$ID
            return(merge(AM$annotation$annotation,out,by.x=AM$annotation$ID,by.y=AM$data$ID))}
        else {
            return(merge(AM$annotation$annotation,AM$data$data,by.x=AM$annotation$ID,by.y=AM$data$ID))
        }

    }
}

get.clusters<-function(AM,rID=1) return(AM$results$clusters[[rID]])

set.components<-function(AM,clusterRes=NULL,components=NULL,rID=1,col="main_component"){
    if (!is.null(clusterRes)){
        AM$results$clusters[[rID]]$main_component<-clusterRes$main_components
        AM$data$data$main_component<-AM$results$clusters[[rID]]$main_component[AM$data$data$clusters]
    }
    if(!is.null(components)){

        uu<-unique(AM$results$clusters[[rID]][,col])

        if (length(which(is.na(uu)))>0) uu<-uu[-which(is.na(uu))]

        if (length(intersect(components,uu))!=length(uu)) stop("incorrect components!")

        AM$results$clusters[[rID]][,col]<-factor(AM$results$clusters[[rID]][,col],levels=components)
    }
    return(AM)
}

get.components<- function(AM) AM$annotation$components

get.presence<-function(AM) AM$data$present

roc.AM1<-function(AM,rID=NULL,component=NULL){
    if (is.null(rID)) rID<-1:length(AM$annotation$components)
    dd<-get.data(AM)

    if (is.null(component)){
        component<-unique(unlist(AM$annotation$components))
    }
    res<-list()[1:length(component)]
    names(res)<-component

    for (i in component){
        comList<-list()[1:length(rID)]
        res[[i]]<-comList
        for (j in 1:length(rID)){
            cls<-get.clusters(AM,rID=j)
            ##sel<-which(dd[,paste("main_component",j,sep="_")]==i)
            sel<-which(dd[,AM$annotation$components.col[j]]==i)
            ## pur<-cls$purity_main_component[dd$clusters[sel]]
            if (is.null(cls[dd$clusters[sel],paste(i,"ratio",sep="_")])) next else pur<-cls[dd$clusters[sel],paste(i,"ratio",sep="_")]
            purity<-sort(unique(pur),decreasing=TRUE)

            if (length(purity)>0) loc<-data.frame(purity=purity,ratio=NA) else {
                                                                              loc<-NULL
                                                                              next
                                                                          }
            N<-length(purity)
            for (a in 1:N){
                loc[a,2]<-length(which(pur>=purity[a])) /length(sel)

            }
            loc<-rbind(c(purity=1,ratio=0),loc,c(purity=0,ratio=1))

            res[[i]][[j]]<-loc
        }
    }
    reso<-list()[1:2]
    names(reso)<-c("Annotation","rocAM")
    reso[[1]]<-sub("^Annot_","",AM$annotation$components.col)
    reso[[2]]<-res
    class(reso)<-"rocAM"
    return(reso)
}

roc.AM<-function(AM,rID=NULL,component=NULL){
    if (is.null(rID)) rID<-1:length(AM$annotation$components)
    dd<-get.data(AM)

    if (is.null(component)){
        for (i in 1:length(AM$annotation$components)) {
            sel<-which(dd[,AM$annotation$components.col[i]] %in% c("LYSOSOME","ENDOSOME"))
            dd[sel,AM$annotation$components.col[i]]<-"LYSOSOME&ENDOSOME"
            sel<-which(dd[,AM$annotation$components.col[i]] %in% c("NUCLEOLUS"))
            dd[sel,AM$annotation$components.col[i]]<-"NUCLEUS"
        }
        component<-NULL
        for (i in 1:length(AM$annotation$components)) component<-unique(c(component,unique(as.character(get.clusters(AM,rID=i)$assigned_location))))
        component<-as.character(na.omit(component))

    }
    res<-list()[1:length(component)]
    names(res)<-component

    for (i in component){
        comList<-list()[1:length(rID)]
        res[[i]]<-comList
        for (j in 1:length(rID)){
            cls<-get.clusters(AM,rID=j)
            ##sel<-which(dd[,paste("main_component",j,sep="_")]==i)

            sel<-which(dd[,AM$annotation$components.col[j]]==i)

            ## pur<-cls$purity_main_component[dd$clusters[sel]]
            if (is.null(cls[dd$clusters[sel],"purity_assigned_location"])) next else pur<-cls[dd$clusters[sel],"purity_assigned_location"]
            purity<-sort(unique(pur),decreasing=TRUE)

            if (length(purity)>0) loc<-data.frame(purity=purity,ratio=NA) else {
                                                                              loc<-NULL
                                                                              next
                                                                          }
            N<-length(purity)
            for (a in 1:N){
                loc[a,2]<-length(which(pur>=purity[a])) /length(sel)

            }
            loc<-rbind(c(purity=1,ratio=0),loc) #,c(purity=0,ratio=max(loc[a,2],1)))

            res[[i]][[j]]<-loc
        }
    }
    reso<-list()[1:2]
    names(reso)<-c("Annotation","rocAM")
    reso[[1]]<-sub("^Annot_","",AM$annotation$components.col)
    reso[[2]]<-res
    class(reso)<-"rocAM"
    return(reso)
}



plot.prAM<-function(rocAM){  ##,mfrow=c(1,1),mar=c(1, 4, 2.2, 1) + 0.1)
    ## par(mfrow=mfrow,mar=mar)

    if (class(rocAM)!="rocAM") {if (class(rocAM)=="AnnoMass") rocAM<-roc.AM(rocAM) else stop()}
    annotation<-rocAM[[1]]
    rocAM<-rocAM[[2]]
    for(i in 1:length(rocAM)){

        for (jj in 1:(length(rocAM[[i]]))) if (!is.null(rocAM[[i]][[jj]])) break
        if (is.null(rocAM[[i]][[jj]])) next
        xlm1<-max(as.numeric(unlist(rocAM[[i]])),na.rm=TRUE)
        plot(rocAM[[i]][[jj]][,c(2,1)],type="l",col=jj,lty=jj,xlim=c(0,xlm1),ylim=c(0,1),main=names(rocAM)[i],xlab="Recall",ylab="Precision")
        K<-length(rocAM[[i]])
        legend("bottomleft",legend=paste(annotation),lty=1:K,col=1:K,cex=1)
        if (length(rocAM[[i]])<=jj) next

        for (j in (jj+1):length(rocAM[[i]])){

            lines(rocAM[[i]][[j]][,c(2,1)],col=j,lty=j)

        }
    }

}

analyze.MSfile<-function(MSfile,Annotation=NULL,Metadata="Christoforou",annotation.ID=2,data.ID=1,markers=3,group_names=NULL,clusters=500,output="results",sep="\t",method="kmeans",metric="euclidean",iter.max=100,nstart=1,group=NULL,subset=NULL,sort.by=1,cluster.metadata=FALSE,overlap=NULL){
    annotation.component<-markers
    all_ov<-ifelse(is.null(overlap),FALSE,TRUE)
    if (!is.null(output)){
        output.data<-paste(output,"_table.txt",sep="")
        output.roc<-paste(output,"_pr.pdf",sep="")
        output.cdt<-paste(output,"_javatree.cdt",sep="")
    } else output.data<-output.roc<-output.cdt<-NULL
    if (is.null(Annotation)){
        data(AnnotationAM,envir =  environment())
        Annotation<-AnnotationAM
    } else {
        if (!is.data.frame(Annotation)) Annotation<-read.table(Annotation,header=TRUE,stringsAsFactors=FALSE,sep=sep,comment.char="")
    }

    if (!is.null(Metadata)){
        metaD<-list()[1:length(Metadata)]
        names(metaD)<-Metadata
        for (i in Metadata){
            data(list=i,envir =  environment())
            metaD[[i]]<-eval(parse(text=i))

        }
    }


    if (!is.data.frame(MSfile)) if (length(MSfile)>1){
                                    if (length(data.ID)==1) data.ID<-rep(data.ID,length(MSfile))
                                    if (!all(is.character(MSfile))) stop("multiple MSfiles must be filenames")
                                    res<-read.table(MSfile[1],header=TRUE,stringsAsFactors=FALSE,sep=sep,comment.char="")
                                    if (any(duplicated(res[,data.ID[1]]))){
                                        warning("Duplicated IDs are not alowed for multiple files. Duplicated IDs are removed.")
                                        res<-res[-which(duplicated(res[,data.ID[1]])),]
                                    }
                                    for (i in 2:length(MSfile)){

                                        resloc<-read.table(MSfile[i],header=TRUE,stringsAsFactors=FALSE,sep=sep,comment.char="")
                                        if (any(duplicated(resloc[,data.ID[i]]))){
                                            warning("Duplicated IDs are not alowed for multiple files. Duplicated IDs are removed.")
                                            resloc<-resloc[-which(duplicated(resloc[,data.ID[i]])),]
                                        }
                                        res<-merge(res,data.frame(non_NUM_space="-",resloc),by.x=data.ID[1],by.y=data.ID[i]+1,all.x=all_ov,all.y=all_ov)
                                    }
                                    MSfile<-res
                                    data.ID<-1



                                } else  if (!is.data.frame(MSfile)) MSfile<-read.table(MSfile,header=TRUE,stringsAsFactors=FALSE,sep=sep,comment.char="")

    if (!is.null(group) & length(group)==1) if (group==0 & (is.null(Metadata) | !cluster.metadata)) stop("Nothing to cluster!")
    meta_gr<-1
    if (!is.null(Metadata)){
        Metadata<-metaD[[1]]

        if (length(metaD)>1) for (i in 2:length(metaD)){
                                 Metadata<-merge(data.frame(Metadata,non_NUM_space="-"),metaD[[i]],by=1)

                             }
        Metadata<-merge(Annotation[,c(2,annotation.ID)],data.frame(non_NUM_space="-",Metadata),by.x=1,by.y=2)
        Mdata.cols<-which(sapply(Metadata,is.numeric))
        jumps<-which(diff(Mdata.cols)>1)
        ngroups<-length(jumps)+1
        MSdata.cols<-which(sapply(MSfile,is.numeric))
        jumps<-which(diff(MSdata.cols)>1)
        ngroupsMS<-length(jumps)+1
        meta_gr<-ngroupsMS
        if(is.null(group)) group<-1:ngroupsMS

        MSfile<-merge(data.frame(Metadata,non_NUM_space="-"),MSfile,by.x=2,by.y=1,all.x=all_ov,all.y=all_ov)

        if (!is.null(group) & length(group)==1){
            if (group==0) group<-1:ngroups else if (cluster.metadata)  group<-c(1:ngroups,group+ngroups) else group<-group+ngroups
        } else {
            if (cluster.metadata)  group<-c(1:ngroups,group+ngroups) else group<-group+ngroups
        }


    }

    if (all_ov){
        cc<-which(sapply(MSfile,is.numeric))
        ss<-which(is.na(MSfile[,cc]),arr.ind=TRUE)
        MSFc<-MSfile[,cc]
        MSFc[is.na(MSFc)]<-1
        MSfile[,cc]<-MSFc
    }


    AM<-construct.AM(Annotation,MSfile,annotation.ID=annotation.ID,data.ID=data.ID,annotation.component=annotation.component,group_names=group_names,meta_gr=meta_gr)

    if (all_ov){
        pres<-rowSums(get.presence(AM))
        subset<-which(pres>=overlap)
    }
    res<-run.annotation(AM,clusters=clusters,method=method,metric=metric,iter.max=iter.max,nstart=nstart,group=group,subset=subset)

    data(AnnotationAM,envir =  environment())
    ctexist<-FALSE


    for (i in 1:length(annotation.component)){

        if (TRUE){
            data(levelsC)
            presentCOMP<-unique(res$results$clusters[[i]][,"main_component"])
            ss<-which(!(presentCOMP %in% levelsC) & !is.na(presentCOMP) & presentCOMP!="")
            ss<-which(!(presentCOMP %in% levelsC))
            presentCOMP<-presentCOMP[ss]
            ss<-which(!is.na(presentCOMP))
            presentCOMP<-presentCOMP[ss]
            ss<-which(levelsC %in% res$results$clusters[[i]][,"main_component"])
            try(res<-set.components(res,rID=i,components=c(levelsC[ss],presentCOMP),col="main_component"))
            cls<-get.clusters(res,rID=i)
            try(categ<-categorize_cluster(cls))
            try(nbct<-categ[[2]])
            try(categ<-categ[[1]])
            score<-2*nbct-cls$Nb_of_annotations
            try(res<-addreplace.column(res,rID=i,assigned_location=categ))
            levelsCo<-levelsC
            levelsC<-levelsC[-4]
            levelsC[4]<-"LYSOSOME&ENDOSOME"
            presentCOMP<-unique(res$results$clusters[[i]][,"assigned_location"])

            ss<-which(!(presentCOMP %in% levelsC) & !is.na(presentCOMP) & presentCOMP!="")
            ss<-which(!(presentCOMP %in% levelsC))
            presentCOMP<-presentCOMP[ss]
            ss<-which(!is.na(presentCOMP))
            presentCOMP<-presentCOMP[ss]
            ss<-which(levelsC %in% res$results$clusters[[i]][,"assigned_location"])
            try(res<-set.components(res,rID=i,components=c(levelsC[ss],presentCOMP),col="assigned_location"))
            levelsC<-levelsCo
            ##try(res<-addreplace.column(res,rID=i,score=score,add2data=FALSE))
            try(res<-addreplace.column(res,rID=i,Nb_main_component=cls$Nb_of_annotations*cls$purity_main_component,Nb_assigned_location=nbct,add2data=FALSE))
            try(res<-addreplace.column(res,rID=i,purity_assigned_location=nbct/cls$Nb_of_annotations,add2data=TRUE))
            ctexist<-TRUE
        }
    }


    cls<-get.clusters(res,rID=sort.by)



    if (ctexist) ord<-order(cls$assigned_location,-cls$Nb_assigned_location, -cls$purity_assigned_location) else ord<-order(cls$main_component,-cls$Nb_main_component, -cls$purity_main_component)
    res<-order.AM(res,ordering=ord)
    data<-get.data(res)
    ord<-order(data$updated_order)
    if (!is.null(output.data)) write.table(get.data(res,out=TRUE,fulltext=TRUE)[ord,],file=output.data,col.names=TRUE,row.names=FALSE,sep=sep)

    if (!is.null(output.cdt)) write.cdt(res,file=output.cdt)

    if (!is.null(output.roc)){

        if (ctexist) Categ<-"assigned_location" else Categ<-"main_component"
        dd<-get.data(res)


        pdf(output.roc)
        par(mfrow=c(2,2),cex=0.5)
        plot.prAM(res)
        dev.off()

    }
    return(invisible(res))
}

addreplace.column<-function(AM,rID=1,add2data=TRUE,...){
    x<-list(...)
    if (any(lapply(x,length)!=nrow(AM$results$clusters[[rID]]))) stop("column must be the same length as number of clusters!")
    namesD<-paste(names(x),rID,sep="_")
    namesCls<-names(x)
    AM$results$clusters[[rID]][,namesCls]<-x
    if (add2data) AM$data$data[,namesD]<-AM$results$clusters[[rID]][AM$data$data$clusters,namesCls]
    return(AM)

}

categorize_cluster<-function(cls){

    res<-rep(NA,nrow(cls))
    counts<-rep(NA,nrow(cls))
    if (is.null(cls$ENDOSOME_ratio)) cls$ENDOSOME_ratio<-0
    if (is.null(cls$PROTEASOME_ratio)) cls$PROTEASOME_ratio<-0
    if (is.null(cls$LYSOSOME_ratio)) cls$LYSOSOME_ratio<-0
    if (is.null(cls$CS_ratio)) cls$CS_ratio<-0
    if (is.null(cls$GOLGI_ratio)) cls$GOLGI_ratio<-0
    if (is.null(cls$NUCLEOLUS_ratio))  cls$NUCLEOLUS_ratio<-0
    if (is.null(cls$PM_ratio)) cls$PM_ratio<-0
    if (is.null(cls$CYTOSOL_ratio)) cls$CYTOSOL_ratio<-0
    ##if (is.null(cls$CYTOSOL_ratio)) print(cls)
    if(is.null(cls$MITOCHONDRION_ratio)) cls$MITOCHONDRION_ratio<-0

    for (i in 1:nrow(cls)){

        Nb<-cls$Nb_of_annotations[i]
        if (cls$Nb_of_annotations[i]==0) next
        if (!is.null(cls$RIBOSOME_ratio)) if (cls$RIBOSOME_ratio[i]>0.51){
                                              res[i]<-"RIBOSOME"
                                              counts[i]<-Nb*cls$RIBOSOME_ratio[i]
                                              next
                                          }
        if ((cls$CYTOSOL_ratio[i]+cls$CS_ratio[i])>=0.51){
            if (cls$CS_ratio[i]>0.3) res[i]<-"CS" else res[i]<-"CYTOSOL"
            counts[i]<-Nb*(cls$CYTOSOL_ratio[i]+cls$CS_ratio[i])
            next
        }
        if ((cls$PM_ratio[i]+cls$ER_ratio[i]+cls$GOLGI_ratio[i]+cls$MITOCHONDRION_ratio[i]+cls$LYSOSOME_ratio[i]+cls$ENDOSOME_ratio[i])>=0.51){
            if ((cls$LYSOSOME_ratio[i]+cls$ENDOSOME_ratio[i])>0.51){
                res[i]<-"LYSOSOME&ENDOSOME"

                next
            }
            rr<-c(PM=cls$PM_ratio[i],ER=cls$ER_ratio[i],GOLGI=cls$GOLGI_ratio[i],MITOCHONDRION=cls$MITOCHONDRION_ratio[i],LYSOSOME=cls$LYSOSOME_ratio[i],ENDOSOME=cls$ENDOSOME_ratio[i])

            res[i]<-names(rr)[which.max(rr)[1]]
            if (res[i] %in% c("LYSOSOME","ENDOSOME")){
                res[i]<-"LYSOSOME&ENDOSOME"
                counts[i]<-Nb*(cls$LYSOSOME_ratio[i]+cls$ENDOSOME_ratio[i])
            } else {
                counts[i]<-Nb*max(rr)
            }
            next
        }

        if ((cls$NUCLEOLUS_ratio[i]+cls$NUCLEUS_ratio[i])>=0.51){
            if (cls$NUCLEOLUS_ratio[i]>0.25) res[i]<-"NUCLEOLUS" else res[i]<-"NUCLEUS"
            counts[i]<-Nb*(cls$NUCLEOLUS_ratio[i]+cls$NUCLEUS_ratio[i])
            next
        }
        i
    }
    ss<-which(is.na(res))

    res[ss]<-as.character(cls$main_component[ss])
    ss<-which(res %in% c("LYSOSOME","ENDOSOME"))
    if (length(ss)>0) res[ss]<-"LYSOSOME&ENDOSOME"
    ss<-which(is.na(counts))
    counts[ss]<-cls$Nb_of_annotations[ss]*cls$purity_main_component[ss]
    return(list(res,counts))

}
