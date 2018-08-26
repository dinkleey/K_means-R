#======================== load dataset =================================
library(readxl)
df <- read_excel("D:/KNN/dataframe.xlsx")
summary(df)
df$chain<-as.factor(df$chain)
summary(df$chain)

#normalize/scale  data

library(caret)
df_scl<-df_st
summary(df_scl)

featureNormalize<-function(x){
  (x-min(x))/(max(x)-min(x))
}

df_scl[3:74]<-lapply(df_scl[3:74], FUN = featureNormalize)
summary(df_scl)
df_kmeans<-df_scl[3:74]
summary(df_kmeans)
str(df_kmeans)

#df_upload<-df_scl[3:74]

#=============================== kmeans ======================================

#det. no of clusters

errors_kmean <- function(dat) {
  list_errors <- c()
  for (nr_clusters in 2:10) {
    kmodel <- kmeans(x = dat, centers = nr_clusters, nstart = 5)
    list_errors <- c(list_errors, kmodel$tot.withinss)
  }
  return(list_errors)
}

errors<-errors_kmean(df_kmeans)
plot(x=2:10, y=errors)

km_pharm<-kmeans(df_kmeans, centers = 5, nstart = 30)
df_scl["cluster"]<-km_pharm$cluster
df_cl<-data.frame(km_pharm$centers)
df_cl["score"]<-rowSums(df_cl)
df_cl["orig_cl"]<-1:5
df_cl<-df_cl[order(df_cl$score),]
df_cl["CLUSTER_ORD"]<-5:1
df_pharm_seg<-merge(x=df_scl, y=df_cl, by.x = "cluster", by.y = "orig_cl")


str(df_pharm_seg)
colnames(df_pharm_seg)



#============================== import metadata for https://projector.tensorflow.org/ visualization & merge dfs===================

mtdt<- read_excel("D:/PharmaOanaCod/KNN pharmacies/mtdt.xlsx")
df_all<-merge(x=df_pharm_seg, y=mtdt, by.x = "unitid", by.y = "PHID")

head(df_all[3:75])
str(df_all)

df_upload<-df_all[4:75]
colnames(df_all)
df_mtdt<-df_all[c(3,149:157)]



# ============================= write tsvs ========================

write.table(df_upload, file='CHAIN.tsv', quote=FALSE, sep="\t", 
            col.names = FALSE, row.names = FALSE)


write.table(df_mtdt, file = "MTDT.tsv", quote = FALSE, sep = "\t", 
            col.names = TRUE, row.names = FALSE)

colnames(df_all)
df_all<-df_all[1:157]
colnames(df_all)
write.csv(df_all, file = "df_final_all.csv") 




