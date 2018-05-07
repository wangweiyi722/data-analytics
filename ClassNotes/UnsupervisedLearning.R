Principal Components
====================
  We will use the `USArrests` data (which is in R)
```{r}
dimnames(USArrests)
apply(USArrests,2,mean)
apply(USArrests,2, var)
```

We see that `Assault` has a much larger variance than the other variables. It would dominate the principal components, so we choose to standardize the variables when we perform PCA

```{r}
pca.out=prcomp(USArrests, scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out, scale=0)
```

K-Means Clustering
==================
  K-means works in any dimension, but is most fun to demonstrate in two, because we can plot pictures.
Lets make some data with clusters. We do this by shifting the means of the points around.
```{r}
set.seed(101)
x=matrix(rnorm(100*2),100,2)
xmean=matrix(rnorm(8,sd=4),4,2)
which=sample(1:4,100,replace=TRUE)
x=x+xmean[which,]
plot(x,col=which,pch=19)

# ggplot alternative:
df <- data.frame(x, which)
names(df)
df %>% ggplot(aes(x=X1, y=X2, colour = as.factor(which))) + geom_point()
```
We know the "true" cluster IDs, but we wont tell that to the `kmeans` algorithm.

```{r}
km.out=kmeans(x,4,nstart=15)
km.out
plot(x,col=km.out$cluster,cex=2,pch=1,lwd=2)
points(x,col=which,pch=19)
points(x,col=c(4,3,2,1)[which],pch=19)

# ggplot alternative:
km.out$cluster
df <- data.frame(x, km.out$cluster)
names(df)
df %>% ggplot(aes(x=X1, y=X2, colour = as.factor(km.out.cluster))) + geom_point()

df1 <- data.frame(km.out$centers, km.out$size)
names(df1)
df %>% ggplot() + geom_point(mapping = aes(x=X1, y=X2, colour = as.factor(km.out.cluster))) + geom_point(data=df1, mapping=aes(X1, X2, size=km.out.size))
```

Hierarchical Clustering
=======================
  We will use these same data and use hierarchical clustering

```{r}
hc.complete=hclust(dist(x),method="complete")
plot(hc.complete)
hc.single=hclust(dist(x),method="single")
plot(hc.single)
hc.average=hclust(dist(x),method="average")
plot(hc.average)

```
Lets compare this with the actualy clusters in the data. We will use the function `cutree` to cut the tree at level 4.
This will produce a vector of numbers from 1 to 4, saying which branch each observation is on. You will sometimes see pretty plots where the leaves of the dendrogram are colored. I searched a bit on the web for how to do this, and its a little too complicated for this demonstration.

We can use `table` to see how well they match:
  ```{r}
hc.cut=cutree(hc.complete,4)
table(hc.cut,which)
hc.cut
which
table(hc.cut,km.out$cluster)
```
or we can use our group membership as labels for the leaves of the dendrogram:
  ```{r}
plot(hc.complete,labels=which)
```

