library(psych)
library(MASS)
library(smacof)
?sim2diss
nat_d <- Nwish
rownames(nat_d) <- c("Brazil","Congo","Cuba", "Egypt", "France", "India", "Israel",	"Japan",	"China",	"UdSSR",	"USA",	"Yugoslavia")
view(nat_d)

nat.dissim <- sim2diss(nat_d, method = 7, to.dist = TRUE)
nat_mds <- isoMDS(nat.dissim, k=2)
print(nat_mds)
nat_mds$points
nat_mds$stress
x <- nat_mds$points[,1] 
y <- nat_mds$points[,2] 

#Plot of distances
plot(x, y, main = "Score plot", xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = range(nat_mds$points[,1])*1.2, type = "n")
text(x, y, labels = colnames(nat_d), cex = 1, col = "dark red")
abline(h=0, v=0, col = "forest green", lty = 2)

#Shephards Diagram
ShepNat <- Shepard(nat.dissim, nat_mds$points)

plot(ShepNat, pch = 20, xlab = "Dissimilarity",
     ylab = "Distance", col= "darkgray", main = "Shephard's Diagram")
lines(ShepNat$x, ShepNat$yf, type = "S")

