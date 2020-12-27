########### Training data (rankings only, no dates):
con = url("http://www.tau.ac.il/~saharon/StatsLearn2020/train_ratings_all.dat")
X.tr = read.table (con)
con = url("http://www.tau.ac.il/~saharon/StatsLearn2020/train_y_rating.dat")
y.tr = read.table (con)

con = url("http://www.tau.ac.il/~saharon/StatsLearn2020/train_y_date.dat")
y.da.tr = read.table (con)

########### Test Data
con = url("http://www.tau.ac.il/~saharon/StatsLearn2020/test_ratings_all.dat")
X.te = read.table (con)

con = url("http://www.tau.ac.il/~saharon/StatsLearn2020/test_y_date.dat")
y.da.te = read.table (con)



con = url("http://www.tau.ac.il/~saharon/StatsLearn2020/movie_titles.txt")
titles = read.table(con,sep=",")
names (X.tr) = substr(as.character(titles[,2]),1,50)


########### Divide training data into training and validation
n = dim(X.tr)[1]
nva = 2000
ntr = n-nva
va.id = sample (n,nva) # choose 2000 points for validation



#The data is assumed to only have the X part, meaning we pass X.tr, X.te to it
X.bool <- X.tr
X.bool[X.bool == 0] <- NA
for (i in colnames(X.bool)){
  X.bool[,i][!is.na(X.bool[,i])] <- 1
  X.bool[,i][is.na(X.bool[,i])] <- 0
}


trtr.full.bool = data.frame (X = X.with.bool[-va.id,],y=y.tr[-va.id,])
va.full.bool = data.frame (X = X.with.bool[va.id,],y=y.tr[va.id,])
te.full.bool = data.frame (X = X.with.bool)



lin.mod.full.bool = lm (y~.,data = trtr.full.bool)

print("insample:")
lin.pred.bool.insample = predict(lin.mod.full.bool, newdata = trtr.full.bool)
lin.pred.bool.insample = pmin(lin.pred.bool.insample, 5)
lin.pred.bool.insample = pmax(lin.pred.bool.insample, 1)
print(sqrt(mean((trtr.full.bool$y-lin.pred.bool.insample)^2)))


print("on validation")
lin.pred.bool.va = predict(lin.mod.full.bool, newdata = va.full.bool)
lin.pred.bool.va = pmin(lin.pred.bool.va, 5)
lin.pred.bool.va = pmax(lin.pred.bool.va, 1)
print(sqrt(mean((va.full.bool$y-lin.pred.bool.va)^2)))
