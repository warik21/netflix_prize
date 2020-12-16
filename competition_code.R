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

########### Get to know our data a little:
table (y.tr) # What rankings does our target get?
apply(data.frame(X.tr[,1:14],y.tr),2,mean) # Which movies are liked?
cor(y.tr,X.tr[,1:14]) # which movies correlated with Miss Congeniality?
apply (X.tr==0, 2, sum) # how many missing?
cor (y.tr, y.da.tr) # changes with time?



########### Divide training data into training and validation
n = dim(X.tr)[1]
nva = 2000
ntr = n-nva
va.id = sample (n,nva) # choose 2000 points for validation
trtr.date = data.frame (X = X.tr[-va.id,], yda=y.da.tr[-va.id,], y=y.tr[-va.id,]) # include dates
trtr = data.frame (X = X.tr[-va.id,],y=y.tr[-va.id,])
va.date = data.frame (X = X.tr[va.id,], yda=y.da.tr[va.id,], y=y.tr[va.id,]) #include dates
va = data.frame (X = X.tr[va.id,],y=y.tr[va.id,])


########### baseline RMSE
print("the baseline RMSE is:")
sqrt(mean((va$y-mean(trtr$y))^2))


########### RMSE on a full X, filling the 0's with the mean of the column.

###this part is done to calculate the X.tr.full matrix, it takes a while
X.tr.full.col = X.tr
X.te.full.col = X.te
colnames(X.te.full.col) <- colnames(X.tr)

# replace zeros with NA on train data
X.tr.full.col[X.tr.full.col == 0] <- NA
for (i in colnames(X.tr.full.col)){
  #replacing the NA values with the mean for each column
  X.tr.full.col[,i][is.na(X.tr.full.col[,i])] <- mean(X.tr.full.col[,i], na.rm = T)
}

# replace zeros with NA on test data
X.te.full.col[X.te.full.col == 0] <- NA
for (i in colnames(X.te.full.col)){
  #replacing the NA values with the mean for each column
  X.te.full.col[,i][is.na(X.te.full.col[,i])] <- mean(X.te.full.col[,i], na.rm = T)
}

trtr.full.col = data.frame (X = X.tr.full.col[-va.id,],y=y.tr[-va.id,])
va.full.col = data.frame (X = X.tr.full.col[va.id,],y=y.tr[va.id,])
te.full.col = data.frame (X = X.te.full.col)

print("the column model results are:")
lin.mod.full.col = lm (y~.,data = trtr.full.col)

print("insample:")
lin.pred.cols.insample = predict(lin.mod.full.col, newdata = trtr.full.col)
lin.pred.cols.insample = pmin(lin.pred.cols.insample, 5)
lin.pred.cols.insample = pmax(lin.pred.cols.insample, 1)
print(sqrt(mean((trtr.full.col$y-lin.pred.cols.insample)^2)))

print("on validation")
lin.pred.cols.va = predict(lin.mod.full.col, newdata = va.full.col)
lin.pred.cols.va = pmin(lin.pred.cols.va, 5)
lin.pred.cols.va = pmax(lin.pred.cols.va, 1)
print(sqrt(mean((va.full.col$y-lin.pred.cols.va)^2)))

print("on test")
lin.pred.cols.test = predict(lin.mod.full.col, newdata = te.full.col)
lin.pred.cols.test = pmin(lin.pred.cols.test, 5)
lin.pred.cols.test = pmax(lin.pred.cols.test, 1)

write.csv(data.frame(lin.pred.cols.test), "file.csv")

########### RMSE on a full X, filling the 0's with the mean of the row if the
########### mean is larger than 4.5, and with the mean of the column otherwise.


#X.tr.full.both = X.tr
#X.te.full.both = X.te
#colnames(X.te.full.both) <- colnames(X.tr.full.both)
#
#for (j in rownames(X.tr.full.both)){
#  
#  for (i in colnames(X.tr.full.both)){
#    if (X.tr.full.both[j, i] == 0) {
#      #he a lover
#      if (row.means[j] > 4.5){
#        X.tr.full.both[j, i] <- 5
#      #he a hater
#      } else if (row.means[j] < 1.5){
#        X.tr.full.both[j, i] <- 1
#      #he is neither  
#      }else{
#        X.tr.full.both[j, i] <- col.means[i]
#      }
#    }
#  }
#}




