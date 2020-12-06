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
names (X.tr) = substr(as.character(titles[,2]),1,10)



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

# replace zeros with NA
X.tr.full.col[X.tr.full.col == 0] <- NA
for (i in colnames(X.tr.full.col)){
  #replacing the NA values with the mean for each column
  X.tr.full.col[,i][is.na(X.tr.full.col[,i])] <- mean(X.tr.full.col[,i], na.rm = T)
}

trtr.full.col = data.frame (X = X.tr.full.col[-va.id,],y=y.tr[-va.id,])
va.full.col = data.frame (X = X.tr.full.col[va.id,],y=y.tr[va.id,])


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




########### RMSE on a full X, filling the 0's with the mean of the row.

X.tr.full.row = X.tr
X.te.full.row = X.te
colnames(X.te.full.row) <- colnames(X.tr.full.row)
row.means <- rep(NaN, nrow(X.tr.full.row))

for (j in rownames(X.tr.full.row)){
  
  #getting the mean without the zeroes:
  curr.row <- data.frame(X.tr.full.row[j ,])
  curr.row <- t(curr.row)
  curr.row <- curr.row[colSums(curr.row != 0) > 0, ]
  row.means[j] <- mean(curr.row) 
  
  for (i in colnames(X.tr.full.row)){
    
    if (X.tr.full.row[j, i] == 0){
      X.tr.full.row[j, i] <- row.means[j]
    }
    #we use strtoi because we need the integer part of j, which for some reason
    # is not trivial in this stupid language, which thinks "10000"<2931 is True
    #if (strtoi(j) < 2931){
      #if (X.te.full[j, i] == 0){
        #X.te.full[j, i] <- a
        
      #}
    #}
    
  }
}

trtr.full.row = data.frame (X = X.tr.full.row[-va.id,],y=y.tr[-va.id,])
va.full.row = data.frame (X = X.tr.full.row[va.id,],y=y.tr[va.id,])


print("the row model results are:")
lin.mod.full.row = lm (y~.,data = trtr.full.row)

print("insample:")
lin.pred.rows.insample = predict(lin.mod.full.row, newdata = trtr.full.row)
lin.pred.rows.insample = pmin(lin.pred.rows.insample, 5)
lin.pred.rows.insample = pmax(lin.pred.rows.insample, 1)
print(sqrt(mean((trtr.full.row$y-lin.pred.rows.insample)^2)))


print("on validation")
lin.pred.rows.va = predict(lin.mod.full.row, newdata = va.full.row)
lin.pred.rows.va = pmin(lin.pred.rows.va, 5)
lin.pred.rows.va = pmax(lin.pred.rows.va, 1)
print(sqrt(mean((va.full.row$y-lin.pred.rows.va)^2)))








########### RMSE on a full X, filling the 0's with the mean of the row if the
########### mean is larger than 4.5, and with the mean of the column otherwise.


X.tr.full.both = X.tr
X.te.full.both = X.te
colnames(X.te.full.both) <- colnames(X.tr.full.both)

for (j in rownames(X.tr.full.both)){
  
  for (i in colnames(X.tr.full.both)){
    #he a lover
    if (row.means[j] > 4.5){
      X.tr.full.both[j, i] <- 5
    #he a hater
    }else if ((row.means[j] < 1.5)){
      X.tr.full.both[j, i] <- 1
    #he is neither  
    }else{
      X.tr.full.both[j, i] <- col.means[i]
    }
    #we use strtoi because we need the integer part of j, which for some reason
    # is not trivial in this stupid language, which thinks "10000"<2931 is True
    #if (strtoi(j) < 2931){
    #if (X.te.full[j, i] == 0){
    #X.te.full[j, i] <- a
    
    #}
    #}
    
  }
}

trtr.full.both = data.frame (X = X.tr.full.both[-va.id,],y=y.tr[-va.id,])
va.full.both = data.frame (X = X.tr.full.both[va.id,],y=y.tr[va.id,])


print("the both model results for 4.5 are:")
lin.mod.full.both = lm (y~.,data = trtr.full.both)

print("insample:")
lin.pred.both.insample = predict(lin.mod.full.both, newdata = trtr.full.both)
lin.pred.both.insample = pmin(lin.pred.both.insample, 5)
lin.pred.both.insample = pmax(lin.pred.both.insample, 1)
print(sqrt(mean((trtr.full.both$y-lin.pred.both.insample)^2)))


print("on validation")
lin.pred.both.va = predict(lin.mod.full.both, newdata = va.full.both)
lin.pred.both.va = pmin(lin.pred.both.va, 5)
lin.pred.both.va = pmax(lin.pred.both.va, 1)
print(sqrt(mean((va.full.both$y-lin.pred.both.va)^2)))



















#trtr = data.frame (X = X.tr[-va.id,], yda=y.da.tr[-va.id,], y=y.tr[-va.id,]) # include dates
trtr.full = data.frame (X = X.tr.full[-va.id,],y=y.tr[-va.id,])
#va = data.frame (X = X.tr[va.id,], yda=y.da.tr[va.id,], y=y.tr[va.id,]) #include dates
va.full = data.frame (X = X.tr.full[va.id,],y=y.tr[va.id,])

lin.mod.full = lm (y~.,data = trtr.full)
lin.pred.full = predict(lin.mod.full, newdata = va.full)
lin.pred.full = pmin(lin.pred.full, 5)
lin.pred.full = pmax(lin.pred.full, 1)
#in sample:
lin.pred.insample = predict(lin.mod.full, newdata = trtr.full)
sqrt(mean((trtr.full$y-lin.pred.insample)^2))
#validation
sqrt(mean((va.full$y-lin.pred.full)^2))


#check everything on test
#not sure whether or not we need a new variable, but I added it for readability
tete.full = data.frame(X = X.te.full)

lin.pred.test = predict(lin.mod.full, newdata = tete.full)
lin.pred.test.cap = pmin(lin.pred.test, 5)
lin.pred.test.cap2 = pmax(lin.pred.test, 1)

final.pred <- lin.pred.test.cap2


#check everything on the virgin data
lin.mod = lm (y~.,data = trtr)
lin.pred = predict(lin.mod, newdata = trtr)
sqrt(mean((trtr$y-lin.pred)^2))



