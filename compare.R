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


########### RMSE on a full X, filling the 0's with the mean of the column.

X.tr.full.col = X.tr
X.te.full.col = X.te
colnames(X.te.full.col) <- colnames(X.tr)

# replace zeros with NA
X.tr.full.col[X.tr.full.col == 0] <- NA
for (i in colnames(X.tr.full.col)){
  #replacing the NA values with the mean for each column
  X.tr.full.col[,i][is.na(X.tr.full.col[,i])] <- mean(X.tr.full.col[,i], na.rm = T)
}


######################### replace zeros with NA ############################

X.tr.test.col = X.tr
X.te.test.col = X.te
colnames(X.te.full.col) <- colnames(X.tr)

X.tr.test.col[X.tr.test.col == 0] <- NA
for (i in colnames(X.tr.test.col)){
  #replacing the NA values with the mean for each column
  X.tr.test.col[,i][is.na(X.tr.test.col[,i])] <- mean(X.tr.test.col[,i], na.rm = T)
}


######################### Iterate columns and complete mean ############################
X.tr.full.col = X.tr
X.te.full.col = X.te
colnames(X.te.full.col) <- colnames(X.tr)
col.means <- rep(NaN, ncol(X.tr.full.col))

for (i in colnames(X.tr.full.col)){
  
  curr.col <- data.frame(X.tr.full.col[ ,i])
  curr.col <- curr.col[rowSums(curr.col != 0) > 0, ]
  col.means[i] <- mean(curr.col)
  
  for (j in rownames(X.tr.full.col)){
    
    if (X.tr.full.col[j, i] == 0){
      X.tr.full.col[j, i] <- col.means[i]
    }
  }
}

# TODO: compare X.tr.test.col and X.tr.full.col