# Qucik guide


## github
Open git bush and move to the folder you'd like to make a copy of this repository using `cd "path to the working directory"`.
Then run:

```
git clone https://github.com/ShunHasegawa/FACE_IEM.git
```

This will copy (or "clone"") this repository to your local directory.


## Update library
Once you finishe clonning the repository, open .Rproj from the clone. Firstly you need to restore packages which are used for this project. In order to do so, run

```r
packrat::restore()
```

You may get an error message like:

```
Installing cluster (1.15.2) ... Error in download.file(url, destfile, method, mode = "wb", ...) : 
  cannot open URL 'http://cran.rstudio.com/bin/windows/contrib/3.1/cluster_1.15.2.tar.gz'
In addition: Warning message:
In download.file(url, destfile, method, mode = "wb", ...) :
  cannot open: HTTP status was '404 Not Found'
Warning in download.packages(pkgRecord$name, destdir = pkgSrcDir, available = availablePkgs,  :
  download of package ‘cluster’ failed
Error in fileLoc[1, 2] : subscript out of bounds
```

In this case try:

```r
for (i in 1:40) try({packrat::restore()}, silent = TRUE) 
# ignore error and run restore() 30 times.
```

If you find `Matrix` repeatedly in the error mesages, then stop this by hitting _Esc_, then run:

```r
install.packages("Matrix") # for some reasons it's not installed from packrat
```

Then, repaeat the above code till you get `Already up to date`. Once you get this messgae, you got all packages restored and this project is ready to be used.