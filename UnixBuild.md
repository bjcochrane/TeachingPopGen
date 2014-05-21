Title
========================================================

Let's see if we can install our nascent package on Unix

```r
install.packages("TeachingPopGen", repos = NULL, type = "source")
```

```
## Installing package into '/home/bruce/R/x86_64-pc-linux-gnu-library/3.1'
## (as 'lib' is unspecified)
```

```
## Warning: installation of package 'TeachingPopGen' had non-zero exit status
```


OK.  All the depends need to be installed.  And for Unix, it is important to install the public key and do sudo apt-get update and sudo apt-get upgrade.  I'm wondering if there is a way this can be automated in the install.packages 



Trying a compiled c load per [this](http://users.stat.umn.edu/~geyer/rc/)

```r
dyn.load("foo.so")
```

Had to move it into the working directory, but it's a start.  What we'll do is to write a function to call it, stick it in the package, and see if it will fly like that.  But let's try a call first

```r
x = .C("foo", n = 5, x = as.double(seq(0.2, 1, 0.2)))
x
```

```
## $n
## [1] 5
## 
## $x
## [1] 0.2 0.4 0.6 0.8 1.0
```

OK.  It works, but it's rather awkward.  Try a wrapper function

```r
foof <- function(x) {
    if (!is.numeric(x)) 
        stop("argument x must be numeric")
    out <- .C("foo", n = as.integer(length(x)), x = as.double(x))
    return(out$x)
}
```

OK.  That works.  Now to put it into a file and add it to the package.  
