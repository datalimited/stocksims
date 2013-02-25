Title
========================================================

Calculates "r" from a Leslie matrix
--------------------------------------------------------
  

```r
library(FLAdvice)
library(popbio)
library(biodyn)

load("/home/laurie/Desktop/Dropbox/collaboration/LaurieIago/bd.RData")

brp = lh(lh)
```


Calculate "r"

```r
les = leslie(brp, fbar = c(refpts(brp)["crash", "harvest"]))

log(lambda(les))
```

```
## [1] 0.5736
```



Set up biodyn

```r
tst = biodyn("pellat", FLPar(r = 0.8, k = 1000, b0 = 0.8, p = 1), catch = catch(stock))
tst = fwd(tst, catch = catch(stock))

names(dimnames(tst@catch))[1] = "quant"
```


What do the starting guesses look like

```r
ggplot(as.data.frame(mcf(FLQuants(obs = stock(stock), hat = stock(tst))), drop = T)) + 
    geom_line(aes(year, data, group = qname, col = qname))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


Fit 

```r
setParams(tst) = stock(stock)
setControl(tst) = params(tst)

control(tst)[c("b0", "p"), "phase"] = -1

tst = pella(tst, stock(stock))
```


Fitted conpared to actual

```r
ggplot(as.data.frame(mcf(FLQuants(obs = stock(stock), hat = stock(tst))), drop = T)) + 
    geom_line(aes(year, data, group = qname, col = qname))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


Residuals look OK!

```r
plot(tst@diags$residual)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 




```r
plot(stock)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 



```r
plot(tst)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'as.data.frame': Error in FLQuants(lapply(fn, function(fn, x)
## quantile(fn(x), probs = probs, : error in evaluating the argument 'object'
## in selecting a method for function 'FLQuants': Error in quantile(fn(x),
## probs = probs, na.rm = T) : error in evaluating the argument 'x' in
## selecting a method for function 'quantile': Error: could not find function
## "fn" Calls: lapply -> FUN -> quantile
```

