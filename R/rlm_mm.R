# from MASS Package
# copyright (C) 1994-2016 W. N. Venables and B. D. Ripley

rlm_mm <-
  function(x, y, weights, ..., w = rep(1, nrow(x)),
           init = "ls", psi = psi.huber,
           k2 = 1.345,
           wt.method = c("inv.var", "case"),
           maxit = 20, acc = 1e-4,
           test.vec = "resid",
           lqs.control=NULL)
  {
    irls.delta <- function(old, new)
      sqrt(sum((old - new)^2)/max(1e-20, sum(old^2)))
    irls.rrxwr <- function(x, w, r)
    {
      w <- sqrt(w)
      max(abs((matrix(r * w, 1L, length(r)) %*% x)/
                sqrt(matrix(w, 1L, length(r)) %*% (x^2))))/sqrt(sum(w * r^2))
    }
    wmad <- function(x, w)
    {
      o <- sort.list(abs(x)); x <- abs(x)[o]; w <- w[o]
      p <- cumsum(w)/sum(w)
      n <- sum(p < 0.5)
      if (p[n + 1L] > 0.5) x[n + 1L]/0.6745 else (x[n + 1L] + x[n + 2L])/(2*0.6745)
    }

    # method <- match.arg(method)
    wt.method <- match.arg(wt.method)
    nmx <- deparse(substitute(x))
    if(is.null(dim(x))) {
      x <- as.matrix(x)
      colnames(x) <- nmx
    } else x <- as.matrix(x)
    if(is.null(colnames(x)))
      colnames(x) <- paste("X", seq(ncol(x)), sep="")
    if(qr(x)$rank < ncol(x))
      stop("'x' is singular: singular fits are not implemented in 'rlm'")

    if(!(any(test.vec == c("resid", "coef", "w", "NULL"))
         || is.null(test.vec))) stop("invalid 'test.vec'")
    ## deal with weights
    xx <- x
    yy <- y
    if(!missing(weights)) {
      if(length(weights) != nrow(x))
        stop("length of 'weights' must equal number of observations")
      if(any(weights < 0)) stop("negative 'weights' value")
      if(wt.method == "inv.var") {
        fac <- sqrt(weights)
        y <- y*fac; x <- x* fac
        wt <- NULL
      } else {
        w <- w * weights
        wt <- weights
      }
    } else wt <- NULL

      temp <- do.call("lqs",
                      c(list(x, y, intercept = FALSE, method = "S",
                             k0 = 1.548), lqs.control))
      coef <- temp$coefficients
      resid <- temp$residuals
      psi <- psi.bisquare
      if(length(arguments <- list(...)))
        if(match("c", names(arguments), nomatch = 0L)) {
          c0 <- arguments$c
          if (c0 > 1.548) formals(psi)$c <- c0
          else
            warning("'c' must be at least 1.548 and has been ignored")
        }
      scale <- temp$scale

    done <- FALSE
    conv <- NULL
    n1 <- (if(is.null(wt)) nrow(x) else sum(wt)) - ncol(x)
    theta <- 2*pnorm(k2) - 1
    gamma <- theta + k2^2 * (1 - theta) - 2 * k2 * dnorm(k2)

    for(iiter in 1L:maxit) {
      if(!is.null(test.vec)) testpv <- get(test.vec) ## check this

      w <- psi(resid/scale)
      if(!is.null(wt)) w <- w * weights
      temp <- lm.wfit(x, y, w, method="qr")
      coef <- temp$coefficients
      resid <- temp$residuals
      if(!is.null(test.vec)) convi <- irls.delta(testpv, get(test.vec))
      else convi <- irls.rrxwr(x, w, resid)
      conv <- c(conv, convi)
      done <- (convi <= acc)
      if(done) break
    }
    if(!done)
      warning(gettextf("'rlm' failed to converge in %d steps", maxit),
              domain = NA)
    fitted <- drop(xx %*% coef)
    ## fix up call to refer to the generic, but leave arg name as `formula'
    cl <- match.call()
    cl[[1L]] <- as.name("rlm")
    fit <- list(coefficients = coef, residuals = yy - fitted)
    fit
  }

psi.huber <- function(u, k = 1.345, deriv=0)
{
  if(!deriv) return(pmin(1, k / abs(u)))
  abs(u) <= k
}

psi.hampel <- function(u, a = 2, b = 4, c = 8, deriv=0)
{
  U <- pmin(abs(u) + 1e-50, c)
  if(!deriv) return(ifelse(U <= a, U, ifelse(U <= b, a, a*(c-U)/(c-b) ))/U)
  ifelse(abs(u) <= c, ifelse(U <= a, 1, ifelse(U <= b, 0, -a/(c-b))), 0)
}

psi.bisquare <- function(u, c = 4.685, deriv=0)
{
  if(!deriv) return((1  - pmin(1, abs(u/c))^2)^2)
  t <- (u/c)^2
  ifelse(t < 1, (1 - t)*(1 - 5*t), 0)
}

se.contrast.rlm <-
  function(object, contrast.obj,
           coef = contr.helmert(ncol(contrast))[, 1L],
           data = NULL, ...)
  {
    contrast.weight.aov <- function(object, contrast)
    {
      asgn <- object$assign[object$qr$pivot[1L:object$rank]]
      uasgn <- unique(asgn)
      nterms <- length(uasgn)
      nmeffect <- c("(Intercept)",
                    attr(object$terms, "term.labels"))[1L + uasgn]
      effects <- as.matrix(qr.qty(object$qr, contrast))
      res <- matrix(0, nrow = nterms, ncol = ncol(effects),
                    dimnames = list(nmeffect, colnames(contrast)))
      for(i in seq(nterms)) {
        select <- (asgn == uasgn[i])
        res[i,] <- colSums(effects[seq_along(asgn)[select], , drop = FALSE]^2)
      }
      res
    }
    if(is.null(data)) contrast.obj <- eval(contrast.obj)
    else contrast.obj <- eval(substitute(contrast.obj), data, parent.frame())
    if(!is.matrix(contrast.obj)) { # so a list
      if(!missing(coef)) {
        if(sum(coef) != 0)
          stop("'coef' must define a contrast, i.e., sum to 0")
        if(length(coef) != length(contrast.obj))
          stop("'coef' must have same length as 'contrast.obj'")
      }
      contrast <-
        sapply(contrast.obj, function(x)
        {
          if(!is.logical(x))
            stop(gettextf("each element of '%s' must be logical",
                          substitute(contrasts.list)),
                 domain = NA)
          x/sum(x)
        })
      if(!length(contrast) || all(is.na(contrast)))
        stop("the contrast defined is empty (has no TRUE elements)")
      contrast <- contrast %*% coef
    } else {
      contrast <- contrast.obj
      if(any(abs(colSums(contrast)) > 1e-8))
        stop("columns of 'contrast.obj' must define a contrast (sum to zero)")
      if(!length(colnames(contrast)))
        colnames(contrast) <- paste("Contrast", seq(ncol(contrast)))
    }
    weights <- contrast.weight.aov(object, contrast)
    summary(object)$stddev *
      if(!is.matrix(contrast.obj)) sqrt(sum(weights)) else sqrt(colSums(weights))
  }

predict.rlm <- function (object, newdata = NULL, scale = NULL, ...)
{
  ## problems with using predict.lm are the scale and
  ## the QR decomp which has been done on down-weighted values.
  object$qr <- qr(sqrt(object$weights) * object$x)
  NextMethod(object, scale = object$s, ...)
}

vcov.rlm <- function (object, ...)
{
  so <- summary(object, corr = FALSE)
  so$stddev^2 * so$cov.unscaled
}
