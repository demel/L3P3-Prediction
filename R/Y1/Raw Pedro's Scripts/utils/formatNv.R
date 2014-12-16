#Methods for clear display of numeric vectors
#Useful for linear models, glms, and others

#Print numeric vector
printNv<-function(v,...)
{print.default(formatNv(v),...,quote=FALSE)}

#Print numeric matrix
printNvMatrix<-function(m,...)
{print.default(formatNvMatrix(m),...,quote=FALSE)}

#format numeric vector
formatNv<-function(v,...)
{aaply(v, 1,format,...)}

#format numeric matrix
formatNvMatrix<-function(m,...)
{aaply(m,1,formatNv)}

#format numeric dataframe
formatNvDf<-function(d,...)
{
  df<-colwise(formatNv)(d)
  rownames(df)<-rownames(d)
  df
}



# Use instead of print.glm
printGlm<-function (x, digits = max(3L, getOption("digits") - 3L), ...) 
{
  cat("\nCall:  ", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  if (length(coef(x))) {
    cat("Coefficients")
    if (is.character(co <- x$contrasts)) 
      cat("  [contrasts: ", apply(cbind(names(co), co), 
                                  1L, paste, collapse = "="), "]")
    cat(":\n")
    #print.default(format(x$coefficients, digits = digits), 
    #print.gap = 2, quote = FALSE)
    print.default(format(aaply(x$coefficients, 1,format, digits = digits),justify="right"), 
                  print.gap = 2, quote = FALSE)
  }
  else cat("No coefficients\n\n")
  cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ", 
      x$df.residual, "Residual\n")
  if (nzchar(mess <- naprint(x$na.action))) 
    cat("  (", mess, ")\n", sep = "")
  cat("Null Deviance:\t   ", format(signif(x$null.deviance, 
                                           digits)), "\nResidual Deviance:", format(signif(x$deviance, 
                                                                                           digits)), "\tAIC:", format(signif(x$aic, digits)))
  cat("\n")
  invisible(x)
}


#Use instead of print.glm.summary


printGlmSum<-function (y, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
                       signif.stars = getOption("show.signif.stars"), ...) 
{
  if("glm" %in% class(y)) {x<-summary(y)}else{x<-y}
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Deviance Residuals: \n")
  if (x$df.residual > 5) {
    x$deviance.resid <- setNames(quantile(x$deviance.resid, 
                                          na.rm = TRUE), c("Min", "1Q", "Median", "3Q", "Max"))
  }
  xx <- zapsmall(x$deviance.resid, digits + 1L)
  print.default(xx, digits = digits, na.print = "", print.gap = 2L)
  if (length(x$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  }
  else {
    df <- if ("df" %in% names(x)) 
      x[["df"]]
    else NULL
    if (!is.null(df) && (nsingular <- df[3L] - df[1L])) 
      cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", 
          sep = "")
    else cat("\nCoefficients:\n")
    coefs <- x$coefficients
    if (!is.null(aliased <- x$aliased) && any(aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4L, dimnames = list(cn, 
                                                               colnames(coefs)))
      coefs[!aliased, ] <- x$coefficients
    }
    printCoefmatNV(coefs, digits = digits, signif.stars = signif.stars, 
                   na.print = "NA", ...)
  }
  cat("\n(Dispersion parameter for ", x$family$family, " family taken to be ", 
      format(x$dispersion), ")\n\n", apply(cbind(paste(format(c("Null", 
                                                                "Residual"), justify = "right"), "deviance:"), format(unlist(x[c("null.deviance", 
                                                                                                                                 "deviance")]), digits = max(5L, digits + 1L)), " on", 
                                                 format(unlist(x[c("df.null", "df.residual")])), " degrees of freedom\n"), 
                                           1L, paste, collapse = " "), sep = "")
  if (nzchar(mess <- naprint(x$na.action))) 
    cat("  (", mess, ")\n", sep = "")
  cat("AIC: ", format(x$aic, digits = max(4L, digits + 1L)), 
      "\n\n", "Number of Fisher Scoring iterations: ", x$iter, 
      "\n", sep = "")
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) {
        print(symnum(correl, abbr.colnames = NULL))
      }
      else {
        correl <- format(round(correl, 2L), nsmall = 2L, 
                         digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n")
  invisible(x)
}

# Replaces auxiliary method for print.summary.glm

printCoefmatNV <-function (x, digits = max(3L, getOption("digits") - 2L), signif.stars = getOption("show.signif.stars"), 
                           signif.legend = signif.stars, dig.tst = max(1L, min(5L, digits - 
                                                                                 1L)), cs.ind = 1:k, tst.ind = k + 1, zap.ind = integer(), 
                           P.values = NULL, has.Pvalue = nc >= 4 && substr(colnames(x)[nc], 
                                                                           1, 3) == "Pr(", eps.Pvalue = .Machine$double.eps, na.print = "NA", 
                           ...) 
{
  if (is.null(d <- dim(x)) || length(d) != 2L) 
    stop("'x' must be coefficient matrix/data frame")
  nc <- d[2L]
  if (is.null(P.values)) {
    scp <- getOption("show.coef.Pvalues")
    if (!is.logical(scp) || is.na(scp)) {
      warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
      scp <- TRUE
    }
    P.values <- has.Pvalue && scp
  }
  else if (P.values && !has.Pvalue) 
    stop("'P.values' is TRUE, but 'has.Pvalue' is not")
  if (has.Pvalue && !P.values) {
    d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
    nc <- nc - 1
    has.Pvalue <- FALSE
  }
  else xm <- data.matrix(x)
  k <- nc - has.Pvalue - (if (missing(tst.ind)) 
    1
    else length(tst.ind))
  if (!missing(cs.ind) && length(cs.ind) > k) 
    stop("wrong k / cs.ind")
  Cf <- array("", dim = d, dimnames = dimnames(xm))
  ok <- !(ina <- is.na(xm))
  for (i in zap.ind) xm[, i] <- zapsmall(xm[, i], digits)
  if (length(cs.ind)) {
    acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
    if (any(ia <- is.finite(acs))) {
      digmin <- 1 + if (length(acs <- acs[ia & acs != 0])) 
        floor(log10(range(acs[acs != 0], finite = TRUE)))
      else 0
      Cf[, cs.ind] <- formatNv(round(coef.se, max(1L, digits - 
                                                    digmin)), digits = digits)
    }
  }
  if (length(tst.ind)) 
    Cf[, tst.ind] <- formatNv(round(xm[, tst.ind], digits = dig.tst), 
                              digits = digits)
  if (any(r.ind <- !((1L:nc) %in% c(cs.ind, tst.ind, if (has.Pvalue) nc)))) 
    for (i in which(r.ind)) Cf[, i] <- formatNv(xm[, i], digits = digits)
  ok[, tst.ind] <- FALSE
  okP <- if (has.Pvalue) 
    ok[, -nc]
  else ok
  x1 <- Cf[okP]
  dec <- getOption("OutDec")
  if (dec != ".") 
    x1 <- chartr(dec, ".", x1)
  x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
  if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
    Cf[okP][not.both.0] <- formatNv(xm[okP][not.both.0], digits = max(1L, 
                                                                      digits - 1L))
  }
  if (any(ina)) 
    Cf[ina] <- na.print
  if (P.values) {
    if (!is.logical(signif.stars) || is.na(signif.stars)) {
      warning("option \"show.signif.stars\" is invalid: assuming TRUE")
      signif.stars <- TRUE
    }
    if (any(okP <- ok[, nc])) {
      pv <- as.vector(xm[, nc])
      Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst, 
                                 eps = eps.Pvalue)
      signif.stars <- signif.stars && any(pv[okP] < 0.1)
      if (signif.stars) {
        Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                         symbols = c("***", "**", "*", ".", " "))
        Cf <- cbind(Cf, format(Signif))
      }
    }
    else signif.stars <- FALSE
  }
  else signif.stars <- FALSE
  print.default(Cf, quote = FALSE, right = TRUE, na.print = na.print, 
                ...)
  if (signif.stars && signif.legend) 
    cat("---\nSignif. codes:  ", attr(Signif, "legend"), 
        "\n", sep = "")
  invisible(x)
}

#use instead of print.summary.lm()



#TMP

printAovSum<-function (object, intercept = FALSE, split, expand.split = TRUE, 
          keep.zero.df = TRUE, ...) 
{
  splitInteractions <- function(split, factors, names, asgn, 
                                df.names) {
    ns <- names(split)
    for (i in unique(asgn)) {
      if (i == 0 || names[i + 1L] %in% ns) 
        next
      f <- rownames(factors)[factors[, i] > 0]
      sp <- f %in% ns
      if (any(sp)) {
        if (sum(sp) > 1) {
          old <- split[f[sp]]
          nn <- setNames(nm = f[sp])
          marg <- lapply(nn, function(x) df.names[asgn == 
                                                    (match(x, names) - 1L)])
          term.coefs <- strsplit(df.names[asgn == i], 
                                 ":", fixed = TRUE)
          ttc <- sapply(term.coefs, function(x) x[sp])
          rownames(ttc) <- nn
          splitnames <- setNames(nm = apply(expand.grid(lapply(old, 
                                                               names)), 1L, function(x) paste(x, collapse = ".")))
          tmp <- sapply(nn, function(i) names(old[[i]])[match(ttc[i, 
                                                                  ], marg[[i]])])
          tmp <- apply(tmp, 1L, function(x) paste(x, 
                                                  collapse = "."))
          new <- lapply(splitnames, function(x) match(x, 
                                                      tmp))
          split[[names[i + 1L]]] <- new[sapply(new, function(x) length(x) > 
                                                 0L)]
        }
        else {
          old <- split[[f[sp]]]
          marg.coefs <- df.names[asgn == (match(f[sp], 
                                                names) - 1L)]
          term.coefs <- strsplit(df.names[asgn == i], 
                                 ":", fixed = TRUE)
          ttc <- sapply(term.coefs, function(x) x[sp])
          new <- lapply(old, function(x) seq_along(ttc)[ttc %in% 
                                                          marg.coefs[x]])
          split[[names[i + 1L]]] <- new
        }
      }
    }
    split
  }
  asgn <- object$assign[object$qr$pivot[1L:object$rank]]
  uasgn <- unique(asgn)
  nterms <- length(uasgn)
  effects <- object$effects
  if (!is.null(effects)) 
    effects <- as.matrix(effects)[seq_along(asgn), , drop = FALSE]
  rdf <- object$df.residual
  nmeffect <- c("(Intercept)", attr(object$terms, "term.labels"))
  coef <- as.matrix(object$coefficients)
  resid <- as.matrix(object$residuals)
  wt <- object$weights
  if (!is.null(wt)) 
    resid <- resid * wt^0.5
  nresp <- NCOL(resid)
  ans <- vector("list", nresp)
  if (nresp > 1) {
    names(ans) <- character(nresp)
    for (y in 1L:nresp) {
      cn <- colnames(resid)[y]
      if (is.null(cn) || cn == "") 
        cn <- y
      names(ans)[y] <- paste(" Response", cn)
    }
  }
  if (!is.null(effects) && !missing(split)) {
    ns <- names(split)
    if (!is.null(Terms <- object$terms)) {
      if (!is.list(split)) 
        stop("the 'split' argument must be a list")
      if (!all(ns %in% nmeffect)) {
        na <- sum(!ns %in% nmeffect)
        stop(sprintf(ngettext(na, "unknown name %s in the 'split' list", 
                              "unknown names %s in the 'split' list"), paste(sQuote(ns[na]), 
                                                                             collapse = ", ")), domain = NA)
      }
    }
    if (expand.split) {
      df.names <- names(coef(object))
      split <- splitInteractions(split, attr(Terms, "factors"), 
                                 nmeffect, asgn, df.names)
      ns <- names(split)
    }
  }
  for (y in 1L:nresp) {
    if (is.null(effects)) {
      nterms <- 0
      df <- ss <- ms <- numeric()
      nmrows <- character()
    }
    else {
      df <- ss <- numeric()
      nmrows <- character()
      for (i in seq(nterms)) {
        ai <- (asgn == uasgn[i])
        df <- c(df, sum(ai))
        ss <- c(ss, sum(effects[ai, y]^2))
        nmi <- nmeffect[1 + uasgn[i]]
        nmrows <- c(nmrows, nmi)
        if (!missing(split) && !is.na(int <- match(nmi, 
                                                   ns))) {
          df <- c(df, unlist(lapply(split[[int]], length)))
          if (is.null(nms <- names(split[[int]]))) 
            nms <- paste0("C", seq_along(split[[int]]))
          ss <- c(ss, unlist(lapply(split[[int]], function(i, 
                                                           e) sum(e[i]^2), effects[ai, y])))
          nmrows <- c(nmrows, paste0("  ", nmi, ": ", 
                                     nms))
        }
      }
    }
    if (rdf > 0) {
      df <- c(df, rdf)
      ss <- c(ss, sum(resid[, y]^2))
      nmrows <- c(nmrows, "Residuals")
    }
    nt <- length(df)
    ms <- ifelse(df > 0L, ss/df, NA)
    x <- list(Df = df, `Sum Sq` = ss, `Mean Sq` = ms)
    if (rdf > 0) {
      TT <- ms/ms[nt]
      TP <- pf(TT, df, rdf, lower.tail = FALSE)
      TT[nt] <- TP[nt] <- NA
      x$"F value" <- TT
      x$"Pr(>F)" <- TP
    }
    class(x) <- c("anova", "data.frame")
    attr(x, "row.names") <- format(nmrows)
    if (!keep.zero.df) 
      x <- x[df > 0, ]
    pm <- pmatch("(Intercept)", row.names(x), 0L)
    if (!intercept && pm > 0) 
      x <- x[-pm, ]
    ans[[y]] <- x
  }
  class(ans) <- c("summary.aov", "listof")
  attr(ans, "na.action") <- object$na.action
  ans
}

