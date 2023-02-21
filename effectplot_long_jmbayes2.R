effectplot_JMBayes2 = function(JMbayes2Object, newdata, org.data){
  families <- JMbayes2Object$model_info$families
  fams <- sapply(families, "[[", "family")
  links <- sapply(families, "[[", "link")
  n_outcomes <- length(families)
  betas <- JMbayes2Object$mcmc[grep("betas", names(JMbayes2Object$mcmc))]  
  Terms <- terms(JMbayes2Object)
  
  form <- lapply(Terms, formula)
  Terms <- lapply(form, FUN = function (x) delete.response(terms(x)))
  mfX <- lapply(Terms, FUN = function (x) model.frame(x, data = org.data))
  
  Terms_new <- lapply(mfX, FUN = function (x) attr(x, "terms"))
  xlev <- mapply(FUN = function (x, y) .getXlevels(x, y), Terms, mfX, SIMPLIFY = FALSE)
  mfX_new <- mapply(FUN = function (x, y) model.frame(x, data = newdata, 
                                                      xlev = y), Terms_new, xlev, 
                    SIMPLIFY = FALSE)
  X <- mapply(FUN = function (x, y) model.matrix(x, y), Terms_new, mfX_new, 
              SIMPLIFY = FALSE)
  pred <- mapply(FUN = function (x, y) x %*% t(y), betas$betas1, X, SIMPLIFY = FALSE)
  preds <- lapply(pred, FUN = function (x) apply(x, 2, mean))
  lows <- lapply(pred, FUN = function (x) apply(x, 2, quantile, probs = 0.025))
  upps <- lapply(pred, FUN = function (x) apply(x, 2, quantile, probs = 0.975))
  i <- 1
  for (i in 1:n_outcomes) {
    names(preds)[[i]] <- paste0("pred_", i)
    names(lows)[[i]] <- paste0("low_", i)
    names(upps)[[i]] <- paste0("upp_", i)
    newcolnams <- c(names(preds)[[i]], names(lows)[[i]], names(upps)[[i]])
    newdata <- cbind(newdata, preds[[i]], lows[[i]], upps[[i]])
    colnames(newdata)[(ncol(newdata) - 2):ncol(newdata)] <- newcolnams
  }
  newdata
}
