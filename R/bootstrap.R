
#' bootstrap
#' 
#' Apply \code{fun} to the data \code{n} times.  The data is randomly sampled
#' each time. This yields both more robust results and approximate confidence
#' intervals for the results.  Due to the random sampling, the results of this
#' function are stochastic. Set the seed with \code{set.seed(n)} for
#' reproducible results. 
#'
#' @param 
#' @return 
#' @examples
#' pred <- sample(seq(0,1,0.001), size = 150, replace = TRUE)
#' actual <- sample(c(0,1), size = 150, replace = TRUE)
#' bootstrap(pred,actual,roc)
#'
#' @export
bootstrap <- function(pred, actual, fun, draws = 100, parallel = FALSE, ...){
   if(length(pred) != length(actual)) stop("pred and actual must have the same length!")

   data <- data.frame(pred = pred,actual = actual)

   drawn <- rep(FALSE,nrow(data))

   base <- call("lapply",
      X = 1:draws,
      FUN  = function(sample){
         # Here is the sampling
         # Currently doing what pROC and ROCr are doing,
         # sampling the entire set _with replacement_.
         data <- data[sample(nrow(data),replace = TRUE),]
         fun(data$pred,data$actual)
      }
   )

   if(parallel){base[[1]] <- parallel::mclapply; base[["mc.cores"]] <- parallel::detectCores() - 1}

   res <- eval(base) 
   res[!is.null(res)]
}

#' bootstrappedROC
#' 
#' Helper function that makes ROC graph data from predictions and outcomes with
#' confidence intervals and standard deviation. 
#'
#' @param pred Probability predictions
#' @param actual Outcomes
#' @return A list containing ROC data and AUC with confidence intervals.
#' @examples
#' pred <- sample(seq(0,1,0.001), size = 150, replace = TRUE)
#' actual <- sample(c(0,1), size = 150, replace = TRUE)
#' bootstrappedROC(pred,actual,roc)
#'
#' @export
bootstrappedROC <- function(pred,actual, ...){
   rocs <- bootstrap(pred,actual,roc, ...)
   aucs <- lapply(rocs, function(curve){auc(curve$fallout,curve$recall)}) 

   matrices <- lapply(rocs, as.matrix)
   cube <- array(do.call(c,matrices), dim = c(dim(matrices[[1]]),length(matrices))) 

   # make this more flexible..
   probs = c(0.25,0.975)

   aucResult <- list(score = mean(unlist(aucs)),
                     sd = sd(unlist(aucs)),
                     quantiles = quantile(unlist(aucs), probs))


   fallout_quantiles <- apply(cube[,1,],1,quantile, probs = probs)
   recall_quantiles <- apply(cube[,2,],1,quantile, probs = probs)

   rocResult <- data.frame(
      fallout_mean = apply(cube[,1,],1,mean),
      fallout_025 = fallout_quantiles[1,],
      fallout_975 = fallout_quantiles[2,],
      fallout_sd = apply(cube[,1,],1,sd),

      recall_mean = apply(cube[,2,],1,mean),
      recall_025 = recall_quantiles[1,],
      recall_975 = recall_quantiles[2,],
      recall_sd = apply(cube[,1,],1,sd),

      th = cube[,3,1]
   )

   list(roc = rocResult,auc = aucResult)
}
