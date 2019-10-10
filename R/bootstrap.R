
#' bootstrap
#' 
#' Apply \code{fun} to the data \code{n} times.  The data is randomly sampled
#' each time. This yields both more robust results and approximate confidence
#' intervals for the results.  Due to the random sampling, the results of this
#' function are stochastic. Set the seed with \code{set.seed(n)} for
#' reproducible results. 
#'
#' @param pred A vector of predictions
#' @param actual A vector of actual values 
#' @param fun The function to apply
#' @param draws Number of times to randomly sample the data for bootstrapping.
#' Larger numbers yield better results, but also increase runtime. 
#' @param parallel Bool. Run in parallel?
#' @param ... Arguments passed to fun
#' @return A list containing the results of each iteration
#' @examples
#' pred <- sample(seq(0,1,0.001), size = 150, replace = TRUE)
#' actual <- sample(c(0,1), size = 150, replace = TRUE)
#' bootstrap(pred,actual,roc)
#'
#' @export
bootstrap <- function(pred, actual, fun, draws = 100, parallel = FALSE, ...){
   print(rlang::dots_list(...))

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
         fun(data$pred,data$actual, ...)
      }
   )

   if(parallel){base[[1]] <- parallel::mclapply; base[["mc.cores"]] <- parallel::detectCores() - 1}

   eval(base) 
}

#' bootstrappedMetricCurve
#' 
#' Makes \code{metricCurve} data using bootstrapping, which makes it  
#' possible to calculate the standard deviation, and the quantiles of 
#' the results.
#'
#' Note that the function requires specification of the resolution of the
#' \code{metricCurve}. This is because each bootstrap iteration must have the
#' same dimensions, since it must be possible to collapse these into an array.
#'
#' @param pred Probability predictions
#' @param actual Outcomes
#' @param res Resolution of ROC thresholds. A float between 0 and 1. 
#' @param draws Number of times to randomly sample the data for bootstrapping.
#' Larger numbers yield better results, but also increase runtime. 
#' @param parallel Boolean. Run in parallel? Uses ncores - 1 CPU cores.
#' @param probs Vector of quantiles to return
#' @return A list containing ROC data and AUC with confidence intervals.
#' @examples
#' tdat <- examplePredictions()
#' bootstrappedMetricCurve(tdat$pred,tdat$actual,roc)
#'
#' @export
bootstrappedMetricCurve <- function(pred, actual, x, y,
                                    res = 0.1, draws = 100, parallel = FALSE){
   xFnName <- as.character(substitute(x))
   yFnName <- as.character(substitute(y))

   probs = c(0.25,0.975)

   curves <- bootstrap(pred,actual,fun = metricCurve,
                       res = seq(1,0+res,-res), draws = draws, parallel = parallel,
                       x = x, y = y)
   print(curves[[1]])

   # make this more flexible..
   aucs <- lapply(curves, function(curve){auc(curve[[xFnName]],curve[[yFnName]])}) 
   aucResult <- list(score = mean(unlist(aucs)),
                     sd = sd(unlist(aucs)),
                     quantiles = quantile(unlist(aucs), probs))
   print(aucs[[1]])

   # ROC stuff

   x_i <- which(names(curves[[1]]) == "x")
   y_i <- which(names(curves[[1]]) == "y")
   th_i <- which(names(curves[[1]]) == "th")


   matrices <- lapply(curves, as.matrix)
   print(matrices[[1]])
   cube <- array(do.call(c,matrices), dim = c(dim(matrices[[1]]),length(matrices))) 

   curveResult <- lapply(list(xFnName = x_i, yFnName = y_i), function(index){
      quantiles <- apply(cube[,index,],1,quantile, probs = probs)
      list(
         mean = apply(cube[,index,],1,mean),
         sd = apply(cube[,index,],1,sd),
         q025 = quantiles[1,],
         q975 = quantiles[2,] 
      )
   }) 
   curveResult2 <- do.call(cbind, curveResult)
      #as.data.frame() %>%
   curveResult3 <- cbind(curveResult2, cube[,th_i,1])

   #names(curveResult) <- c(sapply(c(xFnName, yFnName),function(funname){
   #   paste0(funname,"_",c("mean","sd","025","975"))
   #}), "th")

   list(res1 = curveResult, res2 = curveResult2,
        res3 = curveResult3, auc = aucResult) 
}

#' bootstrappedROC
#' 
#' Helper function that makes ROC graph data from predictions and outcomes with
#' confidence intervals and standard deviation. 
#'
#' Note that the function requires specification of the resolution, since 
#' it must be possible to collapse the results into an array.
#'
#' @param pred Probability predictions
#' @param actual Outcomes
#' @param res Resolution of ROC thresholds. A float between 0 and 1. 
#' @param draws Number of times to randomly sample the data for bootstrapping.
#' Larger numbers yield better results, but also increase runtime. 
#' @return A list containing ROC data and AUC with confidence intervals.
#' @examples
#' pred <- sample(seq(0,1,0.001), size = 150, replace = TRUE)
#' actual <- sample(c(0,1), size = 150, replace = TRUE)
#' bootstrappedROC(pred,actual,roc)
#'
#' @export
bootstrappedROC <- function(pred, actual, res, draws = 100, parallel = FALSE){

   rocs <- bootstrap(pred,actual,roc, res = seq(1,0+res,-res), 
                     draws = draws, parallel = parallel)

   fallout_i <- which(names(rocs[[1]]) == "fallout")
   recall_i <- which(names(rocs[[1]]) == "recall")

   aucs <- lapply(rocs, function(curve){auc(curve$fallout,curve$recall)}) 

   matrices <- lapply(rocs, as.matrix)

   cube <- array(do.call(c,matrices), dim = c(dim(matrices[[1]]),length(matrices))) 

   # make this more flexible..
   probs = c(0.25,0.975)

   aucResult <- list(score = mean(unlist(aucs)),
                     sd = sd(unlist(aucs)),
                     quantiles = quantile(unlist(aucs), probs))

   fallout_quantiles <- apply(cube[,fallout_i,],1,quantile, probs = probs)
   recall_quantiles <- apply(cube[,recall_i,],1,quantile, probs = probs)

   rocResult <- data.frame(
      fallout_mean = apply(cube[,fallout_i,],1,mean),
      fallout_025 = fallout_quantiles[1,],
      fallout_975 = fallout_quantiles[2,],
      fallout_sd = apply(cube[,fallout_i,],1,sd),

      recall_mean = apply(cube[,recall_i,],1,mean),
      recall_025 = recall_quantiles[1,],
      recall_975 = recall_quantiles[2,],
      recall_sd = apply(cube[,recall_i,],1,sd),

      th = cube[,3,1]
   )

   list(roc = rocResult,auc = aucResult, rocs = rocs) 
}
