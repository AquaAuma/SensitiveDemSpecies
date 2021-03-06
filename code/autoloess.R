# autoloess.R: compute loess metaparameters automatically
# adapted from Kyle Gorman <gormanky@ohsu.edu>

aicc.loess <- function(fit) {
    # compute AIC_C for a LOESS fit, from:
    # 
    # Hurvich, C.M., Simonoff, J.S., and Tsai, C. L. 1998. Smoothing 
    # parameter selection in nonparametric regression using an improved 
    # Akaike Information Criterion. Journal of the Royal Statistical 
    # Society B 60
    # 
    # @param fit        loess fit
    # @return           'aicc' value
    stopifnot(inherits(fit, 'loess'))
    # parameters
    n <- fit$n
    trace <- fit$trace.hat
    sigma2 <- sum(resid(fit) ^ 2) / (n - 1)
    return(log(sigma2) + 1 + (2 * (trace + 1)) / (n - trace - 2))
}

autoloess <- function(fit, span=c(.1, .9)) {
        assign("last.warning", NULL, envir = baseenv())
        library(testthat)
        # compute loess fit which has span minimizes AIC_C
        # 
        # @param fit        loess fit; span parameter value doesn't matter
        # @param span       a two-value vector representing the minimum and 
        #                   maximum span values
        # @return           loess fit with span minimizing the AIC_C function
        stopifnot(inherits(fit, 'loess'), length(span) == 2)
        # loss function in form to be used by optimize
        f <- function(span) aicc.loess(update(fit, span=span))
        # find best loess according to loss function
            span.opt <- optimize(f, span)$minimum

        return(span.opt)

}
