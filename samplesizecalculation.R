
# ____________________________________________________________________
# 
#                     Sample Size Calculation
#
#_____________________________________________________________________

#   Description of parameter
#   
#             p = probability/prevelance
#             z = z value at alpha 
#             d = margin of error
#           fpc = finite population correction
#             N = population size
#          deff = design effect


samplesize <- function(p=0.5, z=1.96, d=0.05, fpc=FALSE, N=NULL, deff=NULL, nresp=NULL){
  
  n <- (p*(1-p)*(z^2))/(d^2)  
  
  if(isTRUE(fpc) & is.null(N)){
    stop("Finite population correction need population size, N \n")
  }
  if(isTRUE(fpc) & !is.null(N)) {
    n <- n/(1+(n-1)/N)
    }

  if(!is.null(deff)){
    n <- n*deff
  }
  
  if(!is.null(nresp)){
    n <- n+n*nresp
  }
   return(cat("The sample size is ", round(n,0), "\n at, ",
              "p=",p,",  z=",z,",  d=",d, "\n Design effect=", deff,
              ", non response=",nresp, ", with finite population correction is",fpc)) 
          
   
  }


#samplesize(fpc=T, N=4971)

#samplesize(fpc=T, N=4971, deff = 1.3)

#samplesize(fpc=T, N=4971, deff = 1.3, nresp = 0.05)

