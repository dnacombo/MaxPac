
#' Compute minimum Reaction Time
#' 
#' Compute minimum RT above which subjects are above chance in a 2AFC task, given RT and correctness vectors
#' 
#' @param RT Reaction times vector (in ms or adapt bins)
#' @param correct Correctness vector (coerced to logical)
#' @return bins Bin limits for computation
#' @param nBinSucc Number of successive significantly above chance bins
#' @param method chi square. (unused at present)
#' @param thresh threshold for significance of the chi square test.
#' @return minRT the minimum Reaction Time
#' 


computeMinRT = function(RT,correct, bins = seq(min(RT,na.rm = T),max(RT,na.rm = T) + 10,by=10),
nBinSucc = 5, method = 'chi2',thresh = .05)
  {
  
nH = hist(RT[as.logical(correct)],breaks = bins,plot = F)$counts
nM = hist(RT[!as.logical(correct)],breaks = bins,plot = F)$counts
nTot = nH + nM
nTri = sum(c(nH,nM))

minRT = NaN
switch (method,chi2={
          chideux = ((nH - nTot)^2 + (nM - nTot)^2)/ nTot
          p = diff(cumsum(pchisq(chideux,1) > thresh & nH > nM))
          count = 0
          for (i in 1:length(p)) {
            # cat(i)
            if (p[i] == 1) {count = count+1} else {count = 0}
            if (count == nBinSucc){
              minRT = bins[i - count + 1]
              break
            }
          }
        },
        bootstrap={
          stop('not implemented')
        })
  return(minRT)
}