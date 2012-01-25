dendRix <-
function(aberrations=NULL, K=2, minFreq=1, numIter=999999, keep=999,
                    analyzed=NULL, stepLength=999, c=0.5)
{ # {{{ would like to vary K adaptively (i.e. w/a DP or by visit number maybe?)

  if(is.null(aberrations)) { # {{{ print documentation (should be an .Rd)
    print(paste("Usage: dendRix(aberrations, K, minFreq, numIter,",
                "analyzedAberrations, stepLength, c, toKeep"))
    print("aberrations: list OR data.frame of aberrations for each sample")
    print("K: size of sets to sample (would prefer a Dirichlet process here)")
    print("minFreq: minimum frequency aberration to be considered (default: 1)")
    print("numIter: number of MCMC sampler iterations to run (default: 999999")
    print("keep: how many of the top-ranked sets to retain as the results")
    print("analyzed: list of aberrations to track (default: all above minFreq)")
    print("stepLength: number of iterations between two samples (default: 999)")
    print("c: a constant whose function is to ensure mixing of the chain (0.5)")
  } # }}}
  what = getAberrations(aberrations)
  bySubject = getHash(aberrations)
  byAberration = invert(bySubject) 
  allSamples = keys(bySubject)
  if(minFreq > 1) { # {{{
    freq = sapply(keys(byAberration), function(x) length(byAberration[[x]]))
    analyzed = intersect( names(freq)[ which(freq > minFreq) ], analyzed )
    # }}}
  } else if(!is.null(analyzed)) { # {{{
    analyzed = intersect( what, analyzed )
    # }}}
  } else { # {{{
    analyzed = what
  } # }}}
  sampleNumMuts = sapply( keys(bySubject), function(x) length(bySubject[[x]]) )
  print(paste("Number of aberrations:", length(analyzed)))
  print(paste("Number per subject:", summary(sampleNumMuts)))
  solution = sample(analyzed, K)        # K could be sampled from a DP or...?
  visits = c()                          # number of visits to a given set 
  for( iter in seq_along(1:numIter) ) { # {{{ MCMC sampler... move to C++ 
    next_gene = sample(analyzed, 1)
    to_exchange = ifelse(next_gene %in% solution, next_gene, sample(solution,1))
    next_solution = union(setdiff(solution, to_exchange), next_gene)
    expon = measure(next_solution, solution)
    if(runif(1) <= min(1, exp(expon))) solution = next_solution
    if((iter+1) %% step_length == 0) { 
      frozen = frozenSet(solution)
      visits[frozen] = ifelse(frozen %in% names(visits), visits[frozen] + 1, 1)
    }
  } # }}}
  tops = names(visits)[order(visits, decreasing=TRUE)[1:keep]]
  return(visits[tops])

}
