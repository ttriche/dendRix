measure <- function(genesCollection1, genesCollection2, sampleMutatedGenes,c=.5)
{ # {{{ the lone standalone function from Original Dendrix; move to C++ 

	out1 = 0	  # coverage of genes in genes_collection1
	inside1 = 0	# total number of mutations in genes_collection1
	out2 = 0    # coverage of genes_collection2
	inside2 = 0	# total number of mutations in genes_collection2
  message('The sooner this next loop gets ported to C++, the better!')
	for(sampleID in sampleMutatedGenes) { # {{{ here C++ seems like a good idea
		genesInSample = sampleMutatedGenes[sampleID]
		inside_genes1 = intersect(genesCollection1, genesInSample)
    num_ig1 = length(inside_genes1)
		if( num_ig1 > 0 ) out1 = out1 + 1
		inside1 = inside1 + num_ig1
		inside_genes2 = intersect(genesCollection2, genesInSample)
    num_ig2 = length(inside_genes2)
		if( num_ig2 > 0 ) out2 = out2 + 1
		inside2 = inside2 + num_ig2
  } # }}}
	return(c * ((2*out1) - inside1 - (2*out2) - inside2))

}
