rm(list = ls())


#question 1 
#on paper

#part 2

MM <- 357
MN <- 485
NN <- 158

p<-(MM+(1/2 * MN)) / sum( MM + MN + NN)
q<-(NN+(1/2 * MN)) / sum( MM + MN + NN)

r = chisq.test(c(MM,MN, NN), p = c(p^2, 2*p*q,q^2))

r$p.value
#p_value is greater than 0.05 so population is in Hardy–Weinberg equilibrium.


# question 2

#part 1
protein_product = "RecQ type DNA helicase"

#part 2

amino_acid = c("Methinoine","Valine","Valine","Alanine")

#part 3
Speices = "Homo Sapiens"

#part 4
library(seqinr)
original_cdf_seq <- read.fasta("rabnawaz/original_necleotide_seq.fasta")
backtrack_seq <- read.fasta("rabnawaz/backtrack_seq.fasta")

original_cdf_seq$`CU329670.1:c5662-1`[10:12]
backtrack_seq$EMBOSS_001[10:12]


# ATG = ATG = M
# GTG = GTT = V
# GTG = GTT = V 
# GCC = GCT = A

#the sequence are different.




#part 2.5
inverse_compliment <- read.fasta("rabnawaz/reverse_compliment_seq.fasta")

inverse_compliment = inverse_compliment$EMBOSS_001

stop_co1 <-c("t","a","a")
stop_co2 <-c("t","a","g")
stop_co3 <-c("t","g","a")
i = 1; j = 1
result = c()
while (j < length(real)){

  genome <- inverse_compliment[j:(j+2)]
  if(all(genome == stop_co1) || all(genome == stop_co2) || all(genome == stop_co3) )
  {
    result[i] = j
    i = i + 1
  }
  
  j= j+3
}

length(result)



#question 3 

#part 1

#Caenorhabditis elegans is the first multicelar organism that had its whole genome sequenced
#and its has similar feature or charatertistic as human.It is such an impoartant organism , 
#as many developmental and gentical experiments are direcly not possible to implement on human and they
#are very time consuming and coslty. So we can use that organ to do significant experiement in lab by 
#Scientific community.








