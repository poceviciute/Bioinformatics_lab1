###2.1
### Product = RecQ type DNA helicase


### 2.2

### Our sequence in translation is :
# MVVA

# 1. Methionine, Valine, Valine, Alanine

### 2.3

## Homo sapiens specie used to create sequence

### 2.4 

# on left our generated protein seq - In middle Actual nucleotide seq - On right amino acids
# ATG = ATG = M
# GTG = GTT = V
# GTG = GTT = V 
# GCC = GCT = A

## Since the different seq of nucleotides give the same amino acids thus our results are different 

# AGC = S
# GAG = E
# ATC = I
# GCC = A
# AAG = K
# GTG = V
# GCC = A
# AGC = S
# AAG = K
# ACC = T
# GCC = A
# AGG = R 

library(seqinr)
backtransition_seq <- read.fasta(file = "back_seq.FASTA")

j = 1

back_seq <- as.vector(backtransition_seq$EMBOSS_001)
length(back_seq)
protein <- matrix(nrow = round(length(back_seq)/3),ncol = 3)
i <- 1

stop_codon1 <- c("t","a","a")
stop_codon2 <- c("t","a","g")
stop_codon3 <- c("t","g","a")

stop_index <- c()

while(j< length(back_seq)) {

  
  protein[i,] <- back_seq[j:(j+2)]
  i <- i+1
  j = j+3
}

for( i in 1:nrow(protein))
{
  if(all(protein[i,] == stop_codon1) || all(protein[i,] == stop_codon2) || all(protein[i,] == stop_codon3))
    stop_index <- i
  else
    stop_index <- NULL
}


rm(list = ls())
reverse_seq <- read.fasta(file = "reverse_comp_seq.FASTA")

rev_seq <- as.vector(reverse_seq$EMBOSS_001)
length(rev_seq)
protein <- matrix(nrow = round(length(rev_seq)/3),ncol = 3)
i <- 1

stop_codon1 <- c("t","a","a")
stop_codon2 <- c("t","a","g")
stop_codon3 <- c("t","g","a")

stop_index <- c()

j <-1

while(j< length(rev_seq)) {
  
  
  protein[i,] <- rev_seq[j:(j+2)]
  i <- i+1
  j = j+3
}

result <- c()
j <- 1

for( i in 1:nrow(protein))
{
  if(all(protein[i,] == stop_codon1) || all(protein[i,] == stop_codon2) || all(protein[i,] == stop_codon3))
  {
    result[j] <- i  
    j <- j+1
  }

}
length(result)


##3.1

#Caenorhabditis elegans is the first multi-cellular organism that had its whole genome sequenced
#and its has similar feature or characteristic as human.It is such an important organism , 
#as many developmental and genetical experiments are directly not possible to implement on human and they
#are very time consuming and costly. So we can use that organism to do significant experiment in lab by 
#Scientific community

### 3.3,3.2

# query seq = 1 - 1500
# subject sequence = 6529 - 8028

#The direction of query sequence is opposite to database sequence.


#when reverse the query sequence

# query seq = 1 - 1500
# subject sequence = 8028 - 6529 

#The direction of query sequence is same to database sequence.




##3.4

#Chromosome 5. Gene: ife-3


#3.5
complete_seq <- read.fasta(file = "allseq_6936to7818.FASTA")

e1 <- complete_seq$`NC_003283.11:6936-7818`[1:174]
e2 <- complete_seq$`NC_003283.11:6936-7818`[(174+48):(174+48+235)]
e3 <- complete_seq$`NC_003283.11:6936-7818`[(457+40):(457+40+176)]
e4 <- complete_seq$`NC_003283.11:6936-7818`[(673+42):(673+42+168)]

merge_exon = c(e1,e2,e3,e4)

write.fasta(sequences = merge_exon, names = "exons_6936-7818",file.out = "exons.fasta")

# df <- data.frame(exons = merge_exon)
# #save(df, file = "exons.FASTA")
# 
# rm(list = ls())
# load("C:/Saman Zahid/Bioinformatics/Lab1/Bioinformatics_lab1/exons.Rdata")
# 
# data <- df
# write.table(df, "mydata.txt", sep="")
# 
# 

proteins_from_exons <- read.fasta(file = "exons_translation.FASTA")
print(proteins_from_exons)

proteins_from_6936to7818 <- read.fasta(file = "complete_translationq3.FASTA")
print(proteins_from_6936to7818)


### Some parts of protein translation obtained from exons matches to 
# the protein translation of the entire sequence. The protein sequence obtained from the complete sequence
# has a number of stop codes, the translation is quite vague as at some places the sequence stops as soon as it starts
# or start protein occurs again multiple times without before stop.


### 3.6

# eukaryotic translation initiation factor EIF4E family protein recognizes and binds the 7-methylguanosine-containing mRNA cap during an early step in the initiation of protein synthesis 
# It has 4 exons.


