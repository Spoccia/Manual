library(arules)
library(arulesSequences)
#salva il dataframe con solo le colonne delle variabili di cui cercare le regole 
#in un file senza estensione
path<-""
ds<-data.frame(ID=1:4,
               a=c(1,3,1,1),
               b=c("alpha","alpha","beta","beta"),
               c=c("pippo","pluto","pluto","pippo"))
write.table(ds,paste0(path,"/data"),row.names = F,col.names = F)
#carica il file in formato transazione
t<-read.transactions(paste0(path,"/data"),format="basket",cols = 1)

###apriori
#generazione di itemset frequenti (minimo supporto)
freq_items<- apriori(t,parameter = list(supp = 0.1,target = "frequent itemsets"))
#generazione delle regole di associazione (minima confidenza)
rules<-ruleInduction(freq_items,t,confidence=0.1)
#salva regole in dataframe in un formato facilmente manipolabile
ap<-DATAFRAME(rules,separate= TRUE, setStart = '', setEnd = '')
#Dataframe con antecedente(LHS), conseguente (RHS), supporto,confidenza e lift 
#PRONTO!


#N.B.
#Nel dataframe c'è anche una colonna identificativa dell'itemset a cui si riferisce la regola.
#Probabilmente troverai coppie di regole che si riferiscono allo stesso itemset,
#ma con LHS e RHS invertiti. Quindi seleziona quelli che ti interessano tenendo in RHS 
#solo quelli che hanno senso fisico/logico per te e per il caso studio considerato.


