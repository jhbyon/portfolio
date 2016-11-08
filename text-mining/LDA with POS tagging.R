#load required packages

if (!require(tm)) install.packages("tm"); library(tm); #for pre-processing text prior to parts-of-speech tagging
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2);
if (!require(quanteda)) install.packages("quanteda"); library(quanteda); #main text mining tool
if (!require(topicmodels)) install.packages("topicmodels"); library(topicmodels); #for LDA
if (!require(servr)) install.packages("servr"); library(servr); #for LDAvis
if (!require(LDAvis)) install.packages("LDAvis"); library(LDAvis); #for LDAvis

#download freeling and save in C:/
#https://github.com/TALP-UPC/FreeLing/releases

#define corpus
text<-stringvector

####################
#   POS tagging   ##
####################

#extract terms
mycorpus<-tm::Corpus(tm::DataframeSource(as.data.frame(text)))

mycorpus<-tm::tm_map(mycorpus, removeNumbers)
mycorpus<-tm::tm_map(mycorpus, content_transformer(function(x, pattern) gsub(pattern, " ", x)), "[!¡\"/[#$%&'+,./)(*:;<=>¿?¨`@´\\^`{|}~-]")
dtm <- tm::DocumentTermMatrix(mycorpus, control=list(weighting=weightTf))

term <- dtm$dimnames$Terms

txt <- file(description="C:/freeling/terms.txt", open="w", encoding="UTF-8")
write(term, file=txt)
close(txt)

#Create .bat file, execute freeling from cmd, save results as terms_tagged.txt
cmd <- c("cd C:/freeling",
         "C:/freeling/bin/analyzer.bat -f C:/freeling/data/config/es.cfg <terms.txt> terms_tagged.txt")
write(cmd, file="C:/freeling/freeling.bat",sep = "\n")
shell("C:/freeling/freeling.bat", intern=TRUE)


#read terms tagged
term.tagged <- unlist( strsplit ( readLines("C:/freeling/terms_tagged.txt",encoding = "UTF-8"), " "))

term.tagged <- as.data.frame(matrix( term.tagged , ncol = 4 , byrow = TRUE )) 
names(term.tagged) <- c("term", "lema", "tag", "prob")

#extract noun or whatever parts of speech required
term.noun<-unique(term.tagged[which(term.tagged$tag %in% levels(term.tagged$tag)[grep("^[N].*", levels(term.tagged$tag))]),])



###############################################
#   create corpus and document term matrix   ##
###############################################

#create corpus
text.corpus<-quanteda::corpus(text)



#create document feature matrix using only nouns
text.corpus.dfm <-quanteda::dfm(text.corpus,
                                toLower=TRUE,
                                removeNumbers=TRUE,
                                removePunct = TRUE,
                                removeSeparators=TRUE,
                                ignoredFeatures = stopwords("spanish"),
                                language="spanish", 
                                stem=TRUE,
                                keptFeatures=term.noun) 

#remove terms higher than median 
terms.tfidf<-colSums(tfidf(text.corpus.dfm))
keep.terms<-names(terms.tfidf)[terms.tfidf > quantile(terms.tfidf, 0.5)]

text.corpus.dfm<-siniestros.sub.hogar.dfm[, keep.terms] 

#remove docs whose rowsums is zero
keep.rows<-rowSums(text.corpus.dfm) > 0
text.corpus.dfm<-text.corpus.dfm[keep.rows,]



###########################
#lda: VEM with fixed alpha#
###########################

#selecting the best k can be improved through cross validation, but in the meantime, we use the entire training set to find the best k
sequ<-seq(4,12,by=2)
vem_model <- lapply(sequ, function(k){topicmodels::LDA(convert(text.corpus.dfm, to = "topicmodels"), 
                                                       control = list(seed=20161011, alpha=0.1, verbose=100), k)})


#extract alphas and plot
alphas<-sapply(vem_model, slot, "alpha")

ggplot(data.frame(topics=sequ, alphas=alphas), aes(x=topics, y=alphas))+ 
  geom_line()+
  theme_bw()  +
  scale_x_continuous(breaks=sequ)+
  ggtitle("VEM Fixed Alpha 0.10")




#extract loglike and plot
logLikelihood <- as.data.frame(as.matrix(lapply(vem_model, logLik)))
logLikelihood.df <- data.frame(topics=sequ, LL=as.numeric(as.matrix(logLikelihood)))

ggplot(logLikelihood.df, aes(x=topics, y=LL)) + 
  xlab("Number of topics") + ylab("Log likelihood of the model") + 
  geom_line() + 
  theme_bw()  +
  scale_x_continuous(breaks=1:30)+
  ggtitle("LogLik Vehiculos (VEM fixed alpha=0.10)")



#check entropy
entropy<-sapply(vem_model, function(x)
  + mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))

ggplot(data.frame(topics=sequ, entroy=entropy), aes(x=topics, y=entropy))+ 
  geom_line()+
  theme_bw()  +
  scale_x_continuous(breaks=sequ)+
  ggtitle("Entropy for VEM Fixed Alpha 0.10")



############################### fit LDA vem fixed with topics = 12 ####################################
#lets assume the best k = 12
vem_model12 <- LDA(convert(text.corpus.dfm, to = "topicmodels"), 
                   control = list(seed=20161011, alpha=0.1, verbose=100), 
                   k = 12)

# palabras principales por topic
(topwords<-as.data.frame(get_terms(vem_model12, 10)))

#assign topic 
fitted.topics <-topics(vem_model12)

#assess model quality by visualizing highest assignment probability
highest.assignment.prob<-apply(posterior(vem_model12)$topics, 1, max)

ggplot()+ 
  aes(highest.assignment.prob) +
  geom_histogram(bins=50)+
  scale_x_continuous(breaks=seq(0,1,.10))+
  theme_bw()  +
  ggtitle("VEM.Fix Highest Probability Topic Assignment K=12)")



#LDAvis


#compute required values
phi <- as.matrix(posterior(vem_model12)$terms)
theta <- as.matrix(posterior(LDAfit.hogar.vem.fix)$topics)
vocab <- colnames(phi)
doc_length <- as.numeric(rowSums(text.corpus.dfm))
term.frequency <-as.numeric(colSums(text.corpus.dfm))

# Convert to json
json_lda <- LDAvis::createJSON(phi = phi, 
                               theta = theta,
                               vocab = vocab,
                               doc.length = doc_length,
                               term.frequency = term.frequency)

#save output
save.to.path=""
serVis(json_lda, out.dir = save.to.path, open.browser = FALSE)


#check explained variance of first 2 PC as per LDAvis

# first, we compute a pairwise distance between topic distributions
# using a symmetric version of KL-divergence
# http://en.wikipedia.org/wiki/Jensen%E2%80%93Shannon_divergence
jensenShannon <- function(x, y) {
  m <- 0.5*(x + y)
  0.5*sum(x*log(x/m)) + 0.5*sum(y*log(y/m))
}

dist.mat <- proxy::dist(x = phi, method = jensenShannon)

# then, we reduce the K by K proximity matrix down to K by 2 using PCA
pca.fit <- stats::cmdscale(dist.mat, eig=TRUE)
cumsum(pca.fit$eig/sum(pca.fit$eig))[c(1,2)] 

