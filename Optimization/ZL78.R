DNA <- readChar('DNA.txt', file.info('DNA.txt')$size)

text <- 'Linear programming, surprisingly, is not directly related to computer programming.
The term was introduced in the 1950s when computers were few and mostly top secret, and the
word programming was a military term that, at that time, referred to plans or schedules for
training, logistical supply, or deployment of men. The word linear suggests that feasible plans are
reSicted by linear conSaints (inequalities), and also that the quality of the plan (e.g., costs or
duration) is also measured by a linear function of the considered quantities. In a similar spirit,
linear programming soon started to be used for planning all kinds of economic activities, such as
transport of raw materials and products among factories, sowing various crop plants, or cutting
paper rolls into shorter ones in sizes ordered by customers. The phrase ’planning with linear
conSaints’ would perhaps better capture this original meaning of linear programming. However,
the term linear programming has been well established for many years, and at the same time, it
has acquired a considerably broader meaning: Not only does it play a role only in mathematical
economy, it appears frequently in computer science and in many other fields.'

alpha <- 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'



ZL78 <- function(str) {
    #transform one string into multiple elements
    S <- unlist(strsplit(str, ''))
    S <- S[which(S!='\n')]

    #initialize the dictionnary, and the string element to be encoded
    D <- matrix(, 0, 2)
    T <- ''

    #iterative discovery of new sequences, filling a dictionnary
    for (i in 1:length(S)) {
        T <- paste(T, S[i], sep='')
        if (T %in% D[,1]) { 
            #add only the last sequence found
            if (i + length(T) - 1 == length(S)) 
                D <- rbind(D, c(T, which(D == substr(T, 1, nchar(T)))))
            else next
        } else { #store all sequences but the last
            #compute dictionnary row
            p <- ifelse(nchar(T) == 1, 0, 
                which(D == substr(T, 1, nchar(T)-1))) 
            l <- substr(T, nchar(T), nchar(T)) 
            #compute code
            D <- rbind(D, c(T, paste(p,',',l, sep='')))
            T <- c()
        }
    }

    #counting the number of bits needed by our code
    bits <- 7 + ceiling(nrow(D)/255)
    rate <- round((nrow(D)*bits) / (length(S)*8), 4)*100

    #based on the following output, 
    #the decoder will read the first part of each element, 
    #corresponding to the position of a previous element.
    #it will then read the ASCII-256 string after the comma 
    #(the first one, when there are multiple ones)
    list(code = D[,2], compression_ratio = paste(rate,'%'))
}



ZL78(DNA)
ZL78(text)
ZL78(alpha)