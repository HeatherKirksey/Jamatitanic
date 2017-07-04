
## Age Imputation Functions


simpleAge <- function(df) {
        Wothers <- df$SibSp + df$Parch
        df$Age <- ifelse(is.na(df$Age), ifelse(Wothers == 0, 
                                               99,  # If alone and no age, assume OLD
                                               round(mean(df$Age, na.rm = TRUE))), 
                         df$Age)
        df
}


Tage <- function(df) {
        #Use the passenger title to perform mean substitution on missing values.

        allna <- which(is.na(df$Age))
        allmean <- mean(df$Age[-allna])
        
        allmaster <- grep("Master.", df$Name)
        mastermean <- round(mean(df$Age[allmaster], na.rm = TRUE))
        df$Age[allmaster] <- ifelse(is.na(df$Age[allmaster]), mastermean, df$Age[allmaster])
        
        allmiss <- grep("Miss.", df$Name) 
        missMean <- mean(df$Age[allmiss], na.rm = TRUE)
        df$Age[allmiss] <-ifelse(is.na(df$Age[allmiss]), missMean, df$Age[allmiss])
        
        allMr <- grep("Mr.", df$Name)
        MrMean <-mean(df$Age[allMr], na.rm = TRUE)
        df$Age[allMr] <-ifelse(is.na(df$Age[allMr]), MrMean, df$Age[allMr])
        
        allMrs <- grep("Mrs.", df$Name)
        MrsMean <-mean(df$Age[allMrs], na.rm = TRUE)
        df$Age[allMrs] <-ifelse(is.na(df$Age[allMrs]), MrsMean, df$Age[allMrs])
        
        # All the rest get the mean age
        df$Age <- ifelse(is.na(df$Age), allmean, df$Age)
        df
}


## mice() imputation


mice_imp <- function(df){
        library(mice)
        tempmiceout <- mice(df, m=7, maxit=50, meth="rf", seed = 21, print=FALSE)
        complete(tempmiceout, 2)
        # Several reviews show the resulting density plot closely resembles the na.rm verion
        # of the original data
}


addtitle <- function(df) {
        #Create a title factor feature in the DF before using mice_imp
        allmaster <- grep("Master.", df$Name)
        allmiss <- grep("Miss.", df$Name) 
        allMr <- grep("Mr.", df$Name)
        allMrs <- grep("Mrs.", df$Name)
        notOthers <- union(allmaster, union(allmiss, union(allMr, allMrs)))
        titles <- c("Master", "Miss", "Mr", "Mrs", "Other")
        Title <- list(rep(NA, nrow(df)))
        Title[allmaster] <- 1
        Title[allmiss] <- 2
        Title[allMr] <- 3
        Title[allMrs] <- 4
        Title[-notOthers] <- 5
        df$Title <- factor(Title, levels=c(1,2,3,4,5), labels=c("Master", "Miss", "Mr", "Mrs", "Other"))
        df
}
``` 