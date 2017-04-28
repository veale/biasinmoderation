require(tidyverse)
require(boot)
dat <- read.csv("classified_tweets_mi_fe_ma_old_young.csv", stringsAsFactors = F)
dat <- as_tibble(dat)
dat$male_classes <- dat$male_classes %>% as.logical()
dat$female_classes <- dat$female_classes %>% as.logical()
dat$old_classes <- dat$old_classes %>% as.logical()
dat$young_classes <- dat$young_classes %>% as.logical()

table(dat$female_classes)

boo <- list()
mean.fun <- function(dat, idx) {mean(dat[idx], na.rm = TRUE)}
library(snow)
boo$male <- boot(dat$male_classes, 
                 R = 30000, 
                 statistic = mean.fun,
                 parallel = "snow")

boo$male.ci <- boot.ci(boo$male, type = c("norm", "basic"), conf = 0.98)


boo$old <- boot(dat$old_classes, 
                 R = 25000, 
                 statistic = mean.fun,
                 parallel = "snow")

boo$old.ci <- boot.ci(boo$old)

boo$young <- boot(dat$young_classes, 
                R = 25000, 
                statistic = mean.fun,
                parallel = "snow")

boo$young.ci <- boot.ci(boo$young)

boo$female <- boot(dat$female_classes, 
                  R = 25000, 
                  statistic = mean.fun,
                  parallel = "snow")

boo$female.ci <- boot.ci(boo$female)


require(irr)
require(boot)
require(tidyverse)
tox <- read_tsv("/Users/mv/Dropbox (Personal)/Make/Program/CSCW Automated Moderation/4563973/toxicity_annotated_comments.tsv")
tox.dem <- read_tsv("/Users/mv/Dropbox (Personal)/Make/Program/CSCW Automated Moderation/4563973/toxicity_worker_demographics.tsv")
tox.ann <- read_tsv("/Users/mv/Dropbox (Personal)/Make/Program/CSCW Automated Moderation/4563973/toxicity_annotations.tsv")

tox.ann %>% left_join(tox.dem) -> tox.long
tox.long %>% filter(!is.na(gender)) %>% group_by(rev_id, gender) %>% count() 

tox.long %>% group_by(gender, rev_id) %>% count() -> tox.gender.rev

table(tox.gender.rev$n, tox.gender.rev$gender)

ggplot(tox.gender.rev, aes(y = n, group= gender, fill = gender)) + geom_bar(position ="dodge", stat = "identity")

tox.long %>% filter(worker_id == 3603)

tox.long %>% filter(!is.na(gender)) %>% 
  group_by(rev_id, gender) %>% 
  filter(n() > 1) %>%
  mutate(mean.tox = round(mean(toxicity)), 
         dissenter = ifelse(toxicity != mean.tox, 1, 0)) %>%
  summarise(dissent = sum(dissenter)/n()) -> tox.dissent

tox.dissent %>% filter(gender == "female")  -> tox.dissent.f
tox.dissent %>% filter(gender == "male")  -> tox.dissent.m
library(parallel)
cl <- makeCluster(4)
?parallel

mean.fun <- function(dat, idx) {mean(dat[idx], na.rm = TRUE)}
library(snow)
boo <- list()
boo$fem <- boot(tox.dissent.f$dissent, 
                 R = 1000, 
                 statistic = mean.fun,
                 parallel = "multicore")
boo$fem.ci <- boot.ci(boo$fem, type = c("norm", "basic"), conf = 0.98)
boo$fem.ci

boo$mal <- boot(tox.dissent.m$dissent, 
                R = 1000, 
                statistic = mean.fun,
                parallel = "multicore")
boo$mal.ci <- boot.ci(boo$mal, type = c("norm", "basic"), conf = 0.98)
boo$mal.ci

  
  summarise(mean.dissent = mean(dissent))

# more likely to flag somethign that is offensive as not offensive
# more likely to 'let things through' than to miss things out  
2627/25506
570/3163


tox.long %>% 
  filter(gender == "female") %>% 
  select(rev_id, worker_id, toxicity) %>%
  spread(rev_id, value = toxicity) -> mat

mat <- as.matrix(mat)



matrix(c(1, 2, 3, 1, 2, 3, 3, 2, 3, 2, 2, 3, 3, 2, 1, 3, 1, 3), nrow = 6) -> mat
kripp.alpha(mat[1:100, 1:100]) 
kra(t(mat[1:100, 1:100]), metric = "nominal")


-> kripster
kripster

library(rel)
krip.fem <- kra(mat[1:100, 1:100], metric = "nominal")



library(parallel)
cl <- makeCluster(4)


"kra2" <- 
  function(data = NULL, metric = c("nominal","ordinal","interval","ratio"), 
           conf.level = 0.95, R = 0) {
    
    cl <- match.call()
    data <- as.matrix(data)
    nr <- nrow(data)
    nc <- ncol(data)
    data <- matrix(as.numeric(as.factor(data)),nr,nc)
    na <- sum(is.na(data))/(nr*nc)*100
    
    A <- function(data,metric){
      
      #Contingency table
      mval <- max(data,na.rm=TRUE)
      mu <- rowSums(!is.na(data))
      ap <- expand.grid(seq_len(nc),seq_len(nc))
      ap <- ap[ap[,1]!=ap[,2],]
      
      corr <- function(x){
        tab <- quote(na.omit(table(data[x,ap[,1]],data[x,ap[,2]])/(mu[x]-1)))
        mat <- matrix(0,mval,mval) 
        mat[as.numeric(rownames(eval(tab))),as.numeric(colnames(eval(tab)))] <- eval(tab) 
        return(mat)
      }
      mat <- Reduce("+",lapply(X=1:nr,FUN=corr))
      marg <- rowSums(mat)
      
      #Metrics
      suppressWarnings(
        if (metric == "ratio"){  
          metric <- ( (row(mat)-col(mat))/(row(mat)+col(mat)) )^2
          method = paste("Krippendorf's alpha for ratio data")
        } else if (metric == "interval"){
          metric <- (row(mat)-col(mat))^2
          method = paste("Krippendorf's alpha for interval data")
        } else if (metric == "ordinal"){
          ends <- outer(marg,marg,"+")/2
          ord <- function(x){
            res <- matrix(0,mval,mval)
            if (x<=mval-1){
              res[x-1,(x+1):(mval)] <- t(matrix(cumsum(marg[(x):(mval-1)])))
            }  else{
              res <- ends
            }
            return(res)
          }
          cs <- Reduce("+",lapply(X=2:(mval-1),FUN=ord))
          metric <- (ifelse(col(cs)>row(cs)+1,cs+ends,ends))^2
          metric[lower.tri(metric,diag=TRUE)] <- 0
          metric <- metric+t(metric)
          method = paste("Krippendorf's alpha for ordinal data")
        } else{
          metric <- abs(diag(ncol(mat))-1)
          method = paste("Krippendorf's alpha for nominal data")
        })
      
      #Point estimate
      if (dim(mat)[1]==1 && dim(mat)[2]==1){
        vs <- matrix(c(1,1),2,1)
      } else {
        vs <- combn(1:mval,2) 
      }
      ka <- 1-(sum(marg)-1)*sum(mat[upper.tri(mat, diag = FALSE)]*metric[upper.tri(metric,diag=FALSE)])/
        sum(marg[vs[1,]]*marg[vs[2,]]*t(metric[lower.tri(metric,diag=FALSE)]))
      return(list(ka,method))
    }
    out <- A(data=data,metric=metric)
    
    #Bootstrapped confidence intervals
    if (R == 0 || is.nan(out[[1]])){
      res.boot <- c(NA,NA)
    } else{
      a <- unlist(lapply(X=1:R,function(x) A(data[sample(nr, replace=TRUE),], metric=metric)[[1]]))
      res.boot <- quantile(x=a, probs=c((1-conf.level)/2,conf.level+(1-conf.level)/2), na.rm=TRUE) 
    }
    out[is.nan(out[[1]])] <- 0
    attr(res.boot,"names") <- "Const"
    
    res <- structure(list(method = out[[2]],
                          call = cl,
                          obs = nc,
                          sample = nr,
                          na = na,
                          est = out[[1]],
                          conf.level = conf.level,
                          lb = res.boot[1],
                          ub = res.boot[2],
                          data = data),
                     class = c("rel","kra"))
    return(res)
  }


