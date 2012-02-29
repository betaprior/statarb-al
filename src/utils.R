par.save <- function() assign("oldpar", par(no.readonly=TRUE), env=.GlobalEnv) 
par.restore <- function() par(get("oldpar", env=.GlobalEnv)) 

## some aliases
h <- utils::head
first<- function(x) { head(x, n = 1) }
last <- function(x) { tail(x, n = 1) }
zero.length <- function(x) length(x) == 0
has.zero.length <- function(x) length(x) == 0

## functions
reverse.rows <- function(d){ d[nrow(d):1,,drop=F] }
"%w/o%" <- function(x,y) x[!x %in% y] #--  x without y (defined in help for %in%)
"%approx==%" <- function(x,y) abs(x-y)/(0.5*(abs(x)+abs(y))) < 1e-9
mean.var <- function(x,...) c("mean"=mean(x,...),"var"=var(x,...))

## useful for ggplot2:
vplayout<-function(x,y)viewport(layout.pos.row=x,layout.pos.col=y)

getCmdArgs <- function(flag_){ ##when modifying, make sure it doesn't conflict/isn't clobbered by RProfile
   argPosn = match(flag_, commandArgs()) + 1
  # print (paste(flag_ ,' = ',commandArgs()[c( argPosn )]))
   commandArgs()[c( argPosn )]
}

## Command argument/defaults list must be formatted like so:
## cmdargs.default <-
##   list(`-saveSigFile`=list(
##          val=FALSE,
##          class="logical"))
setFromCmdargs <- function(cmdargs.default, vstr) {
  if (! vstr %in% names(cmdargs.default))
    stop(paste(vstr, "not found in the supplied cmdargs list"))
  args.cmdstring <- vstr
  arg.class <- cmdargs.default[[vstr]]$class
  args.default <- cmdargs.default[[vstr]]$val
  arg.val <- as(getCmdArgs(args.cmdstring), arg.class)
  
  if (is.na(arg.val)) 
    arg.var <- if (!is.null(args.default)) args.default
    else
      stop("Can't find default arguments")
  
  arg.val
}


sorted.vector.indices <- function(foo){ sort.data.frame(data.frame(idx=1:length(foo),val=foo),by= ~val)$idx }

sort.data.frame <- function(x, by){
      # Author: Kevin Wright
      # with some ideas from Andy Liaw
      # http://tolstoy.newcastle.edu.au/R/help/04/07/1076.html

      # x: A data.frame
      # by: A one-sided formula using + for ascending and - for descending
      #     Sorting is left to right in the formula

      # Useage is:
      # library(nlme);
      # data(Oats)
      # sort(Oats, by= ~nitro-Variety)

  if(by[[1]] != "~")
    stop("Argument 'by' must be a one-sided formula.")

                                        # Make the formula into character and remove spaces
  formc <- as.character(by[2])
  formc <- gsub(" ", "", formc)
                                        # If the first character is not + or -, add +
  if(!is.element(substring(formc, 1, 1), c("+", "-")))
    formc <- paste("+", formc, sep = "")

                                        # Extract the variables from the formula
  vars <- unlist(strsplit(formc, "[\\+\\-]"))
  vars <- vars[vars != ""] # Remove any extra "" terms
  ## Build a list of arguments to pass to "order" function
  calllist <- list()
  pos <- 1 # Position of + or -
  for(i in 1:length(vars)){
    varsign <- substring(formc, pos, pos)
    pos <- pos + 1 + nchar(vars[i])
    if(is.factor(x[, vars[i]])){
      if(varsign == "-") {
        calllist[[i]] <- -rank(x[, vars[i]])
      } else {
        calllist[[i]] <- rank(x[, vars[i]])
      }
    } else {
      if(varsign == "-") {
        calllist[[i]] <- -x[, vars[i]]
      } else {
        calllist[[i]] <- x[,vars[i]]
      }
    }
  }
  return(x[do.call("order", calllist), ])
}


# ------ Compact representation of logical vectors:

logical2int <- function(x) { sum(x * 2^(rev(seq(along=x)) - 1)) }
apl.encode <- function(number, base) {
  # simple version of APL-encode / APL-representation "T", pw 10/02
  # "encode" converts the numbers "number" using the radix vector "base"
  n.base <- length(base); result <- matrix(0, length(base), length(number))
  for(i in n.base:1){
    result[i,] <- if(base[i]>0) number %% base[i] else number
    number     <- ifelse(rep(base[i]>0,length(number)),
                         floor(number/base[i]), 0)
  }
  return( if(length(number)==1) result[,1] else result )
}
int2logical <- function(x,num.bits) { as.logical(apl.encode(x,rep(2,num.bits))) }

# ------

testObject <- function(object){ exists(as.character(substitute(object))) }

prealloc.mtx <- function(rows,cols,colnames=NULL,rownames=NULL){
  x <- array(0.0,c(rows,cols))
  if(!is.null(colnames)){ colnames(x) <- colnames }
  if(!is.null(rownames)){ rownames(x) <- rownames }
  return(x)
}
