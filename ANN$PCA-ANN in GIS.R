
# 1 Introduction ------------------------------------------------------------



##################################################.
## Project: Susceptibility Prediction Mapping USING ANN  In Package 'neuralnet'and PCA in native R function "prcomp"
## Script purpose:
## Date: 9 March 2019
## Author: Omar AlThuwaynee & Mungu Bertrand A .
##################################################.

# Disclaimer
# As with most of my R posts, I've arranged the functions using ideas from other people that are much more clever than me. I've simply converted these ideas into a useful form in R. Ultimate credit for the sensitivity analysis goes to: 
# add references

#    #######   lsm USING ANN  In Package 'neuralnet' ########.
#The neuralnet package is perhaps not the best option in R for using neural networks. If you ask why, for starters it does not recognize the typical formula y~., it does not support factors, it does not provide a lot of models other than a standard MLP, and it has great competitors in the nnet package that seems to be better integrated in R and can be used with the caret package, and in the MXnet package that is a high level deep learning library which provides a wide variety of neural networks.
#https://www.r-bloggers.com/multilabel-classification-with-neuralnet-package/
# http://www.michaeljgrogan.com/neural-network-modelling-neuralnet-r/
###########################.


# 2 Data prepration ---------------------------------------------------------


setwd("C:/Users/user/Desktop/WD ANN")####
.libPaths("C:/Users/user/Desktop/WD ANN/Library ANN")
#.libPaths("C:/Users/user/Desktop/WorkingDIR/libraryfake")
#.libPaths("C:/Users/user/Desktop/WorkingDIR/MyLibrary")

#install.packages("tidyverse")
#library(tidyverse)
#Import & Load data 
# Read tabular data into R
# Training data
#http://www.learnbymarketing.com/tutorials/neural-networks-in-r-tutorial/

#  2-1 Load dataset -------------------------------------------------


print( "... load dataset ..." )
data = read.csv( "../dataset/vehicle.csv", stringsAsFactor = FALSE )


## Transform dataset
#dataset = data %>% 
  #filter( class == "flood" | class == "Non_flood" ) %>%
  #transform( class = ifelse( class == "Non-flood", 0, 1 ) )
#dataset = as.data.frame( sapply( dataset, as.numeric ) )


## Spliting training and testing dataset
index = sample( 1:nrow( dataset ), nrow( dataset ) * 0.6, replace = FALSE ) 
trainset = dataset[ index, ]
testset = dataset[ -index, ]


# Count the number of 1 and 0 elements with the values of dependent vector
as.data.frame(table(dataset))

# Do Scale the data
# Standarization or Normalization, see this for more info
# https://sebastianraschka.com/Articles/2014_about_feature_scaling.html
# Z-score standardization or [ Min-Max scaling: typical neural network algorithm require data that on a 0-1 scale]
# https://vitalflux.com/data-science-scale-normalize-numeric-data-using-r/

# Original equation: n4 - unitization with zero minimum 
# >> ((x-min)/range))=y
# y: is the scaled value
# x :is the original value
# range: original range
# min:  original minmum value

#  > ((y*range)+min)= x



# Ref (https://stackoverflow.com/questions/15215457/standardize-data-columns-in-r)


#scaling the dataset to 1,0

maxs <- apply(dataset, 2, max) 
mins <- apply(dataset, 2, min)
scaled_dataset <- as.data.frame(scale(dataset, center = mins, scale = maxs - mins))
str(scaled_dataset) 

## Spliting training and testing dataset
index = sample( 1:nrow( scaled_dataset ), nrow( scaled_dataset ) * 0.6, replace = FALSE ) 
trainset = scaled_dataset[ index, ]
testset = scaled_dataset[ -index, ]


# 2-2  Dimensionality Reduction  --------------------------------------------------------
# PCA
### More about the process https://towardsdatascience.com/dimensionality-reduction-does-pca-really-improve-classification-outcome-6e9ba21f0a32 ###
print( "... principal component analysis ..." )
pca_trainset = trainset %>% select( -class )
pca_testset = testset %>% select( -class )

pca = prcomp( pca_trainset, scale = T )
pr_var = (pca$sdev)^2 # variance
prop_varex = pr_var / sum( pr_var )
#plot( prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b" ) #scree plot
#plot( cumsum( prop_varex ), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", type = "b" )

# Creating a new dataset
train = data.frame( class = trainset$class, pca$x )
test = as.data.frame( predict( pca, newdata = pca_testset ) )

new_trainset = train[, 1:9]
new_testset =  test[, 1:8]


# 3 Run NN function ------------------------------------------------


# 3-1 NN for Training data ------------------------------------------------


# Using across-entropy in NNA

# More about the process: https://datascienceplus.com/neuralnet-train-and-test-neural-networks-using-r/
install.packages("neuralnet")
library(neuralnet)
set.seed(7896129)
n = names( trainset )
f = as.formula( paste( "class ~", paste( n[!n %in% "class"], collapse = "+" ) ) )
nn = neuralnet( f, trainset, hidden = 6, linear.output = FALSE, threshold = 0.01 )


plot(nn, radius  =  0.15,  arrow.length  =  0.2,  intercept  =  TRUE,
     intercept.factor  =  0.4,  information  =  TRUE,  information.pos  =  0.1,
     col.entry.synapse  =  "black",  col.entry  =  "black",
     col.hidden  =  "black",  col.hidden.synapse  =  "black",
     col.out  =  "black",  col.out.synapse  =  "black",
     col.intercept  =  "blue",  fontsize  =  12,  dimension  =  6,
     show.weights  =  TRUE)

nn$result.matrix # to check the error
# error= 3.576598e+02

###### ......NOTE --------------------------------------------------------------------


########## In case you get this message "Warning message:algorithm did not converge in 1 of 1 repetition(s) within the stepmax ", and you wont able to plot nn
    #then try the following:
#The rep argument is basically how many times you train your neural network. See the answer to this question. Therefore the higher the rep, the longer it will take.You should increase the stepmax to give your model more chances to learn/converge. Or you can increase your threshold to allow an earlier stop for convergence. Alternatively, you can adjust your model, e.g. lower the number of your hidden layers and nodes
#
##
#Increasing the stepmax value from the default 1e+05 to 1e+08 makes the algorithm take exponentially more time. E.g. 90s for stepmax=1e+05 took 4h for stepmax=1e+08!
#  
#  So preferably increase first the threshold from its default = 0.01 successively to 0.1,0 .2, 0.3 etc because this doesn't affect performance.
###########################.




# 3-2 Add ANN results to dataframe of covariate (independents)-----------------------------------------------------------------


out_train=data.frame(cbind(nn$covariate, nn$net.result[[1]]))
head(out_train)
#dimnames(out_train) <- list(NULL, c("slope","Elevation","Curvature","NDVI","nn-output"))

colnames(out_train)[21] <- "ANN_W" # Add header to ANN results


# Round the decimals EXTRA
#out_train=round(out_train,digits=3)


# 3-3 Pairwise NN model results of Explanatories and Response Data -----------------------



#1-2 Visualize your pairwise comparisons of the result matrix [Note: these relationships, reflect only the training data, i.e. high and low value for each variable it does not reflect the highest and lowest value for the entire varaible. Instead is a general description about the events behaivors within the zones. And is still valid to be used as reference to the non linear relationship btw the variable values and events locations.]

# To check any value from pairwise table with original data use the normalization formula that mentioned earlier (line 75)

# Original value = x * (range) + Min
# Where x is the scaled value (0 to 1) on the graph 

# Source: https://www.r-bloggers.com/five-ways-to-visualize-your-pairwise-comparisons/
## The code below, just copy and paste and RUN it, no changes needed!
#panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
#{
 # usr <- par("usr"); on.exit(par(usr)) 
  #par(usr = c(0, 1, 0, 1)) 
  #r <- abs(cor(x, y)) 
  #txt <- format(c(r, 0.123456789), digits=digits)[1] 
  #txt <- paste(prefix, txt, sep="") 
  #if(missing(cex.cor)) cex <- 1.4/strwidth(txt) 
  
 # test <- cor.test(x,y) 
  # borrowed from printCoefmat
  #Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   #cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   #symbols = c("***", "**", "*", ".", " ")) 
  
  #text(0.5, 0.5, txt, cex = cex * r) 
 # text(.8, .8, Signif, cex=cex, col=2) 
#}

# Now run the plot function Using the Norm variables and ANN prediction, above code, just copy and paste and RUN, no changes needed!
#pairs(out_train, lower.panel=panel.smooth, upper.panel=panel.cor)


# 3-4 ***Generalized Weights plot --------------------------------------------


# 1-3-1 using normalized variables and generalized weights of nn results

par(mfrow=c(3,3))
gwplot(nn,selected.covariate="Elevation", min=-10,  max=10)
gwplot(nn,selected.covariate="Ndvi",min=-10,  max=10)
gwplot(nn,selected.covariate="slope",min=-10,  max=10)
gwplot(nn,selected.covariate="soil", min=-10,  max=10)
gwplot(nn,selected.covariate="spi", min=-10,  max=10)
gwplot(nn,selected.covariate="lulc", min=-10,  max=10)
gwplot(nn,selected.covariate="twi", min=-10,  max=10)
gwplot(nn,selected.covariate="rain", min=-10,  max=10)
gwplot(nn,selected.covariate="dfr", min=-10,  max=10)


#3-3 using back propogation algorithm

nn.bp <- neuralnet(Training ~ ROUGH+TWI+SLOPE+SPI+ELEVATION+CURVE+a+b+c+d+e+f+g+h+l0+l1+l2+l3+l4+l5+l6+l7, data=scaled_t, hidden=5, learningrate = 0.01,algorithm = "backprop",err.fct="ce", linear.output=FALSE,stepmax=1e+06) 

plot(nn.bp)
head(nn.bp$generalized.weights[[1]])


# 4  Variables importance using NNET Package function ---------------------


# 4-1 Run NNET function -------------------------------------------------------------


#Other website *Variable importance in neural networks*
#define number of variables and observations
#https://beckmw.wordpress.com/2013/08/12/variable-importance-in-neural-networks/

install.packages("clusterGeneration")
require(clusterGeneration)
require(nnet)

rand.vars=scaled_train[,c(2:23)]
#rand.vars=scaled_t[,c(3,5,6,7,8)]  ## test to delete !!!!

y=scaled_train[,1]
summary(y)
str(y)
num.vars<-22
num.obs<-1788

mod1<-nnet(rand.vars,y,size=5,linout=T)


# Model prediction
#mod2 <- predict(mod1, scaled_test[,c(-1,-3)], type = c("raw"))
#summary(mod2)


# 4-2 Variables imporance using NNET -------------------------------------


#Get the function JUST COPY AND PASTE BELOW THE LINK CONTENT   

#source_url('https://gist.githubusercontent.com/fawda123/6206737/raw/d6f365c283a8cae23fb20892dc223bc5764d50c7/gar_fun.r')
#################################################################.
install.packages("reshape")
library(reshape)

gar.fun<-function(out.var,mod.in,bar.plot=T,struct=NULL,x.lab=NULL,
                  y.lab=NULL, wts.only = F){
  
  require(ggplot2)
  
  # function works with neural networks from neuralnet, nnet, and RSNNS package
  # manual input vector of weights also okay
  
  #sanity checks
  if('numeric' %in% class(mod.in)){
    if(is.null(struct)) stop('Three-element vector required for struct')
    if(length(mod.in) != ((struct[1]*struct[2]+struct[2]*struct[3])+(struct[3]+struct[2])))
      stop('Incorrect length of weight matrix for given network structure')
    if(substr(out.var,1,1) != 'Y' | 
       class(as.numeric(gsub('^[A-Z]','', out.var))) != 'numeric')
      stop('out.var must be of form "Y1", "Y2", etc.')
  }
  if('train' %in% class(mod.in)){
    if('nnet' %in% class(mod.in$finalModel)){
      mod.in<-mod.in$finalModel
      warning('Using best nnet model from train output')
    }
    else stop('Only nnet method can be used with train object')
  }
  
  #gets weights for neural network, output is list
  #if rescaled argument is true, weights are returned but rescaled based on abs value
  nnet.vals<-function(mod.in,nid,rel.rsc,struct.out=struct){
    
    require(scales)
    require(reshape)
    
    if('numeric' %in% class(mod.in)){
      struct.out<-struct
      wts<-mod.in
    }
    
    #neuralnet package
    if('nn' %in% class(mod.in)){
      struct.out<-unlist(lapply(mod.in$weights[[1]],ncol))
      struct.out<-struct.out[-length(struct.out)]
      struct.out<-c(
        length(mod.in$model.list$variables),
        struct.out,
        length(mod.in$model.list$response)
      )      	
      wts<-unlist(mod.in$weights[[1]])   
    }
    
    #nnet package
    if('nnet' %in% class(mod.in)){
      struct.out<-mod.in$n
      wts<-mod.in$wts
    }
    
    #RSNNS package
    if('mlp' %in% class(mod.in)){
      struct.out<-c(mod.in$nInputs,mod.in$archParams$size,mod.in$nOutputs)
      hid.num<-length(struct.out)-2
      wts<-mod.in$snnsObject$getCompleteWeightMatrix()
      
      #get all input-hidden and hidden-hidden wts
      inps<-wts[grep('Input',row.names(wts)),grep('Hidden_2',colnames(wts)),drop=F]
      inps<-melt(rbind(rep(NA,ncol(inps)),inps))$value
      uni.hids<-paste0('Hidden_',1+seq(1,hid.num))
      for(i in 1:length(uni.hids)){
        if(is.na(uni.hids[i+1])) break
        tmp<-wts[grep(uni.hids[i],rownames(wts)),grep(uni.hids[i+1],colnames(wts)),drop=F]
        inps<-c(inps,melt(rbind(rep(NA,ncol(tmp)),tmp))$value)
      }
      
      #get connections from last hidden to output layers
      outs<-wts[grep(paste0('Hidden_',hid.num+1),row.names(wts)),grep('Output',colnames(wts)),drop=F]
      outs<-rbind(rep(NA,ncol(outs)),outs)
      
      #weight vector for all
      wts<-c(inps,melt(outs)$value)
      assign('bias',F,envir=environment(nnet.vals))
    }
    
    if(nid) wts<-rescale(abs(wts),c(1,rel.rsc))
    
    #convert wts to list with appropriate names 
    hid.struct<-struct.out[-c(length(struct.out))]
    row.nms<-NULL
    for(i in 1:length(hid.struct)){
      if(is.na(hid.struct[i+1])) break
      row.nms<-c(row.nms,rep(paste('hidden',i,seq(1:hid.struct[i+1])),each=1+hid.struct[i]))
    }
    row.nms<-c(
      row.nms,
      rep(paste('out',seq(1:struct.out[length(struct.out)])),each=1+struct.out[length(struct.out)-1])
    )
    out.ls<-data.frame(wts,row.nms)
    out.ls$row.nms<-factor(row.nms,levels=unique(row.nms),labels=unique(row.nms))
    out.ls<-split(out.ls$wts,f=out.ls$row.nms)
    
    assign('struct',struct.out,envir=environment(nnet.vals))
    
    out.ls
    
  }
  
  # get model weights
  best.wts<-nnet.vals(mod.in,nid=F,rel.rsc=5,struct.out=NULL)
  
  # weights only if T
  if(wts.only) return(best.wts)
  
  # get column index value for response variable to measure
  if('numeric' %in% class(mod.in)){
    out.ind <-  as.numeric(gsub('^[A-Z]','',out.var))
  } else {
    out.ind<-which(out.var==colnames(eval(mod.in$call$y)))
  }
  
  #get variable names from mod.in object
  #change to user input if supplied
  if('numeric' %in% class(mod.in)){
    x.names<-paste0(rep('X',struct[1]),seq(1:struct[1]))
    y.names<-paste0(rep('Y',struct[3]),seq(1:struct[3]))
  }
  if('mlp' %in% class(mod.in)){
    all.names<-mod.in$snnsObject$getUnitDefinitions()
    x.names<-all.names[grep('Input',all.names$unitName),'unitName']
    y.names<-all.names[grep('Output',all.names$unitName),'unitName']
  }
  if('nn' %in% class(mod.in)){
    x.names<-mod.in$model.list$variables
    y.names<-mod.in$model.list$response
  }
  if('xNames' %in% names(mod.in)){
    x.names<-mod.in$xNames
    y.names<-attr(terms(mod.in),'factor')
    y.names<-row.names(y.names)[!row.names(y.names) %in% x.names]
  }
  if(!'xNames' %in% names(mod.in) & 'nnet' %in% class(mod.in)){
    if(is.null(mod.in$call$formula)){
      x.names<-colnames(eval(mod.in$call$x))
      y.names<-colnames(eval(mod.in$call$y))
    }
    else{
      forms<-eval(mod.in$call$formula)
      x.names<-mod.in$coefnames
      facts<-attr(terms(mod.in),'factors')
      y.check<-mod.in$fitted
      if(ncol(y.check)>1) y.names<-colnames(y.check)
      else y.names<-as.character(forms)[2]
    } 
  }
  #change variables names to user sub 
  if(!is.null(x.lab)){
    if(length(x.names) != length(x.lab)) stop('x.lab length not equal to number of input variables')
    else x.names<-x.lab
  }
  if(!is.null(y.lab)){
    if(length(y.names) != length(y.lab)) stop('y.lab length not equal to number of output variables')
    else y.names<-y.lab
  }
  
  #get input-hidden weights and hidden-output weights, remove bias
  inp.hid<-data.frame(
    do.call('cbind',best.wts[grep('hidden',names(best.wts))])[-1,],
    row.names=x.names#c(colnames(eval(mod.in$call$x)))
  )
  hid.out<-best.wts[[grep(paste('out',out.ind),names(best.wts))]][-1]
  
  #multiply hidden-output connection for each input-hidden weight
  mult.dat<-data.frame(
    sapply(1:ncol(inp.hid),function(x) inp.hid[,x]*hid.out[x]),
    row.names=rownames(inp.hid)
  )    
  names(mult.dat)<-colnames(inp.hid)
  
  #get relative contribution of each input variable to each hidden node, sum values for each input
  #inp.cont<-rowSums(apply(mult.dat,2,function(x) abs(x)/sum(abs(x))))
  inp.cont<-rowSums(mult.dat)
  
  #get relative contribution
  #inp.cont/sum(inp.cont)
  
  rel.imp<-{
    signs<-sign(inp.cont)
    signs*rescale(abs(inp.cont),c(0,1))
  }
  
  if(!bar.plot){
    return(list(
      mult.wts=mult.dat,
      inp.cont=inp.cont,
      rel.imp=rel.imp
    ))
  }
  
  to_plo <- data.frame(rel.imp,x.names)[order(rel.imp),,drop = F]
  to_plo$x.names <- factor(x.names[order(rel.imp)], levels = x.names[order(rel.imp)])
  out_plo <- ggplot(to_plo, aes(x = x.names, y = rel.imp, fill = rel.imp,
                                colour = rel.imp)) + 
    geom_bar(stat = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y.names)
  
  return(out_plo)
  
}
################################.

install.packages("ggplot2")
install.packages("scales")
library(ggplot2)
library(scales)
scales
#create a pretty color vector for the bar plot
cols<-colorRampPalette(c('lightgreen','lightblue'))(num.vars)
#use the function on the model created above
par(mar=c(3,4,1,1),family='serif')
gar.fun('y',mod1,y.lab = )

###############################.

# 4-3 Plot NNET function --------------------------------------------------

# https://beckmw.wordpress.com/tag/nnet/

#import the function from Github
install.packages("devtools")
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

#plot nnet model
par(mfrow=c(1,1))
plot.nnet(mod1)



# 4-4 Sensitivity analysis for neural networks ---------------------------


# https://beckmw.wordpress.com/tag/nnet/

# 1/ how does a response variable change in relation to increasing or decreasing values of a given explanatory variable? 
# 2/ Is it a linear response, non-linear, uni-modal, no response? 
# 3/ how does the form of the response change given values of the other explanatory variables in the model? 


# Methodology
# For any statistical model where multiple response variables are related to multiple explanatory variables, we choose one response and one explanatory variable. We obtain predictions of the response variable across the range of values for the given explanatory variable. All other explanatory variables are held constant at a given set of respective values (e.g., minimum, 20th percentile, maximum). The final product is a set of response curves for one response variable across the range of values for one explanatory variable, while holding all other explanatory variables constant. This is implemented in R by creating a matrix of values for explanatory variables where the number of rows is the number of observations and the number of columns is the number of explanatory variables. All explanatory variables are held at their mean (or other constant value) while the variable of interest is sequenced from its minimum to maximum value across the range of observations. This matrix (actually a data frame) is then used to predict values of the response variable from a fitted model object. This is repeated for different variables.


require(clusterGeneration)
require(nnet)

#rand.vars<-data.frame(rand.vars)
rand.vars=scaled_train[,c(2:10)]
#rand.vars=scaled_t[,c(3,5,6,7,8)]  ## test to delete !!!!

y=scaled_train[,1]
summary(y)
str(y)
num.vars<-10
num.obs<-80


class(rand.vars)
resp<-y
resp<-data.frame(resp)
names(resp)<-'Y1'
mod1.SA<-nnet(rand.vars,size=5,resp,linout=T)

#mod1.SA<-nnet(rand.vars[3:6],resp,size=8,linout=T) 
#mod1.SA<-nnet(rand.vars,resp,size=8,linout=T)  ##test to del!!!!

source('https://gist.githubusercontent.com/fawda123/6860630/raw/b8bf4a6c88d6b392b1bfa6ef24759ae98f31877c/lek_fun.r')
install.packages("reshape2")
library(reshape2)

# plot all variables
lek.fun(mod1.SA)

# to evaluate different quantile values as well. 
lek.fun(mod1.SA,var.sens=c('soil','Ndvi'))#,split.vals=seq(0,1,by=0.1))
View(lek.fun(mod1.SA,val.out=T))


# Find the quantile split for any varaible in the dataframe
# https://stackoverflow.com/questions/26273892/r-splitting-dataset-into-quartiles-deciles-what-is-the-right-method/26275749

trainset$Elev_Qu = cut( trainset$ELevation, quantile(trainset$Elevation, prob = seq(0, 1, length = 11), type = 5) )
View(trainset)
trainset$Elev_Qu=NULL # to remove column







# lek profile function using the cluster method. #Get the function JUST COPY AND PASTE NO CHANGES NEEEDED  


######Source ; https://github.com/fawda123/NeuralNetTools######
#more about lek function: https://www.rdocumentation.org/packages/NeuralNetTools/versions/1.0.0/topics/lekprofile
# Further reading about application of lek function; Beck MW (2018). "NeuralNetTools: Visualization and Analysis Tools for Neural Networks." Journal of Statistical Software, 85(11), pp. 1-20. doi: 10.18637/jss.v085.i11 (URL: http://doi.org/10.18637/jss.v085.i11).


#' Sensitivity analysis using Lek's profile method
#' 
#' Conduct a sensitivity analysis of model responses in a neural network to input variables using Lek's profile method
#' 
#' @param mod_in input object for which an organized model list is desired.  The input can be an object of class \code{nnet} or \code{mlp}
#' @param xvars \code{\link[base]{data.frame}} of explanatory variables used to create the input model, only needed for \code{mlp} objects
#' @param yvars \code{\link[base]{data.frame}} of explanatory variables used to create the input model, only needed for \code{mlp} objects
#' @param ysel chr string indicating which response variables to plot if more than one, defaults to all
#' @param xsel chr string of names of explanatory variables to plot, defaults to all
#' @param steps numeric value indicating number of observations to evaluate for each explanatory variable from minimum to maximum value, default 100
#' @param group_vals numeric vector with values from 0-1 indicating quantile values at which to hold other explanatory variables constant or a single value indicating number of clusters to define grouping scheme, see details
#' @param val_out logical value indicating if actual sensitivity values are returned rather than a plot, default \code{FALSE}
#' @param group_show logical if a barplot is returned that shows the values at which explanatory variables were held constant while not being evaluated
#' @param grp_nms optional chr string of alternative names for groups in legend
#' @param position chr string indicating bar position (e.g., 'dodge', 'fill', 'stack'), passed to \code{\link[ggplot2]{geom_bar}}, used if \code{group_show = TRUE}
#' @param ... arguments passed to other methods
#' 
#' @details
#' The Lek profile method is described briefly in Lek et al. 1996 and in more detail in Gevrey et al. 2003. The profile method is fairly generic and can be extended to any statistical model in R with a predict method.  However, it is one of few methods used to evaluate sensitivity in neural networks.
#' 
#' The profile method can be used to evaluate the effect of explanatory variables by returning a plot of the predicted response across the range of values for each separate variable.  The original profile method evaluated the effects of each variable while holding the remaining explanatory variables at different quantiles (e.g., minimum, 20th percentile, maximum).  This is implemented in in the function by creating a matrix of values for explanatory variables where the number of rows is the number of observations and the number of columns is the number of explanatory variables. All explanatory variables are held at their mean (or other constant value) while the variable of interest is sequenced from its minimum to maximum value across the range of observations. This matrix (or data frame) is then used to predict values of the response variable from a fitted model object. This is repeated for each explanatory variable to obtain all response curves.  Values passed to \code{group_vals} must range from zero to one to define the quantiles for holding unevaluated explanatory variables. 
#' 
#' An alternative implementation of the profile method is to group the unevaluated explanatory variables using groupings defined by the statistical properties of the data.  Covariance among predictors may present unlikely scenarios if holding all unevaluated variables at the same level.  To address this issue, the function provides an option to hold unevaluated variable at mean values defined by natural clusters in the data.  \code{\link[stats]{kmeans}} clustering is used on the input \code{data.frame} of explanatory variables if the argument passed to \code{group_vals} is an integer value greater than one.  The centers of the clusters are then used as constant values for the unevaluated variables.  An arbitrary grouping scheme can also be passed to \code{group_vals} as a \code{data.frame} where the user can specify exact values for holding each value constant (see the examples). 
#' 
#' For all plots, the legend with the 'Groups' label indicates the colors that correspond to each group.  The groups describe the values at which unevaluated explanatory variables were held constant, either as specific quantiles, group assignments based on clustering, or in the arbitrary grouping defined by the user.  The constant values of each explanatory variable for each group can be viewed as a barplot by using \code{group_show = TRUE}.
#' 
#' Note that there is no predict method for neuralnet objects from the nn package.  The lekprofile method for nn objects uses the nnet package to recreate the input model, which is then used for the sensitivity predictions.  This approach only works for networks with one hidden layer. 
#' 
#' @export
#' 
#' @import ggplot2 nnet
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object for plotting if \code{val_out  =  FALSE}, otherwise a two-element \code{list} is returned with a \code{data.frame} in long form showing the predicted responses at different values of the explanatory variables and the grouping scheme that was used to hold unevaluated variables constant. 
#' 
#' @references
#' 
#' Beck, M.W. 2018. NeuralNetTools: Visualization and Analysis Tools for Neural Networks. Journal of Statistical Software. 85(11):1-20.
#' 
#' Lek, S., Delacoste, M., Baran, P., Dimopoulos, I., Lauga, J., Aulagnier, S. 1996. Application of neural networks to modelling nonlinear relationships in Ecology. Ecological Modelling. 90:39-52.
#' 
#' Gevrey, M., Dimopoulos, I., Lek, S. 2003. Review and comparison of methods to study the contribution of variables in artificial neural network models. Ecological Modelling. 160:249-264.
#' 
#' Olden, J.D., Joy, M.K., Death, R.G. 2004. An accurate comparison of methods for quantifying variable importance in artificial neural networks using simulated data. Ecological Modelling. 178:389-397.
#' 
#' @examples
#' 
#' ## using nnet
#' 
#' library(nnet)
#' 
#' set.seed(123)
#' 
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)
#'  
#' lekprofile(mod)  
#' 
#' \dontrun{
#' ## using RSNNS, no bias layers
#' 
#' library(RSNNS)
#' 
#' x <- neuraldat[, c('X1', 'X2', 'X3')]
#' y <- neuraldat[, 'Y1', drop = FALSE]
#' 
#' mod <- mlp(x, y, size = 5)
#' 
#' lekprofile(mod, xvars = x)
#' 
#' ## using neuralnet
#' 
#' library(neuralnet)
#' 
#' mod <- neuralnet(Y1 ~ X1 + X2 + X3, data = neuraldat, hidden = 5)
#' 
#' lekprofile(mod)
#' 
#' ## back to nnet, not using formula to create model
#' ## y variable must have a name attribute
#' 
#' mod <- nnet(x, y, size = 5)
#' 
#' lekprofile(mod)
#' 
#' ## using caret
#' 
#' library(caret)
#' 
#' mod <- train(Y1 ~ X1 + X2 + X3, method = 'nnet', data = neuraldat, linout = TRUE)
#' 
#' lekprofile(mod)
#' 
#' ## group by clusters instead of sequencing by quantiles
#' 
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)
#'  
#' lekprofile(mod, group_vals = 6) # six clusters
#' 
#' ## enter an arbitrary grouping scheme for the group values
#' ## i.e. hold all values at 0.5
#' group_vals <- rbind(rep(0.5, length = ncol(x)))
#' group_vals <- data.frame(group_vals)
#' names(group_vals) <- names(group_vals)
#' 
#' lekprofile(mod, group_vals = group_vals, xsel = 'X3')
#' }
lekprofile <- function(mod_in, ...) UseMethod('lekprofile')

#' @rdname lekprofile
#'
#' @import ggplot2 
#' 
#' @export
#' 
#' @method lekprofile default
lekprofile.default <- function(mod_in, xvars, ysel = NULL, xsel = NULL, steps = 100, group_vals = seq(0, 1, by = 0.2), val_out = FALSE, group_show = FALSE, grp_nms = NULL, position = 'dodge', ...){
  
  # subset xsel if xsel is not empy
  if(is.null(xsel)) xsel <- names(xvars)
  
  # stop if only one input variable
  if(ncol(xvars) == 1) stop('Lek profile requires greater than one input variable')
  
  # standard lekprofile method using quantile groups or clusters
  if(inherits(group_vals, c('numeric', 'integer'))){
    
    # quantile approach
    if(all(group_vals <= 1)){
      
      grps <- apply(xvars, 2, quantile, group_vals)
      grps <- as.data.frame(rbind(grps))
      
      # kmeans approach      
    } else {
      
      # sanity checks for integer, one value
      if(length(group_vals) > 1) stop('Multiple group_vals must be from 0 and 1')
      if(any(group_vals%%1 != 0)) stop('group_vals as a single value must be an integer')
      
      # get means of cluster centers
      grps <- kmeans(xvars, centers = group_vals)$centers
      
    }
    
    # use matrix or data.frame input for constant values
  } else {
    
    if(ncol(group_vals) != ncol(xvars)) stop('group_vals as matrix must have ncol same as xvars')
    grps <- group_vals
    names(grps) <- names(xvars)
    
  }
  
  # return bar plot for group values
  if(group_show) return(lekgrps(grps, position = position, grp_nms = grp_nms))
  
  #use 'pred_fun' to get pred vals of response across range of vals for an exp vars
  #loops over all explanatory variables of interest and all group values
  lek_vals <- sapply(
    xsel, 
    function(vars) pred_sens(xvars, mod_in, vars, steps, grps, ysel),
    simplify = FALSE  
  )
  
  #melt lek_val list for use with ggplot
  lek_vals <- melt(lek_vals, id.vars = 'x_vars')
  lek_vals$L2 <- factor(lek_vals$L2)#, labels = 1:nrow(grps))
  names(lek_vals) <- c('Explanatory', 'resp_name', 'Response', 'Groups', 'exp_name')
  
  # change factor levels for groups in legend
  if(!is.null(grp_nms)){
    
    uni_grps <- unique(lek_vals$Groups)
    if(length(grp_nms) != length(uni_grps))
      stop('grp_nms must have same length as group_vals')
    
    lek_vals$Groups <- factor(lek_vals$Groups, levels = uni_grps, labels = grp_nms)
    
  }
  
  #return only values if val_out = TRUE
  if(val_out) return(list(lek_vals, grps))
  
  #ggplot object
  p <- ggplot2::ggplot(lek_vals, aes_string(x = 'Explanatory', y = 'Response', group = 'Groups')) + 
    geom_line(aes_string(colour = 'Groups')) + 
    facet_grid(resp_name ~ exp_name, scales = 'free_x') + 
    theme_bw()
  
  return(p)
  
}

#' @rdname lekprofile
#'
#' @import ggplot2 
#' 
#' @export
#' 
#' @method lekprofile nnet
lekprofile.nnet <- function(mod_in, xsel = NULL, ysel = NULL, ...){
  
  # get exp and resp names from mod_in
  # get matrix for exp vars
  if(is.null(mod_in$call$formula)){
    
    ychk <- colnames(eval(mod_in$call$y))
    if(is.null(ychk)) stop('Response variables must have names attribute') 
    xchk <- colnames(eval(mod_in$call$x))
    if(is.null(xchk)) stop('Input variables must have names attribute')
    xvars <- eval(mod_in$call$x)
    
  } else {
    
    forms <- eval(mod_in$call$formula)
    dat_names <- try(model.frame(forms,data = eval(mod_in$call$data)))
    ychk <- as.character(forms)[2]
    xchk <- names(dat_names)[!names(dat_names) %in% as.character(forms)[2]]
    xvars <- dat_names[, !names(dat_names) %in% as.character(forms)[2], drop = F]
    
  }
  
  # replace xsel, ysel with model values if not provided
  if(is.null(xsel)) xsel <- xchk
  if(is.null(ysel)) ysel <- ychk
  
  lekprofile.default(mod_in, xvars = xvars, ysel = ysel, xsel = xsel, ...)
  
}

#' @rdname lekprofile
#' 
#' @import ggplot2 
#' 
#' @export
#' 
#' @method lekprofile mlp
lekprofile.mlp <- function(mod_in, xvars, yvars, xsel = NULL, ysel = NULL, ...){
  
  if(!inherits(xvars, 'data.frame')) stop('xvars must be a data.frame')
  if(!inherits(yvars, 'data.frame')) stop('yvars must be a data.frame')
  
  # getexp and resp names from mod_in if not provided
  # get matrix for exp vars
  if(is.null(ysel))
    ysel <- names(yvars)
  if(is.null(xsel))
    xsel <- names(xvars)
  
  lekprofile.default(mod_in, xvars = xvars, yvars = yvars, xsel = xsel, ysel = ysel, ...)
  
}

#' @rdname lekprofile
#'
#' @import ggplot2 
#' 
#' @export
#' 
#' @method lekprofile train
lekprofile.train <- function(mod_in, xsel = NULL, ysel = NULL, ...){
  
  # input data, x_names, and y_names
  xvars <- mod_in$trainingData
  xvars <- xvars[, !names(xvars) %in% '.outcome']
  ychk <- strsplit(as.character(mod_in$terms[[2]]), ' + ', fixed = TRUE)[[1]]
  mod_in <- mod_in$finalModel
  x_names <- mod_in$xNames
  xvars <- xvars[, x_names]
  
  if(is.null(ysel)) ysel <- ychk
  
  lekprofile.default(mod_in, xvars = xvars, xsel = xsel, ysel = ysel, ...)
  
}

#' @rdname lekprofile
#'
#' @import ggplot2 nnet
#' 
#' @export
#' 
#' @method lekprofile nn
lekprofile.nn <- function(mod_in, xsel = NULL, ysel = NULL, ...){
  
  # recreate the model using nnet (no predict method for nn)
  moddat <- mod_in$data
  modwts <- neuralweights(mod_in)
  modwts <- unlist(modwts$wts)
  modsz <- mod_in$call$hidden
  modfrm <- eval(mod_in$call$formula)
  modlin <- mod_in$call$linear.output
  modlin2 <- TRUE
  if(!is.null(modlin)) modlin2 <- modlin
  
  # stop if multiple hidden layers - nnet can only do one input
  # mlp can do this but does not accept starting weights
  if(length(modsz) > 1) stop('Cannot use lekprofile with multiple hidden layers')
  
  # create call for nnet model
  mod_in <- substitute(
    nnet(formin, data = moddat, size = modsz, 
         Wts = modwts, maxit = 0, linout = modlin2, trace = FALSE), 
    list(formin = formula(modfrm), moddat = moddat, modsz = modsz, modwts = modwts, 
         modlin2 = modlin2)
  )
  
  # eval call
  mod_in <- eval(mod_in)
  
  # pass to lekprofile.nnet
  lekprofile(mod_in, xsel = xsel, ysel = ysel, ...)
  
}

## plot the sensitivity curves 
lekprofile(nn) # prediction curves all variables
lekprofile(nn, group_vals = 6, group_show = TRUE)# bar plots of constant values for clusters of prediction outcome

## plot seprate sensity plot for each explanatory  variable

p1 <- lekprofile(mod1.SA,var.sens=c('Elevation'))#,split.vals=seq(0,1,by=0.1))
p2 <- lekprofile(mod1.SA,var.sens=c('slope'))#,split.vals=seq(0,1,by=0.1))
p3 <- lekprofile(mod1.SA,var.sens=c('Ndvi'))#,split.vals=seq(0,1,by=0.1))
p4 <-lekprofile(mod1.SA,var.sens=c('soil'))#,split.vals=seq(0,1,by=0.1))
p5 <-lekprofile(mod1.SA,var.sens=c('spi'))#,split.vals=seq(0,1,by=0.1))
p6 <-lekprofile(mod1.SA,var.sens=c('lulc'))#,split.vals=seq(0,1,by=0.1))
p7 <-lekprofile(mod1.SA,var.sens=c('twi'))#,split.vals=seq(0,1,by=0.1))
p8 <-lekprofile(mod1.SA,var.sens=c('rain'))#,split.vals=seq(0,1,by=0.1))
p9 <-lekprofile(mod1.SA,var.sens=c('dfr'))#,split.vals=seq(0,1,by=0.1))

p1 + p2 + p3 + p4+ p5+ p6+ p7+ p8+ p9+plot_layout(ncol = 3, guides = 'collect')












# 5 NN Model prediction ---------------------------------------------------


# 5-1 NN prediction using Testing data ------------------------------------


#2 Model prediction test

#2-1 Let us first Test the model prediction performance using the training data (Data used for model building)
head(testset,1)
compute.output_Tr=compute(nn,trainset[,c(-1)])

out_pred_tr <- data.frame(cbind(trainset[,c(-1)], compute.output_Tr$net.result))
str(out_pred_tr)
#dimnames(out_pred) <- list(NULL, c("slope","Elevation","Curvature","NDVI","nn-output"))
colnames(out_pred_tr)[9] <- "ANN_P" # Add header to ANN results

table(trainset$class,  out_pred_tr$ANN_P  >  0.5)


#2-1 Let us Now Test the model prediction performance using the Testing data (not used during model building)

head(testset,1)
compute.output_test=compute(nn,testset[,c(-1)])

out_pred_test <- data.frame(cbind(testset[,c(-1)], compute.output_test$net.result))
str(out_pred_test)
#dimnames(out_pred) <- list(NULL, c("slope","Elevation","Curvature","NDVI","nn-output"))
colnames(out_pred_test)[9] <- "ANN_P" # Add header to ANN results
table(scaled_test$class,  out_pred_test$ANN_P  >  0.5)
#.libPaths("C:/Users/user/Desktop/WorkingDIR/libraryfake")



# 5-2 Predction Validation with AUC value and ROC plot  ---------------------------------------



#ROC summarizes the predictive power for all possible values of p > 0.5.  
#pred  <-  neuralnet::compute(nn.ce, covariate = as.matrix(scaled_test[,-1]))
#pred$net.result
packages <- c("rgeos", "rgdal", "maptools")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(packages)

install.packages("ROCR")
install.packages("gtools")
install.packages("gplots")
install.packages("gdata")
install.packages("bitops")
library(gplots)
library(ROCR)

ROCRpred_pred <- prediction(out_pred_test$ANN_P, testset$class)
ROCRpred_sucess <- prediction(out_pred_tr$ANN_P, trainset$class)

ROCRperf_sucess <- performance(ROCRpred_sucess, 'tpr','fpr')
ROCRperf_pred <- performance(ROCRpred_pred, 'tpr','fpr')

par(mfcol = c(1,1))
plot(ROCRperf_sucess)


#### Find AUC area under curve of ROC: 
# referred to as index of accuracy(A) or concordance index, is a perfect performance metric for ROC curve. 
auc <- performance(ROCRpred_sucess, measure = "auc")
auc <- auc@y.values[[1]]
auc

par(mfcol = c(1,1))
plot(ROCRperf_sucess, main= "Sucess rate AUC=0.95", colorize = TRUE, text.adj = c(-0.5,1.7))



# plot prediction rate

#### Find AUC area under curve of ROC: 
# referred to as index of accuracy(A) or concordance index, is a perfect performance metric for ROC curve. 
auc <- performance(ROCRpred_pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

par(mfcol = c(1,1))
plot(ROCRperf_pred, main= "Prediction rate AUC=0.80", colorize = TRUE, text.adj = c(-0.5,1.7))




# Extra: Modeling delay test ----------------------------------------


# Delay test : A comparison of run times for fastest method
#Cross-entropy
system.time(nn <- neuralnet(Training~slope+Elevation+Curvature+NDVI,data=train_, hidden=3, err.fct="ce", linear.output=FALSE,stepmax=1e+08) )

##back propogation : very slow :)
system.time(nn.bp <- neuralnet(Training~slope+Elevation+Curvature+NDVI,data=train_, hidden=3, learningrate = 0.01,algorithm = "backprop",err.fct="ce", linear.output=FALSE,stepmax=1e+06) )



# 6  Produce prediction map using Raster data ---------------------------


# 6-1 Import and process thematic maps ------------------------------------


#Produce LSM map using Training model results and Raster layers data

# Import Raster
install.packages("raster")
install.packages("rgdal")
library(raster)
library(rgdal)


# load all the data

# Load the Raster data
Elevation = raster("C:/Users/bertrand/Desktop/floodDIR/MyData/Elevation.tif")  
Ndvi= raster("C:/Users/bertrand/Desktop/floodDIR/MyData/Ndvi.tif")
dfr = raster("C:/Users/bertrand/Desktop/floodDIR/MyData/dfr.tif")
slope= raster("C:/Users/bertrand/Desktop/floodDIR/MyData/slope.tif") 
twi= raster("C:/Users/bertrand/Desktop/floodDIR/MyData/twi.tif") 
rain = raster("C:/Users/bertrand/Desktop/floodDIR/MyData/rain.tif")
soil = raster("C:/Users/bertrand/Desktop/floodDIR/MyData/soil.tif")
spi = raster("C:/Users/bertrand/Desktop/floodDIR/MyData/spi.tif")
lulc = raster("C:/Users/bertrand/Desktop/floodDIR/MyData/lulc.tif")

# check attributes and projection and extent
extent(ELEVATION)
extent(Ndvi)
extent(dfr)
extent(slope)
extent(twi)
extent(rain)
extent(soil)
extent(spi)
extent(lulc)


# if you have diffrent extent, then try to Resample them using the smallest area
Ndvi_re<- resample(Ndvi,Elevation, resample='bilinear')
dfr_re<- resample(dfr,Elevation, resample='bilinear')
slope_re<- resample(slope,Elevation, resample='bilinear')
twi_re<- resample(twi,Elevation, resample='bilinear')
rain_re<- resample(rain,Elevation, resample='bilinear')
soil_re<- resample(soil,Elevation, resample='bilinear')
spi_re<- resample(spi,Elevation, resample='bilinear')
lulc_re<- resample(lulc,Elevation, resample='bilinear')



extent(ASPECT_r) # check the new extent
extent(LANDCOVER)

# write to a new geotiff file
writeRaster(Ndvi_re,filename="resampled/Ndvi.tif", format="GTiff", overwrite=TRUE) 
writeRaster(dfr_re,filename="resampled/dfr.tif", format="GTiff", overwrite=TRUE)
writeRaster(slope_re,filename="resampled/slope.tif", format="GTiff", overwrite=TRUE)
writeRaster(twi_re,filename="resampled/twi.tif", format="GTiff", overwrite=TRUE)
writeRaster(rain_re,filename="resampled/rain.tif", format="GTiff", overwrite=TRUE)
writeRaster(soil_re,filename="resampled/soil.tif", format="GTiff", overwrite=TRUE)
writeRaster(spi_re,filename="resampled/spi.tif", format="GTiff", overwrite=TRUE)
writeRaster(lulc_re,filename="resampled/lulc.tif", format="GTiff", overwrite=TRUE)
writeRaster(Elevation,filename="resampled/Elevation.tif", format="GTiff", overwrite=TRUE)



#Stack_List= stack(ASPECT_r,LS_r)#,pattern = "tif$", full.names = TRUE)
#names(Stack_List)
#Stack_List.df = as.data.frame(Stack_List, xy = TRUE, na.rm = TRUE)
#head(Stack_List.df,1)


## stack multiple raster files
Stack_List= list.files(path = "resampled/",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)

names(Rasters)


# 6-1-1 Convert rasters to dataframe with Long-Lat -----------------------


#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)


# Now:Prediction using imported Rasters

# check the varaibles names to match with training data
#colnames(Rasters.df)[4] <- "ElEVATION"   # change columns names 
#colnames(Rasters.df)[6] <- "SlOPE"   # change columns names 

#head(Rasters.df[,c(-9,-10)],1)
#head(nn.ce$covariate,1)

Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y




# 6-1-3 Scale the numeric variables --------------------------------------


# Check the relationship between the numeric varaibles, Scale the numeric var first!
maxss <- apply(Rasters.df_N[,1:9], 2, max) 
minss <- apply(Rasters.df_N[,1:9], 2, min)
Rasters.df_N_scaled <- as.data.frame(scale(Rasters.df_N[,1:9], center = minss, scale = maxss - minss)) # we removed the Aspect levels because it might be changed to NA!


# Now let us add back the (x,y) 
Rasters.df_N_scaled <- data.frame(cbind(Rasters.df[,c(1,2)]))

# Final check if the nn covariate match with Rasters.df_N_scaled
head(nn$covariate,1)
head(Rasters.df_N_scaled,1)

# 6-2 Run the compute (prediction function) -------------------------------


# Run the compute(prediction function) to get the predicted weights of ANN model
compute.SM=compute(nn,Rasters.df_N_scaled[,c(-1,-2)]) # to let the variables matched 

out_SM <- data.frame(cbind(Rasters.df_N_scaled, compute.SM$net.result))
str(out_SM)
colnames(out_SM)[10] <- "ANN_P" # Add header to ANN results


# Check the pairwise between the predicted variable and other variables
#pairs(out_SM)

# Almost done, try to remove other variables and keep x,y and ANN to be ploted
out_SM <- out_SM[,c(1,2,10)]


#Convert Dataframe back to raster with Long-Lat
#https://stackoverflow.com/questions/19627344/how-to-create-a-raster-from-a-data-frame-in-r


# 6-3 Convert Dataframe back to rasters with Long-Lat ---------------------


SM_from_df <- rasterFromXYZ(out_SM)  #Convert first two columns as lon-lat and third as value                
plot(SM_from_df)
SM_from_df
# coord. ref. : NA 


# Add coord. ref. system by using the original data info (Copy n Paste).
projection(SLOPE) # borrow the projection from Raster data

proj4string(SM_from_df)=CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") # set it to lat-long


# 6-4 Export final prediction map as raster TIF ---------------------------

# write to a new geotiff file
writeRaster(SM_from_df,filename="final prediction map/Prediction_Map.tif", format="GTiff", overwrite=TRUE) 

##########DDDDDDDDDDDDOOOOOOOOOOOOOOOONNNNNNNNNNNNNNEEEEEEEEE :) :)






