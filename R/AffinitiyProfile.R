#' @exportClass AffinitiyProfile
#' @name AffinitiyProfile 
#' @rdname AffinitiyProfile
#' @aliases AffinitiyProfile-class 
#' @title The Affinitiy - Profile class by Felix Lindemann
#' 
#'
#' @description 
#' This class is part of the \pkg{wmvg}. It represents 
#' the class for an alterntaive-profile in an evaluation
#' 
#' 
setClass(
  Class = "AffinitiyProfile",
  representation=representation( 
    alternatives    = "data.frame",
    items    = "data.frame",
    values    = "matrix"
  ),  
  validity = function(object){
    
    
    # tmp <- as.node(object)
    if(is.null(object@alternatives))
      return(paste("Invalid Object of Type 'AffinitiyProfile': Error with value alternatives: expected non-null data.frame, but obtained null"))
    
    if(is.null(object@items))
      return(paste("Invalid Object of Type 'AffinitiyProfile': Error with value items: expected non-null data.frame, but obtained null"))
    
    if(is.null(object@values))
      return(paste("Invalid Object of Type 'AffinitiyProfile': Error with value values: expected non-null matrix, but obtained null"))
    
    if( class(object@alternatives)!="data.frame" ) 
      return(paste("Invalid Object of Type 'AffinitiyProfile': Error with value alternatives: expected data.frame, but obtained", class(object@alternatives)))
    
    if( class(object@items)!="data.frame" ) 
      return(paste("Invalid Object of Type 'AffinitiyProfile': Error with value items: expected data.frame, but obtained", class(object@items)))
    
    if( class(object@values)!="data.frame" ) 
      return(paste("Invalid Object of Type 'AffinitiyProfile': Error with value items: expected matrix, but obtained", class(object@values)))
    
    if(ncol(object@values) != nrow(object@items))
      return(paste("Invalid Object of Type 'AffinitiyProfile': Error with value items: expected matrix with",
                   nrow(object@items),"columns. but obtained", nrow(object@items)))
    
    if(nrow(object@values) != nrow(object@items))
      return(paste("Invalid Object of Type 'AffinitiyProfile': Error with value items: expected matrix with",
                   nrow(object@items),"rows- but obtained", nrow(object@items)))
    
    return(TRUE) 
  } 
)
################################### initialize - Method ###################################################
#' @title initialize Method
#' @name initialize
#' @title Initialization Method for The Affinitiy - Profile class by Felix Lindemann
#' @aliases initialize,AffinitiyProfile-method 
#' @rdname initialize-methods  
#' 
#' @param object Object of Type \code{\link{AffinitiyProfile}} 
#' @param alternatives Object of Type \code{\link{data.frame}} 
#' @param items Object of Type \code{\link{matrix}} 
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by this initialize-method}{
#'    \describe{  
#'     \item{log}{\code{logical} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}  
#'     \item{showwarnings}{\code{logical} Optional Parameter. Indicating, if problems should be logged to console. Default is \code{TRUE}.}  
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{initialize}}} 
#'      \item{no parameters forwarded.} 
#'    }
#' }
setMethod("initialize", signature="AffinitiyProfile", function(.Object, #alternatives=NULL, items=NULL, 
                                                               ...) {
  
  li <- list(...)
  if(is.null(li$showwarnings))  li$showwarnings <- TRUE
  
  #init from parent Node-Object.
  N<-1
  .Object@alternatives <- data.frame()
  .Object@items <- data.frame()
  .Object@values <- matrix()
  return(.Object ) 
})


setGeneric("addAlternative", function(object,...)  standardGeneric("addAlternative") )
#' @aliases addAlternative,AffinitiyProfile-method
#' @title Add an alternative
#' @rdname addAlternative
#' 
#' @param object Object of Type \code{\link{AffinitiyProfile}} 
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{addAlternative}}}{
#'    \describe{  
#'     \item{log}{\code{logical} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}  
#'     \item{showwarnings}{\code{logical} Optional Parameter. Indicating, if problems should be logged to console. Default is \code{TRUE}.}  
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{ 
#'      \item{no parameters forwarded.} 
#'    }
#' }
setMethod("addAlternative",signature(object="AffinitiyProfile"),
          function(object,  ...){  # ... --> label, id=NULL, pch=20, col=2,
            li<-list(...) 
            
            if(is.null(li$log)) li$log <- FALSE
            if(is.null(li$N)) li$N <- 1
            
            if(is.null(li$id)) {
              if(is.null(li$label)){   
                stop( "either label or Id have to be provided")
              }else{
                li$id <-  li$label
              }
            }else{
             
            }             
            if(is.null(li$pch)) {
              li$pch <- rep(20,li$N)
            }else{
              if(length(li$pch)!=li$N){
                stop("Unexpected Lenght of parameter pch.")
              }
            }
            if(is.null(li$col)) {
              li$col <- rep(1,li$N)
            }else{
              if(length(li$col)!=li$N){
                stop("Unexpected Lenght of parameter col.")
              }
            }
            
            
            df<- data.frame(id=li$id, label=li$label, col=li$col, pch=li$pch, x=0, y=0)
            object@alternatives <- rbind(object@alternatives, df)
            
            while(ncol(object@values) < nrow(object@items)){
              object@values <- cbind(object@values, t(rep(NA, nrow(object@values))))
            }
              
            validObject(object)
            return (object)
          }
)


setGeneric("addItem", function(object,...)  standardGeneric("addItem") )
#' @aliases addItem,AffinitiyProfile-method
#' @title Add an item
#' @rdname addItem
#' @param object Object of Type \code{\link{AffinitiyProfile}} 
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{addItem}}}{
#'    \describe{  
#'     \item{log}{\code{logical} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}  
#'     \item{showwarnings}{\code{logical} Optional Parameter. Indicating, if problems should be logged to console. Default is \code{TRUE}.}  
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{ 
#'      \item{no parameters forwarded.} 
#'    }
#' }
setMethod("addItem",signature(object="AffinitiyProfile"),
  function(object,  values, ...){  # ... --> label, id=NULL, pch=20, col=2, values
    li<-list(...) 
     
    if(is.null(li$values)) 
       li$values <- matrix(rep(NA, nrow(object@alternatives), ncol=1))
     
    if(nrow(li$values) != nrow(object@alternatives)){
      stop("Parameter 'values' doesn't match the expectations. The number of rows is not equal to the number of alternatives.")
    }
    
    n<-nrow(object@items)
    if(is.null(n)) n<-0
    if(is.null(li$id)) li$id <- letters[n+1]
    
    if(is.null(li$label)) li$label <- paste("item:",li$id)
      
    if(is.null(li$values)) 
      li$values <- matrix(rep(NA, nrow(object@alternatives), ncol=1))
    
     
    object@items <- rbind(object@items, data.frame(label=li$label, id=li$id, pch=li$pch, col= li$col, x=0, y=0))
    object@values <- cbind(object@values, li$values)
    
    while(nrow(object@values) < nrow(object@alternatives)){
      object@values <- cbind(object@values, t(rep(NA, ncol(object@values))))
    }
    
    validObject(object)
    return (object)
  }
  
)


#' @aliases plot,AffinitiyProfile-method
#' @title plot
#' @rdname plot
#' @param object Object of Type \code{\link{AffinitiyProfile}} 
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{addItem}}}{
#'    \describe{  
#'     \item{log}{\code{logical} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}  
#'     \item{showwarnings}{\code{logical} Optional Parameter. Indicating, if problems should be logged to console. Default is \code{TRUE}.} 
#'     \item{drawcircle}{\code{logical} Optional Parameter. Indicating, if an outer circle should be drawn. Default is \code{FALSE}.}  
#'     \item{circle_color}{\code{numeric} Optional Parameter. Representing the border color of an outer circle. Default is \code{2}.}  
#'     \item{circle_lty}{\code{numeric} Optional Parameter. Representing the line-type of the border of an outer circle. Default is \code{1}.}  
#'     \item{nseg}{\code{numeric} Optional Parameter, representing the number of segments for the outer circle. Default is \code{360}.}  
#'     \item{x.cent}{\code{numeric} Optional Parameter, representing the center of the plot. Default is \code{0}.}  
#'     \item{y.cent}{\code{numeric} Optional Parameter, representing the center of the plot. Default is \code{0}.}   
#'     
#'     \item{xlab}{\code{character} Optional Parameter, representing the x-label of the plot. Default is \code{""}.} 
#'     \item{ylab}{\code{character} Optional Parameter, representing the y-label of the plot. Default is \code{""}.} 
#'     \item{main}{\code{character} Optional Parameter, representing the main-title of the plot. Default is \code{""}.} 
#'     \item{sub}{\code{character} Optional Parameter, representing the sub-title of the plot. Default is \code{""}.} 
#'     
#'     
#'     \item{r}{\code{numeric} Optional Parameter, representing the radius of the outer circle. Default is \code{1}.}  
#'     \item{rot}{\code{numeric} Optional Parameter, representing intial rotation of the plot. Default is \code{pi/2}.}  
#'     \item{xlim}{\code{numeric} Optional Parameter, of length 2. Representing the x-limitis of the plot. Default is calculated automatically.}
#'     \item{ylim}{\code{numeric} Optional Parameter, of length 2. Representing the y-limitis of the plot. Default is calculated automatically.}  
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{  
#'       \item{\code{\link{plot}}} 
#'    }
#' }
setMethod("plot",signature("AffinitiyProfile", "ANY"),
          function(x,y=NULL,  ...){  
            # ... --> label, id=NULL, pch=20, col=2,
            li<-list(...) 
            object <- x
            if(is.null(li$items)) 
              stop("Parameter 'items' not provided.")
            if(ncol(li$items) != nrow(object@alternatives)){
              stop("Parameter 'items' doesn't match the expectations. The number of columns is not equal to the number of alternatives.")
            }
            if(is.null(li$drawcircle)) { li$drawcircle <- FALSE }
            if(is.null(li$circle_color)) { li$circle_color <- 2 }
            if(is.null(li$circle_lty)) { li$circle_lty <- 1 }
            if(is.null(li$nseg)) { li$nseg <- 360 }
            if(is.null(li$x.cent)) { li$x.cent <- 0 }
            if(is.null(li$y.cent)) { li$y.cent <- 0 }
            if(is.null(li$r)) { li$r <- 1 }
            if(is.null(li$rot)) { li$rot <- pi/2 }
            
            if(is.null(li$xlab)) { li$xlab <- "" }
            if(is.null(li$ylab)) { li$ylab <- "" }
            if(is.null(li$main)) { li$main <- "" }
            if(is.null(li$sub)) { li$sub <- "" }
            
            if(is.null(li$xlim)) { li$xlim <- c(-1,1)*li$r + li$x.cent } else{ if(length(li$xlim)!=2) stop("wrong length for parameter xlim")}
            if(is.null(li$ylim)) { li$ylim <- c(-1,1)*li$r + li$y.cent  } else{ if(length(li$ylim)!=2) stop("wrong length for parameter ylim")}
            
            if(is.null(li$alternative_pch)) { li$alternative_pch <- 21 }
            if(is.null(li$alternative_cex)) { li$alternative_cex <- 3 }
            if(is.null(li$alternative_bg)) { li$alternative_bg <- "white" }
            if(is.null(li$alternative_col)) { li$alternative_col <- 1 }
            
            if(is.null(li$alternative_label_cex)) { li$alternative_label_cex <- 0.8 } 
            
            plot(NULL,NULL, 
                 xlim=li$xlim, 
                 ylim=li$ylim, 
                 axes=FALSE, 
                 xlab = li$xlab, 
                 ylab = li$ylab, 
                 asp=1,
                 main=li$main, 
                 sub=li$sub)
            
            
            if(li$drawcircle){              
              lines(li$x.cent + r*cos( seq(0,2*pi, length.out=li$nseg) ),li$y.cent + r*sin( seq(0,2*pi, length.out=li$nseg) ),lty=li$circle_lty,col=li$circle_color)
            }
            
            n<- nrow(object@alternatives)
            bog <- 2*pi/n 
            
            y <- li$r * sin(bog * 1:n + li$rot)
            x <- li$r * cos(bog * 1:n + li$rot)
            object@alternatives$x <- x
            object@alternatives$y <- y
             
            # points(x,y,pch=20) 
            #draw n-eck
            lines(x[c(1:n,1)],y[c(1:n,1)])
            #draw alternatives
            points(x,y, 
                   pch=li$alternative_pch, 
                   cex=li$alternative_cex, 
                   bg=li$alternative_bg,
                   col=li$alternative_col
            ) 
            text(object@alternatives$x,object@alternatives$y, object@alternativen$id, cex= li$alternative_label_cex )
            
            for(i in 1:nrow(object@items)){
              object@items$x[i] <- sum(object@alternatives$x * object$values[i,] / sum(object$values[i,]))
              object@items$y[i] <- sum(object@alternatives$y * object$values[i,] / sum(object$values[i,]))
            }
            
            spread.labels(object@items$x, object@items$y, object@items$id, 0.2 , cex=0.8)
            points(object@items$x, object@items$y, pch=object@items$pch, col=object@items$col)
            
            
            
            return (object)
          }
          
)




