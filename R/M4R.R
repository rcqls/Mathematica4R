# Used when the package load.
# Notice that GC automatically closes .GlobalEnv$.MathLink
init.M <- function(Mathematica=FALSE) {
	if(!exists(".MathLink",envir=.GlobalEnv) || !is_active.MLINK(.GlobalEnv$.MathLink)) {
		if(Mathematica)
			.GlobalEnv$.MathLink <- MLINK(opts="-linkname 'Mathematica -mathlink -nogui --server'")
		else
		 	.GlobalEnv$.MathLink <- MLINK()
	}
}

#############################################################################
## The two main Mathematica functions: M gets converted results when m output the result as text
M <- function(...,text=NULL,text.output=FALSE) {
	exprs<-substitute(list(...))
	res <- NULL
	if(!is.null(text)) {
		res <-eval.M(text)
	}
	if(length(exprs)>1) {
		nexprs <- names(exprs)
		.GlobalEnv$.simplify.M<-list()
		for(i in seq(exprs)[-1]) {
			exprs[[i]]<-simplify.M(exprs[[i]],nexprs[i])
		}
		names(exprs) <- ""
		# => exprs contains the list of expression to evaluate inside M

		# First 
		if(length(.GlobalEnv$.simplify.M)) {
			tmp<-names(.GlobalEnv$.simplify.M)
			for(i in seq(.GlobalEnv$.simplify.M)) put_vector.M(tmp[[i]],eval(parse(text=.GlobalEnv$.simplify.M[[i]])),TRUE)
		}
		for(i in seq(exprs)[-1]) {
			instrM <- deparse(exprs[[i]])
			#print(instrM)
			res <- eval.M(instrM,text.output)
		}
	}  
	if(is.null(res) || res=="Null") invisible(res) else res
}

m <- function(...,text.output=TRUE) M(...,text.output=text.output)

#############################################################################
## Forthe two next functions expr is a string containing a general M expression.
## eval.M(...) is equivalent of M(text=...,text.output=TRUE) 
eval.M<-function(exp,text.output=TRUE,set.output=TRUE) {
	put_function.MLINK('EvaluatePacket', 1)
	if(set.output) {
		put_function.MLINK('Set',2)
		put_symbol.MLINK("ROutput")
		text.output <- FALSE
	}
	if(text.output) put_function.MLINK('ToString', 1)
	put_function.MLINK('ToExpression', 1)
	put_string.MLINK(exp)
	end_packet.MLINK()

	## echo the result
	while((pkt <- next_packet.MLINK()) && pkt != RETURNPKT) new_packet.MLINK()
	res <- read_packet.MLINK()

	if(set.output) {
		res <- get_named.M("ROutput")
		if(is.vector(res)) {
			# test for Array
			dims<-eval.M("Dimensions[ROutput]",set.output=FALSE,text.output=FALSE)
			if(length(dims>1)) res <- aperm(array(res,dim=rev(dims),rev(seq(dims))))
		}
	}
	
	res

}

## equivalent of M(text=...)
get.M <- function(expr) {
	eval.M(expr,FALSE) # FALSE for expression output
}
#############################################################################
#   Two functions to transfer vectors between R and M
# 	mode.R allows the user to compute R[...] expressions
#############################################################################
put_vector.M <- function(nm,x,mode.R=FALSE) {
	put_function.MLINK('EvaluatePacket', 1)
	put_function.MLINK('Set', 2)
	if(mode.R) {
		put_function.MLINK('R', 1)
		put_string.MLINK(nm)
	} else put_symbol.MLINK(nm)

	if(is.integer(x)) {
		#OLD: for(e in x) put_integer.MLINK(e)
		if(length(dim(x))>1) put_integer32Array.MLINK(x) else put_integer32List.MLINK(x) 
	} else if(is.numeric(x)) {
		#OLD: for(e in x) put_real.MLINK(e)
		if(length(dim(x))>1) put_real64Array.MLINK(x) else put_real64List.MLINK(x) 
	} else {
		if(length(dim(x))>1) put_stringArray.MLINK(x) else put_stringList.MLINK(x)
	}
	end_packet.MLINK()
	## echo the result (todo: option to skip the packet) 
	while((pkt <- next_packet.MLINK()) && pkt != RETURNPKT) {
  		new_packet.MLINK()
  	}
  	read_packet.MLINK()
}

# nm is a symbol in M
get_named.M <-function(nm,mode.R=FALSE) {
	put_function.MLINK('EvaluatePacket', 1)
	if(mode.R) {
		put_function.MLINK('R', 1)
		put_string.MLINK(nm)
	} else put_symbol.MLINK(nm)
	end_packet.MLINK()
	## echo the result
	while((pkt <- next_packet.MLINK()) && pkt != RETURNPKT) {
  		new_packet.MLINK()
  	}
  	read_packet.MLINK()
}

#############################################################################
simplify.M <- function(expr,nexpr) {
	if(!is.null(nexpr) && nchar(nexpr)>0) {
		#equivalent: expr <- parse(text=paste("Set[",nexpr,",",deparse(simplify2.M(expr)),"]",sep=""))[[1]]
		expr <- as.call(unlist(c(as.name("["),as.name("Set"),as.name(nexpr),simplify2.M(expr))))
	}
	simplify2.M(expr)
}

simplify2.M<-function(e) {
#print(e);print(class(e));print(as.list(e))
	if(length(e)>1) {
	    if(e[[1]]==as.name(":=")) return(as.call(unlist(c(as.name("["),as.name("SetDelayed"),e[[2]],simplify2.M(e[[3]])))))
	 	if(e[[1]]==as.name("[") && (as.character(e[[2]]) %in% c("R","r"))) {
	 		instrR <- deparse(e[[3]])
			e[[3]]<-paste("'",deparse(e[[3]]),"'",sep="")
			.GlobalEnv$.simplify.M[[e[[3]]]]<- instrR #<-c(.GlobalEnv$.simplify.M,e)
			return(e)
		}
		return(as.call(unlist(sapply(e,simplify2.M))))
		#return(e)
	} else  return(e)
}

put_token.MLINK <- function(arg) {
	if(is.integer(arg)) put_integer.MLINK(arg)
	else if(is.numeric(arg)) put_real.MLINK(arg)
	else if(is.character(arg)) {
	  if(all(substr(rep(arg,2),c(1,nchar(arg))->tmp,tmp)=='"'))
		put_string.MLINK(substr(arg,2,tmp[2]-1)) 
	  else
		put_symbol.MLINK(arg)
	}
	else
	  stop(paste("cannot put given token ",arg))
}

send_packet.MLINK<-function(list) {

	header <- list[[1]]
	list <- list[-1]
	put_function.MLINK(header, length(list))

	for( arg in list) {
	  	if(length(arg)>1) {
	  		send_packet.MLINK( arg) 
			next
		}
		put_token.MLINK(arg)
	}
		
}

read_packet.MLINK <-function() {
	token <- get_next.MLINK()
	result <- NULL
	#DEBUG:print(c("token",token))
	if(token==MLTKREAL) result <- get_real.MLINK()
	else if(token == MLTKINT) result <- get_integer.MLINK()
	else if(token == MLTKSTR) result <- get_string.MLINK()
	else if(token == MLTKSYM) result <- as.name(get_symbol.MLINK())
	else if(token == MLTKFUNC) {
		tmp <- get_function.MLINK()
	  	name <- tmp[[1]];nargs <- tmp[[2]]
	  	#DEBUG:print(c(name,nargs))
	  	switch(name,
	  		List={
				result <- list()
				for(i in 1:nargs) result <- c(result,read_packet.MLINK())
				if(all(sapply(result,function(e) length(e)==1 & (is.numeric(e) | is.character(e))))) result <- unlist(result) 
			},
	  		ReturnPacket= {
	  			result <- read_packet.MLINK()
	  		},
	  		{
				result <- list(as.name("["),as.name(name))
		  		for(i in 1:nargs) result <- c(result,read_packet.MLINK())
		  		result <- as.call(unlist(result))
	  		})
	  }
	else
	  stop(paste("Unknown packet type: ",token))
	result
}

# Rmk: not very stable
dispatch.MLINK <- function() {
	  while(packet <- next_packet.MLINK()) {
	  	#DEBUG:print(packet)
		if(packet %in% c(RETURNPKT,MESSAGEPKT,TEXTPKT)) {
			return(read_packet.MLINK())
		}
		#else if(packet==CALLPKT) {
		#  do_callback.MLINK(link)
		#  return read_packet.MLINK(link)
		#} 
		else if(packet==INPUTNAMEPKT) {
		  new_packet.MLINK()
		  next
		} else error(paste("Unknown packet: ",packet))
	  }
}


###################################################################################################
## R is less powerful than M to deal with computable expression without considering formal computation.
## RForm is an attempt to convert M expression in R (when possible).
## COMMENT: Not sure it is very interesting for complex M expression!!!!
addFct.RForm <- function(m,r) {
	if(length(m)>1) for(i in seq(m)) if(missing(r)) addFct.RForm(m[i]) else addFct.RForm(m[i],r[i])
	else {
		if(missing(r)) r<- tolower(m)
		.GlobalEnv$.RForm.M <- c(.GlobalEnv$.RForm.M,m)
		.GlobalEnv$.RForm.R <- c(.GlobalEnv$.RForm.R,r)
	}
}

# addOp.RForm <- function(m,r) {
# 	if(length(m)>1) for(i in seq(m)) if(missing(r)) addOp.RForm(m[i]) else addOp.RForm(m[i],r[i])
# 	else {
# 		if(missing(r)) r<- tolower(m)
# 		.GlobalEnv$.RForm.OpsM <- c(.GlobalEnv$.RForm.OpsM,m)
# 		.GlobalEnv$.RForm.OpsR <- c(.GlobalEnv$.RForm.OpsR,r)
# 	}
# }

init.RForm <- function() {
	if(!exists(".RForm.OpsM",envir=.GlobalEnv)) .GlobalEnv$.RForm.OpsM <- c()
	if(!exists(".RForm.OpsR",envir=.GlobalEnv)) .GlobalEnv$.RForm.OpsR <- c()
	if(!exists(".RForm.M",envir=.GlobalEnv)) .GlobalEnv$.RForm.M <- c()
	if(!exists(".RForm.R",envir=.GlobalEnv)) .GlobalEnv$.RForm.R <- c()
	addFct.RForm(c("Abs","Sign", "Sqrt","Floor", "Ceiling","Round"))
	addFct.RForm(c("Exp", "Log"))
	addFct.RForm(c("Cos","Sin","Tan"))
	addFct.RForm(c("ArgCos","ArgSin","ArgTan"),c("acos","asin","atan"))
	addFct.RForm(c("Cosh","Sinh","Tanh"))
	addFct.RForm(c("ArgCosh","ArgSinh","ArgTanh"),c("acosh","asinh","atanh"))
	addFct.RForm(c("Plus","Times","Substract","Divide","Power","Quotient","Mod"),c("+","*","-","/","^","%/%","%%"))
	addFct.RForm(c("Greater","GreaterEqual","Less","LessEqual","Equal","UnEqual"),c(">",">=","<","<=","==","!="))
	addFct.RForm(c("Not","And","Or","Nand","Nor"),c("!","&","|","&&","||"))
}

RForm <- function(expr) {
	if(length(expr)>1) {
		if(expr[[1]]==as.name("[") && ((i<-match(as.character(expr[[2]]),.GlobalEnv$.RForm.M,nomatch=0))>0)) {
			return(as.call(unlist(c(as.name(.GlobalEnv$.RForm.R[i]),sapply(expr[-(1:2)],RForm)))))
		}
		# if(expr[[1]]==as.name("[") && ((i<-match(as.character(expr[[2]]),.GlobalEnv$.RForm.OpsM,nomatch=0))>0)) {
		# 	return(as.call(unlist(c(as.name(.GlobalEnv$.RForm.R[i]),sapply(expr[-(1:2)],RForm)))))
		# }
		# TODO: And, Or in M  can have more than 2 arguments but not in R 
		return(as.call(unlist(sapply(expr,RForm))))
	} else return(expr)
}

# ‘abs’, ‘sign’, ‘sqrt’,
#               ‘floor’, ‘ceiling’, ‘trunc’,
#               ‘round’, ‘signif’

# ‘exp’, ‘log’, ‘expm1’, ‘log1p’,
#               ‘cos’, ‘sin’, ‘tan’,
#               ‘acos’, ‘asin’, ‘atan’

#               ‘cosh’, ‘sinh’, ‘tanh’,
#               ‘acosh’, ‘asinh’, ‘atanh’

#             • ‘lgamma’, ‘gamma’, ‘digamma’, ‘trigamma’

#             • ‘cumsum’, ‘cumprod’, ‘cummax’, ‘cummin’


# • ‘"+"’, ‘"-"’, ‘"*"’, ‘"/"’, ‘"^"’, ‘"%%"’, ‘"%/%"’

#             • ‘"&"’, ‘"|"’, ‘"!"’

#             • ‘"=="’, ‘"!="’, ‘"<"’, ‘"<="’, ‘">="’, ‘">"’

# ‘Arith’ ‘"+"’, ‘"-"’, ‘"*"’, ‘"^"’, ‘"%%"’, ‘"%/%"’, ‘"/"’

#      ‘Compare’ ‘"=="’, ‘">"’, ‘"<"’, ‘"!="’, ‘"<="’, ‘">="’

#      ‘Logic’ ‘"&"’, ‘"|"’.

#      ‘Ops’ ‘"Arith"’, ‘"Compare"’, ‘"Logic"’

#      ‘Math’ ‘"abs"’, ‘"sign"’, ‘"sqrt"’, ‘"ceiling"’, ‘"floor"’,
#           ‘"trunc"’, ‘"cummax"’, ‘"cummin"’, ‘"cumprod"’, ‘"cumsum"’,
#           ‘"log"’, ‘"log10"’, ‘"log2"’, ‘"log1p"’, ‘"acos"’, ‘"acosh"’,
#           ‘"asin"’, ‘"asinh"’, ‘"atan"’, ‘"atanh"’, ‘"exp"’, ‘"expm1"’,
#           ‘"cos"’, ‘"cosh"’, ‘"sin"’, ‘"sinh"’, ‘"tan"’, ‘"tanh"’,
#           ‘"gamma"’, ‘"lgamma"’, ‘"digamma"’, ‘"trigamma"’

#      ‘Math2’ ‘"round"’, ‘"signif"’

#      ‘Summary’ ‘"max"’, ‘"min"’, ‘"range"’, ‘"prod"’, ‘"sum"’, ‘"any"’,
#           ‘"all"’

#      ‘Complex’ ‘"Arg"’, ‘"Conj"’, ‘"Im"’, ‘"Mod"’, ‘"Re"’


#############################################################################
#    C interface calls
#############################################################################
MLENV <- function() {
	obj <- .External("MLENV_new",package="Mathematica4R")
	class(obj) <- "MLENV"
	obj
}

MLINK <- function(mlenv=MLENV(),opts="-linkname 'math -mathlink'") {
	obj <- .External("MLINK_new",mlenv,opts,package="Mathematica4R")
	attr(obj,"mlenv") <- mlenv
	# load constant only once
	if(!"ML_constants" %in% search()) ML_constants()
	class(obj) <- "MLINK"
	obj
}

free.MLINK <- function(link) {
	.External("MLINK_free",link,package="Mathematica4R")
	free.MLENV(attr(link,"mlenv"))
}

free.MLENV<-function(mlenv) {
	.External("MLENV_free",mlenv,package="Mathematica4R")
}

is_active.MLINK <- function(link) {
	inherits(link,"MLINK") & !.External("ML_is_nil_ptr",attr(link,"mlenv"),package="Mathematica4R") & !.External("ML_is_nil_ptr",link,package="Mathematica4R")
}

ML_constants <- function() {
	ML_constants <- .External("ML_constants",package="Mathematica4R")
	attach(ML_constants)
}

name.MLINK <- function() {
	.External("MLINK_link_name",.GlobalEnv$.MathLink,package="Mathematica4R")
}

activate.MLINK <- function() {
	.External("MLINK_activate",.GlobalEnv$.MathLink,package="Mathematica4R")
}

 
flush.MLINK <- function() {
	invisible(.External("MLINK_flush",.GlobalEnv$.MathLink,package="Mathematica4R"))
}

ready.MLINK <- function() {
	.External("MLINK_ready",.GlobalEnv$.MathLink,package="Mathematica4R")
}
  
put_function.MLINK <- function(name,n) {
	invisible(.External("MLINK_put_function",.GlobalEnv$.MathLink,name,as.integer(n),package="Mathematica4R"))
}

put_integer.MLINK <- function(i) {
	invisible(.External("MLINK_put_integer",.GlobalEnv$.MathLink,as.integer(i),package="Mathematica4R"))
}

put_integer32List.MLINK <- function(i) {
	invisible(.External("MLINK_put_integer32_list",.GlobalEnv$.MathLink,as.integer(i),package="Mathematica4R"))
}

put_integer32Array.MLINK <- function(i) {
	dims <- dim(i)
	invisible(.External("MLINK_put_integer32_array",.GlobalEnv$.MathLink,as.integer(aperm(i,rev(1:length(dims)))),dims,package="Mathematica4R"))
}

put_real.MLINK <- function(num) {
	invisible(.External("MLINK_put_real",.GlobalEnv$.MathLink,as.numeric(num),package="Mathematica4R"))
} 

put_real64List.MLINK <- function(num) {
	invisible(.External("MLINK_put_real64_list",.GlobalEnv$.MathLink,as.numeric(num),package="Mathematica4R"))
} 

put_real64Array.MLINK <- function(num) {
	dims <- dim(num)
	invisible(.External("MLINK_put_real64_array",.GlobalEnv$.MathLink,as.numeric(aperm(num,rev(1:length(dims)))),dims,package="Mathematica4R"))
}
  
put_next.MLINK <- function() {
	invisible(.External("MLINK_put_next",.GlobalEnv$.MathLink,package="Mathematica4R"))
} 

put_string.MLINK <- function(str) {
	invisible(.External("MLINK_put_string",.GlobalEnv$.MathLink,str,package="Mathematica4R"))
} 

put_stringList.MLINK <- function(x) {
	put_function.MLINK('List',length(x))
	for(e in x) put_string.MLINK(as.character(e))
}

put_stringArray.MLINK <- function(x) {
	dims <- dim(x)
	put_function.MLINK('List',dims[1])
	for(i in 1:(dims[1])) {
		x2<-eval(parse(text=paste("x[",i,paste(rep(",",length(dims)-1),collapse=""),"]",sep="")))
		if(is.null(dim(x2))) put_stringList.MLINK(x2) else put_stringArray.MLINK(x2)
	}
}

put_symbol.MLINK <- function(sym) {
	invisible(.External("MLINK_put_symbol",.GlobalEnv$.MathLink,sym,package="Mathematica4R"))
}  
 
get_integer.MLINK <- function() {
	.External("MLINK_get_integer",.GlobalEnv$.MathLink,package="Mathematica4R")
} 

get_real.MLINK <- function() {
	.External("MLINK_get_real",.GlobalEnv$.MathLink,package="Mathematica4R")
}
 
get_arg_count.MLINK <- function() {
	.External("MLINK_get_arg_count",.GlobalEnv$.MathLink,package="Mathematica4R")
} 

check_function.MLINK <- function( name) {
	.External("MLINK_check_function",.GlobalEnv$.MathLink,name,package="Mathematica4R")
} 

end_packet.MLINK <- function() {
	.External("MLINK_end_packet",.GlobalEnv$.MathLink,package="Mathematica4R")
} 

get_next.MLINK <- function() {
	.External("MLINK_get_next",.GlobalEnv$.MathLink,package="Mathematica4R")
}
 
disown_symbol.MLINK <- function(sym) {
	.External("MLINK_disown_symbol",.GlobalEnv$.MathLink,sym,package="Mathematica4R")
} 
 
get_symbol.MLINK <- function() {
	.External("MLINK_get_symbol",.GlobalEnv$.MathLink,package="Mathematica4R")
}
 
get_function.MLINK <- function() {
	.External("MLINK_get_function",.GlobalEnv$.MathLink,package="Mathematica4R")
} 

disown_string.MLINK <- function(str) {
	.External("MLINK_disown_string",.GlobalEnv$.MathLink,str,package="Mathematica4R")
} 

get_string.MLINK <- function() {
	.External("MLINK_get_string",.GlobalEnv$.MathLink,package="Mathematica4R")
}   

next_packet.MLINK <- function() {
	.External("MLINK_next_packet",.GlobalEnv$.MathLink,package="Mathematica4R")
} 

new_packet.MLINK <- function() {
	.External("MLINK_new_packet",.GlobalEnv$.MathLink,package="Mathematica4R")
} 

error.MLINK <- function() {
	.External("MLINK_error",.GlobalEnv$.MathLink,package="Mathematica4R")
} 

error_message.MLINK <- function() {
	.External("MLINK_error_message",.GlobalEnv$.MathLink,package="Mathematica4R")
} 

set_message_handler.MLINK <- function() {
	.External("MLINK_set_message_handler",.GlobalEnv$.MathLink,package="Mathematica4R")
}

# evaluate_string.MLINK <- function(str) {
# 	.External("MLINK_evaluate_string",.GlobalEnv$.MathLink,str,package="Mathematica4R")
# } 


# USED in ruby-mathematica for used-defined call
# do_callback.MLINK <- function(link) {
# 	  func_no <- read_packet.MLINK(link)
# 	  args <- read_packet.MLINK(link)
	  
# 	  if(@func_table[func_no]) {
# 		obj, method = @func_table[func_no]
# 		if(args.kind_of?(Array))
# 		  result = obj.send(method, args)
# 		else
# 		  result = obj.send(method)
# 		end
# 		@link.put_token(result)
# 	  }
	  
# 	  self
# 	end

######################
# SAVE with link arg #
######################
# activate.MLINK <- function(link) {
# 	.External("MLINK_activate",link,package="Mathematica4R")
# }
 
# flush.MLINK <- function(link) {
# 	.External("MLINK_flush",link,package="Mathematica4R")
# }
  
# put_function.MLINK <- function(link,name,n) {
# 	.External("MLINK_put_function",link,name,as.integer(n),package="Mathematica4R")
# }

# put_integer.MLINK <- function(link,i) {
# 	.External("MLINK_put_integer",link,as.integer(i),package="Mathematica4R")
# } 
  
# put_next.MLINK <- function(link) {
# 	.External("MLINK_put_next",link,package="Mathematica4R")
# } 

# put_string.MLINK <- function(link,str) {
# 	.External("MLINK_put_string",link,str,package="Mathematica4R")
# } 

# put_symbol.MLINK <- function(link,sym) {
# 	.External("MLINK_put_symbol",link,sym,package="Mathematica4R")
# }  
 
# get_integer.MLINK <- function(link) {
# 	.External("MLINK_get_integer",link,package="Mathematica4R")
# } 

# get_real.MLINK <- function(link) {
# 	.External("MLINK_get_real",link,package="Mathematica4R")
# }
 
# get_arg_count.MLINK <- function(link) {
# 	.External("MLINK_get_arg_count",link,package="Mathematica4R")
# } 

# check_function.MLINK <- function(link,name) {
# 	.External("MLINK_check_function",link,name,package="Mathematica4R")
# } 

# end_packet.MLINK <- function(link) {
# 	.External("MLINK_end_packet",link,package="Mathematica4R")
# } 

# get_next.MLINK <- function(link) {
# 	.External("MLINK_get_next",link,package="Mathematica4R")
# }
 
# disown_symbol.MLINK <- function(link,sym) {
# 	.External("MLINK_disown_symbol",link,sym,package="Mathematica4R")
# } 
 
# get_symbol.MLINK <- function(link) {
# 	.External("MLINK_get_symbol",link,package="Mathematica4R")
# }
 
# get_function.MLINK <- function(link) {
# 	.External("MLINK_get_function",link,package="Mathematica4R")
# } 

# disown_string.MLINK <- function(link,str) {
# 	.External("MLINK_disown_string",link,str,package="Mathematica4R")
# } 

# get_string.MLINK <- function(link) {
# 	.External("MLINK_get_string",link,package="Mathematica4R")
# }   

# next_packet.MLINK <- function(link) {
# 	.External("MLINK_next_packet",link,package="Mathematica4R")
# } 

# new_packet.MLINK <- function(link) {
# 	.External("MLINK_new_packet",link,package="Mathematica4R")
# } 

# error.MLINK <- function(link) {
# 	.External("MLINK_error",link,package="Mathematica4R")
# } 

# error_message.MLINK <- function(link) {
# 	.External("MLINK_error_message",link,package="Mathematica4R")
# } 

# set_message_handler.MLINK <- function(link) {
# 	.External("MLINK_set_message_handler",link,package="Mathematica4R")
# }

# # evaluate_string.MLINK <- function(link,str) {
# # 	.External("MLINK_evaluate_string",link,str,package="Mathematica4R")
# # } 

# put_token.MLINK <- function(link,arg) {
# 	if(is.integer(arg)) put_integer.MLINK(link,arg)
# 	else if(is.numeric(arg)) put_real.MLINK(link,arg)
# 	else if(is.character(arg)) {
# 	  if(all(substr(rep(arg,2),c(1,nchar(arg))->tmp,tmp)=='"'))
# 		put_string.MLINK(link,substr(arg,2,tmp[2]-1)) 
# 	  else
# 		put_symbol.MLINK(link,arg)
# 	}
# 	else
# 	  stop(paste("cannot put given token ",arg))
# }

# send_packet.MLINK<-function(link,list) {

# 	header <- list[[1]]
# 	list <- list[-1]
# 	put_function.MLINK(link,header, length(list))

# 	for( arg in list) {
# 	  	if(length(arg)>1) {
# 	  		send_packet.MLINK(link, arg) 
# 			next
# 		}
# 		put_token.MLINK(link,arg)
# 	}
		
# }

# read_packet.MLINK <-function(link) {
# 	token <- get_next.MLINK(link)
# 	result <- NULL
# 	#DEBUG:print(c("token",token))
# 	if(token==MLTKREAL) result <- get_real.MLINK(link)
# 	else if(token == MLTKINT) result <- get_integer.MLINK(link)
# 	else if(token == MLTKSTR) result <- get_string.MLINK(link)
# 	else if(token == MLTKSYM) result <- get_symbol.MLINK(link)
# 	else if(token == MLTKFUNC) {
# 		tmp <- get_function.MLINK(link)
# 	  	name <- tmp[[1]];nargs <- tmp[[2]]
# 	  	#DEBUG:print(c(name,nargs))
# 	  	switch(name,
# 	  		List={
# 				result <- list()
# 				for(i in 1:nargs) result <- c(result,read_packet.MLINK(link))
# 			},
# 	  		ReturnPacket= {
# 	  			result <- read_packet.MLINK(link)
# 	  		},
# 	  		{
# 				result <- list(name)
# 		  		for(i in 1:nargs) result <- c(result,read_packet.MLINK(link))
# 	  		})
# 	  }
# 	else
# 	  stop(paste("Unknown packet type: ",token))

# 	result
# }

