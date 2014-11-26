#include <stdio.h>
#include <string.h>

#include "mathlink.h"

#include <Rdefines.h>
#include <R_ext/PrtUtil.h>

#ifndef Win32
#include <R_ext/eventloop.h>
#endif

/*** MathLink constante ***/
#define SetMLConstant(cst) \
  PROTECT(tmp=allocVector(INTSXP,1));\
  INTEGER(tmp)[0]=cst;\
  i++;\
  SET_STRING_ELT(names,i,mkChar(#cst));\
  SET_VECTOR_ELT(constants,i,tmp);\
  UNPROTECT(1)\

SEXP R_MLConstants(void) {
  SEXP constants,names,tmp;
  int i,nbC=89; 

  i=-1;
  PROTECT(constants=allocVector(VECSXP,nbC));
  PROTECT(names=allocVector(STRSXP,nbC));
  SetMLConstant(ILLEGALPKT);
  SetMLConstant(RETURNPKT);
  SetMLConstant(MESSAGEPKT);
  SetMLConstant(TEXTPKT);
  SetMLConstant(CALLPKT);
  SetMLConstant(INPUTNAMEPKT);
  SetMLConstant(RESUMEPKT);
  //SetMLConstant(MLE);
  SetMLConstant(MLEUNKNOWN);
  SetMLConstant(MLEOK);
  SetMLConstant(MLEDEAD);
  SetMLConstant(MLEGBAD);
  SetMLConstant(MLEGSEQ);
  SetMLConstant(MLEPBTK);
  SetMLConstant(MLEPSEQ);
  SetMLConstant(MLEPBIG);
  SetMLConstant(MLEOVFL);
  SetMLConstant(MLEMEM);
  SetMLConstant(MLEACCEPT);
  SetMLConstant(MLECONNECT);
  SetMLConstant(MLECLOSED);
  SetMLConstant(MLEDEPTH);
  SetMLConstant(MLENODUPFCN);
  SetMLConstant(MLENOACK);
  SetMLConstant(MLENODATA);
  SetMLConstant(MLENOTDELIVERED);
  SetMLConstant(MLENOMSG);
  SetMLConstant(MLEFAILED);
  SetMLConstant(MLEGETENDEXPR);
  SetMLConstant(MLEPUTENDPACKET);
  SetMLConstant(MLENEXTPACKET);
  SetMLConstant(MLEUNKNOWNPACKET);
  SetMLConstant(MLEGETENDPACKET);
  SetMLConstant(MLEABORT);
  SetMLConstant(MLEMORE);
  SetMLConstant(MLENEWLIB);
  SetMLConstant(MLEOLDLIB);
  SetMLConstant(MLEBADPARAM);
  SetMLConstant(MLENOTIMPLEMENTED);
  SetMLConstant(MLEINIT);
  SetMLConstant(MLEARGV);
  SetMLConstant(MLEPROTOCOL);
  SetMLConstant(MLEMODE);
  SetMLConstant(MLELAUNCH);
  SetMLConstant(MLELAUNCHAGAIN);
  SetMLConstant(MLELAUNCHSPACE);
  SetMLConstant(MLENOPARENT);
  SetMLConstant(MLENAMETAKEN);
  SetMLConstant(MLENOLISTEN);
  SetMLConstant(MLEBADNAME);
  SetMLConstant(MLEBADHOST);
  SetMLConstant(MLERESOURCE);
  SetMLConstant(MLELAUNCHFAILED);
  SetMLConstant(MLELAUNCHNAME);
  SetMLConstant(MLELAST);
  SetMLConstant(MLETRACEON);
  SetMLConstant(MLETRACEOFF);
  SetMLConstant(MLEDEBUG);
  SetMLConstant(MLEASSERT);
  SetMLConstant(MLEUSER);
  //SetMLConstant(MLTK);
  SetMLConstant(MLTKOLDINT);
  SetMLConstant(MLTKOLDREAL);
  SetMLConstant(MLTKFUNC);
  SetMLConstant(MLTKERROR);
  SetMLConstant(MLTKERR);
  SetMLConstant(MLTKSTR);
  SetMLConstant(MLTKSYM);
  SetMLConstant(MLTKREAL);
  SetMLConstant(MLTKINT);
  SetMLConstant(MLTKPCTEND);
  SetMLConstant(MLTKAPCTEND);
  SetMLConstant(MLTKEND);
  SetMLConstant(MLTKAEND);
  SetMLConstant(MLTKSEND);
  SetMLConstant(MLTKCONT);
  SetMLConstant(MLTKELEN);
  SetMLConstant(MLTKNULL);
  SetMLConstant(MLTKOLDSYM);
  SetMLConstant(MLTKOLDSTR);
  SetMLConstant(MLTKPACKED);
  SetMLConstant(MLTKARRAY);
  SetMLConstant(MLTKDIM);
  SetMLConstant(MLLENGTH_DECODER);
  SetMLConstant(MLTKPACKED_DECODER);
  SetMLConstant(MLTKARRAY_DECODER);
  SetMLConstant(MLTKMODERNCHARS_DECODER);
  SetMLConstant(MLTKNULLSEQUENCE_DECODER);
  SetMLConstant(MLTKALL_DECODERS);
  SetMLConstant(MLTK_FIRSTUSER);
  SetMLConstant(MLTK_LASTUSER);
  setAttrib(constants,R_NamesSymbol,names);
  UNPROTECT(2);
  return constants;
}

#if MLINTERFACE >= 3
#define ML_char const char **
#else
#define ML_char kcharpp_ct
#endif /* MLINTERFACE >= 3 */

void C_MLENV_free(SEXP self) {
  MLENV *env;
  //Rprintf("GC MLENV is in action!\n");
  env=(MLENV*)R_ExternalPtrAddr(self);
  if(env==NULL) return;
  MLDeinitialize(*env);
  R_ClearExternalPtr(self);
}

SEXP R_MLENV_new(void) {
  MLENV *env;
  SEXP self;

  env = Calloc(1,MLENV);
  if(! (*env = MLInitialize(0))) {
    error("Failed to initialize MathLink. -- Failed MLInitialize(0)");
  }
  self=R_MakeExternalPtr(env, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(self,C_MLENV_free,TRUE);
  return self;
}

SEXP R_MLENV_free(SEXP args) {
  C_MLENV_free(CADR(args));
  return R_NilValue;
}
 
void C_MLINK_free(SEXP self) {
  MLINK *link;
  //Rprintf("GC MLINK is in action!\n");
  link=(MLINK*)R_ExternalPtrAddr(self);
  if(link==NULL) return;
  //printf("closing lin\n");
  MLClose(*link);
  //printf("link closed\n");
  R_ClearExternalPtr(self);
}

SEXP R_MLINK_new(SEXP args) {
  MLINK *link;
  MLENV *env;
  SEXP self;
  char* opts;
  #if MLINTERFACE >= 3
  int errno;
#else
  long errno;
#endif /* MLINTERFACE >= 3 */

  env=(MLENV*)R_ExternalPtrAddr(CADR(args));
  if(!isValidString(CADDR(args))) error("invalid options");
  opts = (char*)CHAR(STRING_ELT(CADDR(args), 0));  
  
  link = Calloc(1,MLINK);
  *link = MLOpenString( *env, opts, &errno);
  // if(! *link) {
  //   Rprintf("Failed to open MathLink. %s: errno = %ld", MLErrorString(env, errno), errno);
  //   error("Leaving R ...");
  // }
  self=R_MakeExternalPtr(link, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(self,C_MLINK_free,TRUE);

  return self;
}

SEXP R_MLINK_free(SEXP args) {
  C_MLINK_free(CADR(args));
  return R_NilValue;
}

SEXP R_ML_is_nil_ptr(SEXP args) {
  SEXP ans;

  PROTECT(ans=allocVector(LGLSXP,1));
  if (R_ExternalPtrAddr(CADR(args))==NULL)
    LOGICAL(ans)[0]=TRUE;
  else LOGICAL(ans)[0]=FALSE;
  UNPROTECT(1);
  return ans;
}

SEXP R_MLINK_activate(SEXP args) {
  MLINK *link;
  
  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  MLActivate(*link);
  
  return R_NilValue;
}

SEXP R_MLINK_flush(SEXP args) {
  MLINK *link;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  MLFlush(*link);

  return R_NilValue;
}

SEXP R_MLINK_ready(SEXP args) {
  MLINK *link;
  int result;
  SEXP res;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  result=MLReady(*link);
  PROTECT(res=allocVector(INTSXP,1));
  INTEGER(res)[0]=result;
  UNPROTECT(1);
  return res;
}

 

SEXP R_MLINK_put_function(SEXP args) {
  MLINK *link;
  char* name;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  name = (char*)CHAR(STRING_ELT(CADDR(args), 0));
  MLPutFunction(*link, name,INTEGER(CADDDR(args))[0]);
  return R_NilValue;
}

SEXP R_MLINK_put_integer(SEXP args) {
  MLINK *link;
  int arg;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  arg=INTEGER(CADDR(args))[0];
  //Rprintf("put integer: %d\n",arg);
  MLPutInteger(*link,arg);
  return R_NilValue;
}

SEXP R_MLINK_put_integer32_list(SEXP args) {
  MLINK *link;
  int *arg,n;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  n=length(CADDR(args));
  arg=INTEGER(CADDR(args));
  //Rprintf("put integer: %d\n",arg);
  MLPutInteger32List(*link,arg,n);
  return R_NilValue;
}

SEXP R_MLINK_put_integer32_array(SEXP args) {
  MLINK *link;
  int *dims;
  int *arg,d;


  // Unuseable because M and R puts the vector in symmetric way
  // int *xdims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP));
  //   n = xdims[0]; p = xdims[1];
  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  d=length(CADDDR(args));
  dims = INTEGER(CADDDR(args));//The dim
  //Rprintf("put integer: (%d,%d) %d\n",dims[0],dims[1],d);
  arg=INTEGER(CADDR(args));
  MLPutInteger32Array(*link,arg,(long*)dims,(char**)NULL,(long)d);
  return R_NilValue;
}

SEXP R_MLINK_put_real(SEXP args) {
  MLINK *link;
  double arg;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  arg=REAL(CADDR(args))[0];
  //Rprintf("put double: %lf\n",arg);
  MLPutReal(*link,arg);
  return R_NilValue;
}

SEXP R_MLINK_put_real64_list(SEXP args) {
  MLINK *link;
  int n;
  double *arg;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  n=length(CADDR(args));
  arg=REAL(CADDR(args));
  //Rprintf("put double: %lf\n",arg);
  MLPutReal64List(*link,arg,n);
  return R_NilValue;
}

SEXP R_MLINK_put_real64_array(SEXP args) {
  MLINK *link;
  int *dims,d;
  double *arg;


  // Unuseable because M and R puts the vector in symmetric way
  // int *xdims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP));
  //   n = xdims[0]; p = xdims[1];
  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  d=length(CADDDR(args));
  dims = INTEGER(CADDDR(args));//The dim
  //Rprintf("put integer: (%d,%d) %d\n",dims[0],dims[1],d);
  arg=REAL(CADDR(args));
  MLPutReal64Array(*link,arg,(long*)dims,(char**)NULL,(long)d);
  return R_NilValue;
}

SEXP R_MLINK_put_next(SEXP args) {
  MLINK *link;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));

  MLPutNext(*link,INTEGER(CADDR(args))[0]);
  return R_NilValue;
}

SEXP R_MLINK_put_string(SEXP args) {
  MLINK *link;
  char* name;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  name = (char*)CHAR(STRING_ELT(CADDR(args), 0));
  MLPutString(*link, name);
  return R_NilValue;
}


SEXP R_MLINK_put_symbol(SEXP args) {
  MLINK *link;
  char* name;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  name = (char*)CHAR(STRING_ELT(CADDR(args), 0));
  MLPutSymbol(*link, name);
  return R_NilValue;
}

SEXP R_MLINK_get_integer(SEXP args) {
  MLINK *link;
  int result;
  SEXP res;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  MLGetInteger(*link, &result);
  //Rprintf("get integer: %d\n",result);
  PROTECT(res=allocVector(INTSXP,1));
  INTEGER(res)[0]=result;
  UNPROTECT(1);
  return res;
}

SEXP R_MLINK_get_real(SEXP args) {
  MLINK *link;
  double result;
  SEXP res;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  MLGetReal(*link, &result);
  PROTECT(res=allocVector(REALSXP,1));
  REAL(res)[0]=result;
  UNPROTECT(1);
  return res;
}

SEXP R_MLINK_get_arg_count(SEXP args) {
  MLINK *link;
  int result;
  SEXP res;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  MLGetArgCount(*link, &result);
  PROTECT(res=allocVector(INTSXP,1));
  INTEGER(res)[0]=result;
  UNPROTECT(1);
  return res;
}

SEXP R_MLINK_check_function(SEXP args) {
  MLINK *link;
  char* name;
  long result;
  SEXP res;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  name = (char*)CHAR(STRING_ELT(CADDR(args), 0));
  if(!MLCheckFunction(*link, name,&result)) {
    return R_NilValue;
  }  else {
    PROTECT(res=allocVector(INTSXP,1));
    INTEGER(res)[0]=result;
    UNPROTECT(1);
    return res;
  }
}

SEXP R_MLINK_end_packet(SEXP args) {
  MLINK *link;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  MLEndPacket(*link);

  return R_NilValue;
} 

SEXP R_MLINK_get_next(SEXP args) {
  MLINK *link;
  int result;
  SEXP res;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  result=MLGetNext(*link);
  PROTECT(res=allocVector(INTSXP,1));
  INTEGER(res)[0]=result;
  UNPROTECT(1);
  return res;
}

SEXP R_MLINK_disown_symbol(SEXP args) {
  MLINK *link;
  char* name;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  name = (char*)CHAR(STRING_ELT(CADDR(args), 0));
  MLDisownSymbol(*link, name);
  return R_NilValue;
}


SEXP R_MLINK_get_symbol(SEXP args) {
  MLINK *link;
  char* name;
  SEXP res;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  PROTECT(res=allocVector(STRSXP,1));
  MLGetSymbol(*link, (ML_char) &name);
  if(!name) {
    res=R_NilValue;
  } else {
    SET_STRING_ELT(res,0,mkChar(name));
    MLDisownSymbol(*link, name);
  }
  UNPROTECT(1);
  return res;
}


SEXP R_MLINK_get_function(SEXP args) {
   char *name;
   int n;
   MLINK *link;
   SEXP res,func, num;
  
  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  /* symbol is allocated by Mathematica kernel.
   So we dont need to ALLOC(symbol). */
  MLGetFunction(*link, (ML_char) &name, &n);

  PROTECT(res=allocVector(VECSXP,2));
  if(! name) {
    func = R_NilValue;
    num = R_NilValue;
  } else {
    PROTECT(func=allocVector(STRSXP,1));
    PROTECT(num=allocVector(INTSXP,1));
    SET_STRING_ELT(func,0,mkChar(name)); /* alloc result and memcpy from name */
    MLDisownSymbol(*link, name); /* disown memory allocated by ML */
    INTEGER(num)[0] = n;
    SET_VECTOR_ELT(res,0,func);
    SET_VECTOR_ELT(res,1,num);
    UNPROTECT(2);
  }
  UNPROTECT(1);
  return res;
}


SEXP R_MLINK_disown_string(SEXP args) {
  MLINK *link;
  char* name;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  name = (char*)CHAR(STRING_ELT(CADDR(args), 0));
  MLDisownString(*link, name);
  return R_NilValue;
}

 
SEXP R_MLINK_get_string(SEXP args) {
   char *name;
   MLINK *link;
   SEXP res;
  
  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  /* symbol is allocated by Mathematica kernel.
   So we dont need to ALLOC(symbol). */
  MLGetString(*link, (ML_char) &name);
  //Rprintf("get string %s\n",name);

  if(! name) {
    res = R_NilValue;
  } else {
    PROTECT(res=allocVector(STRSXP,1));
    SET_STRING_ELT(res,0,mkChar(name)); /* alloc result and memcpy from name */
    //MLDisownSymbol(*link, name); /* disown memory allocated by ML */
    UNPROTECT(1);
  }
  return res;
}
 

SEXP R_MLINK_next_packet(SEXP args) {
  MLINK *link;
  int result;
  SEXP res;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  result=MLNextPacket(*link);
  PROTECT(res=allocVector(INTSXP,1));
  INTEGER(res)[0]=result;
  UNPROTECT(1);
  return res;
}

SEXP R_MLINK_new_packet(SEXP args) {
  MLINK *link;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  MLNewPacket(*link);
   
  return R_NilValue;
}

 
SEXP R_MLINK_error(SEXP args) {
  MLINK *link;
  int result;
  SEXP res;

  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  result=MLError(*link);
  if(error) {
    PROTECT(res=allocVector(INTSXP,1));
    INTEGER(res)[0]=result;
    UNPROTECT(1);
  } else res=R_NilValue;
  return res;
}

SEXP R_MLINK_error_message(SEXP args) {
   char *msg;
   MLINK *link;
   SEXP res;
  
  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  /* symbol is allocated by Mathematica kernel.
   So we dont need to ALLOC(symbol). */
  msg=(char*)MLErrorMessage(*link);

  PROTECT(res=allocVector(STRSXP,1));
  SET_STRING_ELT(res,0,mkChar(msg));
  UNPROTECT(1);
   
  return res;
}

SEXP R_MLINK_link_name(SEXP args) {
   const char *name;
   MLINK *link;
   SEXP res;
  
  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  name=(const char*)MLLinkName(*link);

  PROTECT(res=allocVector(STRSXP,1));
  SET_STRING_ELT(res,0,mkChar(name));
  UNPROTECT(1);
   
  return res;
}

void default_handler(mlp, message, n)
   MLINK mlp;
   unsigned long message, n;
{
    switch (message){
    case MLTerminateMessage:
    error("MathLink session is done.");
  case MLInterruptMessage:
    case MLAbortMessage:
    error("MathLink session is abort.");
  default:
    error("MathLink session is default.");
  }
}

 
SEXP R_MLINK_set_message_handler(SEXP args) {
  MLINK *link;
  
  link=(MLINK*)R_ExternalPtrAddr(CADR(args));
  MLSetMessageHandler(*link, default_handler);

  return R_NilValue;
}




// SEXP R_MLINK_evaluate_string(SEXP args) {
//   MLINK *link;
//   char* instr;

//   link=(MLINK*)R_ExternalPtrAddr(CADR(args));
//   instr = (char*)CHAR(STRING_ELT(CADDR(args), 0));
//   MLEvaluateString(*link, instr);

//   return R_NilValue;
// }



/*
int rbIsRVector(VALUE rbobj) {
  VALUE rbobj2;
  int i,n;
  
  if(!rb_obj_is_kind_of(rbobj,rb_cArray)) {
     if(!(rb_obj_is_kind_of(rbobj,rb_cFixnum) || rb_obj_is_kind_of(rbobj,rb_cFloat) || rb_obj_is_kind_of(rbobj,rb_cString) || rb_obj_is_kind_of(rbobj,rb_cTrueClass) || rb_obj_is_kind_of(rbobj,rb_cFalseClass))) 
       return 0;
     rbobj2=rb_ary_new2(1);
     rb_ary_store(rbobj2,0,rbobj);
     rbobj=rbobj2;
  }
  n=RARRAY_LEN(rbobj);
  for(i=0;i<n;i++) {
    rbobj2=rb_ary_entry(rbobj,i);
    if(!(rb_obj_is_kind_of(rbobj2,rb_cFixnum) || rb_obj_is_kind_of(rbobj2,rb_cFloat) || rb_obj_is_kind_of(rbobj2,rb_cString) || rb_obj_is_kind_of(rbobj2,rb_cTrueClass) || rb_obj_is_kind_of(rbobj2,rb_cFalseClass))) {
      return 0;
    }
  }
  return 1;
}


SEXP newRbObj(VALUE rbobj) {
  SEXP ans,class;

  ans=(SEXP)makeRubyObject(rbobj);
  if(rbIsRVector(rbobj)) {
    PROTECT(class=allocVector(STRSXP,2));
    SET_STRING_ELT(class,0, mkChar("rbRVector"));
    SET_STRING_ELT(class,1, mkChar("rbObj"));
  } else {
    PROTECT(class=allocVector(STRSXP,1));
    SET_STRING_ELT(class,0, mkChar("rbObj"));
  }
  //classgets(ans,class);
  SET_CLASS(ans,class);
  UNPROTECT(1);
  return ans;
}


SEXP dotRb(SEXP args)
{
  SEXP ans;
  char *cmd;
  VALUE val; //non utilisé pour l'instant!!!
  int state;

  if(!isValidString(CADR(args)))
    error("invalid argument");
    cmd = (char*)CHAR(STRING_ELT(CADR(args), 0));
//printf("instruction à executer %s\n",cmd);
  val=rb_eval_string_protect(cmd,&state);
  //printf("state ->%d\n",state);
  if(state) {
    //rb_p(state);
    printf("Ruby error (state=%d)!!!\n",state);
    printf("in executing : %s\n",cmd);
    return R_NilValue;
  } else {
    ans=(SEXP)newRbObj(val);
    //ans=(SEXP)rbObj_to_vector((VALUE)val);
    return ans;
  }
}


static VALUE rb4R_require(char *file) {
  rb_require(file);
  return Qnil;
}


SEXP dotRbRequire(SEXP args)
{
  //SEXP ans;
    char *cmd;
    //VALUE val; //non utilisé pour l'instant!!!
    int state=0;

    if(!isValidString(CADR(args)))
	error("invalid argument");
    cmd = (char*)CHAR(STRING_ELT(CADR(args), 0));
    Rprintf("instruction à executer %s\n",cmd);
    rb_protect((VALUE (*)(VALUE))(&rb4R_require),(VALUE)cmd,&state);
    if(state) Rprintf("error in dotRbRequire (state=%d)!!!\n",state);
    //val non converti en RObject pour l'instant!!!
    return R_NilValue;//mkString("ok"); //retour arbitraire
}

// SEXP dotRbLoad(SEXP args)
// {
//   //SEXP ans;
//     char *cmd;
//     //VALUE val; //non utilisé pour l'instant!!!
//     int status;

//     if(!isValidString(CADR(args)))
// 	error("invalid argument");
//     cmd = (char*)CHAR(STRING_ELT(CADR(args), 0));
//     Rprintf("instruction à loader %s\n",cmd);
//     rb_protect ((VALUE (*) ()) rb_load_file, (VALUE) cmd, &status);
//    status = ruby_exec();
//    status = ruby_cleanup(status);
//     //val non converti en RObject pour l'instant!!!
//     return R_NilValue;//mkString("ok"); //retour arbitraire
// }




//convert R Vector in  rbObj
VALUE RVector2rbArray(SEXP vect)
{
  VALUE res;
  char *name;
  int i,n=0;
  Rcomplex cpl;
  VALUE res2; 

  //vect have to be R Vector!!!
  if(!isVector(vect) | isNewList(vect)) return Qnil; 
  n=length(vect);
  if(n>1) {
    res = rb_ary_new2(n);
    switch(TYPEOF(vect)) {
    case REALSXP:
      for(i=0;i<n;i++) {
	      rb_ary_store(res,i,rb_float_new(REAL(vect)[i]));
      }
      break;
    case INTSXP:
      for(i=0;i<n;i++) {
	      rb_ary_store(res,i,INT2FIX(INTEGER(vect)[i]));
      }
      break;
    case LGLSXP:
      for(i=0;i<n;i++) {
	      rb_ary_store(res,i,(INTEGER(vect)[i] ? Qtrue : Qfalse));
      }
      break;
    case STRSXP:
      for(i=0;i<n;i++) {
	      rb_ary_store(res,i,rb_str_new2(CHAR(STRING_ELT(vect,i))));
      }
      break;
    case CPLXSXP:
      rb_require("complex");
      for(i=0;i<n;i++) {
	      cpl=COMPLEX(vect)[i];
	      res2 = rb_eval_string("Complex.new(0,0)");
	      rb_iv_set(res2,"@real",rb_float_new(cpl.r));
	      rb_iv_set(res2,"@image",rb_float_new(cpl.i));
	      rb_ary_store(res,i,res2);
      }
      break;
    }
  } else {
    switch(TYPEOF(vect)) {
    case REALSXP:
      res=rb_float_new(REAL(vect)[0]);
      break;
    case INTSXP:
      res=INT2FIX(INTEGER(vect)[0]);
      break;
    case LGLSXP:
      res=(INTEGER(vect)[0] ? Qtrue : Qfalse);
      break;
    case STRSXP:
      res=rb_str_new2(CHAR(STRING_ELT(vect,0)));
      break;
    case CPLXSXP:
      rb_require("complex");
      cpl=COMPLEX(vect)[0];
      res= rb_eval_string("Complex.new(0,0)");
      rb_iv_set(res,"@real",rb_float_new(cpl.r));
      rb_iv_set(res,"@image",rb_float_new(cpl.i));
      break;
    }
  }
  return res;
}

SEXP rb4R_as_rbRvector(SEXP args)
{
  VALUE val;
  SEXP ans; 
  val=(VALUE)RVector2rbArray(CADR(args));
  ans=(SEXP)newRbObj(val);
  return(ans);
}



//convert rbObj in RVector (assumed to be possible!!!)
SEXP rbArray2RVector(VALUE rbobj)
{
  SEXP ans;
  VALUE arr,class,expr;
  char *name;
  int n,i;
  
  if(!rb_obj_is_kind_of(rbobj,rb_cArray)) {
    if(!(rb_obj_is_kind_of(rbobj,rb_cFixnum) || rb_obj_is_kind_of(rbobj,rb_cFloat) || rb_obj_is_kind_of(rbobj,rb_cString) || rb_obj_is_kind_of(rbobj,rb_cTrueClass) || rb_obj_is_kind_of(rbobj,rb_cFalseClass))) 
      return R_NilValue;
    n=1;
    arr = rb_ary_new2(1);
    rb_ary_push(arr,rbobj);
  } else {
    arr=rbobj;
    n=RARRAY_LEN(rbobj);  
  }

  //Rprintf("n=%d\n",n);

  //TODO : to improve !!!
  class=rb_class_of(rb_ary_entry(arr,0));

  if(class==rb_cFloat) {
    PROTECT(ans=allocVector(REALSXP,n));
    for(i=0;i<n;i++) {
      REAL(ans)[i]=NUM2DBL(rb_ary_entry(arr,i));
    }
  } else if(class==rb_cFixnum || class==rb_cBignum) {
    PROTECT(ans=allocVector(INTSXP,n));
    for(i=0;i<n;i++) {
      INTEGER(ans)[i]=NUM2INT(rb_ary_entry(arr,i));
    }
  } else if(class==rb_cTrueClass || class==rb_cFalseClass) {
    PROTECT(ans=allocVector(LGLSXP,n));
    for(i=0;i<n;i++) {
      LOGICAL(ans)[i]=(rb_class_of(rb_ary_entry(arr,i))==rb_cFalseClass ? FALSE : TRUE);
    }
  } else if(class==rb_cString) {
    PROTECT(ans=allocVector(STRSXP,n));
    for(i=0;i<n;i++) {
      expr = rb_ary_entry(arr,i);
      SET_STRING_ELT(ans,i,mkChar(StringValuePtr(expr)));
    }
  } else ans=R_NilValue;
  UNPROTECT(1);
  return ans; 
}



SEXP rb4R_is_Rvector(SEXP args) {
  SEXP obj,ans;
  VALUE rbobj,rbobj2;
  int vect,i,n;

  obj=CADR(args);
  PROTECT(ans=allocVector(LGLSXP,1));  
  if (!inherits(obj, "rbObj"))  {
    LOGICAL(ans)[0]=FALSE;
    UNPROTECT(1);
    return ans;
  }
  rbobj=(VALUE) R_ExternalPtrAddr(CADR(obj));
  
  if(!rbIsRVector(rbobj)) {
    LOGICAL(ans)[0]=FALSE;
    UNPROTECT(1);
    return ans;
  }
  LOGICAL(ans)[0]=TRUE;
  UNPROTECT(1);
  return ans;
}

//TODO : partially correct : since args is the name of object simple or homogeneous Array!!!
SEXP rb4R_as_Rvector(SEXP args)
{
  SEXP ans;
  VALUE rbobj;

  if (inherits(CADR(args), "rbObj")) {
    rbobj=(VALUE) R_ExternalPtrAddr(CADR(args));
    ans=(SEXP)rbArray2RVector(rbobj);
    return ans; 
  } else return R_NilValue;
}


//Ruby global variable!!!
SEXP rb4R_get_gv(SEXP args) {
  SEXP ans;
  VALUE rbobj;
  char *name;
  
  if(!isValidString(CADR(args)))
    error("invalid argument");
  name = (char*)CHAR(STRING_ELT(CADR(args), 0));
  rbobj=rb_gv_get(name);
  ans=(SEXP) newRbObj(rbobj);
  return ans;
}

SEXP rb4R_set_gv(SEXP args) {
  SEXP vect;
  VALUE rbval;
  char *name;

  if(!isValidString(CADR(args)))
    error("invalid argument");
  name = (char*)CHAR(STRING_ELT(CADR(args), 0));
  vect=CADDR(args);
  rbval=(VALUE)RVector2rbArray(vect);
  rb_gv_set(name,rbval);
  return R_NilValue;
}

static VALUE rbobj_inspect(VALUE rbobj) {
  VALUE expr=rb_inspect(rbobj);
  Rprintf("%s\n",StringValuePtr(expr));
  fflush(stdout);
  return Qnil;
}

//inspect
SEXP rb4R_inspect(SEXP args) {
  VALUE rbobj,rbval;
  int state=0;

  if (inherits(CADR(args), "rbObj")) {
    rbobj=(VALUE) R_ExternalPtrAddr(CADR(args));
    //sometimes, it bugs !!! very strange!!!
    rb_protect(&rbobj_inspect,rbobj,&state);
    if(state) Rprintf("error in rb4R_inspect (state=%d)!!!\n",state);
  }
  return R_NilValue;
  
}


SEXP rb4R_get_iv(SEXP args) {
  SEXP ans;
  VALUE  rbobj;
  char *var;

  if(!inherits(CADR(args),"rbObj")) error("invalid first argument");
  rbobj=(VALUE) R_ExternalPtrAddr(CADR(args));
  if(!isValidString(CADDR(args))) error("invalid second argument");
  var = (char*)CHAR(STRING_ELT(CADDR(args), 0));
  ans=(SEXP) newRbObj(rb_iv_get(rbobj,var));
  return ans;
}

SEXP rb4R_set_iv(SEXP args) {
  SEXP vect;
  VALUE  rbobj,rbval;
  char *var;

  if(!inherits(CADR(args),"rbObj")) error("invalid first argument");
  rbobj=(VALUE) R_ExternalPtrAddr(CADR(args));
  if(!isValidString(CADDR(args))) error("invalid second argument");
  var = (char*)CHAR(STRING_ELT(CADDR(args), 0));
  vect=CADDDR(args);
  rbval=(VALUE)RVector2rbArray(vect);
  rb_iv_set(rbobj,var,rbval);
  return R_NilValue;
}


SEXP rb4R_apply(SEXP args)
{
  SEXP ans;
  char *meth;
  VALUE rbobj,rbargs;
  int state,i,nargs;
  
  nargs=length(args)-1;
  if(nargs<2) error("number of arguments greater than 2!!! ");
  args=CDR(args);
  if(!inherits(CAR(args),"rbObj")) error("invalid first argument");
  rbobj=(VALUE) R_ExternalPtrAddr(CAR(args));
  args=CDR(args);
  if(!isValidString(CAR(args))) error("invalid second argument");
  meth = (char*)CHAR(STRING_ELT(CAR(args), 0));
  nargs=nargs-2;
  rbargs = rb_ary_new2(nargs);
  if(nargs > 0) {
    for(i=0;i<nargs;i++) {
      args=CDR(args);
      if(inherits(CAR(args),"rbObj")) {
	      rb_ary_store(rbargs,i,(VALUE) R_ExternalPtrAddr(CAR(args)));
      } else {
	      rb_ary_store(rbargs,i,RVector2rbArray(CAR(args)));
      }
    } 
  }
  ans=(SEXP) newRbObj(rb_apply(rbobj,rb_intern(meth),rbargs));
  return ans;
  
  //strmeth=CHAR(STRING_ELT(meth, 0));
  //printf("%s.%s executed\n",strobj,strmeth);
  //rb_funcall(rb_gv_get(strobj),rb_intern(strmeth),0); 
  //val non converti en RObject pour l'instant!!!
  //return R_NilValue;//mkString("ok"); //retour arbitraire
}


//UNUSED!!!!
SEXP getListElement(SEXP list, char *str) 
{
  int i;
  SEXP elmt=R_NilValue, names=getAttrib(list,R_NamesSymbol);
  for ( i=0; i< length(list); i++)
    if(strcmp(CHAR(STRING_ELT(names,i)),str)==0) {
      elmt= VECTOR_ELT(list,i);
      break;
    }
  return elmt;
}

*/


#include <R_ext/Rdynload.h>
static const R_CMethodDef cMethods[] = {
  {NULL,NULL,0}
};

static const R_ExternalMethodDef externalMethods[] = {
  {"MLENV_free",(DL_FUNC)&R_MLENV_free,-1},
  {"MLENV_new",(DL_FUNC) &R_MLENV_new,-1},
  {"MLINK_free",(DL_FUNC)&R_MLINK_free,-1},
  {"MLINK_new",(DL_FUNC)&R_MLINK_new,-1},
  {"ML_constants",(DL_FUNC)&R_MLConstants,-1},
  {"MLINK_activate", (DL_FUNC)&R_MLINK_activate, -1},
  {"MLINK_flush", (DL_FUNC)&R_MLINK_flush, -1},
  {"MLINK_ready", (DL_FUNC)&R_MLINK_ready, -1},
  {"MLINK_put_function", (DL_FUNC)&R_MLINK_put_function, -1},
  {"MLINK_put_integer", (DL_FUNC)&R_MLINK_put_integer,-1},
  {"MLINK_put_integer32_list", (DL_FUNC)&R_MLINK_put_integer32_list,-1},
  {"MLINK_put_integer32_array", (DL_FUNC)&R_MLINK_put_integer32_array,-1},
  {"MLINK_put_real", (DL_FUNC)&R_MLINK_put_real,-1},
  {"MLINK_put_real64_list", (DL_FUNC)&R_MLINK_put_real64_list,-1},
  {"MLINK_put_real64_array", (DL_FUNC)&R_MLINK_put_real64_array,-1},
  {"MLINK_put_next", (DL_FUNC)&R_MLINK_put_next, -1},
  {"MLINK_put_string", (DL_FUNC)&R_MLINK_put_string, -1},
  //{"MLINK_put_byte_string", (DL_FUNC)&R_MLINK_put_byte_string,-1},
  {"MLINK_put_symbol", (DL_FUNC)&R_MLINK_put_symbol, -1},
  {"MLINK_get_next", (DL_FUNC)&R_MLINK_get_next, -1},
  {"MLINK_get_symbol", (DL_FUNC)&R_MLINK_get_symbol, -1},
  {"MLINK_disown_symbol", (DL_FUNC)&R_MLINK_disown_symbol, -1},
  {"MLINK_get_string", (DL_FUNC)&R_MLINK_get_string, -1},
  {"MLINK_disown_string", (DL_FUNC)&R_MLINK_disown_string, -1},
  {"MLINK_get_integer", (DL_FUNC)&R_MLINK_get_integer, -1},
  {"MLINK_get_real", (DL_FUNC)&R_MLINK_get_real, -1},
  {"MLINK_get_function", (DL_FUNC)&R_MLINK_get_function, -1},
  {"MLINK_get_arg_count", (DL_FUNC)&R_MLINK_get_arg_count, -1},
  {"MLINK_check_function", (DL_FUNC)&R_MLINK_check_function, -1},
  {"MLINK_new_packet", (DL_FUNC)&R_MLINK_new_packet, -1},
  {"MLINK_next_packet", (DL_FUNC)&R_MLINK_next_packet, -1},
  {"MLINK_end_packet", (DL_FUNC)&R_MLINK_end_packet, -1},
  {"MLINK_error", (DL_FUNC)&R_MLINK_error, -1},
  {"MLINK_error_message", (DL_FUNC)&R_MLINK_error_message, -1},
  {"MLINK_link_name", (DL_FUNC)&R_MLINK_link_name, -1},
  {"MLINK_set_message_handler", (DL_FUNC)&R_MLINK_set_message_handler, -1},
  {"ML_is_nil_ptr", (DL_FUNC)&R_ML_is_nil_ptr,-1},
  //{"MLINK_evaluate_string", (DL_FUNC)&R_MLINK_evaluate_string, -1},
  {NULL,NULL,0}
};

static const R_CallMethodDef callMethods[] = {
  {NULL,NULL,0}
};

void R_init_Mathematica4R(DllInfo *info) {
  R_registerRoutines(info,cMethods,callMethods,NULL,externalMethods);
}

