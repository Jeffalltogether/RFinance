predict.regsubsets =function (object ,newdata ,id ,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata )
  coefi = coef(object, id=id)
  xvars = names(coefi )
  mat[,xvars ]%*% coefi
}