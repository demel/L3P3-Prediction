findTextInFactor<-function(f,text)
{
  l<-levels(f)[grep(text,levels(f))]
  which(f==l)
}

escapeRegex<-function(text)
{
  # gsub("/[-[\\]\\{\\\\}()*+?.,\\^$|#\\s]/g","\\$&",text )
  t<- gsub("\\[","\\\\[",text, perl=T)
  gsub("\\]","\\\\]",t, perl=T)
}

findTextInCharacter<-function(text,c)
{
 # c[grep(text,c)]
  grep(escapeRegex(text),c)
}



RegExp.escape = function(text) {
  return text.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, "\\$&");
};


#levels(log$id1)[grepl("01",levels(log$id1))]