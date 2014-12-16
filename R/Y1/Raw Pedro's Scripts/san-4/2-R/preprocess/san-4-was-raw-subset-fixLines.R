indicesA<-indices[indices[1:length(indices)-1]- indices[2:length(indices)] == -1]
indicesB<-indices[indices[1:length(indices)-1]- indices[2:length(indices)] == -1]+1
lines[indicesA]<-paste(lines[indicesA], lines[indicesB])
lines<-lines[-indicesB]