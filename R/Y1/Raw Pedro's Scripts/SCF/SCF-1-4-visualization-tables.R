# This script takes resources ('resultList$logTable), and event data ('eventTable') from SCF, 
# as generated from script [SCF]/...
# and generates the following output:
#


require(reshape2)
require(plyr)
require(dplyr)


  # folder to load utility functions
utilsPath<- "C:/Users/capelastegui/workspace/OFP/utils"
  # folder to load data
baseDataPath <- "C:/Users/capelastegui/workspace/OFP/SCF/SCF-1/1-Data"
  # Folder to store output
savePath <- file.path(baseDataPath,"4-VisualizationTables")
  # load utility functions
source(file.path(utilsPath,"refactor.R"))
source(file.path(utilsPath,"removeNAs.R"))

  # events_allnodes: table with events(date,node,type) for all system nodes
events_allnodes<-eventTable %.% filter(severity!="BLANK") %.% select (date,node,type) %.% refactor()
  # events_allnodes.filtered: table with a subset of events_allnodes (from 1/2/14 to 16/2/14)
events_allnodes.filtered <- events_allnodes %.% filter(date > dmy(01022014) & date < dmy(16022014)) %.% refactor()
  # events_bynode - list of event tables for each node
events_bynode<-split(event_allnodes_table, event_allnodes_table$node)
  # resources_rawList - not fully parsed resources data, for all nodes
resources_rawList<-resultList$logTable
  #resources_bynode - list of resource tables for each node
resources_bynode<-llply(resources_rawList,
                        function(node)
                        {nodeResources<-
                           llply(names(node), function(resource,node)
                           {res2<-node[[resource]][,2:3];
                            names(res2)<-c("date",resource); res2},
                           node
                           )
                         result<-Reduce(function(d1,d2){join(d1,d2,by="date")},nodeResources)
                         result<-result[complete.cases(result),]
                         result<-result[!duplicated(result$date),]
                         result %.% arrange (date)
                         })

  #resources_allnodes - list of resources for all nodes, long format
resources_allnodes<-ldply(resources_bynode,.id="node") %.% select(date,node,cpu,int,mem,hd) %.% arrange(date)
  # remove path from node names
levels(resources_allnodes$node)<-laply(levels(resources_allnodes$node),function(s){tail(strsplit(s,"/")[[1]],1)})
  # for visualization performance, choose a random subset with only a few nodes
set.seed(1)
resources_allnodes.filtered <- resources_allnodes %.% 
  filter(node %in% c("lbpeuin01.sceu.corp", 
                     "lbpeuin02.sceu.corp", 
                     "lbpeuid01.sceu.corp",
                     "lbpeuid02.sceu.corp")) %.%
  filter(date > dmy(01022014) & date < dmy(16022014)) %.% refactor()
resources_allnodes.filtered <- resources_allnodes.filtered [sample(nrow(resources_allnodes.filtered),4000),] %.% tbl_df() 
  #res.events_allnodes - combined table with events, resources for all nodes
res.events_allnodes<-rbind.fill(resources_allnodes,events_allnodes)
  # need to filter out rows with NAs, 
res.events_allnodes<-removeNAs.df(res.events_allnodes, replace.fac="not.event") %.% 
  mutate(event=type!="not.event") %.%     # and indicate which rows are events/resources
  melt(id=c("date", "node", "type")) %.%  # and melt it into a tall format
  filter(!(variable=="event"&type=="not.event")& !(type!="not.event" & value==0) )  
        # and filter out inconsistencies between resource/event rows

  # for visualization performance, choose a random subset with only a few nodes, 'res.events_allnodes.sample'
set.seed(1)
res.events_allnodes.sample<- res.events_allnodes[sample(nrow(res.events_allnodes),20000),] %.% tbl_df()
  #hack to fix visualization: change event type to numeric variable, with "not_event" = 0.
res.events_allnodes.sample$type <- as.numeric(res.events_allnodes.sample$type)
res.events_allnodes.sample$type[res.events_allnodes.sample$type==2]<-0


  # save output to files
write.table(resources_bynode[[1]], 
            file.path(savePath,"resources-node1.csv") ,row.names=FALSE, quote=FALSE, sep= ",")
write.table(resources_allnodes,
            file.path(savePath,"resources-allnodes.csv"),row.names=FALSE, quote=FALSE, sep= ",")
write.table(resources_allnodes.filtered, 
            file.path(savePath,"resources-allnodes.filtered.csv"),row.names=FALSE, quote=FALSE, sep= ",")
write.table(events_bynode$lbpeuid01.sceu,
            file.path(savePath,"events-node1.csv"), row.names=FALSE, quote=FALSE, sep= ",")
write.table(events_allnodes,
            file.path(savePath,"events-allnodes.csv"),row.names=FALSE, quote=FALSE, sep= ",")
write.table(events_allnodes.filtered,
            file.path(savePath,"events-allnodes.filtered.csv"),row.names=FALSE, quote=FALSE, sep= ",")
write.table(res.events_allnodes,
            file.path(savePath,"res.events-allnodes.csv"),row.names=FALSE, quote=FALSE, sep= ",")
write.table(res.events_allnodes.sample,
            file.path(savePath,"res.events-allnodes.sample.csv"),row.names=FALSE, quote=FALSE, sep= ",")