\name{athi$kroki} 
\alias{athi$kroki} 
\alias{athi_kroki} 
\title{ Create diagrams using the kroki online service. } 
\description{ 
 This function creates an URL which can be easily embedded into Markdown code for displaying 
  diagrams supported by the online tool at https://kroki.io 
  There is as well an online diagram editor, see here https://niolesk.top/ 
} 
\usage{ athi_kroki(text,filename=NULL,type="ditaa",ext="png",cache=TRUE,plot=FALSE,server="plantuml") } 
\arguments{ 
\item{text}{some diagram code,default: "A --> B" } 
\item{filename}{some input file, either 'text' or 'file' must be given, default: NULL} 
\item{type}{diagram type, supported is 'ditaa', 'graphviz', 'plantuml' and many others, see the kroki website, default: 'ditaa'} 
\item{ext}{file extension, usally 'png', 'svg' or 'pdf', not all extensions support 'svg' or 'pdf', default: 'png'} 
\item{cache}{should the image be cached locally using crc32 digest files in an 'img' folder, default: TRUE} 
\item{plot}{should the image directly plotted, default: FALSE} 
\item{server}{Which server to use, 'kroki' or 'plantuml', default: 'plantuml'} 
} 
\examples{ 
url1=athi$kroki(' 
  digraph g {  
  rankdir="LR"; 
  node[style=filled,fillcolor=beige]; 
  A -> B -> C ;  
}', 
type="graphviz",server="kroki") 
url2=athi$kroki(" 
  +---------+    +--------+ 
  |    AcEEF+--->+   BcFEE+ 
  +---------+    +--------+ 
  ",server="kroki") 
url1 
url2 
} 
 
 
