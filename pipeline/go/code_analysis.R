library(CodeDepends)
library(Rgraphviz)

sc <- readScript(here::here("pipeline", "processing.R"))
g <- makeVariableGraph(info = getInputs(sc))
pdf("~/Desktop/processing.pdf")
plot(g)
dev.off()
