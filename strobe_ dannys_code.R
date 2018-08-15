install.packages("tidyverse")
install.packages("haven")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("plyr")
install.packages("grid")
install.packages("Gmisc")
install.packages("DiagrammeR")
install.packages("DiagrammeRsvg")
install.packages("rsvg")
## tableone

library(tidyverse)
library(haven)
library(Hmisc)
library(dplyr)
library(plyr)
library(grid)
library(Gmisc)
library(DiagrammeR)
library(rsvg)

a1 <- 'Total available patients\n(n = x)'
b1 <- ''
c1 <- ''
d1 <- 'Included for analysis\n(n = x - y - z)'
e1 <- 'Data linked with\nexternal dataset'
a2 <- ''
b2 <- 'Excluded because of\nexclusion criteria (n = y)'
c2 <- 'Excluded because of\nmissing values (n = z)'
d2 <- ''
e2 <- ''

#Create a node dataframe
ndf <- create_node_df(
  n = 10,
  label = c(a1, b1, c1, d1, e1, #Column 1
            a2, b2, c2, d2, e2), #Column 2
  style = c('solid', 'invis', 'invis', 'solid', 'solid', #Column 1
            'invis', 'solid', 'solid', 'invis', 'invis'), #Column 2
  shape = c('box', 'point', 'point', 'box', 'box', #Column 1 
            'plaintext', 'box', 'box', 'point', 'point'), #Column 2
  width = c(3, 0.001, 0.001, 3, 3, #Column 1
            2, 2.5, 2.5, 0.001, 0.001), #Column 2
  height = c(1, 0.001, 0.001, 1, 1, #Column 1
             1, 1, 1, 0.001, 0.001), #Column 2
  fontsize = c(rep(14, 10)),
  fontname = c(rep('Helvetica', 10)),
  penwidth = 1.5,
  fixedsize = 'true')

#Create an edge dataframe
edf <- create_edge_df(
  from = c(1, 2, 3, 4, #Column 1
           6, 7, 8, 9, #Column 2
           2, 3 #Horizontals
  ),
  to = c(2, 3, 4, 5, #Column 1
         7, 8, 9, 10, #Column 2
         7, 8 #Horizontals
  ),
  arrowhead = c('none', 'none', 'normal', 'normal', #Column 1
                'none', 'none', 'none', 'none', #Column 2
                'normal', 'normal' #Horizontals
  ),
  color = c('black', 'black', 'black', 'black', #Column 1
            '#00000000', '#00000000', '#00000000', '#00000000', #Column 2
            'black', 'black' #Horizontals
  ),
  constraint = c(rep('true', 8), #Columns
                 rep('false', 2) #Horizontals
  )
)

g <- create_graph(ndf,
                  edf,
                  attr_theme = NULL)

#Not run: but to run this in R Studio, uncomment below
render_graph(g)


