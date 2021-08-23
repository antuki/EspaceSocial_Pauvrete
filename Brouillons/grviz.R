library(DiagrammeR)
grViz("
digraph boxes_and_circles {
  
  # a 'graph' statement
  graph [overlap = true, fontsize = 20,rankdir = LR,
  bgcolor=transparent, splines=true]
  
   node [shape = circle, width=1.5,
        fontcolor=black, fixedsize=true, fontsize=25]
   subgraph {
    rank = same;
    I [label='Institutio.'];
    M [label='Monétaire'];
    S [label='Subjectif'];
   }
   
   # liens entre facteurs
  edge [shape = circle, alpha=1 color=black style=filled penwidth=1 arrowsize=.75 minlen=2]
  subgraph {
    #rank = same;
  S -> M [label = 1.03 penwidth=1.15  dir=both];
  M -> I [label = 0.92 penwidth=1.15  dir=both];
  S -> I [label = 0.78 penwidth=1.15  dir=both];
  }
  
  node [shape = box, fixedsize = true,width = 3, fontsize=20]
   subgraph {
    #rank = same;
  2 [label='Diff. fin. perçues'];
  1 [label='Sentiment de pauvreté'];
  3 [label='(-) Quintile niveau de vie'];
  4 [label='(-) Revenus locatifs'];
  5 [label='(-) Revenus financiers'];
  6 [label='RSA'];
  7 [label='Chômage'];
  8 [label='APL'];
  9 [label='Hand./Dépend.'];
  10 [label='Bourse étude'];
  11 [label='HLM'];
   }
  
  # loadings
  I->6 [ label = 0.75 penwidth=1.15];
  I->7 [ label = 0.35 penwidth=1.15];
  I->8 [ label = 0.88 penwidth=1.15];
  I->9 [ label = 0.35 penwidth=1.15];
  I->10 [ label = 0.44 penwidth=1.15];
  I->11 [ label = 0.66 penwidth=1.15];
  M->3 [ label = 0.85 penwidth=1.15];
  M->4 [ label = 0.59 penwidth=1.15];
  M->5 [ label = 0.60 penwidth=1.15];
  S->1 [ label = 0.67 penwidth=1.15];
  S->2 [ label = 0.76 penwidth=1.15];
  
  # variances
  edge [shape = circle, alpha=.5 color=gray50 style=dashed penwidth=1.15 arrowsize=.75 minlen=2 dir=both]
1:e -> 1:e [label = 0.55];
  2:e -> 2:e [label = 0.42];
  3:e -> 3:e [label = 0.28];
  4:e -> 4:e [label = 0.65];
  5:e -> 5:e [label = 0.63];
  6:e -> 6:e [label = 0.44];
  7:e -> 7:e [label = 0.88];
  8:e -> 8:e [label = 0.23];
  9:e -> 9:e [label = 0.87];
  10:e -> 10:e [label = 0.81];
  11:e -> 11:e [label = 0.57];
  S -> S [label = 1.00];
  M -> M [label = 1.00];
  I -> I [label = 1.00];

  }
")
