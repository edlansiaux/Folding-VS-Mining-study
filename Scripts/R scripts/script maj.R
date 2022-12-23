#GPU
##GPU dataset preparation

GPU <- Profitability_Banano_Curecoin_Doge_folders_
GPU <- GPU[1:18,]
GP1<-GPU[1:9,]
GP2<-GPU[11:16,]
GP3<-GPU[18,]
GPU<-rbind(GP1,GP2,GP3)

coln <- GPU[1,]
coln[3]<-'PPD'
coln[4]<-'Whattomine Mh/s Equivalent (CURE)'
coln[5]<-'GPU TDP/TBP Watts (est)'
coln[7]<-'BANANO coins per 1,000,000 PPD'
coln[8]<-'CURECOIN coins per 1,000,000 PPD'
coln[9]<-'DOGECOIN coins per 1,000,000 PPD (mining)'
coln[10]<-'DOGECOIN coins per 1,000,000 PPD (folding)'
coln[11]<-"BANANO coins per day"
coln[12]<-"CURECOIN coins per day"
coln[13]<-"DOGECOIN coins per day (mining)"
coln[14]<-"DOGECOIN coins per day (folding)"
coln[15]<-"BANANO cost per coin"
coln[16]<-"CURECOIN cost per coin"
coln[17]<-"DOGECOIN cost per coin (mining)"
coln[18]<-"DOGECOIN cost per coin (folding)"
coln[19]<-"BANANO cost per day"
coln[20]<-"CURECOIN cost per day"
coln[21]<-"DOGECOIN cost per day (mining)"
coln[22]<-"DOGECOIN cost per day (folding)"
coln[23]<-"BANANO kWh used per day"
coln[24]<-"CURECOIN kWh used per day"
coln[25]<-"DOGECOIN kWh used per day (mining)"
coln[26]<-"DOGECOIN kWh used per day (folding)"
coln[2]<-"GPU model"
colnames(GPU)<-coln
GPU<-GPU[-1,]
GPU<-GPU[,-1]
## GPU analysis

x<-as.numeric(unlist(GPU[,2]))
y<-as.numeric(unlist(GPU[,3]))
z<-as.numeric(unlist(GPU[,4]))
### Fig 1
GPUs<-as.data.frame(cbind(x,y,z))
m <- lm(z ~ x + y, data = GPUs)
model1<-m
xs <- unique(GPUs$x)
ys <- unique(GPUs$y)
grid <- with(GPUs, expand.grid(xs, ys))
d <- setNames(data.frame(grid), c("x", "y"))
vals <- predict(m, newdata = d)
m <- matrix(vals, nrow = length(unique(d$x)), ncol = length(unique(d$y)))
Fig1 <-  plotly::layout( plotly::plot_ly(GPU, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers", showlegend = FALSE ),title="Figure 1. GPUs characteristics", scene = list(xaxis = list(title = 'PPD'), yaxis = list(title = 'Whattomine Mh/s Equivalent'),zaxis = list(title = 'GPU TDP/TBP Watts')))
Fig1 <- plotly::add_markers(Fig1, size = 5, showlegend = FALSE)
Fig1 <- plotly::add_trace(Fig1, x = xs, y = ys, z = m[,1], type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

### Fig 2

#### Fig 2A
x1<- as.numeric(unlist(c(GPU[,2],GPU[,2],GPU[,2],GPU[,2])))
y1<- as.numeric(unlist(c(GPU[,6],GPU[,7],GPU[,8],GPU[,9])))
z1<- as.numeric(unlist(c(GPU[,4],GPU[,4],GPU[,4],GPU[,4])))
y2<- (unlist(c("BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING")))
xA<- as.numeric(unlist(c(GPU[,2])))
yA<- as.numeric(unlist(c(GPU[,6])))
zA<- as.numeric(unlist(c(GPU[,4])))
yB<- as.numeric(unlist(c(GPU[,7])))
yC<- as.numeric(unlist(c(GPU[,8])))
yD<- as.numeric(unlist(c(GPU[,9])))
GPU2A<-as.data.frame(cbind(x1,y1,z1,y2))
GPU2A1<-as.data.frame(cbind(xA,yA,zA))
GPU2A2<-as.data.frame(cbind(xA,yB,zA))
GPU2A3<-as.data.frame(cbind(xA,yC,zA))
GPU2A4<-as.data.frame(cbind(xA,yD,zA))


m1A <- lm(zA ~ xA + yA, data = GPU2A1)
model2A<-m1A
xs1A <- unique(GPU2A1$xA)
ys1A <- unique(GPU2A1$yA)
grid1A <- with(GPU2A1, expand.grid(xs1A, ys1A))
d1A <- setNames(data.frame(grid1A), c("xA", "yA"))
vals1A <- predict(m1A, newdata = d1A)
m1A <- matrix(vals1A, nrow = length(unique(d1A$xA)), ncol = length(unique(d1A$yA)))
Fig2A <- plotly::layout( plotly::plot_ly(GPU2A, x = ~x1, y = ~y1, z = ~z1, type = "scatter3d", mode = "markers", scene='scene'))
Fig2A <-plotly::add_markers(Fig2A, color= ~y2, type = "scatter3d", mode = "markers", showlegend = FALSE)
Fig2A <- plotly::add_trace(Fig2A, x = xs1A, y = ys1A, z = m1A[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m2A <- lm(zA ~ xA + yB, data = GPU2A2)
model2B<-m2A
xs1B <- unique(GPU2A2$xA)
ys1B <- unique(GPU2A2$yB)
grid1B <- with(GPU2A2, expand.grid(xs1B, ys1B))
d1B <- setNames(data.frame(grid1B), c("xA", "yB"))
vals1B <- predict(m2A, newdata = d1B)
m2A <- matrix(vals1B, nrow = length(unique(d1B$xA)), ncol = length(unique(d1B$yB)))
Fig2A <- plotly::add_trace(Fig2A, x = xs1B, y = ys1B, z = m2A[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))


m3A <- lm(zA ~ xA + yC, data = GPU2A3)
model2C<-m3A
xs1C <- unique(GPU2A3$xA)
ys1C <- unique(GPU2A3$yC)
grid1C <- with(GPU2A3, expand.grid(xs1C, ys1C))
d1C <- setNames(data.frame(grid1C), c("xA", "yC"))
vals1C <- predict(m3A, newdata = d1C)
m3A <- matrix(vals1C, nrow = length(unique(d1C$xA)), ncol = length(unique(d1C$yC)))
Fig2A <- plotly::add_trace(Fig2A, x = xs1C, y = ys1C, z = m3A[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m4A <- lm(zA ~ xA + yD, data = GPU2A4)
model2D<-m4A
xs1D <- unique(GPU2A4$xA)
ys1D <- unique(GPU2A4$yD)
grid1D <- with(GPU2A4, expand.grid(xs1D, ys1D))
d1D <- setNames(data.frame(grid1D), c("xA", "yD"))
vals1D <- predict(m4A, newdata = d1D)
m4A <- matrix(vals1D, nrow = length(unique(d1D$xA)), ncol = length(unique(d1D$yD)))
Fig2A <- plotly::add_trace(Fig2A, x = xs1D, y = ys1D[1], z = m4A[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

####Fig2B
y1<- as.numeric(unlist(c(GPU[,10],GPU[,11],GPU[,12],GPU[,13])))
GPU2B<-as.data.frame(cbind(x1,y1,z1,y2))
xA<- as.numeric(unlist(c(GPU[,2])))
yA<- as.numeric(unlist(c(GPU[,10])))
zA<- as.numeric(unlist(c(GPU[,4])))
yB<- as.numeric(unlist(c(GPU[,11])))
yC<- as.numeric(unlist(c(GPU[,12])))
yD<- as.numeric(unlist(c(GPU[,13])))
GPU2B1<-as.data.frame(cbind(xA,yA,zA))
GPU2B2<-as.data.frame(cbind(xA,yB,zA))
GPU2B3<-as.data.frame(cbind(xA,yC,zA))
GPU2B4<-as.data.frame(cbind(xA,yD,zA))


m1B <- lm(zA ~ xA + yA, data = GPU2B1)
model2Ag<-m1B
xs2A <- unique(GPU2B1$xA)
ys2A <- unique(GPU2B1$yA)
ys2A<-ys2A[-15]
grid2A <- with(GPU2B1, expand.grid(xs2A, ys2A))
d2A <- setNames(data.frame(grid2A), c("xA", "yA"))
vals2A <- predict(m1B, newdata = d2A)
m1B <- matrix(vals2A, nrow = length(unique(d2A$xA)), ncol = length(unique(d1A$yA)))
Fig2B <- plotly::layout( plotly::plot_ly(GPU2B, x = ~x1, y = ~y1, z = ~z1, scene='scene2'))
Fig2B <-plotly::add_markers(Fig2B, color= ~y2, showlegend = FALSE)
Fig2B <- plotly::add_trace(Fig2B, x = xs2A, y = ys2A, z = m1B[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m2B <- lm(zA ~ xA + yB, data = GPU2B2)
model2Bg<-m2B
xs2B <- unique(GPU2B2$xA)
ys2B <- unique(GPU2B2$yB)
grid2B <- with(GPU2B2, expand.grid(xs2B, ys2B))
d2B <- setNames(data.frame(grid2B), c("xA", "yB"))
vals2B <- predict(m2B, newdata = d1B)
m2B <- matrix(vals2B, nrow = length(unique(d2B$xA)), ncol = length(unique(d2B$yB)))
Fig2B <- plotly::add_trace(Fig2B, x = xs2B, y = ys2B, z = m2B[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m3B <- lm(zA ~ xA + yC, data = GPU2B3)
model2Cg<-m3B
xs2C <- unique(GPU2B3$xA)
ys2C <- unique(GPU2B3$yC)
grid2C <- with(GPU2B3, expand.grid(xs2C, ys2C))
d2C <- setNames(data.frame(grid2C), c("xA", "yC"))
vals2C <- predict(m3B, newdata = d2C)
m3B <- matrix(vals2C, nrow = length(unique(d2C$xA)), ncol = length(unique(d2C$yC)))
Fig2B <- plotly::add_trace(Fig2B, x = xs2C, y = ys2C, z = m3B[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m4B <- lm(zA ~ xA + yD, data = GPU2B4)
model2Dg<-m4B
xs2D <- unique(GPU2B4$xA)
ys2D <- unique(GPU2B4$yD)
grid2D <- with(GPU2B4, expand.grid(xs2D, ys2D))
d2D <- setNames(data.frame(grid2D), c("xA", "yD"))
vals2D <- predict(m4B, newdata = d2D)
m4B <- matrix(vals2D, nrow = length(unique(d2D$xA)), ncol = length(unique(d2D$yD)))
Fig2B <- plotly::add_trace(Fig2B, x = xs2D, y = ys2D, z = m4B[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

####Fig2C
y1<- as.numeric(unlist(c(GPU[,14],GPU[,15],GPU[,16],GPU[,17])))
GPU2C<-as.data.frame(cbind(x1,y1,z1,y2))
xA<- as.numeric(unlist(c(GPU[,2])))
yA<- as.numeric(unlist(c(GPU[,14])))
zA<- as.numeric(unlist(c(GPU[,4])))
yB<- as.numeric(unlist(c(GPU[,15])))
yC<- as.numeric(unlist(c(GPU[,16])))
yD<- as.numeric(unlist(c(GPU[,17])))
GPU2C1<-as.data.frame(cbind(xA,yA,zA))
GPU2C2<-as.data.frame(cbind(xA,yB,zA))
GPU2C3<-as.data.frame(cbind(xA,yC,zA))
GPU2C4<-as.data.frame(cbind(xA,yD,zA))

m1C <- lm(zA ~ xA + yA, data = GPU2C1)
model3A<-m1C
xs3A <- unique(GPU2C1$xA)
ys3A <- unique(GPU2C1$yA)
ys3A <- ys3A[-15]
grid3A <- with(GPU2C1, expand.grid(xs3A, ys3A))
d3A <- setNames(data.frame(grid3A), c("xA", "yA"))
vals3A <- predict(m1C, newdata = d3A)
m1C <- matrix(vals3A, nrow = length(unique(d3A$xA)), ncol = length(unique(d3A$yA)))
Fig2C <- plotly::layout( plotly::plot_ly(GPU2C, x = ~x1, y = ~y1, z = ~z1, scene='scene3'))
Fig2C <-plotly::add_markers(Fig2C, color= ~y2, showlegend = FALSE)
Fig2C <- plotly::add_trace(Fig2C, x = xs3A, y = ys3A, z = m1C[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m2C <- lm(zA ~ xA + yB, data = GPU2C2)
model3B <-m2C
xs3B <- unique(GPU2C2$xA)
ys3B <- unique(GPU2C2$yB)
ys3B <- ys3B[-15]
grid3B <- with(GPU2C2, expand.grid(xs3B, ys3B))
d3B <- setNames(data.frame(grid3B), c("xA", "yB"))
vals3B <- predict(m2C, newdata = d3B)
m2C <- matrix(vals3B, nrow = length(unique(d3B$xA)), ncol = length(unique(d3B$yB)))
Fig2C <- plotly::add_trace(Fig2C, x = xs3B, y = ys3B, z = m2C[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m3C <- lm(zA ~ xA + yC, data = GPU2C3)
model3C<-m3C
xs3C <- unique(GPU2C3$xA)
ys3C <- unique(GPU2C3$yC)
ys3C <- ys3C[-15]
grid3C <- with(GPU2C3, expand.grid(xs3C, ys3C))
d3C<- setNames(data.frame(grid3C), c("xA", "yC"))
vals3C <- predict(m3C, newdata = d3C)
m3C <- matrix(vals3C, nrow = length(unique(d3C$xA)), ncol = length(unique(d3C$yC)))
Fig2C <- plotly::add_trace(Fig2C, x = xs3C, y = ys3C, z = m3C[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m4C <- lm(zA ~ xA + yD, data = GPU2C4)
model3D<-m4C
xs3D <- unique(GPU2C4$xA)
ys3D <- unique(GPU2C4$yD)
grid3D <- with(GPU2C4, expand.grid(xs3D, ys3D))
d3D <- setNames(data.frame(grid3D), c("xA", "yD"))
vals3D <- predict(m4C, newdata = d3D)
m4C <- matrix(vals3D, nrow = length(unique(d3D$xA)), ncol = length(unique(d3D$yD)))
Fig2C <- plotly::add_trace(Fig2C, x = xs3D, y = ys3D, z = m4C[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

####Fig2D
y1<- as.numeric(unlist(c(GPU[,18],GPU[,19],GPU[,20],GPU[,21])))
GPU2D<-as.data.frame(cbind(x1,y1,z1,y2))
xA<- as.numeric(unlist(c(GPU[,2])))
yA<- as.numeric(unlist(c(GPU[,18])))
zA<- as.numeric(unlist(c(GPU[,4])))
yB<- as.numeric(unlist(c(GPU[,19])))
yC<- as.numeric(unlist(c(GPU[,20])))
yD<- as.numeric(unlist(c(GPU[,21])))
GPU2D1<-as.data.frame(cbind(xA,yA,zA))
GPU2D2<-as.data.frame(cbind(xA,yB,zA))
GPU2D3<-as.data.frame(cbind(xA,yC,zA))
GPU2D4<-as.data.frame(cbind(xA,yD,zA))

m1D <- lm(zA ~ xA + yA, data = GPU2D1)
model4A<-m1D
xs4A <- unique(GPU2D1$xA)
xs4A <- xs4A[-14]
ys4A <- unique(GPU2D1$yA)
grid4A <- with(GPU2D1, expand.grid(xs4A, ys4A))
d4A <- setNames(data.frame(grid4A), c("xA", "yA"))
vals4A <- predict(m1D, newdata = d4A)
m1D <- matrix(vals4A, nrow = length(unique(d4A$xA)), ncol = length(unique(d4A$yA)))
m1D<- m1D[,-14]
Fig2D <- plotly::layout( plotly::plot_ly(GPU2D, x = ~x1, y = ~y1, z = ~z1, scene='scene4'))
Fig2D <-plotly::add_markers(Fig2D, color= ~y2, showlegend = FALSE)
Fig2D <- plotly::add_trace(Fig2D, x = xs4A, y = ys4A, z = m1D[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m2D <- lm(zA ~ xA + yB, data = GPU2D2)
model4B <-m2D
xs4B <- unique(GPU2D2$xA)
xs4B <- xs4B[-14]
ys4B <- unique(GPU2D2$yB)
grid4B <- with(GPU2D2, expand.grid(xs4B, ys4B))
d4B <- setNames(data.frame(grid4B), c("xA", "yB"))
vals4B <- predict(m2D, newdata = d4B)
m2D <- matrix(vals4B, nrow = length(unique(d4B$xA)), ncol = length(unique(d4B$yB)))
m2D<- m2D[,-14]
Fig2D <- plotly::add_trace(Fig2D, x = xs4B, y = ys4B, z = m2D[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m3D <- lm(zA ~ xA + yC, data = GPU2D3)
model3C<-m3D
xs4C <- unique(GPU2D3$xA)
ys4C <- unique(GPU2D3$yC)
ys4C <- ys4C[-14]
grid4C <- with(GPU2D3, expand.grid(xs4C, ys4C))
d4C<- setNames(data.frame(grid3C), c("xA", "yC"))
vals4C <- predict(m3D, newdata = d3C)
m3D <- matrix(vals4C, nrow = length(unique(d3C$xA)), ncol = length(unique(d3C$yC)))
Fig2D <- plotly::add_trace(Fig2D, x = xs4C, y = ys4C, z = m3D[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m4D <- lm(zA ~ xA + yD, data = GPU2D4)
model4D<-m4D
xs4D <- unique(GPU2D4$xA)
ys4D <- unique(GPU2D4$yD)
grid4D <- with(GPU2D4, expand.grid(xs4D, ys4D))
d4D <- setNames(data.frame(grid4D), c("xA", "yD"))
vals4D <- predict(m4D, newdata = d4D)
m4D <- matrix(vals4D, nrow = length(unique(d4D$xA)), ncol = length(unique(d4D$yD)))
Fig2D <- plotly::add_trace(Fig2D, x = xs4D, y = ys4D, z = m4D[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

###Fig2 final layout
annotations = list(
  list(
    x = 0.2,
    y = 1.0,
    text = "2.A. Coins per 1,000,000 PPD",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 1,
    text = "2.B. Coins per day",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.2,
    y = 0.45,
    text = "2.C. Cost per coin ",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 0.45,
    text = "2.D. Cost per day",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ))


Fig2<-plotly::layout(plotly::subplot(Fig2A,Fig2B,Fig2C,Fig2D), annotations=annotations, title='Figure 2. Studied variables according to GPUs features',
                     scene = list(domain=list(x=c(0,0.5),y=c(0.5,1)),
                                  xaxis = list(gridcolor='rgb(255, 255, 255)',
                                               zerolinecolor='rgb(255, 255, 255)',
                                               showbackground=TRUE,
                                               backgroundcolor='rgb(230, 230,230)', title = 'PPD'), yaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                 zerolinecolor='rgb(255, 255, 255)',
                                                                                                                 showbackground=TRUE,
                                                                                                                 backgroundcolor='rgb(230, 230,230)', title = 'Coins per 1,000,000 PPD'),zaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                                                                                                      zerolinecolor='rgb(255, 255, 255)',
                                                                                                                                                                                                      showbackground=TRUE,
                                                                                                                                                                                                      backgroundcolor='rgb(230, 230,230)', title = 'GPU TDP/TBP Watts'),
                                  aspectmode='cube'),
                     scene2 = list(domain=list(x=c(0.5,1),y=c(0.5,1)),
                                   xaxis = list(gridcolor='rgb(255, 255, 255)',
                                                zerolinecolor='rgb(255, 255, 255)',
                                                showbackground=TRUE,
                                                backgroundcolor='rgb(230, 230,230)', title = 'PPD'), yaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                  zerolinecolor='rgb(255, 255, 255)',
                                                                                                                  showbackground=TRUE,
                                                                                                                  backgroundcolor='rgb(230, 230,230)', title = 'Coins per day'),zaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                                                                                             zerolinecolor='rgb(255, 255, 255)',
                                                                                                                                                                                             showbackground=TRUE,
                                                                                                                                                                                             backgroundcolor='rgb(230, 230,230)', title = 'GPU TDP/TBP Watts'),
                                   aspectmode='cube'),
                     scene3 = list(domain=list(x=c(0,0.5),y=c(0,0.5)),
                                   xaxis = list(gridcolor='rgb(255, 255, 255)',
                                                zerolinecolor='rgb(255, 255, 255)',
                                                showbackground=TRUE,
                                                backgroundcolor='rgb(230, 230,230)', title = 'PPD'), yaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                  zerolinecolor='rgb(255, 255, 255)',
                                                                                                                  showbackground=TRUE,
                                                                                                                  backgroundcolor='rgb(230, 230,230)', title = 'Cost per coin'),zaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                                                                                             zerolinecolor='rgb(255, 255, 255)',
                                                                                                                                                                                             showbackground=TRUE,
                                                                                                                                                                                             backgroundcolor='rgb(230, 230,230)', title = 'GPU TDP/TBP Watts'),
                                   aspectmode='cube'),
                     scene4 = list(domain=list(x=c(0.5,1),y=c(0,0.5)),
                                   xaxis = list(gridcolor='rgb(255, 255, 255)',
                                                zerolinecolor='rgb(255, 255, 255)',
                                                showbackground=TRUE,
                                                backgroundcolor='rgb(230, 230,230)', title = 'PPD'), yaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                  zerolinecolor='rgb(255, 255, 255)',
                                                                                                                  showbackground=TRUE,
                                                                                                                  backgroundcolor='rgb(230, 230,230)', title = 'Cost per day'),zaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                                                                                            zerolinecolor='rgb(255, 255, 255)',
                                                                                                                                                                                            showbackground=TRUE,
                                                                                                                                                                                            backgroundcolor='rgb(230, 230,230)', title = 'GPU TDP/TBP Watts'),
                                   aspectmode='cube'))


Fig2
#CPU
CPU <- Profitability_Banano_Curecoin_Doge_folders_2
CPU <- CPU[1:13,]
CP1<-CPU[1:7,]
CP2<-CPU[9:13,]
CPU<-rbind(CP1,CP2)
coln <- CPU[1,]
coln[3]<-'PPD'
coln[4]<-'Whattomine Mh/s Equivalent (CURE)'
coln[5]<-'CPU TDP/TBP Watts (est)'
coln[7]<-'BANANO coins per 1,000,000 PPD'
coln[8]<-'CURECOIN coins per 1,000,000 PPD'
coln[9]<-'DOGECOIN coins per 1,000,000 PPD (mining)'
coln[10]<-'DOGECOIN coins per 1,000,000 PPD (folding)'
coln[11]<-"BANANO coins per day"
coln[12]<-"CURECOIN coins per day"
coln[13]<-"DOGECOIN coins per day (mining)"
coln[14]<-"DOGECOIN coins per day (folding)"
coln[15]<-"BANANO cost per coin"
coln[16]<-"CURECOIN cost per coin"
coln[17]<-"DOGECOIN cost per coin (mining)"
coln[18]<-"DOGECOIN cost per coin (folding)"
coln[19]<-"BANANO cost per day"
coln[20]<-"CURECOIN cost per day"
coln[21]<-"DOGECOIN cost per day (mining)"
coln[22]<-"DOGECOIN cost per day (folding)"
coln[23]<-"BANANO kWh used per day"
coln[24]<-"CURECOIN kWh used per day"
coln[25]<-"DOGECOIN kWh used per day (mining)"
coln[26]<-"DOGECOIN kWh used per day (folding)"
coln[2]<-"CPU model"
colnames(CPU)<-coln
CPU<-CPU[-1,]
CPU<-CPU[,-1]
CPU<-CPU[-1,]
## CPU analysis
x<-as.numeric(unlist(CPU[,2]))
y<-as.numeric(unlist(CPU[,3]))
z<-as.numeric(unlist(CPU[,4]))
### Fig 3
CPUs<-as.data.frame(cbind(x,y,z))
m <- lm(z ~ x + y, data = CPUs)
xs <- unique(CPUs$x)
ys <- unique(CPUs$y)
grid <- with(CPUs, expand.grid(xs, ys))
d <- setNames(data.frame(grid), c("x", "y"))
vals <- predict(m, newdata = d)
m <- matrix(vals, nrow = length(unique(d$x)), ncol = length(unique(d$y)))

Fig3 <- plotly::layout( plotly::plot_ly(CPU, x = ~x, y = ~y, z = ~z), title="Figure 3. CPUs characteristics", scene = list(xaxis = list(title = 'PPD'), yaxis = list(title = 'Whattomine Mh/s Equivalent'),zaxis = list(title = 'GPU TDP/TBP Watts')))
Fig3 <- plotly::add_markers(Fig3, size = 5, showlegend = FALSE)
Fig3 <- plotly::add_trace(Fig3, x = xs, y = ys, z = m[,1], type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))
model3<-m
x1<- as.numeric(unlist(c(CPU[,2],CPU[,2],CPU[,2],CPU[,2])))
y1<- as.numeric(unlist(c(CPU[,6],CPU[,7],CPU[,8],CPU[,9])))
z1<- as.numeric(unlist(c(CPU[,4],CPU[,4],CPU[,4],CPU[,4])))
y2<- (unlist(c("BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","BANANO","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","CURECOIN","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE MINING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING","DOGE FOLDING")))
### Fig 4
#### Fig 4A
xA<- as.numeric(unlist(c(CPU[,2])))
yA<- as.numeric(unlist(c(CPU[,6])))
zA<- as.numeric(unlist(c(CPU[,4])))
yB<- as.numeric(unlist(c(CPU[,7])))
yC<- as.numeric(unlist(c(CPU[,8])))
yD<- as.numeric(unlist(c(CPU[,9])))
CPU2A<-as.data.frame(cbind(x1,y1,z1,y2))
CPU2A1<-as.data.frame(cbind(xA,yA,zA))
CPU2A2<-as.data.frame(cbind(xA,yB,zA))
CPU2A3<-as.data.frame(cbind(xA,yC,zA))
CPU2A4<-as.data.frame(cbind(xA,yD,zA))


m1AB <- lm(zA ~ xA + yA, data = CPU2A1)
model2ACPU<-m1AB
xs1A <- unique(CPU2A1$xA)
ys1A <- unique(CPU2A1$yA)
grid1A <- with(CPU2A1, expand.grid(xs1A, ys1A))
d1A <- setNames(data.frame(grid1A), c("xA", "yA"))
vals1A <- predict(m1AB, newdata = d1A)
m1AB <- matrix(vals1A, nrow = length(unique(d1A$xA)), ncol = length(unique(d1A$yA)))
Fig4A <- plotly::layout( plotly::plot_ly(CPU2A, x = ~x1, y = ~y1, z = ~z1, type = "scatter3d", mode = "markers", scene='scene'))
Fig4A <-plotly::add_markers(Fig4A, color= ~y2, type = "scatter3d", mode = "markers", showlegend = FALSE)
Fig4A <- plotly::add_trace(Fig4A, x = xs1A, y = ys1A, z = m1AB[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m2AB <- lm(zA ~ xA + yB, data = CPU2A2)
model2BCPU<-m2AB
xs1B <- unique(CPU2A2$xA)
ys1B <- unique(CPU2A2$yB)
grid1B <- with(CPU2A2, expand.grid(xs1B, ys1B))
d1B <- setNames(data.frame(grid1B), c("xA", "yB"))
vals1B <- predict(m2AB, newdata = d1B)
m2AB <- matrix(vals1B, nrow = length(unique(d1B$xA)), ncol = length(unique(d1B$yB)))
Fig4A <- plotly::add_trace(Fig4A, x = xs1B, y = ys1B, z = m2AB[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))


m3AB <- lm(zA ~ xA + yC, data = CPU2A3)
model2CCPU<-m3AB
xs1C <- unique(CPU2A3$xA)
ys1C <- unique(CPU2A3$yC)
grid1C <- with(CPU2A3, expand.grid(xs1C, ys1C))
d1C <- setNames(data.frame(grid1C), c("xA", "yC"))
vals1C <- predict(m3AB, newdata = d1C)
m3AB <- matrix(vals1C, nrow = length(unique(d1C$xA)), ncol = length(unique(d1C$yC)))
Fig4A <- plotly::add_trace(Fig4A, x = xs1C, y = ys1C, z = m3AB[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m4AB <- lm(zA ~ xA + yD, data = CPU2A4)
model2DCPU<-m4AB
xs1D <- unique(CPU2A4$xA)
ys1D <- unique(CPU2A4$yD)
grid1D <- with(CPU2A4, expand.grid(xs1D, ys1D))
d1D <- setNames(data.frame(grid1D), c("xA", "yD"))
vals1D <- predict(m4AB, newdata = d1D)
m4AB <- matrix(vals1D, nrow = length(unique(d1D$xA)), ncol = length(unique(d1D$yD)))
Fig4A <- plotly::add_trace(Fig4A, x = xs1D, y = ys1D[1], z = m4AB[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

####Fig2B
y1<- as.numeric(unlist(c(CPU[,10],CPU[,11],CPU[,12],CPU[,13])))
CPU2B<-as.data.frame(cbind(x1,y1,z1,y2))
xA<- as.numeric(unlist(c(CPU[,2])))
yA<- as.numeric(unlist(c(CPU[,10])))
zA<- as.numeric(unlist(c(CPU[,4])))
yB<- as.numeric(unlist(c(CPU[,11])))
yC<- as.numeric(unlist(c(CPU[,12])))
yD<- as.numeric(unlist(c(CPU[,13])))
CPU2B1<-as.data.frame(cbind(xA,yA,zA))
CPU2B2<-as.data.frame(cbind(xA,yB,zA))
CPU2B3<-as.data.frame(cbind(xA,yC,zA))
CPU2B4<-as.data.frame(cbind(xA,yD,zA))


m1B2 <- lm(zA ~ xA + yA, data = CPU2B1)
model2AB<-m1B2
xs2A <- unique(CPU2B1$xA)
ys2A <- unique(CPU2B1$yA)
ys2A<-ys2A[-15]
grid2A <- with(CPU2B1, expand.grid(xs2A, ys2A))
d2A <- setNames(data.frame(grid2A), c("xA", "yA"))
vals2A <- predict(m1B2, newdata = d2A)
m1B2 <- matrix(vals2A, nrow = length(unique(d2A$xA)), ncol = length(unique(d1A$yA)))
Fig4B <- plotly::layout( plotly::plot_ly(CPU2B, x = ~x1, y = ~y1, z = ~z1, scene='scene2'))
Fig4B <-plotly::add_markers(Fig4B, color= ~y2, showlegend = FALSE)
Fig4B <- plotly::add_trace(Fig4B, x = xs2A, y = ys2A, z = m1B2[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m2B2 <- lm(zA ~ xA + yB, data = CPU2B2)
model2BB<-m2B2
xs2B <- unique(CPU2B2$xA)
ys2B <- unique(CPU2B2$yB)
grid2B <- with(CPU2B2, expand.grid(xs2B, ys2B))
d2B <- setNames(data.frame(grid2B), c("xA", "yB"))
vals2B <- predict(m2B2, newdata = d1B)
m2B2 <- matrix(vals2B, nrow = length(unique(d2B$xA)), ncol = length(unique(d2B$yB)))
Fig4B <- plotly::add_trace(Fig4B, x = xs2B, y = ys2B, z = m2B2[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m3B2 <- lm(zA ~ xA + yC, data = CPU2B3)
model2C<-m3B2
xs2C <- unique(CPU2B3$xA)
ys2C <- unique(CPU2B3$yC)
grid2C <- with(CPU2B3, expand.grid(xs2C, ys2C))
d2C <- setNames(data.frame(grid2C), c("xA", "yC"))
vals2C <- predict(m3B2, newdata = d2C)
m3B2 <- matrix(vals2C, nrow = length(unique(d2C$xA)), ncol = length(unique(d2C$yC)))
Fig4B <- plotly::add_trace(Fig4B, x = xs2C, y = ys2C, z = m3B2[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m4B2 <- lm(zA ~ xA + yD, data = CPU2B4)
model2D<-m4B2
xs2D <- unique(CPU2B4$xA)
ys2D <- unique(CPU2B4$yD)
grid2D <- with(CPU2B4, expand.grid(xs2D, ys2D))
d2D <- setNames(data.frame(grid2D), c("xA", "yD"))
vals2D <- predict(m4B2, newdata = d2D)
m4B2 <- matrix(vals2D, nrow = length(unique(d2D$xA)), ncol = length(unique(d2D$yD)))
Fig4B <- plotly::add_trace(Fig4B, x = xs2D, y = ys2D, z = m4B2[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

####Fig2C
y1<- as.numeric(unlist(c(CPU[,14],CPU[,15],CPU[,16],CPU[,17])))
CPU2C<-as.data.frame(cbind(x1,y1,z1,y2))
xA<- as.numeric(unlist(c(CPU[,2])))
yA<- as.numeric(unlist(c(CPU[,14])))
zA<- as.numeric(unlist(c(CPU[,4])))
yB<- as.numeric(unlist(c(CPU[,15])))
yC<- as.numeric(unlist(c(CPU[,16])))
yD<- as.numeric(unlist(c(CPU[,17])))
CPU2C1<-as.data.frame(cbind(xA,yA,zA))
CPU2C2<-as.data.frame(cbind(xA,yB,zA))
CPU2C3<-as.data.frame(cbind(xA,yC,zA))
CPU2C4<-as.data.frame(cbind(xA,yD,zA))

m1C <- lm(zA ~ xA + yA, data = CPU2C1)
model3A<-m1C
xs3A <- unique(CPU2C1$xA)
ys3A <- unique(CPU2C1$yA)
ys3A <- ys3A[-15]
grid3A <- with(CPU2C1, expand.grid(xs3A, ys3A))
d3A <- setNames(data.frame(grid3A), c("xA", "yA"))
vals3A <- predict(m1C, newdata = d3A)
m1C <- matrix(vals3A, nrow = length(unique(d3A$xA)), ncol = length(unique(d3A$yA)))
Fig4C <- plotly::layout( plotly::plot_ly(CPU2C, x = ~x1, y = ~y1, z = ~z1, scene='scene3'))
Fig4C <-plotly::add_markers(Fig4C, color= ~y2, showlegend = FALSE)
Fig4C <- plotly::add_trace(Fig4C, x = xs3A, y = ys3A, z = m1C[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m2C <- lm(zA ~ xA + yB, data = CPU2C2)
model3B <-m2C
xs3B <- unique(CPU2C2$xA)
ys3B <- unique(CPU2C2$yB)
ys3B <- ys3B[-15]
grid3B <- with(CPU2C2, expand.grid(xs3B, ys3B))
d3B <- setNames(data.frame(grid3B), c("xA", "yB"))
vals3B <- predict(m2C, newdata = d3B)
m2C <- matrix(vals3B, nrow = length(unique(d3B$xA)), ncol = length(unique(d3B$yB)))
Fig4C <- plotly::add_trace(Fig4C, x = xs3B, y = ys3B, z = m2C[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m3C <- lm(zA ~ xA + yC, data = CPU2C3)
model3C<-m3C
xs3C <- unique(CPU2C3$xA)
ys3C <- unique(CPU2C3$yC)
ys3C <- ys3C[-15]
grid3C <- with(CPU2C3, expand.grid(xs3C, ys3C))
d3C<- setNames(data.frame(grid3C), c("xA", "yC"))
vals3C <- predict(m3C, newdata = d3C)
m3C <- matrix(vals3C, nrow = length(unique(d3C$xA)), ncol = length(unique(d3C$yC)))
Fig4C <- plotly::add_trace(Fig4C, x = xs3C, y = ys3C, z = m3C[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m4C <- lm(zA ~ xA + yD, data = CPU2C4)
model3D<-m4C
xs3D <- unique(CPU2C4$xA)
ys3D <- unique(CPU2C4$yD)
grid3D <- with(CPU2C4, expand.grid(xs3D, ys3D))
d3D <- setNames(data.frame(grid3D), c("xA", "yD"))
vals3D <- predict(m4C, newdata = d3D)
m4C <- matrix(vals3D, nrow = length(unique(d3D$xA)), ncol = length(unique(d3D$yD)))
Fig4C <- plotly::add_trace(Fig4C, x = xs3D, y = ys3D, z = m4C[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

####Fig2D
y1<- as.numeric(unlist(c(CPU[,18],CPU[,19],CPU[,20],CPU[,21])))
CPU2D<-as.data.frame(cbind(x1,y1,z1,y2))
xA<- as.numeric(unlist(c(CPU[,2])))
yA<- as.numeric(unlist(c(CPU[,18])))
zA<- as.numeric(unlist(c(CPU[,4])))
yB<- as.numeric(unlist(c(CPU[,19])))
yC<- as.numeric(unlist(c(CPU[,20])))
yD<- as.numeric(unlist(c(CPU[,21])))
CPU2D1<-as.data.frame(cbind(xA,yA,zA))
CPU2D2<-as.data.frame(cbind(xA,yB,zA))
CPU2D3<-as.data.frame(cbind(xA,yC,zA))
CPU2D4<-as.data.frame(cbind(xA,yD,zA))

m1D <- lm(zA ~ xA + yA, data = CPU2D1)
model4A<-m1D
xs4A <- unique(CPU2D1$xA)
xs4A <- xs4A[-(8:10)]
ys4A <- unique(CPU2D1$yA)
grid4A <- with(CPU2D1, expand.grid(xs4A, ys4A))
d4A <- setNames(data.frame(grid4A), c("xA", "yA"))
vals4A <- predict(m1D, newdata = d4A)
m1D <- matrix(vals4A, nrow = length(unique(d4A$xA)), ncol = length(unique(d4A$yA)))
m1D<- m1D[,-(8:10)]
Fig4D <- plotly::layout( plotly::plot_ly(CPU2D, x = ~x1, y = ~y1, z = ~z1, scene='scene4'))
Fig4D <-plotly::add_markers(Fig4D, color= ~y2, showlegend = FALSE)
Fig4D <- plotly::add_trace(Fig4D, x = xs4A, y = ys4A, z = m1D[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m2D <- lm(zA ~ xA + yB, data = CPU2D2)
model4B <-m2D
xs4B <- unique(CPU2D2$xA)
xs4B <- xs4B[-(8:10)]
ys4B <- unique(CPU2D2$yB)
grid4B <- with(CPU2D2, expand.grid(xs4B, ys4B))
d4B <- setNames(data.frame(grid4B), c("xA", "yB"))
vals4B <- predict(m2D, newdata = d4B)
m2D <- matrix(vals4B, nrow = length(unique(d4B$xA)), ncol = length(unique(d4B$yB)))
m2D<- m2D[,-(8:10)]
Fig4D <- plotly::add_trace(Fig4D, x = xs4B, y = ys4B, z = m2D[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m3D <- lm(zA ~ xA + yC, data = CPU2D3)
model4C<-m3D
xs4C <- unique(CPU2D3$xA)
ys4C <- unique(CPU2D3$yC)
grid4C <- with(CPU2D3, expand.grid(xs4C, ys4C))
d4C<- setNames(data.frame(grid3C), c("xA", "yC"))
vals4C <- predict(m3D, newdata = d3C)
m3D <- matrix(vals4C, nrow = length(unique(d3C$xA)), ncol = length(unique(d3C$yC)))
Fig4D <- plotly::add_trace(Fig4D, x = xs4C, y = ys4C, z = m3D[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))

m4D <- lm(zA ~ xA + yD, data = CPU2D4)
model4D<-m4D
xs4D <- unique(CPU2D4$xA)
xs4D<- xs4D[-(8:10)]
ys4D <- unique(CPU2D4$yD)
grid4D <- with(CPU2D4, expand.grid(xs4D, ys4D))
d4D <- setNames(data.frame(grid4D), c("xA", "yD"))
vals4D <- predict(m4D, newdata = d4D)
m4D <- matrix(vals4D, nrow = length(unique(d4D$xA)), ncol = length(unique(d4D$yD)))
m4D <- m4D[,-(8:10)]
Fig4D <- plotly::add_trace(Fig4D, x = xs4D, y = ys4D, z = m4D[,1], showlegend = FALSE, type = "scatter3d", mode = "lines", line = list(color = "black", width = 5, dash = 'dash'))
####Fig 4 layout
annotations = list(
  list(
    x = 0.2,
    y = 1.0,
    text = "4.A. Coins per 1,000,000 PPD",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 1,
    text = "4.B. Coins per day",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.2,
    y = 0.45,
    text = "4.C. Cost per coin ",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 0.45,
    text = "4.D. Cost per day",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ))


Fig4<-plotly::layout(plotly::subplot(Fig4A,Fig4B,Fig4C,Fig4D), annotations=annotations, title='Figure 4. Studied variables according to CPUs features',
                     scene = list(domain=list(x=c(0,0.5),y=c(0.5,1)),
                                  xaxis = list(gridcolor='rgb(255, 255, 255)',
                                               zerolinecolor='rgb(255, 255, 255)',
                                               showbackground=TRUE,
                                               backgroundcolor='rgb(230, 230,230)', title = 'PPD'), yaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                 zerolinecolor='rgb(255, 255, 255)',
                                                                                                                 showbackground=TRUE,
                                                                                                                 backgroundcolor='rgb(230, 230,230)', title = 'Coins per 1,000,000 PPD'),zaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                                                                                                      zerolinecolor='rgb(255, 255, 255)',
                                                                                                                                                                                                      showbackground=TRUE,
                                                                                                                                                                                                      backgroundcolor='rgb(230, 230,230)', title = 'GPU TDP/TBP Watts'),
                                  aspectmode='cube'),
                     scene2 = list(domain=list(x=c(0.5,1),y=c(0.5,1)),
                                   xaxis = list(gridcolor='rgb(255, 255, 255)',
                                                zerolinecolor='rgb(255, 255, 255)',
                                                showbackground=TRUE,
                                                backgroundcolor='rgb(230, 230,230)', title = 'PPD'), yaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                  zerolinecolor='rgb(255, 255, 255)',
                                                                                                                  showbackground=TRUE,
                                                                                                                  backgroundcolor='rgb(230, 230,230)', title = 'Coins per day'),zaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                                                                                             zerolinecolor='rgb(255, 255, 255)',
                                                                                                                                                                                             showbackground=TRUE,
                                                                                                                                                                                             backgroundcolor='rgb(230, 230,230)', title = 'GPU TDP/TBP Watts'),
                                   aspectmode='cube'),
                     scene3 = list(domain=list(x=c(0,0.5),y=c(0,0.5)),
                                   xaxis = list(gridcolor='rgb(255, 255, 255)',
                                                zerolinecolor='rgb(255, 255, 255)',
                                                showbackground=TRUE,
                                                backgroundcolor='rgb(230, 230,230)', title = 'PPD'), yaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                  zerolinecolor='rgb(255, 255, 255)',
                                                                                                                  showbackground=TRUE,
                                                                                                                  backgroundcolor='rgb(230, 230,230)', title = 'Cost per coin'),zaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                                                                                             zerolinecolor='rgb(255, 255, 255)',
                                                                                                                                                                                             showbackground=TRUE,
                                                                                                                                                                                             backgroundcolor='rgb(230, 230,230)', title = 'GPU TDP/TBP Watts'),
                                   aspectmode='cube'),
                     scene4 = list(domain=list(x=c(0.5,1),y=c(0,0.5)),
                                   xaxis = list(gridcolor='rgb(255, 255, 255)',
                                                zerolinecolor='rgb(255, 255, 255)',
                                                showbackground=TRUE,
                                                backgroundcolor='rgb(230, 230,230)', title = 'PPD'), yaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                  zerolinecolor='rgb(255, 255, 255)',
                                                                                                                  showbackground=TRUE,
                                                                                                                  backgroundcolor='rgb(230, 230,230)', title = 'Cost per day'),zaxis = list(gridcolor='rgb(255, 255, 255)',
                                                                                                                                                                                            zerolinecolor='rgb(255, 255, 255)',
                                                                                                                                                                                            showbackground=TRUE,
                                                                                                                                                                                            backgroundcolor='rgb(230, 230,230)', title = 'GPU TDP/TBP Watts'),
                                   aspectmode='cube'))


Fig4
