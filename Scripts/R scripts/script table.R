# Tables
## GPUs
GPUs_intercept <- c(model2A[["coefficients"]][["(Intercept)"]],
                    model2B[["coefficients"]][["(Intercept)"]],
                    model2C[["coefficients"]][["(Intercept)"]],
                    model2D[["coefficients"]][["(Intercept)"]],
                    model2Ag[["coefficients"]][["(Intercept)"]],
                    model2Bg[["coefficients"]][["(Intercept)"]],
                    model2Cg[["coefficients"]][["(Intercept)"]],
                    model2Dg[["coefficients"]][["(Intercept)"]],
                    model3A[["coefficients"]][["(Intercept)"]],
                    model3B[["coefficients"]][["(Intercept)"]],
                    model3C[["coefficients"]][["(Intercept)"]],
                    model3D[["coefficients"]][["(Intercept)"]],
                    model4A[["coefficients"]][["(Intercept)"]],
                    model4B[["coefficients"]][["(Intercept)"]],
                    model4C[["coefficients"]][["(Intercept)"]],
                    model4D[["coefficients"]][["(Intercept)"]]
)

GPUs_x <- c(model2A[["coefficients"]][["xA"]],
            model2B[["coefficients"]][["xA"]],
            model2C[["coefficients"]][["xA"]],
            model2D[["coefficients"]][["xA"]],
            model2Ag[["coefficients"]][["xA"]],
            model2Bg[["coefficients"]][["xA"]],
            model2Cg[["coefficients"]][["xA"]],
            model2Dg[["coefficients"]][["xA"]],
            model3A[["coefficients"]][["xA"]],
            model3B[["coefficients"]][["xA"]],
            model3C[["coefficients"]][["xA"]],
            model3D[["coefficients"]][["xA"]],
            model4A[["coefficients"]][["xA"]],
            model4B[["coefficients"]][["xA"]],
            model4C[["coefficients"]][["xA"]],
            model4D[["coefficients"]][["xA"]]
)

GPUs_y <- c(model2A[["coefficients"]][["yA"]],
            model2B[["coefficients"]][["yB"]],
            model2C[["coefficients"]][["yC"]],
            model2D[["coefficients"]][["yD"]],
            model2Ag[["coefficients"]][["yA"]],
            model2Bg[["coefficients"]][["yB"]],
            model2Cg[["coefficients"]][["yC"]],
            model2Dg[["coefficients"]][["yD"]],
            model3A[["coefficients"]][["yA"]],
            model3B[["coefficients"]][["yB"]],
            model3C[["coefficients"]][["yC"]],
            model3D[["coefficients"]][["yD"]],
            model4A[["coefficients"]][["yA"]],
            model4B[["coefficients"]][["yB"]],
            model4C[["coefficients"]][["yC"]],
            model4D[["coefficients"]][["yD"]]
)

Crypto<-c("BANANO","CURECOIN","DOGECOIN MINING","DOGECOIN FOLDING",
          "BANANO","CURECOIN","DOGECOIN MINING","DOGECOIN FOLDING",
          "BANANO","CURECOIN","DOGECOIN MINING","DOGECOIN FOLDING",
          "BANANO","CURECOIN","DOGECOIN MINING","DOGECOIN FOLDING")

Figure<-c("Fig 2.A.", "Fig 2.A.", "Fig 2.A.", "Fig 2.A.",
          "Fig 2.B.", "Fig 2.B.", "Fig 2.B.", "Fig 2.B.",
          "Fig 2.C.", "Fig 2.C.", "Fig 2.C.", "Fig 2.C.",
          "Fig 2.D.", "Fig 2.D.", "Fig 2.D.", "Fig 2.D.")

GPUs_comp<-as.data.frame(cbind(Figure,Crypto,GPUs_intercept,GPUs_x,GPUs_y))
GPUs_comp<-gridExtra::tableGrob(GPUs_comp)
grid::grid.draw(GPUs_comp)
## CPUs
CPUs_intercept <- c(model2ACPU[["coefficients"]][["(Intercept)"]],
                    model2BCPU[["coefficients"]][["(Intercept)"]],
                    model2CCPU[["coefficients"]][["(Intercept)"]],
                    model2DCPU[["coefficients"]][["(Intercept)"]],
                    model2AB[["coefficients"]][["(Intercept)"]],
                    model2BB[["coefficients"]][["(Intercept)"]],
                    model2C[["coefficients"]][["(Intercept)"]],
                    model2D[["coefficients"]][["(Intercept)"]],
                    model3A[["coefficients"]][["(Intercept)"]],
                    model3B[["coefficients"]][["(Intercept)"]],
                    model3C[["coefficients"]][["(Intercept)"]],
                    model3D[["coefficients"]][["(Intercept)"]],
                    model4A[["coefficients"]][["(Intercept)"]],
                    model4B[["coefficients"]][["(Intercept)"]],
                    model4C[["coefficients"]][["(Intercept)"]],
                    model4D[["coefficients"]][["(Intercept)"]]
                    )

CPUs_x <- c(model2ACPU[["coefficients"]][["xA"]],
                    model2BCPU[["coefficients"]][["xA"]],
                    model2CCPU[["coefficients"]][["xA"]],
                    model2DCPU[["coefficients"]][["xA"]],
                    model2AB[["coefficients"]][["xA"]],
                    model2BB[["coefficients"]][["xA"]],
                    model2C[["coefficients"]][["xA"]],
                    model2D[["coefficients"]][["xA"]],
                    model3A[["coefficients"]][["xA"]],
                    model3B[["coefficients"]][["xA"]],
                    model3C[["coefficients"]][["xA"]],
                    model3D[["coefficients"]][["xA"]],
                    model4A[["coefficients"]][["xA"]],
                    model4B[["coefficients"]][["xA"]],
                    model4C[["coefficients"]][["xA"]],
                    model4D[["coefficients"]][["xA"]]
)

CPUs_y <- c(model2ACPU[["coefficients"]][["yA"]],
            model2BCPU[["coefficients"]][["yB"]],
            model2CCPU[["coefficients"]][["yC"]],
            model2DCPU[["coefficients"]][["yD"]],
            model2AB[["coefficients"]][["yA"]],
            model2BB[["coefficients"]][["yB"]],
            model2C[["coefficients"]][["yC"]],
            model2D[["coefficients"]][["yD"]],
            model3A[["coefficients"]][["yA"]],
            model3B[["coefficients"]][["yB"]],
            model3C[["coefficients"]][["yC"]],
            model3D[["coefficients"]][["yD"]],
            model4A[["coefficients"]][["yA"]],
            model4B[["coefficients"]][["yB"]],
            model4C[["coefficients"]][["yC"]],
            model4D[["coefficients"]][["yD"]]
)

Crypto<-c("BANANO","CURECOIN","DOGECOIN MINING","DOGECOIN FOLDING",
          "BANANO","CURECOIN","DOGECOIN MINING","DOGECOIN FOLDING",
          "BANANO","CURECOIN","DOGECOIN MINING","DOGECOIN FOLDING",
          "BANANO","CURECOIN","DOGECOIN MINING","DOGECOIN FOLDING")

Figure<-c("Fig 4.A.", "Fig 4.A.", "Fig 4.A.", "Fig 4.A.",
          "Fig 4.B.", "Fig 4.B.", "Fig 4.B.", "Fig 4.B.",
          "Fig 4.C.", "Fig 4.C.", "Fig 4.C.", "Fig 4.C.",
          "Fig 4.D.", "Fig 4.D.", "Fig 4.D.", "Fig 4.D.")

CPUs_comp<-as.data.frame(cbind(Figure,Crypto,CPUs_intercept,CPUs_x,CPUs_y))
CPUs_comp<-gridExtra::tableGrob(CPUs_comp)
grid::grid.draw(CPUs_comp)
