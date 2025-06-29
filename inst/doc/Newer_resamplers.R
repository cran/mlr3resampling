## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.width=10,
  fig.height=6)
data.table::setDTthreads(1)
## output: rmarkdown::html_vignette above creates html where figures are limited to 700px wide.
## Above CSS from https://stackoverflow.com/questions/34906002/increase-width-of-entire-html-rmarkdown-output main-container is for html_document, body is for html_vignette
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----simulationScatter--------------------------------------------------------
N <- 2100
abs.x <- 70
set.seed(2)
x.vec <- runif(N, -abs.x, abs.x)
str(x.vec)
library(data.table)
(task.dt <- data.table(
  x=x.vec,
  y = sin(x.vec)+rnorm(N,sd=0.5)))
if(require(ggplot2)){
  text.size <- 6
  my_theme <- theme_bw(20)
  theme_set(my_theme)
  ggplot()+
    geom_point(aes(
      x, y),
      shape=1,
      data=task.dt)
}

## -----------------------------------------------------------------------------
atomic.group.size <- 2
task.dt[, agroup := rep(seq(1, N/atomic.group.size), each=atomic.group.size)][]
task.dt[, random_group := rep(
  rep(c("A","B","B","C","C","C","C"), each=atomic.group.size),
  l=.N
)][]
table(group.tab <- task.dt$random_group)

## -----------------------------------------------------------------------------
reg.task <- mlr3::TaskRegr$new(
  "sin", task.dt, target="y")
reg.task$col_roles$group <- "agroup"
reg.task$col_roles$stratum <- "random_group"
reg.task$col_roles$feature <- "x"

## -----------------------------------------------------------------------------
same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
reg.task$col_roles$subset <- "random_group" 

## -----------------------------------------------------------------------------
same_other_sizes_cv$instantiate(reg.task)
same_other_sizes_cv$instance$iteration.dt

## ----SameOtherCV--------------------------------------------------------------
(reg.learner.list <- list(
  mlr3::LearnerRegrFeatureless$new()))
if(requireNamespace("rpart")){
  reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
}
(same.other.grid <- mlr3::benchmark_grid(
  reg.task,
  reg.learner.list,
  same_other_sizes_cv))
##if(require(future))plan("multisession")
lgr::get_logger("mlr3")$set_threshold("warn")
(same.other.result <- mlr3::benchmark(
  same.other.grid, store_models = TRUE))
same.other.score <- mlr3resampling::score(
  same.other.result, mlr3::msr("regr.rmse"))
plot(same.other.score)+my_theme

## ----fig.height=3-------------------------------------------------------------
same.other.score[, n.train := sapply(train, length)]
same.other.score[1]
if(require(ggplot2)){
  ggplot()+
    geom_point(aes(
      regr.rmse, train.subsets, color=algorithm),
      shape=1,
      data=same.other.score)+
    geom_text(aes(
      Inf, train.subsets,
      label=sprintf("n.train=%d ", n.train)),
      size=text.size,
      hjust=1,
      vjust=1.5,
      data=same.other.score[algorithm=="featureless" & test.fold==1])+
    facet_grid(. ~ test.subset, labeller=label_both, scales="free")+
    scale_x_continuous(
      "Root mean squared prediction error (test set)")
}

## ----fig.height=3-------------------------------------------------------------
same.other.wide <- dcast(
  same.other.score,
  algorithm + test.subset + train.subsets ~ .,
  list(mean, sd),
  value.var="regr.rmse")
if(require(ggplot2)){
  ggplot()+
    geom_segment(aes(
      regr.rmse_mean+regr.rmse_sd, train.subsets,
      xend=regr.rmse_mean-regr.rmse_sd, yend=train.subsets,
      color=algorithm),
      data=same.other.wide)+
    geom_point(aes(
      regr.rmse_mean, train.subsets, color=algorithm),
      shape=1,
      data=same.other.wide)+
    geom_text(aes(
      Inf, train.subsets,
      label=sprintf("n.train=%d ", n.train)),
      size=text.size,
      hjust=1,
      vjust=1.5,
      data=same.other.score[algorithm=="featureless" & test.fold==1])+
    facet_grid(. ~ test.subset, labeller=label_both, scales="free")+
    scale_x_continuous(
      "Root mean squared prediction error (test set)")
}

## -----------------------------------------------------------------------------
plist <- mlr3resampling::pvalue(same.other.score, digits=3)
plot(plist)+my_theme

## -----------------------------------------------------------------------------
if(require(ggplot2)){
  ggplot()+
    geom_line(aes(
      n.train, regr.rmse,
      color=algorithm,
      group=paste(algorithm, test.fold)),
      data=same.other.score)+
    geom_label(aes(
      n.train, regr.rmse,
      color=algorithm,
      label=train.subsets),
      size=text.size,
      data=same.other.score)+
    facet_grid(. ~ test.subset, labeller=label_both, scales="free")+
    scale_y_continuous(
      "Root mean squared prediction error (test set)")
}

## -----------------------------------------------------------------------------
task.no.subset <- mlr3::TaskRegr$new(
  "sin", task.dt, target="y")
task.no.subset$col_roles$group <- "agroup"
task.no.subset$col_roles$stratum <- "random_group"
task.no.subset$col_roles$feature <- "x"
str(task.no.subset$col_roles)

## -----------------------------------------------------------------------------
same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
same_other_sizes_cv$param_set$values$sizes <- 5
same_other_sizes_cv$instantiate(task.no.subset)
same_other_sizes_cv$instance$iteration.dt

## -----------------------------------------------------------------------------
(reg.learner.list <- list(
  mlr3::LearnerRegrFeatureless$new()))
if(requireNamespace("rpart")){
  reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
}
(same.other.grid <- mlr3::benchmark_grid(
  task.no.subset,
  reg.learner.list,
  same_other_sizes_cv))
##if(require(future))plan("multisession")
lgr::get_logger("mlr3")$set_threshold("warn")
(same.other.result <- mlr3::benchmark(
  same.other.grid, store_models = TRUE))
same.other.score <- mlr3resampling::score(
  same.other.result, mlr3::msr("regr.rmse"))
same.other.score[, n.train := sapply(train, length)]
same.other.score[1]

if(require(ggplot2)){
  ggplot()+
    geom_line(aes(
      n.train, regr.rmse,
      color=algorithm,
      group=paste(algorithm, test.fold)),
      data=same.other.score)+
    geom_point(aes(
      n.train, regr.rmse,
      color=algorithm),
      data=same.other.score)+
    facet_grid(. ~ test.subset, labeller=label_both, scales="free")+
    scale_x_log10(
      "Number of train rows",
      breaks=unique(same.other.score$n.train))+
    scale_y_continuous(
      "Root mean squared prediction error (test set)")
}

## ----simulationShort----------------------------------------------------------
N <- 600
abs.x <- 20
set.seed(1)
x.vec <- sort(runif(N, -abs.x, abs.x))
str(x.vec)
library(data.table)
(task.dt <- data.table(
  x=x.vec,
  y = sin(x.vec)+rnorm(N,sd=0.5)))
if(require(ggplot2)){
  ggplot()+
    geom_point(aes(
      x, y),
      shape=1,
      data=task.dt)+
    coord_equal()
}
atomic.subset.size <- 2
task.dt[, agroup := rep(seq(1, N/atomic.subset.size), each=atomic.subset.size)][]
task.dt[, random_subset := rep(
  rep(c("A","B","B","B"), each=atomic.subset.size),
  l=.N
)][]
table(subset.tab <- task.dt$random_subset)

reg.task <- mlr3::TaskRegr$new(
  "sin", task.dt, target="y")
reg.task$col_roles$subset <- "random_subset"
reg.task$col_roles$group <- "agroup"
reg.task$col_roles$stratum <- "random_subset"
reg.task$col_roles$feature <- "x"
same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()

## -----------------------------------------------------------------------------
same_other_sizes_cv$param_set$values$sizes <- 0
same_other_sizes_cv$instantiate(reg.task)
same_other_sizes_cv$instance$it
(reg.learner.list <- list(
  mlr3::LearnerRegrFeatureless$new()))
if(requireNamespace("rpart")){
  reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
}
(same.other.grid <- mlr3::benchmark_grid(
  reg.task,
  reg.learner.list,
  same_other_sizes_cv))
##if(require(future))plan("multisession")
lgr::get_logger("mlr3")$set_threshold("warn")
(same.other.result <- mlr3::benchmark(
  same.other.grid, store_models = TRUE))
same.other.score <- mlr3resampling::score(
  same.other.result, mlr3::msr("regr.rmse"))
same.other.score[1]

## ----fig.height=2-------------------------------------------------------------
if(require(ggplot2)){
ggplot()+
  geom_point(aes(
    regr.rmse, train.subsets, color=algorithm),
    shape=1,
    data=same.other.score[groups==n.train.groups])+
  facet_grid(. ~ test.subset, labeller=label_both)
}

## ----fig.height=4-------------------------------------------------------------
same.other.score[, subset.N := paste(train.subsets, n.train.groups)]
(levs <- same.other.score[order(train.subsets, n.train.groups), unique(subset.N)])
same.other.score[, subset.N.fac := factor(subset.N, levs)]
if(require(ggplot2)){
  ggplot()+
    geom_point(aes(
      regr.rmse, subset.N.fac, color=algorithm),
      shape=1,
      data=same.other.score)+
    facet_wrap("test.subset", labeller=label_both, scales="free", nrow=1)
}
(levs <- same.other.score[order(n.train.groups, train.subsets), unique(subset.N)])
same.other.score[, N.subset.fac := factor(subset.N, levs)]
if(require(ggplot2)){
  ggplot()+
    geom_point(aes(
      regr.rmse, N.subset.fac, color=algorithm),
      shape=1,
      data=same.other.score)+
    facet_wrap("test.subset", labeller=label_both, scales="free", nrow=1)
}

## ----fig.height=5-------------------------------------------------------------
if(require(ggplot2)){
  ggplot()+
    geom_point(aes(
      n.train.groups, regr.rmse,
      color=train.subsets),
      shape=1,
      data=same.other.score)+
    geom_line(aes(
      n.train.groups, regr.rmse,
      group=paste(train.subsets, seed, algorithm),
      linetype=algorithm,
      color=train.subsets),
      data=same.other.score)+
    facet_grid(test.fold ~ test.subset, labeller=label_both)
}
rpart.score <- same.other.score[algorithm=="rpart" & train.subsets != "other"]
if(require(ggplot2)){
  ggplot()+
    geom_point(aes(
      n.train.groups, regr.rmse,
      color=train.subsets),
      shape=1,
      data=rpart.score)+
    geom_line(aes(
      n.train.groups, regr.rmse,
      group=paste(train.subsets, seed, algorithm),
      color=train.subsets),
      data=rpart.score)+
    facet_grid(test.fold ~ test.subset, labeller=label_both)
}

## -----------------------------------------------------------------------------
str(reg.task$col_roles)

## -----------------------------------------------------------------------------
ignore.cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
ignore.cv$param_set$values$ignore_subset <- TRUE
ignore.cv$instantiate(reg.task)
ignore.cv$instance$iteration.dt

## -----------------------------------------------------------------------------
do_benchmark <- function(subtrain.valid.cv){
  reg.learner.list <- list(
    mlr3::LearnerRegrFeatureless$new())
  if(requireNamespace("rpart")){
    reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
    if(requireNamespace("mlr3tuning")){
      rpart.learner <- mlr3::LearnerRegrRpart$new()
      ##mlr3tuningspaces::lts(rpart.learner)$param_set$values
      rpart.learner$param_set$values$cp <- paradox::to_tune(1e-4, 0.1, log=TRUE)
      reg.learner.list$rpart.tuned <- mlr3tuning::auto_tuner(
        tuner = mlr3tuning::tnr("grid_search"), #mlr3tuning::TunerBatchGridSearch$new()
        learner = rpart.learner,
        resampling = subtrain.valid.cv,
        measure = mlr3::msr("regr.rmse"))
    }
  }
  same.other.grid <- mlr3::benchmark_grid(
    reg.task,
    reg.learner.list,
    same_other_sizes_cv)
  lgr::get_logger("bbotk")$set_threshold("warn")
  same.other.result <- mlr3::benchmark(
    same.other.grid, store_models = TRUE)
}

## -----------------------------------------------------------------------------
ignore.cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
ignore.cv$param_set$values$ignore_subset <- TRUE
(same.other.result <- do_benchmark(ignore.cv))

## ----fig.height=7-------------------------------------------------------------
same.other.score <- mlr3resampling::score(
  same.other.result, mlr3::msr("regr.rmse"))
same.other.score[1]
same.other.wide <- dcast(
  same.other.score,
  algorithm + test.subset + train.subsets ~ .,
  list(mean, sd),
  value.var="regr.rmse")
if(require(ggplot2)){
  ggplot()+
    geom_segment(aes(
      regr.rmse_mean+regr.rmse_sd, train.subsets,
      xend=regr.rmse_mean-regr.rmse_sd, yend=train.subsets),
      data=same.other.wide)+
    geom_point(aes(
      regr.rmse_mean, train.subsets),
      shape=1,
      data=same.other.wide)+
    facet_grid(algorithm ~ test.subset, labeller=label_both)
}

## -----------------------------------------------------------------------------
data(AZtrees,package="mlr3resampling")
library(data.table)
AZdt <- data.table(AZtrees)
AZdt[1]

## -----------------------------------------------------------------------------
x.center <- -111.72
y.center <- 35.272
rect.size <- 0.01/2
x.min.max <- x.center+c(-1, 1)*rect.size
y.min.max <- y.center+c(-1, 1)*rect.size
rect.dt <- data.table(
  xmin=x.min.max[1], xmax=x.min.max[2],
  ymin=y.min.max[1], ymax=y.min.max[2])
if(require(ggplot2)){
  tree.fill.scale <- scale_fill_manual(
    values=c(Tree="black", "Not tree"="white"))
  ggplot()+
    tree.fill.scale+
    geom_rect(aes(
      xmin=xmin, xmax=xmax, ymin=ymin,ymax=ymax),
      data=rect.dt,
      fill="red",
      linewidth=3,
      color="red")+
    geom_point(aes(
      xcoord, ycoord, fill=y),
      shape=21,
      data=AZdt)+
    coord_equal()
}

## -----------------------------------------------------------------------------
if(require(ggplot2)){
  gg <- ggplot()+
    tree.fill.scale+
    geom_point(aes(
      xcoord, ycoord, fill=y),
      shape=21,
      data=AZdt)+
    coord_equal()+
    scale_x_continuous(
      limits=x.min.max)+
    scale_y_continuous(
      limits=y.min.max)
  if(require(directlabels)){
    gg <- gg+geom_dl(aes(
      xcoord, ycoord, label=paste("polygon",polygon)),
      data=AZdt,
      method=list(cex=2, "smart.grid"))
  }
  gg
}

## -----------------------------------------------------------------------------
##dput(RColorBrewer::brewer.pal(3,"Dark2"))
region.colors <- c(NW="#1B9E77", NE="#D95F02", S="#7570B3")
if(require(ggplot2)){
  ggplot()+
    tree.fill.scale+
    scale_color_manual(
      values=region.colors)+
    geom_point(aes(
      xcoord, ycoord, color=region3, fill=y),
      shape=21,
      data=AZdt)+
    coord_equal()
}

## -----------------------------------------------------------------------------
ctask <- mlr3::TaskClassif$new(
  "AZtrees", AZdt, target="y")
ctask$col_roles$subset <- "region3"
ctask$col_roles$group <- "polygon"
ctask$col_roles$stratum <- "y"
ctask$col_roles$feature <- grep("SAMPLE",names(AZdt),value=TRUE)
str(ctask$col_roles)

## -----------------------------------------------------------------------------
same.other.cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
same.other.cv$param_set$values$folds <- 3
same.other.cv$instantiate(ctask)
same.other.cv$instance$iteration.dt[, .(
  train.subsets, test.fold, test.subset, n.train.groups,
  train.rows=sapply(train, length))]

## -----------------------------------------------------------------------------
AZdt[, .(
  polygons=length(unique(polygon))
), by=region3][
, train.polygons := polygons*with(same.other.cv$param_set$values, (folds-1)/folds)
][]

## -----------------------------------------------------------------------------
same.other.cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
(learner.list <- list(
  mlr3::LearnerClassifFeatureless$new()))
if(requireNamespace("rpart")){
  learner.list$rpart <- mlr3::LearnerClassifRpart$new()
}
for(learner.i in seq_along(learner.list)){
  learner.list[[learner.i]]$predict_type <- "prob"
}
(bench.grid <- mlr3::benchmark_grid(ctask, learner.list, same.other.cv))

## -----------------------------------------------------------------------------
bench.result <- mlr3::benchmark(bench.grid)
measure.list <- mlr3::msrs(c("classif.acc","classif.auc"))
score.dt <- mlr3resampling::score(bench.result, measure.list)
score.dt[1]

## -----------------------------------------------------------------------------
score.long <- melt(
  score.dt,
  measure.vars=measure(variable, pattern="classif.(acc|auc)"))
if(require(ggplot2)){
  ggplot()+
    geom_point(aes(
      value, train.subsets, color=algorithm),
      data=score.long)+
    facet_grid(test.subset ~ variable, labeller=label_both, scales="free")
}

## -----------------------------------------------------------------------------
plot(score.dt)+my_theme

## -----------------------------------------------------------------------------
score.wide <- dcast(
  score.long,
  algorithm + test.subset + train.subsets + variable ~ .,
  list(mean, sd),
  value.var="value")
if(require(ggplot2)){
  ggplot()+
    geom_point(aes(
      value_mean, train.subsets, color=algorithm),
      size=3,
      fill="white",
      shape=21,
      data=score.wide)+
    geom_segment(aes(
      value_mean+value_sd, train.subsets,
      color=algorithm,
      linewidth=algorithm,
      xend=value_mean-value_sd, yend=train.subsets),
      data=score.wide)+
    scale_linewidth_manual(values=c(featureless=2, rpart=1))+
    facet_grid(test.subset ~ variable, labeller=label_both, scales="free")+
    scale_x_continuous(
      "Mean +/- SD of test accuracy/AUC over folds/splits")
}

## -----------------------------------------------------------------------------
AZ_pval <- mlr3resampling::pvalue(score.dt, digits=3)
plot(AZ_pval)+my_theme

## -----------------------------------------------------------------------------
AZ_pval_AUC <- mlr3resampling::pvalue(score.dt, "classif.auc", digits=3)
plot(AZ_pval_AUC)+my_theme

## -----------------------------------------------------------------------------
sessionInfo()

