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

## -----------------------------------------------------------------------------
N <- 300
library(data.table)
set.seed(1)
abs.x <- 2
reg.dt <- data.table(
  x=runif(N, -abs.x, abs.x),
  person=rep(1:2, each=0.5*N))
reg.pattern.list <- list(
  easy=function(x, person)x^2,
  impossible=function(x, person)(x^2+person*3)*(-1)^person)
reg.task.list <- list()
for(task_id in names(reg.pattern.list)){
  f <- reg.pattern.list[[task_id]]
  yname <- paste0("y_",task_id)
  reg.dt[, (yname) := f(x,person)+rnorm(N)][]
  task.dt <- reg.dt[, c("x","person",yname), with=FALSE]
  reg.task <- mlr3::TaskRegr$new(
    task_id, task.dt, target=yname)
  if(requireNamespace("mlr3resampling"))reg.task$col_roles$subset <- "person"
  reg.task$col_roles$stratum <- "person"
  reg.task$col_roles$feature <- "x"
  reg.task.list[[task_id]] <- reg.task
}
reg.dt

## -----------------------------------------------------------------------------
(reg.tall <- nc::capture_melt_single(
  reg.dt,
  task_id="easy|impossible",
  value.name="y"))

## ----fig.height=7-------------------------------------------------------------
if(require(animint2)){
  print_theme <- theme_bw(20)
  ggplot()+
    print_theme+
    geom_point(aes(
      x, y),
      data=reg.tall)+
    facet_grid(
      task_id ~ person,
      labeller=label_both,
      space="free",
      scales="free")+
    scale_y_continuous(
      breaks=seq(-100, 100, by=2))
}

## -----------------------------------------------------------------------------
(reg_same_other <- mlr3resampling::ResamplingSameOtherCV$new())

## -----------------------------------------------------------------------------
(reg.learner.list <- list(
  if(requireNamespace("rpart"))mlr3::LearnerRegrRpart$new(),
  mlr3::LearnerRegrFeatureless$new()))

## -----------------------------------------------------------------------------
(reg.bench.grid <- mlr3::benchmark_grid(
  reg.task.list,
  reg.learner.list,
  reg_same_other))

## -----------------------------------------------------------------------------
if(FALSE){#for CRAN.
  if(require(future))plan("multisession")
}
if(require(lgr))get_logger("mlr3")$set_threshold("warn")
(reg.bench.result <- mlr3::benchmark(
  reg.bench.grid, store_models = TRUE))

## -----------------------------------------------------------------------------
reg.bench.score <- mlr3resampling::score(reg.bench.result)
reg.bench.score[1]

## -----------------------------------------------------------------------------
if(require(animint2)){
  ggplot()+
    print_theme+
    scale_x_log10()+
    geom_point(aes(
      regr.mse, train.subsets, color=algorithm),
      shape=1,
      data=reg.bench.score)+
    facet_grid(
      task_id ~ person,
      labeller=label_both,
      scales="free")
}

## ----SimulationsAnimintRegression---------------------------------------------
inst <- reg.bench.score$resampling[[1]]$instance
rect.expand <- 0.3
grid.dt <- data.table(x=seq(-abs.x, abs.x, l=101), y=0)
grid.task <- mlr3::TaskRegr$new("grid", grid.dt, target="y")
pred.dt.list <- list()
point.dt.list <- list()
for(score.i in 1:nrow(reg.bench.score)){
  reg.bench.row <- reg.bench.score[score.i]
  task.dt <- data.table(
    reg.bench.row$task[[1]]$data(),
    reg.bench.row$resampling[[1]]$instance$id.dt)
  names(task.dt)[1] <- "y"
  set.ids <- data.table(
    set.name=c("test","train")
  )[
  , data.table(row_id=reg.bench.row[[set.name]][[1]])
  , by=set.name]
  i.points <- set.ids[
    task.dt, on="row_id"
  ][
    is.na(set.name), set.name := "unused"
  ]
  point.dt.list[[score.i]] <- data.table(
    reg.bench.row[, .(task_id, iteration)],
    i.points)
  i.learner <- reg.bench.row$learner[[1]]
  pred.dt.list[[score.i]] <- data.table(
    reg.bench.row[, .(
      task_id, iteration, algorithm
    )],
    as.data.table(
      i.learner$predict(grid.task)
    )[, .(x=grid.dt$x, y=response)]
  )
}
(pred.dt <- rbindlist(pred.dt.list))
(point.dt <- rbindlist(point.dt.list))
set.colors <- c(
  train="#1B9E77",
  test="#D95F02",
  unused="white")
algo.colors <- c(
  featureless="blue",
  rpart="red")
make_person_subset <- function(DT){
  DT[, "person/subset" := person]
}
make_person_subset(point.dt)
make_person_subset(reg.bench.score)

if(require(animint2)){
  viz <- animint(
    title="SOAK algorithm: train/predict on subsets, regression",
    video="https://vimeo.com/1053413000",
    pred=ggplot()+
      ggtitle("Predictions for selected train/test split")+
      theme_animint(
        rowspan=2,
        width=600,
        height=700)+
      scale_fill_manual(values=set.colors)+
      geom_point(aes(
        x, y, fill=set.name),
        showSelected="iteration",
        size=3,
        help="One dot for each train/test/unused data point.",
        shape=21,
        data=point.dt)+
      scale_color_manual(values=algo.colors)+
      geom_line(aes(
        x, y, color=algorithm,
        group=paste(algorithm, iteration)),
        help="One line for each learned prediction function.",
        showSelected="iteration",
        data=pred.dt)+
      facet_grid(
        task_id ~ `person/subset`,
        labeller=label_both,
        space="free",
        scales="free")+
      scale_x_continuous(
        "x = input/feature in regression")+
      scale_y_continuous(
        "y = output to predict in regression",
        breaks=seq(-100, 100, by=2)),
    err=ggplot()+
      ggtitle("Test error for each split")+
      theme_animint(
        height=400,
        width=400,
        last_in_row=TRUE)+
      guides(fill="none")+
      scale_y_log10(
        "Mean squared error on test set")+
      scale_fill_manual(values=algo.colors)+
      scale_x_discrete(
        "People/subsets in train set")+
      geom_point(aes(
        train.subsets, regr.mse, fill=algorithm),
        help="One dot per test set and learning algorithm.",
        shape=1,
        size=5,
        stroke=2,
        color="black",
        color_off=NA,
        showSelected="algorithm",
        clickSelects="iteration",
        data=reg.bench.score)+
      facet_grid(
        task_id ~ `person/subset`,
        labeller=label_both,
        scales="free"),
    diagram=ggplot()+
      ggtitle("Select train/test split")+
      theme_animint(height=300, width=350)+
      facet_grid(
        . ~ train.subsets,
        scales="free",
        space="free")+
      scale_size_manual(values=c(subset=3, fold=1))+
      scale_color_manual(values=c(subset="orange", fold="grey50"))+
      geom_rect(aes(
        xmin=-Inf, xmax=Inf,
        color=rows,
        size=rows,
        ymin=display_row, ymax=display_end),
        help="One rect per chunk of data with common fold (grey) and subset (gold).",
        fill=NA,
        data=inst$viz.rect.dt)+
      scale_fill_manual(values=set.colors)+
      geom_label_aligned(aes(
        x=ifelse(rows=="subset", Inf, -Inf),
        y=(display_row+display_end)/2,
        color=rows,
        hjust=ifelse(rows=="subset", 1, 0),
        label=paste0(rows, "=", ifelse(rows=="subset", subset, fold))),
        help="Text labels indicate chunks of data with common fold (grey) and subset (gold).",
        showSelected="rows",
        data=data.table(train.name="same", inst$viz.rect.dt))+
      geom_rect(aes(
        xmin=iteration-rect.expand, ymin=display_row,
        xmax=iteration+rect.expand, ymax=display_end,
        fill=set.name),
        help="One rect per chunk of data assigned to train/test set in cross-validation.",
        alpha=0.5,
        alpha_off=0.5,
        color="black",
        color_off=NA,
        clickSelects="iteration",
        data=inst$viz.set.dt)+
      scale_x_continuous(
        "Split number",
        breaks=c(1,6, 7,12, 13,18))+
      scale_y_continuous(
        "Row number"),
    first=list(iteration=10),
    source="https://github.com/tdhock/mlr3resampling/blob/main/vignettes/Older_resamplers.Rmd")
}
if(FALSE){
  animint2pages(viz, "2023-12-13-train-predict-subsets-regression", chromote_sleep_seconds = 5)
}

## -----------------------------------------------------------------------------
N <- 200
library(data.table)
(full.dt <- data.table(
  label=factor(rep(c("spam","not spam"), l=N)),
  person=rep(1:2, each=0.5*N)
)[, signal := ifelse(label=="not spam", 0, 3)][])

## -----------------------------------------------------------------------------
set.seed(1)
n.people <- length(unique(full.dt$person))
for(person.i in 1:n.people){
  use.signal.vec <- list(
    easy=rep(if(person.i==1)TRUE else FALSE, N),
    impossible=full.dt$person==person.i)
  for(task_id in names(use.signal.vec)){
    use.signal <- use.signal.vec[[task_id]]
    full.dt[
    , paste0("x",person.i,"_",task_id) := ifelse(
      use.signal, signal, 0
    )+rnorm(N)][]
  }
}
full.dt

## -----------------------------------------------------------------------------
(scatter.dt <- nc::capture_melt_multiple(
  full.dt,
  column="x[12]",
  "_",
  task_id="easy|impossible"))

## -----------------------------------------------------------------------------
if(require(animint2)){
  ggplot()+
    print_theme+
    geom_point(aes(
      x1, x2, color=label),
      shape=1,
      data=scatter.dt)+
    facet_grid(
      task_id ~ person,
      labeller=label_both)
}

## -----------------------------------------------------------------------------
class.task.list <- list()
for(task_id in c("easy","impossible")){
  feature.names <- grep(task_id, names(full.dt), value=TRUE)
  task.col.names <- c(feature.names, "label", "person")
  task.dt <- full.dt[, task.col.names, with=FALSE]
  this.task <- mlr3::TaskClassif$new(
    task_id, task.dt, target="label")
  this.task$col_roles$subset <- "person"
  this.task$col_roles$stratum <- c("person","label")
  this.task$col_roles$feature <- setdiff(names(task.dt), this.task$col_roles$stratum)
  class.task.list[[task_id]] <- this.task
}
class.task.list

## -----------------------------------------------------------------------------
(class_same_other <- mlr3resampling::ResamplingSameOtherCV$new())

## -----------------------------------------------------------------------------
(class.learner.list <- list(
  if(requireNamespace("rpart"))mlr3::LearnerClassifRpart$new(),
  mlr3::LearnerClassifFeatureless$new()))

## -----------------------------------------------------------------------------
(class.bench.grid <- mlr3::benchmark_grid(
  class.task.list,
  class.learner.list,
  class_same_other))

## -----------------------------------------------------------------------------
if(FALSE){
  if(require(future))plan("multisession")
}
if(require(lgr))get_logger("mlr3")$set_threshold("warn")
(class.bench.result <- mlr3::benchmark(
  class.bench.grid, store_models = TRUE))

## -----------------------------------------------------------------------------
class.bench.score <- mlr3resampling::score(class.bench.result)
class.bench.score[1]

## -----------------------------------------------------------------------------
if(require(animint2)){
  ggplot()+
    print_theme+
    geom_point(aes(
      classif.ce, train.subsets, color=algorithm),
      shape=1,
      data=class.bench.score)+
    facet_grid(
      person ~ task_id,
      labeller=label_both,
      scales="free")
}

## ----SimulationsAnimintClassification-----------------------------------------
inst <- class.bench.score$resampling[[1]]$instance
rect.expand <- 0.3
grid.value.dt <- scatter.dt[
, lapply(.SD, function(x)do.call(seq, c(as.list(range(x)), l=21)))
, .SDcols=c("x1","x2")]
grid.class.dt <- data.table(
  label=full.dt$label[1],
  do.call(
    CJ, grid.value.dt
  )
)
class.pred.dt.list <- list()
class.point.dt.list <- list()
for(score.i in 1:nrow(class.bench.score)){
  class.bench.row <- class.bench.score[score.i]
  task.dt <- data.table(
    class.bench.row$task[[1]]$data(),
    class.bench.row$resampling[[1]]$instance$id.dt)
  names(task.dt)[2:3] <- c("x1","x2")
  set.ids <- data.table(
    set.name=c("test","train")
  )[
  , data.table(row_id=class.bench.row[[set.name]][[1]])
  , by=set.name]
  i.points <- set.ids[
    task.dt, on="row_id"
  ][
    is.na(set.name), set.name := "unused"
  ][]
  class.point.dt.list[[score.i]] <- data.table(
    class.bench.row[, .(task_id, iteration)],
    i.points)
  if(class.bench.row$algorithm!="featureless"){
    i.learner <- class.bench.row$learner[[1]]
    i.learner$predict_type <- "prob"
    i.task <- class.bench.row$task[[1]]
    setnames(grid.class.dt, names(i.task$data()))
    grid.class.task <- mlr3::TaskClassif$new(
      "grid", grid.class.dt, target="label")
    pred.grid <- as.data.table(
      i.learner$predict(grid.class.task)
    )[, data.table(grid.class.dt, prob.spam)]
    names(pred.grid)[2:3] <- c("x1","x2")
    pred.wide <- dcast(pred.grid, x1 ~ x2, value.var="prob.spam")
    prob.mat <- as.matrix(pred.wide[,-1])
    contour.list <- contourLines(
      grid.value.dt$x1, grid.value.dt$x2, prob.mat, levels=0.5)
    class.pred.dt.list[[score.i]] <- data.table(
      class.bench.row[, .(
        task_id, iteration, algorithm
      )],
      data.table(contour.i=seq_along(contour.list))[, {
        do.call(data.table, contour.list[[contour.i]])[, .(level, x1=x, x2=y)]
      }, by=contour.i]
    )
  }
}
(class.pred.dt <- rbindlist(class.pred.dt.list))
(class.point.dt <- rbindlist(class.point.dt.list))

set.colors <- c(
  train="#1B9E77",
  test="#D95F02",
  unused="white")
algo.colors <- c(
  featureless="blue",
  rpart="red")
make_person_subset <- function(DT){
  DT[, "person/subset" := person]
}
make_person_subset(class.point.dt)
make_person_subset(class.bench.score)
if(require(animint2)){
  viz <- animint(
    title="SOAK algorithm: train/predict on subsets, classification",
    video="https://vimeo.com/1053464329",
    pred=ggplot()+
      ggtitle("Predictions for selected train/test split")+
      theme_animint(height=700, width=700, rowspan=2)+
      scale_fill_manual(values=set.colors)+
      scale_color_manual(values=c(spam="black","not spam"="white"))+
      geom_point(aes(
        x1, x2, color=label, fill=set.name),
        showSelected="iteration",
        size=3,
        help="One dot for each train/test/unused data point.",
        stroke=2,
        shape=21,
        data=class.point.dt)+
      geom_path(aes(
        x1, x2, 
        group=paste(algorithm, iteration, contour.i)),
        showSelected=c("iteration","algorithm"),
        help="Red path represents decision boundary of rpart decision tree learning algorithm.",
        color=algo.colors[["rpart"]],
        data=class.pred.dt)+
      facet_grid(
        task_id ~ `person/subset`,
        labeller=label_both,
        space="free",
        scales="free")+
      scale_y_continuous(
        breaks=seq(-100, 100, by=2)),
    err=ggplot()+
      ggtitle("Test error for each split")+
      theme_animint(height=400, width=400, last_in_row=TRUE)+
      theme(panel.margin=grid::unit(1, "lines"))+
      scale_y_continuous(
        "Classification error on test set",
        breaks=seq(0, 1, by=0.25))+
      scale_fill_manual(values=algo.colors)+
      scale_x_discrete(
        "People/subsets in train set")+
      geom_hline(aes(
        yintercept=yint),
        help="Horizontal lines highlight baseline error rate of 50%.",
        data=data.table(yint=0.5),
        color="grey50")+
      geom_point(aes(
        train.subsets, classif.ce, fill=algorithm),
        help="One dot per test set and learning algorithm.",
        shape=1,
        size=5,
        stroke=2,
        color="black",
        color_off=NA,
        clickSelects="iteration",
        data=class.bench.score)+
      facet_grid(
        task_id ~ `person/subset`,
        labeller=label_both),
    diagram=ggplot()+
      ggtitle("Select train/test split")+
      theme_animint(height=300, width=400)+
      facet_grid(
        . ~ train.subsets,
        scales="free",
        space="free")+
      scale_size_manual(values=c(subset=3, fold=1))+
      scale_color_manual(values=c(subset="orange", fold="grey50"))+
      geom_rect(aes(
        xmin=-Inf, xmax=Inf,
        color=rows,
        size=rows,
        ymin=display_row, ymax=display_end),
        help="One rect per chunk of data with common fold (grey) and subset (gold).",
        fill=NA,
        data=inst$viz.rect.dt)+
      scale_fill_manual(values=set.colors)+
      geom_label_aligned(aes(
        x=ifelse(rows=="subset", Inf, -Inf),
        y=(display_row+display_end)/2,
        color=rows,
        hjust=ifelse(rows=="subset", 1, 0),
        label=paste0(rows, "=", ifelse(rows=="subset", subset, fold))),
        help="Text labels indicate chunks of data with common fold (grey) and subset (gold).",
        showSelected="rows",
        data=data.table(train.name="same", inst$viz.rect.dt))+
      geom_rect(aes(
        xmin=iteration-rect.expand, ymin=display_row,
        xmax=iteration+rect.expand, ymax=display_end,
        fill=set.name),
        help="One rect per chunk of data assigned to train/test set in cross-validation.",
        alpha=0.5,
        alpha_off=0.5,
        color="black",
        color_off=NA,
        clickSelects="iteration",
        data=inst$viz.set.dt)+
      scale_x_continuous(
        "Split number / cross-validation iteration",
        breaks=c(1,6, 7,12, 13,18))+
      scale_y_continuous(
        "Row number"),
    first=list(iteration=10),
    source="https://github.com/tdhock/mlr3resampling/blob/main/vignettes/Older_resamplers.Rmd")
}
if(FALSE){
  animint2pages(viz, "2023-12-13-train-predict-subsets-classification", chromote_sleep_seconds=5)
}

## -----------------------------------------------------------------------------
N <- 300
abs.x <- 10
set.seed(1)
x.vec <- runif(N, -abs.x, abs.x)
str(x.vec)

## -----------------------------------------------------------------------------
reg.pattern.list <- list(
  sin=sin,
  constant=function(x)0)

## -----------------------------------------------------------------------------
library(data.table)
reg.task.list <- list()
reg.data.list <- list()
for(task_id in names(reg.pattern.list)){
  f <- reg.pattern.list[[task_id]]
  task.dt <- data.table(
    x=x.vec,
    y = f(x.vec)+rnorm(N,sd=0.5))
  reg.data.list[[task_id]] <- data.table(task_id, task.dt)
  reg.task.list[[task_id]] <- mlr3::TaskRegr$new(
    task_id, task.dt, target="y"
  )
}
(reg.data <- rbindlist(reg.data.list))

## -----------------------------------------------------------------------------
if(require(animint2)){
  ggplot()+
    print_theme+
    geom_point(aes(
      x, y),
      data=reg.data)+
    facet_grid(task_id ~ ., labeller=label_both)
}

## -----------------------------------------------------------------------------
reg_size_cv <- mlr3resampling::ResamplingVariableSizeTrainCV$new()
reg_size_cv$param_set$values$train_sizes <- 6
reg_size_cv

## -----------------------------------------------------------------------------
reg_size_cv$instantiate(reg.task.list[["sin"]])
reg_size_cv$instance

## -----------------------------------------------------------------------------
(reg.learner.list <- list(
  if(requireNamespace("rpart"))mlr3::LearnerRegrRpart$new(),
  mlr3::LearnerRegrFeatureless$new()))

## -----------------------------------------------------------------------------
(reg.bench.grid <- mlr3::benchmark_grid(
  reg.task.list,
  reg.learner.list,
  reg_size_cv))

## -----------------------------------------------------------------------------
if(FALSE){
  if(require(future))plan("multisession")
}
if(require(lgr))get_logger("mlr3")$set_threshold("warn")
(reg.bench.result <- mlr3::benchmark(
  reg.bench.grid, store_models = TRUE))

## -----------------------------------------------------------------------------
reg.bench.score <- mlr3resampling::score(reg.bench.result)
reg.bench.score[1]

## -----------------------------------------------------------------------------
train_size_vec <- unique(reg.bench.score$train_size)
if(require(animint2)){
  ggplot()+
    print_theme+
    scale_x_log10(
      breaks=train_size_vec)+
    scale_y_log10()+
    geom_line(aes(
      train_size, regr.mse,
      group=paste(algorithm, seed),
      color=algorithm),
      shape=1,
      data=reg.bench.score)+
    geom_point(aes(
      train_size, regr.mse, color=algorithm),
      shape=1,
      data=reg.bench.score)+
    facet_grid(
      test.fold~task_id,
      labeller=label_both,
      scales="free")
}

## -----------------------------------------------------------------------------
reg.mean.dt <- dcast(
  reg.bench.score,
  task_id + train_size + test.fold + algorithm ~ .,
  list(mean, sd),
  value.var="regr.mse")
if(require(animint2)){
  ggplot()+
    print_theme+
    scale_x_log10(
      breaks=train_size_vec)+
    scale_y_log10()+
    geom_ribbon(aes(
      train_size,
      ymin=regr.mse_mean-regr.mse_sd,
      ymax=regr.mse_mean+regr.mse_sd,
      fill=algorithm),
      alpha=0.5,
      data=reg.mean.dt)+
    geom_line(aes(
      train_size, regr.mse_mean, color=algorithm),
      shape=1,
      data=reg.mean.dt)+
    facet_grid(
      test.fold~task_id,
      labeller=label_both,
      scales="free")
}

## ----ResamplingVariableSizeTrainCVAnimintRegression---------------------------
grid.dt <- data.table(x=seq(-abs.x, abs.x, l=101), y=0)
grid.task <- mlr3::TaskRegr$new("grid", grid.dt, target="y")
pred.dt.list <- list()
point.dt.list <- list()
for(score.i in 1:nrow(reg.bench.score)){
  reg.bench.row <- reg.bench.score[score.i]
  task.dt <- data.table(
    reg.bench.row$task[[1]]$data(),
    reg.bench.row$resampling[[1]]$instance$id.dt)
  set.ids <- data.table(
    set.name=c("test","train")
  )[
  , data.table(row_id=reg.bench.row[[set.name]][[1]])
  , by=set.name]
  i.points <- set.ids[
    task.dt, on="row_id"
  ][
    is.na(set.name), set.name := "unused"
  ]
  point.dt.list[[score.i]] <- data.table(
    reg.bench.row[, .(task_id, iteration)],
    i.points)
  i.learner <- reg.bench.row$learner[[1]]
  pred.dt.list[[score.i]] <- data.table(
    reg.bench.row[, .(
      task_id, iteration, algorithm
    )],
    as.data.table(
      i.learner$predict(grid.task)
    )[, .(x=grid.dt$x, y=response)]
  )
}
(pred.dt <- rbindlist(pred.dt.list))
(point.dt <- rbindlist(point.dt.list))
set.colors <- c(
  train="#1B9E77",
  test="#D95F02",
  unused="white")
algo.colors <- c(
  featureless="blue",
  rpart="red")
if(require(animint2)){
  viz <- animint(
    title="Variable size train set, regression",
    pred=ggplot()+
      ggtitle("Predictions for selected train/test split")+
      theme_animint(height=400)+
      scale_fill_manual(values=set.colors)+
      geom_point(aes(
        x, y, fill=set.name),
        help="One dot per sample in train/test/unused set.",
        showSelected="iteration",
        size=3,
        shape=21,
        data=point.dt)+
      scale_size_manual(values=c(
        featureless=3,
        rpart=2))+
      scale_color_manual(values=algo.colors)+
      geom_line(aes(
        x, y,
        color=algorithm,
        size=algorithm,
        group=paste(algorithm, iteration)),
        help="One line per learned prediction function.",
        showSelected="iteration",
        data=pred.dt)+
      facet_grid(
        task_id ~ .,
        labeller=label_both),
    err=ggplot()+
      ggtitle("Test error for each split")+
      theme_animint(width=500)+
      theme(
        panel.margin=grid::unit(1, "lines"),
        legend.position="none")+
      scale_y_log10(
        "Mean squared error on test set")+
      scale_color_manual(values=algo.colors)+
      scale_x_log10(
        "Train set size",
        breaks=train_size_vec)+
      geom_line(aes(
        train_size, regr.mse,
        group=paste(algorithm, seed),
        color=algorithm),
        help="One line per algorithm and random seed used to order train set.",
        clickSelects="seed",
        alpha_off=0.2,
        showSelected="algorithm",
        size=4,
        data=reg.bench.score)+
      facet_grid(
        test.fold~task_id,
        labeller=label_both,
        scales="free")+
      geom_point(aes(
        train_size, regr.mse,
        color=algorithm),
        help="One point per algorithm and train set size, for the selected random ordering.",
        size=5,
        stroke=3,
        fill="black",
        fill_off=NA,
        showSelected=c("algorithm","seed"),
        clickSelects="iteration",
        data=reg.bench.score),
    video="https://vimeo.com/1053467310",
    source="https://github.com/tdhock/mlr3resampling/blob/main/vignettes/Older_resamplers.Rmd")
}
if(FALSE){
  animint2pages(viz, "2023-12-26-train-sizes-regression", chromote_sleep_seconds = 5)
}

## -----------------------------------------------------------------------------
class.N <- 900
class.abs.x <- 1
rclass <- function(){
  runif(class.N, -class.abs.x, class.abs.x)
}
library(data.table)
set.seed(1)
class.x.dt <- data.table(x1=rclass(), x2=rclass())
class.fun.list <- list(
  constant=function(...)0.5,
  xor=function(x1, x2)xor(x1>0, x2>0))
class.data.list <- list()
class.task.list <- list()
for(task_id in names(class.fun.list)){
  class.fun <- class.fun.list[[task_id]]
  y <- factor(ifelse(
    class.x.dt[, class.fun(x1, x2)+rnorm(class.N, sd=0.5)]>0.5,
    "spam", "not"))
  task.dt <- data.table(class.x.dt, y)
  this.task <- mlr3::TaskClassif$new(
    task_id, task.dt, target="y")
  this.task$col_roles$stratum <- "y"
  class.task.list[[task_id]] <- this.task
  class.data.list[[task_id]] <- data.table(task_id, task.dt)
}
(class.data <- rbindlist(class.data.list))

## -----------------------------------------------------------------------------
class.data[, .(count=.N), by=.(task_id, y)]

## -----------------------------------------------------------------------------
if(require(animint2)){
  ggplot()+
    print_theme+
    geom_point(aes(
      x1, x2, color=y),
      shape=1,
      data=class.data)+
    facet_grid(. ~ task_id, labeller=label_both)+
    coord_equal()
}

## -----------------------------------------------------------------------------
class.learner.list <- list(
  if(requireNamespace("rpart"))mlr3::LearnerClassifRpart$new(),
  mlr3::LearnerClassifFeatureless$new())
size_cv <- mlr3resampling::ResamplingVariableSizeTrainCV$new()
(class.bench.grid <- mlr3::benchmark_grid(
  class.task.list,
  class.learner.list,
  size_cv))

## -----------------------------------------------------------------------------
if(FALSE){
  if(require(future))plan("multisession")
}
if(require(lgr))get_logger("mlr3")$set_threshold("warn")
(class.bench.result <- mlr3::benchmark(
  class.bench.grid, store_models = TRUE))

## -----------------------------------------------------------------------------
class.bench.score <- mlr3resampling::score(class.bench.result)
class.bench.score[1]

## -----------------------------------------------------------------------------
if(require(animint2)){
  ggplot()+
    print_theme+
    geom_line(aes(
      train_size, classif.ce,
      group=paste(algorithm, seed),
      color=algorithm),
      shape=1,
      data=class.bench.score)+
    geom_point(aes(
      train_size, classif.ce, color=algorithm),
      shape=1,
      data=class.bench.score)+
    facet_grid(
      task_id ~ test.fold,
      labeller=label_both)+
    scale_x_log10(
      breaks=unique(class.bench.score$train_size))+
    scale_y_continuous(
      "Test error rate",
      limits=c(0.1,0.6),
      breaks=seq(0.1,0.6,by=0.1))
}

## ----ResamplingVariableSizeTrainCVAnimintClassification-----------------------
class.grid.vec <- seq(-class.abs.x, class.abs.x, l=21)
class.grid.dt <- CJ(x1=class.grid.vec, x2=class.grid.vec)
class.pred.dt.list <- list()
class.point.dt.list <- list()
for(score.i in 1:nrow(class.bench.score)){
  class.bench.row <- class.bench.score[score.i]
  task.dt <- data.table(
    class.bench.row$task[[1]]$data(),
    class.bench.row$resampling[[1]]$instance$id.dt)
  set.ids <- data.table(
    set.name=c("test","train")
  )[
  , data.table(row_id=class.bench.row[[set.name]][[1]])
  , by=set.name]
  i.points <- set.ids[
    task.dt, on="row_id"
  ][
    is.na(set.name), set.name := "unused"
  ][]
  class.point.dt.list[[score.i]] <- data.table(
    class.bench.row[, .(task_id, iteration)],
    i.points)
  if(class.bench.row$algorithm!="featureless"){
    i.learner <- class.bench.row$learner[[1]]
    i.learner$predict_type <- "prob"
    i.task <- class.bench.row$task[[1]]
    grid.class.task <- mlr3::TaskClassif$new(
      "grid", class.grid.dt[, label:=factor(NA,levels(task.dt$y))], target="label")
    pred.grid <- as.data.table(
      i.learner$predict(grid.class.task)
    )[, data.table(class.grid.dt, prob.spam)]
    pred.wide <- dcast(pred.grid, x1 ~ x2, value.var="prob.spam")
    prob.mat <- as.matrix(pred.wide[,-1])
    if(length(table(prob.mat))>1){
      contour.list <- contourLines(
        class.grid.vec, class.grid.vec, prob.mat, levels=0.5)
      class.pred.dt.list[[score.i]] <- data.table(
        class.bench.row[, .(
          task_id, iteration, algorithm
        )],
        data.table(contour.i=seq_along(contour.list))[, {
          do.call(data.table, contour.list[[contour.i]])[, .(level, x1=x, x2=y)]
        }, by=contour.i]
      )
    }
  }
}
(class.pred.dt <- rbindlist(class.pred.dt.list))
(class.point.dt <- rbindlist(class.point.dt.list))

set.colors <- c(
  train="#1B9E77",
  test="#D95F02",
  unused="white")
algo.colors <- c(
  featureless="blue",
  rpart="red")
if(require(animint2)){
  viz <- animint(
    title="Variable size train sets, classification",
    pred=ggplot()+
      ggtitle("Predictions for selected train/test split")+
      theme(panel.margin=grid::unit(1, "lines"))+
      theme_animint(width=600)+
      coord_equal()+
      scale_fill_manual(values=set.colors)+
      scale_color_manual(values=c(spam="black","not spam"="white"))+
      geom_point(aes(
        x1, x2, color=y, fill=set.name),
        showSelected="iteration",
        help="One dot per data sample in the train/test/unused set.",
        size=3,
        stroke=2,
        shape=21,
        data=class.point.dt)+
      geom_path(aes(
        x1, x2, 
        group=paste(algorithm, iteration, contour.i)),
        showSelected=c("iteration","algorithm"),
        help="Red path represents decision boundary of rpart decision tree learning algorithm.",
        color=algo.colors[["rpart"]],
        data=class.pred.dt)+
      facet_grid(
        . ~ task_id,
        labeller=label_both,
        space="free",
        scales="free"),
    err=ggplot()+
      ggtitle("Test error for each split")+
      theme_animint(height=400)+
      theme(panel.margin=grid::unit(1, "lines"))+
      scale_y_continuous(
        "Classification error on test set",
        limits=c(0.1,0.6),
        breaks=seq(0.1,0.6,by=0.1))+
      scale_color_manual(values=algo.colors)+
      scale_x_log10(
        "Train set size",
        breaks=unique(class.bench.score$train_size))+
      geom_line(aes(
        train_size, classif.ce,
        group=paste(algorithm, seed),
        color=algorithm),
        help="One line per algorithm and random seed used to order train set.",
        clickSelects="seed",
        alpha_off=0.2,
        showSelected="algorithm",
        size=4,
        data=class.bench.score)+
      facet_grid(
        test.fold~task_id,
        labeller=label_both,
        scales="free")+
      geom_point(aes(
        train_size, classif.ce,
        color=algorithm),
        size=5,
        stroke=3,
        fill="black",
        fill_off=NA,
        help="One point per algorithm and train set size, for the selected random ordering.",
        showSelected=c("algorithm","seed"),
        clickSelects="iteration",
        data=class.bench.score),
    video="https://vimeo.com/1053477025",
    source="https://github.com/tdhock/mlr3resampling/blob/main/vignettes/Older_resamplers.Rmd")
}
if(FALSE){
  animint2pages(viz, "2023-12-27-train-sizes-classification", chromote_sleep_seconds = 5)
}

## -----------------------------------------------------------------------------
sessionInfo()

