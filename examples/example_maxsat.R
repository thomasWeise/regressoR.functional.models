# An Example for Testing the Behavior of the Standard Models on Some Example Data
#
# The example data stems from an experiment with a m-flip hill climber on a MAX-SAT instance

# first we load the data
data   <- read.csv("examples/example_maxsat_uf225-02_mFlipHCrs_01.txt", sep="\t");
data.x <- data[[1]];
data.y <- data[[3]];

# make the default plot
plot(log(data.x), data.y);

# then we load the models and assign colors to them
models  <- regressoR.functional.models::FunctionalModel.all();
count   <- length(models);
colors  <- c("red", "blue", "green", "orange", "darkgreen", "violet",
             "cyan", "gold", "darkblue", "darkred", "darkmagenta",
             "aquamarine", "gray", "tan", "darkgray");
if(length(colors) < count) {
  colors <- unique(unlist(c(colors, rainbow(count))));
  if(length(colors) > count) {
    colors <- colors[1:count];
  }
}
if(length(colors) > count) { colors <- colors[1:count]; }
names <- sapply(models, as.character);

legend("topright",
        text.col=as.vector(colors),
        legend=names);

# prepare the metric
metric    <- regressoR.quality::RegressionQualityMetric.default(data.x, data.y);
qualities <- NULL;
times     <- NULL;

# add the lines
for(i in 1L:100L) {
  results <- lapply(X=models, FUN=function(m) {
      time <- system.time(result <- regressoR.functional::FunctionalModel.fit(metric, m))[3];
      return(list(time=time, result=result));
    });

  # plot the lines
  for(j in 1:count) {
    if(!(is.null(results[[j]]$result))) {
      lines(log(data.x), results[[j]]$result@f(data.x), col=colors[[j]]);
    }
  }

  # update qualities list
  .temp   <- vapply(X=results,
                    FUN=function(res) if(is.null(res$result)) +Inf else res$result@quality,
                    FUN.VALUE = +Inf);
  if(is.null(qualities)) { qualities <- matrix(.temp, nrow=1, ncol=count); }
  else                   { qualities <- rbind(qualities, .temp); }

  # update measured times
  .temp <- vapply(X=results,
                  FUN=function(res) res$time,
                  FUN.VALUE = +Inf);
  if(is.null(times)) { times <- matrix(.temp, nrow=1, ncol=count); }
  else               { times <- rbind(times, .temp); }

  cat("\n######## Iteration ", i, "########\n");

  prmatrix(
    rbind(c("model", "|", "q/med", "q/q.1", "q/q.9", "|", "q/count", "|", "t/med", "t/q.1", "t/q.9"),
    t(as.matrix(sapply(X=1:count,
           FUN=function(i) {
             c(names[[i]],
               "|",
               round(as.vector(unname(unlist(quantile(x=qualities[,i], probs=c(0.5, 0.1, 0.9))))), 3),
               "|",
               sum(is.finite(qualities[,i])),
               "|",
               round(as.vector(unname(unlist(quantile(x=times[,i], probs=c(0.5, 0.1, 0.9))))), 3))
           })))),
    quote = FALSE, rowlab=rep("", count+1L), collab=rep("", 11));
}
