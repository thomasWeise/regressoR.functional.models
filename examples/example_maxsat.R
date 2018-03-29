# An Example for Testing the Behavior of the Standard Models on Some Example Data
#
# The example data stems from an experiment with a m-flip hill climber on a MAX-SAT instance

# first we load the data
data   <- read.csv("examples/example_maxsat_uf225-02_mFlipHCrs_01.txt", sep="\t");
data.x <- data[[1]];
data.y <- data[[3]];

# make the default plot
plot(log(data.x), data.y);

# then we load the models
models <- regressoR.functional.models::FunctionalModel.all();
names  <- sapply(models, as.character);
count  <- length(models);

# now we assign colors
# we first make a normal list of colors and create darker versions as needed

# source: https://gist.github.com/Jfortin1/72ef064469d1703c6b30
darken <- function(color, factor=1.7){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

# the original color vector
colors.orig  <- c("red", "blue", "green", "orange",  "violet",
                  "cyan", "gold", "gray", "darkviolet");
colors  <- colors.orig;
# if we do not have enough colors, synthesize more
while(length(colors) < count) {
  colors <- unique(unlist(c(colors, sapply(colors.orig, darken))));
}
if(length(colors) > count) { colors <- colors[1:count]; }
# OK, now we have exacty the right number of colors

# add a legend to the plot
legend("topright",
        text.col=as.vector(colors),
        legend=names);

# prepare the quality metric
metric    <- regressoR.quality::RegressionQualityMetric.default(data.x, data.y);

# we will keep running statistics about the progress, which initially are NULL
qualities <- NULL;
times     <- NULL;

# we will do at most 100 iterations ... this will take a long time
for(i in 1L:100L) {

  # fit all the models, while measuring the required time, and also plot the lines
  results <- lapply(X=1:count, FUN=function(m) {
      time <- system.time(result <- regressoR.functional::FunctionalModel.fit(metric, models[[m]]))[3];

      # plot the lines
      if(!(is.null(result))) {
        cat(models[[m]]@name, ": quality", result@quality, " in ", time, "s, par=c(", paste(result@par, sep="", collapse=", "), ")\n");
        lines(log(data.x), result@f(data.x), col=colors[[m]]);
      } else {
        cat(models[[m]]@name, ": failed after ", time, "s", "\n");
      }

      return(list(time=time, result=result));
    });


# line plottig now already done inside lapply
#  for(j in 1:count) {
#    if(!(is.null(results[[j]]$result))) {
#      lines(log(data.x), results[[j]]$result@f(data.x), col=colors[[j]]);
#    }
#  }

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

  # print the state after the iteration

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
