# scraps--modeling with classification trees


ntropy in the predicted leaver group would be: -.58 x log(.58) - .42 x log(.42)) = .68.

But is 500,000 the best value of `house` to use for splitting the data?  (We chose that number arbitrarily.)  Here is a plot showing the entropy of the predicted leaver group associated with splitting the data at different values of house (this resembles  figure 3-3 in DSB).



## Simple model

Our goal is to create a model of `leave`. Before getting into the complications of a tree model let's consider a very simple model consisting in a rule: always predict the majority class.  The majority class is the most frequent class label in the data.  Use `table()` and `prop.table()` to calculate frequencies in the data to identify the majority class:  how many customers left and how many stayed? 

```{r table, exercise=TRUE, exercise.lines = 5}


```

```{r table-hint}
table(d$leave)

table(d$leave) %>% 
  prop.table %>% 
  round(2)

```

`STAY` is the majority class by a slight margin, at 51%.  Now, suppose we always predicted the majority class. How good would the model be at predicting the event we are interested in ---`LEAVE`?  The model would be correct 49% of the time and incorrect 51% of the time.  Obviously, this is not a very clever model, nor would it be very accurate (being wrong more than half the time), but it serves as a useful performance benchmark for subsequent modeling. Whatever  model we develop for predicting `LEAVE` should  do better than 49% accuracy.  We will keep this in mind.

Note that accuracy is an important concept in evaluating the  performance of classification  models, and is defined as simply the proportion of correct predictions. Our majority class model, in always predicting `STAY`, would correctly predict `LEAVE` 49% of the time and the model's accuracy would be .49.

## Confusion matrix

We can summarize the results of any classification model in a confusion matrix, so named because it shows where the model is confused, tallying how many times the model correctly and incorrectly predicts the event happening (in this case the "event" we are interested in predicting is `LEAVE`) and and how many times it correctly and incorrectly predicts the event not happening (`STAY`). Notice that there are four possibilities here:
  
  - predict event correctly, known as a "true positive" (TP)
- predict event incorrectly, known as a "false positive" (FP)
- predict no event correctly, known as a "true negative" (TN)
- predict no event incorrectly, known as a "false negative" (FN)

The four possibilities fit into a 2 x 2 matrix:
  
  ```{r}
cm <- table(predicted = d$leave,
            observed = rep(c("LEAVE","STAY"), nrow(d)/2))

cm[1,1] <- "TP"
cm[2,1] <- "FN"
cm[1,2] <- "FP"
cm[2,2] <- "TN"

cm 
```

Here are the actual numbers from the majority class model:
  
  ```{r}
cm <- table(predicted = rep("STAY", nrow(d)),
            observed = d$leave)

cm <- rbind(cm, c(0,0))

row.names(cm)[2] <- "STAY"

cm <- as.table(cm)

names(dimnames(cm)) <- c("predicted","observed")

cm

```

How to read this?  The model's predictions are in the rows, and the observed values in the data are in the columns. This model only predicts `LEAVE` which is why the entries in the secnd row are all 0.  In the first row, the model predicts `LEAVE`


Here is nother example of a simple model. 

In the last tutorial we noticed that `house` had a fairly strong relationship to  the target variable,`leave`. Write code to make a boxplot of `leave ~ house`:


```{r house-plot, exercise=TRUE}

  
```


```{r house-plot-hint}

ggplot(d, aes(leave, house)) +
  geom_boxplot() +
  coord_flip() + 
  labs(title = "leave ~ house")
  
```

Suppose we created a model of `leave`, based on this boxplot, consisting in the following rule: for all customers with houses worth more than 500,000  we predict `STAY` and for those with houses worth less than 500,000 we predict `LEAVE`. Would this model have better accuracy than  the majprity class benchmark of .51? 

We can summarize the performance of this new model with what is known as a "confusion matrix," a 2 x 2 table that indicates when the model's predictions (listed in the rows) were correct with reference to observed or actual values (listed in the columns).

```{r acc_table, exercise =TRUE}
# Create prediction table
table(prediction = ifelse(d$house > 500000, "STAY", "LEAVE"), 
      observed = d$leave)

```

Accuracy for this model can be calculated by adding the counts from the cells along the diagonal from  the upper left (known as true positives) to the lower right (true negatives) and dividing by the total number of observations:
  
  ```{r calc_acc, exercise =TRUE}
# Calculate accuracy

(1592 + 1380) / (1592 + 1380 + 876 + 1146)

```

So, this model does offer an improvement over the benchmark accuracy of .51.  However, the predicted groups are significantly impure, in the technical sense of being mixed, with the predicted leave group, for example, consisting in only 1592 / (1592 + 1146) or .58 actual leavers (the proportion of actual stayers is 1 - .58 or .42).

## Entropy

Entropy formalizes the notion of impurity.   It ranges between 0 and 1, with 0 indicating  no  disorder (the group is pure)  and 1 indicating maximal disorder (the group is balanced). Entropy is  formally defined as:
  
  $entropy = -p_1 log(p_1) - p_2 log(p_2) ...$
  
  In the model above, entropy in the predicted leaver group would be: -.58 x log(.58) - .42 x log(.42)) = .68.

But is 500,000 the best value of `house` to use for splitting the data?  (We chose that number arbitrarily.)  Here is a plot showing the entropy of the predicted leaver group associated with splitting the data at different values of house (this resembles  figure 3-3 in DSB).

```{r include = F}
# Create a data.frame of house prices from min to max by 1000
# and an empty column for storing the corresponding entropy
df <- data.frame(house = seq(min(d$house) + 1000, max(d$house), by = 500),
                 entropy = 0,
                 info_gain = 0)

# Check the data frame
# head(df)

# parent entropy
parent_entropy <- d %>%
  summarize(p1 = mean(ifelse(leave=="STAY", 1, 0)),
            p2 = 1-p1,
            entropy = -p1*log(p1)-p2*log(p2)) %>%
  dplyr::select(entropy) %>% 
  as.numeric

# Set up a loop to compute entropies
for(i in seq_along(df$house)){
  temp <- d %>%
    mutate(predict = ifelse(house > df$house[i], "LEAVE", "STAY"),
           leave = leave) %>%
    count(predict, leave) %>%
    group_by(predict) %>%
    mutate(perc = n/sum(n),
           entropy = -(first(perc)* log(first(perc)) + last(perc)* log(last(perc)))) %>% 
    group_by(predict) %>% 
    mutate(total = sum(n)) %>%
    slice(1) %>% 
    dplyr::select(predict, entropy, total) %>% 
    ungroup %>% 
    mutate(perc=total/sum(total)) 
  
  df$entropy[i] <- -(temp$perc[1] * log(temp$perc[1]) + temp$perc[2] * log(temp$perc[2]))
  
  df$info_gain[i] <- parent_entropy - (temp$perc[1] * temp$entropy[1] +
                                         temp$perc[2] * temp$entropy[2])
}



ggplot(df, aes(house, entropy)) +
  geom_line() +
  geom_vline(xintercept = 500000, col=2, lty=2) +
  labs(title="Entropy in the predicted leaver group at different splits on house",
       subtitle="Red dashed line at 500k") +
  theme_minimal()

```


Our choice to split at 500,000 (the red dashed line) actually produced groups with near-maximum entropy. This is where the metric of *information gain* is helpful.  

## Information gain

What we'd really like to know is which split most *decreases* entropy in the  resulting two groups (known as children) as compared to the original group (parent).  

$$IG(parent, children) = entropy(parent) - [p(c_1) entropy(c_1) + p(c_2) entropy(c_2) + ...]$$

Assuming a split at `house` = 500,000 for the churn problem, we can calculate the following components of information gain:

```{r include = F}

table(ifelse(d$house > 500000, "STAY", "LEAVE"), d$leave)

# parent entropy
prop.table(table(d$leave))
-.49 * log(.49) - .51 * log(.51)

#pc1
(1592 + 1146)/4994 # = .55

# pc2
(876 + 1380)/ 4994 # .45

# entropy c1
1592/(1592+1146) #=.58
-.58 * log(.58) - .42 * log(.42) #= .68

#entropy c2
876/(876+1380) #=.39
-.39 * log(.39) - .61 * log(.61) #=.67

.69 - (.55*.68 + .45*.67)

```

- $entropy(parent)$: -.49 x log(.49) - .51 x log(.51) = .69
- $p(c_1)$: .55
- $p(c_2)$: 1 - .55 = .45
- $entropy(c_1)$: -.58 x log(.58) - .42 x log(.42) = .68.
- $entropy(c_2)$: -.61 x log(.61) - .39 x log(.39) = .67

$IG = .69 - [.55(.68) + .45(.67) ] = .015$


This calculation is a little off due to rounding error.  Below is a plot showing the exact information gain associated with each potential split on house. Ideally we would pick a value of `house` that maximized information gain so as to produce groups with  *minimum* entropy. `house` at maximum information gain is about 600,000 (black dashed line).

```{r}

names(df)[3] <- "Information gain"

ggplot(df, aes(house, `Information gain`)) +
  geom_line() +
  geom_vline(xintercept = 500000, col=2, lty=2) +
  geom_vline(xintercept = df[which(df$`Information gain`==max(df$`Information gain`)), "house"], lty=2)+
  labs(title="Information gain at different splits on house",
       subtitle="Red dashed line at 500k, black dashed line at max IG") + 
  theme_minimal()

```


## Classification Trees

Classification trees use information gain to select splits.  The tree algorithm does an efficient search for the best split among all the available predictors---the split that most increases information gain.  The search is "greedy" in the sense that the algorithm looks for the best split conditional on all the previous splits. This makes the splits locally optimal, with no guarantee of being globally optimal.

>A greedy algorithm is an algorithmic paradigm that follows the problem solving heuristic of making the locally optimal choice at each stage with the intent of finding a global optimum. In many problems, a greedy strategy does not usually produce an optimal solution, but nonetheless a greedy heuristic may yield locally optimal solutions that approximate a globally optimal solution in a reasonable amount of time. [Wikipedia](https://en.wikipedia.org/wiki/Greedy_algorithm)

In other words, a greedy algorithm always chooses the best step at a given point, which is not necessarily (though probably gets close to) the best step overall.

As described in DSB, a classification tree can be visualized as an upside down tree with the "root" at the top and "nodes" at each split or decision point.

![](images/class_tree.png)

The tree is finished when the algorithm can find no additional information gain-improving splits.  The resulting "leaf" nodes ideally define groups that have low entropy.  The majority class in each leaf node will be used as the model's prediction for observations sharing those characteristics.  In the above Figure 3-10 from DSB, the group in the bottom right leaf node is defined as:  not employed, balance $\ge$ 50k, age $\ge$ 45. The majority of the individuals in this group have observed outcomes of Write-off; that will be the class label assigned to this group by the model and used in the future for prediction for individuals with these characteristics when the outcome is unknown.

## Classification Trees in R

We will be using the `rpart` package in R, an implementation of the CART algorithm, to fit a classification tree model to the MegaTelCo data. (`rpart` stands for "Recursive Partitioning and Regression Trees"; see the package vignette for implementation details.)  The model syntax used in this package is typical for R, consisting in a function, `rpart()`, with formula and data arguments.  Formulas in R typically take the form of `y ~ x + z`, with the tilde indicating "modeled by" or "explained by" and the predictors (here `x` and `z`) included additively.  

Let's demonstrate how to use  `rpart` for classification by fitting a simple tree---call it a stump---of `leave` with just one predictor, `house`.

```{r tree, echo=T}
# Fit tree
(tree_model1 <- rpart(leave ~ house, data = d))

```

I have assigned the model to an object, `tree_model`, which we will use below for prediction.  For now, let's work through these simplified results.

- `n =`: n is the number of rows in the data set: 4994.
- `1) root`:  The root node by definition has no split, and consists in the entire dataset: 4994 observations.  The next number, 2468, referred to as "loss" in this output, is the count of misclassifications that would occur if the majority class label in the node, `STAY`, were used for prediction.  (Incidentally, this would be identical to the first, benchmark model discussed above:  majority class prediction.) The proportions of `LEAVE` and `STAY` in the data are .49 and .51, respectively.
- `2) house < 600490`: the first split is on `house` at 600,490, with the left branch (< 600,490) producing a group of size 3306, with loss of 1364 incorrectly classified observations.  The class label prediction for this node is `LEAVE`, accounting for almost 59% of the observations in the node. This is a leaf node, as indicated by the asterisk.
- `3) house >= 600490`: the right branch produces a group of 1688, with loss of 526.  The class label prediction for this node is `STAY`, accounting for almost 69% of the observations in this leaf node.

Here is a visualization of the tree:
  
  ```{r tree-plot, exercise =TRUE}
# Plot tree object
rpart.plot(tree_model1)

```

66% of the data (3306) is in the left branch and 34% (1688) in the right.

How good is this model? Above we calculated the accuracy for the model that split at `house` = 500,000.  Let's calculate accuracy for this tree model. To do so we need to get the model's predicted class labels, which we will do by using the generic `predict()` function in base R with the type of prediction set to "class." In this case, although we are using the `predict()` function, we are not predicting in the sense of forecasting unknown values, but are rather using the function to extract the so-called "fitted values" from the model.  These are the class labels assigned by the model.

Here is an example of the output from `predict()`:
  
  ```{r tree-predict, exercise = TRUE}
# An example of the fitted values from the tree model
predict(tree_model1, type = "class") %>% 
  head

```

Try using `predict()` to create a confusion matrix for this model.

```{r model_predict, exercise = TRUE}

```

```{r model_predict-hint}

# Create confusion matrix
table(predicted = predict(tree_model1, type = "class"), 
      observed = d$leave)

```

The model has 1942 true positives and 1162 true negatives.  These are cases where the model gets it right: predictions align with the observed values. 

Calculate the model's accuracy using the values from the confusion matrix.

```{r accuracy, exercise =TRUE}

```

```{r accuracy-hint}
# Accuracy
(1942 + 1162) / (1942 + 1162 + 1364 + 526)
```

Here is an alternate method for calculating accuracy:

```{r auto-acc, exercise = TRUE}
sum(predict(tree_model1, type = "class") == d$leave) / nrow(d)

```

This performance is definitely an improvement over the model discussed above in which we arbitrarily split at `house` = 500,000.

Can we improve the model further?  Let's add all the available predictors using the `.` syntax:
  
  
  ```{r tree_model2, echo= TRUE}
# Fit tree
(tree_model2 <- rpart(leave ~ ., data = d))

# Visualize
rpart.plot(tree_model2)
```

Notice that this plot is different---indeed the model is different---from the one displayed in Figure 3-18 of DSB.  That is presumably because of differences in software implementation.  Notice in the model summary that there are missing nodes---the count jumps from 13 to 22.  The missing nodes have been "pruned" by the algorithm after growing the tree because they did not improve model performance.

Create a confusion matrix for this model and calculate accuracy.

```{r model2-confusion, exercise=TRUE}

```

```{r model2-confusion-hint}
# Create confusion matrix
table(predicted = predict(tree_model2, type = "class"), 
      observed = d$leave)
```

```{r model2-acc, exercise=TRUE}

```


```{r model2-acc-hint}
# Evaluate accuracy
sum(predict(tree_model2, type = "class") == d$leave) / nrow(d)

```

The model's accuracy has improved to .71.

What does this mean?  Well, in terms of a marketing campaign, we should be realistic about the fact that something like 30% of the customers predicted to churn, and to whom we might extend a special retention offer, will have been incorrectly classified.  (The number might actually be higher than that; we will ignore the possibility for now that the model might be overfitting.) We need to factor this error--- and uncertainty about how large the error is likely to be in practice---into any recommended approach to improving customer retention at MegaTelCo.

## Variable importance

We can get a sense of which variables are most important in predicting churn by looking at the tree plot:  the splitting variables closer to the root node are more important. However, the `rpart()` function also automatically calculates variable importance. What matters in this variable importance list is not the actual importance score (the calculation of which is somewhat obscure in this package) but the ranking:

```{r echo = T}
tree_model2$variable.importance
```

Here we can see that `overage` is actually more important factor than `house` with `over_15mins_calls_per_month` coming in third. (This ranking is different from Figure 3-17 in DSB because variable importance in `rpart()` is not identical to information gain.) This ranking can be useful when thinking about what factors might contribute to churn.  However, it is important to understand that variable importance does not imply causation.  For example, we would not want to say that certain housing values cause churn.  Notice that the surprising finding from EDA was confirmed by the tree model:  customer satisfaction is not a predictor of churn.