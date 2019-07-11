### The profit curve

The profit curve is related to the confusion matrix.  When we use the tree model to predict a class we are implicitly using a probability threshold of .5 to classify observations.  Why?  Recall that the tree uses the majority class in a given leaf node to model observations in that node. The majority represents a proportion of observations greater than .5, and this proportion, as we've noted, is the probability.  But note that there is nothing magic about the .5 probability threshold for classifying observations.  Using different thresholds produces different sorts of model errors in the confusion matrices.  Observe:

Using `predict(tree_model, type = "class")` is identical to using a probability threshold of .5 as the class cutoff.

```{r echo = T}
table(predicted = predict(tree_model, type = "class"),
      observed = d$leave)

table(predicted = ifelse(predict(tree_model, type = "prob")[,1] > .5, "LEAVE", "STAY"),
      observed = d$leave)


```

With the default probability threshold the model's precision, defined as TP / (TP + FP), is:  1930 / (1930 + 919) = .68.  

Changing the probability threshold changes the sorts of errors the model makes. If we *increase* the probability for determining whether to predict `LEAVE` then we will make fewer errors in positive predictions, which is to say that the model's precision will go up.

```{r echo = T}

table(predicted = ifelse(predict(tree_model, type = "prob")[,1] > .7, "LEAVE", "STAY"),
      observed = d$leave)


```

With a probaiblity threshold of .7 the TPR gone up: 1076 / (1076 + 297) = .78.

And if we *decrease* the probability for determining whether to predict `LEAVE` then we make more errors when predicting `LEAVE`.

```{r echo = T}

table(predicted = ifelse(predict(tree_model, type = "prob")[,1] > .3, "LEAVE", "STAY"),
      observed = d$leave)


```

2267 / (2267 + 1609) = .58.

The profit curve essentially visualizes what happens to additional profit at different probability thresholds. And because the data is sorted on probability, those probability thresholds correspond to different proportions of customers.

```{r}
data.frame(c("", "predicted", "LEAVE","STAY"),
           c("observed", "LEAVE","STAY"))

```

```{r echo = T}

revenue <- 1000
cost <- 100

d %>%
  mutate(probability = predict(tree_model, type = "prob")[,1],
         increased_revenue = ifelse(leave == "STAY", -cost, revenue - cost)) %>%
  group_by(probability) %>% 
  summarize(revenue = mean(increased_revenue))

d %>%
  mutate(probability = predict(tree_model, type = "prob")[,1],
         increased_revenue = ifelse(leave == "STAY", -cost, revenue - cost)) %>%
  arrange(probability) %>% 
  mutate(cum_increased_rev = cumsum(increased_revenue)) %>% 
  ggplot(aes(probability, cum_increased_rev))+
  geom_line()
  group_by(probability) %>% 
  summarize(n=n(),
            avg_increased_revenue = mean(eb_t),
            sum(eb_nt))
  mutate(cum_n = cumsum(n),
         cost = n * 100,
         revenue_increase = n * probability * 1000 - cost,
         cum_revenue_increase = cumsum(revenue_increase)) %>% 
  ggplot(aes(probability, cum_revenue_increase))  +
  geom_line(stat = "identity")
  
d %>%
  mutate(probability = predict(tree_model, type = "prob")[,1]) %>%
  arrange(probability) %>% 
  mutate(row = 1:n(),
         revenue_increase = ifelse(leave == "LEAVE", 1000 -100, -100),
         cum_revenue_increase = cumsum(revenue_increase)) %>% 
  ggplot(aes(row, cum_revenue_increase))  +
  geom_line()
  count(probability) %>% 
  mutate(cost = n * 100,
         revenue_increase = n * probability * 1000 - cost)
  
  
```
## Report

Your next task is to write a report. You will very likely present your findings in a presentation,  which is the main way of disseminating information in business organizations, but a report is  nevertheless an important supplement, serving as a reference for the details in the presentation.

What should be included in a data analysis report?   How should it be written?  

### Report style

Technical writing found in a data analysis report should be clear and spare, relying on visualizations of the central findings. Here are some guidelines.

#### Figures

People read quickly and sloppily.  They will miss the main point if it isn't emphasized.  The best way to emphasize, and convey, the main point is in a plot.  Pick one or two plots to convey the main point.  They should tell the "story." *If a plot doesn't add value to the storyline then it shouldn't be there.*  (Never include a plot just because.) 

Plots are usually better than tables. Why? It is easier to discern important information---especially relationships or comparisons---when it is presented visually.  Sometimes, of course, a table works better than a plot: for example, when exact numbers are required.

Always use captions that provide context and that tell the reader what the plot or table means (what key point does it convey?). The plot and caption should stand alone, in the sense of being interpretable even if the reader does not read the full text (since most won't). It is okay for the caption to repeat the  text of the report.  Always number figures and tables and reference them in the text:  "In Figure 2, we see that ...."

Here is an example of a plot with a lengthy caption.

![](images/Copyofgelman_plot3.jpg)


And here is an example of a table with a caption.

![](images/Copyofgelman_table1.jpg)

The writer---Andrew Gelman, in both cases---is not assuming that the reader will automatically know what is going on in the plots or in the table but is rather explaining it in great detail.


#### Text

Use white space and structure---bolded headings, subheadings, lists with italicized main points---to help convey findings.  Specifically:

- Avoid big blocks of text unless the main point is summarized in an informative heading or subheading. 

- Write so that a reader could read the headings, subheadings, bulleted points and the table and figure captions and  still get the gist of the report. 

- Redundancy is usually not a virtue but it has a place in report writing:  we don't worry about repeating our figure and table captions in the text and again in our summary section.
                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                              Here is an example of an effective use of white space and headings:
                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                ![](images/Copyoftext1.jpg)
                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                              *Our goal as technical writers should be to write in such a way that a reader could skim the report---reading the executive summary, headings, plots and tables (with informative captions)---and still get the main findings.*
                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                ### Report structure
                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                A data analysis report should include the following sections: 
                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                1. *Introduction*. 
                                                                                                                                                                                                                                                              - Briefly describe the business, and, at more length, the business problem. 
                                                                                                                                                                                                                                                              - What was your primary objective in the project. 
                                                                                                                                                                                                                                                              - Summarize the data, your analysis, your recommendations, as well as any relevant substantive context, background, or framing issues.
                                                                                                                                                                                                                                                              - Brief outline of remainder of the report.
                                                                                                                                                                                                                                                              2. *Data*
                                                                                                                                                                                                                                                                3. *Methods and Analysis*
                                                                                                                                                                                                                                                                4. *Results*
                                                                                                                                                                                                                                                                5. *Conclusion(s)/Discussion*. The conclusion should reprise the questions and conclusions of the introduction, perhaps augmented with discussion of any additional observations or details from the analysis. New questions and future work can also be raised here.  Circle back to  the company's business problem: to what extent did your project solve that problem?
6. *Appendix/Appendices*. One or more appendices are the place to describe analytical details and ancillary materials. These might include such items as:
    - Technical descriptions of (unusual) statistical procedures
    - Detailed tables or model output
    - Figures that were not central to the arguments presented in the body of the report 
    - Computer code used to obtain results.

    It is often difficult to find the right balance between what to put in the appendix and what to put in the body of the paper. Generally you should put just enough in the body to make the point, and refer the reader to specific sections or page numbers in the appendix for additional graphs, tables and other details.


### Report audience

The data analysis report should be written for several different audiences at the same time:

- *Primary audience*: A primary collaborator or client. Reads the *Introduction* and perhaps the *Conclusion* to find out what you did and what your conclusions were, and then perhaps fishes/skims the rest, stopping only for some additional details on the parts that he/she thought were interesting or eye-catching. Organize the report around the conversation you want to have with this person about what you've learned. 
                                                                                                                                                                                                                                                              - *Secondary Audience*: An executive person. Probably only skims the *Introduction* and perhaps the *Conclusion* to find out what you did and what your conclusions were. 
                                                                                                                                                                                                                                                              - *Secondary Audience*: A technical supervisor. Reads everything, including the *Appendix* for quality control: Were your conclusions and recommendations supported by your analysis?  Add text to the technical material in the *Appendix* so that this person sees how and why you carried out the more detailed work shown in the *Appendix*.
                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                              ## Example report
                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                              ### Audience Analysis
                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                              
