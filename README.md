# OMSBA-5300-DTC

## Questions to be answered with data from https://cps.ipums.org/cps-action/variables/group 
  1. How has COVID affected the health of the retail industry, as measured by employment?
  2. How has retail fared relative to other industries?
  3. Retail needs to worry about who has money to spend - what has changed about who is working and earning money?

## Assignment Requirements

#### *The Write-Up*
###### Should be in a Rmarkup Document and answer the below questions
  1. Why you are running the analyses you are running?
  2. How the analyses answer the question being asked, and what the result is?
  3. Carefully interpreting the results
  4. Presenting the results in an appealing way. Graphs are great, sumtable() is great, export_summs() is great - put a little effort into formatting            tables and figures to make them look nice! At the very least, variable names should be in English rather than statistics-package (‘Education’ not          ‘EDUC’). If you aren’t comfortable enough with ggplot to make its visualizations look nice, feel free to make graphics in Excel or Tableau or anything      you like, and include them in your RMarkdown doc as images. Econometric analyses should be in R.
  5. Acknowledging the assumptions you are making in each analysis, how plausible those assumptions are in the context of your data, and any evidence you        can provide backing up those assumptions
  6. After doing all analyses related to a given question, provide a generalized answer to the main questions.

#### *The Presentation*
###### Presentation should be between 15 and 20 minutes long, plus a few minutes for questions. As many or as few of the people in your group can present as you like.
##### Key Points of the Presentation
  1. Choose two of the main questions you feel you can answer best
  2. Present your analyses related to those questions. Depending on how many you have you may want to select only a subset of them.
  3. Make clear to the audience how your analysis works, how to interpret the results, and what the general answer to the question is
  4. Be prepared to answer questions about your analyses and the assumptions behind them

## *Notes*
  1. You can find the CPS data on Canvas, as well as the file linking the industry codes to their titles. You will probably want to work with the named       industry, since they are more broadly defined and closer to the level you want.
  2. Many of the variables in the data have labeled values, and these will be preserved if you read in the data with read_dta() in haven. For example, a      value of 40 in the educ variable means you left school after 9th grade. To see how the values relate to the labels, send a variable to labeltable()      in vtable (or read the documentation).
  3. Our class has focused a lot on identification error and causality, but not all questions are causal in nature! It’s fine if you’ve picked an              analysis that helps answer a main question without needing to isolate causality - often the methods are the same, but the assumptions you need to        make and the interpretation of the results are different.
  4. You will need to read the data documentation to do this project. See the .XML documentation file, or head to IPUMS CPS to read their documentation        there.
  5. Everything February 2020 and before is “before COVID” and everything April and after is “after COVID.” But what about March? This is something            you’ll need to consider how to deal with in your analyses.
  6. If you want an analysis to be at the industry-month level, you should make your data be at that level too! Use mutate(yearmo = year*100 + month) to      create a year-month variable, and then use group_by(yearmo, indname) %>% summarize() to collapse data to the yearmo/indname level
  7. We didn’t really talk about sample weights in class. Sample weights are a common tool in survey research (and CPS is a survey!) where you receive a      weight based on how heavily they want to count you in the sample. For example, if 1% of their survey sample is Hispanic men aged 30-35 living in          California, and they happen to know that in the full population, that group makes up 2.5% of the population, then each of those men might get a          weight of 2.5 to scale them up so the population is represented. When weights are available (as they are here in wtfinl), it’s a good idea to use        them. Look in your regression function’s documentation - there is usually a weights option. Or if you’re doing group_by() %>% summarize(), look into      weighted.mean() rather than mean().
  
  ## *Grading*
    5% Following all requirements of the assignment
    30% Selecting and accurately performing appropriate analyses to answer the questions
    30% Correctly interpreting your analyses, linking them to the main questions, and discussing flaws and assumptions being made, and the plausibility of those assumptions
    20% Clear and compelling writing
    15% Clear and engaging presentation
    
  ## *Prof Notes*
  
  How Do You Do That?
How can you take a broad generic question and figure out an analysis that will help to answer it?

Let’s take an example broad question not on the list: how has the “Christmas bump” changed in the retail sector?

First, we can ask: What is this question really trying to get at? Well, we know that every year, retail sees a huge boost because of Christmas. So your firm wants to know if the size of that boost is getting bigger than it used to be, smaller, or staying the same over time.

Then, we can ask: What kind of result would answer this question? Well, first we’d need to measure the Christmas bump - the relative amount of employment in retail in December relative to the months of January-October (dropping November since it could go either way) in a given year could be called a Christmas bump. So we want to know if “the amount of employment in retail in December relative to other months” is getting bigger or smaller in recent years.

Next, we ask: How can we get that result from a regression model? This analysis is asking us to compare December to other months, and then compare that comparison across years. Seeing how a comparison changes over time is the realm of interaction terms. So we want to get some sort of “December effect” and interact that with year, to see if the December effect is getting bigger or smaller.

Next, a regression equation! There are a few ways we could put this together (do we collapse January-October into one time period or treat them separately? Do we include fixed effects for all the months? Do we include controls for how the demographic makeup of the country is changing? Do we include change-over-years as a linear or quadratic term, or just compare this year vs. previous years? And so on…) but one way is this:

RetailEmploymenty,m=β0+β1Decemberm+β2Yeary+β3DecembermYeary+εy,m

where m is month and y is year. Now that we have an equation, we should think carefully about if it’s the right one! Should we include November in the data or not? Or include it and give it its own control? Do we need heteroskedasticity-robust standard errors? Clustered standard errors? Should we log that dependent variable? Lots of things to think about.

Once we have our regression model down, we can think about how to run it! This regression seems to want one observation per month, so we’d need to create a year-month variable, use group_by(year, month, yearmo) %>% summarize(RetailEmployment = sum(indname == 'Retail Trade')) to calculate retail employment in each month (and preserve those year and month variables so we can include them in our regression model). Look at your data afterwards to make sure you did it right!
