# AHRC_awards
Semi-supervised LDA topic modelling and analysis of AHRC research grant application abstracts
## About this Data Project

The AHRC Grants Topic Browser is built upon the findings of topic analysis conducted on research grant applications that have been awarded funding by the Arts and Humanities Research Council (AHRC) between 2013 and 2023.
        
## Topics

The 32 topics identified here have been generated using a combination of unsupervised and semi-supervised machine learning techniques (LDA and seeded LDA) in a heuristic manner. The goal was to arrive at a classification of the documents in the corpus that is both statistically robust and intuitively meaningful to a human observer. I have labeled the emerging topics based on my interpretation of the documents identified by the model as most strongly associated with the given category (Most Relevant Projects), and the cluster of terms idenified as having the highest probability of appearing in the associated documents (Most Frequent Words). The topic labels are of necessity imperfect. When selecting them, my aim was to find broad concepts that best capture the semantic overlap within each category.
        
## Data Source

The data analysed here is sourced from publicly available information provided by the UK Research and Innovation (UKRI) at <a href=https://gtr.ukri.org/>Gateway to Research (GtR)</a>. The analysis focused on research grant applications, excluding studentships, fellowships, and training grants awarded by the AHRC. 2270 applications have been analysed.
        
## Credits

Author: <a href=https://github.com/kuslitsanna>Anna Kuslits</a>
      

Acknowledgments: The analysis was performed using the <a href=http://quanteda.io/>quanteda</a> and <a href=https://koheiw.github.io/seededlda/>seededLDA</a> R packages, developed by Kenneth Benoit and Kohei Watanabe at the LSE Data Science Institute. In visualising the results and designing the dashboard, I drew inspiration from <a href=https://dsl.richmond.edu/dispatch/introduction>Mining the Dispatch</a>, created by Robert K. Nelson and the Digital Scholarship Lab at the University of Richmond.
