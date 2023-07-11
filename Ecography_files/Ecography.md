---
title: "Ecography"
author: "Ainhoa Magrach"
date: "2023-06-07"
output:
  bookdown::word_document2:
    fig_caption: yes
    fig_width: 7
    fig_height: 7
    df_print: kable
    keep_md: yes
    number_sections: no
    toc: no
bibliography: references.bib
csl: ecology.csl
link-citations: yes
linkcolor: RoyalBlue
urlcolor: RoyalBlue
links-as-notes: false
editor_options:
  markdown:
    wrap: 72
    self_contained: no
---

# ---
# title: "Ecography"
# author: "Ainhoa Magrach"
# date: "2023-06-07"
# - Alfonso Allen-Perkins ^1, Maddi Artamendi ^1,2^, Daniel Montoya ^2,3^, Ainhoa Magrach ^2,3^*
# 
# output:
#   bookdown::word_document2:
#     
#     fig_caption: yes
#     fig_width: 7
#     fig_height: 7
#     df_print: kable
#     keep_md: yes
#     number_sections: no
#     toc: no
#     #reference_docx: Word_template.docx
# bibliography: references.bib
#   #- knitcitations.bib
# csl: ecology.csl
# link-citations: yes
# linkcolor: RoyalBlue
# urlcolor: RoyalBlue
# links-as-notes: false
# editor_options: 
#   markdown: 
#     wrap: 72
#     self_contained: no
# ---





**Abstract**

**Introduction**

Pollinator efficiency depends, amongst others, on the probability that within a visitation sequence, a pollinator will sequentially visit individuals belonging to the same plant species (REF), as this will reduce the probability of heterospecific pollen reaching the stigmas (REF). Nevertheless, if donor and receiver individuals are within close proximity, this could also increase the potential inbreeding between highly related individuals (relatedness correlated with distance REF).

Recent research has aimed to understand the relationship between community structure and function, using metrics that define the architecture of communities at a given time extracted using network theory (e.g., [@magrach2021; @lázaro2020]. However, most of these advances in network analyses within pollination systems have been carried out at the species level (although see [@tur2013; @arroyo-correa2021; @gómez2020], thus ignoring the sub-species variation that exists between individuals, such as their floral display, the aggregation of floral resources, the potential attractiveness of diverse floral assemblages, or how competition amongst pollinator species can influence individual pollinator behaviors.

**Objective**: to understand what determines pollinator foraging choices within a landscape, how configuration, composition and competitive interactions shape these choices and the impacts these have for plant reproductive success.

Here, we use a recent set of tools derived from multilayer network analyses and diffusion dynamics, multilayer diffusion networks (Allen-Perkins et al. 2023) to understand how spatial and temporal dynamics in plant and pollinator communities shape pollinator foraging choices, their efficiency and the consequences of this for plant reproductive success. In our case, and as opposed to Allen-Perkins (2023) who followed a more plant-centric approach, we follow a pollinator-centric approach, where layers represent individual pollinators and different visitation sequences of plant individuals, while inter-layer links represent overlapping use of plant resources within specific plots by different pollinator individuals, thus accounting for plant and pollinator phenologies, as well as potential intra- and inter-specific competition between pollinators for resources. We further link, intra-layer (i.e., pollinator effectiveness and plant attractiveness values) and inter-layer (i.e., potential intra- and inter-specific competition values) to plant reproductive success measured in the same plots.

We expect that pollinator efficiency (measured as the probability that a given pollinator individual from a particular species will sequentially visit two plant individuals of the same species will change throughout the flowering season, following an increase in floral resource availability through time, and increasing intra- and inter-specific competition as more pollinators emerge. Further, we expect that individual plot traits, such as their floral availability, the diversity of floral resources they offer or their distance to other flowering plots will significantly alter pollinator foraging patterns. Finally, we expect that plant individuals visited by more efficient pollinators (more faithful) and less interlayer links (with a more specialized pollinator group) will produce greater values of reproductive success. 

**Methods**

**Study area and design**

We collected data within 5 stone pine forest fragments located near Donana National Park. Within each forest fragment, we established one 20x20 m square plot, which we subdivided into 1x1m sub-plots. For each sub-plot, biweekly (weather permitting) during the flowering season (Feb-May) for two consecutive years (2020-21, N= 9 sampling rounds per year), we surveyed floral resource availability, recorded interactions between plants and pollinators and obtained data on reproductive success for a subset of plant species (*Cistus salviifolius*, *C. crispus*, *C. ladanifer*, *C. libanotis*, *Halimium halimifolium*, *H. calcynum*, *H. comutatum*, and *Lavandula stoechas*). We considered a successful visit whenever the floral visitor touched the reproductive parts of the flower. All plots were surveyed for 3 hours each week that were sparsed throughout the same day, thus obtaining data at different times throughout the day for each of the sampled weeks. Pollinator individuals were either identified directly in the field or captured and identified in the laboratory by expert taxonomists (see Acknowledgments). With this sampling we obtained 125 hours of sampling over XX weeks. 

*Sampling completeness*

We assessed sampling completeness for plant and pollinator species as well as plant-pollinator interactions in our dataset using the Chao1 estimator of asymptotic species richness for abundance data [@chao1984]. To do this, we used package iNEXT [@iNEXT-2] and estimated how plant and pollinator species richness and plant-pollinator interaction link richness accumulated with increasing sampling efforts. We estimated sampling completeness for each site using data accumulated across all sampling periods.

*Analysis*

We follow a similar logic to that presented by Allen-Perkins et al (2023) who analysed multilayer plant-pollinator interactions from a plant-centric approach. In their study, each layer represented a plant species with intra-layer links representing interactions between different plant individuals (in this case 1m\^2 plots) and their pollinator species, and inter-layer links representing shared pollinator species between different layers (i.e., plant species). In our case, we go one step further by analysing a multilayer network using both an individualized plant and pollinator perspective. In this case individual plants are represented by plants of the same species present within a 1m2 plot and pollinators are represented by specific individuals whose sequence of flower visitation is recorded. In this case, although we have not marked these individual pollinators, we assume that each polllinator flight sequence recorded belongs to that of different individuals. Therefore in our case, intra-layer links represent connections between individual plants (measured at 1 m2 plots) and individual pollinators from different species. In turn, inter-layer links represent instances when the same plot is visited by different pollinator individuals. 

We focus on reproductive success for plant species whose flowers close once pollinated, which allows us to assure that we can link individual plant reproductive success to the specific spatial and temporal conditions present at the time when the plant individual was pollinated. 

\
TO DO

Use of random walks, is the behavior of a pollinator changed with increasing competition (inter-layer links)? Calculate transition between nodes (between different plant species) as a consequence. Are pollinator individuals changing their foraging behavior if there is competition?

Allen-Perkins also considered their networks to be static, i.e., that transition probabilities among nodes (i.e., the weights of the links) do not vary with time and, consequently, do not depend on the previous positions of the walker. However, we are interested here in how pollinator efficiencies change over time and therefore we are interested in previous positions of pollinators and how they change as the conditions of the system change (e.g., flower availability, intra or inter-specific competition). ES POSIBLE?

To measure interaction strength can we somehow weigh interaction visits by the identity of the previous and following plant individuals? That way you give more strength to interactions where there is a transfer of pollen between individuals of the same species and somehow weigh the strength and make it decay as more flowers are visited (some sort of pollen dilution effect). Maybe it can also decay with the distance between plant individuals (plots) which can be calculated as shortest path between 2 plots.

Metrics: at the network level we can calculate modularity but somehow related to space, where link strength is somehow weighted by distance??

Subgraph analyses: categorization according to plant species involved: homospecific or heterospecific (measure of specialization and generalization?) as in Allen-Perkins et al 2023. 

\
\
As opposed to Allen-Perkins et al who used the probability of receiving conspecific pollen as a probability, we included the probability that the previous plant individuals visited in a given visitation sequence belonged to the same plant species, with a probability of bringing conspecific pollen declining linearly.

Recent research has suggested that "..looping movements characteristic of bee exploratory flights combined with perceptual masking effects by which the probability of finding given flowers is affected by the presence of others, would result in strikingly different predictions for flower discovery rates than the typical diffusive random walk movements [@morán2023]. We therefore SOMEHOW INCLUDE PRESENCE OF OTHER FLOWERS

POSSIBLE THINGS TO TEST: Here we have data for both bumblebees and other wild bees, considered as central place foragers as well as for other pollinator species like Syrphid flies that do not rely on specific nest locations. We can therefore test whether random diffusion walks or other models are best suited to describe the foraging behavior of either group of species. 

Also, we can test the probability of bees to find flowers not only based on their size and spatial location, but also on the presence and characteristics of other flowers around them.

\
**Results**

Across our sampling we observed 37 plant species in flower, 28 (76%) of which received visits from \~70 pollinator species or morphospecies. Pollinators were identified to the species (XX%) or morphospecies (XX%) level (see Appendix). 

We recorded XXX interactions 

\
\
\
\

Allen-Perkins et al "For instance, while we show here that for small annual plants, using fine-scale patch-level observations may be a good proxy of individual based pollinator visits, the most critical assumption is that individuals linked by the same pollinator species have larger probabilities of mating (Arroyo-Correa et al., 2021; GÅLomez et al., 2020). While reasonable at the scales investigated, pollinator behavior (Devaux et al., 2014) and morphology are expected to further modulate this relationship. Following individual pollinators to test for floral constancy (e.g. Jakobsson et al., 2008), diet breadth overlap between individuals (Brosi, 2016), or analyzing pollen loads on pollinators bodies (Bosch et al., 2009) deserves future attention to test this assumption.

CAN WE TEST THIS WITH OUR DATA???

\
\
\
\
\
\


```r
summary(cars)
```

```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```

## Including Plots

You can also embed plots, for example:

![](Ecography_files/figure-docx/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
