# crqanlp

This is an early version of a library for conducting dynamic analysis of text under recurrence quantification. We show recurrence quantification (RQA) as a dynamic natural language processing tool, and offer a wrapper around the CRAN `crqa` library for R ([Coco & Dale, 2014](http://co-mind.org/rdmaterials/php.cv/pdfs/article/coco_dale_2014.pdf)). Details about the relationship between RQA and standard NLP, along with examples using `crqanlp`, can be found in the following full paper draft:

-- Dale & Coco (in preparation). [Dynamic natural language processing with recurrence quantification analysis](http://co-mind.org/rdmaterials/php.cv/pdfs/article/dale_coco_arxiv.pdf).

## Installation

`crqanlp` is now on CRAN, and can be installed this way:

```R
install.packages('crqanlp')
```

If you wish to directly install from this repository, copy this R folder to a root path, and run the following code:

```R
lapply(list.files('R'),function(x) { source(paste('R/',x,sep='')) })
```

