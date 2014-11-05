Meta-Analysis of Recent SemSat Literature
=========================================

Datasets and documentation for ongoing research of semantic satiation. This research is a collaboration between Christopher Wetherill and Angela C. Jones, John Carroll University.

Please note: These analyses are in a rough place right now. Take everything with a large helping of salt.

What's semantic satiation?
--------------------------

Semantic satiation refers to the increase in response time in relatedness judgments following the repeated and frequentitive access of a word and its meaning. E.g., if you see the word GIRAFFE repeated 30 times, this theory predicts that you'll probably take longer to figure out whether GIRAFFE and EVEN-TOED UNGULATE are related than if you've only seen GIRAFFE 3 times. Although how familiar you are with even-toed ungulates is a totally different matter... (Or, a less-silly explanation: with a few number of repetitions, priming is facilitatory; with increased repetitions, fatigue onsets and the prime becomes inhibitory.)

The only little issue is that we don't know if the effect really exists: some experiments have found it; others haven't. See Esposito & Pelton's review of the literature up through the early 70s for a great laugh at all the fun methodologies used historically if you have the time and interest. Even still, however, methodologies vary considerably, effect sizes are small, how satiation is actually defined is up in the air (is it the difference in response times between short and long repetitions; is it the rate of change as a function of repetition; is it the change in response time at a given repetition relative to the response times for an unrelated pairmate; etc.).

So that's fun, right?

These issues considered, this project looks to perform a meta-analysis of the semantic satiation literature published in the last 40 years. All prior attempts to assess whether the effect actually exists or not have been narrative reviews of the literature (Esposito & Pelton, 1971; Black, 2003). Although good overviews of an entire field of literature, this type of review does not provide rigorous and empirical methodologies for assessing the evidence for and against a particular effect with, perhaps, a small effect size and small to moderate sample sizes.

What's included here?
---------------------

All data, analyses, data clean-up procedures, etc. will be included for all experiments. Because reproducability is a really dandy thing that researchers don't typically pay enough attention to. So please, download the data if you're researching semantic satiation. Play with them; analyze them; critique my own methods; confirm or refute my results.

The meta-analysis will include data (tentatively) from:

|AUTHOR | YEAR | ARTICLE|
|-----------|----------|------------|
|Lee Cohene | 1978 | Semantic satiation revisited with a lexical decision task|
|Lee Smith | 1984 | Semantic satiation affects category membership decision time but not lexical priming |
|Lee Smith | 1990 | Evidence for semantic satiation: Repeating a category slows subsequent semantic processing|
|David Balota | 1997 | Semantic satiation in healthy young and older adults|
|Sheila Black | 2001 | Semantic satiation and lexical ambiguity resolution|
|Alan Brown | 2005 | Cumulating retrieval inhibition in semantic and lexical domains|
|Xing Tian | 2010 | Testing an associative account of semantic satiation|
|Brice Kuhl | 2011 | More is not always better: Paradoxical effects of repetition on semantic accessibility|
|Sheila Black | 2013 | Effects of homograph meaning frequency on semantic satiation|
|Christopher Wetherill | 2014 | Semantic satiation among lexically ambiguous words|

**Note:** The statistics reported in the Cohene article and the Smith articles get funky. Effect sizes were more likely than not calculated from t-tests. I don't like it all that much either, but those are the only extant data to work off of at this point.

Interpreting `effectSizes.csv`
-----------------------------

`effectSizes.csv` is structured:

1. `Article` is the last name of the first author (occasionally corresponding author) of the article.
2. `Year` is the year in which the article was published.
2. `Experiment` is the experiment number from which the following data were taken. This is included to reduce ambiguity as many single publications report multiple experiments measuring the same variables.
3. `Source` is the variable being reported. `Subjects` refers to the residual or the error term. Two or more variables separated by a full stop ('.') indicate an interaction.
4. `Coded.Source` is a shortcode for the `Source`.
    
    a. `a` corresponds to a measured between-subjects variable
    
    b. `A` corresponds to a manipulated between-subjects variable
    
    c. `P`, `Q`, and `R` correspond to manipulated within-subjects variables
    
    d. `s` corresponds to the subject/residual error term for a within-subjects design

    e. `s/a` corresponds to the subject/residual error term for a mixed-method (Between-Within) design

5. `Type` explicitly states what type of factor the `Source` is: Between, Within, Mixed (BW), or Residual.
6. `DF` is the degrees of freedom of the `Source`.
7. `SS` is the Type III Sum of Squares for the `Source`.
8. `MSE` is the Mean Square Error for the `Source`.
9. `F` is the F-statistic for the `Source`.
10. `Partial.Eta` is the ![Partial Eta](http://tex.sh/tex/$/eta%5E2_P$) for the `Source`, if it can be computed. This computation is done as outlined in Bakeman (2005) "Recommended effect size statistics for repeated measures designs." When an effect size must be computed from a *t*-statistic, it is done by ![paired-t](http://tex.sh/tex/$/frac%7Bt%5E2%7D%7Bt%5E2+N-1%7D$).
11. `n.e.` is the number of participants in the experimental condition.
12. `m.e.` is the mean value in the experimental condition.
13. `s.e.` is the standard error about the mean in the experimental condition.
14. `n.c.` is the number of participants in the control condition.
12. `m.c.` is the mean value in the control condition.
13. `s.c.` is the standard error about the mean in the control condition.

Waiver of Copyright
======

Statement of Purpose

The laws of most jurisdictions throughout the world automatically confer exclusive Copyright and Related Rights (defined below) upon the creator and subsequent owner(s) (each and all, an "owner") of an original work of authorship and/or a database (each, a "Work").

Certain owners wish to permanently relinquish those rights to a Work for the purpose of contributing to a commons of creative, cultural and scientific works ("Commons") that the public can reliably and without fear of later claims of infringement build upon, modify, incorporate in other works, reuse and redistribute as freely as possible in any form whatsoever and for any purposes, including without limitation commercial purposes. These owners may contribute to the Commons to promote the ideal of a free culture and the further production of creative, cultural and scientific works, or to gain reputation or greater distribution for their Work in part through the use and efforts of others.

For these and/or other purposes and motivations, and without any expectation of additional consideration or compensation, the person associating CC0 with a Work (the "Affirmer"), to the extent that he or she is an owner of Copyright and Related Rights in the Work, voluntarily elects to apply CC0 to the Work and publicly distribute the Work under its terms, with knowledge of his or her Copyright and Related Rights in the Work and the meaning and intended legal effect of CC0 on those rights.

1. **Definitions.**

    a. **"Work"** means any and all compilations of data to the extent that they are protected as copyrightable work.

1. **Copyright and Related Rights.** A Work made available under CC0 may be protected by copyright and related or neighboring rights ("Copyright and Related Rights"). Copyright and Related Rights include, but are not limited to, the following:

    a. the right to reproduce, adapt, distribute, perform, display, communicate, and translate a Work;
    
    b. moral rights retained by the original author(s) and/or performer(s);
    
    c. publicity and privacy rights pertaining to a person's image or likeness depicted in a Work;
    
    d. rights protecting against unfair competition in regards to a Work, subject to the limitations in paragraph 4(a), below;
    
    e. rights protecting the extraction, dissemination, use and reuse of data in a Work;
    
    f. database rights (such as those arising under Directive 96/9/EC of the European Parliament and of the Council of 11 March 1996 on the legal protection of databases, and under any national implementation thereof, including any amended or successor version of such directive); and
    
    g. other similar, equivalent or corresponding rights throughout the world based on applicable law or treaty, and any national implementations thereof.
    
2. **Waiver.** To the greatest extent permitted by, but not in contravention of, applicable law, Affirmer hereby overtly, fully, permanently, irrevocably and unconditionally waives, abandons, and surrenders all of Affirmer's Copyright and Related Rights and associated claims and causes of action, whether now known or unknown (including existing as well as future claims and causes of action), in the Work (i) in all territories worldwide, (ii) for the maximum duration provided by applicable law or treaty (including future time extensions), (iii) in any current or future medium and for any number of copies, and (iv) for any purpose whatsoever, including without limitation commercial, advertising or promotional purposes (the "Waiver"). Affirmer makes the Waiver for the benefit of each member of the public at large and to the detriment of Affirmer's heirs and successors, fully intending that such Waiver shall not be subject to revocation, rescission, cancellation, termination, or any other legal or equitable action to disrupt the quiet enjoyment of the Work by the public as contemplated by Affirmer's express Statement of Purpose.

3. **Public License Fallback.** Should any part of the Waiver for any reason be judged legally invalid or ineffective under applicable law, then the Waiver shall be preserved to the maximum extent permitted taking into account Affirmer's express Statement of Purpose. In addition, to the extent the Waiver is so judged Affirmer hereby grants to each affected person a royalty-free, non transferable, non sublicensable, non exclusive, irrevocable and unconditional license to exercise Affirmer's Copyright and Related Rights in the Work (i) in all territories worldwide, (ii) for the maximum duration provided by applicable law or treaty (including future time extensions), (iii) in any current or future medium and for any number of copies, and (iv) for any purpose whatsoever, including without limitation commercial, advertising or promotional purposes (the "License"). The License shall be deemed effective as of the date CC0 was applied by Affirmer to the Work. Should any part of the License for any reason be judged legally invalid or ineffective under applicable law, such partial invalidity or ineffectiveness shall not invalidate the remainder of the License, and in such case Affirmer hereby affirms that he or she will not (i) exercise any of his or her remaining Copyright and Related Rights in the Work or (ii) assert any associated claims and causes of action with respect to the Work, in either case contrary to Affirmer's express Statement of Purpose.

4. **Limitations and Disclaimers.**

    a. No trademark or patent rights held by Affirmer are waived, abandoned, surrendered, licensed or otherwise affected by this document.

    b. Affirmer offers the Work as-is and makes no representations or warranties of any kind concerning the Work, express, implied, statutory or otherwise, including without limitation warranties of title, merchantability, fitness for a particular purpose, non infringement, or the absence of latent or other defects, accuracy, or the present or absence of errors, whether or not discoverable, all to the greatest extent permissible under applicable law.

    c. Affirmer disclaims responsibility for clearing rights of other persons that may apply to the Work or any use thereof, including without limitation any person's Copyright and Related Rights in the Work. Further, Affirmer disclaims responsibility for obtaining any necessary consents, permissions or other rights required for any use of the Work.

    d. Affirmer understands and acknowledges that Creative Commons is not a party to this document and has no duty or obligation with respect to this CC0 or use of the Work.
