# PhD: "Uprising, the Phonology and Phonetics of Intonation in Derry City English"
## Database and Code

### TO DO

* ~~Update OR LMEs to extract ANOVA tables and statistics for 2-level factor statistics.~~
* ~~Update post-hoc p.value script so it can:~~
  ~~1. Extract a list of p.values from tables even if they do not follow the same table structure~~
  ~~2. Insert p.adj back into the appropriate table AFTER p.value~~
  ~~3. Overwrite previous p.adj columns using the same p.adj method~~
  ~~4. Remove any previous signif. columns and insert a new one as a last column.~~

    **Alternative solution**
    
    getModelFixedFX now automatically distinguishes two-level factors from multilevel factors and adds the slope statistics of the two-level factros them to the pairwise analysis.
    
    Also, p values of ANOVAs of models need to be assessed separately as they already incorporate statistics and p.values from the terns in the model.


#### 2022.07.08

* ~~Correct errors in composite models in mode and acc_phon folder~~
* ~~Correct errors in all models in mode only folder~~
* ~~Update Excel workbooks to include 2-level fixed effect factors.~~

#### 2022.07.10

* Have done:
    * Added ability to print tables with adjusted p.values to post_hoc_p_adj.
    * Save formula to anova folder and to output folder
* to do
    * Get post_hoc_p_correction to use formula as caption in print output. 

