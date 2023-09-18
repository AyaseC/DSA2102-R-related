## Dengue

```{r qn1}
qn1_char <- den_df
qn1_char <- str_replace_all(den_df$description, c("Pl[^a-z]"="Place","Ter[^a-z]"="Terrace" ,"Gr[^a-z]"="Grove", "Lk[^a-z]"="Link", "Cl[^a-z]"="Close"))
qn1_char

```




```{r qn2}

qn2_list <- str_extract_all(den_df$description, "(?<=\\(Blk).*?(?=\\))")
qn2_list <- lapply(qn2_list, function(x){as.character(unlist(str_split(x,",")))})
qn2_list

```

```{r qn3}

qn3_list <- str_extract_all(den_df$description, "(?<=\\()[^(Blk)][^(\\)]*+")
qn3_list <- lapply(qn3_list, function(x){as.character(unlist(str_split(x,",")))})
qn3_list
```

```{r qn4}
qn4_list <- qn3_list[den_df$num.cases>=10]
qn4_list
```

```{r qn5}
condition <- lapply(qn3_list, function(x) length(x)>=2)
qn5_list <- qn3_list[unlist(condition)]
qn5_list
```

```{r qn6}
hawker_dist2 <- hawker_dist/1000
hawker_dist2
```


```{r qn7}
two_step <- function(h_name,h_dists){
  if (h_name %in% colnames(h_dists)){
    h_dists <- h_dists[order(h_dists[,h_name],decreasing = FALSE),]
    step1 <- rownames(h_dists)[2]
    h_dists <- h_dists[order(h_dists[,step1],decreasing = FALSE),]
    step2 <- rownames(h_dists)[2]
    if (step2==h_name){
      step2 <- rownames(h_dists)[3]
    }
    return(step2)
  }
  else{
    return()
  }
}
```
