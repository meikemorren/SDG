# step 1:

## design the results from gpt4o and deepseek to the format of llama
## find results_llama.csv in datasets (also where i will store the gpto4o/deepseek results)
### id: ignore this is wrong in llama file (not in the others)
### text: should be identifiable in the other original files
### sdg: is the sdg defined by OSDG
### expected_label: is whether it is this sdg defined by us [FALSE, TRUE]
### predicted_sdgs: list of sdgs from output model
### predicted_label: whether the sdg as defined by OSDG is also in list
### correct: if expected and predicted label are the same


# step 2:

## assemble all files together, and create column for each LLM correct
## compare the models
## find texts in which they disagree
##  add column when LLMs disagree
## save this file -> this is for gib

