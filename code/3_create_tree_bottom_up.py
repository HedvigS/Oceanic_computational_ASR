#This is a script which takes a list of glottocodes (be they of the glottolog level dialects, languages or family level) and produces a subtree of a glottolog family tree where they are the tips.

##Local version of glottolog data necessary
#It is necessary that you have a local version of glottolog data. This can either be a clone of the glottolog/glottolog repository or your fork of it, an unzipped released version of Glottolog from GitHub, or an unzipped download of a released version of Glottolog from ZENODO. This is the object that needs to be specified as "glottolog_data" in this script. This sample scripts contains a file path on my machine, you will need to edit this.
# In this particular example, I'm using the zenodo release of glottolog 4.3. You can fetch it yourself at https://zenodo.org/record/4061162/files/glottolog/glottolog-v4.3.zip. (It needs to be unzipped.)

#You will  need python3 installed as well as the specific packages pyglottolog, newick and pandas.

#list of tips needs to have the relevant glottocodes as its first column

#this code is based on Forkel's example here https://github.com/glottolog/pyglottolog/issues/35#issuecomment-631415798

from pyglottolog import Glottolog 
import newick
import pandas
import json

def get_data():
  with open("config.json", "r") as f:
    config_dic = json.loads(f.read())
    
  glottolog_location = config_dic["data_sources"]["glottolog_tree"]["location"]
  
  glottolog_data = Glottolog(glottolog_location)

  return glottolog_data,config_dic["analysis_specs"]["top_node"]

def run():
  glottolog_data, top_node_name = get_data()
  
  top_node = glottolog_data.languoid(top_node_name) 

  lg_list_fn = 'data/GB/GB_wide_binarised.tsv' #This is the list of desired tips

  lg_list = pandas.read_csv(lg_list_fn, sep='\t')
  lg_list = list(lg_list.iloc[:,0]) #Specifying that we are taking column 1 of the table

  tree = top_node.newick_node(template='{l.id}') #using the newick package to extract the entire tree
  tree.prune_by_names(lg_list, inverse=True) #pruning tree
  tree.remove_redundant_nodes() #removing nodes which aren't necessary for this set of tips

  #print(tree.ascii_art()) #optional, makes ASII-art illustration of tree 
  newick.write(tree, "data/trees/glottolog_4.3_tree_newick.txt") #printing to file

if __name__ =="__main__":
  run()
