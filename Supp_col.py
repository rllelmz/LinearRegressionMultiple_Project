import pandas as pd 

file = pd.read_csv("~/Desktop/World_Happiness_2019(2).csv")
columns = ['Score', 'GDP per capita', 'Social support', 'Healthy life expectancy', 'Freedom to make life choices', 'Generosity', 'Perceptions of corruption']
new_file = file[columns]
new_file.to_csv("~/Desktop/World_Happiness_2019.csv", index = False)
print("Nouveau fichier créé")