import ast
import csv

'''
Solution inspired by user: "Anas" in Kaggle discussion tab for world-factbook-country-profiles.
3 Lines of code taken (14-16) directly from the given solution, the remaining code was written independently.
https://www.kaggle.com/usdod/world-factbook-country-profiles/discussion/71462 
'''

filename="factbook-country-profiles.txt"


##Below code taken directly from https://www.kaggle.com/usdod/world-factbook-country-profiles/discussion/71462
with open(filename, "r", encoding="utf-8") as infile:
	content = infile.read()
	data = ast.literal_eval(content)
##End of taken code
	
with open('factbook-country-profiles_original.csv', mode='w') as csv_file:
	csv_writer = csv.writer(csv_file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
	csv_writer.writerow(['Name', 'Geographic coordinates', 'Map references',
	'Total Area', 'Land area', 'Water area', 'border countries', 'coastline',
	'climate', 'mean elevation', 'natural resources', 'population', 
	'religions', 'median age', 'population growth rate', 'birth rate',
	'death rate', 'net migration rate', 'sex ratio', 
	'mean age of mother at first child', 'life expectancy', 'fertility rate',
	'health expenditure', 'hospital bed density', 'obesity rate', 
	'government type', 'GDP PPP', 'GDP growth rate', 'GDP per capita',
	'unemployment rate', 'population below poverty line'])
	
	for country in data:
		var1 = "N/A"
		try:
			var1 = country['Government']['Country name']['conventional short form']['text']
		except KeyError:
			pass
			
		var2 = "N/A"
		try:
			var2 = country['Geography']['Geographic coordinates']['text']
		except KeyError:
			pass
			
		var3 = "N/A"
		try:
			var3 = country['Geography']['Map references']['text']
		except KeyError:
			pass
			
		var4 = "N/A"
		try:
			var4 = country['Geography']['Area']['total']['text']
		except KeyError:
			pass
			
		var5 = "N/A"
		try:
			var5 = country['Geography']['Area']['land']['text']
		except KeyError:
			pass
			
		var6 = "N/A"
		try:
			var6 = country['Geography']['Area']['water']['text']
		except KeyError:
			pass
			
		var7 = "N/A"
		try:
			var7 = country['Geography']['Land boundaries']['border countries']['text']
		except KeyError:
			pass
			
		var8 = "N/A"
		try:
			var8 = country['Geography']['Coastline']['text']
		except KeyError:
			pass
			
		var9 = "N/A"
		try:
			var9 = country['Geography']['Climate']['text']
		except KeyError:
			pass
			
		var10 = "N/A"
		try:
			var10 = country['Geography']['Elevation']['mean elevation']['text']
		except KeyError:
			pass
			
		var11 = "N/A"
		try:
			var11 = country['Geography']['Natural resources']['text']
		except KeyError:
			pass
			
		var12 = "N/A"
		try:
			var12 = country['People and Society']['Population']['text']
		except KeyError:
			pass
			
		var13 = "N/A"
		try:
			var13 = country['People and Society']['Religions']['text']
		except KeyError:
			pass
			
		var14 = "N/A"
		try:
			var14 = country['People and Society']['Median age']['total']['text']
		except KeyError:
			pass
			
		var15 = "N/A"
		try:
			var15 = country['People and Society']['Population growth rate']['text']
		except KeyError:
			pass
			
		var16 = "N/A"
		try:
			var16 = country['People and Society']['Birth rate']['text']
		except KeyError:
			pass
			
		var17 = "N/A"
		try:
			var17 = country['People and Society']['Death rate']['text']
		except KeyError:
			pass
			
		var18 = "N/A"
		try:
			var18 = country['People and Society']['Net migration rate']['text']
		except KeyError:
			pass
			
		var19 = "N/A"
		try:
			var19 = country['People and Society']['Sex ratio']['total population']['text']
		except KeyError:
			pass
			
		var20 = "N/A"
		try:
			var20 = country['People and Society']['Mothers mean age at first birth']['text']
		except KeyError:
			pass
			
		var21 = "N/A"
		try:
			var21 = country['People and Society']['Life expectancy at birth']['total population']['text']
		except KeyError:
			pass
			
		var22 = "N/A"
		try:
			var22 = country['People and Society']['Total fertility rate']['text']
		except KeyError:
			pass
			
		var23 = "N/A"
		try:
			var23 = country['People and Society']['Health expenditures']['text']
		except KeyError:
			pass
			
		var24 = "N/A"
		try:
			var24 = country['People and Society']['Hospital bed density']['text']
		except KeyError:
			pass
			
		var25 = "N/A"
		try:
			var25 = country['People and Society']['Obesity - adult prevalence rate']['text']
		except KeyError:
			pass
			
		var26 = "N/A"
		try:
			var26 = country['Government']['Government type']['text']
		except KeyError:
			pass
			
		var27 = "N/A"
		try:
			var27 = country['Economy']['GDP (purchasing power parity)']['text']
		except KeyError:
			pass
			
		var28 = "N/A"
		try:
			var28 = country['Economy']['GDP - real growth rate']['text']
		except KeyError:
			pass
			
		var29 = "N/A"
		try:
			var29 = country['Economy']['GDP - per capita (PPP)']['text']
		except KeyError:
			pass
			
		var30 = "N/A"
		try:
			var30 = country['Economy']['Unemployment rate']['text']
		except KeyError:
			pass
			
		var31 = "N/A"
		try:
			var31 = country['Economy']['Population below poverty line']['text']
		except KeyError:
			pass
		
		csv_writer.writerow([var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12,var13,var14,var15,var16,var17,var18,var19,var20,var21,var22,var23,var24,var25,var26,var27,var28,var29,var30, var31])