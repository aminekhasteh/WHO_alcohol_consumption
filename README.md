# Drinking patterns info for obtaining prevalence estimates

*ensure all data is for ages >15

1a) Current Drinkers (CD) in the last 365 days
•	Those who have reported drinking at least 1x in the last year.
•	Input '365' for variable cdtl.
o	cdtl (current drinker time limit) = time frame to assess the current drinking pattern, which in this case is 365 days

CTADS: see ALC_Q10 (Link to CTADS 2017 Questionnaire) in the questionnaire/codebook
STEPS: see code A2 in the questionnaire
RLMS: see M174 in the 2019 questionnaire/codebook *(check if any variable changes for 2016-2018 for this drinking pattern and others.)
GENACIS/GENAHTO: see bfdb in the codebook

1b) CD in the last 30 days
•	If a study reports those who have consumed alcohol in the last 30 days, include this is as well.
•	Input '30' for variable cdtl so we know the time frame is the last 30 days. 

CTADS: not required, but see if you can calculate for ALC_Q70 (if I can get work add it in then do it!)
STEPS: see A5
RLMS: see M80.1

2) Lifetime Abstainer (LA)
•	Those who have never consumed any alcohol in their lives (not just abstaining in the last year).

CTADS: see ALC_Q20 (taking 2)
STEPS: see A1 
RLMS: see M80

3a) Former Drinker (FD) in the last 365 days
•	Those who have not consumed alcohol in the last 1 year but have consumed alcohol before then.
•	Input '365' for variable fdtl.
•	If a survey does not have a question that directly assesses FD, can be calculated by including those who have (1) reported ever drinking alcohol in their lives and subtracting (2) those who have reported not drinking alcohol in the last 1 year.

CTADS see ALC_Q20 (ever had a drink) and ALC_Q10 (drank in the last year) 
STEPS: see A1 and A2 
RLMS: see M80 and M174

3b) FD in the last 30 days
•	Those who have not consumed alcohol in the last 30 days but have consumed previously.
•	Include 3b, if available, but ensure we have 3a, which is a more accurate measure of former drinkers.
•	Input '30' for variable fdtl.
•	Same as above, it can be calculated from assessing the two survey questions except (2) will be those who reported not drinking alcohol in the last month.

CTADS: if can get CD in the last 30 days, use ALC_Q20 and ALC_Q70
STEPS: see A1 and A5
RLMS: see M80 and M80.1

4a) Heavy Episodic Drinker (HED) at least 1x in the last 30 days*
•	Those who have reported binge drinking at least 60 grams or more of pure alcohol on at least one occasion in the past 30 days. A consumption of 60 grams of pure alcohol corresponds approximately to 6 standard alcoholic drinks.
•	Variable hedalc = minimum amount of pure alcohol consumed per binge drinking occasion. Input '60' to indicate 60 grams *(see note below for hedalc variations)**.
o	input ‘30’ for variable hedtl.
•	Variable hedtlm = minimum amount of binge drinking occasions per time frame. Input '1' to indicate 1 drinking occasion.

4b) HED assessed at least 1x in the last 365 days*
•	Input '365' for hedtl. 
•	Input ‘1’ for hedtlm.

4c) HED at least monthly in the last 365 days*
•	input '365' for hedtl.  
•	input '12' for hedtlm to indicate at least 12 binge drinking occasions in the last year (i.e. at least monthly)
•	To calculate from survey questions, use the question that asks if participants have had a binge-drinking occasion in the last year, and then add up the responses (monthly, weekly, daily, etc.) and exclude less than monthly responses.

*if a survey has one or more HED variations (i.e. 4a, 4b, and 4c), input them all. Ensure you are calculating HED among the total sample. If they also have HED among current drinkers in the last 365 days calculate that as well. 

**When hedalc varies:
Not every survey or country follows the WHO definition of HED. Some use different amounts of alcohol (measured by grams or standard drinks) per drinking occasion which we allow. Consider these variations for variable hedalc:

 CTADS: use 68 grams for males and for females obtain both 54.4 grams and 68 grams for hedalc (Ask question later)
o	For males, see ALC_Q60 in the questionnaire:  ‘During the past 12 months, how often have you had five or more drinks on one occasion?’
o	For females, see ALC_Q50: ‘During the past 12 months, how often have you had four or more drinks on one occasion?” and ALC_Q60
o	In Canada, a standard drink is 13.6 grams. You will also see that 4b and/or 4c would be applicable and not 4a as the time limit is in the last year. 

STEPS: use 60 grams for hedalc
o	See code A8 in the questionnaire: ‘During the past 30 days, what was the largest number of standard drinks you had on a single occasion, counting all types of alcoholic drinks together?’
o	Or see code A9: ‘During the past 30 days, how many times did you have six or more standard drinks in a single drinking occasion’
o	They go by 6 standard drinks, and as it is a WHO initiative, you can assume 10g per drink unless stated otherwise. 

RLMS: use 40 grams for females and 60 grams for males for hedalc
o	See M176 in the 2019 questionnaire:
 ‘How often in the last 12 months did you drink in one day including time after midnight.
[FOR WOMEN] four or more helpings of alcoholic beverages or any combinations of them - for instance four glasses of wine (about 0.5 litres), or four cans of beer (2 litres), or one shot glass of vodka and three bottles of beer, or two shot glasses of vodka and two glasses of wine and so on.
[FOR MEN] six or more helpings of alcoholic beverages or any combinations of them - for instance six shot glasses of vodka (half of 0.5l bottle) or six bottles beer(3 litres) or two shot glasses of vodka and four bottles of beer, or three shot glasses of vodka and three glasses of wine and so on.
o	Russia does not have a national definition of a standard drink so we will assume 10 grams per drink. Note that 4b and 4c, and not 4a will be relevant for RLMS.
o	Do not assume the 2019 questionnaire is like 2016-2018. Check earlier versions to see if the question varies (2015 and before there was no HED question).

GENACIS/GENAHTO:  use 60 grams for HEDALC unless stated otherwise for a specific country in the codebook. 
o	Use Bf60 as listed in the codebook (except for New Zealand use bf60_146):
‘During the last 12 months, how often did you have at least 60 gm of any kind of alcoholic beverage in a single day, that is, any combination of cans, bottles or glasses of beer, glasses of wine, or homemade alcoholic beverages of any kind? Was it?’
o	Use 4b and 4c, not 4a.
o	Also see the notes for bmonthly5plus if applicable for one of the countries

5) Daily Drinking Level (DDL)
•	The average daily intake of alcohol (in grams) among drinkers (not total sample)
•	Some surveys may report in standard drinks instead of grams so you will need to convert. Likewise, the mean value may need to be calculated. 
•	Connect with Jakob if you need more clarification on how he calculated previously for each survey. 

CTADS: see ALC_Q70 in the questionnaire: ‘During the past 30 days, on those days when you drank, how many drinks did you usually have?’
o	Convert from standard drinks to grams (1 Canadian standard drink = 13.6g)

STEPS: see code A10 in the questionnaire: ‘During each of the past 7 days, how many standard drinks did you have each day?
o	Convert from standard drinks to grams (Assume 1 standard drink = 10g)

RLMS: see question 84 in the 2019 questionnaire: ‘Now I’m going to list various alcoholic beverages, and you, tell me please, which of these you drank in the last 30 days and, for those you drank, how many grams you usually consumed in a day?
o	Check if the question varies for 2016-2018 questionnaires.

GENACIS/GENAHTO: see variable btyv in the codebook: 'Total consumption of alcohol (in standard drinks) in the last 12 months. 
Respondents reported the quantity and frequency of consuming each of four alcohol beverage types in the past 12 months: beer, wine, spirits and other beverages. The quantity of each of the alcohol beverage types was capped to correct for any unrealistically high reported quantity values. The total consumption of alcohol in the last 12 months for each alcohol beverage type was calculated by multiplying the usual quantity consumed per occasion they consumed the beverage type and frequency of drinking the beverage type. Then the total consumption of alcohol per year from all beverages was derived by the summing the total consumption of alcohol per year from each of the four alcohol beverage types: beer, wine, spirits, and other beverages. Note – units are in centilitres of pure alcohol or standard drinks (1 centilitre = 10ml = 1 standard drink).
o	Convert from centrelitres to grams (1 centilitre = 1 standard drink = 10 grams)
 
CTADS
-Change HEDAEVER and HEDASEEVER to the names HEDA365 and HEDASE365: DONE
-Are you able to add in at least monthly heavy episodic drinking (i.e. HEDTLM = 12)? : DONE
-Are you able add in Lifetime Abstainer data? I know we talked about the issue with the ‘valid skip’ but want to see if we can obtain any LA data.: I have included it. (Total number of people who have never drank / Total number of people within that age interval)
 
-GENACIS
-Same as above for variable name change (HEDA365 and HEDASE365): DONE
-Are you able to add in at least monthly heavy episodic drinking (i.e. HEDTLM = 12)? : DONE
-add in response rates by country for variable ‘resprate’ (see attachment for data). Divide value by 100 (e.g. Chile response rate of 72%  = .72 for ‘resprate’). : DONE
-for variable ‘ref’, update to ‘GENACIS-GENAHTO team’ : DONE
 
STEPS
-Change HEDAEVER and HEDASEEVER to the names HEDA30 and  HEDASE30: DONE
-Are you able to add in at least monthly heavy episodic drinking (i.e. HEDTLM = 12)?  We only have variables for heavy episodic drinking in the past 30 days
 
RLMS (once its ready):
-change HEDAEVER name to whichever is relevant (i.e .30-day) DONE
-Are you able to add in at least monthly heavy episodic drinking (i.e. HEDTLM = 12)? We only have variables for heavy episodic drinking in the past 30 days

