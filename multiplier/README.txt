# Introduction
The purpose of this directory is to calculate multipliers on the FIM. As of May 2024, the plan for this feature is that it will be reincorporated into the new FIM once refactoring is complete. For now, the script in this directory operates separately on an output file that is produced when the FIM runs.

The multipliers we use as a starting point in this section come from Table 2 of Whalen and Reichling, "The Fiscal Multiplier and Economic Policy Analysis in the United States" (February 2015, CBO Working Paper 2015-02). 
https://www.cbo.gov/sites/default/files/114th-congress-2015-2016/workingpaper/49925-FiscalMultiplier_1.pdf

# Procedure
Our procedure is as follows. Multipliers operate on nominal dollar stimulus. We multiply nominal GDP in a given quarter by our FIM to calculate nominal stimulus. To this value, we then apply multipliers, such as (0.5, -0.03, -0.04, -0.05, -0.06, -0.06, -.05, -0.05) to 8 quarters, starting in the quarter the money was disbursed. We then deflate these post-multiplier, nominal values according to an overall deflator to get time series effect of the FIM in real terms, including the multipliers.

EXAMPLE.
Suppose the nominal GDP is $26 T in Q1 2020 and we calculate an (annualized) FIM of 4% for that quarter. That means that we estimate annualized Q1 2020 GDP is 4% higher than it would be sans stimulus: therefore, the nominal stimulus in that quarter can be solved as:
$26 T = 1.04X
Where X is what GDP would have been, sans FIM. In this case, X = $25 T.
Nominal stimulus is then:
$25 T * 0.04 = $1 T.
(Recall that this $1T is annualized, so the actual amount of nominal stimulus from that quarter will be roughly 4x lower. But in the FIM, we work only with annualized numbers, so we won't bother ourselves with this de-annualizing math.)

In general terms, nominal stimulus in a certain quarter can be expressed in terms of the FIM (as a decimal) and nominal GDP as follows:
(nom $ stimulus) = (nom $ GDP)*(FIM)/(1 + FIM)

Now that we know that nominal stimulus in Q1 2020 was $1 T, we simply distribute this dollar amount according to our chosen multiplier scheme. If the scheme is (0.5, 0.5, 0.5, 0.5) for the next 4 quarters (meaning our total multiplier is 2), then we will allocate $0.5 T to Q1 2020, $0.5 T to Q2 2020, $0.5 T to Q3 2020, and $0.5 T to Q4 2020. This is the exact same type of vector math we use to allocate MPCs conceptually in the FIM.

Now that we have these cash flows, we'll sum them with any other cash flows that we get from other quarters. For example, we might repeat this exercise using Q2 2020 nominal GDP and its FIM data and add additional cash flows to some of the quarters listed above. But for the purposes of this illustration, let's assume all other FIM values from other quarters are zero, so our total post-multiplier stimulus in the listed quarters comes only from the Q1 2020 disbursement. In other words, after all our analysis, we find that fiscal stimulus has raised nominal GDP by the following amounts each quarter:
Q1 2020: +$0.5 T
Q2 2020: +$0.5 T
Q3 2020: +$0.5 T
Q4 2020: +$0.5 T

Now our last step is to just deflate this time series. Which multiplier do we choose? A sector-specific one or an economy-wide one? Conceptually, a multiplier is economy-wide. Let's suppose all of the stimulus  measured by the FIM from Q1 2020 comes from strong government purchases of military equipment from defense contractors. Should we use the federal purchases deflator? No. The multiplier effects the entire economy because of the second-, third-, and nth-round effects. Even if the government contractor receives the direct stimulus, boosting its revenues, it may choose to elevate its bonus to its employees, who then spend the additional disposible income on airline flights to vacations, clothing from Target, and new cars. The airlines then spend more money on catering services, Target spend more money on warehouse space, and the car dealers add to their inventory. The effects continue to ripple through the economy.

So, to deflate, we simply apply the deflator time series to these nominal annualized fiscal multiplier time series to get a real annualized fiscal multiplier time series. Suppose inflation is very fast, and our deflator series is (1, 1.25, 1.66, 2.5)

Then in real terms, the GDP effect our post- fiscal multiplier disbursements become:
Q1 2020: +$0.5 T * 1 = +$0.5 T
Q2 2020: +$0.5 T * (1/1.25) = +$0.4 T
Q3 2020: +$0.5 T * (1/1.66) = +$0.3 T
Q4 2020: +$0.5 T * (1/2.5) = +$0.2 T

Voila! We now have the real effect of the FIM, with multipliers. To convert it back into a decimal, we simply take each of these values and divide by annualized real GDP:
[I don't feel like typing this out but it's simple]
