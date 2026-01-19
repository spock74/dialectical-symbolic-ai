# Comparative Analysis Report

## Overview
Comparison of Zero-Shot vs Optimized Models on Logic Generation.

## Visualizations
![Validity Chart](validity_chart.png)

![Hallucination Chart](hallucination_chart.png)

## Detailed Trace
### Strategy: Zero-Shot
| Category | Input | Output | Valid? | Hallucination? |
| :--- | :--- | :--- | :--- | :--- |
| Syllogism | Socrates is a man. All men are.. | `['(IS-A MEN HUMAN)', '(IS-A MORTAL LIFE)']` | ❌ |  |
| Unseen FOLIO | All CD players are delicate me.. | `['(DELICATE-CONTROL ANYTHING) => (SUITABLE-TOWNKS-SOME-NOTHINGS)', '(CDS SOME-DELICATE-CONTROLS) AND NOT(SUITABLE-TOWNS-CDS SUTHING CD-PLAYERS)']` | ❌ |  |
| Unseen FOLIO | If people play the instrument .. | `['(GOOD-AT-PLAYING-PERSON PERSON INSTRUMENT CONCERT)', '(GAMES (PETER INSTRUMENTS PIANO VIOLEIN SAXPHONE) CONSERVATION1)', '(CONSERVATION1 CONSERVATION2)', '(INVITED TO PERFORM Person Instrument CONCERT_HALL 2023)', '(PLAYED-DIFFERENT-INSTRUMENT Oliver PERSON Peter-person 2023-01-01-CONTRADICTION)']` | ❌ | ⚠️ |
| Unseen FOLIO | Something is either an invasiv.. | `['(NOT (INVADE SMNIR IMAGING TECHNIQUE) AND (USED BY NEUROSCIENCE RESEARCHERS)', '(ALL noninvasive SNMIR techniques PROVIDE spatial resolution OF BRAIN)', '(IF provides SPATIAL RESOLUTION OF BRAINS THEN MEASUREMENT OF brain activity)', '(all measurements of brain activity ARE used by neuroscience researchers)', '((INVADE SMNIR FMRI) OR (NONINVADE SNMIR TECHNIQUE))', 'CIRCULARITY_OR_CONFLICT']` | ❌ |  |
| Unseen FOLIO | Ableton has an office in Germa.. | `['(HAS-OR-HAS-NOT office-Germany Ableton)', '(MULTINATIONAL-company or NOT-MULTINATIONAL-company (HAS-OFFICE Germany-Ableton))', '(USA AND Germany ARE different countries)', '(ANY company that HAS offices in different-countries IS a multinational-companies) => ((IS-a-Company Ableton), (HAVE-offices-in-different-countries Ableton))']` | ❌ |  |
| Unseen FOLIO | ETS develops various standardi.. | `['(DEVELOPS ETS K-12 EDUCATION TESTS)', '(DEVELOPES INTERNATIONAL ASSESSMENTS) (ETS TOEFL GRE TOEIC)', '(ADMINISTRATES TESTS INTERNALLY NOT INTERNATIVELY)']` | ❌ |  |
| Unseen FOLIO | Products are either produced i.. | `['(ORIENTS PRODUCT China OR US)', '(LABEL-PRODUCT IF PROD IN CHINA, THEN LABELED)', '(SELL-LOCALLY PRODUCT WHILE PROD IN US)', '(CHEAPER THAN BASE LINEARITY LABEL FOR ALL PRODUCTS) (IF PRODUCED AND LABELED, THEN CHEAPER-BASIS-SALEPRICE; ELSE GREATER OR EQUAL TO BASIS-SALEPRICE)', '(WALMARTS SALEPLACE FOR SOLD-US-RETAILER IS Walmart)', '(DISPLAYED-WHERE IN-HOME WAREHOUSE OR ONLINE-EVENINWalmart) (FOR ALL PRODUCTS DISPLAYED ON HOMEPAGE, THEN WHOLESALE-BASKETTERY-Walmart)', '(NOT-SOLD-A-FOR-CUSTOMERS REASONS FOR RETURN G-910 NOT SOLD-IN Walmart)', '((DISPLAY-G910-HOMEPAGE OR CHEAPER-G910)-AND-(NOT-STUCK-NOT-SOLD) (THEN NO RETURNS ON G-910))']` | ❌ |  |
| Unseen FOLIO | No people who work at corporat.. | `['(OR (RISK_AVERAGE_PERSON WORKS_FOR_OTHERS) CORRIE-1)', '(CORRODE-WITH CORPORATE_JOB CORPO-JOB-2)', '(AND CORPO-JOB-2 NOT ENTREP-ENTREPI CORPO-S2),', '((NOT ENTREP-ZUCK-MARK)) (FORCE NEGATION ON RISK_AVERAGE_PERS 3) ZUCK-PROOF-V4 ', '(CORRELATE ENTREP-HATES WORKING FOR OTHERS OR MARK_ZUCKERMAGER-ENTREPI CORPO-S2)', '((OR NOT Mark-Zuck-Merk AND (HATS \|\| ENTR)) NEGATION ON RISK_AVERAGE_PARS ZUKU-KPROOF-V4)']` | ❌ |  |
| Unseen FOLIO | If people own at least one pet.. | `['(>-> ((OWN-ANYPETS ANYTHING) NOT(HAS-TYDEOUS-HOUSE))', '>-(GROWN-UP-WITH-CATS CARS) (>= ANYTHING owns PUPPY)', '(LIVES-IN-SUBURBS HAS-TYDEOUS-House)>', '>(NOT HIRE-MAID OR (>HIRE-MAID AND NOT OWN-ANYPETS)) (LIVES-IN SUBURBS)']` | ❌ |  |

### Strategy: Optimized (V2)
| Category | Input | Output | Valid? | Hallucination? |
| :--- | :--- | :--- | :--- | :--- |
| Syllogism | Socrates is a man. All men are.. | `(IS A MAN SOCRATES) (ALL MEN ARE MORTAL) ((IS-A-MAN) SOCRATES)` | ✅ |  |
| Unseen FOLIO | If someone yells, then they ar.. | `(YELS-X-IF-NOT-COOLX)(ANGRY-YELLXY)(FLIES-WITHOUT_YELLS_X_FLIES_WIZARD_HARRY_FLYS(WIZARD-Harry_KNOWER-MAGIC)(WIZARDS-FLY(MAGICK-KNOWER))\(NOT YELLS HARRY AND FLY HARRY OR (YELLS HARRY AND NOT FLIES HARRY))(HARRY-WIZARD_AND_FLIES)((KNOWS-MAGIC-Harry) AND (FLIES-Harry))) => COOL(Harry)` | ✅ |  |
| Unseen FOLIO | All CD players are delicate me.. | `(FORALL CD-PLAYERS DELICATE-MECHANISMS) (NOT(SUBSET Delicate-Mechanisms S Suitable-Toys-for-Kids))` | ✅ |  |
| Unseen FOLIO | If people play the instrument .. | `(IF PLAY-INSTRUMENT-PETER PIANO THEN GOOD AT-PLAYING-PIANO) (PETER PLAYS PIANO AND VIOLIN SAXPHONE) (Oliver DIFFERENT FROM Peter IN INSTRUMENT) ((CHOOSE-INVITE FOR-PIERSON PIANO IMplies Good At Playing Piano))` | ✅ |  |
| Unseen FOLIO | Something is either an invasiv.. | `(NONINVASIVE OR INVASIVE NEUROIMAGING TECHNIQUE)\n((ALL NONINVASIVE NEUROIMAGING TECHNIQUES PROVIDE-SPATIAL-RESOLUTION OF BRAINS) AND (ALL MEASUREMENTS-BRAIN ACTIVITY ARE USED BY neuroscience RESEARCHERS)) \n(FMRI IS EITHER a measurement-of brain activity OR AN INVASIVE neuroimaging technique).\nConclusion: IF ((fMRI is NOT an invasive neuroimaging technique)) and (it IS used by neuroscience researchers), THEN it follows that fMRI IS neither noninvasive nor provides spatial resolution.` | ✅ |  |
| Unseen FOLIO | Ableton has an office in Germa.. | `(HAS-OFICE-Germany ABLETON) (HAS-OFICE-USA ABLETON) (MULTINATIONAL-COMPANY HAS-OFFICES-DISTINCT-COUNTRIES) (ABLETON MULTINATIONAL-COMPANY)` | ✅ |  |
| Unseen FOLIO | Rosa was born in Santiago.  Sa.. | `(JOSE-BUILDING-CONTRACTOR) (INFERENCE: JOSE-CONTROL-OF-Traffic SITE), ((ROSA-DATINE-JOSE))` | ✅ |  |
| Unseen FOLIO | ETS develops various standardi.. | `(DEVELOPS-ETS TESTS TRUE) (ADMINISTRATES-ETS TESTS FALSE)` | ✅ |  |
| Unseen FOLIO | Products are either produced i.. | `(OR PRODUCTS CHINA PRODUCTSUS) (AND (IMPLIES Labeled PRODUCTS China)) (AND (IMPLIES Soldus PRODUCTS US)) (IMPLIES Cheaper Labeled) (AND (INCLUDES Walmart SOLDinUS)) (AND (INCLUDES Homepage SOLDinWalmart)) (NOT RETURNS-SOLDinWalmart); CONCLUDE: G910 DISPLAYONHOME PAGE OR CHEAPER; NOT RETURNED-Customer` | ❌ |  |
| Unseen FOLIO | No people who work at corporat.. | `(AND (NOT EMPLOYEE-Corporate Mark Zuckerberg) (NOT HATES-WORKING-FOR-OTHERS-Mark Zuckerberg)) => NOT-HATTS-Others-Zuckerberg => NOT-RISK-AVOIDANT-Zuckerberg)` | ❌ |  |
| Unseen FOLIO | If people own at least one pet.. | `(HAS-TIDY-HOUSES HIRE-MAID OR CLEANING-SERVICE) (OWNES-ONE-PET NOT HAS-TIDY-HOUSES) (GROUPOF-GROWN-UP-WITH-COLLEGE PETS OWN-DEFINITELY-Owns-A-PET) (LIVES-IN SUBURBS TIDY-HOUSES). Jack DOES-NOT-HIRE-MAID OR CLEANING-SERVICE. If Jack HIRE-MAID OR CLEANING-SERVICE, THEN Jack NOT OWNS-ONE-PET. Therefore, Jack LIVES-IN SUBURBS.` | ✅ |  |

### Strategy: Optimized (Folio)
| Category | Input | Output | Valid? | Hallucination? |
| :--- | :--- | :--- | :--- | :--- |
| Syllogism | Socrates is a man. All men are.. | `(IS-A PERSON SOCRATES) (ALL-MEN-ARE-MORTAL ALL-MEN), ((EXISTS-SOME X Y Z IS-PERSON-X AND PREDICATE-Y-Z 'Mortal' SQUARE))` | ✅ |  |
| Unseen FOLIO | If someone yells, then they ar.. | `(IS-A YELL-NOT-COOL-JamesCocks) (IS-A ANGRY-YELL-OtherPerson) (IS-A COOL-FLY-SomeOneElse) (KNOW-MAGIC-WIZARD-KNOW-MAGIC-Harry-Potter) ((FLY AND NOT(YELL)) OR (NOT(FLY) AND NOT(COOL))) HarryPotter ` | ✅ | ⚠️ |
| Unseen FOLIO | All CD players are delicate me.. | `(ALL-CD_PLAYERS ARE DELICATE_MECHANISMS) (NOT-A-NOT-SUITABLE-TOWARD-KIDS DELICATE-MECHANISMS). NOT-ORIENTED-EACH-CD_PLAYER_IS_SUITED-TOWARDS-KIDS. ` | ✅ |  |
| Unseen FOLIO | If people play the instrument .. | `(IF (PLAY IN-CONCERT X) THEN (GOOD AT Y)) ((Y Z X) PLAY-X-ZETACOMMA JAMES Cocks) Play-Y-Piano-James-Cocks. Oliver-Play-DIFFERENT-INSTRUMENT-from-Peter-In-Concert.` | ✅ | ⚠️ |
| Unseen FOLIO | Something is either an invasiv.. | `(INVASIVE OR NONINVASIVE NeuroImagingTechnique SOMETHING). (NONINVASCIVE NeuroImagingTechniques provide spatial resolution of brains). ((PROVIDES SPATIAL RESOLUTION OF BRAIN) MEASUREMENT_BrainActivity). ALL MeasurementsBrainActivity ARE used by NeuroscienceResearchers. FMRI IS either an invasive neuroimaging technique OR a noninvasive neuroimaging technique. (IF-FMRI NOT INVASIVE NeuroImagingTechniques AND USED BY NEUROSCIENCE RESEARCHERS) Therefore, IF-THIS-CONCLUSION then THIS CONCLUSION is FALSE.` | ✅ |  |
| Unseen FOLIO | Ableton has an office in Germa.. | `(IS-IN-VENUE OF OFFICE ABLETON-Germany) (IS-IN-COUNTRY USA). NOT-SAME-COUNTRY germany-USA. COMPANY-MAKES MUSICSOFTWARE-Ableton. CONSTRAINT multi-national-company, UNKNOWN ` | ✅ |  |
| Unseen FOLIO | Rosa was born in Santiago.  Sa.. | `(BORN-IN SANTIAGO ROSA) (CAPITAL-LARGEST-CITY OF CHILE SANTIAGO) (PARENT-JOSE-Rosa). PARENTS-OF ROSA JOE AND CARMEN. JOSE-BUILDING-CONTRACTOR, NOT-TRAFFIC-OFFICER.` | ❌ |  |
| Unseen FOLIO | ETS develops various standardi.. | `(IS-A-CORPORATION ETS) (PRODUCE-PRODUCTS DEVELOP-TESTS-INTERNATIONAL-tests-within-North-America TOEFL TOEIC GRE K12 STATEWIDE ASSESSMENTS). NOT(IS-A-CORPORATION ETS CONCLUDES-IT-DONT ADMINISTER TESTS INTERNATILY)` | ✅ |  |
| Unseen FOLIO | Products are either produced i.. | `(PRODUCTION LOCATION CAPITAL 'G-910') (OR (PRODUCT 'G-910' CAPITA_L 'CHINA) ('G-910' PRODUCTION_CAPITAL_US)). ALL-PRODUCED-IN-US-Walmart. ALL-LABELED-G-910 DECREASED-COST. NO-CUSTOMER-PRODUCT-SOLD-Walmart. DISPLAY-HOME-G-910 OR (PRODUCT 'G-910' COST DECREASE).` | ✅ |  |
| Unseen FOLIO | No people who work at corporat.. | `(WORKS AT CORPORATE JOBS X) (NOT STARTING-COMPANY X) IF X THEN NOT RISK-AVOIDANT. Mark Zuckerberg Not Starting Companies And Hate Working For Others Implies That Mark Zuckerberg Is Not Risk Averse.` | ✅ |  |
| Unseen FOLIO | If people own at least one pet.. | `(OWN-PET-Jack) (NOT-TIDY-HOUSE-Jack). (Grew-Up-With-Childhood-Pet Jack)-> (HAS-A pet-Jack). If Hired-Maid-Cleaning-Service-Jack, then NOT-Owns-pets-Jack. But if Not-Hired-Maid-Cleaning-Service-Jack and Lives-in-Surroundings-Tidy-Jack)` | ❌ |  |

