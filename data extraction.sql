-- body temperature abnormalities and critically ill patients
-- extracted from eICU-CRD

WITH labs_icu AS
(
SELECT
  pvt.uniquepid, pvt.patienthealthsystemstayid, pvt.patientunitstayid
-- blood routine
  , min(CASE WHEN labname = 'WBC x 1000' THEN labresult ELSE null end) as WBC_min
  , max(CASE WHEN labname = 'WBC x 1000' THEN labresult ELSE null end) as WBC_max
  , avg(CASE WHEN labname = 'WBC x 1000' THEN labresult ELSE null end) as WBC_avg
  , min(CASE WHEN labname = '-bands' THEN labresult ELSE null END) as BANDS_min
  , max(CASE WHEN labname = '-bands' THEN labresult ELSE null END) as BANDS_max
  , avg(CASE WHEN labname = '-bands' THEN labresult ELSE null END) as BANDS_avg
  , min(CASE WHEN labname = '-polys' THEN labresult ELSE null END) as POLYS_min
  , max(CASE WHEN labname = '-polys' THEN labresult ELSE null END) as POLYS_max
  , avg(CASE WHEN labname = '-polys' THEN labresult ELSE null END) as POLYS_avg
  , min(CASE WHEN labname = '-lymphs' THEN labresult ELSE null END) as LYMPHS_min
  , max(CASE WHEN labname = '-lymphs' THEN labresult ELSE null END) as LYMPHS_max
  , avg(CASE WHEN labname = '-lymphs' THEN labresult ELSE null END) as LYMPHS_avg
  , min(CASE WHEN labname = 'Hgb' THEN labresult ELSE null END) as HGB_min
  , max(CASE WHEN labname = 'Hgb' THEN labresult ELSE null END) as HGB_max
  , avg(CASE WHEN labname = 'Hgb' THEN labresult ELSE null END) as HGB_avg
  , min(CASE WHEN labname = 'Hct' THEN labresult ELSE null END) as HCT_min
  , max(CASE WHEN labname = 'Hct' THEN labresult ELSE null END) as HCT_max
  , avg(CASE WHEN labname = 'Hct' THEN labresult ELSE null END) as HCT_avg
  , min(CASE WHEN labname = 'platelets x 1000' THEN labresult ELSE null END) as PLT_min
  , max(CASE WHEN labname = 'platelets x 1000' THEN labresult ELSE null END) as PLT_max
  , avg(CASE WHEN labname = 'platelets x 1000' THEN labresult ELSE null END) as PLT_avg
FROM
( -- begin query that extracts the data
  SELECT p.uniquepid, p.patienthealthsystemstayid, p.patientunitstayid, le.labname

  -- add in some sanity checks on the values; same checks from original MIMIC version
  -- the where clause below requires all labresult to be > 0, so these are only upper limit checks
  , CASE
     WHEN labname = 'WBC x 1000' and le.labresult >  1000 THEN null -- 'WBC'
     WHEN labname = 'WBC x 1000' and le.labresult <  0 THEN null
     WHEN labname = '-bands' and le.labresult <     0 THEN null -- immature band forms, %
     WHEN labname = '-bands' and le.labresult >   100 THEN null -- immature band forms, %
     WHEN labname = '-polys' and le.labresult <     0 THEN null -- immature polys forms, %
     WHEN labname = '-polys' and le.labresult >   100 THEN null -- immature polys forms, %
     WHEN labname = '-lymphs' and le.labresult <     0 THEN null -- immature lymphs forms, %
     WHEN labname = '-lymphs' and le.labresult >   100 THEN null -- immature lymphs forms, %
     WHEN labname = 'Hgb' and le.labresult >    50 THEN null -- g/dL 'HEMOGLOBIN'
     WHEN labname = 'Hgb' and le.labresult <    0 THEN null
     WHEN labname = 'Hct' and le.labresult >   100 THEN null -- % 'HEMATOCRIT'
     WHEN labname = 'Hct' and le.labresult <   0 THEN null
     WHEN labname = 'platelets x 1000' and le.labresult > 10000 THEN null -- K/uL 'PLATELET'
     WHEN labname = 'platelets x 1000' and le.labresult < 0 THEN null
   ELSE le.labresult
   END AS labresult

  FROM patient p

  LEFT JOIN lab le
    ON p.patientunitstayid = le.patientunitstayid
    AND le.labresultoffset >= 0
    AND le.labname in
    (
      'WBC x 1000',
      '-bands',
      '-polys',
      '-lymphs',
      'Hgb',
      'Hct',
      'platelets x 1000'
    )
    AND labresult IS NOT null  
) pvt
),
nlratio AS
(
SELECT
  nlr.patientunitstayid
  , max(nlr) as NLR_max
  , min(nlr) as NLR_min
  , avg(nlr) as NLR_avg
FROM
(  SELECT patientunitstayid, chartoffset, polys, lypmhs, (polys::numeric/lypmhs::numeric) AS nlr
   FROM pivoted_lab 
   WHERE chartoffset >= 0
   AND polys > 0 AND l.polys < 100
   AND lypmhs > 0 AND l.lypmhs < 100
) nlr
GROUP BY nlr.patientunitstayid
),
interventions AS
(
    SELECT DISTINCT ON (pt.patientunitstayid)
       pt.patientunitstayid,
       MAX(CASE WHEN t.treatmentstring LIKE 'pulmonary|ventilation and oxygenation|CPAP/PEEP therapy%' 
            OR t.treatmentstring LIKE 'pulmonary|ventilation and oxygenation|mechanical ventilation%' 
            OR t.treatmentstring LIKE 'pulmonary|ventilation and oxygenation|non-invasive ventilation%' 
            OR t.treatmentstring LIKE 'pulmonary|ventilation and oxygenation|ventilator weaning%' 
            OR t.treatmentstring LIKE 'surgery|pulmonary therapies|mechanical ventilation%' 
            OR t.treatmentstring LIKE 'surgery|pulmonary therapies|non-invasive ventilation%' 
            OR t.treatmentstring LIKE 'surgery|pulmonary therapies|ventilator weaning%' 
            OR t.treatmentstring LIKE 'toxicology|drug overdose|mechanical ventilation%' 
            OR t.treatmentstring LIKE 'toxicology|drug overdose|non-invasive ventilation%' 
            THEN 1 ELSE 0 END) AS ventilation_flag,
       MAX(CASE WHEN t.treatmentstring LIKE 'renal|dialysis%' 
            OR t.treatmentstring LIKE '%|electrolyte correction|%|dialysis'
            THEN 1 ELSE 0 END) AS dialysis_flag,
       MAX(CASE WHEN t.treatmentstring LIKE 'cardiovascular|shock|vasopressors%'
            OR t.treatmentstring LIKE 'neurologic|therapy for controlling cerebral perfusion pressure|vasopressors%'
            OR t.treatmentstring LIKE 'surgery|cardiac therapies|vasopressors%'
            OR t.treatmentstring LIKE 'toxicology|drug overdose|vasopressors%'
            THEN 1 ELSE 0 END) AS vasopressor_flag,
       MAX(CASE WHEN t.treatmentstring LIKE 'cardiovascular|other therapies|antibacterials%'
            OR t.treatmentstring LIKE 'infectious diseases|medications|prophylactic antibacterials%'
            OR t.treatmentstring LIKE 'infectious diseases|medications|therapeutic antibacterials%'
            OR t.treatmentstring LIKE 'pulmonary|medications|antibacterials%'
            OR t.treatmentstring LIKE 'pulmonary|medications|antifungal therapy%'
            OR t.treatmentstring LIKE 'renal|medications|antibacterials%'
            OR t.treatmentstring LIKE 'surgery|infection|prophylactic antibacterials%'
            OR t.treatmentstring LIKE 'surgery|infection|therapeutic antibacterials%'
            OR t.treatmentstring LIKE 'surgery|infection|antifungal therapy%'
            OR t.treatmentstring LIKE 'infectious diseases|medications|antifungal therapy%'            
            OR t.treatmentstring LIKE 'renal|medications|antifungal therapy%'
            THEN 1 ELSE 0 END) AS antibiotics_flag
    FROM patient pt
    LEFT JOIN treatment t
    ON pt.patientunitstayid = t.patientunitstayid
),
comorbidities AS
(        
    SELECT DISTINCT ON (pt.patientunitstayid)
       pt.patientunitstayid,
       MAX(CASE WHEN d.diagnosisstring LIKE 'cardiovascular|vascular disorders|hypertension%'
            OR d.diagnosisstring LIKE 'cardiovascular|ventricular disorders|hypertension%'
            OR d.diagnosisstring LIKE 'surgery|acute cardiac problems|hypertension%'
            OR d.diagnosisstring IN ('cardiovascular|ventricular disorders|acute pulmonary edema|due to hypertension', 'cardiovascular|ventricular disorders|acute pulmonary edema|due to malignant hypertension'
            , 'cardiovascular|ventricular disorders|cardiomyopathy|hypertrophic|hypertensive with LVH', 'transplant|s/p kidney transplant|hypertension - kidney transplant')
            THEN 1 ELSE 0 END) AS hypertension_flag,
       MAX(CASE WHEN d.diagnosisstring LIKE 'endocrine|glucose metabolism|diabetes mellitus%'
            OR d.diagnosisstring LIKE 'infectious diseases|skin, bone and joint infections|diabetic foot infection%'
            OR d.diagnosisstring IN ('neurologic|neuromuscular disorders|neuropathy|from diabetes', 'renal|disorder of acid base|metabolic acidosis|ketoacidosis/diabetic')
            THEN 1 ELSE 0 END) AS diabetes_flag,
       MAX(CASE WHEN d.diagnosisstring LIKE 'renal|disorder of kidney|chronic kidney disease%'
            OR d.diagnosisstring IN ('renal|disorder of kidney|chronic renal insufficiency', 'renal|disorder of kidney|ESRD (end stage renal disease)')
            THEN 1 ELSE 0 END) AS CKD_flag,
       MAX(CASE WHEN d.diagnosisstring LIKE 'pulmonary|disorders of the airways|acute COPD exacerbation%'
            OR d.diagnosisstring LIKE 'pulmonary|disorders of the airways|COPD%'
            THEN 1 ELSE 0 END) AS COPD_flag,
       MAX(CASE WHEN d.diagnosisstring LIKE 'cardiovascular|ventricular disorders|congestive heart failure%'
            OR d.diagnosisstring LIKE 'surgery|acute cardiac problems|congestive heart failure%'
            OR d.diagnosisstring IN ('cardiovascular|shock / hypotension|sepsis|sepsis with single organ dysfunction- congestive heart failure', 'endocrine|fluids and electrolytes|hyponatremia|due to congestive heart failure'
            , 'infectious diseases|systemic/other infections|sepsis|sepsis with single organ dysfunction- congestive heart failure', 'pulmonary|disorders of vasculature|pulmonary hemorrhage|due to left sided heart failure'
            , 'pulmonary|pleural disorders|pleural effusion|due to heart failure', 'renal|electrolyte imbalance|hyponatremia|due to congestive heart failure', 'surgery|infections|sepsis|sepsis with single organ dysfunction- congestive heart failure')
            THEN 1 ELSE 0 END) AS heartfailure_flag,
       MAX(CASE WHEN d.diagnosisstring LIKE 'endocrine|endocrine tumors%'
            OR d.diagnosisstring LIKE 'gastrointestinal|intestinal disease|GI obstruction / ileus|%|tumor'
            OR d.diagnosisstring LIKE 'gastrointestinal|intestinal disease|viscus perforation|%|due to tumor'
            OR d.diagnosisstring LIKE 'gastrointestinal|post-GI surgery|s/p surgery for cancer%'
            OR d.diagnosisstring LIKE 'neurologic|CNS mass lesions|brain tumor%'
            OR d.diagnosisstring LIKE 'oncology|chest tumors%'
            OR d.diagnosisstring LIKE 'oncology|CNS tumors%'
            OR d.diagnosisstring LIKE 'oncology|GI tumors%'
            OR d.diagnosisstring LIKE 'oncology|GU tumors%'
            OR d.diagnosisstring LIKE 'oncology|head and neck tumors%'
            OR d.diagnosisstring LIKE 'oncology|skin, muscle and skeletal tumors%'
            OR d.diagnosisstring LIKE 'pulmonary|disorders of lung parenchyma|malignancy%'
            OR d.diagnosisstring LIKE 'surgery|head and neck surgery|s/p head and neck cancer surgery%'
            THEN 1 ELSE 0 END) AS cancer_flag,
        MAX(CASE WHEN d.diagnosisstring LIKE 'infectious diseases|systemic/other infections|bacteremia%'
            OR d.diagnosisstring LIKE 'infectious diseases|systemic/other infections|fungemia%'
            OR d.diagnosisstring LIKE 'infectious diseases|systemic/other infections|sepsis%'
            OR d.diagnosisstring LIKE 'infectious diseases|systemic/other infections|signs and symptoms of sepsis (SIRS)%'
            OR d.diagnosisstring LIKE 'surgery|infections|sepsis%'
            OR d.diagnosisstring LIKE 'surgery|infections|signs and symptoms of sepsis (SIRS)%'
            OR d.diagnosisstring LIKE 'cardiovascular|shock / hypotension|sepsis%'
            OR d.diagnosisstring LIKE 'cardiovascular|shock / hypotension|signs and symptoms of sepsis (SIRS)%'
            OR d.diagnosisstring LIKE 'infectious diseases|systemic/other infections|septic shock%'
            OR d.diagnosisstring LIKE 'surgery|infections|septic shock%'
            OR d.diagnosisstring LIKE 'cardiovascular|shock / hypotension|sepsis%'
            OR d.diagnosisstring LIKE 'cardiovascular|shock / hypotension|septic shock%'
            OR d.diagnosisstring LIKE 'cardiovascular|shock / hypotension|signs and symptoms of sepsis (SIRS)%'
            OR d.diagnosisstring IN ('pulmonary|respiratory failure|acute lung injury|non-pulmonary etiology|sepsis',
            'pulmonary|respiratory failure|ARDS|non-pulmonary etiology|sepsis',
            'renal|disorder of kidney|acute renal failure|due to sepsis', 'renal|electrolyte imbalance|hypocalcemia|due to sepsis',
            'surgery|renal issues|acute renal failure|due to sepsis','cardiovascular|vascular disorders|arterial thromboembolism|due to sepsis',
            'cardiovascular|vascular disorders|peripheral vascular ischemia|due to sepsis', 
            'endocrine|fluids and electrolytes|hypocalcemia|due to sepsis',
            'hematology|coagulation disorders|DIC syndrome|associated with sepsis/septic shock')
            THEN 1 ELSE 0 END) AS sepsis_flag,
        MAX(CASE WHEN d.diagnosisstring LIKE 'infectious diseases|systemic/other infections|septic shock%'
            OR d.diagnosisstring LIKE 'surgery|infections|septic shock%'
            OR d.diagnosisstring LIKE 'cardiovascular|shock / hypotension|septic shock%'
            THEN 1 ELSE 0 END) AS septicshock_flag，
        MAX(CASE WHEN d.diagnosisstring LIKE 'burns/trauma|trauma - CNS%'
            OR d.diagnosisstring LIKE 'neurologic|trauma%'
            OR d.diagnosisstring LIKE 'neurologic|disorders of vasculature|%secondary to trauma'
            OR d.diagnosisstring LIKE 'cardiovascular|cardiac arrest|cardiac arrest%'
            OR d.diagnosisstring LIKE 'infectious diseases|CNS infections%'
            OR d.diagnosisstring LIKE 'neurologic|CNS mass lesions|brain abscess%'
            OR d.diagnosisstring LIKE 'neurologic|infectious disease of nervous system|CNS abscess%'
            OR d.diagnosisstring LIKE 'neurologic|disorders of vasculature|stroke|hemorrhagic stroke|subarachnoid hemorrhage%'
            OR d.diagnosisstring LIKE 'neurologic|post-neurosurgery|post craniotomy%'
            OR d.diagnosisstring IN ('endocrine|fluids and electrolytes|diabetes insipidus|central|from trauma', 'endocrine|pituitary and temperature regulation|central diabetes insipidus|from trauma'
            , 'neurologic|post-neurosurgery|post craniotomy|for hemorrhage', 'neurologic|CNS mass lesions|cerebral mass of unknown etiology|likely hemorrhage')
            THEN 1 ELSE 0 END) AS exclusion_flag
    FROM patient pt
    LEFT JOIN diagnosis d
    ON pt.patientunitstayid = d.patientunitstayid
),
temp AS
(
SELECT 
        pvt.patientunitstayid, pvt.chartoffset AS chartoffset, pvt.temperature AS temp
FROM pivoted_vital pvt 
WHERE pvt.temperature IS NOT NULL
UNION
SELECT
        vt.patientunitstayid, vt.observationoffset AS chartoffset, 
        (CASE WHEN vt.temperature > 25 AND vt.temperature < 46 THEN vt.temperature ELSE NULL END) AS temp
FROM vitalperiodic  vt
WHERE vt.temperature IS NOT NULL   
),
t_icustay AS
(
SELECT 
        patientunitstayid, MAX(temp) AS temp_max, MIN(temp) AS temp_min, AVG(temp) AS temp_avg
FROM temp 
WHERE chartoffset >= 0
GROUP BY patientunitstayid
),
tfirstday AS
(
SELECT 
        patientunitstayid, MAX(temp) AS T_max, MIN(temp) AS T_min, AVG(temp) AS T_avg
FROM temp 
WHERE chartoffset BETWEEN 0 AND 1440
GROUP BY patientunitstayid
),
vitalsfirstday AS
(
SELECT vt.patientunitstayid,
       MAX(vt.heartrate) AS HR_max, MIN(vt.heartrate) AS HR_min, AVG(vt.heartrate) AS HR_avg,
       MAX(vt.RespiratoryRate) AS RR_max, MIN(vt.RespiratoryRate) AS RR_min, AVG(vt.RespiratoryRate) AS RR_avg, 
       MAX(vt.spo2) AS SpO2_max, MIN(vt.spo2) AS SpO2_min, AVG(vt.spo2) AS SpO2_avg
 FROM      
 (
    SELECT  
           pvt.patientunitstayid, 
           pvt.heartrate, pvt.RespiratoryRate, pvt.spo2  
    FROM pivoted_vital pvt
    WHERE pvt.chartoffset BETWEEN 0 AND 1440
 ) vt
 GROUP BY vt.patientunitstayid
)
SELECT DISTINCT ON (pt.uniquepid, pt.patienthealthsystemstayid, pt.patientunitstayid, pt.unitvisitnumber)
       --basline characteristics & outcomes
       pt.uniquepid, pt.patienthealthsystemstayid, pt.patientunitstayid, 
       pt.unitvisitnumber,
       pt.hospitalid, h.region, pt.unittype,
       pt.hospitaladmitoffset, pt.hospitaldischargeoffset,
       pt.unitdischargeoffset,
       ap.apachescore AS apache_iv,
       pt.hospitaldischargeyear,
       pt.age, pt.apacheadmissiondx, 
       ag.apachedxgroup,
       CASE WHEN lower(pt.hospitaldischargestatus) like '%alive%' THEN 0
            WHEN lower(pt.hospitaldischargestatus) like '%expired%' THEN 1
            ELSE NULL END AS hosp_mortality,
       CASE WHEN lower(pt.unitdischargestatus) like '%alive%' THEN 0
            WHEN lower(pt.unitdischargestatus) like '%expired%' THEN 1
            ELSE NULL END AS icu_mortality,  
       CASE WHEN lower(pt.gender) like '%female%' THEN 0
            WHEN lower(pt.gender) like '%male%' THEN 1
            ELSE NULL END AS gender,
       pt.ethnicity, pt.admissionheight, pt.admissionweight, pt.dischargeweight,
       ROUND(pt.unitdischargeoffset/60) AS icu_los_hours,
       ROUND((pt.hospitaldischargeoffset - pt.hospitaladmitoffset)/60) AS hosp_los_hours,
       --interventions
       i.ventilation_flag, i.dialysis_flag, i.vasopressor_flag, i.antibiotics_flag,
       i.cooling_flag, i.warming_flag, i.antipyretics_flag,
    -- Comorbidities    
       c.hypertension_flag, c.diabetes_flag, c.CKD_flag, c.COPD_flag, c.heartfailure_flag, c.cancer_flag,    
       c.sepsis_flag, c.septicshock_flag, c.exclusion_flag, app.aids, 
    --Tmax
       ti.temp_max, ti.temp_min, (ti.temp_max - ti.temp_min) AS t_change,
    --vitalsigns day1
       v.HR_max AS HR_max_24hours, v.HR_min AS HR_min_24hours, v.HR_avg AS HR_avg_24hours,
       v.RR_max AS RR_max_24hours, v.RR_min AS RR_min_24hours, v.RR_avg AS RR_avg_24hours, 
       v.SpO2_max AS SpO2_max_24hours, v.SpO2_min AS SpO2_min_24hours, v.SpO2_avg AS SpO2_avg_24hours, 
    --labresults day1
       --blood routine--
       l1.WBC_min , l1.WBC_max , l1.WBC_avg ,  
       l1.POLYS_min , l1.POLYS_max , l1.POLYS_avg , 
       l1.LYMPHS_min , l1.LYMPHS_max , l1.LYMPHS_avg ,
       nlr.NLR_max , nlr.NLR_min , nlr.NLR_avg ,
       l1.HGB_min , l1.HGB_max , l1.HGB_avg ,
       l1.HCT_min , l1.HCT_max , l1.HCT_avg , 
       l1.PLT_min , l1.PLT_max , l1.PLT_avg 
FROM patient pt
INNER JOIN hospital h
    ON pt.hospitalid = h.hospitalid
INNER JOIN apachepatientresult ap
    ON pt.patientunitstayid = ap.patientunitstayid
INNER JOIN apache_groups ag
    ON pt.patientunitstayid = ag.patientunitstayid
INNER JOIN t_icustay ti
    ON pt.patientunitstayid = ti.patientunitstayid
INNER JOIN comorbidities c
    ON pt.patientunitstayid = c.patientunitstayid
    AND c.exclusion_flag = 0
LEFT JOIN interventions i
    ON pt.patientunitstayid = i.patientunitstayid
LEFT JOIN apachepredvar app
    ON pt.patientunitstayid = app.patientunitstayid
LEFT JOIN vitalsfirstday v
    ON pt.patientunitstayid = v.patientunitstayid
LEFT JOIN labs_icu l1
    ON pt.patientunitstayid = l1.patientunitstayid
LEFT JOIN nlratio nlr
    ON pt.patientunitstayid = nlr.patientunitstayid
ORDER BY pt.uniquepid, pt.patienthealthsystemstayid, pt.patientunitstayid, pt.unitvisitnumber;

