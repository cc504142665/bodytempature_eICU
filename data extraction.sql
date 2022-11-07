--body temperature to sepsis versus non-sepsis
--eICU-CRD v2.0

with culture as
(
    select patientunitstayid, organism
    FROM `physionet-data.eicu_crd.microlab` 
    where organism not like 'no growth'
    and organism is not null
    and culturetakenoffset between -24*60 and 24*60
),
cohort as
(
    select d.patientunitstayid, id.diagnosisstring, c.organism, ss.sofatotal AS sofa
    from `physionet-data.eicu_crd.diagnosis` d
    inner join `my-project-sepsisprediction.infection.infection_diagnosis2` id
    on d.diagnosisstring = id.diagnosisstring
    and d.diagnosisoffset between -24*60 and 24*60
    full outer join culture c
    on d.patientunitstayid = c.patientunitstayid
    inner join `my-project-sepsisprediction.infection.sofascore` ss
    on d.patientunitstayid = ss.patientunitstayid
    and ss.sofatotal >= 2
),
comorbidities AS 
(    
    SELECT 
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
            THEN 1 ELSE 0 END) AS exclusion_flag,
        MAX(CASE WHEN d.diagnosisstring LIKE 'infectious diseases|systemic/other infections|septic shock%'
            OR d.diagnosisstring LIKE 'surgery|infections|septic shock%'
            --OR d.diagnosisstring LIKE 'cardiovascular|shock / hypotension|sepsis%'
            OR d.diagnosisstring LIKE 'cardiovascular|shock / hypotension|septic shock%'
            --OR d.diagnosisstring LIKE 'cardiovascular|shock / hypotension|signs and symptoms of sepsis (SIRS)%'
            THEN 1 ELSE 0 END) AS septicshock_flag
    FROM `physionet-data.eicu_crd.patient` pt
    LEFT JOIN `physionet-data.eicu_crd.diagnosis` d
    ON pt.patientunitstayid = d.patientunitstayid
    GROUP BY pt.patientunitstayid
),
interventions AS 
(
    SELECT 
       pt.patientunitstayid,
       MAX(CASE when t.treatmentstring like 'pulmonary|ventilation and oxygenation|mechanical ventilation|non-invasive ventilation%' then 0
            WHEN t.treatmentstring LIKE 'pulmonary|ventilation and oxygenation|CPAP/PEEP therapy%' 
            OR t.treatmentstring LIKE 'pulmonary|ventilation and oxygenation|mechanical ventilation%' 
            --OR t.treatmentstring LIKE 'pulmonary|ventilation and oxygenation|non-invasive ventilation%' 
            OR t.treatmentstring LIKE 'pulmonary|ventilation and oxygenation|ventilator weaning%' 
            OR t.treatmentstring LIKE 'surgery|pulmonary therapies|mechanical ventilation%' 
            --OR t.treatmentstring LIKE 'surgery|pulmonary therapies|non-invasive ventilation%' 
            OR t.treatmentstring LIKE 'surgery|pulmonary therapies|ventilator weaning%' 
            OR t.treatmentstring LIKE 'toxicology|drug overdose|mechanical ventilation%' 
            --OR t.treatmentstring LIKE 'toxicology|drug overdose|non-invasive ventilation%' 
            OR t.treatmentstring LIKE 'pulmonary|radiologic procedures / bronchoscopy|endotracheal tube%'
            OR t.treatmentstring LIKE 'toxicology|drug overdose|endotracheal tube%'
            OR t.treatmentstring LIKE 'pulmonary|surgery / incision and drainage of thorax|tracheostomy%'
            THEN 1 ELSE 0 END) AS ventilation_flag,
       MAX (CASE WHEN t.treatmentstring like 'pulmonary|surgery / incision and drainage of thorax|tracheostomy%'
            THEN 1 ELSE 0 END) as tracheostomy_flag,
       MAX(CASE WHEN t.treatmentstring LIKE 'renal|dialysis%' 
            OR t.treatmentstring LIKE '%|electrolyte correction|%|dialysis'
            THEN 1 ELSE 0 END) AS dialysis_flag,
       MAX(CASE WHEN t.treatmentstring LIKE 'cardiovascular|shock|vasopressors%'
            OR t.treatmentstring LIKE 'neurologic|therapy for controlling cerebral perfusion pressure|vasopressors%'
            OR t.treatmentstring LIKE 'surgery|cardiac therapies|vasopressors%'
            OR t.treatmentstring LIKE 'toxicology|drug overdose|vasopressors%'
            or t.treatmentstring like 'cardiovascular|%|inotropic agent%'
            or t.treatmentstring like 'surgery|cardiac therapies|inotropic agent%'
            or t.treatmentstring in ('renal|electrolyte correction|treatment of hypernatremia|vasopressin',
            'gastrointestinal|medications|hormonal therapy (for varices)|vasopressin',
            'toxicology|drug overdose|agent specific therapy|beta blockers overdose|dopamine')
            THEN 1 ELSE 0 END) AS vasopressor_flag,
        max(case when t.treatmentstring LIKE 'toxicology|drug overdose|vasopressors|%epinephrine <= 0.1 micrograms/kg/min'
            or t.treatmentstring like 'surgery|cardiac therapies|vasopressors|%epinephrine <= 0.1 micrograms/kg/min'
            or t.treatmentstring like 'neurologic|therapy for controlling cerebral perfusion pressure|vasopressors|%epinephrine <= 0.1 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|shock|vasopressors|%epinephrine <= 0.1 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|ventricular dysfunction|inotropic agent|%epinephrine <= 0.1 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|shock|inotropic agent|%epinephrine <= 0.1 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|myocardial ischemia / infarction|inotropic agent|%epinephrine <= 0.1 micrograms/kg/min'
            then 1 else 0 end)  as ene_less_flag,
        max(case when t.treatmentstring LIKE 'toxicology|drug overdose|vasopressors|%epinephrine > 0.1 micrograms/kg/min'
            or t.treatmentstring like 'surgery|cardiac therapies|vasopressors|%epinephrine > 0.1 micrograms/kg/min'
            or t.treatmentstring like 'neurologic|therapy for controlling cerebral perfusion pressure|vasopressors|%epinephrine > 0.1 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|shock|vasopressors|%epinephrine > 0.1 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|ventricular dysfunction|inotropic agent|%epinephrine > 0.1 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|shock|inotropic agent|%epinephrine > 0.1 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|myocardial ischemia / infarction|inotropic agent|%epinephrine > 0.1 micrograms/kg/min'
            then 1 else 0 end) as ene_over_flag,
       max(case when t.treatmentstring like 'cardiovascular|shock|inotropic agent|dopamine <= 5 micrograms/kg/min'
            or t.treatmentstring like 'surgery|cardiac therapies|inotropic agent|dopamine <= 5 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|ventricular dysfunction|inotropic agent|dopamine <= 5 micrograms/kg/min' 
            or t.treatmentstring like 'cardiovascular|myocardial ischemia / infarction|inotropic agent|dopamine <= 5 micrograms/kg/min'
            then 1 else 0 end)  as dop_less5_flag,
       max(case when t.treatmentstring like 'toxicology|drug overdose|vasopressors|dopamine 5-15 micrograms/kg/min'
            or t.treatmentstring like 'surgery|cardiac therapies|vasopressors|dopamine  5-15 micrograms/kg/min'
            or t.treatmentstring like 'neurologic|therapy for controlling cerebral perfusion pressure|vasopressors|dopamine 5-15 micrograms/kg/min' 
            or t.treatmentstring like 'cardiovascular|shock|vasopressors|dopamine  5-15 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|shock|inotropic agent|dopamine  5-15 micrograms/kg/min'
            or t.treatmentstring like 'surgery|cardiac therapies|inotropic agent|dopamine  5-15 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|ventricular dysfunction|inotropic agent|dopamine  5-15 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|myocardial ischemia / infarction|inotropic agent|dopamine  5-15 micrograms/kg/min' 
            then 1 else 0 end)  as dop_5to15_flag,
       max(case when t.treatmentstring like 'toxicology|drug overdose|vasopressors|dopamine >15 micrograms/kg/min'
            or t.treatmentstring like 'surgery|cardiac therapies|vasopressors|dopamine >15 micrograms/kg/min'
            or t.treatmentstring like 'neurologic|therapy for controlling cerebral perfusion pressure|vasopressors|dopamine > 15 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|shock|vasopressors|dopamine >15 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|shock|inotropic agent|dopamine >15 micrograms/kg/min'
            or t.treatmentstring like 'surgery|cardiac therapies|inotropic agent|dopamine >15 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|ventricular dysfunction|inotropic agent|dopamine >15 micrograms/kg/min'
            or t.treatmentstring like 'cardiovascular|myocardial ischemia / infarction|inotropic agent|dopamine >15 micrograms/kg/min' 
            then 1 else 0 end) as dop_over15_flag,
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
            OR t.treatmentstring LIKE 'renal|medications|systemic antibiotics%' 
            OR t.treatmentstring LIKE 'gastrointestinal|medications|antibiotics%'
            THEN 1 ELSE 0 END) AS antibiotics_flag
    FROM `physionet-data.eicu_crd.patient` pt
    LEFT JOIN `physionet-data.eicu_crd.treatment` t
    ON pt.patientunitstayid = t.patientunitstayid
    GROUP BY pt.patientunitstayid
    ORDER BY pt.patientunitstayid
),
lab0 as
(
  SELECT
  pvt.uniquepid, pvt.patienthealthsystemstayid, pvt.patientunitstayid
  , max(CASE WHEN pvt.labname = 'WBC x 1000' THEN pvt.labresult ELSE null end) as WBC
  , max(CASE WHEN pvt.labname = '-polys' THEN pvt.labresult ELSE null END) as POLYS
  , max(CASE WHEN pvt.labname = '-lymphs' THEN pvt.labresult ELSE null END) as LYMPHS
  , max(CASE WHEN pvt.labname = 'Hgb' THEN pvt.labresult ELSE null END) as HGB
  , max(CASE WHEN pvt.labname = 'Hct' THEN pvt.labresult ELSE null END) as HCT
  , max(CASE WHEN pvt.labname = 'platelets x 1000' THEN pvt.labresult ELSE null END) as PLT
  , max(CASE WHEN pvt.labname = 'lactate' THEN pvt.labresult ELSE null END) as LAC
  , max(CASE WHEN pvt.labname = 'ALT (SGPT)' THEN pvt.labresult ELSE null END) as ALT
  , max(CASE WHEN pvt.labname = 'AST (SGPT)' THEN pvt.labresult ELSE null END) as AST
  , max(CASE WHEN pvt.labname = 'total bilirubin' THEN pvt.labresult ELSE null END) as BIL
  , max(CASE WHEN pvt.labname = 'BUN' THEN pvt.labresult ELSE null END) as BUN
  , max(CASE WHEN pvt.labname = 'creatinine' THEN pvt.labresult ELSE null END) as SCR
FROM
(
    SELECT p.uniquepid, p.patienthealthsystemstayid, p.patientunitstayid, le.labname
     , CASE
     WHEN le.labname = 'WBC x 1000' and le.labresult >  1000 THEN null -- 'WBC'
     WHEN le.labname = 'WBC x 1000' and le.labresult <  0 THEN null
     WHEN le.labname = '-polys' and le.labresult <     0 THEN null -- immature polys forms, %
     WHEN le.labname = '-polys' and le.labresult >   100 THEN null -- immature polys forms, %
     WHEN le.labname = '-lymphs' and le.labresult <     0 THEN null -- immature lymphs forms, %
     WHEN le.labname = '-lymphs' and le.labresult >   100 THEN null -- immature lymphs forms, %
     WHEN le.labname = 'Hgb' and le.labresult >    50 THEN null -- g/dL 'HEMOGLOBIN'
     WHEN le.labname = 'Hgb' and le.labresult <    0 THEN null
     WHEN le.labname = 'Hct' and le.labresult >   100 THEN null -- % 'HEMATOCRIT'
     WHEN le.labname = 'Hct' and le.labresult <   0 THEN null
     WHEN le.labname = 'platelets x 1000' and le.labresult > 10000 THEN null -- K/uL 'PLATELET'
     WHEN le.labname = 'platelets x 1000' and le.labresult < 0 THEN null
     when le.labname = 'lactate' and le.labresult < 0.1 then null
     when le.labname = 'lactate' and le.labresult > 30 then null
     when le.labname = 'ALT (SGPT)' and le.labresult < 0 then null
     when le.labname = 'ALT (SGPT)' and le.labresult > 2000 then null
     when le.labname = 'AST (SGOT)' and le.labresult < 0 then null
     when le.labname = 'AST (SGOT)' and le.labresult > 2000 then null
     when le.labname = 'total bilirubin' and le.labresult < 0.2 then null
     when le.labname = 'total bilirubin' and le.labresult > 100 then null
     when le.labname = 'BUN' and le.labresult < 1 then null
     when le.labname = 'BUN' and le.labresult > 300 then null
     when le.labname = 'creatinine' and le.labresult < 1 then null
     when le.labname = 'creatinine' and le.labresult > 100 then null
   ELSE le.labresult
   END AS labresult
  FROM `physionet-data.eicu_crd.patient` p
  LEFT JOIN `physionet-data.eicu_crd.lab` le
    ON p.patientunitstayid = le.patientunitstayid
    AND le.labresultoffset between 0 and 24*60
    AND le.labname in
    (
      'WBC x 1000',
      '-polys',
      '-lymphs',
      'Hgb',
      'Hct',
      'platelets x 1000',
      'lactate',
      'ALT (SGPT)',
      'AST (SGPT)',
      'total bilirubin',
      'BUN',
      'creatinine'
    ))  pvt
    group by pvt.uniquepid, pvt.patienthealthsystemstayid, pvt.patientunitstayid
),
lab1 AS
(
SELECT
  l.uniquepid, l.patienthealthsystemstayid, l.patientunitstayid
  , max(l.WBC) as WBC_max
  , min(l.HGB) as HGB_min
  , min(l.HCT) as HCT_min
  , min(l.PLT) as PLT_min
  , max(l.LAC) as LAC_max
  , max(l.ALT) as ALT_max
  , max(l.AST) as AST_max
  , max(l.BIL) as BIL_max
  , max(l.BUN) as BUN_max
  , max(l.SCR) as SCR_max
FROM lab0 l
GROUP BY l.uniquepid, l.patienthealthsystemstayid, l.patientunitstayid
),
nlr0 as
(
SELECT
  nlr.patientunitstayid,
  nlr.nlr,
  row_number() OVER (PARTITION BY nlr.patientunitstayid order by nlr.nlr DESC) as max_nlr
FROM
(  SELECT patientunitstayid, POLYS, LYMPHS, (POLYS/LYMPHS) AS nlr
   FROM lab0 
   where POLYS > 0 AND LYMPHS > 0 
) nlr
),
nlr1 as
(select patientunitstayid, max(case when max_nlr = 1 then nlr else null end ) as NLR_max
from nlr0 
group by patientunitstayid
),
uo as
(
    select patientunitstayid, sum(urineoutput) as uo_24h
    FROM `physionet-data.eicu_crd_derived.pivoted_uo`
    where chartoffset between 0 and 24*60
    group by patientunitstayid
),
oi as
(   
    select oi0.patientunitstayid, max(case when oi0.oimin_num = 1 then oi0.oi else null end) as OI_min
    from
    (select bg1.patientunitstayid, bg1.oi,
    row_number () over (PARTITION by bg1.patientunitstayid order by bg1.oi) as oimin_num
    from 
    (
    select patientunitstayid, coalesce(pao2,100)/coalesce(coalesce(nullif(fio2,0),21),fio2,21) as oi
    FROM `physionet-data.eicu_crd_derived.pivoted_bg` 
    where chartoffset between 0 and 24*60
    and pao2 is not null
    and fio2 is not null) bg1
    ) oi0
    group by oi0.patientunitstayid
),
hemo as
(
    select patientunitstayid, min(co) as CO_min, min(ci) as CI_min, min(svo2) as SVO2_min, max(svr) as SVR_max, max(svri) as SVRI_max
    FROM `physionet-data.eicu_crd_derived.pivoted_vital_other` 
    where chartoffset between 0 and 24*60*7
    group by patientunitstayid
),
map0 AS
(
 SELECT
        pvt.patientunitstayid, pvt.chartoffset AS chartoffset, pvt.ibp_mean AS MAP
 FROM `physionet-data.eicu_crd_derived.pivoted_vital` pvt
 WHERE pvt.ibp_mean IS NOT NULL
 UNION ALL
 SELECT
        pvt.patientunitstayid, pvt.chartoffset AS chartoffset, pvt.nibp_mean AS MAP
 FROM `physionet-data.eicu_crd_derived.pivoted_vital` pvt
 WHERE pvt.nibp_mean IS NOT NULL
 and pvt.chartoffset BETWEEN 0 AND 1440
),
vitals AS
( SELECT
  pvt.patientunitstayid
  , max(pvt.heartrate) as HR_max, avg(pvt.heartrate) as HR_avg
  , max(pvt.respiratoryrate) as RR_max, avg(pvt.respiratoryrate) as RR_avg
  , min(pvt.spo2) as SpO2_min, avg(pvt.spo2) as SpO2_avg
  , MIN(m.MAP) AS MAP_min, AVG(m.MAP) AS MAP_avg
FROM `physionet-data.eicu_crd_derived.pivoted_vital` pvt
left join map0 m
on pvt.patientunitstayid = m.patientunitstayid
where pvt.chartoffset >= 0 AND pvt.chartoffset <= 24*60
GROUP BY pvt.patientunitstayid
),
temp0 AS 
(
SELECT 
    pvt.patientunitstayid, pvt.chartoffset AS chartoffset, pvt.temperature AS temp, tempsite.temperaturelocation, tempsite.tempsite_flag,
    (case when tempsite.tempsite_flag in ('Vascular','Esophageal','Core','Bladder') then 1 else 0 end) as coretemp_flag
FROM `physionet-data.eicu_crd_derived.pivoted_vital` pvt
inner join `my-project-sepsisprediction.infection.tempsite` tempsite
on pvt.temperaturelocation = tempsite.temperaturelocation
WHERE pvt.temperature IS NOT NULL
and pvt.chartoffset BETWEEN 0 AND 1440
--UNION ALL
--SELECT
        --vt.patientunitstayid, vt.observationoffset AS chartoffset, 
        --(CASE WHEN vt.temperature > 25 AND vt.temperature < 46 THEN vt.temperature ELSE NULL END) AS temp, NULL AS temp_site
--FROM  `physionet-data.eicu_crd.vitalperiodic` vt
--WHERE vt.temperature IS NOT NULL   
),
temp1 as
(
    select temp0.patientunitstayid, temp0.temp, temp0.tempsite_flag,
        ROW_NUMBER() OVER (PARTITION BY temp0.patientunitstayid ORDER BY temp0.temp DESC) as seqnum_max,
        ROW_NUMBER() OVER (PARTITION BY temp0.patientunitstayid ORDER BY temp0.temp) as seqnum_min        
    FROM temp0
),
temp_core AS
(
SELECT 
        temp0.patientunitstayid, temp0.temp, temp0.tempsite_flag,
        ROW_NUMBER() OVER (PARTITION BY temp0.patientunitstayid ORDER BY temp0.temp DESC) as seqnum_max,
        ROW_NUMBER() OVER (PARTITION BY temp0.patientunitstayid ORDER BY temp0.temp) as seqnum_min        
FROM temp0
WHERE temp0.coretemp_flag = 1
),
temp_periphreal AS
(
SELECT 
        temp0.patientunitstayid, temp0.temp, temp0.tempsite_flag,
        ROW_NUMBER() OVER (PARTITION BY temp0.patientunitstayid ORDER BY temp0.temp DESC) as seqnum_max,
        ROW_NUMBER() OVER (PARTITION BY temp0.patientunitstayid ORDER BY temp0.temp) as seqnum_min        
FROM temp0 
WHERE temp0.coretemp_flag = 0
),
aceta1 as
(
SELECT patientunitstayid, drugstartoffset
FROM `physionet-data.eicu_crd.medication` 
where drugname in (
  '20.3 ML CUP : ACETAMINOPHEN 160 MG/5ML PO SOLN',
'ACETAMINOPHEN',
'ACETAMINOPHEN  325 MG ORAL TAB',
'ACETAMINOPHEN  500 MG ORAL TAB',
'ACETAMINOPHEN  650 MG RECT SUPP',
'ACETAMINOPHEN 1,000 MG/100 ML (10 MG/ML) IV SOLN',
'ACETAMINOPHEN 10 MG/ML 100 ML VIAL',
'ACETAMINOPHEN 325 MG PO TABS',
'ACETAMINOPHEN 325 MG RECTAL SUPPOSITORY',
'ACETAMINOPHEN 325 MG TABLET',
'ACETAMINOPHEN 500 MG PO TABS',
'ACETAMINOPHEN 650 MG RE SUPP',
'ACETAMINOPHEN 650 MG RECTAL SUPPOSITORY',
'ACETAMINOPHEN TAB',
'Acetaminophen TAB',
'HYDROCODONE 5 MG-ACETAMINOPHEN 325 MG TABLET',
'HYDROCODONE-ACETAMINOPHEN 10-325 MG PO TABS',
'HYDROCODONE-ACETAMINOPHEN 5-325 MG PO TABS',
'HYDROcodone-acetaminophen',
'OXYCODONE-ACETAMINOPHEN 5-325 MG PO TABS',
'acetaMINOPHEN 325 MG TAB',
'acetaMINOPHEN 650MG RECT SUPP',
'acetaminophen',
'acetaminophen (TYLENOL) tab 650 mg',
'acetaminophen (TYLENOL) tab 975 mg',
'acetaminophen (TYLENOL) tablet 650 mg',
'acetaminophen 325 mg po tabs',
'acetaminophen-oxyCODONE 325 mg-5 mg oral tablet',
'oxyCODONE-acetaminophen',
'oxycodone-acetaminophen 5-325 mg po tabs'
)
and drugstartoffset between -6*60 and 30*60
),
aceta2 as
(
    select patientunitstayid, treatmentoffset
    from  `physionet-data.eicu_crd.treatment`
    where treatmentstring like 'surgery|wounds / temperature|antipyretics|acetaminophen'
    and treatmentoffset between -6*60 and 30*60
),
aceta as 
(
    select pt.patientunitstayid, 
    max(case when aceta1.patientunitstayid is not null or aceta2.patientunitstayid is not null then 1 else 0 end) as acetaminophen_flag
    FROM `physionet-data.eicu_crd.patient` pt
    left join aceta1
    on pt.patientunitstayid = aceta1.patientunitstayid
    left join aceta2
    on pt.patientunitstayid = aceta2.patientunitstayid
    group by pt.patientunitstayid
    order by pt.patientunitstayid
),
nsaid1 as
(
    SELECT patientunitstayid, drugstartoffset 
    FROM `physionet-data.eicu_crd.medication` 
    where drugname in (
     'IBUPROFEN',
    'KETOROLAC',
    'KETOROLAC 30 MG/ML 1ML SDV INJ',
    'KETOROLAC INJ',
    'KETOROLAC TROMETHAMINE'
    )
    and drugstartoffset between -6*60 and 30*60
),
nsaid2 as
(
    select patientunitstayid, treatmentoffset
    from  `physionet-data.eicu_crd.treatment`
    where treatmentstring like 'surgery|wounds / temperature|antipyretics|NSAID'
    and treatmentoffset between -6*60 and 30*60
),
nsaid as 
(
    select pt.patientunitstayid, 
    max(case when nsaid1.patientunitstayid is not null or nsaid2.patientunitstayid is not null then 1 else 0 end) as nsaid_flag
    FROM `physionet-data.eicu_crd.patient` pt
    left join nsaid1
    on pt.patientunitstayid = nsaid1.patientunitstayid
    left join nsaid2
    on pt.patientunitstayid = nsaid2.patientunitstayid
    group by pt.patientunitstayid
    order by pt.patientunitstayid
),
cool1 as
(
    select patientunitstayid,nursecareoffset
    FROM `physionet-data.eicu_crd.nursecare` 
    where cellattributevalue in ('heating/cooling blanket','ice bag') 
    and nursecareoffset between -6*60 and 30*60
),
cool2 as
(
    select patientunitstayid, treatmentoffset
    from  `physionet-data.eicu_crd.treatment`
    where treatmentstring in ('endocrine|pituitary and temperature regulation|treatment for hyperthermia|external cooling',
    'surgery|wounds / temperature|cooling the patient',
    'surgery|wounds / temperature|cooling the patient|external cooling',
    'endocrine|pituitary and temperature regulation|treatment for hyperthermia|evaporative cooling')
    and treatmentoffset between -6*60 and 30*60
),
cool as 
(
    select pt.patientunitstayid, 
    max(case when cool1.patientunitstayid is not null or cool2.patientunitstayid is not null then 1 else 0 end) as cooling_flag
    FROM `physionet-data.eicu_crd.patient` pt
    left join cool1
    on pt.patientunitstayid = cool1.patientunitstayid
    left join cool2
    on pt.patientunitstayid = cool2.patientunitstayid
    group by pt.patientunitstayid
    order by pt.patientunitstayid
),
final AS
(
SELECT 
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
       pt.ethnicity, pt.admissionheight, pt.admissionweight, 
       ROUND(pt.unitdischargeoffset/60) AS icu_los_hours,
       ROUND((pt.hospitaldischargeoffset - pt.hospitaladmitoffset)/60) AS hosp_los_hours,
       -- sofa
       cohort.sofa, 
       CASE WHEN cohort.sofa is not null THEN 1 ELSE 0 END AS sepsis_flag,
       -- Comorbidities    
       c.hypertension_flag, c.diabetes_flag, c.CKD_flag, c.COPD_flag, c.heartfailure_flag, c.cancer_flag, c.septicshock_flag,   
       --interventions
       i.ventilation_flag, i.tracheostomy_flag, i.dialysis_flag, i.vasopressor_flag, i.ene_less_flag, 
       i.ene_over_flag, i.dop_less5_flag, i.dop_5to15_flag, i.dop_over15_flag, i.antibiotics_flag,
       --lab
       l1.WBC_max, l1.HGB_min, l1.HCT_min, l1.PLT_min, l1.LAC_max, l1.ALT_max, l1.AST_max, l1.BIL_max, l1.BUN_max, l1.SCR_max,
       --nlr
       nlr1.NLR_max,
       --uo
       uo.uo_24h,
       -- oxygenation index
       oi.OI_min,
       -- hemodynamics
       he.CO_min, he.CI_min, he.SVO2_min, he.SVR_max, he.SVRI_max,
       --vitalsigns day1
       v.HR_max, v.HR_avg, v.RR_max, v.RR_avg, v.SpO2_min, v.SpO2_avg, v.MAP_min, v.MAP_avg,
       --temp_all
        tempmax.temp AS T_max, tempmin.temp as T_min,
       --temp_peri
       t1.temp AS T_max_peri, t1.tempsite_flag as tmax_peri, t2.temp as T_min_peri, t2.tempsite_flag as tmin_peri,
       case when  t1.temp is not null then 1 else 0 end as t_peri_flag,
       --temp_core
       t3.temp as T_max_core, t3.tempsite_flag as tmax_core, t4.temp as T_min_core, t4.tempsite_flag as tmin_core, 
       case when  t3.temp is not null then 1 else 0 end as t_core_flag,
       --case when t3.temp is not null and t1.temp is not null then t3.temp - t1.temp else null end as deltaT,
       aceta.acetaminophen_flag, nsaid.nsaid_flag, cool.cooling_flag,
       ROW_NUMBER() OVER (PARTITION BY pt.uniquepid ORDER BY pt.patienthealthsystemstayid, pt.patientunitstayid, pt.unitvisitnumber) as seqnum
FROM `physionet-data.eicu_crd.patient` pt
INNER JOIN comorbidities c
    ON pt.patientunitstayid = c.patientunitstayid
    AND c.exclusion_flag = 0
left join cohort
    on pt.patientunitstayid = cohort.patientunitstayid
INNER JOIN temp1 tempmax
    on pt.patientunitstayid = tempmax.patientunitstayid
    and tempmax.seqnum_max = 1
left join temp1 tempmin
    on pt.patientunitstayid = tempmin.patientunitstayid
    and tempmin.seqnum_min = 1
left join temp_periphreal t1
    ON pt.patientunitstayid = t1.patientunitstayid
    and t1.seqnum_max = 1
left join temp_periphreal t2
    on pt.patientunitstayid = t2.patientunitstayid
    and t2.seqnum_min = 1
left join temp_core t3
    on pt.patientunitstayid = t3.patientunitstayid
    and t3.seqnum_max = 1
left join temp_core t4
    on pt.patientunitstayid = t4.patientunitstayid
    and t4.seqnum_min = 1
left JOIN `physionet-data.eicu_crd.apachepatientresult` ap
    ON pt.patientunitstayid = ap.patientunitstayid
left JOIN `physionet-data.eicu_crd.hospital` h
    ON pt.hospitalid = h.hospitalid
left JOIN `physionet-data.eicu_crd_derived.apache_groups` ag
    ON pt.patientunitstayid = ag.patientunitstayid
LEFT JOIN interventions i
    ON pt.patientunitstayid = i.patientunitstayid
LEFT JOIN lab1 l1
    ON pt.patientunitstayid = l1.patientunitstayid
left join nlr1
    on pt.patientunitstayid = nlr1.patientunitstayid
left join uo
    on pt.patientunitstayid = uo.patientunitstayid
left join oi
    on pt.patientunitstayid = oi.patientunitstayid
left join hemo he
    on pt.patientunitstayid = he.patientunitstayid
left join vitals v
    on pt.patientunitstayid = v.patientunitstayid
left join aceta
on pt.patientunitstayid = aceta.patientunitstayid
left join nsaid
on pt.patientunitstayid = nsaid.patientunitstayid
left join cool
on pt.patientunitstayid = cool.patientunitstayid
)
select * from final
where final.seqnum = 1
and final.icu_los_hours >= 24
and final.sepsis_flag = 1 -- sepsis
--and final.sepsis_flag = 0 non-sepsis
