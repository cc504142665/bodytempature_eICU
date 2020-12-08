--体温对脓毒症 非脓毒症患者预后的影响
--分N部分，要分部粘贴运行蛤，已经有的视图可以略过

--第一部分 实验室检查
DROP MATERIALIZED VIEW IF EXISTS labs_icu CASCADE;
CREATE materialized VIEW labs_icu AS

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
GROUP BY pvt.uniquepid, pvt.patienthealthsystemstayid, pvt.patientunitstayid
ORDER BY pvt.uniquepid, pvt.patienthealthsystemstayid, pvt.patientunitstayid;

--第二部分 interventions
DROP MATERIALIZED VIEW IF EXISTS interventions CASCADE;
CREATE MATERIALIZED VIEW interventions AS 
    
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
            -- OR t.treatmentstring LIKE 'renal|medications|systemic antibiotics%' 
            -- OR t.treatmentstring LIKE 'gastrointestinal|medications|antibiotics%'
            THEN 1 ELSE 0 END) AS antibiotics_flag,
       MAX(CASE WHEN t.treatmentstring LIKE 'endocrine|pituitary and temperature regulation|treatment for hyperthermia%'
            OR t.treatmentstring LIKE 'surgery|wounds / temperature|cooling the patient%'
            --‘endocrine|pituitary and temperature regulation|treatment for hyperthermia|external cooling’
            --‘surgery|wounds / temperature|cooling the patient’
            --‘surgery|wounds / temperature|cooling the patient|external cooling’
            --‘endocrine|pituitary and temperature regulation|treatment for hyperthermia|evaporative cooling’
            THEN 1 ELSE 0 END) AS cooling_flag,
       Max(CASE WHEN t.treatmentstring LIKE 'surgery|wounds / temperature|active warming%'
            OR t.treatmentstring LIKE 'endocrine|pituitary and temperature regulation|active core rewarming%'
            OR t.treatmentstring LIKE 'endocrine|pituitary and temperature regulation|active external rewarming%'
            OR t.trentmentstring = 'endocrine|pituitary and temperature regulation|passive external rewarming'
            THEN 1 ELSE 0 END) AS warming_flag,
       MAX(CASE WHEN t.treatmentstring LIKE 'surgery|wounds / temperature|antipyretics%'
            OR t.treatmentstring IN ('endocrine|pituitary and temperature regulation|treatment for hyperthermia|analgesic and antipyretic',
            'endocrine|pituitary and temperature regulation|treatment for hyperthermia|benzodiazepine', 
            'endocrine|pituitary and temperature regulation|treatment for hyperthermia|dantrolene')
            THEN 1 ELSE 0 END) AS antipyretics_flag

            --1	surgery|wounds / temperature|antipyretics|acetaminophen
--2 surgery|wounds / temperature|antipyretics|NSAID
--3surgery|wounds / temperature|antipyretics
--4endocrine|pituitary and temperature regulation|treatment for hyperthermia|analgesic and antipyretic

    FROM patient pt
    LEFT JOIN treatment t
    ON pt.patientunitstayid = t.patientunitstayid
    GROUP BY pt.patientunitstayid
    ORDER BY pt.patientunitstayid;
        
--第三部分 Comorbidities include sepsis septic shock in diagnosis table   有新修改！
DROP MATERIALIZED VIEW IF EXISTS comorbidities CASCADE;
CREATE MATERIALIZED VIEW comorbidities AS 
    
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
            --OR d.diagnosisstring LIKE 'cardiovascular|shock / hypotension|sepsis%'
            OR d.diagnosisstring LIKE 'cardiovascular|shock / hypotension|septic shock%'
            --OR d.diagnosisstring LIKE 'cardiovascular|shock / hypotension|signs and symptoms of sepsis (SIRS)%'
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
    GROUP BY pt.patientunitstayid
    ORDER BY pt.patientunitstayid;

--第四部分 pivoted_vital
-- This script duplicates the nurse charting table, making the following changes:
--  "major" vital signs -> pivoted_vital
--  "minor" vital signs -> pivoted_vital_other
DROP TABLE IF EXISTS pivoted_vital CASCADE;
CREATE TABLE pivoted_vital as
-- create columns with only numeric data
with nc as
(
select
    patientunitstayid
  , nursingchartoffset
  , nursingchartentryoffset
  , case
      when nursingchartcelltypevallabel = 'Heart Rate'
       and nursingchartcelltypevalname = 'Heart Rate'
       and nursingchartvalue ~ '^[-]?[0-9]+[.]?[0-9]*$'
       and nursingchartvalue not in ('-','.')
          then cast(nursingchartvalue as numeric)
      else null end
    as heartrate
  , case
      when nursingchartcelltypevallabel = 'Respiratory Rate'
       and nursingchartcelltypevalname = 'Respiratory Rate'
       and nursingchartvalue ~ '^[-]?[0-9]+[.]?[0-9]*$'
       and nursingchartvalue not in ('-','.')
          then cast(nursingchartvalue as numeric)
      else null end
    as RespiratoryRate
  , case
      when nursingchartcelltypevallabel = 'O2 Saturation'
       and nursingchartcelltypevalname = 'O2 Saturation'
       and nursingchartvalue ~ '^[-]?[0-9]+[.]?[0-9]*$'
       and nursingchartvalue not in ('-','.')
          then cast(nursingchartvalue as numeric)
      else null end
    as o2saturation
  , case
      when nursingchartcelltypevallabel = 'Non-Invasive BP'
       and nursingchartcelltypevalname = 'Non-Invasive BP Systolic'
       and nursingchartvalue ~ '^[-]?[0-9]+[.]?[0-9]*$'
       and nursingchartvalue not in ('-','.')
          then cast(nursingchartvalue as numeric)
      else null end
    as nibp_systolic
  , case
      when nursingchartcelltypevallabel = 'Non-Invasive BP'
       and nursingchartcelltypevalname = 'Non-Invasive BP Diastolic'
       and nursingchartvalue ~ '^[-]?[0-9]+[.]?[0-9]*$'
       and nursingchartvalue not in ('-','.')
          then cast(nursingchartvalue as numeric)
      else null end
    as nibp_diastolic
  , case
      when nursingchartcelltypevallabel = 'Non-Invasive BP'
       and nursingchartcelltypevalname = 'Non-Invasive BP Mean'
       and nursingchartvalue ~ '^[-]?[0-9]+[.]?[0-9]*$'
       and nursingchartvalue not in ('-','.')
          then cast(nursingchartvalue as numeric)
      else null end
    as nibp_mean
  , case
      when nursingchartcelltypevallabel = 'Temperature'
       and nursingchartcelltypevalname = 'Temperature (C)'
       and nursingchartvalue ~ '^[-]?[0-9]+[.]?[0-9]*$'
       and nursingchartvalue not in ('-','.')
          then cast(nursingchartvalue as numeric)
      else null end
    as temperature
  , case
      when nursingchartcelltypevallabel = 'Temperature'
       and nursingchartcelltypevalname = 'Temperature Location'
          then nursingchartvalue
      else null end
    as TemperatureLocation
  , case
      when nursingchartcelltypevallabel = 'Invasive BP'
       and nursingchartcelltypevalname = 'Invasive BP Systolic'
       and nursingchartvalue ~ '^[-]?[0-9]+[.]?[0-9]*$'
       and nursingchartvalue not in ('-','.')
          then cast(nursingchartvalue as numeric)
      else null end
    as ibp_systolic
  , case
      when nursingchartcelltypevallabel = 'Invasive BP'
       and nursingchartcelltypevalname = 'Invasive BP Diastolic'
       and nursingchartvalue ~ '^[-]?[0-9]+[.]?[0-9]*$'
       and nursingchartvalue not in ('-','.')
          then cast(nursingchartvalue as numeric)
      else null end
    as ibp_diastolic
  , case
      when nursingchartcelltypevallabel = 'Invasive BP'
       and nursingchartcelltypevalname = 'Invasive BP Mean'
       and nursingchartvalue ~ '^[-]?[0-9]+[.]?[0-9]*$'
       and nursingchartvalue not in ('-','.')
          then cast(nursingchartvalue as numeric)
      -- other map fields
      when nursingchartcelltypevallabel = 'MAP (mmHg)'
       and nursingchartcelltypevalname = 'Value'
       and nursingchartvalue ~ '^[-]?[0-9]+[.]?[0-9]*$'
       and nursingchartvalue not in ('-','.')
          then cast(nursingchartvalue as numeric)
      when nursingchartcelltypevallabel = 'Arterial Line MAP (mmHg)'
       and nursingchartcelltypevalname = 'Value'
       and nursingchartvalue ~ '^[-]?[0-9]+[.]?[0-9]*$'
       and nursingchartvalue not in ('-','.')
          then cast(nursingchartvalue as numeric)
      else null end
    as ibp_mean
  from nursecharting
  -- speed up by only looking at a subset of charted data
  where nursingchartcelltypecat in
  (
    'Vital Signs','Scores','Other Vital Signs and Infusions'
  )
)
select
  patientunitstayid
, nursingchartoffset as chartoffset
, nursingchartentryoffset as entryoffset
, avg(case when heartrate >= 25 and heartrate <= 225 then heartrate else null end) as heartrate
, avg(case when RespiratoryRate >= 0 and RespiratoryRate <= 60 then RespiratoryRate else null end) as RespiratoryRate
, avg(case when o2saturation >= 0 and o2saturation <= 100 then o2saturation else null end) as spo2
, avg(case when nibp_systolic >= 25 and nibp_systolic <= 250 then nibp_systolic else null end) as nibp_systolic
, avg(case when nibp_diastolic >= 1 and nibp_diastolic <= 200 then nibp_diastolic else null end) as nibp_diastolic
, avg(case when nibp_mean >= 1 and nibp_mean <= 250 then nibp_mean else null end) as nibp_mean
, avg(case when temperature >= 25 and temperature <= 46 then temperature else null end) as temperature
, max(temperaturelocation) as temperaturelocation
, avg(case when ibp_systolic >= 1 and ibp_systolic <= 300 then ibp_systolic else null end) as ibp_systolic
, avg(case when ibp_diastolic >= 1 and ibp_diastolic <= 200 then ibp_diastolic else null end) as ibp_diastolic
, avg(case when ibp_mean >= 1 and ibp_mean <= 250 then ibp_mean else null end) as ibp_mean
from nc
WHERE heartrate IS NOT NULL
OR RespiratoryRate IS NOT NULL
OR o2saturation IS NOT NULL
OR nibp_systolic IS NOT NULL
OR nibp_diastolic IS NOT NULL
OR nibp_mean IS NOT NULL
OR temperature IS NOT NULL
OR temperaturelocation IS NOT NULL
OR ibp_systolic IS NOT NULL
OR ibp_diastolic IS NOT NULL
OR ibp_mean IS NOT NULL
group by patientunitstayid, nursingchartoffset, nursingchartentryoffset
order by patientunitstayid, nursingchartoffset, nursingchartentryoffset;

--第五部分 MAP
DROP MATERIALIZED VIEW IF EXISTS map CASCADE;
CREATE MATERIALIZED VIEW map AS
 SELECT
        pvt.patientunitstayid, pvt.chartoffset AS chartoffset, pvt.ibp_mean AS MAP
 FROM pivoted_vital pvt
 WHERE pvt.ibp_mean IS NOT NULL
 UNION 
 SELECT
        pvt.patientunitstayid, pvt.chartoffset AS chartoffset, pvt.nibp_mean AS MAP
 FROM pivoted_vital pvt
 WHERE pvt.nibp_mean IS NOT NULL
 UNION
 SELECT 
        vt.patientunitstayid, vt.observationoffset AS chartoffset, 
        (CASE WHEN vt.systemicmean  > 0 AND vt.systemicmean <=250 THEN vt.systemicmean ELSE NULL END) AS MAP
 FROM vitalperiodic vt
 WHERE vt.systemicmean IS NOT NULL
 ORDER BY patientunitstayid;


--第六部分 mapfirstday
DROP MATERIALIZED VIEW IF EXISTS mapfirstday CASCADE;
CREATE MATERIALIZED VIEW mapfirstday AS
SELECT 
       m.patientunitstayid, MAX(m.MAP) AS MAP_max, MIN(m.MAP) AS MAP_min, AVG(m.MAP) AS MAP_avg
FROM map m 
WHERE m.chartoffset BETWEEN 0 AND 1440
GROUP BY m.patientunitstayid
ORDER BY m.patientunitstayid;


--第七部分 temperature combined
DROP MATERIALIZED VIEW IF EXISTS temp CASCADE;
CREATE MATERIALIZED VIEW temp as
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
ORDER BY patientunitstayid, chartoffset;



with temp AS 
(
SELECT 
        pvt.patientunitstayid, pvt.chartoffset AS chartoffset, pvt.temperature AS temp, pvt.temperaturelocation AS temp_site
FROM `physionet-data.eicu_crd_derived.pivoted_vital` pvt
WHERE pvt.temperature IS NOT NULL
UNION ALL
SELECT
        vt.patientunitstayid, vt.observationoffset AS chartoffset, 
        (CASE WHEN vt.temperature > 25 AND vt.temperature < 46 THEN vt.temperature ELSE NULL END) AS temp, NULL AS temp_site
FROM  `physionet-data.eicu_crd.vitalperiodic` vt
WHERE vt.temperature IS NOT NULL   
),
temp_max AS
(
SELECT 
        temp.patientunitstayid, temp.temp,
        ROW_NUMBER() OVER (PARTITION BY temp.patientunitstayid ORDER BY temp.temp DESC) as seqnum,
        temp.temp_site
FROM temp 
WHERE temp.chartoffset >= 0
)
SELECT temp_max.*
FROM temp_max
WHERE temp_max.seqnum = 1



--第八部分 Tmax
DROP MATERIALIZED VIEW IF EXISTS t_icustay CASCADE;
CREATE MATERIALIZED VIEW t_icustay as
SELECT 
        patientunitstayid, MAX(temp) AS temp_max, MIN(temp) AS temp_min, AVG(temp) AS temp_avg
FROM temp 
WHERE chartoffset >= 0
GROUP BY patientunitstayid
ORDER BY patientunitstayid;


--第九部分 Tfirstday
DROP MATERIALIZED VIEW IF EXISTS tfirstday CASCADE;
CREATE MATERIALIZED VIEW tfirstday as
SELECT 
        patientunitstayid, MAX(temp) AS T_max, MIN(temp) AS T_min, AVG(temp) AS T_avg
FROM temp 
WHERE chartoffset BETWEEN 0 AND 1440
GROUP BY patientunitstayid
ORDER BY patientunitstayid;


--第十部分 vtitalsfirstday
DROP MATERIALIZED VIEW IF EXISTS vitalsfirstday CASCADE;
CREATE MATERIALIZED VIEW vitalsfirstday AS
WITH v1 AS
(SELECT vt.patientunitstayid,
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
 ORDER BY vt.patientunitstayid
)
SELECT v1.patientunitstayid,
       v1.HR_max, v1.HR_min, v1.HR_avg,
       v1.RR_max, v1.RR_min, v1.RR_avg,
       v1.SpO2_max, v1.SpO2_min, v1.SpO2_avg, 
       t.T_max, t.T_min, t.T_avg,
       m.MAP_max, m.MAP_min, m.MAP_avg
FROM v1
LEFT JOIN mapfirstday m
ON v1.patientunitstayid = m.patientunitstayid
LEFT JOIN tfirstday t
ON v1.patientunitstayid = t.patientunitstayid
ORDER BY v1.patientunitstayid;

--第十一部分 pivoted_lab
DROP TABLE IF EXISTS pivoted_lab CASCADE;
CREATE TABLE pivoted_lab as
-- remove duplicate labs if they exist at the same time
with vw0 as
(
  select
      patientunitstayid
    , labname
    , labresultoffset
    , labresultrevisedoffset
  from lab
  where labname in
  (
      'albumin'
    , 'total bilirubin'
    , 'BUN'
    , 'calcium'
    , 'chloride'
    , 'creatinine'
    , 'bedside glucose', 'glucose'
    , 'bicarbonate' -- HCO3
    , 'Total CO2'
    , 'Hct'
    , 'Hgb'
    , 'PT - INR'
    , 'PTT'
    , 'lactate'
    , 'platelets x 1000'
    , 'potassium'
    , 'sodium'
    , 'WBC x 1000'
    , '-bands'
    , '-polys'
    , '-lymphs'
    -- Liver enzymes
    , 'ALT (SGPT)'
    , 'AST (SGOT)'
    , 'alkaline phos.'
  )
  group by patientunitstayid, labname, labresultoffset, labresultrevisedoffset
  having count(distinct labresult)<=1
)
-- get the last lab to be revised
, vw1 as
(
  select
      lab.patientunitstayid
    , lab.labname
    , lab.labresultoffset
    , lab.labresultrevisedoffset
    , lab.labresult
    , ROW_NUMBER() OVER
        (
          PARTITION BY lab.patientunitstayid, lab.labname, lab.labresultoffset
          ORDER BY lab.labresultrevisedoffset DESC
        ) as rn
  from lab
  inner join vw0
    ON  lab.patientunitstayid = vw0.patientunitstayid
    AND lab.labname = vw0.labname
    AND lab.labresultoffset = vw0.labresultoffset
    AND lab.labresultrevisedoffset = vw0.labresultrevisedoffset
  -- only valid lab values
  WHERE
       (lab.labname = 'albumin' and lab.labresult >= 0.5 and lab.labresult <= 6.5)
    OR (lab.labname = 'total bilirubin' and lab.labresult >= 0.2 and lab.labresult <= 70.175)
    OR (lab.labname = 'BUN' and lab.labresult >= 1 and lab.labresult <= 280)
    OR (lab.labname = 'calcium' and lab.labresult > 0 and lab.labresult <= 9999)
    OR (lab.labname = 'chloride' and lab.labresult > 0 and lab.labresult <= 9999)
    OR (lab.labname = 'creatinine' and lab.labresult >= 0.1 and lab.labresult <= 28.28)
    OR (lab.labname in ('bedside glucose', 'glucose') and lab.labresult >= 25 and lab.labresult <= 1500)
    OR (lab.labname = 'bicarbonate' and lab.labresult >= 0 and lab.labresult <= 9999)
    OR (lab.labname = 'Total CO2' and lab.labresult >= 0 and lab.labresult <= 9999)
    -- will convert hct unit to fraction later
    OR (lab.labname = 'Hct' and lab.labresult >= 5 and lab.labresult <= 75)
    OR (lab.labname = 'Hgb' and lab.labresult >  0 and lab.labresult <= 9999)
    OR (lab.labname = 'PT - INR' and lab.labresult >= 0.5 and lab.labresult <= 15)
    OR (lab.labname = 'lactate' and lab.labresult >= 0.1 and lab.labresult <= 30)
    OR (lab.labname = 'platelets x 1000' and lab.labresult >  0 and lab.labresult <= 9999)
    OR (lab.labname = 'potassium' and lab.labresult >= 0.05 and lab.labresult <= 12)
    OR (lab.labname = 'PTT' and lab.labresult >  0 and lab.labresult <= 500)
    OR (lab.labname = 'sodium' and lab.labresult >= 90 and lab.labresult <= 215)
    OR (lab.labname = 'WBC x 1000' and lab.labresult > 0 and lab.labresult <= 100)
    OR (lab.labname = '-bands' and lab.labresult >= 0 and lab.labresult <= 100)
    OR (lab.labname = '-polys' and lab.labresult >= 0 and lab.labresult <= 100)
    OR (lab.labname = '-lymphs' and lab.labresult >= 0 and lab.labresult <= 100)
    OR (lab.labname = 'ALT (SGPT)' and lab.labresult > 0)
    OR (lab.labname = 'AST (SGOT)' and lab.labresult > 0)
    OR (lab.labname = 'alkaline phos.' and lab.labresult > 0)
)
select
    patientunitstayid
  , labresultoffset as chartoffset
  , MAX(case when labname = 'albumin' then labresult else null end) as albumin
  , MAX(case when labname = 'total bilirubin' then labresult else null end) as bilirubin
  , MAX(case when labname = 'BUN' then labresult else null end) as BUN
  , MAX(case when labname = 'calcium' then labresult else null end) as calcium
  , MAX(case when labname = 'chloride' then labresult else null end) as chloride
  , MAX(case when labname = 'creatinine' then labresult else null end) as creatinine
  , MAX(case when labname in ('bedside glucose', 'glucose') then labresult else null end) as glucose
  , MAX(case when labname = 'bicarbonate' then labresult else null end) as bicarbonate
  , MAX(case when labname = 'Total CO2' then labresult else null end) as TotalCO2
  , MAX(case when labname = 'Hct' then labresult else null end) as hematocrit
  , MAX(case when labname = 'Hgb' then labresult else null end) as hemoglobin
  , MAX(case when labname = 'PT - INR' then labresult else null end) as INR
  , MAX(case when labname = 'lactate' then labresult else null end) as lactate
  , MAX(case when labname = 'platelets x 1000' then labresult else null end) as platelets
  , MAX(case when labname = 'potassium' then labresult else null end) as potassium
  , MAX(case when labname = 'PTT' then labresult else null end) as ptt
  , MAX(case when labname = 'sodium' then labresult else null end) as sodium
  , MAX(case when labname = 'WBC x 1000' then labresult else null end) as wbc
  , MAX(case when labname = '-bands' then labresult else null end) as bands
  , MAX(case when labname = '-polys' then labresult else null end) as polys
  , MAX(case when labname = '-lymphs' then labresult else null end) as lypmhs
  , MAX(case when labname = 'ALT (SGPT)' then labresult else null end) as alt
  , MAX(case when labname = 'AST (SGOT)' then labresult else null end) as ast
  , MAX(case when labname = 'alkaline phos.' then labresult else null end) as alp
from vw1
where rn = 1
group by patientunitstayid, labresultoffset
order by patientunitstayid, labresultoffset;

--第十二部分 NLRatio wholeicu
DROP MATERIALIZED VIEW IF EXISTS nlratio CASCADE;
CREATE materialized VIEW nlratio AS
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
ORDER BY nlr.patientunitstayid;

--第十三部分 cohort selection 有新修改!
DROP MATERIALIZED VIEW IF EXISTS temp_sepsis CASCADE;
CREATE MATERIALIZED VIEW temp_sepsis AS 
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
       v.T_max AS T_max_24hours, v.T_min AS T_min_24hours, v.T_avg AS T_avg_24hours,
       v.MAP_max AS MAP_max_24hours, v.MAP_min AS MAP_min_24hours, v.MAP_avg AS MAP_avg_24hours,
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


--最后复制出来
COPY temp_sepsis TO 'C:\tmp\随便啥路径……\temp_sepsis.csv' DELIMITER ',' CSV HEADER;
