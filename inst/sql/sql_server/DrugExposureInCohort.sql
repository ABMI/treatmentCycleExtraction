SELECT distinct subject_id, drug.drug_concept_id, CAST(DRUG_EXPOSURE_START_DATETIME as date) as drug_exposure_start_date, CAST(drug_exposure_end_date as date) as drug_exposure_end_date
,drug.drug_exposure_id
FROM @result_database_schema.@cohort_table AS cohort
JOIN 
(select * from @cdm_database_schema.DRUG_EXPOSURE where person_Id in 
(select subject_id from @result_database_schema.@cohort_table where cohort_definition_id in (@target_cohort_Id))) AS drug
ON cohort.subject_id = drug.person_id
and cohort_definition_id IN (@target_cohort_Id)
{@out_of_cohort_period}?{}:
{
AND drug.drug_exposure_start_date >=cohort.cohort_start_date
AND drug.drug_exposure_end_date <= cohort.cohort_end_date
}
AND drug_concept_id IN (select concept_id
						from @cdm_database_schema.CONCEPT
						where concept_id in
											(@target_concept_Ids)and invalid_reason is null

{@include_descendant}? {
						UNION  select c.concept_id
						from @cdm_database_schema.CONCEPT c
						join @cdm_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
						and ca.ancestor_concept_id in (@target_concept_Ids)
						and c.invalid_reason is null
						}:{}
)

