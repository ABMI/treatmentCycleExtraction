SELECT distinct subject_id, drug_exposure_start_date, drug_exposure_end_date
FROM @result_database_schema.@cohort_table AS cohort
JOIN @cdm_database_schema.DRUG_EXPOSURE AS drug
ON cohort.subject_id = drug.person_id
and cohort_definition_id IN (@target_cohort_id)
{@out_of_cohort_period}?{}:
{
AND drug.drug_exposure_start_date >=cohort.cohort_start_date
AND drug.drug_exposure_end_date <= cohort.cohort_end_date
}
AND drug_concept_id IN (select concept_id
						from @cdm_database_schema.CONCEPT
						where concept_id in
											(@target_concept_ids)

{@include_descendant}? {
						UNION  select c.concept_id
						from @cdm_database_schema.CONCEPT c
						join @cdm_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
						and ca.ancestor_concept_id in (@target_concept_ids)
						
						}:{}
)