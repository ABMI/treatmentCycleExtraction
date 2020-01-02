select concept_id_2 as secondary_drug_list from @voca_database_schema.CONCEPT_RELATIONSHIP
where concept_id_1 in (@regimen_concept_id)
and relationship_id ='Has antineopl Rx' and concept_id_2 not in (@Index_drug_concept_id)
