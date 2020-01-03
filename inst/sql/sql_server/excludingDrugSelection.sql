select distinct concept_id_2 as excludingDrug from @voca_database_schema.CONCEPT_RELATIONSHIP
where concept_id_1 in (select concept_id_1 from (select concept_id_1, count(concept_id_1) as cnt from @voca_database_schema.CONCEPT_RELATIONSHIP
where concept_id_2 in (select concept_id_2 from @voca_database_schema.concept_relationship
where concept_id_1 = @regimen_concept_id
and relationship_id = 'Has antineopl Rx')and relationship_id = 'Has antineopl Rx' group by concept_id_1) as a where cnt = @regimen_drug_no )
and relationship_id = 'Has antineopl Rx' and concept_id_2 not in (select concept_id_2 from @voca_database_schema.concept_relationship where 
																 concept_id_1 = @regimen_concept_id and relationship_id = 'Has antineopl Rx')