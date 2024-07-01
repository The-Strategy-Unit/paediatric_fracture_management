/****** Script for SelectTopNRows command from SSMS  ******/

USE NHSE_Sandbox_StrategyUnit
GO

---Update fracture codes to ensure snomed codes that contain part of one of the codes of interest are not included (one of elbow codes is part of respiratory condition code)
Select *
,CONCAT(code,',') as code1
,CONCAT(SPACE(1), code) as code2
,CONCAT(SPACE(1), code, ',') as code3
Into [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[SL_fracture_codes_edited] 
From [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[SL_fracture_codes]

--- Emergency department activity 
SELECT
      a.[Sex]
      ,a.[Ethnic_Category]
      ,a.[EC_Department_Type]
      ,a.[EC_Seen_For_Treatment_Time_Since_Arrival]
      ,a.[Discharge_Follow_Up_SNOMED_CT]
      ,a.[SUS_HRG_Code]
      ,a.[Der_Pseudo_NHS_Number]
      ,a.[Der_Provider_Code]
      ,a.[Der_Provider_Site_Code]
      ,a.[Der_Commissioner_Code]
      ,a.[Der_Age_At_CDS_Activity_Date]
      ,a.[Der_Financial_Year]
	  ,a.[Der_Activity_Month]
	  ,a.[Der_Number_EC_Diagnosis]
      ,a.[Der_EC_Diagnosis_All]
	  ,a.[Der_AEA_Investigation_All]
	  ,a.[Der_AEA_Treatment_All]
	  ,FORMAT ([Der_EC_Arrival_Date_Time], 'yyyy-MM-dd') as [ArrivalDate]
	  ,CONVERT (time,Der_EC_Arrival_Date_Time, 114) as [ArrivalTime]
	  ,DATENAME(dw, Der_EC_Arrival_Date_Time) as day_of_week
      ,a.[Der_Postcode_LSOA_2011_Code]
	   ,(case when(a.Der_AEA_Treatment_All like '%10%' AND a.Der_AEA_Treatment_All like '%23%') then 1 else 0 end) as manipulation_in_ED_anaes
	  ,(case when(a.Der_AEA_Treatment_All like '%10%') then 1 else 0 end) as manipulation_in_ED
	   ,(case when a.Der_AEA_Investigation_All like '%01%' then 1 else 0 end) as xray
	  ,(case when a.Discharge_Destination_SNOMED_CT like '306706006' then 1 else 0 end) as admitted
	  ,(case when a.Discharge_Follow_Up_SNOMED_CT like '301791000000104' then 1 else 0 end) as fracture_clinic_f_up
	  ,(case when a.Discharge_Follow_Up_SNOMED_CT like '306170007' then 1 else 0 end) as physio_f_up
	  ,(case when (a.Discharge_Follow_Up_SNOMED_CT like '3780001' AND a.Discharge_Destination_SNOMED_CT not like '306706006') then 1 else 0 end) as no_f_up
	  ,dateadd(mm, 3, (FORMAT ([Der_EC_Arrival_Date_Time], 'yyyy-MM-dd'))) as followup_period
	  ,b.[type]
	  ,b.[description]

  INTO [NHSE_Sandbox_StrategyUnit.dbo.paedfractemp]
  FROM [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SUS_EC] a
   inner join [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[SL_fracture_codes_edited] b 
   on (a.Der_EC_Diagnosis_All LIKE CONCAT(b.code1, '%')) OR
       (a.Der_EC_Diagnosis_All LIKE CONCAT('%',b.code2)) OR
       (a.Der_EC_Diagnosis_All LIKE CONCAT('%',b.code3, '%')) OR
	   (a.Der_EC_Diagnosis_All LIKE (b.code))

  where a.[Der_Age_At_CDS_Activity_Date]<=16 AND
        a.[Der_EC_Arrival_Date_Time] between '2018-04-01' AND '2024-03-31' AND
		a.Der_EC_Diagnosis_All!= 'NULL' AND 
		a.Der_Pseudo_NHS_Number is not null

--- Unique Pseudo_NHS_Number and ArrivalDate
SELECT a.*
INTO [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp1] 	 
FROM [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp] a		

INNER JOIN (
    SELECT  Der_Pseudo_NHS_Number
	       ,ArrivalDate
	       ,MAX(ArrivalTime) as ArrivalTime 
    FROM [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp]
    GROUP BY Der_Pseudo_NHS_Number, ArrivalDate) b 
ON a.Der_Pseudo_NHS_Number = b.Der_Pseudo_NHS_Number AND 
          a.ArrivalDate= b.ArrivalDate AND
          a.ArrivalTime = b.ArrivalTime


--- Adding inpatient records for manipulation under anaesthetic
SELECT a.*
      ,b.[APCE_Ident]
      ,b.[Ethnic_Group] 
      ,b.[Admission_Date]
      ,b.[HRG_Code] as inpatient_HRG
      ,b.[Der_Primary_Diagnosis_Code]
      ,b.[Der_Primary_Procedure_Code]

INTO  [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp2] 
FROM  [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp] a
LEFT JOIN [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCE] b
		ON b.Der_Pseudo_NHS_Number = a.Der_Pseudo_NHS_Number AND 
	    b.Admission_Date between a.ArrivalDate AND a.followup_period AND  --- in 3 months post ED attendance
		(b.Der_Primary_Procedure_Code= 'W262' OR 
		 b.Der_Primary_Procedure_Code= 'W268' OR
		 b.Der_Primary_Procedure_Code= 'W269' OR
		 b.Der_Primary_Procedure_Code= 'W663')   --- Admission for closed reduction of fracture 

--- Mark records with associated inpatient admissions, and order of admissions where more than 1
select  a.*
         ,(case when Der_Primary_Procedure_Code IS NULL then 0 else 1 end) as inpatient_manipulation  --- flag cases with inpatient admission
        ,b.admission_number
		,ROW_NUMBER() OVER (PARTITION BY a.Der_Pseudo_NHS_number, a.ArrivalDate order by Admission_Date) as rownum
	into [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp3] 
	from [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp2] a
	       LEFT JOIN (SELECT 
		                Der_Pseudo_NHS_Number
						,ArrivalDate
		                ,count(DISTINCT APCE_Ident) as admission_number
                     FROM [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp2]
                    GROUP BY Der_Pseudo_NHS_Number, ArrivalDate) b
					ON (b.Der_Pseudo_NHS_Number = a.Der_Pseudo_NHS_Number AND
					     b.ArrivalDate = a.ArrivalDate )
	


--- Select records with more than one admission and select first admission
SELECT *
INTO [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp4] 	 
FROM [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp3] 
 where rownum=1 

--- Adding in Outpatient attendances 

SELECT a.* 
      ,b.[Main_Specialty_Code]
      ,b.[Treatment_Function_Code]
	  ,b.[Ethnic_Category] as Ethnic_Category2
      ,b.[Outcome_of_Attendance]
      ,b.[Appointment_Date]
      ,b.[OPA_Referral_Source]
      ,b.[HRG_Code] as Outpatient_HRG
      ,b.[Core_HRG] as Outpatient_Core_HRG
      ,b.[SUS_HRG] as Outpatient_SUS_HRG
      ,b.[Der_Procedure_All] as Outpatient_Procedure
      ,b.[Der_Staff_Type]
      ,b.[Der_Appointment_Type]
      ,b.[Der_Contact_Type]
      ,b.[Der_Professional_Type]
      ,b.[Der_Attendance_Type]
	  ,(case when b.Treatment_Function_Code like '650' then 1 else 0 end) as physio_appt  -- flag physio appts

INTO [NHSE_Sandbox_StrategyUnit.dbo.paedfractemp5]
FROM [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp4] a
left join (select * 
          from [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_OPA] 
 where (Main_Specialty_Code = '110' OR 
	   Main_Specialty_Code = '171' OR 
	   Treatment_Function_Code ='110' OR
	   Treatment_Function_Code ='111' OR
	   Treatment_Function_Code ='115' OR
	   Treatment_Function_Code ='214' OR 
	   Treatment_Function_Code ='650' OR
	    (Main_Specialty_Code = '420' AND  Treatment_Function_Code ='420' AND
		(OPA_Referral_Source='10' OR OPA_Referral_Source='04' )) )) b

on a.Der_Pseudo_NHS_Number=b.Der_Pseudo_NHS_Number AND
   (b.Appointment_Date between a.ArrivalDate AND a.followup_period) AND 
   a.no_f_up='0'


--- Adding in count of number of outpatient attendances	

	select  a.*
	        ,(case when ((Main_Specialty_Code = '110' OR 
						  Main_Specialty_Code = '171' OR 
						  Treatment_Function_Code ='110' OR
						  Treatment_Function_Code ='111' OR
						   Treatment_Function_Code ='115' OR
					       Treatment_Function_Code ='214' OR 
						   (Main_Specialty_Code = '420' AND  Treatment_Function_Code ='420' AND
						   (OPA_Referral_Source='10' OR OPA_Referral_Source='04' )) )) then 1 else 0 end) as outpat_attendance ---non-physio outpatient attendances
	        ,b.outpat_attendance_number
			,b.physio_appt_number
			,ROW_NUMBER() OVER (PARTITION BY a.Der_Pseudo_NHS_number, a.ArrivalDate order by Appointment_Date) as rownum2
	into [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp6] 
	from [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp5] a
	       LEFT JOIN (SELECT 
		                Der_Pseudo_NHS_Number
						,ArrivalDate
						,count(case Appointment_Date when NULL then 0 else 1 end)as outpat_attendance_number
						,count(case when physio_appt=1 then 1 end) as physio_appt_number
                     FROM [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp5]
                    GROUP BY Der_Pseudo_NHS_Number, ArrivalDate) b
					ON (b.Der_Pseudo_NHS_Number = a.Der_Pseudo_NHS_Number AND
					     b.ArrivalDate = a.ArrivalDate)

--- Select the first follow up attendance 
SELECT *
INTO [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfrac_final] 	 
FROM [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp6] 		

where rownum2=1


SELECT Der_Pseudo_NHS_Number, ArrivalDate, EC_Attendance_Number, ArrivalTime , COUNT(*) 
FROM [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfrac_final] 
GROUP BY Der_Pseudo_NHS_Number, ArrivalDate, EC_Attendance_Number, ArrivalTime
HAVING COUNT(*) > 1

SELECT *
FROM [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfrac_final] 


drop table [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp]
drop table [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp1]
drop table [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp2]
drop table [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp3]
drop table [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp4]
drop table [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp5]
drop table [NHSE_Sandbox_StrategyUnit].[GEM\SLucas].[NHSE_Sandbox_StrategyUnit.dbo.paedfractemp6]