/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) [id]
      ,[wage]
  FROM [DataCleaningPractice ].[dbo].[new_employees]

select GREATEST (17, wage) as wage
from new_employees

