test_that("testing weights", {



  courses <- data.frame( code = c( "CHEM101","CHEM102","CLSE101",
                                   "EGMN103","EGMN190","EGRB102","EGRB104","EGRE101",
                                   "ENGL201",
                                   "MATH201","MATH302",
                                   "PHYS101"),
                         credits = c(3,3,3,1,1,3,1,4,3,4,3,3)  )

  expect_equal( course_weight( "CHEM101", courses ),
                data.frame( Course = "CHEM101",
                            Weight = 1 ) )
  # CHEM101 1.0
  expect_equal( course_weight("CHEM101&CHEM102", courses ),
                data.frame( Course = c("CHEM101",
                                       "CHEM102"),
                            Weight = 1.0 ) )
  # CHEM101 1.0; CHEM102 1.0

  # OR of three
  expect_equal( course_weight("CHEM101|PHYS101|MATH302", courses ),
                data.frame( Course = c("CHEM101",
                                       "PHYS101",
                                       "MATH302"),
                            Weight = 1.0/3.0 ) )
  # each 1/3

  # AND precedence inside OR
  expect_equal( course_weight("CLSE101|EGRB102 & EGRB104|EGRE101|EGMN103 & EGMN190", courses ),
                data.frame( Course = c( "CLSE101",
                                        "EGRB102",
                                        "EGRB104",
                                        "EGRE101",
                                        "EGMN103",
                                        "EGMN190"),
                            Weight = 0.25 ) )
  # CLSE101 0.25; EGRB102 0.25; EGRB104 0.25; EGRE101 0.25; EGMN103 0.25; EGMN190 0.25


  # Choose-N via [N]
  expect_equal( course_weight("CHEM101[3]CHEM102[3]PHYS101[3]MATH402[3]ENGL201", courses ),
                data.frame( Course = c("CHEM101",
                                       "CHEM102",
                                       "PHYS101",
                                       "MATH402",
                                       "ENGL201"),
                            Weight = 0.6 ) )
  # each gets N/M = 3/5 = 0.6

  # Select ≥ N credits via [Ncred] (needs catalog_df)
  expect_equal( course_weight("CHEM101[6cred]CHEM102[6cred]PHYS101[6cred]MATH201[6cred]ENGL201", courses ),
                data.frame( Course = c("CHEM101",
                                       "CHEM102",
                                       "PHYS101",
                                       "MATH201",
                                       "ENGL201"),
                            Weight = 0.4 ) )

})
