test_that("basic functions", {

  expect_equal( format_for_hm( 0000 ), "00:00 AM" )
  expect_equal( format_for_hm( 0001 ), "00:01 AM" )
  expect_equal( format_for_hm( 1323 ), "01:23 PM" )
  expect_equal( format_for_hm( 2359 ), "11:59 PM" )
  expect_equal( format_for_hm( NA ), NA )
  expect_equal( format_for_hm( logical(0) ), NA )

  expect_error( format_for_hm( FALSE ) )
  expect_error( format_for_hm( "Bob" ) )
  expect_error( format_for_hm( -1200 ) )
  expect_error( format_for_hm( 4200 ) )

})
