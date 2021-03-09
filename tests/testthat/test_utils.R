library(rawutil)


# CONSTANTS ---------------------------------------------------------------

# named vector
dict <- letters[6:10]
names(dict) <- letters[1:5]

bad_dict <- dict
names(bad_dict) <- rep("A", 5)

# test_df
test_df <- make_dummy_df()

# TESTS -------------------------------------------------------------------

# revalue_col() -----------------------------------------------------------

test_that("revalue_col() returns expected values", {
  expect_equal(revalue_col(df = test_df,
                           colname = "chr",
                           dict = dict)$chr,
               c("f", "g", "h", "i", "j", NA))

  expect_equal(revalue_col(df = test_df,
                           colname = "chr_rpt",
                           dict = dict)$chr_rpt,
               c(rep("f", 3), rep("h", 3)))
})

test_that("revalue_col() raises error with non-unique 'keys'", {
  expect_error(revalue_col(df = test_df,
                           colname = "chr",
                           dict = bad_dict),
               "contains non-unique values")
})

test_that("revalue_col() replaces values missing from `dict` with default values appropriately",
          {
            expect_equal(
              # use default_value of "foo". `NA` value should be set to "foo"
              revalue_col(
                df = test_df,
                colname = "chr",
                dict = dict,
                default_value = "foo"
              )$chr[6],
              "foo"
            )
          })

test_that("revalue_col() raises error if df[[colname]] is not of class numeric/character", {
  # error should state that this column is of type factor
  expect_error(revalue_col(df = test_df,
                           colname = "fac",
                           dict = dict,
                           default_value = "foo"),
               "factor")

  # error should state that this column is of type logical
  expect_error(revalue_col(df = test_df,
                           colname = "log",
                           dict = dict,
                           default_value = "foo"),
               "logical")
})
