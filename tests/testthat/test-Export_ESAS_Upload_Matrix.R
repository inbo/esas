# Create a tempoary directory with withr so it'll be cleaned as the tests finish
testing_tempdir <- withr::local_tempdir(pattern = "esas_tests",
                     tmpdir = tempdir())

})
