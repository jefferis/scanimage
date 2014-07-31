context('read ScanImage TIFF')
test_that("read a multipage TIFF",{
  t=read.scanimage(system.file('extdata/Blank-IPA_1s_16r_032.tif',
                               package='scanimage'))
  expect_is(t,'list')
  t1=read.scanimage(system.file('extdata/Blank-IPA_1s_16r_032.tif',
                               package='scanimage'), slices = 1)
  expect_is(t1,'matrix')
})

test_that("parse TIFF description",{
  imfile=system.file('extdata/Blank-IPA_1s_16r_032.tif', package='scanimage')
  d=parse_description(imfile)
  expect_is(d,'list')
  expect_equal(d$state.configName, 'ajdm_piezo')
  expect_equal(d$state.acq.externallyTriggered, 1)
  expect_is(parse_description(imfile, raw = TRUE), 'character')

})
