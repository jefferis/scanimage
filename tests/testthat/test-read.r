context('read ScanImage TIFF')
test_that("read a multipage TIFF",{
  img=system.file('extdata/Blank-IPA_1s_16r_032.tif', package='scanimage')
  t=read.scanimage(img)
  expect_is(t,'list')
  t1=read.scanimage(img, slices = 1)
  t2=read.scanimage(img, frames = 1)
  t3=read.scanimage(img, frames = 1, channels = 1)

  expect_is(t1,'matrix')
  expect_equal(t1, t2[[1]])
  expect_equal(t3, t2)
  expect_error(read.scanimage(img, channels = 2))

})

test_that("parse TIFF description",{
  imfile=system.file('extdata/Blank-IPA_1s_16r_032.tif', package='scanimage')
  d=parse_description(imfile)
  expect_is(d,'list')
  expect_equal(d$state.configName, 'ajdm_piezo')
  expect_equal(d$state.acq.externallyTriggered, 1)
  expect_is(parse_description(imfile, raw = TRUE), 'character')

})

test_that("parse TIFF description",{
  imfile=system.file('extdata/Blank-IPA_1s_16r_032.tif', package='scanimage')
  d=scanimageinfo(imfile)
  expect_is(d,'data.frame')
  expect_equivalent(d[, 1:4], c(64L, 64L, 1L, 30L))
})
