data(massi.test.dataset, massi.test.probes)

test_massiInput <- function() {
  checkException(massi.select(massi.test.dataset, massi.test.probes, threshold=5))              
  checkException(massi.select(massi.test.dataset, massi.test.probes, threshold=1.5))
  checkEquals(class(massi.y(massi.test.dataset,massi.test.probes)), "list")
  checkEquals(class(massi.select(massi.test.dataset, massi.test.probes)), "data.frame")
}