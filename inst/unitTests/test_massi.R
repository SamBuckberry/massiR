data(massi.test.dataset, massi.test.probes)

test_massiInput <- function() {
  checkException(massi_select(massi.test.dataset, massi.test.probes, threshold=5))              
  checkException(massi_select(massi.test.dataset, massi.test.probes, threshold=1.5))
  checkEquals(class(massi_y(massi.test.dataset,massi.test.probes)), "list")
  checkEquals(class(massi_select(massi.test.dataset, massi.test.probes)), "data.frame")
}