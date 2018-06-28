module OrphanB where
  import OrphanA
  
  instance Eq Book where
    (==) (Book authorA publisherA) (Book authorB publisherB) = publisherA == publisherB
