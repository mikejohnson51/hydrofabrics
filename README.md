# hydrofabrics
------------------------

<!-- badges: start -->
<!-- badges: end -->

The goal of `hydrofabrics` is to modify existing hydrofabrics for hydrologic modeling.

[**SPRINT 0:**](https://mikejohnson51.github.io/hydrofabrics/01-sprint-0.html) Refactoring a HUC12 based on basin size and topology
[**SPRINT 0:**](https://mikejohnson51.github.io/hydrofabrics/00-data-structures.html) How to represent `hy_features`?

...

**SPRINT 1:** Indexing objects to the flow network (e.g., streamgages & waterbodies) 

******

The key tasks are to:

1.  [X] Develop more *consistent and evenly distributed catchment sizes*
2.  [X] Fix *topology errors* resulting from scalar mismatches in product
    creation
3.  [ ] Build a *referencing system* capable of handling network
    calculations
4.  [ ] Retain a indexing scheme that can allow these new identities to
    relate to there origin identities (e.g., cross walk
5.  [ ] Establish a addressing scheme that allows users to place hydrologic
    features (e.g., a stream gage) on to the network with precise
    spatial and hydrologic addressing.
6.  [ ] Develop an indexing scheme that can be applied at a continental
    scale, but that can allow areas to be processes piecemeal.
