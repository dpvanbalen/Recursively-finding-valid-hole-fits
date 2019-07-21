# Recursively-finding-valid-hole-fits

Attempts to fill holes in Haskell code at compile time.

# Usage:

Requires a fresh GHC built from source, for now. Once the next supported version of GHC ships and the holefitplugin interface is probably more stable, this project will move out of prototype phase

`ghc -fno-max-valid-hole-fits -fplugin Plugin` for the most default experience.

`ghc -fno-max-valid-hole-fits -fplugin Plugin -fplugin-opt=Plugin:3 -fplugin-opt=Plugin:Nothing -fplugin-opt=Plugin:Nothing -fplugin-opt=Plugin:True` for the same default experience, but more easily configurable.

# Settings:

In order, the commandline options represent the depth, width, number of results, and whether to prune away common useless suggestions (such as id, fail, but also a lot of non-total functions like head).

The depth defaults on 3. The depth and width together influence the speed of the search hugely, and in my limited testing I found 3 to be a good starting point. Increasing above 5 is usually not recommended, depending on the width and context.

The width is a Maybe Int, defaulting on Nothing. It represents the branching size of the tree. Smaller widths are immensely faster to compute, but note that a fold already requires a width of 3. Depending on the context, keeping the width on "Just 2" could be a good option. When the width is Nothing, the branching size is equal to the remaining depth at each level. This is a very simple heuristic to find complicated functions with simple arguments efficiently.

When a number of results is specified, the breadth first search is only expanded as far as needed to reach this number. Regardless of whether it is specified, all found results are sorted on their "length" in words.
