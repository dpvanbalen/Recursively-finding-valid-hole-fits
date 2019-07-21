# Recursively-finding-valid-hole-fits

Attempts to fill holes in Haskell code at compile time.

# Usage:

Requires a fresh GHC built from source, for now.

`ghc -fno-max-valid-hole-fits -fplugin Plugin` for the most default experience.

`ghc -fno-max-valid-hole-fits -fplugin Plugin -fplugin-opt=Plugin:3 -fplugin-opt=Plugin:Nothing -fplugin-opt=Plugin:Nothing -fplugin-opt=Plugin:True` for the same default experience, but more easily configurable.

