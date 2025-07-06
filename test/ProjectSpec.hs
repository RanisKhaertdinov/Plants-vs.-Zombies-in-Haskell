module ProjectSpec where

import           Test.Hspec

spec :: Spec
spec = do
  describe "Sanity checks" $ do
    describe "Arithmetic" $ do
      it "2 + 2 = 4" $
        2 + 2 == 4
