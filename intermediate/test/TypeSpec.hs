module TypeSpec (spec) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import qualified Data.Map
import Intermediate.Type
import Test.Hspec

-- run a unification starting from a fresh state;
-- returns the result and the final state
runUnify :: TypeVarM a -> (a, TypeVarState)
runUnify m = runState m initialTypeVarState

-- make two fresh type vars, then run the given test body on them
withVars :: (Type -> Type -> TypeVarM a) -> (a, TypeVarState)
withVars f = runUnify $ do
  a <- newTypeVar
  b <- newTypeVar
  f (TVar a) (TVar b)

arrow :: Type -> Type -> Type
arrow = TArrow NoLabel

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _ = False

spec :: Spec
spec = do
  describe "unify" $ do
    it "unifies identical base types" $
      fst (runUnify (runExceptT (unify TInt TInt))) `shouldBe` Right ()

    it "rejects different base types" $
      fst (runUnify (runExceptT (unify TInt TString))) `shouldSatisfy` isLeft'

    it "unifies a type var with itself without binding it" $ do
      let (result, st) = withVars $ \a _ -> runExceptT (unify a a)
      result `shouldBe` Right ()
      substitutions st `shouldBe` Data.Map.empty

    it "binds a type var to a concrete type (either side)" $ do
      let (r1, r2) = fst $ withVars $ \a b -> do
            _ <- runExceptT (unify a TInt)
            _ <- runExceptT (unify TBool b)
            (,) <$> resolveType a <*> resolveType b
      r1 `shouldBe` TInt
      r2 `shouldBe` TBool

    it "follows chains of bindings" $ do
      let result = fst $ withVars $ \a b -> do
            _ <- runExceptT (unify a b)
            _ <- runExceptT (unify b TInt)
            resolveType a
      result `shouldBe` TInt

    it "rejects a conflict through a chain of bindings" $ do
      let result = fst $ withVars $ \a b -> do
            _ <- runExceptT (unify a b)
            _ <- runExceptT (unify b TInt)
            runExceptT (unify a TString)
      result `shouldSatisfy` isLeft'

    it "unifies arrows structurally" $ do
      let result = fst $ withVars $ \a b -> do
            r <- runExceptT (unify (arrow a TInt) (arrow TBool b))
            (,,) r <$> resolveType a <*> resolveType b
      result `shouldBe` (Right (), TBool, TInt)

    it "rejects arrows with different labels" $
      fst (runUnify (runExceptT (unify (TArrow NoLabel TInt TInt) (TArrow (Labeled "x") TInt TInt))))
        `shouldSatisfy` isLeft'

    it "unifies tuples and rejects different lengths" $ do
      let ok = fst $ withVars $ \a _ -> do
            r <- runExceptT (unify (TTuple [a, TInt]) (TTuple [TBool, TInt]))
            (,) r <$> resolveType a
      ok `shouldBe` (Right (), TBool)
      fst (runUnify (runExceptT (unify (TTuple [TInt]) (TTuple [TInt, TInt]))))
        `shouldSatisfy` isLeft'

    it "unifies type constructors, rejecting different names or arity" $ do
      let ok = fst $ withVars $ \a _ -> do
            r <- runExceptT (unify (TCon "list" [a]) (TCon "list" [TInt]))
            (,) r <$> resolveType a
      ok `shouldBe` (Right (), TInt)
      fst (runUnify (runExceptT (unify (TCon "list" [TInt]) (TCon "array" [TInt]))))
        `shouldSatisfy` isLeft'
      fst (runUnify (runExceptT (unify (TCon "list" [TInt]) (TCon "list" [TInt, TInt]))))
        `shouldSatisfy` isLeft'

    it "fails the occurs check directly" $ do
      let result = fst $ withVars $ \a _ -> runExceptT (unify a (arrow a TInt))
      result `shouldSatisfy` isLeft'

    it "fails the occurs check through existing bindings" $ do
      -- b := (a -> int), then a ~ (b -> int) would make a infinite
      let result = fst $ withVars $ \a b -> do
            _ <- runExceptT (unify b (arrow a TInt))
            runExceptT (unify a (arrow b TInt))
      result `shouldSatisfy` isLeft'

  describe "resolveType" $ do
    it "applies substitutions inside nested types" $ do
      let result = fst $ withVars $ \a b -> do
            _ <- runExceptT (unify a TInt)
            _ <- runExceptT (unify b a)
            resolveType (TTuple [arrow a b, TCon "list" [b]])
      result `shouldBe` TTuple [arrow TInt TInt, TCon "list" [TInt]]

    it "leaves unbound type vars alone" $ do
      let (result, a') = fst $ withVars $ \a _ -> (,) <$> resolveType (arrow a TInt) <*> pure a
      result `shouldBe` arrow a' TInt
