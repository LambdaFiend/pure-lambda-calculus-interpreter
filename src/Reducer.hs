module Reducer where

import Definer
import Parser
import Control.Monad.State.Lazy
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Char



-- For Variable, we apply Set.singleton;
-- for Abstraction, we apply Set.insert;
-- we could union for Abs and App, but
-- it would be linear rather than
-- logarithmic for Abstraction;
-- as for Application, we apply Set.union.
-- We need to be able to distinguish
-- different types of Variables, therefore
-- we have a boolean function as a result.
-- this will be used for other purposes
-- which is why it was created so abstractly.

-- this can easily be made into a function
-- which outputs every value.
-- that way, the burden of traversing the tree
-- only effects once.

foldLambdaSet :: (Index -> Bool) -> Term -> VariableSet
foldLambdaSet b (Variable name index)
  | b index   = Set.singleton name
  | otherwise = Set.empty
foldLambdaSet b (Abstraction (Variable name index) term2)
  | b index   = Set.insert name term2'
  | otherwise = term2'
  where term2' = foldLambdaSet b term2
foldLambdaSet b (Application term1 term2) =
  let term1' = foldLambdaSet b term1
      term2' = foldLambdaSet b term2
   in Set.union term1' term2'

allPossibleVariableSet :: VariableSet
allPossibleVariableSet = Set.fromList $ map (\x -> x:[]) "abcdefghijklmnopqrstuvwxyz"

getFreeVariableSet :: Term -> VariableSet
getFreeVariableSet term = getAllVariableSet term `Set.difference` getBoundVariableSet term

-- index >= 0/index > 0 both work for this,
-- although with different effects. This
-- notion could prove useful.
getBoundVariableSet :: Term -> VariableSet
getBoundVariableSet term = foldLambdaSet (\index -> index == 0) term

getAllVariableSet :: Term -> VariableSet
getAllVariableSet term = foldLambdaSet (\index -> True) term

isAlphaEquivalent :: Term -> Term -> Bool
isAlphaEquivalent (Variable name1 index1) (Variable name2 index2)
  | index1 == -1 = indexCompare && name1 == name2
  | otherwise    = indexCompare
  where indexCompare = index1 == index2
isAlphaEquivalent (Abstraction _ term1) (Abstraction _ term2) =
  isAlphaEquivalent term1 term2
isAlphaEquivalent (Application term1 term1') (Application term2 term2') =
  isAlphaEquivalent term1 term2 && isAlphaEquivalent term1' term2'
isAlphaEquivalent _ _ = False

-- make a function for beta equivalence? is it possible???
-- if so, can it be achieved without too much complexity?


alphaConvertIfNeeded :: Term -> Term -> Term
alphaConvertIfNeeded term1 term2 =
  if (fV `Set.intersection` bV == Set.empty)
    then term1
    else
      let pV@possibleVariables = allPossibleVariableSet
          aV@availableVariables =
            pV
            `Set.difference` fV
            `Set.difference` bV
       in autoAlphaConversion term1 (fV, aV, pV, bV) Map.empty
  where bV@boundVariables = getBoundVariableSet term1
        fV@freeVariables  = getFreeVariableSet term2

handleReduction :: Term -> Term -> Name -> (Term, Reduction) 
handleReduction term1 term2 name =
  if isSubstitutionNeeded
    then (term1, ReductionIgnored)
    else (alphaConvertIfNeeded term1 term2, Reduction 1 term2)
  where isSubstitutionNeeded = substitutionNeeded term1 name 1

-- is looking for a reduction
betaReduction :: Term -> Reduction -> (Term, Reduction)
betaReduction term reduced@ReductionIgnored = (term, reduced)
betaReduction (Variable name index) reduced@NoReduction = (Variable name index, reduced)
betaReduction (Abstraction term1 term2) NoReduction =
  let (term2', reduced) = betaReduction term2 NoReduction
  in (Abstraction term1 term2', reduced)

-- this is where the reduction is initiated  
betaReduction (Application (Abstraction (Variable name _) term1) term2) NoReduction =
  let (term1', reduction) = handleReduction term1 term2 name
   in betaReduction term1' reduction

betaReduction (Application term1 term2) NoReduction =
  let (term1', reduced1) = betaReduction term1 NoReduction
      (term2', reduced2) = betaReduction term2 NoReduction
      reduced =
        if (reduced1 == NoReduction)
          then reduced2
          else reduced1
   in (Application term1' term2', reduced)

-- is reducing
betaReduction (Variable name index1) reduced@(Reduction index2 term)
  | index1 > 0 && index1 == index2 = (term, reduced)
  | otherwise = (Variable name index1, reduced)
betaReduction (Abstraction term1 term2) reduced@(Reduction index1 term) =
  let index1' = index1 + 1
      (term2', _)  = betaReduction term2 (Reduction index1' term)
   in (Abstraction term1 term2', reduced)
betaReduction (Application term1 term2) reduction@(Reduction index1 term) =
  let (term1', _) = betaReduction term1 reduction
      (term2', _) = betaReduction term2 reduction
   in (Application term1' term2', reduction)

autoAlphaConversion :: Term -> VariableSet4 -> NameToNewName -> Term
autoAlphaConversion (Variable name index) varSets nTNM@nameToNewName =
  let name' =
        if (Set.member name $ fst4 varSets)
          then Map.findWithDefault "" name nTNM
          else name
   in Variable name' index
autoAlphaConversion (Abstraction (Variable name index) term2) varSets nTNM@nameToNewName =
  let (name', varSets') =
        if (Set.member name $ fst4 varSets)
          then renameVariable name varSets
          else (name, varSets)
      nTNM'  = Map.insert name name' nTNM
      term2' = autoAlphaConversion term2 varSets' nTNM'
   in Abstraction (Variable name' index) term2'
autoAlphaConversion (Application term1 term2) varSets nTNM@nameToNewName =
  let term1' = autoAlphaConversion term1 varSets nTNM
      term2' = autoAlphaConversion term2 varSets nTNM
   in Application term1' term2'


renameVariable :: String -> VariableSet4 -> (String, VariableSet4)
renameVariable name varSets = 
  let result   = renameVariable' name varSets
      name'    = reverse $ fst result
      varSets' = snd result      
   in (name', varSets')


-- an open idea for the future...
-- to give priority to certain chars.
-- supposed to convert the head of name
{--
prioritizeXYZ :: Char -> Char
prioritizeXYZ c =
  case c of
    'a' -> 'x'
    'b' -> 'y'
    'c' -> 'z'
    x   -> chr $ ord x - 3
--}



renameVariable' :: String -> VariableSet4 -> (String, VariableSet4)
renameVariable' name varSets =
  let name' = Set.findMin $ snd4 varSets'
      varSets'' = ( fst4 varSets'
                  , Set.deleteMin $ snd4 varSets'
                  , trd4 varSets'
                  , frt4 varSets'
                  )
      varSets''' = renewAvailableVariables varSets''
  in (name', varSets''')
  where varSets' = renewAvailableVariables varSets

renewAvailableVariables :: VariableSet4 -> VariableSet4
renewAvailableVariables varSets
  | snd4 varSets == Set.empty =
    let nPV@newPossibleVariables  = Set.fromList $ map (\x -> '\'':x) $ Set.toList $ trd4 varSets
        nAV@newAvailableVariables =
          nPV
          `Set.difference` fst4 varSets
          `Set.difference` frt4 varSets
     in
       if (newAvailableVariables == Set.empty)
         then renewAvailableVariables (fst4 varSets, nAV, nPV, frt4 varSets)
         else (fst4 varSets, nAV, nPV, frt4 varSets)
  | otherwise                 = varSets


substitutionNeeded :: Term -> Name -> Index -> Bool
substitutionNeeded (Variable _ index1) name index2 = index1 /= index2
substitutionNeeded (Abstraction (Variable name1 _) term2) name2 index
  | name1 == name2 = True
  | otherwise = substitutionNeeded term2 name2 (index + 1)
substitutionNeeded (Application term1 term2) name index =
  let result1 = substitutionNeeded term1 name index
      result2 = substitutionNeeded term2 name index
   in result1 && result2


reduceMultiple :: Term -> Int -> Term
reduceMultiple term 0 = term
reduceMultiple term n
  | reduced == NoReduction = term
  | otherwise              = reduceMultiple term' (n - 1)
  where (term', reduced) = betaReduction term NoReduction


etaReductionSearch :: Term -> Term
etaReductionSearch (Abstraction term1 term2) = etaReductionApply term2 term1
etaReductionSearch term@(Application term1 term2) =
  let term1' = etaReductionSearch term1
      term2' = etaReductionSearch term2
   in Application term1' term2'
etaReductionSearch term = term

etaReductionApply :: Term -> Term -> Term
etaReductionApply (Application term2 (Variable name 1)) _
  | not $ Set.member name fV = term2
  where fV@freeVariables = getFreeVariableSet term2
etaReductionApply term1 term = Abstraction term term1

type Reductions = Int

-- currently, it's not as efficient as initially thought
-- I had forgotten to check if the right side of application
-- for valid cases was a free variable on the left side
-- thus, I had to add a way to know the free variables
-- which allowed me to figure out a bug I was not aware of
-- but as things are, I have to call this method multiple times
-- (n times, for all n eta redexes)
-- maybe someday I'll create a data which allows me to
-- efficiently redetermine the free variables

chainEtaReductionSearch :: Term -> [Term] -> Reductions -> (Term, Reductions)
chainEtaReductionSearch (Abstraction term1 term2) terms n =
  chainEtaReductionSearch term2 (term1:terms) $ n - 1
chainEtaReductionSearch (Application term1 term2) [] n =
  let (term1', n')  = chainEtaReductionSearch term1 [] n
      (term2', n'') = chainEtaReductionSearch term2 [] n'
   in (Application term1' term2', n'')
chainEtaReductionSearch term@(Application term1 term2) terms n =
  (chainEtaReductionApply term $ reverse terms) n
chainEtaReductionSearch term _ n = (term, n)

chainEtaReductionApply :: Term -> [Term] -> Reductions -> (Term, Reductions)
chainEtaReductionApply term [] n = (term, n)
chainEtaReductionApply (Application term1 (Variable name index)) (term:terms) n
  | index == length (term:terms) && not (Set.member name fV) =
    let (term1', n') =
          if (terms /= [])
            then (term1, n)
            else chainEtaReductionSearch term1 [] n
     in chainEtaReductionApply term1' terms n'
  where fV@freeVariables = getFreeVariableSet term1
chainEtaReductionApply term1 terms n =
  let (term1', n') = chainEtaReductionSearch term1 [] n
   in (foldr Abstraction term1' terms, n')


-- left to right
-- -1 for convert all, 0 for none, n > 0 for n conversions,
-- from left to right and only looking at 1 depth

alphaConversion :: Term -> Name -> Name -> Conversions -> (Term, Conversions, Valid)
alphaConversion term nameOriginal nameTarget conversions =
  let conversion = alphaConversionSearch term nameOriginal nameTarget conversions
   in conversion

alphaConversionSearch :: Term -> Name -> Name -> Conversions -> (Term, Conversions, Valid)
alphaConversionSearch (Abstraction (Variable nameOriginal1 _) term) nameOriginal2 nameTarget conversions
  | conversions /= 0 && nameOriginal1 == nameOriginal2 =
    let term1 = Variable nameTarget 0
        term2 = alphaConversionApply term nameTarget 1
        fV@freeVariables = getFreeVariableSet term
        valid = not $ Set.member nameTarget fV
     in (Abstraction term1 term2, conversions - 1, valid)
alphaConversionSearch (Application term1 term2) nameOriginal nameTarget conversions
  | conversions /= 0 =
    let (term1', conversions1, valid1) = alphaConversionSearch term1 nameOriginal nameTarget conversions
        (term2', conversions2, valid2) = alphaConversionSearch term2 nameOriginal nameTarget conversions1
     in (Application term1' term2', conversions2, valid1 && valid2)
alphaConversionSearch term _ _ conversions = (term, conversions, True)

alphaConversionApply :: Term -> Name -> Index -> Term
alphaConversionApply term@(Variable _ index1) name index2
  | index1 == index2 = Variable name index1
  | otherwise        = term
alphaConversionApply (Abstraction term1 term2) name index =
  let term2' = alphaConversionApply term2 name (index + 1)
   in Abstraction term1 term2'
alphaConversionApply (Application term1 term2) name index =
  let term1' = alphaConversionApply term1 name index
      term2' = alphaConversionApply term2 name index
   in Application term1' term2'

getPossibleBetaReductions :: Term -> [(Term, Term)]
getPossibleBetaReductions term@(Variable _ _) = []
getPossibleBetaReductions term@(Abstraction term1 term2) =
  map (\(reductions, term2') -> (reductions, Abstraction term1 term2')) $ getPossibleBetaReductions term2
getPossibleBetaReductions term@(Application term1@(Abstraction _ _) term2) =
  let betaRedexes1  = getPossibleBetaReductions term1
      betaRedexes1' = map (\(reductions, term1') -> (reductions, Application term1' term2)) betaRedexes1
      betaRedexes2  = getPossibleBetaReductions term2
      betaRedexes2' = map (\(reductions, term2') -> (reductions, Application term1 term2')) betaRedexes2
   in (term, Variable "_" (-3)):betaRedexes1' ++ betaRedexes2'
getPossibleBetaReductions (Application term1 term2) =
  let betaRedexes1  = getPossibleBetaReductions term1
      betaRedexes1' = map (\(reductions, term1') -> (reductions, Application term1' term2)) betaRedexes1
      betaRedexes2  = getPossibleBetaReductions term2
      betaRedexes2' = map (\(reductions, term2') -> (reductions, Application term1 term2')) betaRedexes2
   in betaRedexes1' ++ betaRedexes2'

findAndGlue :: Term -> Term -> Term
findAndGlue (Variable _ (-3)) reducedTerm = reducedTerm
findAndGlue term@(Variable _ _) _ = term
findAndGlue (Abstraction term1 term2) reducedTerm =
  let term2' = findAndGlue term2 reducedTerm
   in Abstraction term1 term2'
findAndGlue (Application term1 term2) reducedTerm =
  let term1' = findAndGlue term1 reducedTerm
      term2' = findAndGlue term2 reducedTerm
   in Application term1' term2'

isBetaEquivalent :: Term -> Term -> Bool
isBetaEquivalent term1 term2 =
  let comparison1 = compareAndReduceUntilTrue term1 term2
      comparison2 = comparison1 || compareAndReduceUntilTrue term2 term1
   in comparison2

compareAndReduceUntilTrue :: Term -> Term -> Bool
compareAndReduceUntilTrue term1 term2 =
  let comparison           = isAlphaEquivalent term1 term2
      (term1', reduction) = betaReduction term1 NoReduction 
   in
     if (reduction == NoReduction)
       then comparison
       else comparison || compareAndReduceUntilTrue term1' term2

chainBetaReduction :: Term -> ReductionSequence -> Reductions -> Reductions -> Term
chainBetaReduction term@(Variable name index) seq n k
  | substitution == Variable "" (-2) = term
  | otherwise                        =
    betaReductionUpdateIndexes substitution (Seq.length seq - k)
  where substitution = extractTermFromMaybe $ Seq.lookup (index - 1) seq
chainBetaReduction (Abstraction term1 term2) seq n k =
  let seq'   = (Variable "" (-2)) Seq.<| seq
      term2' = chainBetaReduction term2 seq' n k
   in Abstraction term1 term2'
chainBetaReduction (Application (Abstraction term1 term2) term3) seq n k =
  let seq'   = term3 Seq.<| seq
      term2' = chainBetaReduction term2 seq' n (k + 1)
   in term2'
chainBetaReduction (Application term1 term2) seq n k =
  let term1' = chainBetaReduction term1 seq n k
      term2' =
        case term1' of
          term1''@(Abstraction _ _) -> 
            chainBetaReduction (Application term1' term2) seq n k
          _                         ->
            Application term1' $ chainBetaReduction term2 seq n k
   in term2'


-- next, I must add environment variables! I'm getting so close...


betaReductionUpdateIndexes :: Term -> Index -> Term
betaReductionUpdateIndexes term@(Variable name index1) index2
  | index1 > 0 = Variable name (index1 + index2)
  | otherwise  = term
betaReductionUpdateIndexes (Abstraction term1 term2) index =
  let term2' = betaReductionUpdateIndexes term2 index
   in Abstraction term1 term2'
betaReductionUpdateIndexes (Application term1 term2) index = 
  let term1' = betaReductionUpdateIndexes term1 index
      term2' = betaReductionUpdateIndexes term2 index
   in Application term1' term2'



