module IntCodeComputer (State(..), Memory, Input, Output, toState, runState, withPointer, withInput, withOutput, readCurrentPointerMemory) where

    import Data.Bifunctor (bimap)
    import Data.Foldable (toList)
    import Data.IntMap.Strict (IntMap)
    import Data.List (uncons)
    import qualified Data.IntMap.Strict as IntMap
    
    type Input = [Int]
    type Output = [Int]
    type Memory = [Int]
    type Mode = Int
    type Address = Int
    type OpCode = Int
    type RelativeBase = Int
    type MemoryMap = IntMap Int
    
    data State = State {
        _pointer :: Address,
        _input :: Input,
        _output :: Output,
        _memory :: MemoryMap,
        _relativeBase :: RelativeBase,
        _haltOnOutput :: Bool
    } deriving (Show)
    
    withPointer :: Address -> State -> State
    withPointer pointer st = st {_pointer = pointer}
    
    withInput :: Input -> State -> State
    withInput input st = st {_input = input}
    
    withOutput :: Output -> State ->  State
    withOutput output st = st {_output = output}
    
    withMemory :: MemoryMap -> State ->  State
    withMemory memory st = st {_memory = memory}
    
    withRelativeBase :: RelativeBase -> State -> State
    withRelativeBase base st = st {_relativeBase = base}
    
    -- This is unsafe - non exhaustive match on OpCode
    instruction :: OpCode -> ([Mode] -> State -> State)
    instruction 1 = sumOp 
    instruction 2 = mulOp
    instruction 3 = inputOp
    instruction 4 = outputOp
    instruction 5 = jumpOp id
    instruction 6 = jumpOp not
    instruction 7 = ltOp
    instruction 8 = eqOp
    instruction 9 = adjustBaseOp
    instruction 99 = const id
    
    sumOp :: [Mode] -> State -> State
    sumOp = binOp (+)
    
    mulOp :: [Mode] -> State -> State
    mulOp = binOp (*)
    
    ltOp :: [Mode] -> State -> State
    ltOp = binOp (\a b -> if a < b then 1 else 0)
    
    eqOp :: [Mode] -> State -> State
    eqOp = binOp (\a b -> if a == b then 1 else 0)
    
    -- This is unsafe - non exhaustive match on [Mode]
    adjustBaseOp :: [Mode] -> State -> State
    adjustBaseOp (m:_) current = runState . withRelativeBase (currentRelativeBase + adjustment) . withPointer (succ pointer) $ current
        where pointer = succ . _pointer $ current
              currentRelativeBase = _relativeBase current
              adjustment = readAndAccess m currentRelativeBase pointer (_memory current)
    
    -- This is unsafe - non exhaustive match on [Mode]
    jumpOp :: (Bool -> Bool) -> [Mode] -> State -> State
    jumpOp f (m1:m2:_) current = runState . withPointer newPointer $ current
        where pointer = _pointer current
              memory = _memory current
              relativeBase = _relativeBase current -- 0
              test = readAndAccess m1 relativeBase (pointer + 1) memory  --0
              target = readAndAccess m2 relativeBase (pointer + 2) memory -- 4
              newPointer = if f (test /= 0) then target else pointer + 3
    
    -- This is unsafe - non exhaustive match on [Mode]
    binOp :: (Int -> Int -> Int) -> [Mode] -> (State -> State)
    binOp f (m1:m2:m3:_) current = runState . withMemory newMemory . withPointer (pointer + 4) $ current
        where pointer = _pointer current
              currentMemory = _memory current
              relativeBase = _relativeBase current
              v1 = readAndAccess m1 relativeBase (pointer + 1) currentMemory
              v2 = readAndAccess m2 relativeBase (pointer + 2) currentMemory
              newMemory = writeMemory m3 relativeBase (readMemory (pointer + 3) currentMemory) (f v1 v2) currentMemory
    
    -- This is unsafe - non exhaustive match on [Mode]
    inputOp :: [Mode] -> (State -> State)
    inputOp (m:_) current = maybe current next . uncons . _input $ current
        where pointer = succ . _pointer $ current
              next (i, is) = runState . withInput is . withMemory (memory i) . withPointer (succ pointer) $ current 
              memory v = let currentMemory = _memory current in writeMemory m (_relativeBase current) (readMemory pointer currentMemory) v currentMemory
    
    -- This is unsafe - non exhaustive match on [Mode]
    outputOp :: [Mode] -> State -> State
    outputOp (m:_) current = if _haltOnOutput newState then newState else runState newState
        where pointer = succ . _pointer $ current
              newOutput = let memory = _memory current in (readAndAccess m (_relativeBase current) pointer memory :) . _output $ current
              newState = withPointer (succ pointer) . withOutput newOutput $ current
    
    -- This assumes Mode ∈ {0, 1, 2}
    writeMemory :: Mode -> RelativeBase -> Address -> Int -> MemoryMap -> MemoryMap
    writeMemory 2 base = \a -> writeMemory 0 base (a + base)
    writeMemory _ _ = IntMap.insert
    
    -- This is unsafe - non exhaustive match on Mode
    accessMemory ::  Mode -> RelativeBase -> Address -> MemoryMap -> Int
    accessMemory 0 _ = IntMap.findWithDefault 0
    accessMemory 1 _ = const
    accessMemory 2 base = \a -> accessMemory 0 base (base + a)
    
    readMemory :: Address -> MemoryMap -> Int
    readMemory = accessMemory 0 0
    
    readAndAccess :: Mode -> RelativeBase -> Address -> MemoryMap -> Int
    readAndAccess mode base pointer memory = accessMemory mode base (readMemory pointer memory) memory
    
    readCurrentPointerMemory :: State -> Int
    readCurrentPointerMemory st = readMemory (_pointer st) (_memory st)
    
    toState :: Foldable t => Bool -> Input -> t Int -> State
    toState haltOnOutput input memory = State{
        _pointer = 0,
        _input = input,
        _output = [],
        _memory = IntMap.fromDistinctAscList . zip [0..] . toList $ memory,
        _relativeBase = 0,
        _haltOnOutput = haltOnOutput
      }
    
    parseInstruction :: Int -> (OpCode, [Mode])
    parseInstruction = bimap (fromDigits . reverse) (++ repeat 0) . splitAt 2 . toDigitsReversed
        where fromDigits = foldl addDigit 0 where addDigit num d = 10 * num + d
              toDigitsReversed n = let (d, m) = n `quotRem` 10 in if d == 0 then [m] else m : toDigitsReversed d
    
    runState ::  State -> State
    runState current = transform current
        where transform = uncurry instruction . parseInstruction . readCurrentPointerMemory $ current