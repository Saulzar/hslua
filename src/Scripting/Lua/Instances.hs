
module Scripting.Lua.Instances
  ( peekTagged
  , pushTagged
  , peekValue
  , peekKey
  
  , popStack
  
  
  , pushPair
  , iterTable
  , popTable
  , peekTable
  
  , Pair(..)
  
  , globalIndex
  ) where


import qualified Data.Map as M

import Scripting.Lua (luaerror, peek, push, pop, StackValue)
import Control.Monad

import qualified Scripting.Lua as Lua
import Scripting.Lua (asserttype)

import qualified Data.Text as T
import qualified Data.Vector as V

globalIndex :: Lua.LuaState -> Int -> IO Int
globalIndex l i
  | i < 0 = do
      top <- Lua.gettop l
      return $ top + i + 1
  | otherwise = return i


instance (Lua.StackValue o) => Lua.StackValue (Maybe o) where
  push l (Just x)  = Lua.push l x
  push l (Nothing) = Lua.pushnil l
  peek l ix = do  
    i   <- globalIndex l ix    
    nil <- Lua.isnil l i
    if nil then return Nothing else fmap Just $ Lua.peek l i

    
instance (Lua.StackValue l, Lua.StackValue r) => Lua.StackValue (Either l r) where
  push l (Left x) = pushPair l ("Left", x)
  push l (Right x) = pushPair l ("Right", x)
  peek l ix = peekTagged l ix $ \tag -> case tag of 
    "Left"  -> peekValue l Left
    "Right" -> peekValue l Right
    _       -> luaerror "expected Left or Right key"

    
data Pair a b = Pair a b deriving (Eq, Ord, Show, Read)    
    
-- Alternative pair encoding where the pair is a singleton key, value pair in a table
pushPair :: (Lua.StackValue a, Lua.StackValue b) => Lua.LuaState -> (a, b) -> IO ()
pushPair l (k, v) = do
  Lua.createtable l 0 1
  Lua.push l k
  Lua.push l v
  Lua.rawset l (-3)      
  
  
peekPair :: (Lua.StackValue a, Lua.StackValue b) => Lua.LuaState -> Int -> IO (a, b)
peekPair l ix = do 
  i   <- globalIndex l ix
  
  asserttype l i Lua.TTABLE
  Lua.pushnil l
  next <- Lua.next l i
  
  if next then popTable l
          else luaerror "empty table"
    
instance (Lua.StackValue a, Lua.StackValue b) => Lua.StackValue (Pair a b) where
  push l (Pair a b) = pushPair l (a, b)
  peek l ix = do
    (a, b) <- peekPair l ix
    return (Pair a b)
    
    
iterTable ::  a -> (a -> b) -> (Lua.LuaState -> a -> IO a) -> Lua.LuaState -> Int -> IO b
iterTable r f' f l ix = do 
  i   <- globalIndex l ix
  asserttype l i Lua.TTABLE
  Lua.pushnil l
  loop r i 
  
  where
    loop r i = do
--     
      next <- Lua.next l i
      if next 
        then getNext r i
        else  return (f' r)
  
    getNext r i = do
      v <- f l r 
      Lua.pop l 1 
      loop v i
  
pushTagged :: (Lua.StackValue a, Lua.StackValue b) => Lua.LuaState -> a -> b -> IO ()  
pushTagged l = curry $ pushPair l 
  
peekTagged :: (Lua.StackValue a) => Lua.LuaState -> Int -> (a -> IO b) -> IO b
peekTagged l ix f = Lua.addloc "reading tagged value: " $ do  
  i   <- globalIndex l ix
  
  asserttype l i Lua.TTABLE
  Lua.pushnil l
  
  next <- Lua.next l i
  unless next $ Lua.luaerror "empty table"
    
  v <- peekKey l >>= f
  Lua.pop l 1
  
  -- If the table contains more than one element, we need to pop 2 elements to restore the stack
  next <- Lua.next l i
  when next $ Lua.pop l 2
  
  return v
    
    
   

data S = forall a. Lua.StackValue a => S a
  
instance Lua.StackValue S where
  push l (S a) = push l a
  peek l ix    = error "Not implemented"
  
    

popStack :: forall a. StackValue a => Lua.LuaState -> IO a
popStack l = do
  x <- Lua.peek l (-1)
  Lua.pop l 1
  return x
  
 
instance (Lua.StackValue a) => Lua.StackValue [a]
  where
    push l xs = do
     
      Lua.createtable l (length xs + 1) 0
      forM_ (zip [1..] xs) $ \(i, val) -> do
        Lua.push l val
        Lua.rawseti l (-2) i

    peek = iterTable [] reverse $ \l xs -> do
      x <- peekArray l
      return (x:xs)
      
 

peekArray :: Lua.StackValue v => Lua.LuaState -> IO v
peekArray l = do
  t  <- Lua.ltype l (-2)
  when (t /= Lua.TNUMBER) $ Lua.luaerror "non numeric index in array"                  
  
  Lua.addloc "reading array element: " $ Lua.peek l (-1)
  

peekField :: (Lua.StackValue a) => Lua.LuaState -> Int -> Int -> IO a
peekField l i k = do
  
  push l k
  Lua.gettable l i
  mv <- popStack l 
  
  case mv of
    Just v  -> return v
    Nothing -> Lua.luaerror $ "index not found in table: " ++ (show k)

  
peekKey :: (Lua.StackValue k) => Lua.LuaState -> IO k
peekKey l =  Lua.addloc "reading table key: " $ Lua.peek l (-2)
  
peekValue :: (Lua.StackValue a) => Lua.LuaState -> (a -> b) -> IO b
peekValue l f = Lua.addloc "reading table field: " $ fmap f $ Lua.peek l (-1)
      
peekTable :: (Lua.StackValue k, Lua.StackValue v) => Lua.LuaState -> IO (k, v)
peekTable l = do
  k <- peekKey l
  v <- peekValue l id
  
  return (k, v)
  
popTable :: (Lua.StackValue k, Lua.StackValue v) => Lua.LuaState -> IO (k, v)  
popTable l = do
  v <- peekTable l
  Lua.pop l 1
  return v  
    
--     
instance (Lua.StackValue a) => Lua.StackValue (V.Vector a) where
  push l xs = do     
    Lua.createtable l (V.length xs) 0
    forM_ [0..V.length xs - 1] $ \i -> do
      Lua.push l (xs V.! i)
      Lua.rawseti l (-2) (i + 1)

  peek l i = fmap V.fromList $ peek l i
          

instance (Lua.StackValue k, Lua.StackValue v, Ord k) => Lua.StackValue (M.Map k v) where
  push l m = do
    Lua.createtable l (M.size m) 0
    M.foldlWithKey f (return ()) m
    where
      f m' k v = m' >> do
        Lua.push l k
        Lua.push l v
        Lua.rawset l (-3)
        
  peek = iterTable M.empty id $ \l m -> do
    (k, v) <- peekTable l
    return $ M.insert k v m


instance (Lua.StackValue a, Lua.StackValue b) => Lua.StackValue (a,b) where
  push l (a,b) = push l [S a, S b]
  peek l ix = do
    i   <- globalIndex l ix
    asserttype l i Lua.TTABLE
    a <- peekField l i 1
    b <- peekField l i 2   
    return (a, b)


instance (Lua.StackValue a, Lua.StackValue b, Lua.StackValue c) => Lua.StackValue (a,b,c) where
  push l (a,b,c) = push l [S a, S b, S c]
  peek l ix = do
    i   <- globalIndex l ix
    asserttype l i Lua.TTABLE
    a <- peekField l i 1
    b <- peekField l i 2
    c <- peekField l i 3   
    return (a, b, c)
    

instance (Lua.StackValue a, Lua.StackValue b, Lua.StackValue c, Lua.StackValue d) => Lua.StackValue (a,b,c,d) where
  push l (a,b,c,d) = push l [S a, S b, S c, S d]
  peek l ix = do
    i   <- globalIndex l ix
    asserttype l i Lua.TTABLE
    a <- peekField l i 1
    b <- peekField l i 2
    c <- peekField l i 3
    d <- peekField l i 4
    return (a, b, c, d)



instance (Lua.StackValue a, Lua.StackValue b, Lua.StackValue c, Lua.StackValue d, Lua.StackValue e) => Lua.StackValue (a,b,c,d,e) where
  push l (a,b,c,d,e) = push l [S a, S b, S c, S d, S e]
  peek l ix = do
    i   <- globalIndex l ix
    asserttype l i Lua.TTABLE
    a <- peekField l i 1
    b <- peekField l i 2
    c <- peekField l i 3
    d <- peekField l i 4
    e <- peekField l i 5
    return (a, b, c, d, e)
    

instance (Lua.StackValue a, Lua.StackValue b, Lua.StackValue c, Lua.StackValue d, Lua.StackValue e, Lua.StackValue f) => Lua.StackValue (a,b,c,d,e,f) where
  push l (a,b,c,d,e,f) = push l [S a, S b, S c, S d, S e, S f]
  peek l ix = do
    i   <- globalIndex l ix
    asserttype l i Lua.TTABLE
    a <- peekField l i 1
    b <- peekField l i 2
    c <- peekField l i 3
    d <- peekField l i 4
    e <- peekField l i 5
    f <- peekField l i 6   
    return (a, b, c, d, e, f)