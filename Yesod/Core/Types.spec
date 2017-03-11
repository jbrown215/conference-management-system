module spec Yesod.Core.Types where

data HandlerT site m a <p :: b -> Bool> = Yesod.Core.Types.HandlerT {
        unHandlerT :: Yesod.Core.Types.HandlerData site (Yesod.Core.Types.MonadRoute m) -> m a
        }

data variance HandlerT covariant covariant covariant contravariant

instance Monad HandlerT where 
 >>= :: forall <p :: c -> Bool, f:: a -> b -> Bool>. 
        x:Tagged <p> a
     -> (u:a -> Tagged <p> (b <f u>))
     -> Tagged <p> (b<f (content x)>); 
 >>  :: x:Tagged a
     -> Tagged b
     -> Tagged b;
 return :: forall <p :: User -> Bool>. a -> Tagged <p> a 
